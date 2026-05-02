# Takes a foreign code object and tries to make it feel somewhat like a Raku
# one. Note that it doesn't have signature information we can know about.

my class ForeignCode
  does Callable
  does Rakudo::Internals::ImplementationDetail
{ # declared in BOOTSTRAP
    # class ForeignCode
    #     has Code $!do;                # Code object we delegate to

    method arity(          --> 0) { }
    method count(        --> Inf) { }
    method has-phasers(--> False) { }
    method has-loop-phasers(--> False) { }

    method signature(ForeignCode:D:) { (sub (|) { }).signature }

    method name() { (nqp::can($!do, 'name') ?? $!do.name !! nqp::getcodename($!do)) || '<anon>' }
}

my class Rakudo::Internals::EvalIdSource {
    my Int $count = 0;
    my Lock $lock = Lock.new;
    method next-id() {
        $lock.protect: { $count++ }
    }
}
proto sub EVAL(
  $code is copy where Blob|Cool|Callable|RakuAST::Node,
  Str()       :$lang is copy = 'Raku',
  PseudoStash :context($ctx),
  Str()       :$filename = Str,
  Bool()      :$check,
  *%_
) is raw {
    die "EVAL() in Raku is intended to evaluate strings or ASTs, did you mean 'try'?"
      if nqp::istype($code,Callable);

# TEMPORARY HACK
$lang = 'Raku' if $lang eq 'perl6';

    # First look in compiler registry.
    my $compiler := nqp::getcomp($lang);
    if nqp::isnull($compiler) {
        # Try a multi-dispatch to another EVAL candidate. If that fails to
        # dispatch, map it to a typed exception.
        CATCH {
            when X::Multi::NoMatch {
                X::Eval::NoSuchLang.new(:$lang).throw
            }
        }
        return {*};
    }

    my $*CTXSAVE; # make sure we don't use the EVAL's MAIN context for the
                    # currently compiling compilation unit
    my $context := nqp::defined($ctx) ?? $ctx !! CALLER::LEXICAL::;
    my $compiled;
    my $eval_ctx := nqp::getattr(nqp::decont($context), PseudoStash, '$!ctx');

    if nqp::istype($code, RakuAST::Node) {
        # Wrap as required to get compilation unit.
        my $comp-unit := do if nqp::istype($code, RakuAST::CompUnit) {
            $code
        }
        else {
            my $statement-list := do if nqp::istype($code, RakuAST::StatementList) {
                $code
            }
            else {
                my $statement := do if nqp::istype($code, RakuAST::Statement) {
                    $code
                }
                elsif nqp::istype($code, RakuAST::Expression) {
                    RakuAST::Statement::Expression.new(expression => $code)
                }
                else {
                    die "Cannot evaluate a $code.^name() node; expected a compilation unit, " ~
                        "statement list, statement, or expression";
                }
                RakuAST::StatementList.new($statement)
            }
            RakuAST::CompUnit.new:
                :outer-cu($*CU // RakuAST::CompUnit),
                :eval, :$statement-list,
                :comp-unit-name($filename // 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id)
        }

        # Perform symbol resolution, then compile to QAST and in turn bytecode.
        # When called from a BEGIN block the captured outer-context chain may
        # not yet be linked to the setting; in that case thread the setting
        # through from the currently-compiling CompUnit so lexicals like &say
        # can still be resolved at compile time.
        my $outer-setting := $*CU && nqp::istype($*CU, RakuAST::CompUnit)
            ?? $*CU.setting !! Mu;
        my $resolver := RakuAST::Resolver::EVAL.new(
            :context($eval_ctx), :global(GLOBAL), :setting($outer-setting));
        $comp-unit.begin($resolver);
        $comp-unit.check($resolver);
        if $resolver.has-compilation-errors {
            $resolver.produce-compilation-exception.throw;
        }
        my $from := $compiler.exists_stage('optimize') ?? 'optimize' !! 'qast';
        my $qast-cu := $comp-unit.IMPL-TO-QAST-COMP-UNIT;
        my $context := $comp-unit.context;
        if $context.is-nested {
            # Nested QAST::CompUnits skip deserialization_code emission, so
            # their static code refs never get scsetcode'd into the shared
            # SC. If EVAL returns a Sub the outer compile references,
            # outer's precomp serialization fails with "missing static
            # code ref" because the inner code ref's static_code isn't in
            # any SC. Keep the compunit around via :compunit_ok and run
            # the shared fixup loop to register each code ref in the
            # shared SC at the index stashed when the stub was added.
            my $precomp := $compiler.compile($qast-cu, :$from, :compunit_ok(1));
            $context.IMPL-FIXUP-COMPILED-CODEREFS(nqp::compunitcodes($precomp));
            # Run cleanup-tasks registered during begin/compile (mainly to
            # null @!compstuff on each Code object, which still holds the
            # IMPL-STUB-CODE closure). The normal compile pipeline does
            # this from the qast stage in Perl6::Compiler, but this EVAL
            # path bypasses the stage system.
            $comp-unit.cleanup;
            $compiled := $compiler.backend.compunit_mainline($precomp);
        }
        else {
            $compiled := $compiler.compile(:$from, $qast-cu);
        }
    }
    else {
        $code = nqp::istype($code,Blob) ?? $code.decode('utf8') !! $code.Str;

        my $?FILES   := $filename // 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;

        my $LANG := $context<%?LANG>:exists ?? $context<%?LANG> !! Nil;
        my $*INSIDE-EVAL := 1;
        $compiled := $compiler.compile:
            $code,
            :outer_ctx($eval_ctx),
            :global(GLOBAL),
            :language_version(nqp::getcomp('Raku').language_version),
            |(:optimize($_) with nqp::getcomp('Raku').cli-options<optimize>),
            |(%(:grammar($LANG<MAIN>), :actions($LANG<MAIN-actions>)) if $LANG);
    }

    if $check {
        Nil
    }
    else {
        nqp::forceouterctx(
          nqp::getattr($compiled,ForeignCode,'$!do'),$eval_ctx
        );
        $compiled()
    }
}

multi sub EVAL(
  $code,
  Str :$lang where { ($lang // '') eq 'Perl5' },
  PseudoStash :$context,
  Str() :$filename = Str,
  :$check,
) {
    if $check {
        NYI(":check on EVAL :from<Perl5>").throw;
    }
    else {
        my $?FILES :=
          $filename // 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;
        Rakudo::Internals.PERL5.run: nqp::istype($code,Blob)
          ?? Blob.new($code).decode('utf8-c8')
          !! $code.Str
    }
}

proto sub EVALFILE($, *%) {*}
multi sub EVALFILE($filename, :$lang = 'Raku', :$check) {
    EVAL slurp(:bin, $filename), :$lang, :$check, :context(CALLER::LEXICAL::), :$filename
}

# vim: expandtab shiftwidth=4
