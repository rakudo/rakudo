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

    # Compile a RakuAST comp-unit the rest of the way to a code object.
    # IMPL-TO-QAST-COMP-UNIT runs outside the compiler's qast stage here,
    # so bind the dynamic that stage provides, and run the comp-unit's
    # cleanup even when a later stage throws.
    my sub compile-rakuast-comp-unit($comp-unit) {
        LEAVE $comp-unit.cleanup;
        my $*COMPILING_CORE_SETTING := 0;
        my $qast-cu := $comp-unit.IMPL-TO-QAST-COMP-UNIT;
        my $eval-context := $comp-unit.context;
        # Run the qast-stage SC bookkeeping that compiling from QAST skips.
        my $precomp := $compiler.compile($qast-cu, :from($compiler.qast-stage), :compunit_ok(1));
        $eval-context.IMPL-FIXUP-COMPILED-CODEREFS(nqp::compunitcodes($precomp));
        $compiler.backend.compunit_mainline($precomp)
    }

    if nqp::istype($code, RakuAST::Node) {
        # Wrap as required to get compilation unit. A unit that already
        # exists (rather than the wrapper created below, whose creation
        # pushes its fresh SC) needs its SC put back on the compiling-SC
        # stack, which the backend pops after each non-nested compile.
        my $reused-comp-unit := False;
        my $comp-unit := do if nqp::istype($code, RakuAST::CompUnit) {
            $reused-comp-unit := True;
            $code
        }
        # A statement list from Cool.AST was already begun as part of its
        # parse's compilation unit; begin-time state such as an implicit
        # proto or require's symbol captures lives on that unit's mainline
        # scope, not in the statement list. Compile that unit, as wrapping
        # the (begun, so begin cannot run again) statements in a fresh one
        # would lose the state. Only when the statement list is still the
        # unit's own: one transplanted into a user-built tree is theirs.
        elsif nqp::istype($code, RakuAST::StatementList)
          && nqp::isconcrete(my $origin := $code.IMPL-ORIGIN-COMP-UNIT)
          && nqp::eqaddr(nqp::decont($origin.statement-list), nqp::decont($code)) {
            $reused-comp-unit := True;
            $origin
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
        # Run the optimize phase the comp-unit action would run for parsed
        # code, so a synthetic AST compiles the same way, honouring the
        # process's optimize option just as the string form forwards it
        # below. Note the phase rewrites the tree being EVALled in place,
        # just as begin and check resolve and annotate it in place.
        my $optimize-option = nqp::getcomp('Raku').cli-options<optimize> // '';
        $comp-unit.optimize($resolver)
            unless $optimize-option eq 'off' || $optimize-option eq '0';
        $comp-unit.IMPL-PUSH-COMPILING-SC if $reused-comp-unit;
        $compiled := compile-rakuast-comp-unit($comp-unit);
    }
    else {
        $code = nqp::istype($code,Blob) ?? $code.decode('utf8') !! $code.Str;

        my $?FILES   := $filename // 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;

        my $LANG := $context<%?LANG>:exists ?? $context<%?LANG> !! Nil;
        my $*INSIDE-EVAL := 1;
        # The RakuAST frontend (the only frontend with a qast stage) stops
        # at the AST so the comp-unit can take the same finishing path as
        # AST EVAL, fixing up code objects to survive an enclosing
        # compilation's precomp and wrapping nested EVAL output.
        my $rakuast-frontend := $compiler.exists_stage('qast');
        my $result := $compiler.compile:
            $code,
            :outer_ctx($eval_ctx),
            :global(GLOBAL),
            :language_version(nqp::getcomp('Raku').language_version),
            |(%(:target('ast'), :compunit_ok(1)) if $rakuast-frontend),
            |(:optimize($_) with nqp::getcomp('Raku').cli-options<optimize>),
            |(%(:grammar($LANG<MAIN>), :actions($LANG<MAIN-actions>)) if $LANG);
        $compiled := $rakuast-frontend
            ?? compile-rakuast-comp-unit($result)
            !! $result;
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
