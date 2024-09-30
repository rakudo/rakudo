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
        my $resolver := RakuAST::Resolver::EVAL.new(:context($eval_ctx), :global(GLOBAL));
        $comp-unit.begin($resolver);
        $comp-unit.check($resolver);
        if $resolver.has-compilation-errors {
            $resolver.produce-compilation-exception.throw;
        }
        my $from := $compiler.exists_stage('optimize') ?? 'optimize' !! 'qast';
        $compiled := $compiler.compile: :$from, $comp-unit.IMPL-TO-QAST-COMP-UNIT;
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
