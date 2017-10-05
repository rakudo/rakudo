# Takes a foreign code object and tries to make it feel somewhat like a Perl
# 6 one. Note that it doesn't have signature information we can know about.

my class ForeignCode does Callable { # declared in BOOTSTRAP
    # class ForeignCode
    #     has Code $!do;                # Code object we delegate to

    method arity() { self.signature.arity }

    method count() { self.signature.count }

    method signature(ForeignCode:D:) { (sub (|) { }).signature }

    method name() { (nqp::can($!do, 'name') ?? $!do.name !! nqp::getcodename($!do)) || '<anon>' }

    multi method gist(ForeignCode:D:) { self.name }

    multi method Str(ForeignCode:D:) { self.name }
}

my class Rakudo::Internals::EvalIdSource {
    my Int $count = 0;
    my Lock $lock = Lock.new;
    method next-id() {
        $lock.protect: { $count++ }
    }
}
proto sub EVAL(Cool $code, Str() :$lang = 'perl6', PseudoStash :$context, *%n) {
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
    $context := CALLER:: unless nqp::defined($context);
    my $eval_ctx := nqp::getattr(nqp::decont($context), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;
    my \mast_frames := nqp::hash();
    my $*CTXSAVE; # make sure we don't use the EVAL's MAIN context for the currently compiling compilation unit
    my $compiled;
    my $LANG := $context<%?LANG>;
    if !$LANG {
        $LANG := CALLERS::<%?LANG>;
    }
    if $LANG {
        # XXX
        my $grammar := $LANG<MAIN>;
        my $actions := $LANG<MAIN-actions>;
        $compiled := $compiler.compile(
            $code.Stringy,
            :outer_ctx($eval_ctx),
            :global(GLOBAL),
            :mast_frames(mast_frames),
            :grammar($grammar),
            :actions($actions),
        );
    }
    else {
        $compiled := $compiler.compile(
            $code.Stringy,
            :outer_ctx($eval_ctx),
            :global(GLOBAL),
            :mast_frames(mast_frames),
        );
    }
    if $*W and $*W.is_precompilation_mode() { # we are still compiling
        $*W.add_additional_frames(mast_frames);
    }
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}

multi sub EVAL(Cool $code, Str :$lang where { ($lang // '') eq 'Perl5' }, PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ (state $no)++;
    state $p5;
    unless $p5 {
        {
            my $compunit := $*REPO.need(CompUnit::DependencySpecification.new(:short-name<Inline::Perl5>));
            GLOBAL.WHO.merge-symbols($compunit.handle.globalish-package);
            CATCH {
                #X::Eval::NoSuchLang.new(:$lang).throw;
                note $_;
            }
        }
        $p5 = ::("Inline::Perl5").default_perl5;
    }
    $p5.run($code);
}

proto sub EVALFILE($, *%) {*}
multi sub EVALFILE($filename, :$lang = 'perl6') {
    EVAL slurp($filename), :$lang, :context(CALLER::);
}

# vim: ft=perl6 expandtab sw=4
