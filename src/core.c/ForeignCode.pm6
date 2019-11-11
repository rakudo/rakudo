# Takes a foreign code object and tries to make it feel somewhat like a Perl
# 6 one. Note that it doesn't have signature information we can know about.

my class ForeignCode
  does Callable
  does Rakudo::Internals::ImplementationDetail
{ # declared in BOOTSTRAP
    # class ForeignCode
    #     has Code $!do;                # Code object we delegate to

    method arity() { self.signature.arity }

    method count() { self.signature.count }

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
  $code is copy where Blob|Cool|Callable,
  Str()       :$lang = 'perl6',
  PseudoStash :$context,
  Str()       :$filename = Str,
  Bool()      :$check = False,
  *%_
) {
    die "EVAL() in Perl 6 is intended to evaluate strings, did you mean 'try'?"
      if nqp::istype($code,Callable);

# TEMPORARY HACK
$lang = 'perl6' if $lang eq 'Raku';

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
    $code = nqp::istype($code,Blob) ?? $code.decode('utf8') !! $code.Str;

    $context := CALLER:: unless nqp::defined($context);
    my $eval_ctx := nqp::getattr(nqp::decont($context), PseudoStash, '$!ctx');
    my $?FILES   := $filename // 'EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;
    my \mast_frames := nqp::hash();
    my $*CTXSAVE; # make sure we don't use the EVAL's MAIN context for the
                  # currently compiling compilation unit

    my $LANG := $context<%?LANG>:exists
                    ?? $context<%?LANG>
                    !! (CALLERS::<%?LANG>:exists ?? CALLERS::<%?LANG> !! Nil);
    my $*INSIDE-EVAL := 1;
    my $compiled := $compiler.compile:
        $code,
        :outer_ctx($eval_ctx),
        :global(GLOBAL),
        :mast_frames(mast_frames),
        :language_version(nqp::getcomp('perl6').language_version),
        |(:optimize($_) with nqp::getcomp('perl6').cli-options<optimize>),
        |(%(:grammar($LANG<MAIN>), :actions($LANG<MAIN-actions>)) if $LANG);

    if $check {
        Nil
    }
    else {
        $*W.add_additional_frames(mast_frames)
          if $*W and $*W.is_precompilation_mode; # we are still compiling
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
  Bool() :$check = False,
) {
    if $check {
        X::NYI.new(feature => ":check on EVAL :from<Perl5>").throw;
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
multi sub EVALFILE($filename, :$lang = 'perl6', Bool() :$check = False) {
    EVAL slurp(:bin, $filename), :$lang, :$check, :context(CALLER::), :$filename
}

# vim: ft=perl6 expandtab sw=4
