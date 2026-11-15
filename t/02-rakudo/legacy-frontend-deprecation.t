use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 5;

# normalize line endings, for checkouts with CR LF (e.g. on Windows)
sub lf(Str:D $text --> Str:D) { $text.subst("\r\n", "\n", :g) }
sub lf-eq(Str:D $expected) { -> $got { lf($got) eq $expected } }

my $message = lf q:to/MESSAGE/;
    The legacy frontend, selected by setting RAKUDO_RAKUAST=0, is deprecated and
    will be removed in a future release. Please use the RakuAST frontend instead,
    by unsetting RAKUDO_RAKUAST or setting it to 1.
    If something works with the legacy frontend but not with the RakuAST frontend,
    we would love to hear about it! Please open an issue at
        https://github.com/rakudo/rakudo/issues/new
    so that it can be fixed before the legacy frontend is removed. Thank you!
    MESSAGE

my $footer = lf q:to/FOOTER/;
    Please contact the author to have these occurrences of deprecated code
    adapted, so that this message will disappear!
    FOOTER

sub report(*@reports, :$footer) {
    "Saw {+@reports} occurrence{'s' if @reports != 1} of deprecated code.\n"
      ~ ('=' x 80) ~ "\n"
      ~ @reports.map({ $_ ~ ('-' x 80) ~ "\n" }).join
      ~ ($footer // '')
}

my $legacy-report = report($message);

# run &code with only the given frontend / deprecation variables set
sub with-env(&code, :$rakuast, *%set) {
    temp %*ENV;
    %*ENV{$_}:delete for <RAKUDO_RAKUAST RAKUDO_NO_DEPRECATIONS RAKUDO_DEPRECATIONS_FATAL>;
    %*ENV<RAKUDO_RAKUAST> = $_ with $rakuast;
    %*ENV{.key} = .value for %set;
    code
}

with-env :rakuast<0>, {
    is-run 'print "alive"', :out<alive>, :err(lf-eq $legacy-report),
      'legacy frontend: reported when the program exits';
}

with-env {
    is-run 'print "alive"', :out<alive>,
      'RakuAST frontend: nothing reported';
}

with-env :rakuast<0>, :RAKUDO_NO_DEPRECATIONS<1>, {
    is-run 'print Deprecation.report.raku', :out<Nil>,
      'legacy frontend with RAKUDO_NO_DEPRECATIONS=1: not reported or recorded';
}

{
    my $dir = make-temp-dir;
    $dir.add('LegacyFrontendDeprecation.rakumod').spurt:
      'unit module LegacyFrontendDeprecation; sub answer is export { 42 }';

    with-env :rakuast<0>, {
        is-run "use lib $dir.absolute.raku(); use LegacyFrontendDeprecation; print answer",
          :out<42>, :err(lf-eq $legacy-report),
          'legacy frontend: reported once when precompiling a module';
    }
}

with-env {
    is-run 'sub foo { Rakudo::Deprecations.DEPRECATED: "meow" }; foo; print "alive"',
      :out<alive>, :err(lf-eq report(
            "Sub foo (from GLOBAL) seen at:\n  -e, line 1\nPlease use meow instead.\n",
            :$footer
        )),
      'other deprecations: reported unchanged, with footer';
}

# vim: expandtab shiftwidth=4
