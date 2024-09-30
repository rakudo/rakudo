#!/usr/bin/env raku
use v6;

my $template = q:to/END_TEMPLATE/;
# Announce: Rakudo compiler, Release #«release-num» («release-name»)

On behalf of the Rakudo development team, I’m very happy to announce the
«month» «year» release of Rakudo #«release-num». Rakudo is an implementation of
the Raku[^1] language.

The source tarball for this release is available from
<https://rakudo.org/files/rakudo>.
Pre-compiled archives will be available shortly.

New in «release-name»:
«changes»

The following people contributed to this release:

«contributors»

This release implements 6.c and 6.d versions of the Raku specification.
6.c version of the language is available if you use the `use v6.c`
version pragma, otherwise 6.d is the default.

Upcoming releases in «upcoming-year» will include new functionality that is not
part of 6.c or 6.d specifications, available with a lexically scoped
pragma. Our goal is to ensure that anything that is tested as part of
6.c and 6.d specifications will continue to work unchanged. There may
be incremental spec releases this year as well.

If you would like to contribute or get more information, visit
<https://raku.org>, <https://rakudo.org/community>, ask on the
<perl6-compiler@perl.org> mailing list, or ask on IRC #raku on Libera.

Additionally, we invite you to make a donation to The Perl & Raku Foundation
to sponsor Raku development: <https://donate.perlfoundation.org/>
(put “Raku Core Development Fund” in the ‘Purpose’ text field)

The next release of Rakudo (#«next-release-num»), is tentatively scheduled for «next-release-date».

A list of the other planned release dates is available in the
“docs/release_guide.pod” file.

The development team appreciates feedback! If you’re using Rakudo, do
get back to us. Questions, comments, suggestions for improvements, cool
discoveries, incredible hacks, or any other feedback – get in touch with
us through (the above-mentioned) mailing list or IRC channel. Enjoy!

Please note that recent releases have known issues running on the JVM.
We are working to get the JVM backend working again but do not yet have
an estimated delivery date.

[^1]: See <https://raku.org/>
END_TEMPLATE

my @ENGLISH-MONTHS = flat Any,
    <January February March April
     May June July August September
     October November December>;

sub prompt-with-default(Str $prompt, Str $default-value) returns Str {
    my $value = prompt("$prompt [$default-value]");
    if $value.defined && $value == '' {
        $value = $default-value;
    }

    $value
}

sub find-release-num() {
    my $offset = 14; # 14 releases aren't tagged with a year-month or
                     # year.month pattern
    my $proc = run :out, :cwd($*RAKUDO_DIR),
        'git', 'tag', '--list', '????.??', '--list', '????-??';

    my $num-releases = $proc.out.slurp-rest(:close).lines.elems;
    $num-releases += $offset;

    $num-releases + 1
}

sub find-changes() {
    my $changelog = $*RAKUDO_DIR.child: 'docs/ChangeLog';
    fail "Couldn't find $changelog" unless $changelog.e;

    my $today = Date.today;
    my $last-month = $today.earlier(:1month);
    my $current-month = $today.month;
    my $current-year = $today.year;

    my $start = sprintf('%04d.%02d', $today.year, $today.month);
    my $end = sprintf('%04d.%02d', $last-month.year, $last-month.month);

    my @lines = do
        gather for $changelog.lines -> $line {
            if $line ~~ m:s/New in "$start"/ ^ff^ $line ~~ m:s/New in "$end"/ {
                take $line;
            }
        };
    @lines ?? @lines.join("\n") !! '(no changes yet)'
}

sub find-release-name() {
    my $today = Date.today;
    sprintf('%04d.%02d', $today.year, $today.month)
}

sub find-contributors(
    :$last_release, :$rakudo, :$doc, :$nqp, :$moar, :$roast,
) {
    my $proc = run :out, :!err, $*EXECUTABLE,
            $*RAKUDO_DIR.child('tools/contributors.raku'),
                "--rakudo=$rakudo",
                "--doc=$doc",
                "--nqp=$nqp",
                "--moar=$moar",
                "--roast=$roast",
                |($last_release if $last_release);

    $proc.out.slurp-rest(:close).lines[1..*]».trim-trailing.join: "\n";
}

sub find-next-release-date() {
    my $release_guide = $*RAKUDO_DIR.child: 'docs/release_guide.pod';
    fail "Couldn't find $release_guide" unless $release_guide.e;

    my @lines = $release_guide.lines;
    my $this-release = sprintf('%04d-%02d', Date.today.year, Date.today.month);
    my $this-release-index = @lines.first: /^ \s*"$this-release"/, :k;
    my $next-release = @lines[$this-release-index];
    if $next-release ~~ /^ \s* (\d\d\d\d '-' \d\d '-' \d\d)/ {
        ~$0
    } else {
        fail "Could not find next release date in a correct format";
    }
}

sub MAIN (
    $last_release? is copy,
    :$rakudo = '.',
    :$doc    = '../doc',
    :$nqp    = 'nqp',
    :$moar   = 'nqp/MoarVM',
    :$roast  = 't/spec',
) {
    my $*RAKUDO_DIR = $rakudo.IO;
    my %template-vars;

    %template-vars<release-num> = find-release-num();
    %template-vars<next-release-num> = %template-vars<release-num> + 1;
    %template-vars<release-name> = find-release-name();
    %template-vars<year> = Date.today.year;
    %template-vars<month> = @ENGLISH-MONTHS[Date.today.month];
    %template-vars<changes> = find-changes();
    %template-vars<contributors> = find-contributors(
        :$last_release, :$rakudo, :$doc, :$nqp, :$moar, :$roast,
    );
    %template-vars<next-release-date> = find-next-release-date();
    %template-vars<upcoming-year> = %template-vars<next-release-date>.split(‘-’)[0];

    my $content = $template.subst(/'«' $<ident>=[[\w| '-']+] '»'/, -> $/ {
        my $key = ~$<ident>;
        my $value = %template-vars{$key};

        die "Undefined template variable '$key'" unless $value;
        $value
    }, :g);

    say $content.chomp;
}
