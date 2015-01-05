
my %DEPRECATIONS; # where we keep our deprecation info

class Obsolete is Str {
    has $!name;
    has $!value;
    has $!instead;
    has $!from;
    has $!removed;

    submethod BUILD (:$!name, :$!value, :$!instead, :$!from, :$!removed) { }

    method Str  { DEPRECATED( $!instead, $!from, $!removed, :up(2), :what($!name) ); $!value }
    method gist { DEPRECATED( $!instead, $!from, $!removed, :up(2), :what($!name) ); $!value }
}

class Deprecation {
    has $.file;         # file of the code that is deprecated
    has $.type;         # type of code (sub/method etc.) that is deprecated
    has $.package;      # package of code that is deprecated
    has $.name;         # name of code that is deprecated
    has $.alternative;  # alternative for code that is deprecated
    has %.callsites;    # places where called (file -> line -> count)
    has Version $.from;    # release version from which deprecated
    has Version $.removed; # release version when will be removed

    multi method WHICH (Deprecation:D:) {
        ($!file||"",$!type||"",$!package||"",$!name).join(':');
    }

    proto method report (|) { * }
    multi method report (Deprecation:U:) {
        return Nil unless %DEPRECATIONS;

        my $message = "Saw {+%DEPRECATIONS} call{ 's' if +%DEPRECATIONS != 1 } to deprecated code during execution.\n";
        $message ~= ("=" x 80) ~ "\n";
        for %DEPRECATIONS.values -> $d {
            $message ~= $d.report;
            $message ~= ("-" x 80) ~ "\n";
        }

        %DEPRECATIONS = ();  # reset for new batches if applicable

        $message.chop;
    }
    multi method report (Deprecation:D:) {
        my $type    = $.type ?? "$.type " !! "";
        my $name    = $.name ?? "$.name " !! "";
        my $package = $.package ?? "(from $.package) " !! "";
        my $message = $type ~ $name ~ $package ~ "called at:\n";
        for %.callsites.kv -> $file, $lines {
            $message ~=
              "  $file, line{ 's' if +$lines > 1 } {$lines.keys.sort.join(',')}\n";
            $message ~=
              "Deprecated since v$.from, will be removed {$.removed
                ?? 'with release v' ~ $.removed ~ '!'
                !! 'at some time in the future'
              }\n" if $.from;
        }
        $message ~= "Please use $.alternative instead.\n";
        $message;
    }

    # system variable deprecations
    method obsolete (|c) { Obsolete.new(|c) }
}

sub DEPRECATED ( $alternative, $from?, $removed?, :$up = 1, :$what ) {

    # not deprecated yet
    state $version = $*PERL.compiler.version;
    my Version $vfrom;
    my Version $vremoved;
    if $from {
        $vfrom = Version.new($from);
        return if $version cmp $vfrom ~~ Less | Same; # can be better?
    }
    $vremoved = Version.new($removed) if $removed;

    my $bt = Backtrace.new;
    my $deprecated = $bt[ my $index = $bt.next-interesting-index(2, :named) ];
    my $callsite;
    $callsite = $bt[$index = $bt.next-interesting-index($index, :noproto)]
      for ^$up;

    # get object, existing or new
    my $dep = $what
      ?? Deprecation.new(
        :name($what),
        :$alternative,
        :from($vfrom),
        :removed($vremoved) )
      !! Deprecation.new(
        file    => $deprecated.file,
        type    => $deprecated.subtype.tc,
        package => try {
            $deprecated.package.HOW.name($deprecated)
        } // 'unknown',
        name    => $deprecated.subname,
        :$alternative,
        :from($vfrom),
        :removed($vremoved),
    );
    $dep = %DEPRECATIONS{$dep.WHICH} //= $dep;

    # update callsite
    $dep.callsites{$callsite.file}{$callsite.line}++;
}

END {
    unless %*ENV<RAKUDO_NO_DEPRECATIONS> {
        if Deprecation.report -> $message {
            note $message;   # q:to/TEXT/ doesn't work in settings
            note 'Please contact the author to have these calls to deprecated code adapted,
so that this message will disappear!

Please note that *ALL* deprecated features will be removed at the release
of Perl 6.0.0 (expected some time in 2015).'
        }
    }
}

# vim: ft=perl6 expandtab sw=4
