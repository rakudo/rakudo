
my %DEPRECATIONS; # where we keep our deprecation info

class Deprecation {
    has $.file;         # file of the code that is deprecated
    has $.type;         # type of code (sub/method etc.) that is deprecated
    has $.package;      # package of code that is deprecated
    has $.name;         # name of code that is deprecated
    has $.alternative;  # alternative for code that is deprecated
    has %.callsites;    # places where called (file -> line -> count)

    method WHICH { ($!file||"",$!type||"",$!package||"",$!name).join(':') }

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
              "  $file, line{ 's' if +$lines > 1 } {$lines.keys.join(',')}\n";
        }
        $message ~= "Please use $.alternative instead.\n";
        $message;
    }

    class Obsolete is Str {
        has $!name;
        has $!value;
        has $!instead;

        submethod BUILD (:$!name, :$!value, :$!instead) {}

        method Str  { DEPRECATED( $!instead, :up(2), :what($!name) ); $!value }
        method gist { DEPRECATED( $!instead, :up(2), :what($!name) ); $!value }
    }

    # system variable deprecations
    method obsolete (|c) { Obsolete.new(|c) }
}

sub DEPRECATED ($alternative, :$up = 1, :$what ) {

    my $bt = Backtrace.new;
    my $deprecated = $bt[ my $index = $bt.next-interesting-index(2, :named) ];
    my $callsite;
    $callsite = $bt[$index = $bt.next-interesting-index($index, :noproto)]
      for ^$up;

    # get object, existing or new
    my $dep = $what
      ?? Deprecation.new( :name($what), :$alternative )
      !! Deprecation.new(
        file    => $deprecated.file,
        type    => $deprecated.subtype.tc,
        package => try {
            $deprecated.package.HOW.name($deprecated)
        } // 'unknown',
        name    => $deprecated.subname,
        :$alternative );
    $dep = %DEPRECATIONS{$dep.WHICH} //= $dep;

    # update callsite
    $dep.callsites{$callsite.file}{$callsite.line}++;
} 

END {
    if my $message = Deprecation.report {
        note $message;
        note "Please contact the author to have these calls to deprecated code adapted,";
        note "so that this message will disappear!";
    }
}

# vim: ft=perl6 expandtab sw=4
