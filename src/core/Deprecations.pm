my %DEPRECATIONS; # where we keep our deprecation info

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

        my $message = "Saw {+%DEPRECATIONS} occurrence{ 's' if +%DEPRECATIONS != 1 } of deprecated code.\n";
        $message ~= ("=" x 80) ~ "\n";
        for %DEPRECATIONS.sort(*.key)>>.value>>.report -> $r {
            $message ~= $r;
            $message ~= ("-" x 80) ~ "\n";
        }

        %DEPRECATIONS = ();  # reset for new batches if applicable

        $message.chop;
    }
    multi method report (Deprecation:D:) {
        my $type    = $.type ?? "$.type " !! "";
        my $name    = $.name ?? "$.name " !! "";
        my $package = $.package ?? "(from $.package) " !! "";
        my $message = $type ~ $name ~ $package ~ "seen at:\n";
        for %.callsites.kv -> $file, $lines {
            $message ~=
              "  $file, line{ 's' if +$lines > 1 } {$lines.keys.sort.join(',')}\n";
            if $.from or $.removed {
                $message ~= $.from
                  ?? "Deprecated since v$.from, will be removed"
                  !! "Will be removed";
                $message ~= $.removed
                  ?? " with release v$.removed!\n"
                  !! " sometime in the future\n";
            }
        }
        $message ~= "Please use $.alternative instead.\n";
        $message;
    }
}

sub DEPRECATED($alternative,$from?,$removed?,:$up = 1,:$what,:$file,:$line) {

    # not deprecated yet
    state $version = $*PERL.compiler.version;
    my Version $vfrom;
    my Version $vremoved;
    if $from {
        $vfrom = Version.new($from);
        return unless $version cmp $vfrom === More;
    }
    $vremoved = Version.new($removed) if $removed;

    my $bt = Backtrace.new;
    my $deprecated =
      $bt[ my $index = $bt.next-interesting-index(2, :named, :setting) ];

    if $up ~~ Whatever {
        $index = $_ with $bt.next-interesting-index($index, :noproto);
    }
    else {
        $index = $_
          with $bt.next-interesting-index($index, :noproto, :setting)
          for ^$up;
    }
    my $callsite = $bt[$index];

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
        package => try { $deprecated.package.^name } // 'unknown',
        name    => $deprecated.subname,
        :$alternative,
        :from($vfrom),
        :removed($vremoved),
    );
    $dep = %DEPRECATIONS{$dep.WHICH} //= $dep;

    state $fatal = %*ENV<RAKUDO_DEPRECATIONS_FATAL>;
    die $dep.report if $fatal;

    # update callsite
    $dep.callsites{$file // $callsite.file.IO}{$line // $callsite.line}++;
}

END {
    unless %*ENV<RAKUDO_NO_DEPRECATIONS> {
        if Deprecation.report -> $message {
            note $message;   # q:to/TEXT/ doesn't work in settings
            note 'Please contact the author to have these occurrences of deprecated code
adapted, so that this message will disappear!';
        }
    }
}

# vim: ft=perl6 expandtab sw=4
