class Deprecation {
    has str $.file;         # file of the code that is deprecated
    has str $.type;         # type of code (sub/method etc.) that is deprecated
    has str $.package;      # package of code that is deprecated
    has str $.name;         # name of code that is deprecated
    has str $.alternative;  # alternative for code that is deprecated
    has %.callsites;        # places where called (file -> line -> count)
    has Version $.from;     # release version from which deprecated
    has Version $.removed;  # release version when will be removed

    my %DEPRECATIONS; # where we keep our deprecation info
    method DEPRECATIONS() is raw is implementation-detail { %DEPRECATIONS }

    multi method WHICH (Deprecation:D: --> ValueObjAt:D) {
        my $which := nqp::list_s("Deprecation");
        nqp::push_s($which,$!file    || "");
        nqp::push_s($which,$!type    || "");
        nqp::push_s($which,$!package || "");
        nqp::push_s($which,$!name    || "");
        nqp::box_s(
          nqp::join("|",$which),
          ValueObjAt
        )
    }

    proto method report (|) {*}
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

class Rakudo::Deprecations {

    my %DEPRECATIONS := Deprecation.DEPRECATIONS;

    my $ver;
    method DEPRECATED(
      $alternative, $from?, $removed?,
      :$up = 1, :$what, :$file, :$line, Bool :$lang-vers
    ) is implementation-detail {
        $ver //= $*RAKU.compiler.version;
        my $version = $lang-vers ?? nqp::getcomp('Raku').language_version !! $ver;
        # if $lang-vers was given, treat the provided versions as language
        # versions, rather than compiler versions. Note that we can't
        # `state` the lang version (I think) because different CompUnits
        # might be using different versions.

        my Version $vfrom;
        my Version $vremoved;
        $from && nqp::iseq_i($version cmp ($vfrom = Version.new: $from), -1)
              && return; # not deprecated yet;
        $vremoved = Version.new($removed) if $removed;

        my $bt = Backtrace.new;
        my $deprecated =
#?if !js
          $bt[ my $index = $bt.next-interesting-index(1, :named, :setting) // 0 ];
#?endif
#?if js
          $bt[ my $index = $bt.next-interesting-index(2, :named, :setting) // 0 ];
#?endif

        if $up ~~ Whatever {
            $index = $_ with $bt.next-interesting-index($index, :noproto);
        }
        else {
            for ^$up -> $level {
                $index = $_
                  with $bt.next-interesting-index($index, :noproto, :setting)
            }
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
        ++$dep.callsites{$file // $callsite.file.IO}{$line // $callsite.line};
    }
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

sub DEPRECATED(|c) is hidden-from-backtrace is implementation-detail {
    Rakudo::Deprecations.DEPRECATED(|c)
}

# vim: expandtab shiftwidth=4
