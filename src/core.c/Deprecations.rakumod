class Deprecation {
    has str $.file;         # file of the code that is deprecated
    has str $.type;         # type of code (sub/method etc.) that is deprecated
    has str $.package;      # package of code that is deprecated
    has str $.name;         # name of code that is deprecated
    has str $.alternative;  # alternative for code that is deprecated
    has %.callsites;        # places where called (file -> line -> count)
    has Version $.from;     # release version from which deprecated
    has Version $.removed;  # release version when will be removed
    has Str $.message;      # free-form text replacing the generated report

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
        if %DEPRECATIONS {
            my $message = "Saw {+%DEPRECATIONS} occurrence{ 's' if +%DEPRECATIONS != 1 } of deprecated code.\n";
            $message ~= ("=" x 80) ~ "\n";
            for %DEPRECATIONS.sort(*.key)>>.value>>.report -> $r {
                $message ~= $r;
                $message ~= ("-" x 80) ~ "\n";
            }

            %DEPRECATIONS = ();  # reset for new batches if applicable

            $message.chop
        }
        else {
            Nil
        }
    }
    multi method report (Deprecation:D:) {

        # a free-form message is reported as is
        if $!message -> $message {
            return $message.ends-with("\n") ?? $message !! "$message\n";
        }

        my $type    = $.type ?? "$.type " !! "";
        my $name    = $.name ?? "$.name " !! "";
        my $package = $.package ?? "(from $.package) " !! "";
        my $message = $type ~ $name ~ $package ~ "seen at:\n";
        for %.callsites.kv -> $file, $lines {
            if $file eq 'environment variable' {
                $message = "The $.name environment variable being set, support will be removed with v$.removed.\n";
                last;
            }
            else {
                $message ~= "  $file, line{ 's' if +$lines > 1 } {
                    $lines.keys.sort(*.Int).join(',')
                }\n";
                if $.from or $.removed {
                    $message ~= $.from
                      ?? "Deprecated since v$.from, will be removed"
                      !! "Will be removed";
                    $message ~= $.removed
                      ?? " with release v$.removed!\n"
                      !! " sometime in the future\n";
                }
            }
        }
        $message ~= "Please use $.alternative instead.\n";
        $message
    }
}

class Rakudo::Deprecations {

    my %DEPRECATIONS := Deprecation.DEPRECATIONS;

    my $ver;
    method DEPRECATED(
      $alternative, $from?, $removed?,
      :$up = 1, :$what, :$file, :$line, Bool :$lang-vers, Str :$message
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
          $bt[ my $index = $bt.next-interesting-index(1, :named, :setting, :reveal) // 0 ];
#?endif
#?if js
          $bt[ my $index = $bt.next-interesting-index(2, :named, :setting, :reveal) // 0 ];
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
            :removed($vremoved),
            :$message )
          !! Deprecation.new(
            file    => $deprecated.file,
            type    => $deprecated.subtype.tc,
            package => try { $deprecated.package.^name } // 'unknown',
            name    => $deprecated.subname,
            :$alternative,
            :from($vfrom),
            :removed($vremoved),
            :$message,
        );
        $dep = %DEPRECATIONS{$dep.WHICH} //= $dep;

        state $fatal = %*ENV<RAKUDO_DEPRECATIONS_FATAL>;
        die $dep.report if $fatal;

        # update callsite
        ++$dep.callsites{$file // $callsite.file.IO}{$line // $callsite.line};
    }

    # Called once per process at startup, from core_epilogue.rakumod
    method DEPRECATE-LEGACY-FRONTEND(--> Nil) is implementation-detail {
        self.DEPRECATED(
          'the RakuAST frontend',
          :what('legacy frontend'),
          :file('environment variable'),
          :message((
            'The legacy frontend, selected by setting RAKUDO_RAKUAST=0, is deprecated and',
            'will be removed in a future release. Please use the RakuAST frontend instead,',
            'by unsetting RAKUDO_RAKUAST or setting it to 1.',
            'If something works with the legacy frontend but not with the RakuAST frontend,',
            'we would love to hear about it! Please open an issue at',
            '    https://github.com/rakudo/rakudo/issues/new',
            'so that it can be fixed before the legacy frontend is removed. Thank you!',
          ).join("\n"))
        ) if nqp::iseq_s(
               nqp::ifnull(nqp::gethllsym('Raku','COMPILER-FRONTEND'),''),
               'legacy'
             )
          # not while compiling CORE.d/CORE.e, which runs this mainline
          && nqp::isfalse(
               nqp::ifnull(nqp::getlexdyn('$*COMPILING_CORE_SETTING'),0)
             )
          # not in precomp workers, whose stderr the parent re-prints
          && !(%*ENV<RAKUDO_PRECOMP_WITH>:exists)
          # not when silenced, so Deprecation.report doesn't show it either
          && !%*ENV<RAKUDO_NO_DEPRECATIONS>;
    }
}

END {
    # footer only if some lack a message; check before report resets them
    my $footer := ?Deprecation.DEPRECATIONS.values.first({ !.message });

    if Deprecation.report -> $message {
        unless %*ENV<RAKUDO_NO_DEPRECATIONS> {
            note $message;   # q:to/TEXT/ doesn't work in settings
            note 'Please contact the author to have these occurrences of deprecated code
adapted, so that this message will disappear!' if $footer;
        }
    }
}

sub DEPRECATED(|c) is hidden-from-backtrace is implementation-detail {
    Rakudo::Deprecations.DEPRECATED(|c)
}

# vim: expandtab shiftwidth=4
