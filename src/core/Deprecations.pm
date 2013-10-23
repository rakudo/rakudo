
my %DEPRECATIONS; # where we keep our deprecation info

class Deprecation {
    has $.file;         # file of the code that is deprecated
    has $.type;         # type of code (sub/method etc.) that is deprecated
    has $.package;      # package of code that is deprecated
    has $.name;         # name of code that is deprecated
    has $.alternative;  # alternative for code that is deprecated
    has %.callsites;    # places where called (file -> line -> count)

    method WHICH { ($!file,$!type,$!package,$!name).join(':') }

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
        my $message = "$.type $.name (from $.package) called at:\n";
        for %.callsites.kv -> $file, $lines {
            $message ~=
              "  $file, line{ 's' if +$lines > 1 } {$lines.keys.join(',')}\n";
        }
        $message ~= "Please use $.alternative instead.\n";
        $message;
    }
}

sub DEPRECATED ($alternative) {

    my $bt = Backtrace.new;
    my $deprecated = $bt[ my $index = $bt.next-interesting-index(2, :named) ];
    my $callsite   = $bt[$index = $bt.next-interesting-index($index, :noproto)];

    # get object, existing or new
    my $what = Deprecation.new(
      file    => $deprecated.file,
      type    => $deprecated.subtype.tc,
      package => $deprecated.package.HOW.name($deprecated),
      name    => $deprecated.subname,
      :$alternative,
    );
    $what = %DEPRECATIONS{$what.WHICH} //= $what;

    # update callsite
    $what.callsites{$callsite.file}{$callsite.line}++;
} 

END {
    if my $message = Deprecation.report {
        my Mu $err := nqp::getstderr();
        my sub say2 ($s) { nqp::printfh($err, "$s\n") }

        say2 $message;
        say2 "Please contact the author to have these calls to deprecated code adapted,";
        say2 "so that this message will disappear!";
    }
}
