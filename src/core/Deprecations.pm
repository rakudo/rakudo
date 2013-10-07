
my %DEPRECATIONS; # where we keep our deprecation info

class Deprecation {
    has $.file;         # file of the code that is deprecated
    has $.type;         # type of code (sub/method etc.) that is deprecated
    has $.package;      # package of code that is deprecated
    has $.name;         # name of code that is deprecated
    has $.alternative;  # alternative for code that is deprecated
    has %.callsites;    # places where called (file -> line -> count)

    method WHICH { ($!file,$!type,$!package,$!name).join(':') }
}

sub DEPRECATED ($alternative) {

    my $bt = Backtrace.new;
    my $deprecated = $bt[ my $index = $bt.next-interesting-index(2,:named) ];
    my $callsite   = $bt[ $bt.next-interesting-index($index) ];

    # get object, existing or new
    my $what = Deprecation.new(
      file    => $deprecated.file,
      type    => $deprecated.subtype.tc,
      package => $deprecated.package.^name,
      name    => $deprecated.subname,
      :$alternative,
    );
    $what = %DEPRECATIONS{$what.WHICH} //= $what;

    # update callsite
    $what.callsites{$callsite.file}{$callsite.line}++;
} 

END {
    if %DEPRECATIONS {
        my Mu $err := nqp::getstderr();
        my sub say2 ($s) { nqp::printfh($err, "$s\n") }

        say2 "Saw {+%DEPRECATIONS} call{ 's' if +%DEPRECATIONS != 1 } to deprecated code during execution.";
        say2 "=" x 80;

        for %DEPRECATIONS.values -> $d {
            say2 "{$d.type} {$d.name} (from {$d.package}) called at:";
            for $d.callsites.kv -> $file, $lines {
                say2 "  $file, line{ 's' if +$lines > 1 } {$lines.keys.join(',')}";
            }
            say2 "Please use {$d.alternative} instead.";
            say2 "-" x 80;
        }

        say2 "Please contact the author to have these calls to deprecated";
        say2 "code adapted, so that this message will disappear!";
    }
}
