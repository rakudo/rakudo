{
    my %OPTS;

    my $O := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'DELIMITED-OPTIONS');
    if nqp::defined($O) {
        if nqp::ishash($O) {
            my Mu $iterator := nqp::iterator($O);
            while $iterator {
                my $cur := nqp::shift($iterator);
                my $key := nqp::p6box_s(nqp::iterkey_s($cur));
                my $value := nqp::iterval($cur);
                if nqp::ishash($value) {
                    %OPTS{$key} := [];
                    for $value {
                        %OPTS{$key}.push: nqp::p6box_s($_);
                    }
                }
                else {
                    %OPTS{$key} := $value;
                }
            }
        }
    }

    PROCESS::<%OPTS> := %OPTS;
}
