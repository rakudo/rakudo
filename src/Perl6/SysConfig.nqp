class Perl6::SysConfig is HLL::SysConfig {
    has %!rakudo-build-config;
    has $!rakudo-home;

    method BUILD() {
        %!rakudo-build-config := nqp::hash();

        # Determine Rakudo home.
        my $execname := nqp::execname();
        my $install-dir := $execname eq ''
            ?? %!rakudo-build-config<prefix>
            !! nqp::substr($execname, 0, nqp::rindex($execname, self.path-sep, nqp::rindex($execname, self.path-sep) - 1));

        $!rakudo-home := nqp::getenvhash()<RAKUDO_HOME>
            // nqp::getenvhash()<PERL6_HOME>
            // %!rakudo-build-config<static-rakudo-home>
            || $install-dir ~ '/share/perl6';
        if nqp::substr($!rakudo-home, nqp::chars($!rakudo-home) - 1) eq self.path-sep {
            $!rakudo-home := nqp::substr($!rakudo-home, 0, nqp::chars($!rakudo-home) - 1);
        }
    }

    method rakudo-build-config() { %!rakudo-build-config }

    method rakudo-home() { $!rakudo-home }
}
