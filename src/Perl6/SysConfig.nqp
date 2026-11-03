class Perl6::SysConfig is HLL::SysConfig {
    has %!rakudo-build-config;
    has $!rakudo-home;

    method new(%rakudo-build-config) {
        my $obj := nqp::create(self);
        $obj.BUILD(%rakudo-build-config);
        $obj
    }

    # A naive implementation of path normalization.
    # Resolves '..' and '.' path components. It does not resolve symlinks.
    # It does not understand UNC paths. It misses loads of cornercases I'm
    # blissfully unaware of.
    sub normalize-rakudo-home($path, $sep) {
        my $all-slash := nqp::join('/', nqp::split('\\', $path));

        my @new-comps;
        for nqp::split('/', $all-slash) -> $component {
            if $component eq '.' {
                next;
            }
            elsif $component eq '..' {
                if nqp::elems(@new-comps) == 1 &&
                        (@new-comps[0] eq ''                   # path starts with "/.."
                      || nqp::substr(@new-comps[0], 1) eq ':') # path starts with "C:/.."
                {
                    # Updir-ing beyond the root is a noop. Thus we'll just drop it.
                }
                else {
                    # Will die if @new-comps is empty. This would mean the path
                    # starts with "../". This should not happen as we don't
                    # expect relative paths here.
                    @new-comps.pop;
                }
            }
            else {
                @new-comps.push: $component;
            }
        }

        if !nqp::elems(@new-comps)               # path is ""
        || (@new-comps[0] ne ''                  # path doesn't start with "/"
        && nqp::substr(@new-comps[0], 1) ne ':') # path doesn't start with "C:/"
        {
            die("Invalid relative rakudo-home path found: $path");
        }

        nqp::join($sep, @new-comps)
    }

    method BUILD(%rakudo-build-config) {
        self.build-hll-sysconfig();

        %!rakudo-build-config := %rakudo-build-config;

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

        $!rakudo-home := normalize-rakudo-home($!rakudo-home, self.path-sep);
    }

    method rakudo-build-config() { %!rakudo-build-config }

    method rakudo-home() { $!rakudo-home }
}
