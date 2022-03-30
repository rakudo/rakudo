class CompUnit::Repository::Spec {
    has Str:D $.short-id is required;
    has Str:D $.Str      is required;
    has Str:D $.path = "";
    has %.options;

    method from-string(str $spec, str :$default-short-id = 'file') {

# Examples:
#  perl5#
#  inst#/Foo/Bar/rakudo/gen/build_rakudo_home/core
#  CompUnit::Repository::Staging#name(core)#/Foo/Bar/rakudo/install/share/perl6/core

        if nqp::chars($spec) {                 # at least some kind of spec
            my $parts := nqp::split("#",$spec);
            if nqp::elems($parts) == 1 {       # no # found
                my str $path = PROCESS::<$SPEC>.canonpath($spec);
                self.new: :short-id($default-short-id), :$path,
                  Str => $default-short-id ~ '#' ~ $path
            }
            else {                             # found at least one #
                nqp::pop($parts)
                  unless nqp::chars(nqp::atpos($parts,nqp::elems($parts) - 1));
                my str $short-id = nqp::shift($parts);

                if nqp::elems($parts) {        # has a path
                    my str $path = nqp::pop($parts) // "";
                    if nqp::elems($parts) -> int $nr-options {  # has options
                        my $options := nqp::hash;
                        my int $i = -1;
                        nqp::while(
                          ++$i < $nr-options,
                          nqp::stmts(
                            (my str $option = nqp::atpos($parts,$i)),
                            (my int $index = nqp::index($option,'(')),
                            nqp::bindkey(
                              $options,
                              nqp::substr($option,0,$index),
                              nqp::substr(
                                $option,
                                $index + 1,
                                nqp::chars($option) - $index - 2
                              )
                            )
                          )
                        );
                        self.new: :$short-id, :$path, :Str($spec),
                          :options(nqp::p6bindattrinvres(nqp::create(Map),
                            Map,'$!storage',$options))
                    }
                    else {                     # short-id and just a path
                        self.new: :$short-id, :$path, :Str($spec)
                    }
                }
                else {                         # short-id without path
                    self.new: :$short-id, :Str($short-id ~ "#")
                }
            }
        }
    }
}

# vim: expandtab shiftwidth=4
