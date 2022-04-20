class CompUnit::Repository::Spec {
    has Str $.short-id is built(:bind);
    has Str $.Str      is built(:bind);
    has Str $.path     is built(:bind);
    has %.options;

    # alternate instantiator called from CompUnit::RepositoryRegistry
    method from-string(
      str $spec, str $default-short-id
    --> CompUnit::Repository::Spec:D) is implementation-detail {

# Examples:
#  perl5#
#  inst#/Foo/Bar/rakudo/gen/build_rakudo_home/core
#  CompUnit::Repository::Staging#name(core)#/Foo/Bar/rakudo/install/share/perl6/core

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
                    my %options;
                    my int $i = -1;
                    while ++$i < $nr-options {
                        %options{$0} := $1.Str
                          if nqp::atpos($parts,$i).match: / ^
                            (<[\w-]>+)
                            <[ <([{ ]>
                            (<-[ >)\]} ]>+)
                            <[ >)\]} ]>
                          $ /;
                    }
                    self.new: :$short-id, :$path, :Str($spec), :%options
                }
                else {                     # short-id and just a path
                    self.new: :$short-id, :$path, :Str($spec)
                }
            }
            else {                         # short-id without path
                self.new: :$short-id, :path(''), :Str($short-id ~ "#")
            }
        }
    }
}

# vim: expandtab shiftwidth=4
