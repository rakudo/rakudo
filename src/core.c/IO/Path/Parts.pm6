class IO::Path::Parts
  does Positional   # all of these can go as soon as we don't need
  does Associative  # the compatibility with the original "List of
  does Iterable     # Pairs" and Map implemention anymore.
{
    has str $.volume;
    has str $.dirname;
    has str $.basename;

    method !SET-SELF($!volume, $!dirname, $!basename) { self }
    method new(\volume, \dirname, \basename) {
        nqp::create(self)!SET-SELF(volume, dirname, basename)
    }

    method raku() {
        'IO::Path::Parts.new('
          ~ $!volume.raku
          ~ ','
          ~ $!dirname.raku
          ~ ','
          ~ $!basename.raku
          ~ ')'
    }

#-------------------------------------------------------------------------------
# all of the code below is just to provide a compatibility layer with the
# original List of Pairs / Map implementation.  As soon as this is no longer
# needed, this can go.

    method of() { Str }

    method by-index(int $pos) is implementation-detail {
        $pos == 2
          ?? :$!basename
          !! $pos == 1
            ?? :$!dirname
            !! $pos
              ?? Nil
              !! :$!volume
    }

    class Iterate does Iterator {
        has $!parts;
        has int $!i;
        method new(\parts) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!parts',parts)
        }
        method pull-one() { $!i < 3 ?? $!parts.by-index($!i++) !! IterationEnd }
    }
    method iterator() { Iterate.new(self) }

    method AT-POS(int $pos) { self.by-index($pos) }

    method AT-KEY(str $key) {
        $key eq 'basename'
          ?? $!basename
          !! $key eq 'dirname'
            ?? $!dirname
            !! $key eq 'volume'
              ?? $!volume
              !! Nil
    }
}

# vim: ft=perl6 expandtab sw=4
