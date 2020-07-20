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

    method iterator() { (:$!volume, :$!dirname, :$!basename).iterator }

    method AT-POS(int $pos) {
        $pos == 2
          ?? :$!basename
          !! $pos == 1
            ?? :$!dirname
            !! $pos
              ?? Nil
              !! :$!volume
    }

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

# vim: expandtab shiftwidth=4
