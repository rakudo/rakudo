#!/usr/bin/env raku

# This script reads the native_array.pm6 file, and generates the
# shapedintarray, shapednumarray and shapedstrarray roles in it, and writes
# it back to the file.

# always use highest version of Raku
use v6.*;

my $generator = $*PROGRAM-NAME;
my $generated = DateTime.now.gist.subst(/\.\d+/,'');
my $start     = '#- start of generated part of shaped';
my $idpos     = $start.chars;
my $idchars   = 3;
my $end       = '#- end of generated part of shaped';
my %null = str => '""', int => "0", num => "0e0", uint => "0";

# slurp the whole file and set up writing to it
my $filename = "src/core.c/native_array.pm6";
my @lines = $filename.IO.lines;
$*OUT = $filename.IO.open(:w);

# for all the lines in the source that don't need special handling
while @lines {
    my $line := @lines.shift;

    # nothing to do yet
    unless $line.starts-with($start) {
        say $line;
        next;
    }

    # found header
    my $type = $line.substr($idpos,$idchars);
    $type = 'uint' if $type eq 'uin';
    die "Don't know how to handle $type" unless $type eq "int" | "uint" | "num" | "str";
    say $start ~ $type ~ "array role -----------------------------";
    say "#- Generated on $generated by $generator";
    say "#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE";

    # skip the old version of the code
    while @lines {
        last if @lines.shift.starts-with($end);
    }

    # set up template values
    my %mapper =
      postfix => $type.substr(0,1),
      type    => $type,
      Type    => $type eq 'uint' ?? 'UInt' !! $type.tclc,
      null    => %null{$type},
    ;

    # spurt the roles
    say Q:to/SOURCE/.subst(/ '#' (\w+) '#' /, -> $/ { %mapper{$0} }, :g).chomp;

    role shaped#type#array does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices --> #type#) is raw {
            nqp::if(
              nqp::iseq_i(
                (my int $numdims = nqp::numdimensions(self)),
                (my int $numind  = @indices.elems),  # reifies
              ),
              nqp::stmts(
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                (my $idxs := nqp::list_i),
                nqp::while(                          # native index list
                  nqp::isge_i(--$numdims,0),
                  nqp::push_i($idxs,nqp::shift($indices))
                ),
                nqp::multidimref_#postfix#(self,$idxs)
              ),
              nqp::if(
                nqp::isgt_i($numind,$numdims),
                X::TooManyDimensions.new(
                  operation => 'access',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw,
                NYI("Partially dimensioned views of shaped arrays").throw
              )
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: **@indices --> #type#) {
            nqp::stmts(
              (my #type# $value = @indices.pop),
              nqp::if(
                nqp::iseq_i(
                  (my int $numdims = nqp::numdimensions(self)),
                  (my int $numind  = @indices.elems),  # reifies
                ),
                nqp::stmts(
                  (my $indices := nqp::getattr(@indices,List,'$!reified')),
                  (my $idxs := nqp::list_i),
                  nqp::while(                          # native index list
                    nqp::isge_i(--$numdims,0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  nqp::bindposnd_#postfix#(self, $idxs, $value)
                ),
                nqp::if(
                  nqp::isgt_i($numind,$numdims),
                  X::TooManyDimensions,
                  X::NotEnoughDimensions
                ).new(
                  operation => 'assign to',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        my class Copy:<obj> does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := nqp::getattr(from,List,'$!reified')),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_#postfix#($!list,$!indices,
                  nqp::atposnd($!from,$!indices))
            }
        }

        my class Copy:<#type#> does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := from),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_#postfix#($!list,$!indices,
                  nqp::atposnd_#postfix#($!from,$!indices))
            }
        }

        my class ITERCPY-#type# does Rakudo::Iterator::ShapeBranch {
            has $!iterators;
            method !INIT(\to,\from) {
                nqp::stmts(
                  self!SET-SELF(to),
                  ($!iterators := nqp::setelems(
                    nqp::list(from.iterator),
                    nqp::add_i($!maxdim,1)
                  )),
                  self
                )
            }
            method new(\to,\from) { nqp::create(self)!INIT(to,from) }
            method done(--> Nil) {
                nqp::unless(                        # verify lowest
                  nqp::atpos($!iterators,0).is-lazy # finite iterator
                    || nqp::eqaddr(                 # and something there
                         nqp::atpos($!iterators,0).pull-one,IterationEnd),
                  nqp::atposnd_#postfix#($!list,$!indices)    # boom!
                )
            }
            method process(--> Nil) {
                nqp::stmts(
                  (my int $i = $!level),
                  nqp::while(
                    nqp::isle_i(++$i,$!maxdim),
                    nqp::if(
                      nqp::eqaddr((my \item :=      # exhausted ?
                        nqp::atpos($!iterators,nqp::sub_i($i,1)).pull-one),
                        IterationEnd
                      ),
                      nqp::bindpos($!iterators,$i,  # add an empty one
                        Rakudo::Iterator.Empty),
                      nqp::if(                      # is it an iterator?
                        nqp::istype(item,Iterable) && nqp::isconcrete(item),
                        nqp::bindpos($!iterators,$i,item.iterator),
                        X::Assignment::ToShaped.new(shape => $!dims).throw
                      )
                    )
                  ),
                  (my \iter := nqp::atpos($!iterators,$!maxdim)),
                  nqp::until(                       # loop over highest dim
                    nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                      || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                    nqp::stmts(
                      nqp::bindposnd_#postfix#($!list,$!indices,pulled),
                      nqp::bindpos_i($!indices,$!maxdim,  # increment index
                        nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                    )
                  ),
                  nqp::unless(
                    nqp::eqaddr(pulled,IterationEnd) # if not exhausted
                      || nqp::isle_i(                 # and index too high
                           nqp::atpos_i($!indices,$!maxdim),$!maxind)
                      || iter.is-lazy,                # and not lazy
                    nqp::atposnd_#postfix#($!list,$!indices)  # boom!
                  )
                )
            }
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            ITERCPY-#type#.new(to,from).sink-all;
            to
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              EQV_DIMENSIONS(self,from),
              nqp::stmts(
                Copy:<#type#>.new(self,from).sink-all,
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: array:D \from) {
            nqp::if(
              nqp::istype(from.of,#Type#),
              nqp::if(
                EQV_DIMENSIONS(self,from),
                nqp::stmts(
                  Copy:<#type#>.new(self,from).sink-all,
                  self
                ),
                X::Assignment::ArrayShapeMismatch.new(
                  source-shape => from.shape,
                  target-shape => self.shape
                ).throw
              ),
              X::TypeCheck::Assignment.new(
                symbol   => self.^name ~ '[' ~ self.shape.join(';') ~ ']',
                expected => #Type#,
                got      => from.of
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \from) {
            nqp::if(
              nqp::can(from,'shape'),
              nqp::if(
                from.shape eqv self.shape,
                nqp::stmts(
                  Copy:<obj>.new(self,from).sink-all,
                  self
                ),
                X::Assignment::ArrayShapeMismatch.new(
                    source-shape => from.shape,
                    target-shape => self.shape
                ).throw
              ),
              ITERCPY(self,from)
            )
        }

        my class Iterate-#type# does Rakudo::Iterator::ShapeLeaf {
            method result() is raw {
                nqp::multidimref_#postfix#($!list,nqp::clone($!indices))
            }
        }
        method iterator(::?CLASS:D: --> Iterate-#type#:D) {
            Iterate-#type#.new(self)
        }

        my class KV-#type# does Rakudo::Iterator::ShapeLeaf {
            has int $!on-key;
            method result() is raw {
                nqp::if(
                  ($!on-key = nqp::not_i($!on-key)),
                  nqp::stmts(
                    (my \result := self.indices),
                    (nqp::bindpos_i($!indices,$!maxdim,  # back 1 for next
                      nqp::sub_i(nqp::atpos_i($!indices,$!maxdim),1))),
                    result
                  ),
                  nqp::multidimref_#postfix#($!list,nqp::clone($!indices))
                )
            }
            # needs its own push-all since it fiddles with $!indices
            method push-all(\target --> IterationEnd) {
                nqp::until(
                  nqp::eqaddr((my \pulled := self.pull-one),IterationEnd),
                  target.push(pulled)
                )
            }
        }
        multi method kv(::?CLASS:D: --> Seq:D) { Seq.new(KV-#type#.new(self)) }

        my class Pairs-#type# does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(
                  self.indices,
                  nqp::multidimref_#postfix#($!list,nqp::clone($!indices))
                )
            }
        }
        multi method pairs(::?CLASS:D: --> Seq:D) { Seq.new(Pairs-#type#.new(self)) }

        my class Antipairs-#type# does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(nqp::atposnd_#postfix#($!list,$!indices),self.indices)
            }
        }
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Antipairs-#type#.new(self))
        }
    }  # end of shaped#type#array role

    role shaped1#type#array does shaped#type#array {
        multi method AT-POS(::?CLASS:D: int \one --> #type#) is raw {
           nqp::atposref_#postfix#(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D $one --> #type#) is raw {
           nqp::atposref_#postfix#(self,$one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, #type# \value --> #type#) {
            nqp::bindpos_#postfix#(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $one, #type# \value --> #type#) {
            nqp::bindpos_#postfix#(self,$one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, #Type#:D \value --> #type#) {
            nqp::bindpos_#postfix#(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $one, #Type#:D \value --> #type#) {
            nqp::bindpos_#postfix#(self,$one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D $one --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i($one,0) && nqp::islt_i($one,nqp::elems(self))
            )
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              nqp::iseq_i((my int $elems = nqp::elems(self)),nqp::elems(from)),
              nqp::stmts(
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(++$i,$elems),
                  nqp::bindpos_#postfix#(self,$i,nqp::atpos_#postfix#(from,$i))
                ),
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            my \iter := Rakudo::Iterator.TailWith(in.iterator,#null#);
            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,nqp::elems(self)),
              nqp::bindpos_#postfix#(self,$i,iter.pull-one)
            );
            # too many values? then throw by just accessing out of range
            nqp::atpos_#postfix#(list,$i) unless iter.exhausted;
            self
        }
        multi method STORE(::?CLASS:D: #Type#:D \item) {
            nqp::bindpos_#postfix#(self,0,item);
            self
        }

        my class Iterate-#type# does PredictiveIterator {
            has Mu $!list;
            has int $!pos;
            method !SET-SELF(Mu \list) {
                nqp::stmts(
                  ($!list := list),
                  ($!pos = -1),
                  self
                )
            }
            method new(Mu \list) { nqp::create(self)!SET-SELF(list) }
            method pull-one() is raw {
                nqp::islt_i(++$!pos,nqp::elems($!list))
                  ?? nqp::atposref_#postfix#($!list,$!pos)
                  !! IterationEnd
            }
            method skip-one() {
                nqp::islt_i(++$!pos,nqp::elems($!list))
            }
            method push-all(\target --> IterationEnd) {
                nqp::stmts(
                  (my int $elems = nqp::elems($!list)),
                  (my int $pos = $!pos),
                  nqp::while(
                    nqp::islt_i(++$pos,$elems),
                    target.push(nqp::atpos_#postfix#($!list,$pos))
                  ),
                  ($!pos = $pos)
                )
            }
            method count-only(--> Int:D) {
                nqp::p6box_i(
                  nqp::elems($!list)
                    - $!pos
                    - nqp::islt_i($!pos,nqp::elems($!list))
                )
            }
            method sink-all(--> IterationEnd) {
                $!pos = nqp::elems($!list)
            }
        }
        method iterator(::?CLASS:D: --> Iterate-#type#:D) {
            Iterate-#type#.new(self)
        }

        multi method kv(::?CLASS:D: --> Seq:D) {
            my int $i = -1;
            my int $elems = nqp::add_i(nqp::elems(self),nqp::elems(self));
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(++$i,$elems),
                  nqp::if(
                    nqp::bitand_i($i,1),
                    nqp::atposref_#postfix#(self,nqp::bitshiftr_i($i,1)),
                    nqp::bitshiftr_i($i,1)
                  ),
                  IterationEnd
                )
            }))
        }
        multi method pairs(::?CLASS:D: --> Seq:D) {
            my int $i = -1;
            my int $elems = nqp::elems(self);
            Seq.new(Rakudo::Iterator.Callable({
                nqp::islt_i(++$i,$elems)
                  ?? Pair.new($i,nqp::atposref_#postfix#(self,$i))
                  !! IterationEnd
            }))
        }
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D: --> ::?CLASS:D) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(++$i,$elems),
                nqp::bindpos_#postfix#($to,nqp::sub_i($last,$i),
                  nqp::atpos_#postfix#(self,$i))
              ),
              $to
            )
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1 --> ::?CLASS:D) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(++$i,$elems),
                nqp::bindpos_#postfix#(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_#postfix#(self,$i)
                ),
              ),
              $to
            )
        }
    } # end of shaped1#type#array role

    role shaped2#type#array does shaped#type#array {
        multi method AT-POS(::?CLASS:D: int \one, int \two --> #type#) is raw {
            nqp::multidimref_#postfix#(self,nqp::list_i(one, two))
        }
        multi method AT-POS(::?CLASS:D: Int:D $one, Int:D $two --> #type#) is raw {
            nqp::multidimref_#postfix#(self,nqp::list_i($one, $two))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, #Type#:D \value --> #type#) {
            nqp::bindpos2d_#postfix#(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $one, Int:D $two, #Type#:D \value --> #type#) {
            nqp::bindpos2d_#postfix#(self,$one,$two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D $one, Int:D $two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i($one,0)
                && nqp::isge_i($two,0)
                && nqp::islt_i($one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i($two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2#type#array role

    role shaped3#type#array does shaped#type#array {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three --> #type#) is raw {
            nqp::multidimref_#postfix#(self,nqp::list_i(one, two, three))
        }
        multi method AT-POS(::?CLASS:D: Int:D $one, Int:D $two, Int:D $three --> #type#) is raw {
            nqp::multidimref_#postfix#(self,nqp::list_i($one, $two, $three))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, #Type#:D \value --> #type#) {
            nqp::bindpos3d_#postfix#(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $one, Int:D $two, Int:D $three, #Type#:D \value --> #type#) {
            nqp::bindpos3d_#postfix#(self,$one,$two,$three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D $one, Int:D $two, Int:D $three --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i($one,0)
                && nqp::isge_i($two,0)
                && nqp::isge_i($three,0)
                && nqp::islt_i($one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i($two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i($three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
    } # end of shaped3#type#array role
SOURCE

    # we're done for this role
    say "#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE";
    say $end ~ $type ~ "array role -------------------------------";
}

# close the file properly
$*OUT.close;

# vim: expandtab sw=4
