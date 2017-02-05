my class DateTime { ... }
my role  IO { ... }
my class IO::Path { ... }
my class Seq { ... }
my class Lock is repr('ReentrantMutex') { ... }
my class Rakudo::Iterator { ... }
my class Rakudo::Metaops { ... }
my class X::Cannot::Lazy { ... }
my class X::IllegalOnFixedDimensionArray { ... };
my class X::Assignment::ToShaped { ... };
my class X::Str::Sprintf::Directives::BadType { ... };
my class X::Str::Sprintf::Directives::Count { ... };
my class X::Str::Sprintf::Directives::Unsupported { ... };
my class X::IllegalDimensionInShape { ... };

my class Rakudo::Internals {

    our class WeightedRoll {
        has @!pairs;
        has $!total;

        method !SET-SELF(\list-of-pairs) {
            $!total = 0;
            for list-of-pairs.pairs {
                my $value := .value;
                if $value > 0 {
                    @!pairs.push($_);
                    $!total = $!total + $value;
                }
            }
            self
        }
        method new(\list-of-pairs) { nqp::create(self)!SET-SELF(list-of-pairs) }
        method roll() {
            my $rand = $!total.rand;
            my $seen = 0;
            return .key if ( $seen = $seen + .value ) > $rand for @!pairs;
        }
    }

    # rotate nqp list to another given list without using push/pop
    method RotateListToList(\from,\n,\to) {
        nqp::stmts(
          (my $from := nqp::getattr(from,List,'$!reified')),
          (my int $elems = nqp::elems($from)),
          (my $to := nqp::getattr(to,List,'$!reified')),
          (my int $i = -1),
          (my int $j = nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),n),$elems)),
          nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::bindpos(
              $to,
              ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
              nqp::atpos($from,$i)
            ),
          ),
          to
        )
    }

    method ReverseListToList(\from,\to) {
        nqp::stmts(
          (my $from := nqp::getattr(from,List,'$!reified')),
          (my int $elems = nqp::elems($from)),
          (my int $last  = nqp::sub_i($elems,1)),
          (my int $i     = -1),
          (my $to := nqp::getattr(to,List,'$!reified')),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::bindpos($to,nqp::sub_i($last,$i),nqp::atpos($from,$i))
          ),
          to
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # The parameter is the HLL List to be sorted *in place* using simple cmp.
    method MERGESORT-REIFIED-LIST(\list) {
        nqp::stmts(

          # $A has the items to sort; $B is a work array
          (my Mu $A := nqp::getattr(list,List,'$!reified')),
          nqp::if(
            nqp::isgt_i((my int $n = nqp::elems($A)),2),
            nqp::stmts(     # we actually need to sort
              (my Mu $B := nqp::setelems(nqp::list,$n)),

              # Each 1-element run in $A is already "sorted"
              # Make successively longer sorted runs of length 2, 4, 8, 16...
              # until $A is wholly sorted
              (my int $width = 1),
              nqp::while(
                nqp::islt_i($width,$n),
                nqp::stmts(
                  (my int $l = 0),

                  # $A is full of runs of length $width
                  nqp::while(
                    nqp::islt_i($l,$n),

                    nqp::stmts(
                      (my int $left  = $l),
                      (my int $right = nqp::add_i($l,$width)),
                      nqp::if(nqp::isge_i($right,$n),($right = $n)),
                      (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                      nqp::if(nqp::isge_i($end,$n),($end = $n)),

                      (my int $i = $left),
                      (my int $j = $right),
                      (my int $k = nqp::sub_i($left,1)),

                      # Merge two runs: $A[i       .. i+width-1] and
                      #                 $A[i+width .. i+2*width-1]
                      # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                      nqp::while(
                        nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                        nqp::if(
                          nqp::islt_i($i,$right) && (
                            nqp::isge_i($j,$end)
                              || nqp::iseq_i(
                                   nqp::decont(  # for some reason we need this
                                     nqp::atpos($A,$i) cmp nqp::atpos($A,$j)
                                       || nqp::cmp_i($i,$j)
                                   ), # apparently code gen with || isn't right
                                   -1
                                 )
                          ),
                          nqp::stmts(
                            (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                            ($i = nqp::add_i($i,1))
                          ),
                          nqp::stmts(
                            (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                            ($j = nqp::add_i($j,1))
                          )
                        )
                      ),
                      ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                    )
                  ),

                  # Now work array $B is full of runs of length 2*width.
                  # Copy array B to array A for next iteration.  A more
                  # efficient implementation would swap the roles of A and B.
                  (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                  # Now array $A is full of runs of length 2*width.

                  ($width = nqp::add_i($width,$width))
                )
              ),
              nqp::p6bindattrinvres(list,List,'$!reified',$A)
            ),
            nqp::if(
              nqp::islt_i($n,2)
                || nqp::isle_i(nqp::atpos($A,0) cmp nqp::atpos($A,1),0),
              list,  # nothing to be done, we already have the result
              nqp::p6bindattrinvres(list,List,'$!reified',  # need to swap
                nqp::list(nqp::atpos($A,1),nqp::atpos($A,0)))
            )
          )
        )
    }
    # Takes the HLL List to be sorted *in place* using the comparator
    method MERGESORT-REIFIED-LIST-WITH(\list, &comparator) {
        nqp::stmts(

          # $A has the items to sort; $B is a work array
          (my Mu $A := nqp::getattr(list,List,'$!reified')),
          nqp::if(
            nqp::isgt_i((my int $n = nqp::elems($A)),2),
            nqp::stmts(     # we actually need to sort
              (my Mu $B := nqp::setelems(nqp::list,$n)),

              # Each 1-element run in $A is already "sorted"
              # Make successively longer sorted runs of length 2, 4, 8, 16...
              # until $A is wholly sorted
              (my int $width = 1),
              nqp::while(
                nqp::islt_i($width,$n),
                nqp::stmts(
                  (my int $l = 0),

                  # $A is full of runs of length $width
                  nqp::while(
                    nqp::islt_i($l,$n),

                    nqp::stmts(
                      (my int $left  = $l),
                      (my int $right = nqp::add_i($l,$width)),
                      nqp::if(nqp::isge_i($right,$n),($right = $n)),
                      (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                      nqp::if(nqp::isge_i($end,$n),($end = $n)),

                      (my int $i = $left),
                      (my int $j = $right),
                      (my int $k = nqp::sub_i($left,1)),

                      # Merge two runs: $A[i       .. i+width-1] and
                      #                 $A[i+width .. i+2*width-1]
                      # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                      nqp::while(
                        nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                        nqp::if(
                          nqp::islt_i($i,$right) && (
                            nqp::isge_i($j,$end)
                              || nqp::iseq_i(
                                   nqp::decont(  # for some reason we need this
                                     comparator(
                                       nqp::atpos($A,$i),nqp::atpos($A,$j))
                                        || nqp::cmp_i($i,$j)
                                   ), # apparently code gen with || isn't right
                                   -1
                                 )
                          ),
                          nqp::stmts(
                            (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                            ($i = nqp::add_i($i,1))
                          ),
                          nqp::stmts(
                            (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                            ($j = nqp::add_i($j,1))
                          )
                        )
                      ),
                      ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                    )
                  ),

                  # Now work array $B is full of runs of length 2*width.
                  # Copy array B to array A for next iteration. A more
                  # efficient implementation would swap the roles of A and B.
                  (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                  # Now array $A is full of runs of length 2*width.

                  ($width = nqp::add_i($width,$width))
                )
              ),
              nqp::p6bindattrinvres(list,List,'$!reified',$A)
            ),
            nqp::if(
              nqp::islt_i($n,2)
                || nqp::iseq_i(
                    comparator(nqp::atpos($A,0),nqp::atpos($A,1)),-1),
              list,  # nothing to be done, we already have the result
              nqp::p6bindattrinvres(list,List,'$!reified',  # need to swap
                nqp::list(nqp::atpos($A,1),nqp::atpos($A,0)))
            )
          )
        )
    }
    # Takes the HLL List to be sorted *in place* using the mapper
    method MERGESORT-REIFIED-LIST-AS(\list,&mapper) {
        nqp::stmts(

          (my Mu $O := nqp::getattr(list,List,'$!reified')),  # Original
          nqp::if(
            nqp::isgt_i((my int $n = nqp::elems($O)),2),
            nqp::stmts(     # we actually need to sort
              (my Mu $S := nqp::clone($O)),                   # the Schwartz
              (my Mu $A := nqp::setelems(nqp::list_i,$n)),    # indexes to sort
              (my Mu $B := nqp::setelems(nqp::list_i,$n)),    # work array
              (my int $s = -1),
              nqp::while(  # set up the Schwartz and the initial indexes
                nqp::islt_i(($s = nqp::add_i($s,1)),$n),
                nqp::bindpos($S,nqp::bindpos_i($A,$s,$s),
                  mapper(nqp::atpos($S,$s)))
              ),

              # Each 1-element run in $A is already "sorted"
              # Make successively longer sorted runs of length 2, 4, 8, 16...
              # until $A is wholly sorted
              (my int $width = 1),
              nqp::while(
                nqp::islt_i($width,$n),
                nqp::stmts(
                  (my int $l = 0),

                  # $A is full of runs of length $width
                  nqp::while(
                    nqp::islt_i($l,$n),

                    nqp::stmts(
                      (my int $left  = $l),
                      (my int $right = nqp::add_i($l,$width)),
                      nqp::if(nqp::isge_i($right,$n),($right = $n)),
                      (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                      nqp::if(nqp::isge_i($end,$n),($end = $n)),

                      (my int $i = $left),
                      (my int $j = $right),
                      (my int $k = nqp::sub_i($left,1)),

                      # Merge two runs: $A[i       .. i+width-1] and
                      #                 $A[i+width .. i+2*width-1]
                      # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                      nqp::while(
                        nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                        nqp::if(
                          nqp::islt_i($i,$right) && (
                            nqp::isge_i($j,$end)
                              || (nqp::iseq_i(
                                   nqp::decont(
                                     nqp::atpos($S,nqp::atpos_i($A,$i))
                                       cmp nqp::atpos($S,nqp::atpos_i($A,$j))
                                       || nqp::cmp_i($i,$j)
                                   ),
                                   -1
                                 ))
                          ),
                          nqp::stmts(
                            (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                            ($i = nqp::add_i($i,1))
                          ),
                          nqp::stmts(
                            (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                            ($j = nqp::add_i($j,1))
                          )
                        )
                      ),
                      ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                    )
                  ),

                  # Now work array $B is full of runs of length 2*width.
                  # Copy array B to array A for next iteration. A more
                  # efficient implementation would swap the roles of A and B.
                  (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                  # Now array $A is full of runs of length 2*width.

                  ($width = nqp::add_i($width,$width))
                )
              ),
              ($s = -1),
              nqp::while(   # repurpose the Schwartz for the result
                nqp::islt_i(($s = nqp::add_i($s,1)),$n),
                nqp::bindpos($S,$s,nqp::atpos($O,nqp::atpos_i($A,$s)))
              ),
              nqp::p6bindattrinvres(list,List,'$!reified',$S)
            ),

            nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
              nqp::if(
                nqp::islt_i($n,2)
                  || nqp::iseq_i(
                      mapper(nqp::atpos($O,0)) cmp mapper(nqp::atpos($O,1)),-1),
                nqp::clone($O), # nothing to be done, we already have the result
                nqp::list(nqp::atpos($O,1),nqp::atpos($O,0))  # need to swap
              )
            )
          )
        )
    }

    method SET_LEADING_DOCS($obj, $docs) {
        my $current_why := $obj.WHY;

        if $current_why {
            my $end := nqp::elems($*POD_BLOCKS) - 1;
            my $i   := $end;

            while $i >= 0 {
                if $docs === nqp::atpos($*POD_BLOCKS, $i) {
                    nqp::splice($*POD_BLOCKS, nqp::list(), $i, 1);
                    last;
                }
                $i := $i - 1;
            }

            $current_why._add_leading(~$docs);
        } else {
            $obj.set_why($docs);
        }
    }

    method SET_TRAILING_DOCS($obj, $docs) {
        my $current_why := $obj.WHY;

        if $current_why {
            $current_why._add_trailing(~$docs);
        } else {
            $obj.set_why($docs);
            $*POD_BLOCKS.push($docs);
        }
    }

    method EXPORT_SYMBOL(\exp_name, @tags, Mu \sym) {
        my @export_packages = $*EXPORT;
        for flat nqp::hllize(@*PACKAGES) {
            unless .WHO.EXISTS-KEY('EXPORT') {
                .WHO<EXPORT> := Metamodel::PackageHOW.new_type(:name('EXPORT'));
                .WHO<EXPORT>.^compose;
            }
            @export_packages.append: .WHO<EXPORT>;
        }
        for @export_packages -> $p {
            for @tags -> $tag {
                my $install_in;
                if $p.WHO.EXISTS-KEY($tag) {
                    $install_in := $p.WHO.{$tag};
                }
                else {
                    $install_in := Metamodel::PackageHOW.new_type(:name($tag));
                    $install_in.^compose;
                    $p.WHO{$tag} := $install_in;
                }
                if $install_in.WHO.EXISTS-KEY(exp_name) {
                    unless ($install_in.WHO){exp_name} =:= sym {
                        X::Export::NameClash.new(symbol => exp_name).throw;
                    }
                }
                $install_in.WHO{exp_name} := sym;
            }
        }
        0;
    }

    method THE_END {
        my @END := nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
            nqp::getcurhllsym("@END_PHASERS"));
        for @END -> $end { $end() };
    }

    # fast whitespace trim: str to trim, str to store trimmed str
    method TRIM(\string, \trimmed --> Nil) {
        my int $pos  = nqp::chars(string) - 1;
        my int $left =
          nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, string, 0, $pos + 1);
        $pos = $pos - 1
          while nqp::isge_i($pos, $left)
            && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, string, $pos);
        trimmed = nqp::islt_i($pos, $left)
          ?? ''
          !! nqp::substr(string, $left, $pos + 1 - $left);
        Nil
    }

    # fast key:value split: Str to split, str to store key, str to store value
    method KEY_COLON_VALUE(Str $command, \key, \value --> Nil) {
        my str $str   = nqp::unbox_s($command);
        my int $index = nqp::index($str,':');
        if nqp::isgt_i($index,0) {
            self.TRIM(nqp::substr($str,0,$index),key);
            self.TRIM(nqp::substr($str,$index + 1,nqp::chars($str) - $index),value);
        }
        elsif nqp::islt_i($index,0) {
            self.TRIM($str,key);
            value = '';
        }
        else {
            key = '';
            self.TRIM(nqp::substr($str,1,nqp::chars($str) - 1),value);
        }
        Nil
    }

    # key space value split: Str to split, str to store key, str to store value
    method KEY_SPACE_VALUE(Str $command, \key, \value --> Nil) {
        my str $str   = nqp::unbox_s($command);
        my int $index = nqp::index($str,' ');
        if nqp::isgt_i($index,0) {
            key   = nqp::substr($str,0,$index);
            value = nqp::substr($str,$index + 1,nqp::chars($str) - $index);
        }
        elsif nqp::islt_i($index,0) {
            key   = $str;
            value = '';
        }
        else {
            key   = '';
            value = nqp::substr($str,1,nqp::chars($str) - 1);
        }
        Nil
    }

    my $encodings := nqp::hash(
      # fast mapping for identicals
      'utf8',            'utf8',
      'utf16',           'utf16',
      'utf32',           'utf32',
      'ascii',           'ascii',
      'iso-8859-1',      'iso-8859-1',
      'windows-1252',    'windows-1252',
      # with dash
      'utf-8',           'utf8',
      'utf-16',          'utf16',
      'utf-32',          'utf32',
      # according to http://de.wikipedia.org/wiki/ISO-8859-1
      'iso_8859-1:1987', 'iso-8859-1',
      'iso_8859-1',      'iso-8859-1',
      'iso-ir-100',      'iso-8859-1',
      'latin1',          'iso-8859-1',
      'latin-1',         'iso-8859-1',
      'csisolatin1',     'iso-8859-1',
      'l1',              'iso-8859-1',
      'ibm819',          'iso-8859-1',
      'cp819',           'iso-8859-1',
    );
    method NORMALIZE_ENCODING(Str:D \encoding) {
        my str $key = nqp::unbox_s(encoding);
        if nqp::existskey($encodings,$key) {
            nqp::atkey($encodings,$key)
        }
        else {
            my str $lc = nqp::lc($key);
            nqp::existskey($encodings,$lc)
              ?? nqp::atkey($encodings,$lc)
              !! nqp::lc($key)
        }
    }

    method SET_LINE_ENDING_ON_HANDLE(Mu \handle, $ending) {
        if nqp::istype($ending, Iterable) {
            my \endings = nqp::list_s();
            for @$ending -> $e {
                nqp::push_s(endings, nqp::unbox_s($e.Str));
            }
#?if !jvm
            nqp::setinputlineseps(handle, endings);
#?endif
#?if jvm
            nqp::setinputlinesep(handle, nqp::atpos_s(endings, 0))
#?endif
        }
        else {
            nqp::setinputlinesep(handle, nqp::unbox_s($ending.Str))
        }
        Nil
    }

    # number of elems of type if all, otherwise 0
    method ALL_TYPE(\values,\type) {
        nqp::stmts(
          (my int $elems = values.elems),   # reifies
          (my $values := nqp::getattr(values,List,'$!reified')),
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
              && nqp::istype(nqp::atpos($values,$i),type),
            nqp::null
          ),
          nqp::iseq_i($i,$elems) && $elems
        )
    }

    # number of elems of defined && type if all, otherwise 0
    method ALL_DEFINED_TYPE(\values,\type) {
        nqp::stmts(
          (my int $elems = values.elems),   # reifies
          (my $values := nqp::getattr(values,List,'$!reified')),
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
              && nqp::defined(nqp::atpos($values,$i))
              && nqp::istype(nqp::atpos($values,$i),type),
            nqp::null
          ),
          nqp::iseq_i($i,$elems) && $elems
        )
    }

    method TRANSPOSE(str $string, str $original, str $final) {
        nqp::join($final,nqp::split($original,$string))
    }
    method TRANSPOSE-ONE(str $string, str $original, str $final) {
        nqp::if(
          nqp::iseq_i((my int $index = nqp::index($string, $original)), -1),
          $string,
          nqp::concat(
            nqp::substr($string,0,$index),
            nqp::concat(
              $final,
              nqp::substr($string,nqp::add_i($index,nqp::chars($final)))
            )
          )
        )
    }

    my constant \SHAPE-STORAGE-ROOT := do {
        my Mu $root := nqp::newtype(nqp::knowhow(), 'Uninstantiable');
        nqp::setparameterizer($root, -> $, $key {
            my $dims := $key.elems.pred;
            my $type := $key.AT-POS(1);
            my $dim_type := nqp::newtype($key.AT-POS(0), 'MultiDimArray');
            nqp::composetype($dim_type, nqp::hash('array',
                nqp::hash('dimensions', $dims, 'type', $type)));
            nqp::settypehll($dim_type, 'perl6');
            $dim_type
        });
        nqp::settypehll($root, 'perl6');
        $root
    }

    method SHAPED-ARRAY-STORAGE(\spec, Mu \meta-obj, Mu \type) {
        nqp::stmts(
          (my $types := nqp::list(meta-obj)),  # meta + type of each dimension
          (my $dims  := nqp::list_i),          # elems per dimension
          nqp::if(
            nqp::istype(spec,List),
            nqp::stmts(                        # potentially more than 1 dim
              (my $spec  := nqp::getattr(nqp::decont(spec),List,'$!reified')),
              (my int $elems = nqp::elems($spec)),
              (my int $i     = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::if(
                  nqp::istype((my $dim := nqp::atpos($spec,$i)),Whatever),
                  X::NYI.new(feature => 'Jagged array shapes').throw,
                  nqp::if(
                    nqp::isbig_I(nqp::decont($dim := nqp::decont($dim.Int)))
                      || nqp::isle_i($dim,0),
                    X::IllegalDimensionInShape.new(:$dim).throw,
                    nqp::stmts(
                      nqp::push($types,type),
                      nqp::push_i($dims,$dim)
                    )
                  )
                )
              )
            ),
            nqp::stmts(                        # only 1 dim
              nqp::push($types,type),
              nqp::push_i($dims,spec.Int)
            )
          ),
          nqp::setdimensions(
            nqp::create(nqp::parameterizetype(SHAPE-STORAGE-ROOT,$types)),
            $dims
          )
        )
    }

    our role ShapedArrayCommon {
        method !illegal($operation) {
            X::IllegalOnFixedDimensionArray.new(:$operation).throw
        }
        proto method pop(::?CLASS:D: |)    { self!illegal("pop")    }
        proto method shift(::?CLASS:D: |)  { self!illegal("shift")  }
        proto method splice(::?CLASS:D: |) { self!illegal("splice") }

        proto method push(|c) is nodal {
            self.DEFINITE ?? self!illegal("push")    !! self.Any::push(|c)
        }
        proto method append(|c) is nodal {
            self.DEFINITE ?? self!illegal("append")  !! self.Any::append(|c)
        }
        proto method unshift(|c) is nodal {
            self.DEFINITE ?? self!illegal("unshift") !! self.Any::unshift(|c)
        }
        proto method prepend(|c) is nodal {
            self.DEFINITE ?? self!illegal("prepend") !! self.Any::prepend(|c)
        }

        multi method STORE(::?CLASS:D: Slip:D \slip) {
            nqp::if(
              nqp::eqaddr(slip,Empty),
              (die "Cannot Empty a shaped array as its size is fixed"),
              self.STORE(slip.List)
            )
        }

        # illegal unless overridden for 1dimmed case
        method reverse(::?CLASS:D: |) { self!illegal("reverse") }
        method rotate(::?CLASS:D: |)  { self!illegal("rotate") }

        multi method values(::?CLASS:D:) { Seq.new(self.iterator) }
        multi method keys(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.ShapeIndex(self.shape))
        }
        multi method invert(::?CLASS:D:) {
            self.keys.map({ nqp::decont(self.AT-POS(|$_)) »=>» $_ }).flat
        }

        # These work on the flat view
        method roll(|c)         { self.flat.roll(|c) }
        method pick(|c)         { self.flat.pick(|c) }
        method permutations(|c) { self.flat.permutations(|c) }
        method combinations(|c) { self.flat.combinations(|c) }
        method join(|c)         { self.flat.join(|c) }
        method sort(|c)         { self.flat.sort(|c) }

        multi method gist(::?CLASS:D:) {
            self.gistseen('Array', { self!gist([], self.shape) })
        }
        method !gist(@path, @dims) {
            if @dims.elems == 1 {
                 '[' ~ (^@dims[0]).map({ self.AT-POS(|@path, $_).gist }).join(' ') ~ ']';
            }
            else {
                my @nextdims = @dims[1..^@dims.elems];
                '[' ~ (^@dims[0]).map({ self!gist((flat @path, $_), @nextdims) }).join(' ') ~ ']';
            }
        }

        multi method perl(::?CLASS:D \SELF:) {
            SELF.perlseen('Array', {
                self.^name
                ~ '.new(:shape'
                ~ nqp::decont(self.shape).perl
                ~ ', '
                ~ self!perl([], self.shape)
                ~ ')'
                ~ (nqp::iscont(SELF) ?? '.item' !! '')
            })
        }
        method !perl(@path, @dims) {
            if @dims.elems == 1 {
                 '[' ~
                    (^@dims[0]).map({ nqp::decont(self.AT-POS(|@path, $_)).perl }).join(', ') ~
                    ',' x (@dims[0] == 1 && nqp::istype(self.AT-POS(|@path, 0), Iterable)) ~
                 ']'
            }
            else {
                my @nextdims = @dims[1..^@dims.elems];
                '[' x (@path.elems > 0) ~
                    (^@dims[0]).map({ self!perl((flat @path, $_), @nextdims) }).join(', ') ~
                    ',' x (@dims[0] == 1) ~
                ']' x (@path.elems > 0)
            }
        }

        multi method Slip() {
            Slip.from-iterator(self.iterator)
        }

        proto method AT-POS(|) is raw {*}
        multi method AT-POS(::?CLASS:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(::?CLASS:D:) is raw {
            die "Must specify at least one index with {self.^name}.AT-POS"
        }

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(::?CLASS:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(::?CLASS:D:) {
            die "Must specify at least one index and a value with {self.^name}.ASSIGN-POS"
        }
        multi method ASSIGN-POS(::?CLASS:D: $) {
            die "Must specify at least one index and a value with {self.^name}.ASSIGN-POS"
        }

        proto method EXISTS-POS(|) {*}
        multi method EXISTS-POS(::?CLASS:U: |c) {
            self.Any::EXISTS-POS(|c)
        }
        multi method EXISTS-POS(::?CLASS:D:) {
            die "Must specify at least one index with {self.^name}.EXISTS-POS"
        }
    }

    our class SupplySequencer {
        has &!on-data-ready;
        has &!on-completed;
        has &!on-error;
        has $!buffer;
        has int $!buffer-start-seq;
        has int $!done-target;
        has int $!bust;
        has $!lock;

        submethod BUILD(
          :&!on-data-ready!, :&!on-completed!, :&!on-error! --> Nil) {
            $!buffer := nqp::list();
            $!buffer-start-seq = 0;
            $!done-target = -1;
            $!bust = 0;
            $!lock := Lock.new;
        }

        method process(Mu \seq, Mu \data, Mu \err) {
            $!lock.protect: {
                if err {
                    &!on-error(err);
                    $!bust = 1;
                }
                elsif nqp::isconcrete(data) {
                    my int $insert-pos = seq - $!buffer-start-seq;
                    nqp::bindpos($!buffer, $insert-pos, data);
                    self!emit-events();
                }
                else {
                    $!done-target = seq;
                    self!emit-events();
                }
            }
        }

        method !emit-events() {
            unless $!bust {
                until nqp::elems($!buffer) == 0 || nqp::isnull(nqp::atpos($!buffer, 0)) {
                    &!on-data-ready(nqp::shift($!buffer));
                    $!buffer-start-seq = $!buffer-start-seq + 1;
                }
                if $!buffer-start-seq == $!done-target {
                    &!on-completed();
                }
            }
        }
    }

    my int $sprintfHandlerInitialized = 0;
    method initialize-sprintf-handler(--> Nil) {
        class SprintfHandler {
            method mine($x) { nqp::reprname($x) eq "P6opaque"; }
            method int($x) { $x.Int }
        }
        unless $sprintfHandlerInitialized {
            nqp::sprintfaddargumenthandler(SprintfHandler.new);
            $sprintfHandlerInitialized = 1;
        }
    }

    method SUBSTR-START-OOR(\from,\max) {
        X::OutOfRange.new(
          :what('Start argument to substr'),
          :got(from.gist),
          :range("0.." ~ max),
          :comment( nqp::istype(from, Callable) || -from > max
            ?? ''
            !! "use *-{abs from} if you want to index relative to the end"),
        );
    }
    method SUBSTR-CHARS-OOR(\chars) {
        X::OutOfRange.new(
          :what('Number of characters argument to substr'),
          :got(chars.gist),
          :range<0..^Inf>,
          :comment("use *-{abs chars} if you want to index relative to the end"),
        );
    }
    method SUBSTR-SANITY(Str \what, $start, $want, \from, \chars) {
        my Int $max := what.chars;
        from = nqp::istype($start, Callable)
          ?? $start($max)
          !! nqp::istype($start, Range)
            ?? $start.min + $start.excludes-min
            !! $start.Int;
        Rakudo::Internals.SUBSTR-START-OOR(from,$max).fail
          if from < 0 || from > $max;

        chars = nqp::istype($start, Range)
          ?? $start == Inf
            ?? $max - from
            !! $start.max - $start.excludes-max - from + 1
          !! $want.defined
            ?? $want === Inf
              ?? $max - from
              !! nqp::istype($want, Callable)
                ?? $want($max - from)
                !! (nqp::istype($want,Int) ?? $want !! $want.Int)
            !! $max - from;
        chars < 0 ?? Rakudo::Internals.SUBSTR-CHARS-OOR(chars).fail !! 1;
    }

    my $IS-WIN = do {
        my str $os = Rakudo::Internals.TRANSPOSE(nqp::lc(
#?if jvm
          nqp::atkey(nqp::jvmgetproperties,'os.name')
#?endif
#?if !jvm
          nqp::atkey(nqp::backendconfig,'osname')
#?endif
        )," ","");
        nqp::p6bool(
          nqp::iseq_s($os,'mswin32')
            || nqp::iseq_s($os,'mingw')
            || nqp::iseq_s($os,'msys')
            || nqp::iseq_s($os,'cygwin')
        )
    }
    method IS-WIN() { $IS-WIN }

    method NUMERIC-ENV-KEY(\key) {
        %*ENV.EXISTS-KEY(key)
          ?? %*ENV.AT-KEY(key)
            ?? +%*ENV.AT-KEY(key)
            !! 0
          !! Nil
    }

    method error-rcgye() {  # red clear green yellow eject
        self.NUMERIC-ENV-KEY("RAKUDO_ERROR_COLOR") // !self.IS-WIN
          ?? ("\e[31m", "\e[0m", "\e[32m", "\e[33m", "\x[23CF]")
          !! ("", "", "", "", "<HERE>");
    }

    my num $init-time-num = nqp::time_n;
    method INITTIME() { $init-time-num }

    my $escapes := nqp::hash(
     "\0",   '\0',
     '$',    '\$',
     '@',    '\@',
     '%',    '\%',
     '&',    '\&',
     '{',    '\{',
     "\b",   '\b',
     "\x0A", '\n',
     "\r",   '\r',
     "\t",   '\t',
     '"',    '\"',
     '\\',   '\\\\',
    );

    method PERLIFY-STR(Str \string) {
        sub char-to-escapes(Str $char) {
#?if moar
            '\x[' ~ $char.NFC.list.map({ .fmt('%0x') }).join(',') ~ ']'
#?endif
#?if !moar
            $char.ord.fmt('\x[%x]')
#?endif
        }

        # Under NFG-supporting implementations, must be sure that any leading
        # combiners are escaped, otherwise they will be combined onto the "
        # under concatenation closure, which ruins round-tripping. Also handle
        # the \r\n grapheme correctly.
        my str $to-escape = nqp::unbox_s(string);
        my str $escaped = '';

        my int $chars = nqp::chars($to-escape);
        my int $i = -1;
        while ($i = $i + 1) < $chars {
            my str $char = nqp::substr($to-escape, $i, 1);
#?if moar
            my int $ord = nqp::ord($char);
            $escaped ~= nqp::isge_i($ord,256)
              && +uniprop($ord,'Canonical_Combining_Class')
              ?? char-to-escapes($char)
              !! nqp::iseq_s($char,"\r\n") ?? '\r\n' !!
#?endif
#?if !moar
            $escaped ~=
#?endif

              nqp::existskey($escapes,$char)
                ?? nqp::atkey($escapes,$char)
                !! nqp::iscclass(nqp::const::CCLASS_PRINTING,$char,0)
                  ?? $char
                  !! char-to-escapes($char);
        }
        $escaped
    }

    # easy access to compile options
    my Mu $compiling-options := nqp::atkey(%*COMPILING, '%?OPTIONS');

    # running with --ll-exception
    method LL-EXCEPTION() {
        nqp::existskey($compiling-options, 'll-exception')
          ?? '--ll-exception'
          !! Empty
    }
    # running with --profile
    method PROFILE() {
        nqp::existskey($compiling-options, 'profile')
          ?? '--profile'
          !! Empty
    }
    # running with --optimize=X
    method OPTIMIZE() {
        nqp::existskey($compiling-options, 'optimize')
          ?? '--optimize=' ~ nqp::atkey($compiling-options, 'optimize')
          !! Empty
    }
    # whatever specified with -I
    method INCLUDE() {
        nqp::existskey($compiling-options,'I')
          ?? do {
                my $I := nqp::atkey($compiling-options,'I');
                nqp::islist($I) ?? $I !! nqp::list($I)
             }
          !! nqp::list()
    }

#?if moar
    method PRECOMP-EXT()    { "moarvm" }
    method PRECOMP-TARGET() { "mbc"    }
#?endif
#?if jvm
    method PRECOMP-EXT()    { "jar" }
    method PRECOMP-TARGET() { "jar" }
#?endif

    method get-local-timezone-offset() {
        my $utc     = time;
        my Mu $fia := nqp::p6decodelocaltime(nqp::unbox_i($utc));

        DateTime.new(
          :year(nqp::atpos_i($fia,5)),
          :month(nqp::atpos_i($fia,4)),
          :day(nqp::atpos_i($fia,3)),
          :hour(nqp::atpos_i($fia,2)),
          :minute(nqp::atpos_i($fia,1)),
          :second(nqp::atpos_i($fia,0)),
        ).posix(True) - $utc;
    }

# Keep track of the differences between TAI and UTC for internal use.
# The "BEGIN" and "END" comments are for tools/update-tai-utc.pl.
#
# Some handy tables:
# http://tf.nist.gov/pubs/bulletin/leapsecond.htm
# http://hpiers.obspm.fr/eop-pc/earthor/utc/TAI-UTC_tab.html

    my int $initial-offset = 10;
    # TAI - UTC at the Unix epoch (1970-01-01T00:00:00Z).

    my $leap-second-dates :=
        #BEGIN leap-second-dates
        (
        '1972-06-30',
        '1972-12-31',
        '1973-12-31',
        '1974-12-31',
        '1975-12-31',
        '1976-12-31',
        '1977-12-31',
        '1978-12-31',
        '1979-12-31',
        '1981-06-30',
        '1982-06-30',
        '1983-06-30',
        '1985-06-30',
        '1987-12-31',
        '1989-12-31',
        '1990-12-31',
        '1992-06-30',
        '1993-06-30',
        '1994-06-30',
        '1995-12-31',
        '1997-06-30',
        '1998-12-31',
        '2005-12-31',
        '2008-12-31',
        '2012-06-30',
        '2015-06-30',
        '2016-12-31',
        )
        #END leap-second-dates
    ;

    # our %leap-seconds =
    #     @leap-second-dates Z=> $initial-offset + 1 .. *;

    # So for any date $d in @leap-second-dates, $d 23:59:00 UTC
    # is the leap second that made (or will make) UTC
    # %leap-seconds{$d} seconds behind TAI.

    # Ambiguous POSIX times.
    my $leap-second-posix :=
        #BEGIN leap-second-posix
        (
          78796800,
          94694400,
         126230400,
         157766400,
         189302400,
         220924800,
         252460800,
         283996800,
         315532800,
         362793600,
         394329600,
         425865600,
         489024000,
         567993600,
         631152000,
         662688000,
         709948800,
         741484800,
         773020800,
         820454400,
         867715200,
         915148800,
        1136073600,
        1230768000,
        1341100800,
        1435708800,
        1483228800,
        )
        #END leap-second-posix
    ;

    my $dates    := nqp::getattr($leap-second-dates,List,'$!reified');
    my $posixes  := nqp::getattr($leap-second-posix,List,'$!reified');
    my int $elems = nqp::elems($dates);

    method is-leap-second-date(\date) {
        my str $date = nqp::unbox_s(date);
        my int $i    = -1;
        Nil while ($i = $i + 1) < $elems && $date gt nqp::atpos($dates,$i);
        $i < $elems && $date eq nqp::atpos($dates,$i);
    }

    method tai-from-posix(\posix,$prefer-leap-second = False) {
        my Int $p = posix.floor;
        my int $i = -1;
        Nil while ($i = $i + 1) < $elems && $p > nqp::atpos($posixes,$i);
        posix + $initial-offset + $i +
          ($i < $elems && !$prefer-leap-second && $p == nqp::atpos($posixes,$i))
    }

    method posix-from-tai(\tai) {
        my Int $t = tai.floor - $initial-offset;
        my int $i = -1;
        Nil while ($i = $i + 1) < $elems && nqp::atpos($posixes,$i) < ($t - $i);
        tai - $initial-offset - $i,
          nqp::p6bool($i < $elems && nqp::atpos($posixes,$i) == $t - $i)
    }

    my $initializers;
#nqp::print("running mainline\n");
#method INITIALIZERS() { $initializers }

    method REGISTER-DYNAMIC(Str:D \name, &code, Str $version = '6.c' --> Nil) {
#nqp::print("Registering ");
#nqp::print(name);
#nqp::print("\n");
        nqp::stmts(
          (my str $with = $version ~ "\0" ~ name),
          nqp::if(
            nqp::existskey(
              nqp::unless($initializers,$initializers := nqp::hash),
              $with
            ),
            (die "Already have initializer for '{name}' ('$version')"),
            nqp::bindkey($initializers,$with,&code)
          ),
          nqp::unless(                                 # first come, first kept
            nqp::existskey($initializers,nqp::unbox_s(name)),
            nqp::bindkey($initializers,nqp::unbox_s(name),&code)
          )
        )
    }
    method INITIALIZE-DYNAMIC(str \name) {
#nqp::print("Initializing");
#nqp::print(name);
#nqp::print("\n");
        nqp::stmts(
          (my str $with = nqp::getcomp('perl6').language_version ~ "\0" ~ name),
          nqp::if(
            nqp::existskey(
              nqp::unless($initializers,$initializers := nqp::hash),
              $with
            ),
            nqp::atkey($initializers,$with)(),
            nqp::if(
              nqp::existskey($initializers,name),
              nqp::atkey($initializers,name)(),
              Failure.new(X::Dynamic::NotFound.new(:name(name)))
            )
          )
        )
    }

    method EXPAND-LITERAL-RANGE(Str:D \x,$list) {
        my str $s      = nqp::unbox_s(x);
        my int $chars  = nqp::chars($s);
        my Mu $result := nqp::list();
        my int $start  = 1;
        my int $found  = nqp::index($s,'..',$start);

        # found and not at the end without trail
        while nqp::isne_i($found,-1) && nqp::isne_i($found,$chars-2) {

            if $found - $start -> $unsplit {
                nqp::splice(
                  $result,
                  nqp::split("",nqp::substr($s,$start - 1,$unsplit)),
                  nqp::elems($result),
                  0
                )
            }

            # add the range excluding last (may be begin point next range)
            my int $from = nqp::ordat($s,$found - 1) - 1;
            my int $to   = nqp::ordat($s,$found + 2);
            nqp::push($result,nqp::chr($from))
              while nqp::islt_i($from = $from + 1,$to);

            # look for next range
            $found = nqp::index($s,'..',$start = $found + 3);
        }

        # add final bits
        nqp::splice(
          $result,
          nqp::split("",nqp::substr($s,$start - 1)),
          nqp::elems($result),
          0
        ) if nqp::isle_i($start,$chars);

        $list ?? $result !! nqp::join("",$result)
    }

    my int $VERBATIM-EXCEPTION = 0;
    method VERBATIM-EXCEPTION($set?) {
        my int $value = $VERBATIM-EXCEPTION;
        $VERBATIM-EXCEPTION = $set if defined($set);
        $value
    }

    method MAKE-ABSOLUTE-PATH(Str:D $path, Str:D $abspath) {
        if $path.ord == 47 {              # 4x faster substr($path,0,1) eq "/"
            $path
        }
        elsif $path.substr-eq(":",1) {  # assume C: something
            if $path.substr-eq("/",2) { #  assume C:/ like prefix
                $path
            }
            elsif !$abspath.starts-with(substr($path,0,2)) {
                die "Can not set relative dir from different roots";
            }
            else {
                $abspath ~ substr($path,2)
            }
        }
        else {                            # assume relative path
            $abspath ~ $path;
        }
    }

    method MAKE-BASENAME(Str:D \abspath) {
        my str $abspath = nqp::unbox_s(abspath);
        my int $offset  = nqp::rindex($abspath,'/');
        nqp::iseq_i($offset,-1)
          ?? abspath
          !! nqp::p6box_s(nqp::substr($abspath,$offset + 1));
    }

    method MAKE-EXT(Str:D \basename) {
        my str $basename = nqp::unbox_s(basename);
        my int $offset   = nqp::rindex($basename,'.');
        nqp::iseq_i($offset,-1)
          ?? ''
          !! nqp::p6box_s(nqp::substr($basename,$offset + 1));
    }

    my $clean-parts-nul := nqp::hash( '..', 1, '.', 1, '', 1);
    method MAKE-CLEAN-PARTS(Str:D \abspath) {
        my str $abspath = nqp::unbox_s(abspath);
        my $parts := nqp::split('/',$abspath);

        # handle //unc/ on win
        if nqp::iseq_s(nqp::atpos($parts,1),'')        # //
          && nqp::iseq_s(nqp::atpos($parts,0),'') {    # and no C: like stuff
            my str $front = nqp::join('/',nqp::list(   # collapse to '//unc/'
                nqp::atpos($parts,0),
                nqp::atpos($parts,1),
                nqp::atpos($parts,2),
            ));
            nqp::splice($parts,nqp::list($front),0,3); # and replace
        }

        # front part cleanup
        my $empty := nqp::list();
        nqp::splice($parts,$empty,1,1)
          while nqp::existskey($clean-parts-nul,nqp::atpos($parts,1));

        # recursive ".." and "." handling
        sub updirs($index is copy) {

            # the end
            if $index == 1 {
                nqp::splice($parts,$empty,1,1);
                1
            }

            # something to check
            elsif nqp::atpos($parts,$index - 1) -> $part {
                if nqp::iseq_i(nqp::ord($part),46) { # substr($part,0,1) eq '.'
                    if nqp::iseq_s($part,'..') {
                        updirs($index - 1);
                    }
                    elsif nqp::iseq_s($part,'.') {
                        nqp::splice($parts,$empty,$index,1);
                        updirs($index - 1);
                    }
                    else {
                        nqp::splice($parts,$empty,--$index,2);
                        $index;
                    }
                }
                else {
                    nqp::splice($parts,$empty,--$index,2);
                    $index;
                }
            }

            # nul, just ignore
            else {
                nqp::splice($parts,$empty,$index,1);
                updirs($index);
            }
        }

        # back part cleanup
        my int $checks = nqp::elems($parts) - 1;
        while nqp::isgt_i($checks,1) {
            if nqp::atpos($parts,$checks) -> $part {
                nqp::iseq_s($part,'..')
                  ?? ($checks = updirs($checks))
                  !! nqp::iseq_s($part,'.')
                    ?? nqp::splice($parts,$empty,$checks--,1)
                    !! --$checks;
            }
            else {
                nqp::splice($parts,$empty,$checks--,1);
            }
        }

        # need / at the end
        nqp::push($parts,"");
        $parts
    }

    method REMOVE-ROOT(Str:D \root, Str:D \path) {
        my str $root = nqp::unbox_s(root);
        my str $path = nqp::unbox_s(path);

        nqp::eqat($path,$root,0)
          ?? nqp::p6box_s(nqp::substr($path,nqp::chars($root)))
          !! path;
    }

    method DIR-RECURSE(
      \abspath,
      Mu :$dir  = -> str $elem { nqp::not_i(nqp::eqat($elem,'.',0)) },
      Mu :$file = True
    ) {
        Seq.new(class :: does Iterator {
            has str $!abspath;
            has $!handle;
            has $!dir;
            has $!file,
            has str $!dir-sep;
            has $!todo;
            has $!seen;
            method !SET-SELF(\abspath,$!dir,$!file) {
                $!abspath = abspath;
                if nqp::stat($!abspath,nqp::const::STAT_EXISTS)
                  && nqp::stat($!abspath,nqp::const::STAT_ISDIR) {
                    $!handle := nqp::opendir($!abspath);
                    $!dir-sep = $*SPEC.dir-sep;
                    $!todo   := nqp::list_s;
                    $!seen   := nqp::hash($!abspath,1);
                    $!abspath = nqp::concat($!abspath,$!dir-sep);
                    self
                }
                else {
                    Rakudo::Iterator.Empty
                }
            }
            method new(\ap,\d,\f) { nqp::create(self)!SET-SELF(ap,d,f) }

            method !next() {
                nqp::while(
                  nqp::isnull_s(my str $elem = nqp::nextfiledir($!handle))
                    || nqp::iseq_i(nqp::chars($elem),0),
                  nqp::stmts(
                    nqp::closedir($!handle),
                    nqp::if(
                      nqp::elems($!todo),
                      nqp::stmts(
                        ($!abspath = nqp::pop_s($!todo)),
                        ($!handle := nqp::opendir($!abspath)),
                        ($!abspath = nqp::concat($!abspath,$!dir-sep))
                      ),
                      return ''
                    )
                  )
                );
                $elem
            }
            method pull-one() {
                nqp::while(
                  nqp::chars(my str $entry = self!next),
                  nqp::if(
                    nqp::stat(
                      (my str $path = nqp::concat($!abspath,$entry)),
                      nqp::const::STAT_EXISTS
                    ),
                    nqp::if(
                      nqp::stat($path,nqp::const::STAT_ISREG)
                        && $!file.ACCEPTS($entry),
                      (return $path),
                      nqp::if(
                        nqp::stat($path,nqp::const::STAT_ISDIR)
                          && $!dir.ACCEPTS($entry),
                        nqp::stmts(
                          nqp::if(
                            nqp::fileislink($path),
                            $path = IO::Path.new(
                              $path,:CWD($!abspath)).resolve.abspath
                          ),
                          nqp::unless(
                            nqp::existskey($!seen,$path),
                            nqp::stmts(
                              nqp::bindkey($!seen,$path,1),
                              nqp::push_s($!todo,$path)
                            )
                          )
                        )
                      )
                    )
                  )
                );
                IterationEnd
            }
        }.new(abspath,$dir,$file))
    }

    method FILETEST-E(Str:D \abspath) {
        nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_EXISTS)
    }
    method FILETEST-LE(Str:D \abspath) {
        nqp::lstat(nqp::unbox_s(abspath),nqp::const::STAT_EXISTS)
    }
    method FILETEST-D(Str:D \abspath) {
        my int $d = nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_ISDIR);
        nqp::isge_i($d,0)
          ?? $d
          !! Failure.new(X::IO::Unknown.new(:trying<d>))
    }
    method FILETEST-F(Str:D \abspath) {
        my int $f = nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_ISREG);
        nqp::isge_i($f,0)
          ?? $f
          !! Failure.new(X::IO::Unknown.new(:trying<f>))
    }
    method FILETEST-S(Str:D \abspath) {
        nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_FILESIZE)
    }
    method FILETEST-L(Str:D \abspath) {
        my int $l = nqp::fileislink(nqp::unbox_s(abspath));
        nqp::isge_i($l,0)
          ?? $l
          !! Failure.new(X::IO::Unknown.new(:trying<l>))
    }
    method FILETEST-R(Str:D \abspath) {
        my int $r = nqp::filereadable(nqp::unbox_s(abspath));
        nqp::isge_i($r,0)
          ?? $r
          !! Failure.new(X::IO::Unknown.new(:trying<r>))
    }
    method FILETEST-W(Str:D \abspath) {
        my int $w = nqp::filewritable(nqp::unbox_s(abspath));
        nqp::isge_i($w,0)
          ?? $w
          !! Failure.new(X::IO::Unknown.new(:trying<w>))
    }
    method FILETEST-RW(Str:D \abspath) {
        my str $abspath = nqp::unbox_s(abspath);
        my int $r = nqp::filereadable($abspath);
        my int $w = nqp::filewritable($abspath);
        nqp::isge_i($r,0)
          ?? nqp::isge_i($w,0)
            ?? nqp::bitand_i($r,$w)
            !! Failure.new(X::IO::Unknown.new(:trying<w>))
          !! Failure.new(X::IO::Unknown.new(:trying<r>))
    }
    method FILETEST-X(Str:D \abspath) {
        my int $x = nqp::fileexecutable(nqp::unbox_s(abspath));
        nqp::isge_i($x,0)
          ?? $x
          !! Failure.new(X::IO::Unknown.new(:trying<x>))
    }
    method FILETEST-RWX(Str:D \abspath) {
        my str $abspath = nqp::unbox_s(abspath);
        my int $r = nqp::filereadable($abspath);
        my int $w = nqp::filewritable($abspath);
        my int $x = nqp::fileexecutable($abspath);
        nqp::isge_i($r,0)
          ?? nqp::isge_i($w,0)
            ?? nqp::isge_i($x,0)
              ?? nqp::bitand_i(nqp::bitand_i($r,$w),$x)
              !! Failure.new(X::IO::Unknown.new(:trying<x>))
            !! Failure.new(X::IO::Unknown.new(:trying<w>))
          !! Failure.new(X::IO::Unknown.new(:trying<r>))
    }
    method FILETEST-Z(Str:D \abspath) {
        nqp::iseq_i(
          nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_FILESIZE),0)
    }

    method FILETEST-MODIFIED(Str:D \abspath) {
        nqp::stat_time(nqp::unbox_s(abspath), nqp::const::STAT_MODIFYTIME)
    }
    method FILETEST-ACCESSED(Str:D \abspath) {
        nqp::stat_time(nqp::unbox_s(abspath), nqp::const::STAT_ACCESSTIME)
    }
    method FILETEST-CHANGED(Str:D \abspath) {
        nqp::stat_time(nqp::unbox_s(abspath), nqp::const::STAT_CHANGETIME)
    }

    our class CompilerServices {
        has Mu $!compiler;

        method generate_accessor(str $name, Mu \package_type, str $attr_name, Mu \type, int $rw) {
            $!compiler.generate_accessor($name, package_type, $attr_name, type, $rw);
        }
    }

    method HANDLE-NQP-SPRINTF-ERRORS(Mu \exception) {
        my $vmex := nqp::getattr(nqp::decont(exception), Exception, '$!ex');
        my \payload := nqp::getpayload($vmex);
        if nqp::elems(payload) == 1 {
            if nqp::existskey(payload, 'BAD_TYPE_FOR_DIRECTIVE') {
                X::Str::Sprintf::Directives::BadType.new(
                    type      => nqp::atkey(nqp::atkey(payload, 'BAD_TYPE_FOR_DIRECTIVE'), 'TYPE'),
                    directive => nqp::atkey(nqp::atkey(payload, 'BAD_TYPE_FOR_DIRECTIVE'), 'DIRECTIVE'),
                ).throw
            }
            if nqp::existskey(payload, 'BAD_DIRECTIVE') {
                X::Str::Sprintf::Directives::Unsupported.new(
                    directive => nqp::atkey(nqp::atkey(payload, 'BAD_DIRECTIVE'), 'DIRECTIVE'),
                    sequence  => nqp::atkey(nqp::atkey(payload, 'BAD_DIRECTIVE'), 'SEQUENCE'),
                ).throw
            }
            if nqp::existskey(payload, 'DIRECTIVES_COUNT') {
                X::Str::Sprintf::Directives::Count.new(
                    args-have => nqp::atkey(nqp::atkey(payload, 'DIRECTIVES_COUNT'), 'ARGS_HAVE'),
                    args-used => nqp::atkey(nqp::atkey(payload, 'DIRECTIVES_COUNT'), 'ARGS_USED'),
                ).throw
            }
        }
    }

#- start of generated part of succ/pred ---------------------------------------
#- Generated on 2016-08-10T14:19:20+02:00 by tools/build/makeMAGIC_INC_DEC.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    # normal increment magic chars & incremented char at same index
    my $succ-nlook = '012345678ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxyΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨαβγδεζηθικλμνξοπρστυφχψאבגדהוזחטיךכלםמןנסעףפץצקרשАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮабвгдежзийклмнопрстуфхцчшщъыьэю٠١٢٣٤٥٦٧٨۰۱۲۳۴۵۶۷۸߀߁߂߃߄߅߆߇߈०१२३४५६७८০১২৩৪৫৬৭৮੦੧੨੩੪੫੬੭੮૦૧૨૩૪૫૬૭૮୦୧୨୩୪୫୬୭୮௦௧௨௩௪௫௬௭௮౦౧౨౩౪౫౬౭౮೦೧೨೩೪೫೬೭೮൦൧൨൩൪൫൬൭൮෦෧෨෩෪෫෬෭෮๐๑๒๓๔๕๖๗๘໐໑໒໓໔໕໖໗໘༠༡༢༣༤༥༦༧༨၀၁၂၃၄၅၆၇၈႐႑႒႓႔႕႖႗႘០១២៣៤៥៦៧៨᠐᠑᠒᠓᠔᠕᠖᠗᠘᥆᥇᥈᥉᥊᥋᥌᥍᥎᧐᧑᧒᧓᧔᧕᧖᧗᧘᪀᪁᪂᪃᪄᪅᪆᪇᪈᪐᪑᪒᪓᪔᪕᪖᪗᪘᭐᭑᭒᭓᭔᭕᭖᭗᭘᮰᮱᮲᮳᮴᮵᮶᮷᮸᱀᱁᱂᱃᱄᱅᱆᱇᱈᱐᱑᱒᱓᱔᱕᱖᱗᱘⁰ⁱ⁲⁳⁴⁵⁶⁷⁸₀₁₂₃₄₅₆₇₈ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹⅺ①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂⒃⒄⒅⒆⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴▁▂▃▄▅▆▇⚀⚁⚂⚃⚄❶❷❸❹❺❻❼❽❾꘠꘡꘢꘣꘤꘥꘦꘧꘨꣐꣑꣒꣓꣔꣕꣖꣗꣘꣠꣡꣢꣣꣤꣥꣦꣧꣨꤀꤁꤂꤃꤄꤅꤆꤇꤈꧐꧑꧒꧓꧔꧕꧖꧗꧘꧰꧱꧲꧳꧴꧵꧶꧷꧸꩐꩑꩒꩓꩔꩕꩖꩗꩘꯰꯱꯲꯳꯴꯵꯶꯷꯸０１２３４５６７８🍺🐪';
    my $succ-nchrs = '123456789BCDEFGHIJKLMNOPQRSTUVWXYZbcdefghijklmnopqrstuvwxyzΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩβγδεζηθικλμνξοπρστυφχψωבגדהוזחטיךכלםמןנסעףפץצקרשתБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯбвгдежзийклмнопрстуфхцчшщъыьэюя١٢٣٤٥٦٧٨٩۱۲۳۴۵۶۷۸۹߁߂߃߄߅߆߇߈߉१२३४५६७८९১২৩৪৫৬৭৮৯੧੨੩੪੫੬੭੮੯૧૨૩૪૫૬૭૮૯୧୨୩୪୫୬୭୮୯௧௨௩௪௫௬௭௮௯౧౨౩౪౫౬౭౮౯೧೨೩೪೫೬೭೮೯൧൨൩൪൫൬൭൮൯෧෨෩෪෫෬෭෮෯๑๒๓๔๕๖๗๘๙໑໒໓໔໕໖໗໘໙༡༢༣༤༥༦༧༨༩၁၂၃၄၅၆၇၈၉႑႒႓႔႕႖႗႘႙១២៣៤៥៦៧៨៩᠑᠒᠓᠔᠕᠖᠗᠘᠙᥇᥈᥉᥊᥋᥌᥍᥎᥏᧑᧒᧓᧔᧕᧖᧗᧘᧙᪁᪂᪃᪄᪅᪆᪇᪈᪉᪑᪒᪓᪔᪕᪖᪗᪘᪙᭑᭒᭓᭔᭕᭖᭗᭘᭙᮱᮲᮳᮴᮵᮶᮷᮸᮹᱁᱂᱃᱄᱅᱆᱇᱈᱉᱑᱒᱓᱔᱕᱖᱗᱘᱙ⁱ⁲⁳⁴⁵⁶⁷⁸⁹₁₂₃₄₅₆₇₈₉ⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅫⅱⅲⅳⅴⅵⅶⅷⅸⅹⅺⅻ②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂⒃⒄⒅⒆⒇⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵▂▃▄▅▆▇█⚁⚂⚃⚄⚅❷❸❹❺❻❼❽❾❿꘡꘢꘣꘤꘥꘦꘧꘨꘩꣑꣒꣓꣔꣕꣖꣗꣘꣙꣡꣢꣣꣤꣥꣦꣧꣨꣩꤁꤂꤃꤄꤅꤆꤇꤈꤉꧑꧒꧓꧔꧕꧖꧗꧘꧙꧱꧲꧳꧴꧵꧶꧷꧸꧹꩑꩒꩓꩔꩕꩖꩗꩘꩙꯱꯲꯳꯴꯵꯶꯷꯸꯹１２３４５６７８９🍻🐫';

    # magic increment chars at boundary & incremented char at same index
    my $succ-blook = '9ZzΩωתЯя٩۹߉९৯੯૯୯௯౯೯൯෯๙໙༩၉႙៩᠙᥏᧙᪉᪙᭙᮹᱉᱙⁹₉Ⅻⅻ⑳⒇⒵█⚅❿꘩꣙꣩꤉꧙꧹꩙꯹９🍻🐫';
    my $succ-bchrs = '10AAaaΑΑααאאААаа١٠۱۰߁߀१०১০੧੦૧૦୧୦௧௦౧౦೧೦൧൦෧෦๑๐໑໐༡༠၁၀႑႐១០᠑᠐᥇᥆᧑᧐᪁᪀᪑᪐᭑᭐᮱᮰᱁᱀᱐᱐ⁱ⁰₁₀ⅠⅠⅰⅰ①①⑴⑴⒜⒜▁▁⚀⚀❶❶꘡꘠꣐꣐꣠꣠꤁꤀꧑꧐꧱꧰꩑꩐꯱꯰１０🍻🍺🐫🐪';

    # normal decrement magic chars & incremented char at same index
    my $pred-nlook = '123456789BCDEFGHIJKLMNOPQRSTUVWXYZbcdefghijklmnopqrstuvwxyzΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩβγδεζηθικλμνξοπρστυφχψωבגדהוזחטיךכלםמןנסעףפץצקרשתБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯбвгдежзийклмнопрстуфхцчшщъыьэюя١٢٣٤٥٦٧٨٩۱۲۳۴۵۶۷۸۹߁߂߃߄߅߆߇߈߉१२३४५६७८९১২৩৪৫৬৭৮৯੧੨੩੪੫੬੭੮੯૧૨૩૪૫૬૭૮૯୧୨୩୪୫୬୭୮୯௧௨௩௪௫௬௭௮௯౧౨౩౪౫౬౭౮౯೧೨೩೪೫೬೭೮೯൧൨൩൪൫൬൭൮൯෧෨෩෪෫෬෭෮෯๑๒๓๔๕๖๗๘๙໑໒໓໔໕໖໗໘໙༡༢༣༤༥༦༧༨༩၁၂၃၄၅၆၇၈၉႑႒႓႔႕႖႗႘႙១២៣៤៥៦៧៨៩᠑᠒᠓᠔᠕᠖᠗᠘᠙᥇᥈᥉᥊᥋᥌᥍᥎᥏᧑᧒᧓᧔᧕᧖᧗᧘᧙᪁᪂᪃᪄᪅᪆᪇᪈᪉᪑᪒᪓᪔᪕᪖᪗᪘᪙᭑᭒᭓᭔᭕᭖᭗᭘᭙᮱᮲᮳᮴᮵᮶᮷᮸᮹᱁᱂᱃᱄᱅᱆᱇᱈᱉᱑᱒᱓᱔᱕᱖᱗᱘᱙ⁱ⁲⁳⁴⁵⁶⁷⁸⁹₁₂₃₄₅₆₇₈₉ⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅫⅱⅲⅳⅴⅵⅶⅷⅸⅹⅺⅻ②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂⒃⒄⒅⒆⒇⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵▂▃▄▅▆▇█⚁⚂⚃⚄⚅❷❸❹❺❻❼❽❾❿꘡꘢꘣꘤꘥꘦꘧꘨꘩꣑꣒꣓꣔꣕꣖꣗꣘꣙꣡꣢꣣꣤꣥꣦꣧꣨꣩꤁꤂꤃꤄꤅꤆꤇꤈꤉꧑꧒꧓꧔꧕꧖꧗꧘꧙꧱꧲꧳꧴꧵꧶꧷꧸꧹꩑꩒꩓꩔꩕꩖꩗꩘꩙꯱꯲꯳꯴꯵꯶꯷꯸꯹１２３４５６７８９🍻🐫';
    my $pred-nchrs = '012345678ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxyΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨαβγδεζηθικλμνξοπρστυφχψאבגדהוזחטיךכלםמןנסעףפץצקרשАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮабвгдежзийклмнопрстуфхцчшщъыьэю٠١٢٣٤٥٦٧٨۰۱۲۳۴۵۶۷۸߀߁߂߃߄߅߆߇߈०१२३४५६७८০১২৩৪৫৬৭৮੦੧੨੩੪੫੬੭੮૦૧૨૩૪૫૬૭૮୦୧୨୩୪୫୬୭୮௦௧௨௩௪௫௬௭௮౦౧౨౩౪౫౬౭౮೦೧೨೩೪೫೬೭೮൦൧൨൩൪൫൬൭൮෦෧෨෩෪෫෬෭෮๐๑๒๓๔๕๖๗๘໐໑໒໓໔໕໖໗໘༠༡༢༣༤༥༦༧༨၀၁၂၃၄၅၆၇၈႐႑႒႓႔႕႖႗႘០១២៣៤៥៦៧៨᠐᠑᠒᠓᠔᠕᠖᠗᠘᥆᥇᥈᥉᥊᥋᥌᥍᥎᧐᧑᧒᧓᧔᧕᧖᧗᧘᪀᪁᪂᪃᪄᪅᪆᪇᪈᪐᪑᪒᪓᪔᪕᪖᪗᪘᭐᭑᭒᭓᭔᭕᭖᭗᭘᮰᮱᮲᮳᮴᮵᮶᮷᮸᱀᱁᱂᱃᱄᱅᱆᱇᱈᱐᱑᱒᱓᱔᱕᱖᱗᱘⁰ⁱ⁲⁳⁴⁵⁶⁷⁸₀₁₂₃₄₅₆₇₈ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹⅺ①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂⒃⒄⒅⒆⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴▁▂▃▄▅▆▇⚀⚁⚂⚃⚄❶❷❸❹❺❻❼❽❾꘠꘡꘢꘣꘤꘥꘦꘧꘨꣐꣑꣒꣓꣔꣕꣖꣗꣘꣠꣡꣢꣣꣤꣥꣦꣧꣨꤀꤁꤂꤃꤄꤅꤆꤇꤈꧐꧑꧒꧓꧔꧕꧖꧗꧘꧰꧱꧲꧳꧴꧵꧶꧷꧸꩐꩑꩒꩓꩔꩕꩖꩗꩘꯰꯱꯲꯳꯴꯵꯶꯷꯸０１２３４５６７８🍺🐪';

    # magic decrement chars at boundary & incremented char at same index
    my $pred-blook = '0AaΑαאАа٠۰߀०০੦૦୦௦౦೦൦෦๐໐༠၀႐០᠐᥆᧐᪀᪐᭐᮰᱀᱐⁰₀Ⅰⅰ①⑴⒜▁⚀❶꘠꣐꣠꤀꧐꧰꩐꯰０🍺🐪';
    my $pred-bchrs = '9ZzΩωתЯя٩۹߉९৯੯૯୯௯౯೯൯෯๙໙༩၉႙៩᠙᥏᧙᪉᪙᭙᮹᱉᱙⁹₉Ⅻⅻ⑳⒇⒵█⚅❿꘩꣙꣩꤉꧙꧹꩙꯹９🍻🐫';

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of succ/pred -----------------------------------------

    # number of chars that should be considered for magic .succ/.pred
    method POSSIBLE-MAGIC-CHARS(str \string) {

        # only look at stuff before the last period
        my int $i = nqp::index(string,".");
        nqp::iseq_i($i,-1) ?? nqp::chars(string) !! $i
    }

    # return -1 if string cannot support .succ, else index of last char
    method CAN-SUCC-INDEX(str \string, int \chars) {
        my int $i = chars;
        Nil while nqp::isge_i($i = nqp::sub_i($i,1),0)
          && nqp::iseq_i(nqp::index($succ-nlook,nqp::substr(string,$i,1)),-1)
          && nqp::iseq_i(nqp::index($succ-blook,nqp::substr(string,$i,1)),-1);
        $i
    }

    # next logical string frontend, hopefully inlineable (pos >= 0)
    method SUCC(str \string, int \pos) {
        my int $at = nqp::index($succ-nlook,nqp::substr(string,pos,1));
        nqp::iseq_i($at,-1)
          ?? SUCC-NOT-SO-SIMPLE(string,pos)
          !! nqp::replace(string,pos,1,nqp::substr($succ-nchrs,$at,1))
    }

    # slow path for next logical string
    sub SUCC-NOT-SO-SIMPLE(str \string, int \pos) {

        # nothing magical going on
        my int $at = nqp::index($succ-blook,nqp::substr(string,pos,1));
        if nqp::iseq_i($at,-1) {
            string
        }

        # we have a boundary
        else {

            # initial change
            my int $i   = pos;
            my str $str = nqp::replace(string,$i,1,
              nqp::substr($succ-bchrs,nqp::add_i($at,$at),2));

            # until we run out of chars to check
            while nqp::isge_i($i = nqp::sub_i($i,1),0) {

                # not an easy magical
                $at = nqp::index($succ-nlook,nqp::substr($str,$i,1));
                if nqp::iseq_i($at,-1) {

                    # done if not a boundary magical either
                    $at = nqp::index($succ-blook,nqp::substr($str,$i,1));
                    return $str if nqp::iseq_i($at,-1);

                    # eat first of last magical, and continue
                    $str = nqp::replace($str,$i,2,
                      nqp::substr($succ-bchrs,nqp::add_i($at,$at),2));
                }

                # normal magical, eat first of last magical, and we're done
                else {
                   return nqp::replace($str,$i,2,
                     nqp::substr($succ-nchrs,$at,1));
                }
            }
            $str
        }
    }

    # previous logical string frontend, hopefully inlineable
    method PRED(str \string, int \pos) {
        my int $at = nqp::index($pred-nlook,nqp::substr(string,pos,1));
        nqp::iseq_i($at,-1)
          ?? PRED-NOT-SO-SIMPLE(string,pos)
          !! nqp::replace(string,pos,1,nqp::substr($pred-nchrs,$at,1))
    }

    # slow path for previous logical string
    sub PRED-NOT-SO-SIMPLE(str \string, int \pos) {

        # nothing magical going on
        my int $at = nqp::index($pred-blook,nqp::substr(string,pos,1));
        if nqp::iseq_i($at,-1) {
            string
        }

        # we have a boundary
        else {

            # initial change
            my int $i   = pos;
            my str $str = nqp::replace(string,$i,1,
              nqp::substr($pred-bchrs,$at,1));

            # until we run out of chars to check
            while nqp::isge_i($i = nqp::sub_i($i,1),0) {

                # not an easy magical
                $at = nqp::index($pred-nlook,nqp::substr($str,$i,1));
                if nqp::iseq_i($at,-1) {

                    # not a boundary magical either
                    $at = nqp::index($pred-blook,nqp::substr($str,$i,1));
                    nqp::iseq_i($at,-1)
                      ?? fail('Decrement out of range')
                      !! ($str = nqp::replace($str,$i,1,
                           nqp::substr($pred-bchrs,$at,1)))
                }

                # normal magical, update, and we're done
                else {
                    return nqp::replace($str,$i,1,
                      nqp::substr($pred-nchrs,$at,1))
                }
            }
            Failure.new('Decrement out of range')
        }
    }

    method WALK-AT-POS(\target,\indices) is raw {
        my $target   := target;
        my $indices  := nqp::getattr(indices,List,'$!reified');
        my int $elems = nqp::elems($indices);
        my int $i     = -1;
        $target := $target.AT-POS(nqp::atpos($indices,$i))
          while nqp::islt_i(++$i,$elems);
        $target
    }

    proto method coremap(|) { * }

    multi method coremap(\op, Associative \h, Bool :$deep) {
        my @keys = h.keys;
        hash @keys Z self.coremap(op, h{@keys}, :$deep)
    }

    multi method coremap(\op, \obj, Bool :$deep) {
        my \iterable = obj.DEFINITE && nqp::istype(obj, Iterable)
                ?? obj
                !! obj.list;

        my \result := class :: does SlippyIterator {
            has &!block;
            has $!source;

            method new(&block, $source) {
                my $iter := nqp::create(self);
                nqp::bindattr($iter, self, '&!block', &block);
                nqp::bindattr($iter, self, '$!source', $source);
                $iter
            }

            method is-lazy() {
                $!source.is-lazy
            }

            method pull-one() is raw {
                my int $redo = 1;
                my $value;
                my $result;
                if $!slipping && nqp::not_i(nqp::eqaddr(($result := self.slip-one),IterationEnd)) {
                    $result
                }
                elsif nqp::eqaddr(($value := $!source.pull-one),IterationEnd) {
                    $value
                }
                else {
                    nqp::while(
                        $redo,
                        nqp::stmts(
                            $redo = 0,
                            nqp::handle(
                                nqp::stmts(
                                    nqp::if(
                                        $deep,
                                        nqp::if(
                                            nqp::istype($value, Iterable),
                                            ($result := Rakudo::Internals.coremap(&!block, $value, :$deep).item),
                                            ($result := &!block($value))
                                        ),
                                        ($result := &!block($value))
                                    ),
                                    nqp::if(
                                        nqp::istype($result, Slip),
                                        nqp::stmts(
                                            ($result := self.start-slip($result)),
                                            nqp::if(
                                                nqp::eqaddr($result, IterationEnd),
                                                nqp::stmts(
                                                    ($value := $!source.pull-one()),
                                                    ($redo = 1 unless nqp::eqaddr($value, IterationEnd))
                                            ))
                                        ))
                                ),
                                'NEXT', nqp::stmts(
                                    ($value := $!source.pull-one()),
                                    nqp::eqaddr($value, IterationEnd)
                                        ?? ($result := IterationEnd)
                                        !! ($redo = 1)),
                                'REDO', $redo = 1,
                                'LAST', ($result := IterationEnd))),
                        :nohandler);
                    $result
                }
            }
        }.new(op, iterable.iterator);

        my $type = nqp::istype(obj, List) ?? obj.WHAT !! List; # keep subtypes of List
        my \buffer := IterationBuffer.new;
        result.push-all(buffer);
        my \retval = $type.new;
        nqp::bindattr(retval, List, '$!reified', buffer);
        nqp::iscont(obj) ?? retval.item !! retval;
    }

}

# expose the number of bits a native int has
my constant $?BITS = do {
    my int $a = 0x1ffffffff;
    nqp::iseq_i($a,8589934591) ?? 64 !! 32
}

# we need this to run *after* the mainline of Rakudo::Internals has run
Rakudo::Internals.REGISTER-DYNAMIC: '&*EXIT', {
    PROCESS::<&EXIT> := sub exit($status) {
        state $exit;
        $exit = $status;

        once {
            Rakudo::Internals.THE_END();
            nqp::exit(nqp::unbox_i($exit.Int));
        }
        $exit;
    }
}

sub exit($status = 0) { &*EXIT($status) }

# vim: ft=perl6 expandtab sw=4
