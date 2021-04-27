my class DateTime { ... }
my role  IO { ... }
my class IO::Handle { ... }
my class IO::Path { ... }
my class Rakudo::Metaops { ... }
my class List::Reifier { ... }
my class Proc { ... }
my class Proc::Async { ... }

my class X::Assignment::ToShaped { ... }
my class X::IllegalDimensionInShape { ... }
my class X::IllegalOnFixedDimensionArray { ... }
my class X::Localizer::NoContainer { ... }
my class X::Str::Sprintf::Directives::BadType { ... }
my class X::Str::Sprintf::Directives::Count { ... }
my class X::Str::Sprintf::Directives::Unsupported { ... }
my class X::TypeCheck { ... }

# Marker symbol for 6.c-mode regex boolification.
my class Rakudo::Internals::RegexBoolification6cMarker { }

my class Rakudo::Internals {

    method SLICE_HUH(\object, @nogo, %d, %adv) {
        @nogo.unshift('delete')  # recover any :delete if necessary
          if @nogo && @nogo[0] ne 'delete' && %adv.EXISTS-KEY('delete');
        for <delete exists kv p k v> -> $valid { # check all valid params
            if nqp::existskey(%d,nqp::unbox_s($valid)) {
                nqp::deletekey(%d,nqp::unbox_s($valid));
                @nogo.push($valid);
            }
        }

        Failure.new(X::Adverb.new(
          :what<slice>,
          :source(try { object.VAR.name } // object.^name),
          :unexpected(%d.keys),
          :nogo(@nogo),
        ))
    }

    # for use in nqp::splice
    my $empty := nqp::list;

    our class CompilerServices {
        has Mu $!compiler;
        has Mu $!current-match;

        method generate_accessor(str $name, Mu \package_type, str $attr_name, Mu \type, int $rw) {
            $!compiler.generate_accessor(
              $!current-match, $name, package_type, $attr_name, type, $rw);
        }
        method generate_buildplan_executor(Mu \obj, Mu \buildplan) {
            $!compiler.generate_buildplan_executor(
              $!current-match, obj, buildplan)
        }
    }

    # Marker symbol for lexicals that we have lowered away.
    class LoweredAwayLexical {
        method dynamic() { False }
    }

    method RANGE-AS-ints ($range, $exception) {
        # Convert a Range to min/max values that can fit into an `int`
        # Treats values smaller than int.Range.min as int.Range.min
        # Treats values larger than int.Range.max as int.Range.max
        # Throws $exception for non-Numeric ranges or ranges with any NaN endpoints
        # If $exception is a Str, calls `die $exception`
        my $min := $range.min;
        my $max := $range.max;
        nqp::unless(
             nqp::istype($min, Numeric) && nqp::isfalse($min.isNaN)
          && nqp::istype($max, Numeric) && nqp::isfalse($max.isNaN),
          nqp::if(nqp::istype($exception, Str), die($exception), $exception.throw));

        # Get rid of Infs
        $min := Int($min + $range.excludes-min) // -2**63;
        $max := Int($max - $range.excludes-max) //  2**63-1;

        # we have isbig_I, but it tells whether the value is above max int32 value
        nqp::if( nqp::islt_I(nqp::decont($min), -2**63),
                                         $min = -2**63);
        nqp::if( nqp::isgt_I(nqp::decont($max),  2**63-1),
                                         $max =  2**63-1);
        ($min, $max);
    }

    method GistList2list_s(List:D \list) {
        my \values := nqp::getattr(list,List,'$!reified');
        my \parts  := nqp::list_s;
        nqp::while(
          nqp::elems(values),
          nqp::push_s(parts,nqp::shift(values).gist)
        );
        parts
    }

    method StrList2list_s(List:D \list) {
        my \values := nqp::getattr(list,List,'$!reified');
        my \parts  := nqp::list_s;
        nqp::while(
          nqp::elems(values),
          nqp::push_s(parts,nqp::shift(values).Str)
        );
        parts
    }

    method SET_LEADING_DOCS($obj, $docs) {
        my $current_why := $obj.WHY;

        if $current_why {
            my $end := nqp::elems($*POD_BLOCKS) - 1;
            my $i   := $end;

            while $i >= 0 {
                if $docs === nqp::atpos($*POD_BLOCKS, $i) {
                    nqp::splice($*POD_BLOCKS, $empty, $i, 1);
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
        for nqp::hllize(@*PACKAGES).list {
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

    method createENV() {
        my $hash := nqp::hash;
        my $iter := nqp::iterator(nqp::getenvhash);
        nqp::while(
          $iter,
          nqp::bindkey(
            $hash,
            nqp::iterkey_s(nqp::shift($iter)),
            nqp::assign(
              nqp::p6scalarfromdesc(nqp::null),
              val(nqp::box_s(nqp::iterval($iter),Str))
            )
          )
        );
        $hash
    }

    # Helper method for prefix:<let>/prefix:<temp>, which really do the same
    # thing apart from where they store data.  Takes the IterationBuffer in
    # which to save data, the container to be inspected, and the type of op
    # for any error messaging.
    method TEMP-LET(\restore, \cont, str $localizer) is raw {
        my int $i = nqp::elems(restore);

        nqp::while(
          nqp::isgt_i($i,0),
          nqp::if(
            nqp::eqaddr(nqp::atpos(restore,($i = nqp::sub_i($i,2))),cont),
            (return-rw cont)
          )
        );

        nqp::if(
          nqp::istype(cont,Failure),
          cont.exception.throw,
          nqp::stmts(
            nqp::push(restore,cont),
            nqp::if(
              nqp::iscont(cont),
              nqp::push(restore,nqp::decont(cont)),
              nqp::if(
                nqp::istype(cont,Array),
                nqp::push(restore,cont.clone),
                nqp::if(
                  nqp::istype(cont,Hash),
                  nqp::push(restore,
                    nqp::p6bindattrinvres(
                      Hash.^parameterize(Mu,Mu).new,
                      Hash, '$!descriptor',
                      nqp::getattr(cont, Hash, '$!descriptor')).STORE: cont),
                  nqp::stmts(
                    nqp::pop(restore),  # lose the erroneously pushed value
                    X::Localizer::NoContainer.new(:$localizer).throw
                  )
                )
              )
            )
          )
        );
        cont
    }

    # fast whitespace trim: str to trim, str to store trimmed str
    method !TRIM(\string, \trimmed --> Nil) {
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
            self!TRIM(nqp::substr($str,0,$index),key);
            self!TRIM(nqp::substr($str,$index + 1,nqp::chars($str) - $index),value);
        }
        elsif nqp::islt_i($index,0) {
            self!TRIM($str,key);
            value = '';
        }
        else {
            key = '';
            self!TRIM(nqp::substr($str,1,nqp::chars($str) - 1),value);
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
    # Fast mapping for identicals
    ### If updating encodings here, also update src/core.c/Encoding/Registry.pm6
#?if moar
    my constant $encodings = nqp::hash(
#?endif
#?if !moar
    my $encodings := nqp::hash(
#?endif
      # utf8
      'utf8',            'utf8',
      'utf-8',           'utf8',
      # utf8-c8
      'utf8-c8',         'utf8-c8',
      'utf8c8',          'utf8-c8',
      'utf-8-c8',        'utf8-c8',
      # utf16
      'utf16',           'utf16',
      'utf-16',          'utf16',
      # utf16le
      'utf16le',         'utf16le',
      'utf-16le',        'utf16le',
      'utf16-le',        'utf16le',
      'utf-16-le',       'utf16le',
      # utf16be
      'utf16be',         'utf16be',
      'utf-16be',        'utf16be',
      'utf16-be',        'utf16be',
      'utf-16-be',       'utf16be',
#?if !moar
      # utf32
      'utf32',           'utf32',
      'utf-32',          'utf32',
#?endif
      # ascii
      'ascii',           'ascii',
      # iso-8859-1 according to http://de.wikipedia.org/wiki/ISO-8859-1
      'iso-8859-1',      'iso-8859-1',
      'iso_8859-1:1987', 'iso-8859-1',
      'iso_8859-1',      'iso-8859-1',
      'iso-ir-100',      'iso-8859-1',
      'latin1',          'iso-8859-1',
      'latin-1',         'iso-8859-1',
      'csisolatin1',     'iso-8859-1',
      'l1',              'iso-8859-1',
      'ibm819',          'iso-8859-1',
      'cp819',           'iso-8859-1',
      # windows-1251
      'windows-1251',    'windows-1251',
      'windows1251',    'windows-1251',
      # windows-1252
      'windows-1252',    'windows-1252',
      'windows1252',    'windows-1252',
      # ShiftJIS
      'windows-932',     'windows-932',
      'windows932',      'windows-932',
      # GB2312
      'gb2312',          'gb2312',
      # GB18030
      'gb18030',         'gb18030',
    );
    method NORMALIZE_ENCODING(\encoding) {
        nqp::if(
          nqp::isconcrete(encoding),
          nqp::ifnull(
            nqp::atkey($encodings,encoding),
            nqp::ifnull(
              nqp::atkey($encodings,nqp::lc(encoding)),
              nqp::lc(encoding)
            )
          ),
          'utf8'
        )
    }

    # 1 if all elements of given type, otherwise 0
    method ALL_TYPE(\values,\type) {
        nqp::if(
          (my int $elems = values.elems),   # reifies
          nqp::stmts(
            (my $values := nqp::getattr(values,List,'$!reified')),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::istype(nqp::atpos($values,$i),type),
              nqp::null
            ),
            nqp::iseq_i($i,$elems)
          )
        )
    }

    # 1 if all elems defined && type, otherwise 0
    method ALL_DEFINED_TYPE(\values,\type) {
        nqp::if(
          (my int $elems = values.elems),   # reifies
          nqp::stmts(
            (my $values := nqp::getattr(values,List,'$!reified')),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::istype(nqp::atpos($values,$i),type)
                && nqp::defined(nqp::atpos($values,$i)),
              nqp::null
            ),
            nqp::iseq_i($i,$elems)
          )
        )
    }

    # 1 if any element of defined && type, otherwise 0
    method ANY_DEFINED_TYPE(\values,\type) {
        nqp::if(
          (my int $elems = values.elems),   # reifies
          nqp::stmts(
            (my $values := nqp::getattr(values,List,'$!reified')),
            (my int $i = -1),
            nqp::until(
              nqp::iseq_i(($i = nqp::add_i($i,1)),$elems)
                || (nqp::istype(nqp::atpos($values,$i),type)
                     && nqp::defined(nqp::atpos($values,$i))),
              nqp::null
            ),
            nqp::isne_i($i,$elems)
          )
        )
    }

    method TRANSPOSE(str $string, str $original, str $final) {
        nqp::join($final,nqp::split($original,$string))
    }
    method TRANSPOSE-ONE(Str:D $string, Str:D $original, Str:D $final) {
        nqp::iseq_i((my int $index = nqp::index($string, $original)), -1)
          ?? $string
          !! nqp::concat(
               nqp::substr($string,0,$index),
               nqp::concat(
                 $final,
                 nqp::substr($string,nqp::add_i($index,nqp::chars($original)))
               )
             )
    }

    my constant \SHAPE-STORAGE-ROOT := do {
        my Mu $root := nqp::newtype(nqp::knowhow(), 'Uninstantiable');
        nqp::setdebugtypename($root, 'MultiDimArray root');
        nqp::setparameterizer($root, -> $, $key {
            # We "steal" the meta-object for the multi-dim storage.
            my $dims := $key.elems.pred;
            my $meta := $key.AT-POS(0);
            my $type := $key.AT-POS(1);
            my $dim_type := nqp::newtype($meta, 'MultiDimArray');
            nqp::composetype($dim_type, nqp::hash('array',
                nqp::hash('dimensions', $dims, 'type', $type)));
            nqp::settypehll($dim_type, 'Raku');
            # Make sure the debug name and various caches are propagated
            # for native arrays (where this type is exposed to userspace).
            if nqp::objprimspec($type) {
                nqp::setdebugtypename($dim_type, $meta.name($dim_type));
                $meta.publish_method_cache($dim_type);
                $meta.publish_type_cache($dim_type);
            }
            $dim_type
        });
        nqp::settypehll($root, 'Raku');
        $root
    }

    method SHAPED-ARRAY-STORAGE(\spec, Mu \meta-obj, Mu \type) {
        my $types := nqp::list(meta-obj);  # meta + type of each dimension
        my $dims  := nqp::list_i;          # elems per dimension

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
                  nqp::istype(($dim := nqp::decont($dim.Int)),Failure),
                  $dim.throw,
                  nqp::if(
                    nqp::isbig_I($dim) || nqp::isle_i($dim,0),
                    X::IllegalDimensionInShape.new(:$dim).throw,
                    nqp::stmts(
                      nqp::push($types,type),
                      nqp::push_i($dims,$dim)
                    )
                  )
                )
              )
            )
          ),
          nqp::stmts(                        # only 1 dim
            nqp::push($types,type),
            nqp::push_i($dims,spec.Int)
          )
        );

        nqp::setdimensions(
          nqp::create(nqp::parameterizetype(SHAPE-STORAGE-ROOT,$types)),
          $dims
        )
    }

    our role ImplementationDetail {
        method new(|) { die self.gist }
        method gist(--> Str:D) {
            "The '{self.^name}' class is a Rakudo-specific
implementation detail and has no serviceable parts inside"
        }
        method Str( --> Str:D) { self.gist }
        method raku(--> Str:D) { self.gist }
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

        proto method STORE(::?CLASS:D: |) {*}
        multi method STORE(::?CLASS:D: Slip:D \slip) {
            nqp::eqaddr(slip,Empty)
              ?? X::AdHoc.new( payload => "Cannot Empty a shaped array as its size is fixed").throw
              !! self.STORE(slip.List)
        }

        # illegal unless overridden for 1dimmed case
        method reverse(::?CLASS:D: |) { self!illegal("reverse") }
        method rotate(::?CLASS:D: |)  { self!illegal("rotate") }

        multi method values(::?CLASS:D:) { Seq.new(self.iterator) }
        multi method keys(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.ShapeIndex(self.shape))
        }
        multi method invert(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.Invert(self.pairs.iterator))
        }

        # These work on the flat view
        method roll(|c)         { self.flat.roll(|c) }
        method pick(|c)         { self.flat.pick(|c) }
        method permutations(|c) { self.flat.permutations(|c) }
        method combinations(|c) { self.flat.combinations(|c) }
        method join(|c)         { self.flat.join(|c) }
        method sort(|c)         { self.flat.sort(|c) }

        multi method gist(::?CLASS:D:) {
            self.gistseen('Array', { self!gist(nqp::create(Array),self.shape) })
        }
        method !gist(@path, @dims) {
            if @dims.elems == 1  {
                '[' ~ (^@dims[0]).map(
                    -> $elem {
                        given ++$ {
                            when 11 { '...' }
                            when 12 { last }
                            default { self.AT-POS(|@path, $elem).gist }
                        }
                    }
                ).join(' ') ~ ']';
            } else {
                my @nextdims = @dims[1..^@dims.elems];
                '[' ~ (^@dims[0]).map(
                    -> $elem {
                        given ++$ {
                            when 11 { '...' }
                            when 12 { last }
                            default { self!gist((flat @path, $elem), @nextdims) }
                        }
                    }
                ).join("\n" ~ (' ' x @path.elems + 1)) ~ ']';
            }
        }

        multi method raku(::?CLASS:D \SELF:) {
            SELF.rakuseen('Array', {
                self.^name
                ~ '.new(:shape'
                ~ nqp::decont(self.shape).raku
                ~ ', '
                ~ self!perl(nqp::create(Array), self.shape)
                ~ ')'
                ~ (nqp::iscont(SELF) ?? '.item' !! '')
            })
        }
        method !perl(@path, @dims) {
            if @dims.elems == 1 {
                 '[' ~
                    (^@dims[0]).map({ nqp::decont(self.AT-POS(|@path, $_)).raku }).join(', ') ~
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
            $!lock := Lock::Async.new;
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

            proto method int(|) {*}
            multi method int(Mu:D \n) { n.Int }
            multi method int(Mu:U \n) { n.Numeric.Int }

            proto method float(|) {*}
            multi method float(Numeric:D \n) { n }
            multi method float(Mu \n) { n.Numeric }
        }
        unless $sprintfHandlerInitialized {
            nqp::sprintfaddargumenthandler(SprintfHandler.new);
            $sprintfHandlerInitialized = 1;
        }
    }

    method MAYBE-STRING(Mu \thing, Str:D :$method = 'gist' --> Str:D) {
        my Mu \decont = nqp::decont(thing);
        nqp::can(decont, nqp::decont_s($method))
          ?? decont."$method"()
          !! nqp::can(decont.HOW, 'name')
            ?? decont.^name
            !! '?'
    }
    method SHORT-STRING(Mu \thing, Str:D :$method = 'gist' --> Str:D) {
        my str $str = nqp::unbox_s(self.MAYBE-STRING: thing, :$method);
        nqp::isnull_s($str)
          ?? ""
          !! nqp::isgt_i(nqp::chars($str),23)
            ?? nqp::concat(nqp::substr($str, 0, 20), '...')
            !! $str
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
        nqp::hllbool(
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

    my num $init-time-num = nqp::div_n(nqp::time,1000000000e0);
    method INITTIME() is raw { $init-time-num }

#?if moar
    my $init-thread := nqp::currentthread();
    method INITTHREAD() { $init-thread }
#?endif

    # easy access to compile options
    my Mu $compiling-options := nqp::ifnull(  # cannot be lazy
      nqp::atkey(nqp::getlexdyn('%*COMPILING'),'%?OPTIONS'),
      nqp::hash
    );
    my $LL-EXCEPTION := my $PROFILE := my $OPTIMIZE :=
      my $STAGESTATS := my $INCLUDE := my $E := nqp::null;

    # running with --ll-exception
    method LL-EXCEPTION() {
        nqp::ifnull(
          $LL-EXCEPTION,
          $LL-EXCEPTION := nqp::if(
            nqp::existskey($compiling-options,'ll-exception'),
            '--ll-exception',
            Empty
          )
        )
    }

    # running with --profile
    method PROFILE() {
        nqp::ifnull(
          $PROFILE,
          $PROFILE := nqp::if(
            nqp::existskey($compiling-options,'profile'),
            '--profile',
            Empty
          )
        )
    }

    # running with --optimize=X
    method OPTIMIZE() {
        nqp::ifnull(
          $OPTIMIZE,
          $OPTIMIZE := nqp::if(
            nqp::existskey($compiling-options,'optimize'),
            nqp::concat('--optimize=',nqp::atkey($compiling-options,'optimize')),
            Empty
          )
        )
    }

    # running with --stagestats
    method STAGESTATS() {
        nqp::ifnull(
          $STAGESTATS,
          $STAGESTATS := nqp::if(
            nqp::existskey($compiling-options,'stagestats'),
            '--stagestats',
            Empty
          )
        )
    }

    # whatever specified with -I
    method INCLUDE() {
        nqp::ifnull(
          $INCLUDE,
          $INCLUDE := nqp::p6bindattrinvres(
            nqp::create(List),List,'$!reified',
            nqp::if(
              nqp::existskey($compiling-options,'I'),
              nqp::stmts(
                (my $I := nqp::atkey($compiling-options,'I')),
                nqp::if(nqp::islist($I),$I,nqp::list($I))
              ),
              nqp::list
            )
          )
        )
    }

    # the program to execute, either a script or -e, code
    method PROGRAM() {
        nqp::ifnull(
          $E,
          $E := nqp::if(
            nqp::existskey($compiling-options,'e'),
            ('-e', nqp::atkey($compiling-options,'e')),
            $*PROGRAM-NAME
          )
        )
    }

#?if moar
    method PRECOMP-EXT(--> "moarvm") { }
    method PRECOMP-TARGET(--> "mbc") { }
#?endif
#?if jvm
    method PRECOMP-EXT(   --> "jar") { }
    method PRECOMP-TARGET(--> "jar") { }
#?endif
#?if js
    method PRECOMP-EXT(   --> "js") { }
    method PRECOMP-TARGET(--> "js") { }
#?endif
    method TARGET() { "--target=" ~ Rakudo::Internals.PRECOMP-TARGET }

# Keep track of the differences between TAI and UTC for internal use.
# The "BEGIN" and "END" comments are for tools/add-leap-second.raku.
#
# Some handy tables:
# http://tf.nist.gov/pubs/bulletin/leapsecond.htm
# http://hpiers.obspm.fr/eop-pc/earthor/utc/TAI-UTC_tab.html

    my int constant $initial-offset = 10;
    # TAI - UTC at the Unix epoch (1970-01-01T00:00:00Z).

#?if !js
    my constant $dates = nqp::list_s(
#?endif
#?if js
    my $dates := nqp::list_s(
#?endif
        #BEGIN leap-second-dates
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
        #END leap-second-dates
    );
#?if !js
    my int constant $elems = nqp::elems($dates);
#?endif
#?if js
    my int $elems = nqp::elems($dates);
#?endif

#?if !js
    my constant $daycounts = nqp::list_i(
#?endif
#?if js
    my $daycounts := nqp::list_i(
#?endif
        #BEGIN leap-second-daycount
        41498,
        41682,
        42047,
        42412,
        42777,
        43143,
        43508,
        43873,
        44238,
        44785,
        45150,
        45515,
        46246,
        47160,
        47891,
        48256,
        48803,
        49168,
        49533,
        50082,
        50629,
        51178,
        53735,
        54831,
        56108,
        57203,
        57753,
        #END leap-second-daycount
    );

    method daycount-leapseconds(int $daycount) {
        my int $i = nqp::elems($daycounts);
        nqp::while(
          nqp::isge_i(($i = nqp::sub_i($i,1)),0)
            && nqp::islt_i($daycount,nqp::atpos_i($daycounts,$i)),
          nqp::null
        );
        nqp::isge_i($i,0) && nqp::iseq_i($daycount,nqp::atpos_i($daycounts,$i))
    }

    # our %leap-seconds =
    #     @leap-second-dates Z=> $initial-offset + 1 .. *;

    # So for any date $d in @leap-second-dates, $d 23:59:00 UTC
    # is the leap second that made (or will make) UTC
    # %leap-seconds{$d} seconds behind TAI.

    # Ambiguous POSIX times.
#?if !js
    my constant $posixes = nqp::list_i(
#?endif
#?if js
    my $posixes := nqp::list_i(
#?endif
        #BEGIN leap-second-posix
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
        #END leap-second-posix
    );

    method is-leap-second-date(\date) {
        nqp::hllbool(
          nqp::stmts(
            (my str $date = date),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::isgt_s($date,nqp::atpos_s($dates,$i)),
              nqp::null
            ),
            nqp::islt_i($i,$elems) && nqp::iseq_s($date,nqp::atpos_s($dates,$i))
          )
        )
    }

    method tai-from-posix(\posix, int $prefer-leap-second) {
        my int $p = posix.floor;
        my int $i = -1;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::isgt_i($p,nqp::atpos_i($posixes,$i)),
          nqp::null
        );

        posix + nqp::add_i(
          nqp::add_i($initial-offset,$i),
          nqp::islt_i($i,$elems)
            && nqp::not_i($prefer-leap-second)
            && nqp::iseq_i($p,nqp::atpos_i($posixes,$i))
        )
    }

    # take TAI, return epoch
    method epoch-from-tai(\tai) {
        my int $tai = tai.floor;
        my int $t = $tai - $initial-offset;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::islt_i(nqp::atpos_i($posixes,$i),nqp::sub_i($t,$i)),
          nqp::null
        );
        nqp::sub_i($tai,nqp::add_i($initial-offset,$i))
    }


    # take TAI, return posix
    method posix-from-tai(\tai) {
        my int $t = tai.floor - $initial-offset;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::islt_i(nqp::atpos_i($posixes,$i),nqp::sub_i($t,$i)),
          nqp::null
        );
        tai - nqp::add_i($initial-offset,$i)
    }

    # take TAI, return epoch and if in leap-second
    method posix-and-leap-from-tai(\tai) {
        my int $t = tai.floor - $initial-offset;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::islt_i(nqp::atpos_i($posixes,$i),nqp::sub_i($t,$i)),
          nqp::null
        );
        (tai - nqp::add_i($initial-offset,$i),
         nqp::hllbool(
           nqp::islt_i($i,$elems)
             && nqp::iseq_i(nqp::atpos_i($posixes,$i),nqp::sub_i($t,$i))
         )
        )
    }

    my $initializers;
#nqp::print("running mainline\n");
#method INITIALIZERS() { $initializers }

    method REGISTER-DYNAMIC(Str:D \name, &code, Str $version = '6.c' --> Nil) {
#nqp::say('Registering ' ~ name);
        my str $with = nqp::concat($version, nqp::concat("\0", name));

        nqp::if(
          nqp::existskey(
            nqp::unless($initializers,$initializers := nqp::hash),
            $with
          ),
          (die "Already have initializer for '{name}' ('$version')"),
          nqp::bindkey($initializers,$with,&code)
        );

        nqp::unless(                                 # first come, first kept
          nqp::existskey($initializers,nqp::unbox_s(name)),
          nqp::bindkey($initializers,nqp::unbox_s(name),&code)
        );
    }
    method INITIALIZE-DYNAMIC(str \name) is raw {
#nqp::say('Initializing ' ~ name);
        nqp::ifnull(
          nqp::atkey(
            $initializers,
            nqp::concat(
              nqp::getcomp('Raku').language_version,
              nqp::concat("\0",name)
            )
          ),
          nqp::ifnull(
            nqp::atkey($initializers,name),
            { Failure.new(X::Dynamic::NotFound.new(:name(name))) }
          )
        )();
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

#?if moar
    my constant $clean-parts-nul = nqp::hash(
#?endif
#?if !moar
    my $clean-parts-nul := nqp::hash(
#?endif
      '..', 1, '.', 1, '', 1
    );

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

    my class DirRecurse does Iterator {
        has str $!abspath;
        has $!handle;
        has $!dir;
        has $!file,
        has str $!dir-sep;
        has $!todo;
        has $!seen;
        method !SET-SELF(\abspath,$!dir,$!file) {
            $!abspath = abspath;
            $!handle := nqp::opendir($!abspath);
            $!dir-sep = $*SPEC.dir-sep;
            $!todo   := nqp::list_s;
            $!seen   := nqp::hash($!abspath,1);
            $!abspath = nqp::concat($!abspath,$!dir-sep);
            self
        }
        method new(\abspath,\dir,\file) {
            nqp::stat(abspath,nqp::const::STAT_EXISTS)
              && nqp::stat(abspath,nqp::const::STAT_ISDIR)
              ?? nqp::create(self)!SET-SELF(abspath,dir,file)
              !! Rakudo::Iterator.Empty
        }

        method !next() {
            nqp::while(
              !$!handle
                || nqp::isnull_s(my str $elem = nqp::nextfiledir($!handle))
                || nqp::iseq_i(nqp::chars($elem),0),
              nqp::stmts(
                nqp::if(
                  nqp::defined($!handle),
                  nqp::stmts(
                    nqp::closedir($!handle),
                    ($!handle := Any),
                  )
                ),
                nqp::if(
                  nqp::elems($!todo),
                  nqp::stmts(
                    ($!abspath = nqp::pop_s($!todo)),
                    nqp::handle(
                      ($!handle := nqp::opendir($!abspath)),
                      'CATCH', 0
                    ),
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
                          $path,:CWD($!abspath)).resolve.absolute
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
    }
    method DIR-RECURSE(
      \abspath,
      Mu :$dir  = -> str $elem { nqp::not_i(nqp::eqat($elem,'.',0)) },
      Mu :$file = True
    ) {
        Seq.new(DirRecurse.new(abspath,$dir,$file))
    }

    method FILETEST-E(str \abspath) is raw {
        nqp::stat(abspath,nqp::const::STAT_EXISTS)
    }
    method FILETEST-LE(str \abspath) is raw {
        nqp::lstat(abspath,nqp::const::STAT_EXISTS)
    }
    method FILETEST-D(str \abspath) is raw {
        nqp::stat(abspath,nqp::const::STAT_ISDIR);
    }
    method FILETEST-F(str \abspath) is raw {
        nqp::stat(abspath,nqp::const::STAT_ISREG);
    }
    method FILETEST-S(str \abspath) is raw {
        nqp::stat(abspath,nqp::const::STAT_FILESIZE)
    }
    method FILETEST-ES(str \abspath) is raw {
        nqp::stat(abspath,nqp::const::STAT_EXISTS)
          && nqp::stat(abspath,nqp::const::STAT_FILESIZE)
    }
    method FILETEST-L(str \abspath) is raw {
        nqp::fileislink(abspath)
    }
    method FILETEST-R(str \abspath) is raw {
        nqp::filereadable(abspath)
    }
    method FILETEST-W(str \abspath) is raw {
        nqp::filewritable(abspath)
    }
    method FILETEST-RW(str \abspath) is raw {
        nqp::filereadable(abspath) && nqp::filewritable(abspath)
    }
    method FILETEST-X(str \abspath) is raw {
        nqp::fileexecutable(abspath)
    }
    method FILETEST-RWX(str \abspath) is raw {
        nqp::filereadable(abspath)
          && nqp::filewritable(abspath)
          && nqp::fileexecutable(abspath)
    }
    method FILETEST-Z(str \abspath) is raw {
        nqp::iseq_i(nqp::stat(abspath,nqp::const::STAT_FILESIZE),0)
    }

    method FILETEST-MODIFIED(str \abspath) is raw {
        nqp::stat_time(abspath,nqp::const::STAT_MODIFYTIME)
    }
    method FILETEST-ACCESSED(str \abspath) is raw {
        nqp::stat_time(abspath,nqp::const::STAT_ACCESSTIME)
    }
    method FILETEST-CHANGED(str \abspath) is raw {
        nqp::stat_time(abspath,nqp::const::STAT_CHANGETIME)
    }
    method FILETEST-MODE(str \abspath) is raw {
        nqp::bitand_i(nqp::stat(abspath,nqp::const::STAT_PLATFORM_MODE),0o7777)
    }

    method HANDLE-NQP-SPRINTF-ERRORS(Mu \exception) {
        my $vmex := nqp::getattr(nqp::decont(exception), Exception, '$!ex');
        my \payload := nqp::getpayload($vmex);
        if nqp::elems(payload) == 1 {
            if nqp::existskey(payload, 'BAD_TYPE_FOR_DIRECTIVE') {
                X::Str::Sprintf::Directives::BadType.new:
                    type      => nqp::atkey(nqp::atkey(payload, 'BAD_TYPE_FOR_DIRECTIVE'), 'TYPE'),
                    directive => nqp::atkey(nqp::atkey(payload, 'BAD_TYPE_FOR_DIRECTIVE'), 'DIRECTIVE'),
                    value     => nqp::atkey(nqp::atkey(payload, 'BAD_TYPE_FOR_DIRECTIVE'), 'VALUE'),
            }
            elsif nqp::existskey(payload, 'BAD_DIRECTIVE') {
                X::Str::Sprintf::Directives::Unsupported.new:
                    directive => nqp::atkey(nqp::atkey(payload, 'BAD_DIRECTIVE'), 'DIRECTIVE'),
                    sequence  => nqp::atkey(nqp::atkey(payload, 'BAD_DIRECTIVE'), 'SEQUENCE'),
            }
            elsif nqp::existskey(payload, 'DIRECTIVES_COUNT') {
                X::Str::Sprintf::Directives::Count.new:
                    args-have => nqp::atkey(nqp::atkey(payload, 'DIRECTIVES_COUNT'), 'ARGS_HAVE'),
                    args-used => nqp::atkey(nqp::atkey(payload, 'DIRECTIVES_COUNT'), 'ARGS_USED'),
            }
            else { exception }
        }
        else { exception }
    }

#- start of generated part of succ/pred ---------------------------------------
#- Generated on 2016-08-10T14:19:20+02:00 by tools/build/makeMAGIC_INC_DEC.raku
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

    method INFIX_COMMA_SLIP_HELPER(\reified, \future) {
        my $list :=
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',reified);
        nqp::bindattr($list,List,'$!todo',
          my $todo:= nqp::create(List::Reifier)
        );
        nqp::bindattr($todo,List::Reifier,'$!reified',reified);
        nqp::bindattr($todo,List::Reifier,'$!future',
          nqp::getattr(future,List,'$!reified')
        );
        nqp::bindattr($todo,List::Reifier,'$!reification-target',reified);
        $list
    }

    my $METAOP_ASSIGN := nqp::null;  # lazy storage for core METAOP_ASSIGN ops
    method METAOP_ASSIGN(\op) {
        my \op-is := nqp::ifnull(
          nqp::atkey(                                # is it a core op?
            nqp::ifnull($METAOP_ASSIGN,INSTALL-CORE-METAOPS()),
            nqp::objectid(op)
          ),
          -> Mu \a, Mu \b { a = op.( ( a.DEFINITE ?? a !! op.() ), b) }
        );
        op-is.set_name(op.name ~ ' + {assigning}');  # checked for in Hyper.new
        op-is
    }

    # Method for lazily installing fast versions of METAOP_ASSIGN ops for
    # core infix ops.  Since the compilation of &[op] happens at build time
    # of the setting, we're sure we're referring to the core ops and not one
    # that has been locally installed.  Called by METAOP_ASSIGN.  Please add
    # any other core ops that seem to be necessary.
    sub INSTALL-CORE-METAOPS() {
        my $metaop_assign := nqp::create(Rakudo::Internals::IterationSet);
        for (
          &[+], -> Mu \a, Mu \b { a = a.DEFINITE ?? a + b !! +b },
          &[%], -> Mu \a, Mu \b { a = a.DEFINITE ?? a % b !! Failure.new("No zero-arg meaning for infix:<%>")},
          &[-], -> Mu \a, Mu \b { a = a.DEFINITE ?? a - b !! -b },
          &[*], -> Mu \a, Mu \b { a = a.DEFINITE ?? a * b !! +b },
          &[~], -> Mu \a, Mu \b { a = a.DEFINITE ?? a ~ b !! ~b },
        ) -> \op, \metaop {
            metaop.set_name(op.name ~ ' + {assigning}');
            nqp::bindkey($metaop_assign, nqp::objectid(op), metaop);
        }
        $METAOP_ASSIGN := $metaop_assign;
    }

    # handle parameterization by just adding a "keyof" method
    my role KeyOf[::CONSTRAINT] {
        method keyof() { CONSTRAINT }
    }
    method PARAMETERIZE-KEYOF(Mu \base, Mu \type) {
        my \what := base.^mixin(KeyOf[type]);
        what.^set_name(base.^name ~ '[' ~ type.^name ~ ']');
        what
    }

    # Return a nqp list iterator from an IterationSet
    proto method IterationSet2keys(|) {*}
    multi method IterationSet2keys(IterationSet:U) {
        nqp::list_s
    }
    multi method IterationSet2keys(IterationSet:D \iterationset) {
        my $iter := nqp::iterator(iterationset);
        my $keys := nqp::list_s;
        nqp::while(
          $iter,
          nqp::push_s($keys,nqp::iterkey_s(nqp::shift($iter)))
        );
        $keys
    }

    # Return an Inline::Perl5 interpreter if possible
    my $P5;
    method PERL5() {
        $P5 //= do {
            {
                my $compunit := $*REPO.need(
                  CompUnit::DependencySpecification.new(
                    :short-name<Inline::Perl5>
                  )
                );
                GLOBAL.WHO.merge-symbols($compunit.handle.globalish-package);
                CATCH {
                    #X::Eval::NoSuchLang.new(:$lang).throw;
                    .note;
                }
            }
            ::("Inline::Perl5").default_perl5
        }
    }

    my %vm-sigs;
    method VM-SIGNALS() { %vm-sigs ?? %vm-sigs !! %vm-sigs = nqp::getsignals }

    # Re-run current process with a given environment variable and value,
    # given as a named variable, e.g. .RERUN-WITH(MVM_SPESH_INLINE_LOG => 1).
    # If that environment variable is *not* set, it will run the current
    # executor with the originally given parameters *and* the environment
    # variable set and return the associated Proc::Async object.  Once the
    # process is done, it will do an exit with the status result of the
    # child process.  If that environment variable is set, it will return
    # Nil (and do nothing), allowing that process to run its normal course.
    method RERUN-WITH() {
        my str $name = nqp::iterkey_s(
          nqp::shift(nqp::iterator(nqp::getattr(%_,Map,'$!storage')))
        );
        return Nil if %*ENV.EXISTS-KEY($name);

        %*ENV{$name} := %_{$name};
        Proc::Async.new(
          $*EXECUTABLE.absolute,
          Rakudo::Internals.LL-EXCEPTION,
          Rakudo::Internals.PROFILE,
          Rakudo::Internals.INCLUDE.map({ ("-I", $_).Slip }),
          Rakudo::Internals.PROGRAM,
          |@*ARGS
        )
    }
}

# expose the number of bits a native int has
my constant $?BITS = nqp::isgt_i(nqp::add_i(2147483648, 1), 0) ?? 64 !! 32;

{   # setting up END phaser handling
    my int $the-end-is-done;
    my $the-end-locker = Lock.new;
    # END handling, returns trueish if END handling already done/in progress
    nqp::bindcurhllsym('&THE_END', {
        unless $the-end-is-done {
            $the-end-locker.protect: {
                unless $the-end-is-done {
                    my $end := nqp::getcurhllsym('@END_PHASERS');
                    my @exceptions;
                    while nqp::elems($end) {           # run all END blocks
                        quietly {
                            my $result := nqp::shift($end)();
                            nqp::isfalse(nqp::isnull($result))
                                && nqp::can($result, 'sink') && $result.sink;
                            CATCH { default { @exceptions.push($_) } }
                        }
                    }
#?if moar
                    # close all open files
                    IO::Handle.^find_private_method(
                      'close-all-open-handles'
                    )(IO::Handle);
#?endif
                    if @exceptions {
                        note "Some exceptions were thrown in END blocks:";
                        note "  $_.^name(): $_.message()\n$_.backtrace.Str.indent(4)"
                          for @exceptions;
                    }
                    nqp::not_i(($the-end-is-done = 1)); # we're really done now
                }
            }
        }
    } );
}

# we need this to run *after* the mainline of Rakudo::Internals has run
Rakudo::Internals.REGISTER-DYNAMIC: '&*EXIT', {
    PROCESS::<&EXIT> := sub exit($status) {
        state $exit = $status;  # first call to exit sets value

        nqp::getcurhllsym('&THE_END')()
          ?? $exit
          !! nqp::exit(nqp::unbox_i($exit.Int))
    }
}

proto sub exit($?, *%) {*}
multi sub exit() { &*EXIT(0) }
multi sub exit(Int(Any) $status) { &*EXIT($status) }

# vim: expandtab shiftwidth=4
