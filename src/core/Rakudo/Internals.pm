my class DateTime { ... }
my class Seq { ... }
my class Lock is repr('ReentrantMutex') { ... }
my class X::IllegalOnFixedDimensionArray { ... };
my class X::Assignment::ToShaped { ... };

my class Rakudo::Internals {

    # an empty hash for when we need to iterate over something
    my \no-keys := nqp::hash;

    our role MappyIterator does Iterator {
        has $!storage;
        has $!iter;

        method !SET-SELF(\hash) {
            $!storage := nqp::getattr(hash,Map,'$!storage');
            $!storage := no-keys unless $!storage.DEFINITE;
            $!iter    := nqp::iterator($!storage);
            self
        }
        method new(\hash) { nqp::create(self)!SET-SELF(hash) }
        method count-only() {
            $!iter := Mu;
            nqp::p6box_i(nqp::elems($!storage))
        }
        method sink-all() {
            $!iter := Mu;
            IterationEnd
        }
    }

    our role BlobbyIterator does Iterator {
        has $!blob;
        has int $!elems;
        has Int $!i;   # cannot be an int yet sadly enough

        method SET-SELF(\blob) {
            $!blob := blob;
            $!i     = -1;
            $!elems = nqp::elems($!blob);
            self
        }
        method new(\blob) { nqp::create(self).SET-SELF(blob) }
        method push-all($target) {
            my $blob      := $!blob;  # attribute access is slower
            my int $i      = $!i;
            my int $elems  = $!elems;
            $target.push(nqp::atpos_i($blob,$i))
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
            IterationEnd
        }
        method count-only() {
            $!i = $!elems;
            nqp::p6box_i($!elems)
        }
        method sink-all() {
            $!i = $!elems;
            IterationEnd
        }
    }

    our class WhateverIterator does Iterator {
        has $!source;
        has $!last;
        has $!whatever;
        method new(\source) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!source', source);
            nqp::bindattr($iter, self, '$!whatever', False);
            $iter
        }
        method pull-one() is raw {
            if ($!whatever) {
                $!last
            }
            else {
                my \value := $!source.pull-one;
                if value =:= IterationEnd {
                    value
                }
                elsif nqp::istype(value, Whatever) {
                    $!whatever := True;
                    self.pull-one()
                }
                else {
                    $!last := value;
                    value
                }
            }
        }
    }

    our class DwimIterator does Iterator {
        has $!source;
        has $!buffer;
        has $!ended;
        has $!whatever;
        has $!i;
        has $!elems;
        method new(\source) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!source', source);
            nqp::bindattr($iter, self, '$!buffer', IterationBuffer.new);
            nqp::bindattr($iter, self, '$!ended', False);
            nqp::bindattr($iter, self, '$!whatever', False);
            nqp::bindattr($iter, self, '$!i', 0);
            nqp::bindattr($iter, self, '$!elems', 0);
            $iter
        }
        method pull-one() is raw {
            if ($!ended) {
                $!buffer.AT-POS( $!whatever
                  ?? $!elems - 1
                  !! (($!i := $!i + 1) - 1) % $!elems
                );
            }
            else {
                my \value := $!source.pull-one;
                if value =:= IterationEnd {
                    $!ended := True;
                    $!elems == 0 ?? value !! self.pull-one()
                }
                elsif nqp::istype(value, Whatever) {
                    $!whatever := True;
                    $!ended := True;
                    self.pull-one()
                }
                else {
                    $!elems := $!elems + 1;
                    $!buffer.push(value);
                    value
                }
            }
        }
        method ended() { $!ended }
        method count-elems() {
            unless ($!ended) {
                $!elems := $!elems + 1 until $!source.pull-one =:= IterationEnd;
            }
            $!elems
        }
    }

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

    # True if given array does not just contain objects of given type
    method NOT_ALL_TYPE(\values,\type) {
        return True unless nqp::istype($_,type) for values;
        False;
    }

    # True if given array does not just contain defined objects of given type
    method NOT_ALL_DEFINED_TYPE(\values,\type) {
        return True unless nqp::defined($_) && nqp::istype($_,type) for values;
        False;
    }

    method TRANSPOSE(Str \string, Str \original, Str \final) {
        nqp::join(nqp::unbox_s(final),
          nqp::split(nqp::unbox_s(original),nqp::unbox_s(string)))
    }

#?if moar
    my $propcode := nqp::hash;
    method PROPCODE(\propname) {
        my str $key = nqp::unbox_s(propname);
        nqp::bindkey($propcode,$key,nqp::unipropcode($key))
          unless nqp::existskey($propcode,$key);
        nqp::atkey($propcode,$key)
    }
    my $pvalcode := nqp::hash;
    method PVALCODE(\prop,\pvalname) {
        my str $pvalname = nqp::unbox_s(pvalname);
        my str $key      = nqp::concat(nqp::tostr_I(prop),$pvalname);
        nqp::bindkey($pvalcode,$key,
          nqp::unipvalcode(nqp::unbox_i(prop),$pvalname))
          unless nqp::existskey($pvalcode,$key);
        nqp::atkey($pvalcode,$key)
    }
#?endif

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

    method SHAPED-ARRAY-STORAGE(@dims, Mu \meta-obj, Mu \type-key) {
        my $key := nqp::list(meta-obj);
        my $dims := nqp::list_i();
        for @dims {
            if nqp::istype($_, Whatever) {
                X::NYI.new(feature => 'Jagged array shapes');
            }
            nqp::push($key, type-key);
            nqp::push_i($dims, $_.Int);
        }
        my $storage := nqp::create(nqp::parameterizetype(SHAPE-STORAGE-ROOT, $key));
        nqp::setdimensions($storage, $dims);
        $storage
    }

    our role ShapedArrayCommon {
        method !illegal($operation) {
            X::IllegalOnFixedDimensionArray.new(:$operation).throw
        }
        multi method pop(::?CLASS:D:)    { self!illegal("pop")    }
        multi method shift(::?CLASS:D:)  { self!illegal("shift")  }
        multi method splice(::?CLASS:D: *@) { self!illegal("splice") }
        multi method plan(::?CLASS:D: *@)   { self!illegal("plan")   }

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

        multi method keys(::?CLASS:D:) {
            my @shape := self.shape;
            @shape.elems == 1
                ?? Seq.new((^@shape[0]).iterator)
                !! cross(@shape.map({ 0..^$_ }).list)
        }
        multi method values(::?CLASS:D:) {
            self.keys.map({ self.AT-POS(|$_) })
        }
        multi method kv(::?CLASS:D:) {
            self.keys.map({ slip($_, self.AT-POS(|$_)) })
        }
        multi method pairs(::?CLASS:D:) {
            self.keys.map({ $_ => self.AT-POS(|$_) })
        }
        multi method antipairs(::?CLASS:D:) {
            self.keys.map({ self.AT-POS(|$_) => $_ })
        }
        multi method invert(::?CLASS:D:) {
            self.keys.map({ nqp::decont(self.AT-POS(|$_)) »=>» $_ }).flat
        }

        method iterator(::?CLASS:D:) {
            # This can be fairly heavily optimized in various ways later
            self.values.iterator
        }

        # These work on the flat view
        method roll(|c) {
            self.flat.roll(|c)
        }
        method pick(|c) {
            self.flat.pick(|c)
        }
        method permutations(|c) {
            self.flat.permutations(|c)
        }
        method combinations(|c) {
            self.flat.combinations(|c)
        }
        method rotor(|c) {
            self.flat.rotor(|c)
        }
        method join(|c) {
            self.flat.join(|c)
        }

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

        method !STORE-PATH(@path, @rest, \in) {
            my int $cur-pos = 0;
            if @rest.elems == 1 {
                for in -> \item {
                    self.ASSIGN-POS(|@path, $cur-pos, item);
                    $cur-pos = $cur-pos + 1;
                }
            }
            else {
                my @nextrest = @rest[1..^@rest.elems];
                for in -> \item {
                    my @nextpath = flat @path, $cur-pos;
                    if nqp::istype(item, Iterable) && nqp::isconcrete(item) {
                        self!STORE-PATH(@nextpath, @nextrest, item)
                    }
                    else {
                        X::Assignment::ToShaped.new(shape => self.shape).throw;
                    }
                    $cur-pos = $cur-pos + 1;
                }
            }
        }

        multi method Slip() {
            Slip.from-iterator(self.iterator)
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
            !! "use *{from} if you want to index relative to the end"),
        );
    }
    method SUBSTR-CHARS-OOR(\chars) {
        X::OutOfRange.new(
          :what('Number of characters argument to substr'),
          :got(chars.gist),
          :range("0..Inf"),
          :comment("use *{chars} if you want to index relative to the end"),
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

    method error-rcgye() {  # red clear green yellow eject
        %*ENV<RAKUDO_ERROR_COLOR> // !self.IS-WIN
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

    my $initializers := nqp::hash;
    method REGISTER-DYNAMIC(Str:D \name, &code, Str \ver = '6.c' --> Nil) {
        my str $name = nqp::unbox_s(name);
        my str $ver  = nqp::unbox_s(ver);
        my str $with = $ver ~ "\0" ~ $name;
        nqp::existskey($initializers,$with)
          ?? die "Already have initializer for '$name' ('$ver')"
          !! nqp::bindkey($initializers,$with,&code);
        nqp::bindkey($initializers,$name,&code)  # first come, first kept
          unless nqp::existskey($initializers,$name);
    }
    method INITIALIZE-DYNAMIC(str \name) {
#print "Initializing {name}\n";
        my str $ver  = nqp::getcomp('perl6').language_version;
        my str $with = $ver ~ "\0" ~ name;
        nqp::existskey($initializers,$with)
          ?? nqp::atkey($initializers,$with)()
          !! nqp::existskey($initializers,name)
            ?? nqp::atkey($initializers,name)()
            !! X::Dynamic::NotFound.new(:name(name));
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

    method FILETEST-E(Str:D \abspath) {
        nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_EXISTS)
    }
    method FILETEST-D(Str:D \abspath) {
        my int $d = nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_ISDIR);
        nqp::isge_i($d,0) ?? $d !! fail X::IO::Unknown.new(:trying<d>)
    }
    method FILETEST-F(Str:D \abspath) {
        my int $f = nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_ISREG);
        nqp::isge_i($f,0) ?? $f !! fail X::IO::Unknown.new(:trying<f>)
    }
    method FILETEST-S(Str:D \abspath) {
        nqp::stat(nqp::unbox_s(abspath),nqp::const::STAT_FILESIZE)
    }
    method FILETEST-L(Str:D \abspath) {
        my int $l = nqp::fileislink(nqp::unbox_s(abspath));
        nqp::isge_i($l,0) ?? $l !! fail X::IO::Unknown.new(:trying<l>)
    }
    method FILETEST-R(Str:D \abspath) {
        my int $r = nqp::filereadable(nqp::unbox_s(abspath));
        nqp::isge_i($r,0) ?? $r !! fail X::IO::Unknown.new(:trying<r>)
    }
    method FILETEST-W(Str:D \abspath) {
        my int $w = nqp::filewritable(nqp::unbox_s(abspath));
        nqp::isge_i($w,0) ?? $w !! fail X::IO::Unknown.new(:trying<w>)
    }
    method FILETEST-RW(Str:D \abspath) {
        my str $abspath = nqp::unbox_s(abspath);
        my int $r = nqp::filereadable($abspath);
        my int $w = nqp::filewritable($abspath);
        nqp::isge_i($r,0)
          ?? nqp::isge_i($w,0)
            ?? nqp::bitand_i($r,$w)
            !! fail X::IO::Unknown.new(:trying<w>)
          !! fail X::IO::Unknown.new(:trying<r>)
    }
    method FILETEST-X(Str:D \abspath) {
        my int $x = nqp::fileexecutable(nqp::unbox_s(abspath));
        nqp::isge_i($x,0) ?? $x !! fail X::IO::Unknown.new(:trying<x>)
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
              !! fail X::IO::Unknown.new(:trying<x>)
            !! fail X::IO::Unknown.new(:trying<w>)
          !! fail X::IO::Unknown.new(:trying<r>)
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
}

# vim: ft=perl6 expandtab sw=4
