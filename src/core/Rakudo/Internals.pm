my class Seq { ... }
my class Lock is repr('ReentrantMutex') { ... }
my class X::IllegalOnFixedDimensionArray { ... };
my class X::Assignment::ToShaped { ... };

my class Rakudo::Internals {

    our role MappyIterator does Iterator {
        has $!hash-storage;
        has $!hash-iter;

        method BUILD(\hash) {
            $!hash-storage := nqp::getattr(hash, Map, '$!storage');
            $!hash-storage := nqp::hash() unless $!hash-storage.DEFINITE;
            $!hash-iter    := nqp::iterator($!hash-storage);
            self
        }
        method new(\hash) { nqp::create(self).BUILD(hash) }
        method count-only() {
            $!hash-iter := Mu;
            nqp::p6box_i(nqp::elems($!hash-storage))
        }
        method sink-all() {
            $!hash-iter := Mu;
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

        method BUILD(\list-of-pairs) {
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
        method new(\list-of-pairs) { nqp::create(self).BUILD(list-of-pairs) }
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
        proto method push(|c) {
            self.DEFINITE
                ?? X::IllegalOnFixedDimensionArray.new(operation => 'push').throw
                !! self.Any::push(|c)
        }
        proto method append(|c) {
            self.DEFINITE
                ?? X::IllegalOnFixedDimensionArray.new(operation => 'append').throw
                !! self.Any::append(|c)
        }

        multi method pop(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'pop').throw
        }

        multi method shift(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'shift').throw
        }

        proto method unshift(|c) {
            self.DEFINITE
                ?? X::IllegalOnFixedDimensionArray.new(operation => 'unshift').throw
                !! self.Any::unshift(|c)
        }
        proto method prepend(|c) {
            self.DEFINITE
                ?? X::IllegalOnFixedDimensionArray.new(operation => 'prepend').throw
                !! self.Any::prepend(|c)
        }

        multi method splice(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'splice').throw
        }

        multi method plan(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'plan').throw
        }

        method reverse(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'reverse').throw
        }

        method rotate(::?CLASS:D: Cool) {
            X::IllegalOnFixedDimensionArray.new(operation => 'rotate').throw
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

        submethod BUILD(:&!on-data-ready!, :&!on-completed!, :&!on-error!) {
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
    method initialize-sprintf-handler() {
        class SprintfHandler {
            method mine($x) { nqp::reprname($x) eq "P6opaque"; }
            method int($x) { $x.Int }
        }
        unless $sprintfHandlerInitialized {
            nqp::sprintfaddargumenthandler(SprintfHandler.new);
            $sprintfHandlerInitialized = 1;
        }
    }
}

# vim: ft=perl6 expandtab sw=4
