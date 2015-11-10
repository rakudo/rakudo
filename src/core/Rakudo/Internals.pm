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
            my $iter := self.CREATE;
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
            my $iter := self.CREATE;
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
}

# vim: ft=perl6 expandtab sw=4
