my class Rakudo::Internals {

    our role MapIterator does Iterator {
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
}

# vim: ft=perl6 expandtab sw=4
