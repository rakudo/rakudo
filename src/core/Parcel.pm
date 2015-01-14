my class Parcel does Positional { # declared in BOOTSTRAP
    # class Parcel is Cool {
    #    has Mu $!storage;        # VM's array of Parcel's elements
    #    has Str $!WHICH;

    multi method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        nqp::p6parcel($args, Nil)
    }

    multi method Bool(Parcel:D:)           { nqp::p6bool($!storage) }
    multi method Numeric(Parcel:D:)        { nqp::elems($!storage) }
    multi method Str(Parcel:D:)            { self.list.Str }

    multi method ACCEPTS(Parcel:D: $topic) {
        my $sseq = self.list;
        my $tseq = $topic.list;

        my int $spos = 0;
        my int $tpos = 0;
        while $spos < +$sseq {
            # if the next element is Whatever
            if nqp::istype($sseq[$spos],Whatever) {
                # skip over all of the Whatevers
                $spos = $spos + 1
                  while $spos <= +$sseq && nqp::istype($sseq[$spos],Whatever);
                # if nothing left, we're done
                return True if !($spos < +$sseq);
                # find a target matching our new target
                $tpos = $tpos + 1
                  while ($tpos < +$tseq) && $tseq[$tpos] !== $sseq[$spos];
                # return false if we ran out
                return False if !($tpos < +$tseq);
            }
            elsif $tpos >= +$tseq || $tseq[$tpos] !=== $sseq[$spos] {
                return False;
            }
            # skip matching elements
            $spos = $spos + 1;
            $tpos = $tpos + 1;
        }
        # If nothing left to match, we're successful.
        $tpos >= +$tseq;
    }


    multi method WHICH(Parcel:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ (^self.elems).map( {'(' ~ self[$_].VAR.WHICH ~ ')'} ).join;
    }
    method Parcel()  { self }
    method Capture() {
        my $cap := nqp::create(Capture);
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        my int $c = nqp::elems($!storage);
        my int $i = 0;
        while $i < $c {
            my $v := nqp::atpos($!storage, $i);
            nqp::istype($v, Pair) ??
                nqp::bindkey($hash, nqp::unbox_s($v.key), $v.value) !!
                nqp::push($list, $v);
            $i = $i + 1;
        }
        nqp::bindattr($cap, Capture, '$!list', $list);
        nqp::bindattr($cap, Capture, '$!hash', $hash);
        $cap
    }

    method elems() { nqp::elems($!storage) }
    method item()  { my $ = self }
    method flat()  { nqp::p6list(nqp::clone($!storage), List, Bool::True) }
    method list()  { nqp::p6list(nqp::clone($!storage), List, Mu) }
    method lol()   { nqp::p6list(nqp::clone($!storage), LoL, Mu) }

    method reverse() {
        my Mu $reverse  := nqp::list();
        my Mu $original := nqp::clone($!storage);
        nqp::push($reverse, nqp::pop($original)) while $original;
        my $parcel := nqp::create(self.WHAT);
        nqp::bindattr($parcel, Parcel, '$!storage', $reverse);
        $parcel;
    }

    method rotate (Int $n is copy = 1) {
        my $elems := nqp::p6box_i(nqp::elems($!storage));
        return self if !$elems;

        $n %= $elems;
        return self if $n == 0;

        my Mu $storage := nqp::clone($!storage);
        if $n > 0 {
            nqp::push($storage, nqp::shift($storage)) while $n--;
        }
        elsif $n < 0 {
            nqp::unshift($storage, nqp::pop($storage)) while $n++;
        }
        my $parcel := nqp::create(self.WHAT);
        nqp::bindattr($parcel, Parcel, '$!storage', $storage);
        $parcel;
    }

    multi method exists_pos(Parcel:D: int \pos) {
        nqp::p6bool(
          nqp::islt_i(pos,nqp::elems($!storage)) && nqp::isge_i(pos,0)
        );
    }
    multi method exists_pos(Parcel:D: Int:D \pos) {
        pos < nqp::elems($!storage) && pos >= 0;
    }

    multi method at_pos(Parcel:D: int \pos) is rw {
        nqp::isge_i(pos,nqp::elems($!storage)) || nqp::islt_i(pos,0)
          ?? Nil
          !! nqp::atpos($!storage,pos);
    }
    multi method at_pos(Parcel:D: Int:D \pos) is rw {
        my int $pos = nqp::unbox_i(pos);
        nqp::isge_i($pos,nqp::elems($!storage)) || nqp::islt_i($pos,0)
          ?? Nil
          !! nqp::atpos($!storage,$pos);
    }

    multi method gist(Parcel:D:) {
        my Mu $gist := nqp::list();
        my Mu $iter := nqp::iterator($!storage);
        nqp::push($gist, nqp::unbox_s(nqp::shift($iter).gist)) while $iter;
        nqp::p6box_s(nqp::join(' ', $gist))
    }

    multi method perl(Parcel:D \SELF:) {
        my Mu $rpa := nqp::clone($!storage);
        my $perl = nqp::iscont(SELF) ?? '$(' !! '(';
        if $rpa {
            $perl = $perl ~ nqp::shift($rpa).perl;
            if $rpa {
                $perl = $perl ~ ', ' ~ nqp::shift($rpa).perl while $rpa;
            }
            else {
                $perl = $perl ~ ',';
            }
            $perl ~ ')'
        }
        # because $() is *NOT* a Parcel but short for "$( $/.made // Str($/) )"
        elsif nqp::iscont(SELF) {
            $perl ~ ' )'
        }
        else {
            $perl ~ ')'
        }
    }

    method STORE(|) {
        # get the list of rvalues to store and lhs containers
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        my $rhs := nqp::p6list($args, List, Bool::True);

        # first pass -- scan lhs containers and pick out
        # scalar versus list assignment.  This also reifies
        # the rhs values
        my Mu $lhs := nqp::clone($!storage);
        my Mu $tv := nqp::list();
        while ($lhs) {
            my Mu \x := nqp::shift($lhs);
            if nqp::iscont(x) {
                # container: scalar assignment
                nqp::push($tv, x);
                nqp::push($tv, $rhs.gimme(1) ?? nqp::decont($rhs.shift) !! Nil);
            }
            elsif nqp::istype(x, Whatever) {
                # Whatever: skip assigning value
                $rhs.shift;
            }
            elsif nqp::istype(x, Parcel) {
                # Parcel: splice into current lhs
                nqp::splice($lhs, nqp::getattr(x, Parcel, '$!storage'), 0, 0)
            }
            else {
                # store entire rhs
                nqp::push($tv, x);
                nqp::push($tv, $rhs);
                $rhs := ().list;
            }
        }

        # second pass, perform the assignments
        while ($tv) { my \x := nqp::shift($tv); x = nqp::shift($tv); }
        self
    }

    multi method DUMP(Parcel:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!storage');
        nqp::push($attrs,  $!storage );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }

    method FLATTENABLE_LIST() { $!storage }
    method FLATTENABLE_HASH() { nqp::hash() }

    method fmt($format = '%s', $separator = ' ') {
        self.list.fmt($format, $separator);
    }
}


my sub infix:<,>(|) is pure {
    nqp::p6parcel(nqp::p6argvmarray(), nqp::null());
}

# vim: ft=perl6 expandtab sw=4
