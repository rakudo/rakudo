my class Parcel does Positional {
    # declared in BOOTSTRAP.pm:
    #    is Cool;              # parent class
    #    has $!storage;        # RPA of Parcel's elements

    multi method Bool(Parcel:D:)           { nqp::p6bool($!storage) }
    multi method Numeric(Parcel:D:)        { self.flat.elems }
    multi method Str(Parcel:D:)            { self.flat.Str }
#    multi method Int(Parcel:D:)            { self.flat.elems }
    multi method ACCEPTS(Parcel:D: $topic) { self.list.ACCEPTS($topic) }

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

    method flat() {
        nqp::p6list(nqp::clone($!storage), List, Bool::True)
    }

    method item() { my $v = self; }

    method list() {
        nqp::p6list(nqp::clone($!storage), List, Mu)
    }

    method lol() {
        nqp::p6list(nqp::clone($!storage), LoL, Mu)
    }

    method at_pos(Parcel:D: \$x) is rw { self.flat.at_pos($x); }

    proto method postcircumfix:<[ ]>(|$)                  { * }
    multi method postcircumfix:<[ ]>() is rw              { self.flat }
    multi method postcircumfix:<[ ]>(Parcel:D: \$x) is rw { self.flat.[$x] }

    multi method gist(Parcel:D:) {
        my Mu $gist := nqp::list();
        my Mu $iter := nqp::iterator($!storage);
        nqp::push($gist, nqp::unbox_s(nqp::shift($iter).gist)) while $iter;
        nqp::p6box_s(nqp::join(' ', $gist))
    }

    multi method perl(Parcel:D \$self:) {
        my Mu $rpa := nqp::clone($!storage);
        my $perl = nqp::iscont($self) ?? '$(' !! '(';
        if $rpa {
            $perl = $perl ~ nqp::shift($rpa).perl;
            if $rpa {
                $perl = $perl ~ ', ' ~ nqp::shift($rpa).perl while $rpa;
            }
            else {
                $perl = $perl ~ ',';
            }
        }
        $perl ~ ')';
    }

    method STORE(|$) {
        # get the list of rvalues to store and lhs containers
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        my $rhs := nqp::p6list($args, List, Bool::True);   # XXX this might need to be Seq

        # first pass -- scan lhs containers and pick out
        # scalar versus list assignment.  This also reifies
        # the rhs values 
        my Mu $lhs := nqp::clone($!storage);
        my Mu $tv := nqp::list();
        while ($lhs) {
            my Mu $x := $lhs.shift;
            if nqp::iscont($x) {
                # container: scalar assignment
                nqp::push($tv, $x);
                nqp::push($tv, $rhs.gimme(1) ?? nqp::p6decont($rhs.shift) !! Nil);
            }
            elsif nqp::istype($x, Whatever) {
                # Whatever: skip assigning value
                $rhs.shift;
            }
            elsif nqp::istype($x, Parcel) {
                # Parcel: splice into current lhs
                nqp::splice($lhs, nqp::getattr($x, Parcel, '$!storage'), 0, 0)
            }
            else {
                # store entire rhs
                nqp::push($tv, $x);
                nqp::push($tv, $rhs);
                $rhs := ().list;
            }
        }

        # second pass, perform the assignments
        while ($tv) { my $x := nqp::shift($tv); $x = nqp::shift($tv); }
        self
    }

    multi method DUMP(Parcel:D:) {
        self.DUMP-ID() ~ '(:storage(' ~ DUMP($!storage) ~ '))'
    }
    
    method FLATTENABLE_LIST() { $!storage }
    method FLATTENABLE_HASH() { nqp::hash() }

    method fmt($format = '%s', $separator = ' ') {
        self.list.fmt($format, $separator);
    }
}


my sub infix:<,>(|$) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), nqp::null());
}


