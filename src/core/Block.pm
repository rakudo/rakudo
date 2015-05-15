my class Block { # declared in BOOTSTRAP
    # class Block is Code {
    #     has Mu $!phasers;

    method add_phaser(Str $name, &block) {
        nqp::isnull($!phasers) &&
            nqp::bindattr(self, Block, '$!phasers', nqp::hash());
        nqp::existskey($!phasers, nqp::unbox_s($name)) ||
            nqp::bindkey($!phasers, nqp::unbox_s($name), nqp::list());
        if $name eq 'LEAVE' || $name eq 'KEEP' || $name eq 'UNDO' {
            nqp::unshift(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
            self.add_phaser('!LEAVE-ORDER', &block);
        }
        elsif $name eq 'NEXT' || $name eq '!LEAVE-ORDER' || $name eq 'POST' {
            nqp::unshift(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
        }
        else {
            nqp::push(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
        }
    }

    method fire_phasers(str $name) {
        if !nqp::isnull($!phasers) && nqp::existskey($!phasers, $name) {
            my Mu $iter := nqp::iterator(nqp::atkey($!phasers, $name));
            nqp::shift($iter).() while $iter;
        }
    }

    method phasers(Str $name) {
        unless nqp::isnull($!phasers) {
            if nqp::existskey($!phasers, nqp::unbox_s($name)) {
                return nqp::p6parcel(nqp::atkey($!phasers, nqp::unbox_s($name)), Mu);
            }
        }
        ()
    }

    method assuming(Block:D $b: |curried) {
        anon sub CURRIED (|direct) {
            $b(|curried, |direct)
        }
    }

    multi method perl(Block:D:) {
        my $perl = '-> ';
        $perl ~= substr(self.signature().perl,1); # lose colon prefix
        $perl ~= ' { #`(' ~ self.WHICH ~ ') ... }';
        $perl
    }
}

# vim: ft=perl6 expandtab sw=4
