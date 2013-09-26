my class Block { # declared in BOOTSTRAP
    # class Block is Code {
    #     has Mu $!phasers;

    method add_phaser(Str $name, &block) {
        nqp::isnull($!phasers) &&
            nqp::bindattr(self, Block, '$!phasers', nqp::hash());
        nqp::existskey($!phasers, nqp::unbox_s($name)) ||
            nqp::bindkey($!phasers, nqp::unbox_s($name), nqp::list());
        if $name eq any(<LEAVE KEEP UNDO>) {
            nqp::unshift(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
            self.add_phaser('!LEAVE-ORDER', &block);
        }
        elsif $name eq any(<NEXT !LEAVE-ORDER POST>) {
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

    method pop_phaser(Str $name) {
        unless nqp::isnull($!phasers) {
            if nqp::existskey($!phasers, nqp::unbox_s($name)) {
                my $phasers := nqp::atkey($!phasers, nqp::unbox_s($name));
                unless nqp::isnull($phasers) {
                    nqp::pop($phasers);
#                    if nqp::elems($phasers) == 0 {
#                        nqp::deletekey($!phasers, nqp::unbox_s($name));
#                        if nqp::elems($!phasers) == 0 {
#                            $!phasers := nqp::null;
#                        }
#                    }
                }
            }
        }
    }
}
