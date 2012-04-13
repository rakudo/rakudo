my class Block {
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
    
    method phasers(Str $name) {
        unless nqp::isnull($!phasers) {
            if nqp::existskey($!phasers, nqp::unbox_s($name)) {
                return nqp::p6parcel(nqp::atkey($!phasers, nqp::unbox_s($name)), Mu);
            }
        }
        ()
    }
}
