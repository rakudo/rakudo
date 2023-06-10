role Distribution::Checksum does Distribution {
    has Str $!checksum;
    method checksum {
        ⚛$!checksum // cas $!checksum, { 
            $_ // nqp::sha1(Rakudo::Internals::JSON.to-json(self.meta, :!pretty, :sorted-keys))
        }
    }
}