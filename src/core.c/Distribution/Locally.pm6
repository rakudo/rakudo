role Distribution::Locally does Distribution {
    has IO::Path $.prefix;
    method content($address) {
        my $path   = IO::Path.new($.meta<files>{$address} // $address, :CWD($!prefix.absolute // $*CWD.absolute));
        my $handle = IO::Handle.new(:$path);
        $handle // $handle.throw;
    }
}

# vim: expandtab shiftwidth=4
