role Distribution::Locally does Distribution {
    has IO::Path $.prefix;

    method content(Str:D $address --> IO::Handle:D) {
        my $handle := IO::Handle.new: path => IO::Path.new:
          $.meta<files>{$address}  // $address,
          CWD => $!prefix.absolute // $*CWD.absolute;
        $handle // $handle.throw;
    }
}

# vim: expandtab shiftwidth=4
