# class for Unclassified IO objects
my class IO::Huh does IO::Pathy {
    has $!this;
    has $!rest;
    has $!that;

    submethod BUILD(:$!this,:$!rest,:$!abspath) { }

    multi method ACCEPTS(IO::Huh:D: \other) {
        self!that
          ?? $!that.ACCEPTS(other)
          // nqp::p6bool(nqp::iseq_s(nqp::unbox_s($!this),nqp::unbox_s(~other)))
          !! False;
    }

    method IO(IO::Huh:D:) { $!that // self }

    method open(IO::Huh:D: |c) {
        my $handle = open($!abspath,|c);
        $!that = IO::File.new(:$!abspath) if $handle;
        $handle;
    }
    method mkdir(IO::Huh:D: |c) {
        my $result = mkdir($!abspath,|c);
        $!that = IO::Dir.new(:$!abspath) if $result;
        $result;
    }
    method spurt(IO::Huh:D: \what, |c) {
        my $result = spurt($!abspath,what,|c);
        $!that = IO::File.new(:$!abspath) if $result;
        $result;
    }

    multi method Str(IO::Huh:D:)  { $!this }
    multi method gist(IO::Huh:D:) { qq|"{ REMOVE-ROOT($*CWD.Str,$!this) }".IO| }
    multi method perl(IO::Huh:D:) { "q|$!this|.IO" }

# Methods that we expect to work on an IO::Huh of which the abspath did not
# exist # at creation time.  We try to create the object again, call the method
# if succeeds, or fail.  Wish there were a less verbose way to do this.

    method e(IO::Huh:D:)   { self!that ?? $!that.e   !! False }
    method f(IO::Huh:D:)   { self!that ?? $!that.f   !! self!fail('f')   }
    method d(IO::Huh:D:)   { self!that ?? $!that.d   !! self!fail('d')   }
    method s(IO::Huh:D:)   { self!that ?? $!that.s   !! self!fail('s')   }
    method l(IO::Huh:D:)   { self!that ?? $!that.l   !! self!fail('l')   }
    method r(IO::Huh:D:)   { self!that ?? $!that.r   !! self!fail('r')   }
    method w(IO::Huh:D:)   { self!that ?? $!that.w   !! self!fail('w')   }
    method rw(IO::Huh:D:)  { self!that ?? $!that.rw  !! self!fail('rw')  }
    method x(IO::Huh:D:)   { self!that ?? $!that.x   !! self!fail('x')   }
    method rx(IO::Huh:D:)  { self!that ?? $!that.rx  !! self!fail('rx')  }
    method wx(IO::Huh:D:)  { self!that ?? $!that.wx  !! self!fail('wx')  }
    method rwx(IO::Huh:D:) { self!that ?? $!that.rwx !! self!fail('rwx') }
    method o(IO::Huh:D:)   { self!that ?? $!that.o   !! self!fail('o')   }
    method z(IO::Huh:D:)   { self!that ?? $!that.z   !! self!fail('z')   }

    method modified(IO::Huh:D:) {
        self!that ?? $!that.modified !! self!fail('modified');
    }
    method accessed(IO::Huh:D:) {
        self!that ?? $!that.accessed !! self!fail('accessed');
    }
    method changed(IO::Huh:D:)  {
        self!that ?? $!that.changed  !! self!fail('changed');
    }
    method device(IO::Huh:D:)  {
        self!that ?? $!that.device   !! self!fail('device');
    }
    method inode(IO::Huh:D:)  {
        self!that ?? $!that.inode    !! self!fail('inode');
    }

    method l-e(IO::Huh:D:) { self!that ?? $!that.l-e !! self!fail('l-e')   }
    method l-s(IO::Huh:D:) { self!that ?? $!that.l-s !! self!fail('l-s')   }

    method l-modified(IO::Huh:D:) {
        self!that ?? $!that.l-modified !! self!fail('l-modified');
    }
    method l-accessed(IO::Huh:D:) {
        self!that ?? $!that.l-accessed !! self!fail('l-accessed');
    }
    method l-changed(IO::Huh:D:)  {
        self!that ?? $!that.l-changed  !! self!fail('l-changed');
    }
    method l-device(IO::Huh:D:)  {
        self!that ?? $!that.l-device   !! self!fail('l-device');
    }
    method l-inode(IO::Huh:D:)  {
        self!that ?? $!that.l-inode    !! self!fail('l-inode');
    }

# private methods

    method !that(IO::Huh:D:) { $!that //= self!what($!abspath,|$!rest) }

    method !fail($trying) {
        fail X::IO::DoesNotExist.new( :path($!this), :$trying );
    }

    method !what(IO::Huh: $abspath, |c) {
        FILETEST-e($abspath)
          ?? OBJECTIFY-ABSPATH($abspath, |c)
          !! Mu;
    }
}

# vim: ft=perl6 expandtab sw=4
