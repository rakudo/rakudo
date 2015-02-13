# class for Unclassified IO objects
my class IOU does IO::Pathy {
    has $!this;
    has $!rest;
    has $!that;

    submethod BUILD(:$!this,:$!rest,:$!abspath) { }
    method new($this, Str() :$CWD = $*CWD, |c) {
        my $abspath := MAKE-ABSOLUTE-PATH($this,$CWD);
        self!what($abspath,|c)                      # either a IO::Local object
          // self.bless(:$this,:rest(c),:$abspath); # or a placeholder
    }

    multi method ACCEPTS(IOU:D: \other) {
        self!that
          ?? $!that.ACCEPTS(other)
          // nqp::p6bool(nqp::iseq_s(nqp::unbox_s($!this),nqp::unbox_s(~other)))
          !! False;
    }

    method IO(IOU:D:) { $!that // self }

    method open(IOU:D: |c) {
        my $handle = open($!abspath,|c);
        $!that = IO::File.new(:$!abspath) if $handle;
        $handle;
    }
    method mkdir(IOU:D: |c) {
        my $result = mkdir($!abspath,|c);
        $!that = IO::Dir.new(:$!abspath) if $result;
        $result;
    }
    method spurt(IOU:D: \what, |c) {
        my $result = spurt($!abspath,what,|c);
        $!that = IO::File.new(:$!abspath) if $result;
        $result;
    }

    multi method Str(IOU:D:)  { $!this }
    multi method gist(IOU:D:) { qq|"$!this".IO| }
    multi method perl(IOU:D:) { "q|$!this|.IO" }

# Methods that we expect to work on an IOU of which the abspath did not exist
# at creation time.  We try to create the object again, call the method if
# succeeds, or fail.  Wish there were a less verbose way to do this.

    method e(IOU:D:)        { self!that ?? $!that.e        !! self!fail('e')   }
    method f(IOU:D:)        { self!that ?? $!that.f        !! self!fail('f')   }
    method d(IOU:D:)        { self!that ?? $!that.d        !! self!fail('d')   }
    method s(IOU:D:)        { self!that ?? $!that.s        !! self!fail('s')   }
    method l(IOU:D:)        { self!that ?? $!that.l        !! self!fail('l')   }
    method r(IOU:D:)        { self!that ?? $!that.r        !! self!fail('r')   }
    method w(IOU:D:)        { self!that ?? $!that.w        !! self!fail('w')   }
    method rw(IOU:D:)       { self!that ?? $!that.rw       !! self!fail('rw')  }
    method x(IOU:D:)        { self!that ?? $!that.x        !! self!fail('x')   }
    method rx(IOU:D:)       { self!that ?? $!that.rx       !! self!fail('rx')  }
    method wx(IOU:D:)       { self!that ?? $!that.wx       !! self!fail('wx')  }
    method rwx(IOU:D:)      { self!that ?? $!that.rwx      !! self!fail('rwx') }
    method o(IOU:D:)        { self!that ?? $!that.o        !! self!fail('o')   }
    method z(IOU:D:)        { self!that ?? $!that.z        !! self!fail('z')   }

    method modified(IOU:D:) {
        self!that ?? $!that.modified !! self!fail('modified');
    }
    method accessed(IOU:D:) {
        self!that ?? $!that.accessed !! self!fail('accessed');
    }
    method changed(IOU:D:)  {
        self!that ?? $!that.changed  !! self!fail('changed');
    }
    method device(IOU:D:)  {
        self!that ?? $!that.device   !! self!fail('device');
    }
    method inode(IOU:D:)  {
        self!that ?? $!that.inode    !! self!fail('inode');
    }

# private methods

    method !that(IOU:D:) { $!that //= self!what($!abspath,|$!rest) }

    method !fail($trying) {
        fail X::IO::DoesNotExist.new( :path($!this), :$trying );
    }

    method !what(IOU: $abspath, |c) {
        FILETEST-e($abspath)
          ?? OBJECTIFY-ABSPATH($abspath, |c)
          !! Mu;
    }
}

# vim: ft=perl6 expandtab sw=4
