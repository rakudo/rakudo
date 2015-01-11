# class for Unclassified IO objects
my class IOU does IO::Pathy {
    has $!this;
    has $!rest;
    has $!that;

    submethod BUILD(:$!this,:$!rest,:$!abspath) { }
    method new($this, :$CWD as Str = $*CWD, |c) {
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

    method e(IOU:D:)           { self!that ?? $!that.e         !! self!fail }
    method f(IOU:D:)           { self!that ?? $!that.f         !! self!fail }
    method d(IOU:D:)           { self!that ?? $!that.d         !! self!fail }
    method s(IOU:D:)           { self!that ?? $!that.s         !! self!fail }
    method l(IOU:D:)           { self!that ?? $!that.l         !! self!fail }
    method r(IOU:D:)           { self!that ?? $!that.r         !! self!fail }
    method w(IOU:D:)           { self!that ?? $!that.w         !! self!fail }
    method rw(IOU:D:)          { self!that ?? $!that.rw        !! self!fail }
    method x(IOU:D:)           { self!that ?? $!that.x         !! self!fail }
    method rx(IOU:D:)          { self!that ?? $!that.rx        !! self!fail }
    method wx(IOU:D:)          { self!that ?? $!that.wx        !! self!fail }
    method rwx(IOU:D:)         { self!that ?? $!that.rwx       !! self!fail }
    method o(IOU:D:)           { self!that ?? $!that.o         !! self!fail }
    method z(IOU:D:)           { self!that ?? $!that.z         !! self!fail }
    method modified(IOU:D:)    { self!that ?? $!that.modified  !! self!fail }
    method accessed(IOU:D:)    { self!that ?? $!that.accessed  !! self!fail }
    method changed(IOU:D:)     { self!that ?? $!that.changed   !! self!fail }

# private methods

    method !that(IOU:D:) { $!that //= self!what($!abspath,|$!rest) }

    method !fail() {
        fail X::IO::DoesNotExist.new(
          :path($!this),
          :trying(callframe(2).my<&?ROUTINE>.name ),  # skip <!> dispatcher
        );
    }

    method !what(IOU: $abspath, |c) {

        if FILETEST-e($abspath) {
            if FILETEST-f($abspath) {
                return IO::File.new(:$abspath, |c);
            }
            elsif FILETEST-d($abspath) {
                return IO::Dir.new(:$abspath, |c);
            }
            else {
                return IO::Local.new(:$abspath, |c);
            }
        }
        Mu;
    }
}

# vim: ft=perl6 expandtab sw=4
