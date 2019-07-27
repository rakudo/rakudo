# IO is done by classes describing files in some way (apart from IO::Handle,
# which uses any other class describing files internally).

my class Instant { ... }

my class IO::Path         { ... }
my class IO::Special      { ... }
# IO::NativeDescriptor is stubbed in src/core/Int.pm6.
my class IO::Notification { ... }

# XXX FIXME: stubbing *anything* in IO makes MoarVM throw with this error
# during compilation: "Cannot invoke this object (REPR: Null; VMNull)".

my role IO {
    # Stringification methods
    # These *must* be implemented by whatever class is doing this role.

    # multi method Str (IO:D: --> Str:D) { ... }
    # multi method gist(IO:D: --> Str:D) { ... }
    # multi method perl(IO:D: --> Str:D) { ... }

    # File I/O methods

    method slurp(IO:D: :$enc, :$bin) {
        # We use an IO::Handle in binary mode, and then decode the string
        # all in one go, which avoids the overhead of setting up streaming
        # decoding.
        nqp::if(
          nqp::istype((my $handle := IO::Handle.new(:file(self)).open(:bin)), Failure),
          $handle,
          nqp::stmts(
            (my $blob := $handle.slurp: :close),
            nqp::if(
              $bin,
              $blob,
              nqp::p6box_s(nqp::join("\n", nqp::split("\r\n", $blob.decode: $enc // 'utf-8')))
            )
          )
        )
    }
    method spurt(IO:D: $data, :$enc, :$append, :$createonly) {
        my $handle := self.open:
            :$enc,     :bin(nqp::istype(nqp::decont($data), Blob)),
            :mode<wo>, :create,
            :$append,  :exclusive($createonly),
            :truncate(nqp::if(
                nqp::isfalse(nqp::decont($append)),
                nqp::isfalse(nqp::decont($createonly))
            ));

        nqp::if(
          nqp::istype($handle, Failure),
          $handle,
          $handle.spurt: $data, :close
        )
    }
    method lines(
        IO:D: :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).lines(|c, :close)
    }
    method comb (
        IO:D: :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).comb(|c, :close)
    }
    method split(
        IO:D: :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).split(|c, :close)
    }
    method words(
        IO:D: :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).words(|c, :close)
    }

    # Stat methods
    # These *must* be implemented by whatever class is doing this role.

    proto method e(IO:D: --> Bool) {*}
    # multi method e(IO:D: --> Bool) { ... }

    proto method d(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<d>
    }
    # multi method d(IO:D: --> Bool) { ... }

    proto method f(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<d>
    }
    # multi method f(IO:D: --> Bool) { ... }

    proto method s(IO:D: --> Int) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<s>
    }
    # multi method s(IO:D: --> Int) { ... }

    proto method l(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<l>
    }
    # multi method l(IO:D: --> Bool) { ... }

    proto method z(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<z>
    }
    # multi method z(IO:D: --> Bool) { ... }

    proto method r(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<r>
    }
    # multi method r(IO:D: --> Bool) { ... }

    proto method w(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<w>
    }
    # multi method w(IO:D: --> Bool) { ... }

    proto method x(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<x>
    }
    # multi method x(IO:D: --> Bool) { ... }

    proto method rw(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<rw>
    }
    # multi method rw(IO:D: --> Bool) { ... }

    proto method rwx(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<rwx>
    }
    # multi method rwx(IO:D: --> Bool) { ... }

    proto method modified(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<modified>
    }
    # multi method modified(IO:D: --> Instant) { ... }

    proto method accessed(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<accessed>
    }
    # multi method accessed(IO:D: --> Instant) { ... }

    proto method changed(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<changed>
    }
    # multi method changed(IO:D: --> Instant) { ... }

    proto method mode(IO:D: --> IntStr) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<mode>
    }
    # multi method mode(IO:D: --> IntStr) { ... }

    # Regular file methods (i.e. files that aren't special)

    proto method open(IO:D: |c --> IO::Handle) {*}
    multi method open(IO:D: |c --> IO::Handle) {
        Failure.new: X::IO::NotAFile.new: :from(self), :trying<open>
    }

#?if moar
    proto method watch(IO:D: | --> IO::Notification) {*}
    multi method watch(IO:D: | --> IO::Notification) {
        Failure.new: X::IO::NotAFile.new: :from(self), :trying<watch>
    }
#?endif

    proto method chmod(IO:D: Int() $mode, | --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Chmod.new: :from(self), :$mode, :os-error(.Str);
        } }
    }
    multi method chmod(IO:D: Int() $mode, | --> Bool) {
        Failure.new: X::IO::NotAFile.new: :from(self), :trying<chmod>
    }

    proto method rename(IO:D: IO() $to, :$createonly, | --> Bool) {
        nqp::unless(
          nqp::istype(nqp::decont($to), IO::Path),
          fail X::IO::NotAPath.new: :from(self), :$to, :trying<rename>
        );

        nqp::if(
          $createonly && $to.e,
          fail X::IO::Rename.new:
              :from(self.?absolute // self),
              :to($to.absolute),
              :os-error(':createonly specified and destination exists')
        );

        return {*};

        CATCH { default {
            fail X::IO::Rename.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method rename(IO:D: IO() $to, :$createonly, | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<rename>
    }

    proto method copy(IO:D: IO() $to, :$createonly, | --> Bool) {
        nqp::unless(
          nqp::istype(nqp::decont($to), IO::Path),
          fail X::IO::NotAPath.new: :from(self), :$to, :trying<copy>
        );

        nqp::if(
          $createonly && $to.e,
          fail X::IO::Copy.new:
              :from(self.?absolute // self),
              :to($to.absolute),
              :os-error(':createonly specified and destination exists')
        );

        return {*};

        CATCH { default {
            fail X::IO::Copy.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method copy(IO:D: IO() $to, :$createonly, | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<copy>
    }

    proto method move(IO:D: | --> Bool) {*}
    multi method move(IO:D: | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<move>
    }

    # File link methods

    proto method unlink(IO:D: | --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Unlink.new: :target(self), :os-error(.Str);
        } }
    }
    multi method unlink(IO:D: | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<unlink>
    }

    proto method symlink(IO:D: IO() $to, | --> Bool) {
        nqp::unless(
          nqp::istype(nqp::decont($to), IO::Path),
          X::IO::NotAPath.new: :from(self), :$to, :trying<link>
        );

        return {*};

        CATCH { default {
            fail X::IO::Symlink.new: :target($to), :os-error(.Str);
        } }
    }
    multi method symlink(IO:D: IO() $to, | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<symlink>
    }

    proto method link(IO:D: IO() $to, | --> Bool) {
        nqp::unless(
          nqp::istype(nqp::decont($to), IO::Path),
          X::IO::NotAPath.new: :from(self), :$to, :trying<link>
        );

        return {*};

        CATCH { default {
            fail X::IO::Link.new: :target($to), :os-error(.Str);
        } }
    }
    multi method link(IO:D: IO() $to, | --> Bool) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<link>
    }

    # Directory methods

    proto method chdir(IO:D: | --> IO) {*}
    multi method chdir(IO:D: | --> IO) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<chdir>
    }

    proto method mkdir(IO:D: Int() $mode = 0o777, | --> IO) {
        return {*};

        CATCH { default {
            fail X::IO::Mkdir.new: :path(self), :$mode, :os-error(.Str);
        } }
    }
    multi method mkdir(IO:D: Int() $mode = 0o777, | --> IO) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<mkdir>
    }

    proto method rmdir(IO:D: | --> IO) {
        return {*};

        CATCH { default {
            fail X::IO::Rmdir.new: :path(self), :os-error(.Str);
        } }
    }
    multi method rmdir(IO:D: | --> IO) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<rmdir>
    }

    proto method dir(IO:D: Mu :$test, |) {
        return {*};

        CATCH { default {
            die X::IO::Dir.new: :path(self), :os-error(.Str);
        } }
    }
    multi method dir(IO:D: Mu :$test, |) {
        Failure.new: X::IO::NotAPath.new: :from(self), :trying<dir>
    }
}

enum SeekType (
  :SeekFromBeginning(0),
  :SeekFromCurrent(1),
  :SeekFromEnd(2),
);
enum ProtocolFamily (
  :PF_LOCAL(0),
  :PF_UNIX(1),
  :PF_INET(2),
  :PF_INET6(3),
  :PF_MAX(4),
);
enum SocketType (
  :SOCK_PACKET(0),
  :SOCK_STREAM(1),
  :SOCK_DGRAM(2),
  :SOCK_RAW(3),
  :SOCK_RDM(4),
  :SOCK_SEQPACKET(5),
  :SOCK_MAX(6),
);
enum ProtocolType (
  :PROTO_TCP(6),
  :PROTO_UDP(17),
);

# vim: ft=perl6 expandtab sw=4
