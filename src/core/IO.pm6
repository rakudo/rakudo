# IO is done by classes representing a file in some way (as a file descriptor,
# as a path, etc.).

my role IO {
    multi method IO(IO: --> IO) { self }

    # Stringification methods
    # These *must* be overridden by whatever class is doing this role.

    multi method Str (::?CLASS:D: --> Str) {...}
    multi method gist(::?CLASS:D: --> Str) {...}
    multi method perl(::?CLASS:D: --> Str) {...}

    # File I/O methods

    proto method slurp(IO:D: :$enc, :$bin) {
        # We use an IO::Handle in binary mode, and then decode the string
        # all in one go, which avoids the overhead of setting up streaming
        # decoding.
        nqp::if(
          nqp::istype((my IO::Handle $handle := {*}, Failure),
          $handle,
          nqp::stmts(
            (my Blob $blob := $handle.slurp: :close),
            nqp::if(
              $bin,
              $blob,
              nqp::p6box_s(nqp::join("\n", nqp::split("\r\n", $blob.decode: $enc // 'utf-8')))
            )
          )
        )
    }
    multi method slurp(IO:D: :$enc, :$bin) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<slurp>
    }

    proto method spurt(IO:D: :$data, :$enc, :$append, :$createonly) {
        my IO::Handle $fh       := {*};
        nqp::if(
          nqp::istype($fh, Failure),
          $fh,
          $fh.spurt: $data, :close
        )
    }
    multi method spurt(IO:D: :$data, :$enc, :$append, :$createonly) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<spurt>
    }

    method lines(IO:D: :$chomp, :$enc, :$nl-in, |c) {
        self.open(:$chomp, :$enc, :$nl-in).lines(|c, :close)
    }
    method comb (IO:D: :$chomp, :$enc, :$nl-in, |c) {
        self.open(:$chomp, :$enc, :$nl-in).comb(|c, :close)
    }
    method split(IO:D: :$chomp, :$enc, :$nl-in, |c) {
        self.open(:$chomp, :$enc, :$nl-in).split(|c, :close)
    }
    method words(IO:D: :$chomp, :$enc, :$nl-in, |c) {
        self.open(:$chomp, :$enc, :$nl-in).words(|c, :close)
    }

    # Stat methods
    # These *must* be overridden by whatever class is doing this role.

    proto method e(IO:D: --> Bool) {*}
    multi method e(IO:D: --> Bool) {...}

    proto method d(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<d>
    }
    multi method d(IO:D: --> Bool) {...}

    proto method f(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<d>
    }
    multi method f(IO:D: --> Bool) {...}

    proto method s(IO:D: --> Int) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<s>
    }
    multi method s(IO:D: --> Int) {...}

    proto method l(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<l>
    }
    multi method l(IO:D: --> Bool) {...}

    proto method z(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<z>
    }
    multi method z(IO:D: --> Bool) {...}

    proto method r(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<r>
    }
    multi method r(IO:D: --> Bool) {...}

    proto method w(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<w>
    }
    multi method w(IO:D: --> Bool) {...}

    proto method x(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<x>
    }
    multi method x(IO:D: --> Bool) {...}

    proto method rw(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<rw>
    }
    multi method rw(IO:D: --> Bool) {...}

    proto method rwx(IO:D: --> Bool) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<rwx>
    }
    multi method rwx(IO:D: --> Bool) {...}

    proto method modified(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<modified>
    }
    multi method modified(IO:D: --> Instant) {...}

    proto method accessed(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<accessed>
    }
    multi method accessed(IO:D: --> Instant) {...}

    proto method changed(IO:D: --> Instant) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<changed>
    }
    multi method changed(IO:D: --> Instant) {...}

    proto method mode(IO:D: --> IntStr) {
        self.e ?? {*} !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<mode>
    }
    multi method mode(IO:D: --> IntStr) {...}

    # Regular file methods (i.e. files that aren't special)

    proto method open(IO:D: |c --> IO::Handle) {*}
    multi method open(IO:D: |c --> IO::Handle) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<open>
    }

#?if moar
    proto method watch(IO:D: --> IO::Notification) {*}
    multi method watch(IO:D: --> IO::Notification) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<watch>
    }
#?endif

    proto method rename(IO:D: IO() $to, :$createonly --> Bool) {
        nqp::unless(
          nqp::istype(nqp::decont($to), IO::Path),
          fail X::IO::PathRequired.new: :from(self), :$to, :trying<rename>
        );

        nqp::if(
          $createonly && $to.e,
          fail X::IO::Rename.new: :from(self), :to($to.absolute), :os-error(':createonly specified and destination exists')
        );

        return {*};

        CATCH { default {
            fail X::IO::Rename.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method rename(IO:D: IO() $to --> Bool) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :$to, :trying<rename>
    }

    proto method copy(IO:D: IO() $to, :$createonly --> Bool) {
        nqp::if(
          $createonly && $to.e,
          fail X::IO::Copy.new: :from(self), :$to, :os-error(':createonly specified and destination exists')
        );

        return {*};

        CATCH { default {
            fail X::IO::Copy.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method copy(IO:D: IO() $to, :$createonly --> Bool) {...}
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :$to, :trying<copy>
    }

    proto method move(IO:D: IO() $to --> Bool) {*}
    multi method move(IO:D: IO() $to --> Bool) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :$to, :trying<move>
    }

    proto method chmod(IO:D: Int() $mode --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Chmod.new: :from(self), :$mode, :os-error(.Str);
        } }
    }
    multi method chmod(IO:D: Int() $mode --> Bool) {
        Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<chmod>
    }

#   proto method chown(IO:D: Str() $user --> Bool) {
#       {*}
#
#       CATCH { default {
#           fail X::IO::Chown.new: :from(self), :$user, :os-error(.Str);
#       } }
#   }
#   multi method chown(IO:D: Str() --> Bool) {
#       Failure.new: X::IO::RegularFileRequired.new: :from(self), :trying<chown>
#   }

    # Symlink methods

    proto method unlink(IO:D: --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Unlink.new: :at(self), :os-error(.Str);
        } }
    }
    multi method unlink(IO:D: --> Bool) {
        Failure.new: X::IO::PathRequired.new: :from(self), :trying<unlink>
    }

    proto method symlink(IO:D: IO() $to --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Symlink.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method symlink(IO:D: IO() $to --> Bool) {
        Failure.new: X::IO::PathRequired.new: :from(self), :$to, :trying<symlink>
    }

    proto method link(IO:D: IO() $to --> Bool) {
        return {*};

        CATCH { default {
            fail X::IO::Link.new: :from(self), :$to, :os-error(.Str);
        } }
    }
    multi method link(IO:D: IO() $to --> Bool) {
        Failure.new: X::IO::PathRequired.new: :from(self), :$to, :trying<link>
    }

    # Directory methods

    proto method chdir(IO:D: | --> IO) {*}
    multi method chdir(IO:D: $to?, | --> IO) {
        Failure.new: X::IO::PathRequired.new: :from(self), :$to, :trying<chdir>
    }

    proto method mkdir(IO:D: Int() --> IO) {
        return {*};

        CATCH { default {
            fail X::IO::Mkdir.new: :at(self), :$mode, :os-error(.Str);
        } }
    }
    multi method mkdir(IO:D: Int() --> IO) {
        Failure.new: X::IO::PathRequired.new: :from(self), :trying<mkdir>
    }

    proto method rmdir(IO:D: --> IO) {
        return {*};

        CATCH { default {
            fail X::IO::Rmdir.new: :at(self), :os-error(.Str);
        } }
    }
    multi method rmdir(IO:D: --> IO) {
        Failure.new: X::IO::PathRequired.new: :from(self), :trying<rmdir>
    }

    proto method dir(IO:D: Mu :$test) {
        return {*};

        CATCH { default {
            fail X::IO::Dir.new: :at(self), :os-error(.Str);
        } }
    }
    multi method dir(IO:D: Mu :$test) {
        Failure.new: X::IO::PathRequired.new: :from(self), :trying<dir>
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
