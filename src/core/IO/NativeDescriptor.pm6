# On Windows, IO::NativeDescriptor represents a pointer to a HANDLE.
# On *NIX, IO::NativeDescriptor represents a file descriptor.

my class IO::NativeDescriptor is Int does IO {
    multi method WHICH(IO::NativeDescriptor:D: --> ValueObjAt) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT, IO::NativeDescriptor),
              'IO::NativeDescriptor|',
              nqp::concat(self.^name, '|')
            ),
            nqp::coerce_is(nqp::unbox_i(self))
          ),
          ValueObjAt
        )
    }

    multi method ACCEPTS(IO::NativeDescriptor:D: int \other --> Bool:D)   {
        nqp::hllbool(nqp::iseq_i(nqp::unbox_i(self), other))
    }
    multi method ACCEPTS(IO::NativeDescriptor:D: Int:D \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_i(nqp::unbox_i(self), nqp::unbox_i(other)))
    }

    proto method new(IO::NativeDescriptor: |)         {*}
    multi method new(IO::NativeDescriptor: int $fd)   {
        nqp::box_i($fd, IO::NativeDescriptor)
    }
    multi method new(IO::NativeDescriptor: Int() $fd) {
        nqp::box_i(nqp::decont_i($fd), IO::NativeDescriptor)
    }

    method Int(IO::NativeDescriptor:D: --> Int:D) {
        nqp::p6box_i(nqp::unbox_i(self))
    }

    method IO(IO::NativeDescriptor:D: --> IO::NativeDescriptor:D) { self }

    multi method Str (IO::NativeDescriptor:D: --> Str:D) {
        nqp::p6box_s(nqp::coerce_is(nqp::unbox_i(self)))
    }
    multi method gist(IO::NativeDescriptor:D: --> Str:D) {
        nqp::p6box_s(
          nqp::concat(
            nqp::coerce_is(nqp::unbox_i(self)),
            '.IO'
          )
        )
    }
    multi method perl(IO::NativeDescriptor:D: --> Str:D) {
        nqp::p6box_s(
          nqp::concat(
            nqp::concat(self.^name, '.new('),
            nqp::concat(nqp::coerce_is(nqp::unbox_i(self)), ')')
          )
        )
    }

    multi method e       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-E: self
    }
    multi method d       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-D: self
    }
    multi method f       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-F: self
    }
    multi method s       (IO::NativeDescriptor:D: --> Int)     {
        # TODO: refactor RI to actually make this work.
        nqp::p6box_i(Rakudo::Internals.FILETEST-S: self)
    }
    multi method l       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-LE(self)
            ?? ?Rakudo::Internals.FILETEST-L(self)
            !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<l>
    }
    multi method z       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-Z: self
    }
    multi method r       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-R: self
    }
    multi method w       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-W: self
    }
    multi method x       (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-X: self
    }
    multi method rw      (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-RW: self
    }
    multi method rwx     (IO::NativeDescriptor:D: --> Bool)    {
        # TODO: refactor RI to actually make this work.
        ?Rakudo::Internals.FILETEST-RWX: self
    }
    multi method modified(IO::NativeDescriptor:D: --> Instant) {
        # TODO: refactor RI to actually make this work.
        Instant.from-posix: Rakudo::Internals.FILETEST-MODIFIED: self
    }
    multi method accessed(IO::NativeDescriptor:D: --> Instant) {
        # TODO: refactor RI to actually make this work.
        Instant.from-posix: Rakudo::Internals.FILETEST-ACCESSED: self
    }
    multi method changed (IO::NativeDescriptor:D: --> Instant) {
        # TODO: refactor RI to actually make this work.
        Instant.from-posix: Rakudo::Internals.FILETEST-CHANGED: self
    }
    multi method mode    (IO::NativeDescriptor:D: --> IntStr)  {
        nqp::stmts(
          (my int $mode = nqp::fstat(nqp::unbox_i(self), nqp::const::STAT_PLATFORM_MODE) +& 0o7777),
          IntStr.new: $mode, sprintf '%04o', $mode
        )
    }

    multi method open  (IO::NativeDescriptor:D: |c --> IO::Handle)               {
        IO::Handle.new(:file(self)).open(|c)
    }
#?if moar
    multi method watch (IO::NativeDescriptor:D: --> IO::Notification)            {
        # TODO: implement nqp::watchfd
        # IO::Notification.watch-file: self
        Failure.new: X::NYI.new: :feature<watchfd>
    }
#?endif
    multi method chmod (IO::NativeDescriptor:D: Int() $mode --> Bool)            {
        # TODO: implement nqp::chmodfd
        # nqp::hllbool(nqp::chmodfd(nqp::unbox_i(self), nqp::decont_i($mode)))
        Failure.new: X::NYI.new: :feature<chmodfd>
    }
}

# vim: ft=perl6 expandtab sw=4
