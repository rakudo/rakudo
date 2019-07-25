# On Windows, IO::FileDescriptor represents a pointer to a HANDLE.
# On *NIX, take a wild guess what IO::FileDescriptor represents.

my class IO::FileDescriptor is Int does IO {
    multi method WHICH(IO::FileDescriptor:D: --> ValueObjAt) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT, IO::FileDescriptor),
              'IO::FileDescriptor|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::coerce_is(nqp::unbox_i(self))
          ),
          ValueObjAt
        )
    }

    multi method ACCEPTS(IO::FileDescriptor:D: int \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_i(nqp::unbox_i(self), other))
    }
    multi method ACCEPTS(IO::FileDescriptor:D: Int() \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_i(nqp::unbox_i(self), nqp::unbox_i(other)))
    }

    proto method new(|) {*}
    multi method new(int $fd) {
        nqp::box_i($fd, IO::FileDescriptor)
    }
    multi method new(Int() $fd) {
        nqp::box_i(nqp::decont_i($fd), IO::FileDescriptor)
    }

    method Int(IO::FileDescriptor:D: --> Int) {
        nqp::p6box_i(nqp::unbox_i(self))
    }

    multi method Str (IO::FileDescriptor:D: --> Str) {
        nqp::p6box_s(nqp::coerce_is(nqp::unbox_i(self)))
    }
    multi method gist(IO::FileDescriptor:D: --> Str) {
        nqp::p6box_s(
          nqp::concat(
            nqp::coerce_is(nqp::unbox_i(self)),
            '.IO'
          )
        )
    }
    multi method perl(IO::FileDescriptor:D: --> Str) {
        nqp::p6box_s(
          nqp::concat(
            nqp::concat(nqp::unbox_s(self.^name), '.new('),
            nqp::concat(nqp::coerce_is(nqp::unbox_i(self)), ')')
          )
        )
    }

    multi method slurp(IO::FileDescriptor:D: :$enc, :$bin --> IO::Handle)                         {
        IO::Handle.new(:fd(self)).open(:$enc, :$bin)
    }
    multi method spurt(IO::FileDescriptor:D: $data, :$enc, :$append, :$createonly --> IO::Handle) {
        my int $truncate = nqp::if(
            nqp::isfalse(nqp::decont($append)),
            nqp::isfalse(nqp::decont($createonly))
        );
        self.open:
            :$enc,                   :bin(nqp::istype($data, Blob)),
            :mode<wo>,               :create,
            :exclusive($createonly), :$append,
            :$truncate
    }

    multi method e       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-E: self
    }
    multi method d       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-D: self
    }
    multi method f       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-F: self
    }
    multi method s       (IO::FileDescriptor:D: --> Int)     {
        nqp::p6box_i(Rakudo::Internals.FILETEST-S: self)
    }
    multi method l       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-LE(self)
            ?? ?Rakudo::Internals.FILETEST-L(self)
            !! Failure.new: X::IO::DoesNotExist.new: :at(self), :trying<l>
    }
    multi method z       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-Z: self
    }
    multi method r       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-R: self
    }
    multi method w       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-W: self
    }
    multi method x       (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-X: self
    }
    multi method rw      (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-RW: self
    }
    multi method rwx     (IO::FileDescriptor:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-RWX: self
    }
    multi method modified(IO::FileDescriptor:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-MODIFIED: self
    }
    multi method accessed(IO::FileDescriptor:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-ACCESSED: self
    }
    multi method changed (IO::FileDescriptor:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-CHANGED: self
    }
    multi method mode    (IO::FileDescriptor:D: --> IntStr)  {
        nqp::stmts(
          (my int $mode = Rakudo::Internals.FILETEST-MODE: self),
          IntStr.new: $mode, sprintf '%04o', $mode
        )
    }

    multi method open  (IO::FileDescriptor:D: |c --> IO::Handle)             {
        IO::Handle.new(:fd(self)).open(|c)
    }
#?if moar
    multi method watch (IO::FileDescriptor:D: --> IO::Notification)            {
        # TODO: implement nqp::watchfd
        # IO::Notification.watch-file: self
        Failure.new: X::NYI.new: :feature<watchfd>
    }
#?endif
    multi method rename(IO::FileDescriptor:D: IO() $to, :$createonly --> Bool) {
        # TODO: implement nqp::renamefd
        # nqp::hllbool(nqp::renamefd(nqp::unbox_i(self), nqp::unbox_s($to.Str)))
        Failure.new: X::NYI.new: :feature<renamefd>
    }
    multi method copy  (IO::FileDescriptor:D: IO() $to, $createonly --> Bool)  {
        # TODO: implement nqp::copyfd
        # nqp::hllbool(nqp::copyfd(nqp::unbox_i(self), nqp::unbox_s($to.Str)))
        Failure.new: X::NYI.new: :feature<copyfd>
    }
    multi method move  (IO::FileDescriptor:D: |c --> True)                     {
        self.copy: |c orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
        self.unlink   orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
    }
    multi method chmod (IO::FileDescriptor:D: Int() $mode --> Bool)            {
        # TODO: implement nqp::chmodfd
        # nqp::hllbool(nqp::chmodfd(nqp::unbox_i(self), nqp::decont_i($mode)))
        Failure.new: X::NYI.new: :feature<chmodfd>
    }
}

# vim: ft=perl6 expandtab sw=4
