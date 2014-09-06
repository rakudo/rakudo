my role IO::FileTestable does IO {
    has Bool $!exists;

    method e() {
        $!exists //= nqp::p6bool(nqp::stat(
          nqp::unbox_s(IO::Spec.rel2abs(self.Str)),
        nqp::const::STAT_EXISTS));
    }

    method d() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_ISDIR))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<d>)
    }

    method f() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_ISREG))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<f>)
    }

    method s() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_ISREG))
            ?? nqp::p6box_i(nqp::stat($unboxed, nqp::const::STAT_FILESIZE))
            !! fail X::IO::NotAFile.new(:path(self.Str),:trying<s>)
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<s>)
    }

    method l() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::fileislink($unboxed))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<l>)
    }

    method r() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::filereadable($unboxed))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<r>)
    }

    method w() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::filewritable($unboxed))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<w>)
    }

    method x() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::fileexecutable($unboxed))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<x>)
    }

    method z() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_ISREG))
            ?? nqp::p6box_i(nqp::stat($unboxed, nqp::const::STAT_FILESIZE)) == 0
            !! fail X::IO::NotAFile.new(:path(self.Str),:trying<z>)
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<z>)
    }

    method modified() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6box_i(nqp::stat($unboxed, nqp::const::STAT_MODIFYTIME))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<modified>)
    }

    method accessed() {
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6box_i(nqp::stat($unboxed, nqp::const::STAT_ACCESSTIME))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<accessed>)
    }

    method changed() { 
        my Mu $unboxed := nqp::unbox_s(IO::Spec.rel2abs(self.Str));
        ($!exists //= nqp::p6bool(nqp::stat($unboxed, nqp::const::STAT_EXISTS)))
          ?? nqp::p6box_i(nqp::stat($unboxed, nqp::const::STAT_CHANGETIME))
          !! fail X::IO::DoesNotExist.new(:path(self.Str),:trying<changed>)
    }
}

# vim: ft=perl6 expandtab sw=4
