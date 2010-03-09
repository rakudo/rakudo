augment class Str {
    multi method Bool { ?(pir::istrue__IP(self)); }

    method Str() { self }

    # CHEAT: this implementation is a bit of a cheat,
    # but works fine for now.
    multi method Int { (+self).Int; }
    multi method Num { (+self).Num; }

    method d() {
        self.e ?? ?pir::stat__ISI(self, 2) !! Bool;
    }

    method f() {
        self.e ?? !pir::stat__ISI(self, 2) !! Bool;
    }

    method s() {
        self.e ?? pir::stat__ISI(self, 1) !! Any;
    }
}
