augment class Str {
    multi method Bool { ?(pir::istrue__IP(self)); }

    # CHEAT: this implementation is a bit of a cheat,
    # but works fine for now.
    multi method Int { (+self).Int; }
    multi method Num { (+self).Num; }
}
