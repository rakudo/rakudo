class Dict is Map {
    multi method new(Dict: *@args --> Dict:D) {
        self.Map::new.STORE(@args, :INITIALIZE, :DECONT);
    }

    method STORE(Dict:D: @args, :$INITIALIZE --> Dict:D) {
        self.Map::STORE(@args, :$INITIALIZE, :DECONT);
    }

    proto method Dict(|) is nodal {*}
    multi method Dict(Dict:) { self  }
}

{
    use MONKEY-TYPING;

    augment class Any {
        multi method Dict(Any:) { Dict.new(self) }
    }
}

# vim: ft=perl6 expandtab sw=4
