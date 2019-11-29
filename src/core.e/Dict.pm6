class Dict is Map {
    has ValueObjAt $!WHICH;

    multi method new(Dict: *@args --> Dict:D) {
        self.Map::new.STORE(@args, :INITIALIZE, :DECONT);
    }

    method STORE(Dict:D: @args, :$INITIALIZE --> Dict:D) {
        self.Map::STORE(@args, :$INITIALIZE, :DECONT);
    }

    proto method Dict(|) is nodal {*}
    multi method Dict(Dict:) { self  }

    multi method WHICH(Dict:D: --> ValueObjAt:D) {
        nqp::if(
          nqp::isconcrete($!WHICH),
          $!WHICH,
          ($!WHICH := nqp::box_s(
            nqp::concat(
              nqp::if(
                nqp::eqaddr(self.WHAT,Dict),
                'Dict|',
                nqp::concat(self.^name,'|')
              ),
              nqp::sha1(self.sort.map({ .key ~ "|" ~ .value }).join("|"))
            ),
            ValueObjAt
          ))
        )
    }
}

{
    use MONKEY-TYPING;

    augment class Any {
        multi method Dict(Any:) { Dict.new(self) }
    }
}

# vim: ft=perl6 expandtab sw=4
