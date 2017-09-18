my class X::Encoding::Unknown { ... }
my class X::Encoding::AlreadyRegistered { ... }

my class Encoding::Registry {
    my $lock := Lock.new;
    my $lookup := nqp::hash;

    method register(Encoding $enc --> Nil) {
        $lock.protect: {
            nqp::stmts(
              nqp::if(
                nqp::existskey($lookup,(my str $key = $enc.name.fc)),
                X::Encoding::AlreadyRegistered.new(name => $enc.name).throw,
                nqp::bindkey($lookup,$key,$enc)
              ),
              (my $names :=
                nqp::getattr($enc.alternative-names,List,'$!reified')),
              (my int $elems = nqp::elems($names)),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::if(
                  nqp::existskey($lookup,($key = nqp::atpos($names,$i).fc)),
                  X::Encoding::AlreadyRegistered.new(
                    name => nqp::atpos($names,$i)).throw,
                  nqp::bindkey($lookup,$key,$enc)
                )
              )
            )
        }
    }

    method find(Str() $name) {
        $lock.protect: {
            nqp::ifnull(
              nqp::atkey($lookup,$name.fc),
              X::Encoding::Unknown.new(:$name).throw
            )
        }
    }
}
