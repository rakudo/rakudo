# Type Environment is an associative mapping generic type names into final types. Only to be provided by MOP.

my class TypeEnv { # declared in BOOTSTRAP
# my class TypeEnv is Map
    my role LexPad {
        multi method AT-KEY(::?CLASS:D: Str:D $key --> Mu) is raw {
            nqp::ifnull(nqp::getlexrel(nqp::getattr(self, Map, '$!storage'), $key), Nil)
        }
        multi method AT-KEY(::?CLASS:D: Mu \key --> Mu) is raw {
            nqp::ifnull(nqp::getlexrel(nqp::getattr(self, Map, '$!storage'), key.Str), Nil)
        }

        multi method EXISTS-KEY(::?CLASS:D: Str:D $key --> Mu) is raw {
            ! nqp::isnull(nqp::getlexrel(nqp::getattr(self, Map, '$!storage'), $key))
        }
        multi method EXISTS-KEY(::?CLASS:D: Mu \key --> Mu) is raw {
            ! nqp::isnull(nqp::getlexrel(nqp::getattr(self, Map, '$!storage'), key.Str))
        }
    }

    method new-from-ctx(Mu \ctx) {
        nqp::p6bindattrinvres(
            nqp::create(nqp::reprname(ctx) eq 'MVMContext' ?? self.WHAT but LexPad !! self.WHAT),
            Map, '$!storage', ctx)
    }

    method instantiate(::?CLASS:D: Mu \obj --> Mu) is raw {
        my Mu \type_environment = nqp::getattr(self, Map, '$!storage');
        nqp::can(obj.HOW, 'instantiate_generic')
            ?? obj.^instantiate_generic(self)
            !! obj.INSTANTIATE-GENERIC(self)
    }
}

Metamodel::Configuration.set_type_env_type(TypeEnv);
