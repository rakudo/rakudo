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

    proto method instantiate(::?CLASS:D: Mu --> Mu) is raw {*}
    multi method instantiate(::?CLASS:D: ContainerDescriptor:D \descriptor) is raw {
        if nqp::getenvhash<RAKUDO_DEBUG> {
            note "+++ instantiating container descriptor ", descriptor.name;
        }
        descriptor.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    multi method instantiate(::?CLASS:D: Attribute:D \attr) is raw {
        attr.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    multi method instantiate(::?CLASS:D: Scalar:D \container) is raw {
        container.VAR.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    multi method instantiate(::?CLASS:D: Signature:D \sign) is raw {
        sign.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    multi method instantiate(::?CLASS:D: Parameter:D \param) is raw {
        param.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    multi method instantiate(::?CLASS:D: Code:D \code) is raw {
        code.instantiate_generic(nqp::getattr(self, Map, '$!storage'))
    }
    # This is slow path.
    multi method instantiate(::?CLASS:D: Mu \obj --> Mu) is raw {
        nqp::can(obj.HOW, 'instantiate_generic')
            ?? obj.^instantiate_generic(self)
            !! obj.INSTANTIATE-GENERIC(self)
    }
}

Metamodel::Configuration.set_type_env_type(TypeEnv);
