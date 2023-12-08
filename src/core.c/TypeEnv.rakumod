# Type Environment is an associative mapping generic type names into final types. Only to be provided by MOP.
# TypeEnv implements caching on per-context and per-object basis. The caches only make sense until all required
# instnatiations are completed. Normally it means the period of time between a role specializer invokes role body code
# and until the specializer finishes and produces complete concretization. To avoid wasting resources per-context cache
# slots must be dropped when no more needed. We do it by claiming the instance of TypeEnv, which creates a missing
# slot, the primary for that slot. The primary is also responsible for removal of the slot when destroyed. If the
# underlying context is preserved after that and a new TypeEnv gets created it would become a new primary and will start
# producing new instantiations for the same generics.

my class TypeEnv::LexPad {...}
my class TypeEnv { # declared in BOOTSTRAP
# my class TypeEnv is Map {
#     has $!primary;
#     has $!WHICH;

    method new(|) { callsame()!INIT() }

    method new-from-ctx(Mu \ctx) {
        nqp::p6bindattrinvres(
            nqp::create(nqp::reprname(ctx) eq 'MVMContext' ?? TypeEnv::LexPad !! self.WHAT),
            Map,
            '$!storage',
            (nqp::istype(ctx, Map) ?? nqp::getattr(ctx, Map, '$!storage') !! ctx) )!INIT()
    }

    method primary { ? $!primary }

    my Mu $ins-cache := nqp::hash();
    my $cache-lock = Lock.new;

    method !INIT() {
        $cache-lock.protect: {
            my $self-key := self.WHICH;
            unless nqp::existskey($ins-cache, $self-key) {
                $!primary := True;
                nqp::bindkey($ins-cache, $self-key, nqp::hash());
            }
        }
        self
    }

    submethod DESTROY {
        if $!primary {
            $cache-lock.protect: {
                nqp::deletekey($ins-cache, self.WHICH);
            }
        }
    }

    proto method instantiate(::?CLASS:D: Mu --> Mu) is raw {*}
    multi method instantiate(::?CLASS:D: ContainerDescriptor:D \descriptor) is raw {
        descriptor.instantiate_generic(self)
    }
    multi method instantiate(::?CLASS:D: Attribute:D \attr) is raw {
        attr.instantiate_generic(self)
    }
    multi method instantiate(::?CLASS:D: Scalar:D \container) is raw {
        container.VAR.instantiate_generic(self)
    }
    multi method instantiate(::?CLASS:D: Signature:D \sign) is raw {
        sign.instantiate_generic(self)
    }
    multi method instantiate(::?CLASS:D: Parameter:D \param) is raw {
        param.instantiate_generic(self)
    }
    multi method instantiate(::?CLASS:D: Code:D \code) is raw {
        code.instantiate_generic(self)
    }
    # This is slow path.
    multi method instantiate(::?CLASS:D: Mu \obj --> Mu) is raw {
        nqp::can(obj.HOW, 'instantiate_generic')
                        ?? obj.^instantiate_generic(self)
                        !! obj.INSTANTIATE-GENERIC(self);
    }

    # Caching is using two different storages at the same time. The first one is the global $ins-cache hash. The second
    # is our context where we keep resolved instantiatiations of non-parameteric types (classes, mostly). The latter
    # is using the convention by which symbols that need instantiation are bound to lexicals with names starting with
    # "!INS_OF_" prefix.
    method cache(::?CLASS:D: Mu \obj, &instantiator? --> Mu) is raw {
        my Mu $ins;
        $cache-lock.protect: {
            my $ins-lexical = '!INS_OF_' ~ obj.^name;
            my \ctx = self.ctx;
            my $has-lexical := nqp::existskey(ctx, $ins-lexical);
            my $obj-id;
            my Mu $obj-idx;

            if $has-lexical {
                $ins := nqp::atkey(ctx, $ins-lexical);
            }
            elsif !$has-lexical {
                my $self-key := self.WHICH;
                $obj-id := ~nqp::objectid(obj);
                $obj-idx := nqp::atkey($ins-cache, $self-key);
                $ins := nqp::atkey($obj-idx, $obj-id);
            }

            if nqp::eqaddr(obj, $ins) || nqp::isnull($ins) {
                $ins := nqp::decont(&instantiator ?? &instantiator() !! self.instantiate(obj));
                if $has-lexical {
                    nqp::bindkey(ctx, $ins-lexical, $ins);
                }
                else {
                    nqp::bindkey($obj-idx, $obj-id, $ins);
                }
            }
        }
        $ins
    }

    # Bind own identity to the underlying context object. This way two independently created instances of TypeEnv
    # would be considered the same entity if they share the same context.
    multi method WHICH(::?CLASS:D:) {
        $!WHICH // ($!WHICH := nqp::box_s(
            self.^name ~ "|" ~ nqp::objectid(nqp::getattr(self, Map, '$!storage')),
            ValueObjAt
        ))
    }
}

my class TypeEnv::LexPad is TypeEnv {
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

Metamodel::Configuration.set_type_env_type(TypeEnv);
