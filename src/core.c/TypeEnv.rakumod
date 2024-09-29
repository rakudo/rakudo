# Type Environment is an associative mapping generic type names into final types. Only to be provided by MOP.
# TypeEnv implements caching on per-context and per-object basis. The caches only make sense until all required
# instnatiations are completed. Normally it means the period of time between a role specializer invokes role body code
# and until the specializer finishes and produces complete concretization. To avoid wasting resources per-context cache
# slots must be dropped when no more needed. We do it by claiming the instance of TypeEnv, which creates a missing
# slot, the primary for that slot. The primary is also responsible for removal of the slot when destroyed. If the
# underlying context is preserved after that and a new TypeEnv gets created it would become a new primary and will start
# producing new instantiations for the same generics.

my class TypeEnv { # declared in BOOTSTRAP
# my class TypeEnv is Map {
#     has $!primary;
#     has $!WHICH;

    method new(|) { callsame()!INIT() }

    method !flatten-ctx(Mu \ctx, Mu \ctx-hash, :$boundary-by) {
        my Mu $cur-ctx := ctx;
        while nqp::isconcrete($cur-ctx) {
            # Stop iterating outers if there is no boundary symbol in the current context (say, `::?ROLE` defines role's
            # generic context); or if we iterated all the way out to the core where it'd be way to expensive to collect
            # all available symbols while there is no sense in doing it.
            last if ($boundary-by && nqp::isnull(nqp::getlexrel($cur-ctx, $boundary-by)))
                    || nqp::existskey($cur-ctx, 'CORE-SETTING-REV');

            my Mu \iter := nqp::iterator(nqp::ctxlexpad($cur-ctx));
            while iter {
                my Mu \pair = nqp::shift(iter);
                my $sym := nqp::iterkey_s(pair);
                unless nqp::existskey(ctx-hash, $sym) {
                    nqp::bindkey(ctx-hash, $sym, (my \v = nqp::iterval(pair)));
                }
            }

            $cur-ctx := try nqp::ctxouter($cur-ctx);
        }
    }

    method new-from-ctx(Mu \ctx, Str :$boundary-by --> ::?CLASS:D) is raw {
        my Mu $type-env;
        my $ctx-repr := nqp::reprname(ctx);
        my Mu $ctx-hash;
        my Mu $which-object;
#?if jvm
        if nqp::reprname(ctx) eq 'ContextRef' {
#?endif
#?if !jvm
        if nqp::reprname(ctx) eq 'MVMContext' {
#?endif
            $which-object := ctx;
            self!flatten-ctx(ctx, ($ctx-hash := nqp::hash()), :$boundary-by);
        }
        elsif nqp::istype(ctx, Map) {
            $ctx-hash := nqp::getattr(ctx, Map, '$!storage');
        }
        elsif nqp::ishash(ctx) {
            $ctx-hash := ctx;
        }
        else {
            die "Can't create a TypeEnv from an instance of '" ~ ctx.^name ~ "'";
        }

        $type-env := nqp::p6bindattrinvres(nqp::create(self.WHAT), Map, '$!storage', $ctx-hash);

        # If created from a nqp::ctx() then use it as our ID for WHICH. Otherwise use the hashe we've got either
        # directly as the argument, or indirectly from a Map.
        $type-env!INIT(nqp::isconcrete($which-object) ?? $which-object !! $ctx-hash)
    }

    method primary { ? $!primary }

    my Mu $ins-cache := nqp::hash();
    my $cache-lock = Lock.new;

    method !INIT(Mu $which-object? is raw) {
        $cache-lock.protect: {
            my $self-key := nqp::isconcrete($which-object) ?? self!WHICH-FROM($which-object) !! self.WHICH;
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
            my \ctx = nqp::getattr(self, Map, '$!storage');
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

    method !WHICH-FROM(Mu \ctx) {
        $!WHICH := nqp::box_s( self.^name ~ "|" ~ nqp::objectid(ctx), ValueObjAt )
    }

    # Bind own identity to the underlying context object. This way two independently created instances of TypeEnv
    # would be considered the same entity if they share the same context.
    multi method WHICH(::?CLASS:D:) {
        $!WHICH // self!WHICH-FROM(nqp::getattr(self, Map, '$!storage'))
    }
}

Metamodel::Configuration.set_type_env_type(TypeEnv);
