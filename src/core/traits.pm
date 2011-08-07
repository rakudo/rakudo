use Perl6::Metamodel;

# Stub these or we can't use any sigil other than $.
my role Positional { ... }
my role Associative { ... }
my role Callable { ... }

proto trait_mod:<is>(|$) { * }
multi trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    $child.HOW.add_parent($child, $parent);
}
multi trait_mod:<is>(Mu:U $type, :$rw!) {
    $type.HOW.set_rw($type);
}

multi trait_mod:<is>(Attribute:D $attr, :$rw!) {
    $attr.set_rw();
}
multi trait_mod:<is>(Attribute:D $attr, :$readonly!) {
    $attr.set_readonly();
}

multi trait_mod:<is>(Routine:D $r, :$rw!) {
    $r.set_rw();
}

multi trait_mod:<is>(Parameter:D $param, :$readonly!) {
    # This is the default.
}
multi trait_mod:<is>(Parameter:D $param, :$rw!) {
    $param.set_rw();
}
multi trait_mod:<is>(Parameter:D $param, :$copy!) {
    $param.set_copy();
}

# TODO: Make this much less cheaty. That'll probably need the
# full-blown serialization, though.
multi trait_mod:<is>(Routine:D \$r, :$export!) {
    if %*COMPILING {
        my @tags = 'ALL', 'DEFAULT';
        for @tags -> $tag {
            my $install_in;
            if $*EXPORT.WHO.exists($tag) {
                $install_in := $*EXPORT.WHO.{$tag};
            }
            else {
                $install_in := $*ST.pkg_create_mo((package { }).HOW.WHAT, :name($tag));
                $*ST.pkg_compose($install_in);
                $*ST.install_package_symbol($*EXPORT, $tag, $install_in);
            }
            $*ST.install_package_symbol($install_in, '&' ~ $r.name, $r);
        }
    }
}

multi trait_mod:<is>(Mu:D $docee, $doc, :$docs!) {
    $docee does role {
        has $!WHY;
        method WHY          { $!WHY      }
        method set_docs($d) { $!WHY = $d }
    }
    $docee.set_docs($doc);
}

multi trait_mod:<is>(Mu:U $docee, $doc, :$docs!) {
    $docee.HOW.set_docs($doc);
}


proto trait_mod:<does>(|$) { * }
multi trait_mod:<does>(Mu:U $doee, Mu:U $role) {
    $doee.HOW.add_role($doee, $role)
}

proto trait_mod:<of>(|$) { * }
multi trait_mod:<of>(Mu:U $target, Mu:U $type) {
    # XXX Ensure we can do this, die if not.
    $target.HOW.set_of($target, $type);
}
multi trait_mod:<of>(Routine:D $target, Mu:U $type) {
    $target.signature.set_returns($type)
}

proto trait_mod:<returns>(|$) { * }
multi trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    $target.signature.set_returns($type)
}

proto trait_mod:<as>(|$) { * }
multi trait_mod:<as>(Parameter:D $param, $type) {
    $param.set_coercion($type);
}

my class Pair { ... }
proto trait_mod:<handles>(|$) { * }
multi trait_mod:<handles>(Attribute:D $target, $thunk) {
    $target does role {
        has $.handles;
        
        method set_handles($expr) {
            $!handles := $expr;
        }
        
        method add_delegator_method($attr: $pkg, $meth_name, $call_name) {
            $pkg.HOW.add_method($pkg, $meth_name, method (**@pos, *%named) {
                $attr.get_value(self)."$call_name"(|@pos, |%named)
            });
        }
        
        method apply_handles(Mu $pkg) {
            sub applier($expr) {
                if $expr.defined() {
                    if $expr ~~ Str {
                        self.add_delegator_method($pkg, $expr, $expr);
                    }
                    elsif $expr ~~ Pair {
                        self.add_delegator_method($pkg, $expr.key, $expr.value);
                    }
                    elsif $expr ~~ Positional {
                        for $expr.list {
                            applier($_);
                        }
                    }
                    else {
                        # XXX Matcher.
                    }
                }
                else {
                    # XXX Todo.
                }
            }
            applier($!handles);
        }
    };
    $target.set_handles($thunk());
}

proto trait_mod:<will>(|$) { * }
multi trait_mod:<will>(Attribute $attr, Block $closure, :$build!) {
    $attr.set_build($closure)
}

proto trait_mod:<trusts>(|$) { * }
multi trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.HOW.add_trustee($truster, $trustee);
}
