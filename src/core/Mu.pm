subset Matcher of Mu where { .can('ACCEPTS') };

augment class Mu {
    method Bool { $.defined }

    method item {
        # This is overridden by non-items.
        self;
    }

    multi method notdef() { !self.defined; }

    multi method perl {
        substr(~self.WHAT, 0, -2) ~ '.new()';
    }
    
    method print() {
        print(self);
    }
    
    method say() {
        say(self);
    }

    method Capture() {
        my %attrs;
        my @mro = self, self.^parents;
        for @mro -> $class {
            for $class.^attributes() -> $attr {
                if $attr.has_accessor {
                    my $name = substr($attr.name, 2);
                    %attrs{$name} //= self."$name"();
                }
            }
        }
        %attrs.Capture()
    }

    method WALK(:$name!, :$canonical, :$ascendant, :$descendant, :$preorder, :$breadth,
                :$super, Matcher :$omit, Matcher :$include) {
        # First, build list of classes in the order we'll need them.
        my @classes;
        if $super {
            @classes = self.^parents(:local);
        } else {
            if $breadth {
                my @search_list = self.WHAT;
                while @search_list {
                    push @classes, @search_list;
                    my @new_search_list;
                    for @search_list -> $current {
                        for $current.^parents(:local) -> $next {
                            unless @new_search_list.grep(* === $next) {
                                push @new_search_list, $next;
                            }
                        }
                    }
                    @search_list = @new_search_list;
                }
            } elsif $ascendant | $preorder {
                sub build_ascendent(Mu $class) {
                    unless @classes.grep(* === $class) {
                        push @classes, $class;
                        for $class.^parents(:local) {
                            build_ascendent($^parent);
                        }
                    }
                }
                build_ascendent(self.WHAT);
            } elsif $descendant {
                sub build_descendent(Mu $class) {
                    unless @classes.grep(* === $class) {
                        for $class.^parents(:local) {
                            build_descendent($^parent);
                        }
                        push @classes, $class;
                    }
                }
                build_descendent(self.WHAT);
            } else {
                # Canonical, the default (just whatever the meta-class says) with us
                # on the start.
                @classes = self.^parents();
                @classes.unshift(self.WHAT);
            }
        }

        # Now we have classes, build method list.
        my @methods;
        for @classes -> $class {
            if (!defined($include) || $include.ACCEPTS($class)) &&
              (!defined($omit) || !$omit.ACCEPTS($class)) {
                try {
                    for $class.^methods(:local) -> $method {
                        my $check_name = $method.?name;
                        if $check_name.defined && $check_name eq $name {
                            @methods.push($method);
                        }
                    }
                }
            }
        }

        return @methods;
    }
}
