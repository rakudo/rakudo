my role Distribution::Utils {
    has $!collapsed-meta;
    has $!depends;

    method meta {...}

    # ?TODO? The sub looks like a good candidate for public API method of a more generic class or role.
    # system-collapse() has been borrowsed from Nick Logan's zef (Zef::Utils::SystemQuery).
    my sub system-collapse($data) {
        return $data unless $data ~~ Hash|Array;

        my sub walk(@path, $idx, $query-source) {
            X::CompUnit::META::DependencySyntax.new(:reason("can't find \$*{@path[0].uc}.{@path[1..*].join('.')}")).throw
                if !$query-source.^can("{@path[$idx]}") && $idx < @path.elems;
            return $query-source."{@path[$idx]}"()
                if $idx+1 == @path.elems;
            return walk(@path, $idx+1, $query-source."{@path[$idx]}"());
        }

        my $return = $data.WHAT.new;

        for $data.keys -> $idx {
            given $idx {
                when /^'by-env-exists'/ {
                    my $key = $idx.split('.')[1];
                    my $value = %*ENV{$key}:exists ?? 'yes' !! 'no';
                    X::CompUnit::META::DependencySyntax.new(:reason("unable to resolve path: {$idx} in \%*ENV, had: {$value}")).throw
                        unless $data{$idx}{$value}:exists;
                    return system-collapse($data{$idx}{$value});
                }
                when /^'by-env'/ {
                    my $key = $idx.split('.')[1];
                    my $value = %*ENV{$key};
                    X::CompUnit::META::DependencySyntax.new(:reason("unable to resolve path: {$idx} in \%*ENV, had: {$value // ''}")).throw
                        unless defined($value) && ($data{$idx}{$value}:exists);
                    return system-collapse($data{$idx}{$value});
                }
                when /^'by-' (distro|kernel|perl|vm)/ {
                    my $query-source = do given $/[0] {
                        when 'distro' { $*DISTRO }
                        when 'kernel' { $*KERNEL }
                        when 'perl'   { $*RAKU   }
                        when 'raku'   { $*RAKU   }
                        when 'vm'     { $*VM     }
                    }
                    my $path  = $idx.split('.');
                    my $value = walk($path, 1, $query-source).Str; # to stringify e.g. True
                    my $fkey  = ($data{$idx}{$value}:exists)
                        ?? $value
                        !! ($data{$idx}{''}:exists)
                            ?? ''
                            !! Any;

                    X::CompUnit::META::DependencySyntax.new(:reason("unable to resolve path: {$path.cache[*-1].join('.')} in \$*DISTRO, had: {$value} ~~ {$value.WHAT.^name}")).throw
                        if Any ~~ $fkey;
                    return system-collapse($data{$idx}{$fkey});
                }
                default {
                    my $val = system-collapse($data ~~ Array ?? $data[$idx] !! $data{$idx});
                    $return{$idx} = $val
                        if $return ~~ Hash;
                    $return.push($val)
                        if $return ~~ Array;

                }
            };
        }

        $return
    }

    method collapsed-meta {
        ⚛$!collapsed-meta // cas $!collapsed-meta, {
            $_ // system-collapse(self.meta)
        }
    }

    # Returns a list of dependency specifications as found in meta after system-collpasing it. If all dep.specs are
    # requested then the order is: build, test, and runtime.
    method meta-depends($type = *) {
        my %meta = ⚛$!collapsed-meta // self.collapsed-meta;

        my sub expand-dep-list(\dep-specs) {
            dep-specs.map({ $_ ~~ Associative && .<any> ?? .<any>.Slip !! $_ })
        }

        my proto sub collect-deps(|) {*}
        multi sub collect-deps(Whatever) {
            <build test runtime>.map({ samewith($_).Slip })
        }
        multi sub collect-deps('runtime') {
            (%meta<depends> andthen
                ($_ ~~ Positional
                    ?? expand-dep-list($_)
                    !! (.<runtime> andthen .<requires> andthen expand-dep-list($_))))
            || ()
        }
        multi sub collect-deps(Str:D $type) {
            (%meta{$type ~ '-depends'} andthen expand-dep-list($_).Slip) || Empty,
            (%meta<depends> ~~ Associative
                && (%meta<depends>{$type}
                    andthen .<requires>
                    andthen expand-dep-list($_).Slip)) || Empty
        }

        collect-deps($type)
    }

    method !depends(::?CLASS:D:) {
        my regex DepSpec { ^
            [ $<module>=[[<.ident> [ <[ ' \- ]> <.ident> ]*]+ % '::']
                || [<-[:]>*]+ % '::'
                    { X::CompUnit::META::DependencySyntax.new(
                        :reason($/.chars ?? "bad module name '$/'" !! "no module name found in '{$/.orig}'")
                        ).throw } ]
            [ $<spec>=[ <?before <.ws> ':' <.alpha>> .+ | \s*]
                || { X::CompUnit::META::DependencySyntax.new(:reason("no adverbs found in '{$/.postmatch}'")).throw } ]
        $ };
        # For thread-safety we hold the depends hash in a Scalar to be able to use atomics with it.
        ⚛$!depends // cas $!depends, {
            $_ // do {
                self.meta-depends.map({
                    my %adverbs;
                    my $name;
                    if $^dep-spec ~~ Associative {
                        %adverbs<from> = $_ with $dep-spec<from>;
                        $name = $dep-spec<name>;
                    }
                    else {
                        $name = $dep-spec;
                    }
                    unless $name ~~ /<DepSpec>/ {
                        X::CompUnit::META::DependencySyntax.new(:reason("failed to parse '$name'")).throw
                    }
                    my ($module, $spec) = ~.<module>, ~(.<spec> || '') with $<DepSpec>;
                    X::CompUnit::META::DependencySyntax.new(:reason($!.message)).throw
                        if try { %adverbs = EVAL('(' ~ $spec ~ ')') } === Nil;
                    $module => %adverbs
                }).Hash
            }
        }
    }

    method module-dependency(::?CLASS:D: Str:D $module_name --> Hash:D) is implementation-detail {
        my %deps := nqp::decont(⚛$!depends // self!depends);
        return $_ with %deps{$module_name};
        # If $module_name is not explicitly mentioned as a dependency then try to locate it's distribution and then
        # locate the distribution in our dependencies. It is possible that a module is provided by more than one
        # distribution in which case we try them all and use the first one matching a dependency record.
        for CompUnit::RepositoryRegistry.candidates($module_name) -> $dist {
            return $_ with %deps{$dist.meta<name>};
        }
        # When no matching dependency found
        %()
    }
}