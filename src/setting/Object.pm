subset Matcher of Object where { $_.can('ACCEPTS') };

class Object is also {
    multi method perl {
        self.WHAT.substr(0, -2) ~ '.new()';
    }

    multi method eigenstates {
        list(self)
    }

    method WALK(:$name!, :$canonical, :$ascendant, :$descendant, :$preorder, :$breadth,
                :$super, Matcher :$omit, Matcher :$include) {
        # First, build list of classes in the order we'll need them.
        my @classes;
        if $super {
            @classes = self.^isa();
        } else {
            if $breadth {
                die ":breadth unimplemented";
            } elsif $ascendant {
                die ":ascendant unimplemented";
            } elsif $descendant {
                die ":descendant unimplemented";
            } elsif $preorder {
                die ":preorder unimplemented";
            } else {
                # Canonical, the default.
                my sub merge_c3(@to_merge) {
                    my $accepted;
                    my $found = 0;
                    my $cand_count = 0;
                    loop (my $i = 0; $i < +@to_merge; $i++) {
                        if +@to_merge[$i] {
                            $cand_count++;
                            my $cand_class = @to_merge[$i][0];
                            my $reject = False;
                            loop (my $j = 0; $j < +@to_merge; $j++) {
                                if $i != $j {
                                    loop (my $k = 1; $k < +@to_merge[$j]; $k++) {
                                        if @to_merge[$j][$k].WHAT =:= $cand_class.WHAT {
                                            $reject = True;
                                            last;
                                        }
                                    }
                                }
                            }
                            unless $reject {
                                $accepted = $cand_class;
                                $found = 1;
                                last;
                            }
                        }
                    }
                    if !$cand_count {
                        return ();
                    }
                    if !$found {
                        die "Could not build C3 linearization: ambiguous hierarchy";
                    }
                    for @to_merge -> @cur_list is rw {
                        @cur_list .= grep({ $^class.WHAT !=:= $accepted.WHAT });
                    }
                    my @result = merge_c3(@to_merge);
                    unshift @result, $accepted;
                    return @result;
                }
                my sub compute_c3($class) {
                    my @immediates = $class.^isa();
                    if @immediates.elems == 0 {
                        @classes = $class;
                    } else {
                        my @to_merge = @immediates.map({ [compute_c3($^parent)] });
                        push @to_merge, [@immediates];
                        my @merged = merge_c3(@to_merge);
                        unshift @merged, $class;
                        return @merged;
                    }
                }
                @classes = compute_c3(self.WHAT());
            }
        }

        # Filter as needed.
        if $omit {
            @classes .= grep { !$omit.ACCEPTS($_) };
        }
        if $include {
            @classes .= grep { !$include.ACCEPTS($_) };
        }

        # Now we have classes, build method list.
        my @methods;
        for @classes -> $class {
            for $class.^methods() -> $method {
                my $check_name = $method.?name;
                if $check_name.defined && $check_name eq $name {
                    @methods.push($method);
                }
            }
        }

        return @methods;
    }
}

# vim: ft=perl6
