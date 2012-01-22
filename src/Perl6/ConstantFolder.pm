# Quick and dirty constant folder; probably needs a bunch more work.
class Perl6::ConstantFolder {
    # Tries to fold. Throws if it's not possible.
    method fold($expr, $scope, $world) {
        if $expr<has_compile_time_value> {
            # It's already got a compile time value, just hand it back.
            $expr
        }
        elsif $expr.isa(PAST::Op) && $expr.name && ($expr.pasttype eq '' || $expr.pasttype eq 'call') {
            # Potentially foldable call. Try to get compile time args (which
            # my involve recursively folding).
            my @args;
            for @($expr) {
                my $arg := self.fold($_, $scope, $world);
                if $arg<has_compile_time_value> {
                    @args.push($arg<compile_time_value>)
                }
                else {
                    pir::die("No compile time value obtainable for argument to " ~ $expr.name);
                }
            }
            
            # Got constant or folded arguments. Look for symbol and invoke it with
            # the arguments.
            my $routine := self.locate_symbol($scope, $expr.name);
            my $result := $routine(|@args);
            
            # Add folded symbol into the world (which'll return a PAST ref to it).
            $world.add_constant_folded_result(pir::nqp_decontainerize__PP($result))
        }
    }
    
    # Look for a symbol needed in the constant folding.
    method locate_symbol($scope, $name) {
        my $block := $scope;
        while $block {
            my %sym := $block.symbol($name);
            if +%sym {
                if nqp::existskey(%sym, 'value') {
                    return %sym<value>;
                }
                else {
                    pir::die("No compile time value for $name");
                }
            }
            $block := $block<outer>;
        }
        pir::die("Could not locate compile time $name");
    }
}
