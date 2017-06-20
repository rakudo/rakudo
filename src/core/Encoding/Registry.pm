my class X::Encoding::Unknown { ... }
my class X::Encoding::AlreadyRegistered { ... }

my class Encoding::Registry {
    my $lock := Lock.new;
    my %lookup;

    method register(Encoding $enc --> Nil) {
        my @names = ($enc.name, $enc.alternative-names).flat.map(*.fc);
        $lock.protect: {
            if %lookup{@names}:k -> @bad {
                X::Encoding::AlreadyRegistered.new(name => @bad[0]).throw;
            }
            %lookup{@names} = $enc xx *;
        }
    }

    method find(Str() $name) {
        $lock.protect: {
            my $fname = $name.fc;
            %lookup{$fname}:exists
                ?? %lookup{$fname}
                !! X::Encoding::Unknown.new(:$name).throw
        }
    }
}
