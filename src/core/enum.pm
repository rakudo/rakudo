role Enum[$key, $value, $enumeration] {
    method key() {
        $key
    }
    
    method value() {
        $value
    }

    method WHAT() {
        $enumeration
    }
}

# XXX Just defining this here, but Rakudo will need it properly before we
# can actually use any of this code.
class EnumMap { }

class Enumeration {
    has $!name;
    has $.enums;

    method new(::UnderlyingType, $name, @values) {
        # Create instance.
        my $self = self.bless(*, :name($name));
        
        # Create all of the enum values.
        my %value_objects;
        my UnderlyingType $current; 
        for @values -> $value {
            my ($key, $val);
            if $value ~~ Pair {
                $key = $value.key;
                $current = $val = $value.value;
            }
            else {
                $key = $value;
                $val = $current++;
            }
            %value_objects{$key} = $val but Enum[$key, $val, $self];
        }

        # Set enum map in place.
        pir::setattribute__vPsP($self, '$!enums', EnumMap.new(%value_objects));
    }

    method defined() {
        0
    }

    method WHAT() {
        self
    }

    method Str() {
        $!name;
    }
}
