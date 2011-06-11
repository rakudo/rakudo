# XXX: should also be Cool, but attributes and MI don't seem to mix yet
my class Rat is Real {
    has $.numerator;
    has $.denominator;

    method new(Rat:U: Int \$nu, Int \$de) {
        my $new = self.CREATE;
        $new.BUILD($nu, $de);
        $new;
    }
    method BUILD(Int \$nu, Int \$de) {
        $!numerator   = $nu;
        $!denominator = $de;
    }

    method nude() { $!numerator, $!denominator }
    method Num() {
        $!numerator.Num / $!denominator.Num
    }

    method Bridge() { self.Num }
    method Rat() { self }
    method Str() {
        self.Num.Str
    }
    method perl() {
        $!numerator ~ '/' ~ $!denominator
    }
    method succ {
        Rat.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        Rat.new($!numerator - $!denominator, $!denominator);
    }
}
