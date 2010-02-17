augment class Mu {
    method Bool { $.defined }

    multi method notdef() { !self.defined; }

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
}
