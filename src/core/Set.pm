class Set does Associative {
    # We could use a hash here, but right now hash keys coerce to Str,
    # so instead let's use an array and &uniq for the time being.
    has @!elems;

    multi method new(@elems) {
        self.bless(self.CREATE, :elems( uniq @elems ));
    }
    multi method new(*@elems) {
        self.bless(self.CREATE, :elems( uniq @elems ));
    }
    multi method new(%elems) {
        self.bless(self.CREATE, :elems( %elems.keys ));
    }
    multi method new(Set $set) {
        $set;
    }

    sub contains(@array, $value) {
        for @array {
            if $value === $_ {
                return True;
            }
        }
        return False;
    }

    method keys() { @!elems }
    method values() { True xx +@!elems }
    method elems() { +@!elems }
    method exists($elem) { contains(@!elems, $elem) }

    method Num() { +self.elems }
    method Bool() { ?self.elems }

    multi method union(@otherset) {
        self.new((@!elems, @otherset));
    }
    multi method union(%otherset) {
        self.union(%otherset.keys);
    }

    multi method intersection(@otherset) {
        self.new(grep { contains(@otherset, $_) }, @!elems);
    }
    multi method intersection(%otherset) {
        self.intersection(%otherset.keys);
    }

    multi method difference(%otherset) {
        self.difference(%otherset.keys);
    }
    multi method difference(@otherset) {
        self.new(grep { !contains(@otherset, $_) }, @!elems);
    }

    multi method subsetorequal(@otherset) {
        ?contains(@otherset, all(@!elems));
    }
    multi method subsetorequal(%otherset) {
        self.subsetorequal(%otherset.keys);
    }

    multi method supersetorequal(@otherset) {
        ?contains(@!elems, all(@otherset));
    }
    multi method supersetorequal(%otherset) {
        self.supersetorequal(%otherset.keys);
    }

    method equal($otherset) {
        +self == +$otherset && self.subsetorequal($otherset);
    }

    method subset($otherset) {
        +self < +Set.new($otherset) && self.subsetorequal($otherset);
    }

    method superset($otherset) {
        +self > +Set.new($otherset) && self.supersetorequal($otherset);
    }

    method perl() {
        'Set.new(' ~ join(', ', map { .perl }, @!elems) ~ ')';
    }
}

our multi sub  infix:<(|)>(Set $a, %b) { $a.union(%b) }
our multi sub  infix:<(|)>(    %a, %b) { Set.new( %a).union(%b) }
our multi sub  infix:<(|)>(    @a, %b) { Set.new(|@a).union(%b) }
our multi sub  infix:<(|)>(    @a, @b) { Set.new(|@a).union(@b) }

our multi sub  infix:<(&)>(Set $a, %b) { $a.intersection(%b) }
our multi sub  infix:<(&)>(    %a, %b) { Set.new( %a).intersection(%b) }
our multi sub  infix:<(&)>(    @a, %b) { Set.new(|@a).intersection(%b) }
our multi sub  infix:<(&)>(    @a, @b) { Set.new(|@a).intersection(@b) }

our multi sub  infix:<(-)>(Set $a, %b) { $a.difference(%b) }
our multi sub  infix:<(-)>(    %a, %b) { Set.new( %a).difference(%b) }
our multi sub  infix:<(-)>(    @a, %b) { Set.new(|@a).difference(%b) }
our multi sub  infix:<(-)>(    @a, @b) { Set.new(|@a).difference(@b) }

our multi sub infix:<(<=)>(Set $a, %b) { $a.subsetorequal(%b) }
our multi sub infix:<(<=)>(    %a, %b) { Set.new( %a).subsetorequal(%b) }
our multi sub infix:<(<=)>(    @a, %b) { Set.new(|@a).subsetorequal(%b) }
our multi sub infix:<(<=)>(    @a, @b) { Set.new(|@a).subsetorequal(@b) }

our multi sub infix:«(>=)»(Set $a, %b) { $a.supersetorequal(%b) }
our multi sub infix:«(>=)»(    %a, %b) { Set.new( %a).supersetorequal(%b) }
our multi sub infix:«(>=)»(    @a, %b) { Set.new(|@a).supersetorequal(%b) }
our multi sub infix:«(>=)»(    @a, @b) { Set.new(|@a).supersetorequal(@b) }

our multi sub  infix:<(<)>(Set $a, %b) { $a.subset(%b) }
our multi sub  infix:<(<)>(    %a, %b) { Set.new( %a).subset(%b) }
our multi sub  infix:<(<)>(    @a, %b) { Set.new(|@a).subset(%b) }
our multi sub  infix:<(<)>(    @a, @b) { Set.new(|@a).subset(@b) }

our multi sub  infix:«(>)»(Set $a, %b) { $a.superset(%b) }
our multi sub  infix:«(>)»(    %a, %b) { Set.new( %a).superset(%b) }
our multi sub  infix:«(>)»(    @a, %b) { Set.new(|@a).superset(%b) }
our multi sub  infix:«(>)»(    @a, @b) { Set.new(|@a).superset(@b) }

# vim: ft=perl6
