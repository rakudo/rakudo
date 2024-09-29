class Collation {
    has int $.collation-level = 85;
    has $!Country = 'International';
    method gist {
        "collation-level => $!collation-level, Country => $!Country, " ~
        "Language => None, primary => {self.primary}, secondary => {self.secondary}, " ~
        "tertiary => {self.tertiary}, quaternary => {self.quaternary}"
    }
    method set (
        Int :$primary    = 1,
        Int :$secondary  = 1,
        Int :$tertiary   = 1,
        Int :$quaternary = 1)
    {
        my int $i = 0;
        $i += 1   if $primary.sign    ==  1;
        $i += 2   if $primary.sign    == -1;

        $i += 4   if $secondary.sign  ==  1;
        $i += 8   if $secondary.sign  == -1;

        $i += 16  if $tertiary.sign   ==  1;
        $i += 32  if $tertiary.sign   == -1;

        $i += 64  if $quaternary.sign ==  1;
        $i += 128 if $quaternary.sign == -1;
        $!collation-level = $i;
        self;
    }
    method check ($more, $less) {
        # Hopefully the user didn't set collation-level manually to have a level
        # both enabled *and* disabled. But check if this is the case anyway.
        return  0 if $!collation-level +& all($more,$less);
        return  1 if $!collation-level +& $more;
        return -1 if $!collation-level +& $less;
        return  0;
    }
    method primary     { self.check( 1,   2) }
    method secondary   { self.check( 4,   8) }
    method tertiary    { self.check(16,  32) }
    method quaternary  { self.check(64, 128) }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*COLLATION', {
    PROCESS::<$COLLATION> := Collation.new;
}

# vim: expandtab shiftwidth=4
