class Collation {
    has int $.collation-level = 15;
    has $!Country = 'International';
    method gist {
        "collation-level => $!collation-level, Country => $!Country, " ~
        "Language => None, primary => {self.primary}, secondary => {self.secondary}, " ~
        "tertiary => {self.tertiary}, quaternary => {self.quaternary}"
    }
    #proto method set (|) { * }
    #multi method set (Int :$collation-level!) {
    #    $!collation-level = $collation-level;
    #}
    method set (Bool :$primary = self.primary,
        Bool :$secondary = self.secondary, Bool :$tertiary = self.tertiary,
        Bool :$quaternary = self.quaternary)
    {
        my int $i = 0;
        $i += 1 if $primary;
        $i += 2 if $secondary;
        $i += 4 if $tertiary;
        $i += 8 if $quaternary;
        $!collation-level = $i;
    }
    method primary     { so $!collation-level +& 1 }
    method secondary   { so $!collation-level +& 2 }
    method tertiary    { so $!collation-level +& 4 }
    method quaternary  { so $!collation-level +& 8 }
}
PROCESS::<$COLLATION> = Collation.new;
