class Version is List {
    has Bool $.plus = False;
    method new(*@positional, :$plus) {
        self.List::new(@positional).MYBUILD(?$plus);
    }
    submethod MYBUILD(Bool $plus) {
        $!plus = $plus;
        self;
    }
    multi method Str(Version:D:) {
        'v' ~ self.map({ $_ ~~ Whatever ?? '*' !! $_}).join('.') ~ ($!plus ?? '+' !! '');
    }
    multi method gist(Version:D:) { self.Str }
    multi method perl(Version:D:) {
        self.^name ~ '.new(' ~ self.List::perl ~ ', :plus(' ~ $!plus.perl ~ '))';

    }
    multi method ACCEPTS(Version:D: Version:D $other) {
        for self.kv -> $i, $v {
            next if $v ~~ Whatever;
            my $o = $other[$i];
            return True unless defined $o;
            next if $o ~~ Whatever;
            return $.plus if $o after  $v;
            return False  if $o before $v;
        }
        True;
    }
}
