class Version {
    has $.parts;
    has Bool $.plus = False;

    multi method new(Str:D $s) {
        my @parts = $s.comb(/:r '*' || \d+ || <.alpha>+/);
        for @parts {
            $_ .= Numeric if .Numeric.defined ;
            $_ = * if $_ eq '*';
        }
        self.bless(*, :parts(@parts), :plus($s.substr(*-1) eq '+'));
    };
    multi method new(*@parts, :$plus) {
        self.bless(*, :parts(@parts.eager), :plus(?$plus));
    }

    multi method Str(Version:D:) {
        'v' ~ $!parts.map({ $_ ~~ Whatever ?? '*' !! $_}).join('.') ~ ($!plus ?? '+' !! '');
    }
    multi method gist(Version:D:) { self.Str }
    multi method perl(Version:D:) {
        self.^name ~ '.new(' ~ $!parts.perl ~ ', :plus(' ~ $!plus.perl ~ '))';

    }
    multi method ACCEPTS(Version:D: Version:D $other) {
        for $!parts.kv -> $i, $v {
            next if $v ~~ Whatever;
            my $o = $other.parts[$i];
            return True unless defined $o;
            next if $o ~~ Whatever;
            return $.plus if $o after  $v;
            return False  if $o before $v;
        }
        True;
    }

    multi method WHICH(Version:D:) {
        my $s := join '|', self.^name, self.Str;
        nqp::box_s(nqp::unbox_s($s), ObjAt);
    }
}

multi sub infix:<eqv>(Version:D $a, Version:D $b) {
    $a.WHAT === $b.WHAT && $a.Str eq $b.Str
}

