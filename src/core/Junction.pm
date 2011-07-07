my class Junction is Mu {
    has $!storage;             # elements of Junction
    has $!type;                # type of Junction

    method new(*@values, :$type) {
        self.bless(*, :storage(@values.eager), :$type);
    }

    multi method Bool(Junction:D:) {
        ($!storage.map({return True if $_}).gimme(*); return False)
            if $!type eq 'any';
        ($!storage.map({return False unless $_}).gimme(*); return True) 
            if $!type eq 'all';
        ($!storage.map({return False if $_}).gimme(*); return True)
            if $!type eq 'none';
        # 'one' junction
        my $count = 0;
        $!storage.map({ $count++ if $_; return False if $count > 1 }).gimme(*);
        $count == 1;
    }

    multi method Str(Junction:D:) {
        self.new($!storage.map({$_.Str}), :type($!type))
    }

    multi method ACCEPTS(Junction:D: Mu \$topic) {
        ($!storage.map({return True if $_.ACCEPTS($topic)}).gimme(*); return False)
            if $!type eq 'any';
        ($!storage.map({return False unless $_.ACCEPTS($topic)}).gimme(*); return True) 
            if $!type eq 'all';
        ($!storage.map({return False if $_.ACCEPTS($topic)}).gimme(*); return True)
            if $!type eq 'none';
        # 'one' junction
        my $count = 0;
        $!storage.map({ $count++ if $_.ACCEPTS($topic); return False if $count > 1 }).gimme(*);
        $count == 1;
    }

    

    submethod BUILD(:$!storage, :$!type) { }

    multi method perl(Junction:D:) { 
        $!type ~ '(' ~ $!storage.map({$_.perl}).join(', ') ~ ')'
    }
}

sub any(*@values) { Junction.new(@values, :type<any>); }
sub all(*@values) { Junction.new(@values, :type<all>); }
sub one(*@values) { Junction.new(@values, :type<one>); }
sub none(*@values) { Junction.new(@values, :type<none>); }

sub infix:<|>(*@values) { Junction.new(@values, :type<any>); }
sub infix:<&>(*@values) { Junction.new(@values, :type<all>); }
sub infix:<^>(*@values) { Junction.new(@values, :type<one>); }

