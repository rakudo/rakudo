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

    multi method gist(Junction:D:) {
        $!type ~ '(' ~ $!storage.map({$_.gist}).join(', ') ~ ')'
    }
    
    multi method perl(Junction:D:) { 
        $!type ~ '(' ~ $!storage.map({$_.perl}).join(', ') ~ ')'
    }
    
    method postcircumfix:<( )>($c) {
        AUTOTHREAD(
            -> $obj, **@cpos, *%cnamed { $obj(|@cpos, |%cnamed) },
            self, |$c);
    }
}

sub any(*@values) { Junction.new(@values, :type<any>); }
sub all(*@values) { Junction.new(@values, :type<all>); }
sub one(*@values) { Junction.new(@values, :type<one>); }
sub none(*@values) { Junction.new(@values, :type<none>); }

sub infix:<|>(**@values) { Junction.new(@values, :type<any>); }
sub infix:<&>(**@values) { Junction.new(@values, :type<all>); }
sub infix:<^>(**@values) { Junction.new(@values, :type<one>); }

sub AUTOTHREAD(&call, **@pos, *%named) {
    # Look for a junctional arg in the positionals.
    loop (my $i = 0; $i < +@pos; $i++) {
        # Junctional positional argument?
        if @pos[$i] ~~ Junction {
            my @states := nqp::getattr(nqp::p6decont(@pos[$i]), Junction, '$!storage');
            my @pre    := @pos[0 ..^ $i];
            my @post   := @pos[$i + 1 ..^ +@pos];
            my @result;
            for @states -> $s {
                push @result, call(|@pre, $s, |@post, |%named);
            }
            return Junction.new(@result,
                :type(nqp::getattr(nqp::p6decont(@pos[$i]), Junction, '$!type')));
        }
    }
    
    # Otherwise, look for one in the nameds.
    for %named.kv -> $k, $v {
        if $v ~~ Junction {
            my %other_nameds;
            for %named.kv -> $kk, $vk {
                if $kk ne $k { %other_nameds{$kk} = $vk }
            }
            my @states := nqp::getattr(nqp::p6decont($v), Junction, '$!storage');
            my @result;
            for @states -> $s {
                push @result, call(|@pos, |{ $k => $s }, |%other_nameds);
            }
            return Junction.new(@result,
                :type(nqp::getattr(nqp::p6decont($v), Junction, '$!type')));
        }
    }
    
    # If we get here, wasn't actually anything to autothread.
    call(|@pos, |%named);
}

sub AUTOTHREAD_METHOD($name, **@pos, *%named) {
    AUTOTHREAD(
        -> $obj, **@cpos, *%cnamed { $obj."$name"(|@cpos, |%cnamed) },
        |@pos, |%named);
}

pir::perl6_setup_junction_autothreading__vPP(Junction, &AUTOTHREAD);
Mu.HOW.setup_junction_fallback(Junction, &AUTOTHREAD_METHOD);
