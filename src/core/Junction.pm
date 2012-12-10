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

    multi method ACCEPTS(Junction:D: Mu \topic) {
        ($!storage.map({return True if $_.ACCEPTS(topic)}).gimme(*); return False)
            if $!type eq 'any';
        ($!storage.map({return False unless $_.ACCEPTS(topic)}).gimme(*); return True) 
            if $!type eq 'all';
        ($!storage.map({return False if $_.ACCEPTS(topic)}).gimme(*); return True)
            if $!type eq 'none';
        # 'one' junction
        my $count = 0;
        $!storage.map({ $count++ if $_.ACCEPTS(topic); return False if $count > 1 }).gimme(*);
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
            -> $obj, |c { $obj(|c) },
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

sub AUTOTHREAD(&call, |args) {
    # Look for a junctional arg in the positionals.
    my Mu $pos_rpa := nqp::getattr(nqp::p6decont(args), Capture, '$!list');
    loop (my int $i = 0; $i < nqp::elems($pos_rpa); $i = $i + 1) {
        # Junctional positional argument?
        my Mu $arg := nqp::atpos($pos_rpa, $i);
        if nqp::istype($arg, Junction) {
            my @states := nqp::getattr(nqp::p6decont($arg), Junction, '$!storage');
            my $type   := nqp::getattr(nqp::p6decont($arg), Junction, '$!type');
            my Mu $res := nqp::list();
            for @states -> $s {
                # Next line is Officially Naughty, since captures are meant to be
                # immutable. But hey, it's our capture to be naughty with...
                nqp::bindpos($pos_rpa, $i, $s);
                nqp::push($res, call(|args));
                Nil;
            }
            return Junction.new(nqp::p6parcel($res, Nil), :type($type));
        }
    }
    
    # Otherwise, look for one in the nameds.
    for args.hash.kv -> $k, $v {
        if nqp::istype($v, Junction) {
            my Mu $nam_hash := nqp::getattr(nqp::p6decont(args), Capture, '$!hash');
            my @states := nqp::getattr(nqp::p6decont($v), Junction, '$!storage');
            my $type   := nqp::getattr(nqp::p6decont($v), Junction, '$!type');
            my Mu $res := nqp::list();
            for @states -> $s {
                nqp::bindkey($nam_hash, $k, $s);
                nqp::push($res, call(|args));
                Nil;
            }
            return Junction.new(nqp::p6parcel($res, Nil), :type($type));
        }
    }
    
    # If we get here, wasn't actually anything to autothread.
    call(|args);
}

sub AUTOTHREAD_METHOD($name, |c) {
    AUTOTHREAD(
        -> $obj, |c { $obj."$name"(|c) },
        |c);
}

pir::perl6_setup_junction_autothreading__vPP(Junction, &AUTOTHREAD);
Mu.HOW.setup_junction_fallback(Junction, &AUTOTHREAD_METHOD);
