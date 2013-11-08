# A KeyReducer provides a thread-safe way to compose a hash from multiple
# sources.
my class X::KeyReducer::ResultObtained is Exception {
    method message() { "Cannot contribute to a KeyReducer after the result has been obtained" }
}
my class KeyReducer {
    has $!initializer;
    has $!reducer;
    has %!result;
    has Mu $!lock;
    has $!exception;
    has $!obtained;
    
    method new($initializer, $reducer) {
        self.bless(:$initializer, :$reducer)
    }
    
    my Mu $interop;
    my Mu $ReentrantLock;
    submethod BUILD(:$!initializer, :$!reducer) {
        unless nqp::isconcrete($interop) {
            $interop := nqp::jvmbootinterop();
            $ReentrantLock := $interop.typeForName('java.util.concurrent.locks.ReentrantLock');
        }
        $!lock := $ReentrantLock.'constructor/new/()V'();
        $!obtained = False;
    }
    
    proto method contribute(|) { * }
    multi method contribute(KeyReducer:D: %h) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            return False;
        }
        if $!obtained {
            $!lock.unlock();
            X::KeyReducer::ResultObtained.new.throw
        }
        try {
            for %h.kv -> $k, $v {
                %!result{$k} = %!result.exists($k)
                    ?? $!reducer(%!result{$k}, $v)
                    !! $!initializer($v)
            }
            CATCH { default { $!exception := $_ } }
        }
        $!lock.unlock();
        True
    }
    multi method contribute(KeyReducer:D: Pair $p) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            return False;
        }
        if $!obtained {
            $!lock.unlock();
            X::KeyReducer::ResultObtained.new.throw
        }
        try {
            %!result{$p.key} = %!result.exists($p.key)
                    ?? $!reducer(%!result{$p.key}, $p.value)
                    !! $!initializer($p.value);
            CATCH { default { $!exception := $_ } }
        }
        $!lock.unlock();
        True
    }
    
    method snapshot(KeyReducer:D:) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            $!exception.throw;
        }
        my %snapshot = %!result;
        $!lock.unlock();
        %snapshot
    }
    
    method result(KeyReducer:D:) {
        $!lock.lock();
        $!obtained = True;
        $!lock.unlock();
        $!exception ?? $!exception.throw !! %!result
    }
}
