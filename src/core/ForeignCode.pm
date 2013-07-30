# Takes a foreign code object and tries to make it feel somewhat like a Perl
# 6 one. Note that it doesn't have signature information we can know about.

my class ForeignCode does Callable { # declared in BOOTSTRAP
    # class ForeignCode {
    #     has $!do;                # Code object we delegate to

    method arity() { self.signature.arity }
    
    method count() { self.signature.count }
    
    method signature(ForeignCode:D:) { (sub (|) { }).signature }
    
    method name() { (nqp::can($!do, 'name') ?? $!do.name !! nqp::getcodename($!do)) || '<anon>' }
    
    multi method gist(ForeignCode:D:) { self.name }
    
    multi method Str(ForeignCode:D:) { self.name }
}
