# should really be X::Base
my class X-Base {
    has $!backtrace;
    has $!message;
	
    method Str() {
	# Just a stub so far.
	$!message.Str // 'Something went wrong'
    }
#    method ID() { ... }
}
