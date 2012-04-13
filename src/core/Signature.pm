my class Signature {
    # declared in BOOTSTRAP.pm:
    #   is Cool;               # parent class
    #   has $!params;          # RPA of parameters
    #   has $!returns;         # return type
    #   has $!arity;           # cached arity
    #   has $!count;           # cached count

    method arity() {
        self.count if nqp::isnull($!arity) || !$!arity.defined;
        $!arity;
    }
 
    method count() {
        if nqp::isnull($!count) || !$!count.defined {
            # calculate the count and arity -- we keep them
            # cached for when we're called the next time.
            my $count = 0;
            my $arity = 0;
            my Mu $iter := nqp::iterator($!params);
            my $param;
            while $iter {
                $param := nqp::shift($iter);
                if $param.capture || $param.slurpy && !$param.named { 
                    $count = Inf; 
                }
                elsif $param.positional {
                    $count++;
                    $arity++ unless $param.optional;
                }
            }
            nqp::bindattr(self, Signature, '$!arity', $arity);
            nqp::bindattr(self, Signature, '$!count', $count);
        }
        $!count
    }
              
    method params() {
        nqp::p6list(nqp::clone($!params), List, Mu);
    }
    
    # XXX TODO: Parameter separators.
    multi method perl(Signature:D:) {
        # Opening.
        my $perl = ':(';
        
        # Parameters.
        my $params = self.params();
        my $sep = '';
        my $i = 0;
        while $i < $params.elems {
            $perl = $perl ~ $sep ~ $params[$i].perl;
            $sep = ', ';
            $i = $i + 1;
        }
        
        # Closer.
        $perl ~ ')'
    }
    
    method returns() { $!returns }
}
