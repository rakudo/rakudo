my class Signature {
    method arity() {
        my $params := self.params;
        my $i      := 0;
        my $elems  := $params.elems;
        my $arity  := 0;
        while $i < $elems {
            if $params[$i].positional && !$params[$i].optional {
                $arity := $arity + 1;
                $i := $i + 1;
            }
            else {
                $i := $elems;
            }
        }
        $arity
    }
    
    method count() {
        my $params := self.params;
        my $i      := 0;
        my $elems  := $params.elems;
        my $arity  := 0;
        while $i < $elems {
            if $params[$i].positional {
                $arity := $arity + 1;
                $i := $i + 1;
            }
            else {
                $i := $elems;
            }
        }
        $arity
    }
    
    method params() {
        pir::perl6_list_from_rpa__PPPP(List, pir::clone__PP($!params), Mu);
    }
    
    # XXX TODO: Parameter separators.
    method perl() {
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
}
