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
        pir::setattribute__0PPsP(
            pir::repr_instance_of__PP(List),
            List, '$!rest', pir::clone__PP($!params));
    }
    
    # XXX TODO: Parameter separators.
    multi method perl() {
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
