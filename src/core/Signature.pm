my class Signature {
    method params() {
        pir::setattribute__0PPsP(
            pir::repr_instance_of__PP(List),
            List, '$!rest', $!params);
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
