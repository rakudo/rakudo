class Str is also {
    multi method encode($encoding = 'UTF-8', $nf = '') {
        my @bytes = Q:PIR {
            .local int bin_coding, i, max, byte
            .local string bin_string
            .local pmc it, result
            $S0 = self
            bin_coding = find_encoding 'fixed_8'
            bin_string = trans_encoding $S0, bin_coding
            result = new ['ResizablePMCArray']
            i = 0
            max = length bin_string
          bytes_loop:
            if i >= max goto bytes_done
            byte = ord bin_string, i
            push result, byte
            inc i
            goto bytes_loop
          bytes_done:
            %r = result
        };
        return Buf.new(|@bytes);
    }
    
    multi method Complex() {
	# this is a really ugly hack for now
	# we should properly parse self and also
	# extract an imaginary part
	(+self).Complex;
    }
}
