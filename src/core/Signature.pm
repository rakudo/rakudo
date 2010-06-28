augment class Signature {
    method ACCEPTS(Signature) {
        die("Sorry, smart-matching a Signature against a Signature is not yet implemented.");
    }
    
    method ACCEPTS(Callable) {
        die("Sorry, smart-matching a Callable against a Signature is not yet implemented.");
    }
    
    method ACCEPTS(Capture $c) {
        my $result = Bool::False;
        try {
            self!BIND($c);
            $result = Bool::True;
        }
        $result
    }
    
    method ACCEPTS($any) {
        my $result = Bool::False;
        try {
            self!BIND((|$any).Capture);
            $result = Bool::True;
        }
        $result
    }

    method perl() {
        my @parts = gather {
            take ':(';
            my $sep = '';
            my $last_was_multi_inv = True;
            for $.params -> $param {
                # First, separator, if any.
                if $last_was_multi_inv && !$param.multi_invocant { $sep = ';; ' }
                take ~$sep;
                $sep = ', ';

                # First the type.
                my $name = $param.name;

                # work around TT #1560
                $name = '' unless chars($name);

                if !$param.slurpy {
                    my $sigil = substr($name, 0, 1);
                    my $perl = $param.type.perl;
                    if $sigil eq '$' {
                        take $perl ~ ' ';
                    }
                    elsif $sigil eq '@' {
                        if $perl ne 'Positional' {
                            take substr($perl, 11, $perl.chars - 12) ~ ' ';
                        }
                    }
                    elsif $sigil eq '%' {
                        if $perl ne 'Associative' {
                            take substr($perl, 12, $perl.chars - 13) ~ ' ';
                        }
                    }
                    elsif substr($perl, 0, 8) eq 'Callable' {
                        if $perl ne 'Callable' {
                            take substr($perl, 9, $perl.chars - 10) ~ ' ';
                        }
                    }
                    else {
                        take $perl ~ ' ';
                    }
                }

                # Any type captures.
                for @($param.type_captures) -> $name {
                    take '::' ~ $name ~ ' ';
                }

                # Slurpiness, namedness, then the name.
                if $param.slurpy  { take '*' }
                if $param.capture { take '|' }
                if $param.parcel  { take '\|' }
                my @names = @($param.named_names);
                for @names -> $name {
                    take ':' ~ $name ~ '(';
                }
                take $name;
                take ')' x +@names;

                # Optionality.
                if $param.optional && !$param.named && !$param.default   { take '?' }
                elsif !$param.optional && $param.named && !$param.slurpy { take '!' }

                # Any constraints?
                my $cons_perl = $param.constraints.perl;
                if $cons_perl ne 'Bool::True' {
                    take ' where ' ~ $cons_perl;
                }
                
                # Any sub-signature?
                if $param.signature {
                    take ' ' ~ substr($param.signature.perl, 1);
                }

                # Default.
                if $param.default {
                    take ' = ' ~ $param.default.perl;
                }

                # Invocant/multi invocant marking.
                if $param.invocant { $sep = ': '; }
                $last_was_multi_inv = $param.multi_invocant;
            }
            take ')';
        };
        return @parts.join('');
    }
}

# vim: ft=perl6
