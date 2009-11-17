class Parameter {
#    multi method new(*%args) {
#        for <rw ref copy named slurpy optional invocant> -> $n {
#            # %args{$n}.=true doesn't seem to work here.
#            %args{$n} = ?%args{$n} if %args.exists($n);
#        }
#        self.bless(*, |%args);
#    }
    has $.name;
    has $.type;
    has $.constraints;
    has $.rw;
    has $.ref;
    has $.copy;
    method readonly() { !$!rw && !$!ref && !$!copy }
    has $.named;
    has $.named_names;
    has $.slurpy;
    has $.optional;
    has $.default;
    has $.invocant;
    has $.multi_invocant;
    has $.type_captures;
    has $.signature;
}

# vim: ft=perl6
