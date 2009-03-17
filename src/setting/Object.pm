class Object is also {
    multi method perl {
        self.WHAT ~ '.new()';
    }

    multi method eigenstates {
        list(self)
    }
}

# vim: ft=perl6
