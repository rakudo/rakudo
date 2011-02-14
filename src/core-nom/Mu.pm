my class Mu {
    proto method ACCEPTS($topic) { * }
    multi method ACCEPTS(Mu:U $self: $topic) {
        pir::type_check__IPP($topic, self.WHAT)
    }

    method Bool() is parrot_vtable('get_bool') {
        self.defined
    }

    method defined() is parrot_vtable('defined') {
        pir::repr_defined(self)
    }

    method item {
        # This is overridden by non-items.
        self
    }

    proto method Str() is parrot_vtable('get_string') { * }
    multi method Str(Mu:U $self:) {
        self.HOW.name(self) ~ '()'
    }

    ##
    ## Parrot Interop
    ##

    method () is parrot_vtable('elements') {
        self.elems
    }

    method () is parrot_vtable('get_bool') {
        self.Bool
    }

    method () is parrot_vtable('get_integer') {
        self.Int
    }

    method () is parrot_vtable('get_number') {
        self.Num
    }

    method () is parrot_vtable('get_string') {
        self.Str
    }
}
