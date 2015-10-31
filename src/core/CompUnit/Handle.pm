class CompUnit::Handle {
    has Mu $!module_ctx;
    has Mu $!unit;

    submethod new(Mu \module_ctx) {
        my $self := nqp::create(self);
        nqp::bindattr($self, CompUnit::Handle, '$!module_ctx', module_ctx);
        $self
    }

    submethod from-unit(Stash $unit) {
        my $self := nqp::create(self);
        nqp::bindattr($self, CompUnit::Handle, '$!unit', nqp::decont($unit));
        $self
    }

    # If the compilation unit has a callable EXPORT subroutine, it will
    # be returned here. A Callable type object otherwise.
    method export-sub() returns Callable {
        my $module := self.unit;
        if $module and nqp::existskey($module, '&EXPORT') {
            nqp::atkey($module, '&EXPORT');
        }
        else {
            Callable
        }
    }

    # The EXPORT package from the UNIT of the compilation unit; a
    # Stash type object if none
    method export-package() returns Stash {
        my $module := self.unit;
        if $module and nqp::existskey($module, 'EXPORT') {
            my $EXPORT := nqp::atkey($module, 'EXPORT');
            nqp::istype($EXPORT.WHO, Stash)
                ?? $EXPORT.WHO
                !! nqp::p6bindattrinvres(nqp::create(Stash), Map, '$!storage', $EXPORT.WHO);
        }
        else {
            Stash
        }
    }

    # The EXPORTHOW package from the UNIT of the compilation unit;
    # a Stash type object if none.
    method export-how-package() returns Stash {
        ...
    }

    # The GLOBALish package from the UNIT of the compilation unit
    # (the module's contributions to GLOBAL, for merging); a Stash
    # type object if none.
    method globalish-package() { # returns Stash {
        if nqp::defined($!module_ctx) {
            my $lexpad := nqp::ctxlexpad($!module_ctx);
            nqp::isnull(nqp::atkey($lexpad, 'GLOBALish')) ?? Stash !! nqp::atkey($lexpad, 'GLOBALish')
        }
        else {
            Stash
        }
    }

    method unit() {
        nqp::defined($!unit)
            ?? $!unit
            !! nqp::defined($!module_ctx) ?? nqp::ctxlexpad($!module_ctx) !! {}
    }
}

# vim: ft=perl6 expandtab sw=4
