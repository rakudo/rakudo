class CompUnit::Handle {
    has Mu $!module_ctx;
    has Mu $!unit;

    proto submethod new(|) {*}
    multi submethod new() {
        nqp::create(self)
    }

    method ctxsave() {
        $!module_ctx := nqp::ctxcaller(nqp::ctx()) unless $!module_ctx;
    }

    multi submethod new(Mu \module_ctx) {
        nqp::p6bindattrinvres(
          nqp::create(self),CompUnit::Handle,'$!module_ctx', module_ctx
        )
    }

    submethod from-unit(Stash $unit) {
        nqp::p6bindattrinvres(
          nqp::create(self),CompUnit::Handle,'$!unit',nqp::decont($unit)
        )
    }

    # If the compilation unit has a callable EXPORT subroutine, it will
    # be returned here. Nil otherwise.
    method export-sub(--> Callable:D) {
        my $module := self.unit;
        $module && nqp::existskey($module, '&EXPORT')
          ?? nqp::atkey($module, '&EXPORT')
          !! Nil
    }

    # The EXPORT package from the UNIT of the compilation unit; a
    # Nil if none
    method export-package(--> Stash:D) {
        my $module := self.unit;
        if $module and nqp::existskey($module, 'EXPORT') {
            my $EXPORT := nqp::atkey($module, 'EXPORT');
            nqp::istype($EXPORT.WHO, Stash)
                ?? $EXPORT.WHO
                !! nqp::p6bindattrinvres(nqp::create(Stash), Map, '$!storage', $EXPORT.WHO);
        }
        else {
            Nil
        }
    }

    # The EXPORTHOW package from the UNIT of the compilation unit;
    # Nil if none.
    method export-how-package(--> Stash:D) {
        my $module := self.unit;
        if $module and nqp::existskey($module, 'EXPORTHOW') {
            my $EXPORTHOW := nqp::atkey($module, 'EXPORTHOW');
            my $who := $EXPORTHOW.WHO;
            nqp::istype($who, Stash)
                ?? $who
                !! nqp::p6bindattrinvres(nqp::create(Stash), Map, '$!storage', $who);
        }
        else {
            Nil
        }
    }

    # The GLOBALish package from the UNIT of the compilation unit
    # (the module's contributions to GLOBAL, for merging);
    # Nil if none.
    method globalish-package() { # returns Stash {
        nqp::if(
          nqp::defined($!module_ctx),
          nqp::ifnull(nqp::atkey(nqp::ctxlexpad($!module_ctx),'GLOBALish').WHO, Nil),
          nqp::if(nqp::defined($!unit), $!unit, Nil)
        )
    }

    method unit() {
        nqp::defined($!unit)
            ?? $!unit
            !! nqp::defined($!module_ctx) ?? nqp::ctxlexpad($!module_ctx) !! {}
    }
}

# vim: expandtab shiftwidth=4
