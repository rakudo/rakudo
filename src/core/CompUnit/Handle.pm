class CompUnit::Handle {
    # If the compilation unit has a callable EXPORT subroutine, it will
    # be returned here. A Callable type object otherwise.
    method export-sub() returns Callable {
        ...
    }

    # The EXPORT package from the UNIT of the compilation unit; a
    # Stash type object if none
    method export-package() returns Stash {
        ...
    }

    # The EXPORTHOW package from the UNIT of the compilation unit;
    # a Stash type object if none.
    method export-how-package() returns Stash {
        ...
    }

    # The GLOBALish package from the UNIT of the compilation unit
    # (the module's contributions to GLOBAL, for merging); a Stash
    # type object if none.
    method globalish-package() returns Stash {
        ...
    }
}
