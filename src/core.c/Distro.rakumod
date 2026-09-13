# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic {
    has Str $.release  is built(:bind);
    has Bool $.is-win  is built(False);
    has Str $.path-sep is built(:bind);

    submethod TWEAK (--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,$!name.lc.trans(" " => ""));  # lowercase spaceless
        $!is-win := so $!name eq any <mswin32 mingw msys cygwin>;
    }

    # This is a temporary migration method needed for installation
    method cur-sep() { "," }
}

# vim: expandtab shiftwidth=4
