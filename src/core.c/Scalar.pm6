my class Scalar { # declared in BOOTSTRAP
    # class Scalar is Any
    #     has Mu $!descriptor;
    #     has Mu $!value;

    method new(|) { X::Cannot::New.new(class => self.WHAT).throw }

    multi method WHICH(Scalar:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            'Scalar|',
            nqp::tostr_I(nqp::objectid($!descriptor))
          ),
          ValueObjAt
        )
    }
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Nil !! $d.name()
    }

    proto method of() {*}
    multi method of(Scalar:U:) { Mu }
    multi method of(Scalar:D:) {
        nqp::isnull($!descriptor) ?? Mu !! $!descriptor.of
    }

    method default() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Any !! $d.default;
    }
    method dynamic() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? False !! nqp::hllbool($d.dynamic);
    }
}

# Also compose native reference classes declared in BOOTSTRAP.
my class IntLexRef  { }
my class NumLexRef  { }
my class StrLexRef  { }
my class IntAttrRef { }
my class NumAttrRef { }
my class StrAttrRef { }
my class IntPosRef  { }
my class NumPosRef  { }
my class StrPosRef  { }

# vim: expandtab shiftwidth=4
