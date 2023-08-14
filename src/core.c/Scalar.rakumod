my class Scalar { # declared in BOOTSTRAP
    # class Scalar is Any
    #     has Mu $!descriptor;
    #     has Mu $!value;

    method new(|) { X::Cannot::New.new(class => self.WHAT).throw }

    multi method WHICH(Scalar:D \SELF: --> ObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::concat(nqp::unbox_s(SELF.^name), '|'),
            nqp::tostr_I(nqp::objectid(SELF))
          ),
          ObjAt
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
my class UIntLexRef  { }
my class NumLexRef  { }
my class StrLexRef  { }
my class IntAttrRef { }
my class UIntAttrRef { }
my class NumAttrRef { }
my class StrAttrRef { }
my class IntPosRef  { }
my class UIntPosRef  { }
my class NumPosRef  { }
my class StrPosRef  { }

# vim: expandtab shiftwidth=4
