# Here we start to piece together the top of the object model hierarchy.
# We can't just declare these bits in CORE.setting with normal Perl 6
# syntax due to circularity issues. Note that we don't compose any of
# these - which is equivalent to a { ... } body.
#
# One particular circularity we break here is that you can't have
# inheritance in Perl 6 without traits, but that needs multiple
# dispatch, which can't function without some a type hierarchy in
# place. It also needs us to be able to declare a signature with
# parameters and a code objects with dispatchees, which in turn need
# attributes. So, we set up quite a few bits in here, though the aim
# is to keep it "lagom". :-)

# Bootstrapping Attribute class that we eventually replace with the read
# one.
my class BOOTSTRAPATTR {
    has $!name;
    has $!type;
    method name() { $!name }
    method type() { $!type }
}

# class Mu { ... }
my $Mu := Perl6::Metamodel::ClassHOW.new_type(:name<Mu>);

# class Any is Mu { ... }
my $Any := Perl6::Metamodel::ClassHOW.new_type(:name<Any>);
$Any.HOW.add_parent($Any, $Mu);

# class Cool is Any { ... }
my $Cool := Perl6::Metamodel::ClassHOW.new_type(:name<Cool>);
$Cool.HOW.add_parent($Cool, $Any);

# class Attribute is Cool {
#     has $!name; # Has to be an bootstrap attribute object for now
#     has $!type;
#     ... # Uncomposed
# }
my $Attribute := Perl6::Metamodel::ClassHOW.new_type(:name<Attribute>);
$Attribute.HOW.add_parent($Attribute, $Cool);
$Attribute.HOW.add_attribute($Attribute, BOOTSTRAPATTR.new(:name<$!name>, :type($Mu)));
$Attribute.HOW.add_attribute($Attribute, BOOTSTRAPATTR.new(:name<$!type>, :type($Mu)));

# class Signature is Cool {
#    has $!params;
#     ... # Uncomposed
# }
my $Signature := Perl6::Metamodel::ClassHOW.new_type(:name<Signature>);
$Signature.HOW.add_parent($Signature, $Cool);
$Signature.HOW.add_attribute($Signature, BOOTSTRAPATTR.new(:name<$!params>, :type($Mu)));

# class Parameter is Cool {
#     has str $!name
#     has $!nom_type
#     ... # Uncomposed
# }
my $Parameter := Perl6::Metamodel::ClassHOW.new_type(:name<Parameter>);
$Parameter.HOW.add_parent($Parameter, $Cool);
$Parameter.HOW.add_attribute($Parameter, BOOTSTRAPATTR.new(:name<$!name>, :type(str)));
$Parameter.HOW.add_attribute($Parameter, BOOTSTRAPATTR.new(:name<$!nom_type>, :type($Mu)));

# class Code is Cool {
#     has $!do;
#     has $!signature;
#     has $!dispatchees;
#     ... # Uncomposed
# }
my $Code := Perl6::Metamodel::ClassHOW.new_type(:name<Code>);
$Code.HOW.add_parent($Code, $Cool);
$Attribute.HOW.add_attribute($Attribute, BOOTSTRAPATTR.new(:name<$!do>, :type($Mu)));
$Attribute.HOW.add_attribute($Attribute, BOOTSTRAPATTR.new(:name<$!signature>, :type($Mu)));
$Attribute.HOW.add_attribute($Attribute, BOOTSTRAPATTR.new(:name<$!dispatchees>, :type($Mu)));

# class Block is Code { ... }
my $Block := Perl6::Metamodel::ClassHOW.new_type(:name<Block>);
$Block.HOW.add_parent($Block, $Code);

# class Routine is Block { ... }
my $Routine := Perl6::Metamodel::ClassHOW.new_type(:name<Routine>);
$Routine.HOW.add_parent($Routine, $Block);

# class Sub is Routine { ... }
my $Sub := Perl6::Metamodel::ClassHOW.new_type(:name<Sub>);
$Sub.HOW.add_parent($Sub, $Routine);

# class Method is Routine { ... }
my $Method := Perl6::Metamodel::ClassHOW.new_type(:name<Method>);
$Method.HOW.add_parent($Method, $Routine);

# Build up EXPORT::DEFAULT.
my module EXPORT {
    our module DEFAULT {
        $?PACKAGE.WHO<Mu>        := $Mu;
        $?PACKAGE.WHO<Any>       := $Any;
        $?PACKAGE.WHO<Cool>      := $Cool;
        $?PACKAGE.WHO<Attribute> := $Attribute;
        $?PACKAGE.WHO<Signature> := $Signature;
        $?PACKAGE.WHO<Parameter> := $Parameter;
        $?PACKAGE.WHO<Code>      := $Code;
        $?PACKAGE.WHO<Block>     := $Block;
        $?PACKAGE.WHO<Routine>   := $Routine;
        $?PACKAGE.WHO<Sub>       := $Sub;
        $?PACKAGE.WHO<Method>    := $Method;
    }
}
