use Perl6::Metamodel;

# Top level classes.
my class Mu { ... }
my class Any is Mu { ... }
my class Cool is Any { ... }

# Attribute.
# XXX Needs to get all its methods from a role in Perl6::Metamodel so they are
# available to the compiler.
my class Attribute is Cool { ... }

{YOU_ARE_HERE}
