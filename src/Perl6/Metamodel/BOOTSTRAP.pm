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
    method compose($obj) { }
}

# class Mu { ... }
my stub Mu metaclass Perl6::Metamodel::ClassHOW { ... };
pir::set_binder_top_type__vP(Mu);

# class Any is Mu { ... }
my stub Any metaclass Perl6::Metamodel::ClassHOW { ... };
Any.HOW.add_parent(Any, Mu);

# class Cool is Any { ... }
my stub Cool metaclass Perl6::Metamodel::ClassHOW { ... };
Cool.HOW.add_parent(Cool, Any);

# class Attribute is Cool {
#     has $!name; # Has to be an bootstrap attribute object for now
#     has $!type;
#     ... # Uncomposed
# }
my stub Attribute metaclass Perl6::Metamodel::ClassHOW { ... };
Attribute.HOW.add_parent(Attribute, Cool);
Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!name>, :type(Mu)));
Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!type>, :type(Mu)));

# class Signature is Cool {
#    has $!params;
#    has $!returns;
#     ... # Uncomposed
# }
my stub Signature metaclass Perl6::Metamodel::ClassHOW { ... };
Signature.HOW.add_parent(Signature, Cool);
Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!params>, :type(Mu)));
Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!returns>, :type(Mu)));

# class Parameter is Cool {
#     has str $!variable_name
#     has $!named_names
#     has $!type_captures
#     has int $!flags
#     has $!nominal_type
#     has $!post_constraints
#     has str $!coerce_to
#     has $!sub_signature
#     has $!default_closure
#     ... # Uncomposed
# }
my stub Parameter metaclass Perl6::Metamodel::ClassHOW { ... };
Parameter.HOW.add_parent(Parameter, Cool);
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!variable_name>, :type(str)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!named_names>, :type(Mu)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!type_captures>, :type(Mu)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!flags>, :type(int)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!nominal_type>, :type(Mu)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!post_constraints>, :type(Mu)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!coerce_to>, :type(str)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!sub_signature>, :type(Mu)));
Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!default_closure>, :type(Mu)));

# class Code is Cool {
#     has $!do;
#     has $!signature;
#     has $!dispatchees;
#     ... # Uncomposed
# }
my stub Code metaclass Perl6::Metamodel::ClassHOW { ... };
Code.HOW.add_parent(Code, Cool);
Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!do>, :type(Mu)));
Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!signature>, :type(Mu)));
Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!dispatchees>, :type(Mu)));

# Need to actually run the code block. Also need this available before we finish
# up the stub.
Code.HOW.add_parrot_vtable_handler_mapping(Code, 'invoke', '$!do');
Code.HOW.publish_parrot_vtable_handler_mapping(Code);

# class Block is Code { ... }
my stub Block metaclass Perl6::Metamodel::ClassHOW { ... };
Block.HOW.add_parent(Block, Code);
Block.HOW.publish_parrot_vtable_handler_mapping(Block);

# class Routine is Block { ... }
my stub Routine metaclass Perl6::Metamodel::ClassHOW { ... };
Routine.HOW.add_parent(Routine, Block);
Routine.HOW.publish_parrot_vtable_handler_mapping(Routine);

# class Sub is Routine { ... }
my stub Sub metaclass Perl6::Metamodel::ClassHOW { ... };
Sub.HOW.add_parent(Sub, Routine);
Sub.HOW.publish_parrot_vtable_handler_mapping(Sub);

# class Method is Routine { ... }
my stub Method metaclass Perl6::Metamodel::ClassHOW { ... };
Method.HOW.add_parent(Method, Routine);
Method.HOW.publish_parrot_vtable_handler_mapping(Method);

# Build up EXPORT::DEFAULT.
my module EXPORT {
    our module DEFAULT {
        $?PACKAGE.WHO<Mu>        := Mu;
        $?PACKAGE.WHO<Any>       := Any;
        $?PACKAGE.WHO<Cool>      := Cool;
        $?PACKAGE.WHO<Attribute> := Attribute;
        $?PACKAGE.WHO<Signature> := Signature;
        $?PACKAGE.WHO<Parameter> := Parameter;
        $?PACKAGE.WHO<Code>      := Code;
        $?PACKAGE.WHO<Block>     := Block;
        $?PACKAGE.WHO<Routine>   := Routine;
        $?PACKAGE.WHO<Sub>       := Sub;
        $?PACKAGE.WHO<Method>    := Method;
    }
}
