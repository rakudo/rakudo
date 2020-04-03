class RakuAST::Declaration::Var is RakuAST::Declaration::Lexical
        is RakuAST::ImplicitLookups is RakuAST::Meta {
    has RakuAST::Type $.type;
    has str $.name;

    method new(str :$name!, RakuAST::Type :$type) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::Var, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Declaration::Var, '$!type', $type // RakuAST::Type);
        $obj
    }

    method lexical-name() {
        $!name
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups := [
            RakuAST::Type::Simple.new('ContainerDescriptor'),
        ];
        # TODO need to decide by sigil here
        @lookups.push(RakuAST::Type::Simple.new('Scalar'));
        if $!type {
            @lookups.push($!type); # Constraint
            @lookups.push($!type); # Default
        }
        else {
            @lookups.push(RakuAST::Type::Simple.new('Mu'));
            @lookups.push(RakuAST::Type::Simple.new('Any'));
        }
        my $list := nqp::create(List);
        nqp::bindattr($list, List, '$!reified', @lookups);
        $list
    }

    method PRODUCE-META-OBJECT() {
        # Form container descriptor.
        my @lookups := nqp::getattr(self.get-implicit-lookups(), List, '$!reified');
        my $cont-desc-type := @lookups[0].resolution.compile-time-value;
        my $of := @lookups[2].resolution.compile-time-value;
        my $default := @lookups[3].resolution.compile-time-value;
        my $cont-desc := $cont-desc-type.new(:$of, :$default, :dynamic(0),
            :name($!name));

        # Form the container.
        my $container-type := @lookups[1].resolution.compile-time-value;
        my $container := nqp::create($container-type);
        nqp::bindattr($container, $container-type, '$!descriptor', $cont-desc);

        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new(
            :scope('lexical'), :decl('contvar'), :name($!name),
            :value($container)
        )
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Compile into an access to the variable itself.
        my str $name := $!name;
        QAST::Var.new( :$name, :scope<lexical> )
    }
}
