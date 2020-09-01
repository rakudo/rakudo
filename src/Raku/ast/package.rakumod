class RakuAST::Package is RakuAST::StubbyMeta is RakuAST::Term
                       is RakuAST::IMPL::ImmediateBlockUser
                       is RakuAST::Declaration is RakuAST::AttachTarget
                       is RakuAST::BeginTime {
    has Str $.package-declarator;
    has Mu $.how;
    has Mu $.attribute-type;
    has RakuAST::Name $.name;
    has Str $.repr;
    has RakuAST::Block $.body;

    # Methods and attributes are not directly added, but rather thorugh the
    # RakuAST::Attaching mechanism. Attribute usages are also attached for
    # checking after compose time.
    has Mu $!attached-methods;
    has Mu $!attached-attributes;
    has Mu $!attached-attribute-usages;

    method new(Str :$package-declarator!, Mu :$how!, Mu :$attribute-type,
               RakuAST::Name :$name, Str :$repr, RakuAST::Block :$body, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Package, '$!package-declarator', $package-declarator);
        nqp::bindattr($obj, RakuAST::Package, '$!how', $how);
        nqp::bindattr($obj, RakuAST::Package, '$!attribute-type',
            nqp::eqaddr($attribute-type, NQPMu) ?? Attribute !! $attribute-type);
        nqp::bindattr($obj, RakuAST::Package, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Package, '$!repr', $repr // Str);
        nqp::bindattr($obj, RakuAST::Package, '$!body', $body // RakuAST::Block.new);
        nqp::bindattr($obj, RakuAST::Package, '$!attached-methods', []);
        nqp::bindattr($obj, RakuAST::Package, '$!attached-attributes', []);
        nqp::bindattr($obj, RakuAST::Package, '$!attached-attribute-usages', []);
        $obj
    }

    method replace-body(RakuAST::Block $new-body) {
        nqp::bindattr(self, RakuAST::Package, '$!body', $new-body);
        Nil
    }

    method default-scope() { 'our' }

    # While a package may be declared `my`, its installation semantics are
    # more complex, and thus handled as a BEGIN-time effect. (For example,
    # `my Foo::Bar { }` should not create a lexical symbol Foo::Bar.)
    method is-simple-lexical-declaration() {
        False
    }

    method attach-target-names() { self.IMPL-WRAP-LIST(['package', 'also']) }

    method clear-attachments() {
        nqp::setelems($!attached-methods, 0);
        nqp::setelems($!attached-attributes, 0);
        Nil
    }

    method ATTACH-METHOD(RakuAST::Method $method) {
        nqp::push($!attached-methods, $method);
        Nil
    }

    # TODO also list-y declarations
    method ATTACH-ATTRIBUTE(RakuAST::VarDeclaration::Simple $attribute) {
        nqp::push($!attached-attributes, $attribute);
        Nil
    }

    method ATTACH-ATTRIBUTE-USAGE(RakuAST::Var::Attribute $attribute) {
        nqp::push($!attached-attribute-usages, $attribute);
        Nil
    }

    # We install the name before parsing the class body.
    method is-begin-performed-before-children() { True }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver) {
        my str $scope := self.scope;
        my $name := $!name;
        if $name && !$name.is-empty && ($scope eq 'my' || $scope eq 'our') {
            # Need to install the package somewhere.
            my $type-object := self.stubbed-meta-object;
            if $name.is-identifier {
                # We always install it as a lexical symbol.
                $resolver.current-scope.add-generated-lexical-declaration:
                    RakuAST::Declaration::LexicalPackage.new:
                        :lexical-name($name.canonicalize),
                        :compile-time-value($type-object);

                # If `our`-scoped, also put it into the current package.
                if $scope eq 'our' {
                    # TODO conflicts, claiming of packages
                    my %stash := $resolver.IMPL-STASH-HASH($resolver.current-package);
                    %stash{$name.canonicalize} := $type-object;
                }
            }
            else {
                nqp::die('multi-part package declarations NYI');
            }
        }
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        # Create the type object and return it; this stubs the type.
        my %options;
        %options<name> := $!name.canonicalize if $!name;
        %options<repr> := $!repr if $!repr;
        $!how.new_type(|%options)
    }

    method PRODUCE-META-OBJECT() {
        # Obtain the stubbed meta-object, which is the type object.
        my $type := self.stubbed-meta-object();

        # Add methods and attributes.
        for $!attached-methods {
            $type.HOW.add_method($type, $_.name.canonicalize, $_.meta-object);
        }
        for $!attached-attributes {
            $type.HOW.add_attribute($type, $_.meta-object);
        }

        # Compose the meta-object and return it.
        $type.HOW.compose($type);
        $type
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $type-object := self.meta-object;
        $context.ensure-sc($type-object);
        QAST::Stmts.new(
            $!body.IMPL-TO-QAST($context, :immediate),
            QAST::WVal.new( :value($type-object) )
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!body);
    }
}
