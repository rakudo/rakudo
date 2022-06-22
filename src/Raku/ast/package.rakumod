class RakuAST::Package is RakuAST::StubbyMeta is RakuAST::Term
                       is RakuAST::IMPL::ImmediateBlockUser
                       is RakuAST::Declaration is RakuAST::AttachTarget
                       is RakuAST::BeginTime is RakuAST::TraitTarget
                       is RakuAST::ImplicitBlockSemanticsProvider {
    has Str $.package-declarator;
    has Mu $.how;
    has Mu $.attribute-type;
    has RakuAST::Name $.name;
    has Str $.repr;
    has RakuAST::Block $.body;
    has Mu $!role-group;

    # Methods and attributes are not directly added, but rather thorugh the
    # RakuAST::Attaching mechanism. Attribute usages are also attached for
    # checking after compose time.
    has Mu $!attached-methods;
    has Mu $!attached-attributes;
    has Mu $!attached-attribute-usages;

    method new(Str :$package-declarator!, Mu :$how!, Mu :$attribute-type,
               RakuAST::Name :$name, Str :$repr, RakuAST::Block :$body,
               str :$scope, List :$traits) {
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
        nqp::bindattr($obj, RakuAST::Package, '$!role-group', Mu);
        $obj.set-traits($traits);
        $obj
    }

    method replace-body(RakuAST::Block $new-body) {
        nqp::bindattr(self, RakuAST::Package, '$!body', $new-body);
        Nil
    }

    method set-repr(Str $repr) {
        nqp::bindattr(self, RakuAST::Package, '$!repr', $repr);
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

    method IMPL-INSTALL-PACKAGE(RakuAST::Resolver $resolver, str $scope, RakuAST::Name $name, Mu $type-object) {
        my $target;
        my $final;
        my $lexical;
        if $name.is-identifier {
            $final := $name.canonicalize;
            $lexical := $resolver.resolve-lexical-constant($final);
            # We always install it as a lexical symbol.
            $resolver.current-scope.merge-generated-lexical-declaration:
                RakuAST::Declaration::LexicalPackage.new:
                    :lexical-name($name.canonicalize),
                    :compile-time-value($type-object);

            # If `our`-scoped, also put it into the current package.
            if $scope eq 'our' {
                # TODO conflicts
                $target := $resolver.current-package;
            }
        }
        else {
            my $resolved := $resolver.partially-resolve-name-constant($name);

            if $resolved { # first parts of the name found
                $resolved := self.IMPL-UNWRAP-LIST($resolved);
                $target := $resolved[0];
                my $parts  := $resolved[1];
                my @parts := self.IMPL-UNWRAP-LIST($parts);
                $final := nqp::pop(@parts).name;
                my $longname := $target.HOW.name($target);

                for @parts {
                    $longname := $longname ~ '::' ~ $_.name;
                    my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                    $package.HOW.compose($package);
                    my %stash := $resolver.IMPL-STASH-HASH($target);
                    %stash{$longname} := $package;
                    $target := $package;
                }
            }
            else {
                my @parts := nqp::clone(self.IMPL-UNWRAP-LIST($name.parts));
                my $first := nqp::shift(@parts).name;
                $target := Perl6::Metamodel::PackageHOW.new_type(name => $first);
                $target.HOW.compose($target);

                if $scope eq 'our' {
                    # TODO conflicts
                    my %stash := $resolver.IMPL-STASH-HASH($resolver.current-package);
                    %stash{$first} := $target;
                }

                $final := nqp::pop(@parts).name;
                my $longname := $first;
                for @parts {
                    $longname := $longname ~ '::' ~ $_.name;
                    my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                    $package.HOW.compose($package);
                    my %stash := $resolver.IMPL-STASH-HASH($target);
                    %stash{$longname} := $package;
                    $target := $package;
                }
            }
            $lexical := $resolver.resolve-lexical-constant($final);
        }

        my %stash := $resolver.IMPL-STASH-HASH($target);
        # upgrade a lexically imported package stub to package scope if it exists
        if $lexical {
            %stash{$final} := $lexical.compile-time-value;
        }
        if nqp::existskey(%stash, $final) {
            nqp::setwho($type-object, %stash{$final}.WHO);
        }
        %stash{$final} := $type-object;
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Install the symbol.
        my str $scope := self.scope;
        $scope := 'our' if $scope eq 'unit';
        my $name := $!name;
        if $name && !$name.is-empty {
            my $type-object := self.stubbed-meta-object;
            my $current-package := $resolver.find-attach-target('package');
            $type-object.HOW.set_name($type-object, $name.qualified-with($current-package.name).canonicalize(:colonpairs(0)))
                if $current-package;

            if ($scope eq 'my' || $scope eq 'our') && $!package-declarator ne 'role' {
                # Need to install the package somewhere.
                self.IMPL-INSTALL-PACKAGE($resolver, $scope, $name, $type-object);
            }

            elsif $!package-declarator eq 'role' {
                # Find an appropriate existing role group
                my $group-name := $name.canonicalize(:colonpairs(0));
                my $group := $resolver.resolve-lexical-constant($group-name);
                if $group {
                    $group := $group.compile-time-value;
                }
                else {
                    # No existing one found - create a role group
                    $group := Perl6::Metamodel::ParametricRoleGroupHOW.new_type(
                        :name($group-name),
                        :repr($!repr)
                    );
                    my $outer := $resolver.find-attach-target('block') // $resolver.find-attach-target('compunit');
                    $outer.add-generated-lexical-declaration(
                        RakuAST::VarDeclaration::Implicit::Constant.new(
                            :name($group-name),
                            :value($group)
                        )
                    );
                    if $scope eq 'our' {
                        self.IMPL-INSTALL-PACKAGE($resolver, $scope, $name, $group);
                    }
                }
                # Add ourselves to the role group
                $type-object.HOW.set_group($type-object, $group);
                nqp::bindattr(self, RakuAST::Package, '$!role-group', $group);
            }
        }

        if $!package-declarator eq 'role' {
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?ROLE', value => self.stubbed-meta-object
                )
            );
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?ROLE', value => self.stubbed-meta-object
                )
            );
        }
        elsif $!package-declarator eq 'module' {
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?MODULE', value => self.stubbed-meta-object
                )
            );
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?MODULE', value => self.stubbed-meta-object
                )
            );
        }
        elsif $!package-declarator ne 'package' {
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?CLASS', value => self.stubbed-meta-object
                )
            );
            $resolver.declare-lexical(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?CLASS', value => self.stubbed-meta-object
                )
            );
        }

        # Apply traits.
        self.apply-traits($resolver, $context, self);
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
            my $name := $_.name.canonicalize;
            my $meta-object := $_.meta-object;
            if nqp::istype($_, RakuAST::Method) && $_.private {
                $type.HOW.add_private_method($type, $name, $meta-object);
            }
            elsif nqp::istype($_, RakuAST::Method) && $_.meta {
                $type.HOW.add_meta_method($type, $name, $meta-object);
            }
            elsif $_.multiness eq 'multi' {
                $type.HOW.add_multi_method($type, $name, $meta-object);
            }
            else {
                $type.HOW.add_method($type, $name, $meta-object);
            }
        }
        for $!attached-attributes {
            $type.HOW.add_attribute($type, $_.meta-object);
        }

        if $!package-declarator eq 'role' {
            $type.HOW.set_body_block($type, $!body.meta-object);

            my $group := $!role-group;
            $group.HOW.add_possibility($group, $type) unless $group =:= Mu;
        }

        # Compose the meta-object and return it.
        $type.HOW.compose($type);
        $type
    }

    method apply-implicit-block-semantics() {
        $!body.add-generated-lexical-declaration(
            RakuAST::VarDeclaration::Implicit::Constant.new(
                name => '$?PACKAGE', value => self.stubbed-meta-object
            )
        );
        if $!package-declarator eq 'role' {
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?ROLE', value => self.stubbed-meta-object
                )
            );
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?ROLE', value => self.stubbed-meta-object
                )
            );
        }
        elsif $!package-declarator eq 'module' {
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?MODULE', value => self.stubbed-meta-object
                )
            );
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?MODULE', value => self.stubbed-meta-object
                )
            );
        }
        elsif $!package-declarator ne 'package' {
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '$?CLASS', value => self.stubbed-meta-object
                )
            );
            $!body.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    name => '::?CLASS', value => self.stubbed-meta-object
                )
            );
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $type-object := self.meta-object;
        $context.ensure-sc($type-object);
        my $body := $!body.IMPL-QAST-BLOCK($context, :blocktype<immediate>);
        my $result := QAST::Stmts.new(
            $body,
            QAST::WVal.new( :value($type-object) )
        );
        if $!package-declarator eq 'role' {
            $body.push(
                QAST::Op.new(:op<list>, QAST::WVal.new(:value($type-object)), QAST::Op.new(:op<curlexpad>))
            );
        }
        $result
    }

    method IMPL-CAN-INTERPRET() {
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.compile-time-value
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        self.visit-traits($visitor);
        $visitor($!body);
    }

    method needs-sink-call() { False }
}
