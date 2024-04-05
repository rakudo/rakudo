#-------------------------------------------------------------------------------
# Base class for all package related objects, and for the -package- statement
# itself

class RakuAST::Package
  is RakuAST::PackageInstaller
  is RakuAST::StubbyMeta
  is RakuAST::Term
  is RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::Declaration
  is RakuAST::AttachTarget
  is RakuAST::BeginTime
  is RakuAST::TraitTarget
  is RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::LexicalScope
  is RakuAST::Lookup
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Name $.name;
    has RakuAST::Code $.body;
    has Mu            $.attribute-type;
    has Mu            $.how;
    has Str           $.repr;
    has Bool          $.augmented;

    has Mu   $!block-semantics-applied;
    has Bool $.is-stub;

    method new(          str :$scope,
               RakuAST::Name :$name,
          RakuAST::Signature :$parameterization,
                        List :$traits,
               RakuAST::Code :$body,
                          Mu :$attribute-type,
                          Mu :$how,
                         Str :$repr,
                        Bool :$augmented,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Package, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Package, '$!attribute-type',
          nqp::eqaddr($attribute-type, NQPMu) ?? Attribute !! $attribute-type);
        nqp::bindattr($obj, RakuAST::Package, '$!how',
          nqp::eqaddr($how,NQPMu) ?? $obj.default-how !! $how);
        nqp::bindattr($obj, RakuAST::Package, '$!repr', $repr // Str);
        nqp::bindattr($obj, RakuAST::Package, '$!augmented',$augmented // False);

        $obj.set-traits($traits) if $traits;
        $obj.replace-body($body, $parameterization);
        $obj.set-WHY($WHY);

        nqp::bindattr($obj, RakuAST::Package, '$!is-stub', False);

        $obj
    }

    # Informational methods
    method declarator()  { "package"             }
    method dba()         { "package"             }
    method default-how() { Metamodel::PackageHOW }

    method default-scope()       { 'our' }
    method can-have-methods()    { False }
    method can-have-attributes() { False }
    method IMPL-CAN-INTERPRET()  { True }

    method parameterization() { Mu }

    # While a package may be declared `my`, its installation semantics are
    # more complex, and thus handled as a BEGIN-time effect. (For example,
    # `my Foo::Bar { }` should not create a lexical symbol Foo::Bar.)
    method is-simple-lexical-declaration() { False }

    # We install the name before parsing the class body.
    method is-begin-performed-before-children() { True }

    # Setter methods
    method replace-body(RakuAST::Code $body, RakuAST::Signature $signature) {
        nqp::bindattr(self, RakuAST::Package, '$!body',
          $body // RakuAST::Block.new);
        Nil
    }

    method set-repr(Str $repr) {
        nqp::bindattr(self, RakuAST::Package, '$!repr', $repr);
    }

    method set-is-stub(Bool $is-stub) {
        nqp::bindattr(self, RakuAST::Package, '$!is-stub', $is-stub ?? True !! False);
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        if $!augmented {
            my $resolved := $resolver.resolve-name(self.name);
            if $resolved {
                self.set-resolution($resolved);
            }
        }
        elsif $!name {
            my $resolved := $resolver.resolve-name-constant($!name);
            if $resolved {
                my $meta := $resolved.compile-time-value;
                my $how  := $meta.HOW;
                if $how.HOW.name($how) ne 'Perl6::Metamodel::PackageHOW'
                  && nqp::can($how, 'is_composed')
                  && !$how.is_composed($meta) {
                    self.set-resolution($resolved);
                }
            }
        }
        Nil
    }

    method attach-target-names() { self.IMPL-WRAP-LIST(['package', 'also']) }

    method clear-attachments() {
        # Attributes and methods only attach once as a BEGIN effect, thus we
        # don't have to deal with duplicates on them.
        Nil
    }

    method IMPL-GENERATE-LEXICAL-DECLARATION(RakuAST::Name $name, Mu $type-object) {
        RakuAST::Declaration::LexicalPackage.new:
            :lexical-name($name),
            :compile-time-value($type-object),
            :package(self);
    }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        # Note that this early return is actually not effective as the
        # begin handler will already be run when the parser enters the
        # package and we only know that it's a stub when we are done
        # parsing the body.
        return Nil if $!is-stub;

        # Install the symbol.
        my str $scope := self.scope;
        $scope := 'our' if $scope eq 'unit';
        my $name := $!name;
        if $name && !$name.is-empty {
            my $type-object := self.stubbed-meta-object;
            my $current     := $resolver.current-package;
            my $full-name   := nqp::eqaddr($current,$resolver.get-global)
              ?? $name
              !! $name.qualified-with(
                   RakuAST::Name.from-identifier-parts(
                     |nqp::split('::', $current.HOW.name($current))
                    )
                 );
            $type-object.HOW.set_name(
                $type-object,
                $full-name.canonicalize(:colonpairs(0))
            ) if !nqp::eqaddr($current, $resolver.get-global);

            # Update the Stash's name, too.
            nqp::bindattr_s($type-object.WHO, Stash, '$!longname',
              $type-object.HOW.name($type-object));

            self.install-in-scope(
              $resolver, $scope, $name, $full-name, $type-object
            );
        }

        # TODO split off the above into a pre-begin handler, so the enter-scope
        # and declarations can go back into RakuAST::Actions
        if nqp::istype($resolver, RakuAST::Resolver::Compile) {
            $resolver.enter-scope(self);
            self.declare-lexicals($resolver);
        }

        # Apply any traits
        self.apply-traits($resolver, $context, self);
    }

    # Need to install the package somewhere
    method install-in-scope($resolver,$scope,$name,$full-name,$type-object) {
        self.IMPL-INSTALL-PACKAGE(
          $resolver, $scope, $name, $type-object, $resolver.current-package
        ) if $scope eq 'my' || $scope eq 'our';
    }

    # Declare the lexicals for this type of package
    method declare-lexicals(RakuAST::Resolver $resolver) {
        self.meta-object-as-lexicals($resolver, 'CLASS')
          unless self.declarator eq 'package';
    }

    # Helper method to create $?CLASS ::?CLASS and similar
    method meta-object-as-lexicals(RakuAST::Resolver $resolver, str $root) {
        for '$?', '::?' {
            $resolver.declare-lexical(self.implicit-constant($_ ~ $root));
        }
    }

    # Helper method to create $?CLASS ::?CLASS and similar
    method meta-object-as-body-lexicals(str $root) {
        my $body := self.body;
        for '$?', '::?' {
            $body.add-generated-lexical-declaration(
              self.implicit-constant($_ ~ $root)
            )
        }
    }

    # Helper method to define an implicit constant for the meta object
    method implicit-constant(str $name) {
        RakuAST::VarDeclaration::Implicit::Constant.new(
          :$name, :value(self.stubbed-meta-object)
        )
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        if $!augmented || self.is-resolved {
            self.resolution.compile-time-value;
        }
        else {
            # Create the type object and return it; this stubs the type.
            my %options;
            %options<name> := $!name.canonicalize if $!name;
            %options<repr> := $!repr if $!repr;
            if $!name {
                for $!name.colonpairs {
                    %options{$_.key} := $_.simple-compile-time-quote-value;
                }
            }
            $!how.new_type(|%options)
        }
    }

    method PRODUCE-META-OBJECT() {
        my $type := self.stubbed-meta-object;
        $type.HOW.compose($type);
        $type
    }

    method apply-implicit-block-semantics() {
        unless $!block-semantics-applied {
            self.meta-object-as-body-lexicals('PACKAGE');
            self.additional-body-lexicals;

            nqp::bindattr(self,RakuAST::Package,'$!block-semantics-applied',1);
        }
    }

    # Add any additional lexicals to the body
    method additional-body-lexicals() {
        self.meta-object-as-body-lexicals('CLASS')
          unless self.declarator eq 'package';
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $type-object := self.meta-object;
        $context.ensure-sc($type-object);
        my $body := $!body.IMPL-QAST-BLOCK($context, :blocktype<immediate>);
        my $result := QAST::Stmts.new(
            $body,
            QAST::WVal.new( :value($type-object) )
        );
        $result
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.compile-time-value
    }

    method IMPL-COMPOSE() {
        self.meta-object; # Ensure it's composed
    }

    method visit-children(Code $visitor) {
        $visitor($!name) if $!name;
        self.visit-traits($visitor);
        $visitor($!body);
        $visitor(self.WHY) if self.WHY;
    }

    method needs-sink-call() { False }
}

#-------------------------------------------------------------------------------
# Role for handling package types that can have methods and attributes
# attached to it

class RakuAST::Package::Attachable
  is RakuAST::Package
{
    # Methods and attributes are not directly added, but rather through the
    # RakuAST::Attaching mechanism. Attribute usages are also attached for
    # checking after compose time.
    has Mu $!attached-methods;
    has Mu $!attached-attributes;
    has Mu $!attached-attribute-usages;
    has Mu $!role-group;

    method new(          str :$scope,
               RakuAST::Name :$name,
          RakuAST::Signature :$parameterization,
                        List :$traits,
               RakuAST::Code :$body,
                          Mu :$attribute-type,
                          Mu :$how,
                         Str :$repr,
                        Bool :$augmented,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Package, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Package, '$!attribute-type',
          nqp::eqaddr($attribute-type, NQPMu) ?? Attribute !! $attribute-type);
        nqp::bindattr($obj, RakuAST::Package, '$!how',
          nqp::eqaddr($how,NQPMu) ?? $obj.default-how !! $how);
        nqp::bindattr($obj, RakuAST::Package, '$!repr', $repr // Str);
        nqp::bindattr($obj, RakuAST::Package, '$!augmented',$augmented // False);

        $obj.set-traits($traits) if $traits;
        $obj.replace-body($body, $parameterization);
        $obj.set-WHY($WHY);

        nqp::bindattr($obj, RakuAST::Package, '$!is-stub', False);

        # Set up internal defaults
        nqp::bindattr($obj, RakuAST::Package::Attachable,
          '$!attached-methods', []);
        nqp::bindattr($obj, RakuAST::Package::Attachable,
          '$!attached-attributes', []);
        nqp::bindattr($obj, RakuAST::Package::Attachable,
          '$!attached-attribute-usages', {});
        nqp::bindattr($obj, RakuAST::Package::Attachable,
          '$!role-group', Mu);

        $obj
    }

    method can-have-methods()    { True }
    method can-have-attributes() { True }

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
        nqp::bindkey($!attached-attribute-usages, $attribute.name, $attribute);
        Nil
    }

    # Add methods and attributes to meta object
    method PRODUCE-META-ATTACHABLES($type, $how) {
        for $!attached-methods {
            my $name        := $_.name.canonicalize;
            my $meta-object := $_.meta-object;

            if nqp::istype($_, RakuAST::Method) && $_.private {
                $how.add_private_method($type, $name, $meta-object);
            }
            elsif nqp::istype($_, RakuAST::Method) && $_.meta {
                $how.add_meta_method($type, $name, $meta-object);
            }
            elsif $_.multiness eq 'multi' {
                $how.add_multi_method($type, $name, $meta-object);
            }
            else {
                $how.add_method($type, $name, $meta-object);
            }
        }
        for $!attached-attributes {

            # attribute defined means we don't need to check it anymore
            nqp::deletekey($!attached-attribute-usages, $_.name);

            # TODO: create method BUILDALL here
            $how.add_attribute($type, $_.meta-object);
        }
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle roles

class RakuAST::Role
  is RakuAST::Package::Attachable
{
    method declarator()  { "role"                       }
    method default-how() { Metamodel::ParametricRoleHOW }

    method replace-body(RakuAST::Code $body, RakuAST::Signature $signature) {
        # The body of a role is internally a Sub that has the parameterization
        # of the role as the signature.  This allows a role to be selected
        # using ordinary dispatch semantics.  The statement list gets a return
        # value added, so that the role's meta-object and lexpad are returned.
        $signature := RakuAST::Signature.new unless $signature;
        $signature.set-is-on-role-body(1);

        $body := RakuAST::Block.new unless $body;
        my $orig-body := $body;
        $body := $body.body;
        $body.statement-list.add-statement(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Nqp.new('list',
              RakuAST::Declaration::ResolvedConstant.new(
                compile-time-value => self.stubbed-meta-object
              ),
              RakuAST::Nqp.new('curlexpad')
            )
          )
        );
        $body := RakuAST::Sub.new(:name(self.name), :$signature, :$body);
        $body.IMPL-TRANSFER-DECLARATIONS($orig-body);

        nqp::bindattr(self, RakuAST::Package, '$!body', $body);
        Nil
    }

    method parameterization() { self.body.signature }

    method declare-lexicals(RakuAST::Resolver $resolver) {
        self.meta-object-as-lexicals($resolver, 'ROLE');

        for '$?CLASS', '::?CLASS' {
            $resolver.declare-lexical(
              RakuAST::Type::Capture.new(RakuAST::Name.from-identifier($_))
            );
        }
    }

    method install-in-scope($resolver,$scope,$name,$full-name,$type-object) {
        # Find an appropriate existing role group
        my $group-name := $full-name.canonicalize(:colonpairs(0));
        my $group      := $resolver.resolve-lexical-constant($group-name);
        if $group {
            $group := $group.compile-time-value;
        }

        # No existing one found - create a role group
        else {
            $group := Perl6::Metamodel::ParametricRoleGroupHOW.new_type(
              :name($group-name), :repr(self.repr)
            );
            my $outer := $resolver.find-attach-target('block')
              // $resolver.find-attach-target('compunit');
            $outer.add-generated-lexical-declaration(
              RakuAST::VarDeclaration::Implicit::Constant.new(
                :name($name.canonicalize(:colonpairs(0))),
                :value($group)
              )
            );

            self.IMPL-INSTALL-PACKAGE(
              $resolver, $scope, $name, $group, $resolver.current-package,
              :no-lexical
            ) if $scope eq 'our';
        }
        # Add ourselves to the role group
        $type-object.HOW.set_group($type-object, $group);
        nqp::bindattr(self,RakuAST::Package::Attachable,'$!role-group',$group);
    }

    method additional-body-lexicals() {
        self.meta-object-as-body-lexicals('ROLE');
    }

    method PRODUCE-META-OBJECT() {
        # Obtain the stubbed meta-object, which is the type object.
        my $type := self.stubbed-meta-object;
        my $how  := $type.HOW;

        self.PRODUCE-META-ATTACHABLES($type, $how);

        $how.set_body_block($type, self.body.meta-object);

        # The role needs to be composed before we add the possibility
        # to the group
        $how.compose($type);

        my $group :=
          nqp::getattr(self, RakuAST::Package::Attachable, '$!role-group');
        $group.HOW.add_possibility($group, $type) unless $group =:= Mu;

        $type
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle classes and grammars

class RakuAST::Class
  is RakuAST::Package::Attachable
{
    method declarator()  { "class"             }
    method default-how() { Metamodel::ClassHOW }

    method IMPL-COMPOSE() {
        # create BUILDALL method if there's something to create,
        # otherwise put in a generic fallback BUILDALL that doesn't
        # do anything
        self.meta-object; # Ensure it's composed
    }

    method PRODUCE-META-OBJECT() {
        # Obtain the stubbed meta-object, which is the type object.
        my $type := self.stubbed-meta-object();
        my $how  := $type.HOW;

        self.PRODUCE-META-ATTACHABLES($type, $how);
        $how.compose($type);
        $type
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle grammars

class RakuAST::Grammar
  is RakuAST::Class
{
    method declarator()  { "grammar"             }
    method default-how() { Metamodel::GrammarHOW }

    method IMPL-COMPOSE() {
        self.meta-object; # Ensure it's composed
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle modules

class RakuAST::Module
  is RakuAST::Package
{
    method declarator()  { "module"           }
    method default-how() { Metamodel::KnowHOW }

    method declare-lexicals(RakuAST::Resolver $resolver) {
        self.meta-object-as-lexicals($resolver, 'MODULE');
    }

    method additional-body-lexicals() {
        self.meta-object-as-body-lexicals('MODULE');
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle -knowhow- blocks

class RakuAST::Knowhow
  is RakuAST::Package
{
    method declarator()  { "knowhow"          }
    method default-how() { Metamodel::KnowHOW }
}

#-------------------------------------------------------------------------------
# Specific logic to handle -native- blocks

class RakuAST::Native
  is RakuAST::Package
{
    method declarator()  { "native"             }
    method default-how() { Metamodel::NativeHOW }
}
