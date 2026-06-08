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
  is RakuAST::ParseTime
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
    has str           $!parsed-declarator;

    has Mu   $!block-semantics-applied;
    has Bool $.is-stub;
    has Bool $!stub-defused;
    has Bool $.is-require-stub;
    has Bool $!installed;

    has Mu $!compose-exception;

    # Enclosing parametric role captured at BEGIN-time so the package can
    # register itself as an instantiation lexical on that role if it ends up
    # archetypally generic after composition.
    has Mu $!generics-pad;

    method new(          str :$scope,
               RakuAST::Name :$name,
          RakuAST::Signature :$parameterization,
                        List :$traits,
               RakuAST::Code :$body,
                          Mu :$attribute-type,
                          Mu :$how,
                         Str :$repr,
                        Bool :$augmented,
                        Bool :$is-require-stub,
                         str :$parsed-declarator,
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
        nqp::bindattr($obj, RakuAST::Package, '$!is-require-stub',$is-require-stub // False);
        nqp::bindattr_s($obj, RakuAST::Package, '$!parsed-declarator', $parsed-declarator);

        $obj.set-traits($traits) if $traits;
        $obj.replace-body($body, $parameterization);
        $obj.set-WHY($WHY);

        nqp::bindattr($obj, RakuAST::Package, '$!is-stub', False);
        nqp::bindattr($obj, RakuAST::Package, '$!stub-defused', False);

        $obj
    }

    # The literal declarator string the parser saw, e.g. `monitor` for a
    # custom `EXPORTHOW::DECLARE` keyword. Diagnostics that want to echo
    # what the user typed prefer this over `declarator`, which returns
    # the AST class's static name.
    method parsed-declarator() { $!parsed-declarator || self.declarator }

    # Informational methods
    method declarator()  { "package"             }
    method dba()         { "package"             }
    method default-how() { Metamodel::PackageHOW }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['anon', 'augment', 'my', 'our', 'unit']) }
    method default-scope()       { 'our' }
    method can-have-methods()    { False }
    method can-have-attributes() { False }
    method IMPL-CAN-INTERPRET()  { True }

    method parameterization() { Mu }

    method creates-block() { False }

    # While a package may be declared `my`, its installation semantics are
    # more complex, and thus handled as a BEGIN-time effect. (For example,
    # `my Foo::Bar { }` should not create a lexical symbol Foo::Bar.)
    method is-simple-lexical-declaration() { False }

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

    method defuse-stub() {
        nqp::bindattr(self, RakuAST::Package, '$!stub-defused', True);
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $!augmented {
            if self.name {
                my $resolved := $resolver.resolve-name(self.name);
                if $resolved {
                    unless $resolved.compile-time-value.HOW.archetypes.augmentable {
                        self.add-sorry:
                            $resolver.build-exception: 'X::Syntax::Augment::Illegal',
                                :package(self.name.canonicalize);
                        $resolver.add-node-with-check-time-problems(self);
                    }
                    self.set-resolution($resolved);
                }
                else {
                    self.add-sorry:
                        $resolver.build-exception: 'X::Augment::NoSuchType',
                            :package(self.name.canonicalize), :package-kind(self.parsed-declarator);
                    $resolver.add-node-with-check-time-problems(self);
                }
            }
            else {
                self.add-sorry:
                    $resolver.build-exception: 'X::Anon::Augment',
                        :package-kind(self.parsed-declarator);
                $resolver.add-node-with-check-time-problems(self);
            }
        }
        elsif $!name {
            my $full-name := self.IMPL-FULL-NAME($resolver);
            # First try to find it using the fully qualified name
            my $resolved := $resolver.resolve-name-constant($full-name, :current-scope-only(self.scope eq 'my'));
            # If not found try locally using just the declared name
            $resolved := $resolver.resolve-name-constant($!name, :current-scope-only)
                unless nqp::isconcrete($resolved);
            if $resolved {
                my $meta := $resolved.compile-time-value;
                my $how  := $meta.HOW;
                if $how.HOW.name($how) ne 'Perl6::Metamodel::PackageHOW' {
                    self.defuse-stub;
                    $resolved.package.defuse-stub if nqp::istype($resolved, RakuAST::Declaration::LexicalPackage);
                    # Note: this won't find role groups as they are not Composing. That's ok as
                    # we do not need to re-use the stub's meta object for roles.
                    if nqp::can($how, 'is_composed') && !$how.is_composed($meta) {
                        self.set-resolution($resolved);
                    }
                }
            }
        }

    }

    method attach-target-names() { ['package', 'also'] }

    method IMPL-GENERATE-LEXICAL-DECLARATION(RakuAST::Name $name, Mu $type-object) {
        $type-object := self.stubbed-meta-object if nqp::eqaddr($type-object, Mu);
        my $package := RakuAST::Declaration::LexicalPackage.new:
            :lexical-name($name),
            :compile-time-value($type-object),
            :package(self);
        $package
    }

    method IMPL-FULL-NAME($resolver) {
        my $name := $!name;
        my $current := $resolver.current-package;
        my $full-name := nqp::eqaddr($current,$resolver.get-global)
            ?? $name.is-global-lookup ?? $name.without-first-part !! $name
            !! $name.qualified-with(
                RakuAST::Name.from-identifier-parts(
                    |nqp::split('::', $current.HOW.name($current))
                )
            );
    }

    method ensure-installed(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless $!installed {
            nqp::bindattr(self, RakuAST::Package, '$!installed', True);

            # Install the symbol.
            my str $scope := self.scope;
            $scope := 'our' if $scope eq 'unit';
            my $name := $!name;
            if $name && $name.is-installable {
                my $type-object := self.stubbed-meta-object(:$resolver, :$context);
                my $full-name := self.IMPL-FULL-NAME($resolver);
                $type-object.HOW.set_name(
                    $type-object,
                    $full-name.canonicalize(:colonpairs(0))
                );

                # Update the Stash's name, too.
                nqp::bindattr_s($type-object.WHO, Stash, '$!longname',
                  $type-object.HOW.name($type-object));

                self.install-in-scope($resolver, $scope, $name, $full-name);
            }

            self.install-extra-declarations($resolver);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $!body.to-begin-time($resolver, $context); # In case it's the default generated by replace-body

        self.ensure-installed($resolver, $context);

        # Apply any traits
        self.apply-traits($resolver, $context, self);

        # Remember the enclosing parametric role, if any, so that we can
        # register an instantiation lexical with it once the type is composed.
        # We cannot add to the role's body directly here: for the outer role,
        # $ast.replace-body(...) in Raku/Actions.nqp has not yet run, so
        # self.body still refers to the stub body that will be discarded. The
        # Role collects the request and applies it during additional-body-lexicals
        # (which runs after the body replacement).
        unless nqp::istype(self, RakuAST::Role) {
            my $pad := $resolver.find-attach-target('generics-pad');
            nqp::bindattr(self, RakuAST::Package, '$!generics-pad', $pad) if $pad;
        }
    }

    # Shared archetype filter used by both the declaration side
    # (IMPL-MAYBE-REGISTER-INSTANTIATION-LEXICAL below) and the reference
    # side (RakuAST::Type::Simple.IMPL-EXPR-QAST). A type is eligible for
    # `!INS_OF_<fullname>` registration exactly when its archetypes are
    # generic, nominal, and non-parametric. Mirrors the filter traditional
    # grammar uses in src/Perl6/Actions.nqp package_def.
    method IMPL-IS-INSTANTIATION-REGISTRABLE(Mu $type) {
        my $how := $type.HOW;
        return 0 unless nqp::can($how, 'archetypes');
        my $archetypes := $how.archetypes($type);
        $archetypes.generic && $archetypes.nominal
          && !(nqp::can($archetypes, 'parametric') && $archetypes.parametric)
            ?? 1
            !! 0
    }

    # After the package has been composed, if the type ended up archetypally
    # generic/nominal/non-parametric, declare a `!INS_OF_<fullname>` lexical
    # on the enclosing role and register it on the role's
    # instantiation-lexicals list so resolve_instantiations rebinds it per
    # specialization. Called from the base IMPL-COMPOSE, so any
    # RakuAST::Package subclass gets the behavior without its
    # IMPL-COMPOSE override needing to remember.
    method IMPL-MAYBE-REGISTER-INSTANTIATION-LEXICAL() {
        return Nil unless $!generics-pad;
        my $type := self.stubbed-meta-object;
        return Nil unless RakuAST::Package.IMPL-IS-INSTANTIATION-REGISTRABLE($type);
        my str $ins-name := '!INS_OF_' ~ $type.HOW.name($type);
        my $decl := RakuAST::VarDeclaration::Implicit::Constant.new(
            :name($ins-name), :value($type), :scope('my')
        );
        $!generics-pad.IMPL-QUEUE-INSTANTIATION-LEXICAL($decl);
        Nil
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $name := $!name;
        if $name && !$name.is-empty && $!name.has-colonpairs {
            my $colonpairs := $!name.IMPL-UNWRAP-LIST($!name.colonpairs);
            for $colonpairs {
                my $key := $_.key;
                if $key ne 'ver' && $key ne 'api' && $key ne 'auth' {
                    self.add-sorry:
                        $resolver.build-exception: 'X::Syntax::' ~ ($!augmented ?? 'Augment' !! 'Type') ~ '::Adverb',
                            adverb => $key
                }
                elsif $!augmented && $key eq 'auth' {
                    self.add-sorry:
                        $resolver.build-exception: 'X::Syntax::Augment::Adverb',
                            adverb => $key
                }
            }
        }

        if $!compose-exception {
            self.add-sorry: $resolver.convert-exception($!compose-exception)
        }

        self.check-scope($resolver, self.declarator);

        self.add-trait-sorries;

        if $!is-stub && !$!stub-defused && !$!is-require-stub
            && !self.stubbed-meta-object.HOW.is_composed(self.stubbed-meta-object)
            && !nqp::istype(self, RakuAST::Role) # No idea why roles are excempt
        { # Should be replaced by now
            self.add-sorry:
                $resolver.build-exception: 'X::Package::Stubbed',
                    packages => [$!name.canonicalize];
        }

        if self.is-resolved && $!repr {
            self.add-sorry: $resolver.build-exception: 'X::TooLateForREPR', type => self.stubbed-meta-object;
        }

        nqp::findmethod(RakuAST::LexicalScope, 'PERFORM-CHECK')(self, $resolver, $context);
    }

    method install-extra-declarations(RakuAST::Resolver $resolver) {
        Nil
    }

    # Need to install the package somewhere
    method install-in-scope(RakuAST::Resolver $resolver, str $scope, RakuAST::Name $name, RakuAST::Name $full-name) {
        self.IMPL-INSTALL-PACKAGE(
          $resolver, $scope, $name, $resolver.current-package, :meta-object(Mu)
        ) if $scope eq 'my' || $scope eq 'our';
    }

    # Declare the lexicals for this type of package
    method declare-lexicals(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.meta-object-as-lexicals($resolver, 'PACKAGE', :$context);
        self.meta-object-as-lexicals($resolver, 'CLASS', :$context)
          unless self.declarator eq 'package';
    }

    # Helper method to create $?CLASS ::?CLASS and similar
    method meta-object-as-lexicals(RakuAST::Resolver $resolver, str $root, :$context) {
        for '$?', '::?' {
            $resolver.declare-lexical(self.implicit-constant($_ ~ $root, :$resolver, :$context));
        }
    }

    # Helper method to create $?CLASS ::?CLASS and similar
    method meta-object-as-body-lexicals(str $root, :$resolver, :$context) {
        my $body := self.body;
        for '$?', '::?' {
            $body.add-generated-lexical-declaration(
              self.implicit-constant($_ ~ $root, :$resolver, :$context)
            )
        }
    }

    # Helper method to define an implicit constant for the meta object
    method implicit-constant(str $name, :$resolver, :$context) {
        RakuAST::VarDeclaration::Implicit::Constant.new(
          :$name, :value(self.stubbed-meta-object(:$resolver, :$context))
        )
    }

    method PRODUCE-STUBBED-META-OBJECT(:$resolver, :$context) {
        if self.is-resolved {
            self.resolution.compile-time-value;
        }
        elsif $!augmented && nqp::istype(self, RakuAST::Role) {
            Nil # Will report the error a little later
        }
        else {
            # Create the type object and return it; this stubs the type.
            # Colonpair values evaluate against $resolver/$context when
            # present, so callers in the compile pipeline must pass both.
            # Packages without colonpairs work fine without context.
            my %options;
            %options<name> := $!name.canonicalize if $!name && $!name.is-installable;
            %options<repr> := $!repr if $!repr;
            if $!name {
                my @colonpairs := $!name.IMPL-UNWRAP-LIST($!name.colonpairs);
                if nqp::elems(@colonpairs) {
                    nqp::die("RakuAST::Package.stubbed-meta-object: package with colonpairs `"
                      ~ $!name.canonicalize
                      ~ "' requires resolver and context, but caller did not pass them")
                        unless nqp::isconcrete($resolver) && nqp::isconcrete($context);
                    my $Failure := $resolver.type-from-setting('Failure');
                    for @colonpairs {
                        my $key := $_.key;
                        my $value := $_.IMPL-EVAL-COLONPAIR-VALUE-OR-RETHROW(
                            $resolver, $context, $Failure);
                        next if $key eq 'auth' && nqp::eqaddr($value, Nil);
                        $value := Version.new($value) if $key eq 'ver' || $key eq 'api';
                        %options{$key} := $value;
                    }
                }
            }
            my $meta-object := $!how.new_type(|%options);
            if $!is-require-stub {
                my $cont := nqp::create(Scalar);
                nqp::bindattr($cont, Scalar, '$!value', $meta-object);
                my $cont-desc := ContainerDescriptor::Untyped.new(:of(Mu), :default(Mu), :!dynamic);
                nqp::bindattr($cont, Scalar, '$!descriptor', $cont-desc);
                $meta-object := $cont;
            }
            $meta-object
        }
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $type := self.stubbed-meta-object(:$resolver, :$context);
        self.IMPL-COMPOSE-TYPE($type);
        CATCH {
            nqp::bindattr(self, RakuAST::Package, '$!compose-exception', $_)
        }
        $type
    }

    # Compose $type via its HOW
    method IMPL-COMPOSE-TYPE(Mu $type) {
        $type.HOW.compose($type);
    }

    method apply-implicit-block-semantics(:$resolver, :$context) {
        unless $!block-semantics-applied {
            self.meta-object-as-body-lexicals('PACKAGE', :$resolver, :$context);
            self.additional-body-lexicals(:$resolver, :$context);

            nqp::bindattr(self,RakuAST::Package,'$!block-semantics-applied',1);
        }
    }

    # Add any additional lexicals to the body
    method additional-body-lexicals(:$resolver, :$context) {
        self.meta-object-as-body-lexicals('CLASS', :$resolver, :$context)
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

    method IMPL-COMPOSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # $resolver/$context are mandatory here: this is the first
        # meta-object access for the package and fills the cache. A bare
        # call would cache the degraded compose and starve later callers
        # of accessor QAST.
        self.meta-object(:$resolver, :$context);
        self.IMPL-MAYBE-REGISTER-INSTANTIATION-LEXICAL;
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
    # Methods and attributes are not directly added, but rather thorugh the
    # attach target mechanism. Attribute usages are also attached for checking
    # after compose time.
    has Mu $.attached-methods;
    has Mu $.attached-attributes;
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
                         str :$parsed-declarator,
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
        nqp::bindattr_s($obj, RakuAST::Package, '$!parsed-declarator', $parsed-declarator);

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
        my $type := self.stubbed-meta-object;
        $type.HOW.add_attribute($type, $attribute.meta-object);
        Nil
    }

    method ATTACH-ATTRIBUTE-USAGE(RakuAST::Var::Attribute $attribute) {
        nqp::bindkey($!attached-attribute-usages, $attribute.name, $attribute);
        Nil
    }

    # Add methods and attributes to meta object
    method PRODUCE-META-ATTACHABLES($type, $how) {
        for $!attached-methods {
            my str $name    := $_.name.canonicalize;
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
        }
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle roles

class RakuAST::Role
  is RakuAST::Package::Attachable
{
    has Array $.instantiation-lexicals;
    has Array $!pending-ins-lexicals;
    has RakuAST::LexicalFixup $!fixup;

    method declarator()  { "role"                       }
    method default-how() { Metamodel::ParametricRoleHOW }
    method attach-target-names() { self.IMPL-WRAP-LIST(['package', 'also', 'generics-pad']) }

    # Called twice: once from Package.new with no real body, then again
    # from package-def with the parsed body. Only wrap the body when we
    # have one, since the wrapping calls stubbed-meta-object and that
    # memoizes; running it before parser state is bound caches a wrong
    # meta-object.
    method replace-body(RakuAST::Code $role-body, RakuAST::Signature $signature) {
        # The body of a role is internally a Sub that has the parameterization
        # of the role as the signature.  This allows a role to be selected
        # using ordinary dispatch semantics.  The statement list gets a return
        # value added, so that the role's meta-object and lexpad are returned.
        nqp::bindattr(self, RakuAST::Role, '$!fixup', RakuAST::LexicalFixup.new) unless $!fixup;
        unless nqp::defined($!instantiation-lexicals) {
            nqp::bindattr(self, RakuAST::Role, '$!instantiation-lexicals', []);
        }

        unless $signature {
            $signature := $role-body ?? $role-body.signature !! RakuAST::Signature.new;
        }
        my $body-node := $role-body // RakuAST::RoleBody.new(:$signature);
        $body-node.set-fixup($!fixup);
        $body-node.replace-name(self.name);
        $body-node.replace-signature($signature);

        if $role-body {
            for $signature.IMPL-UNWRAP-LIST($signature.parameters) {
                $_.set-owner($role-body);
            }

            my $body := $role-body.body;
            my $resolve-instantiations;
            $body.statement-list.unshift-statement(
                $resolve-instantiations := RakuAST::Role::ResolveInstantiations.new(
                    $!instantiation-lexicals)
            );

            $body.statement-list.add-statement(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Nqp.new('list',
                  RakuAST::Declaration::ResolvedConstant.new(
                    compile-time-value => self.stubbed-meta-object
                  ),
                  nqp::elems($!instantiation-lexicals)
                      ?? RakuAST::Role::TypeEnvVar.new($resolve-instantiations.type-env-var)
                      !! RakuAST::Nqp.new('curlexpad')
                )
              )
            );
        }

        nqp::bindattr(self, RakuAST::Package, '$!body', $body-node);
        Nil
    }

    method parameterization() { self.body.signature }

    method declare-lexicals(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.meta-object-as-lexicals($resolver, 'PACKAGE', :$context);
        self.meta-object-as-lexicals($resolver, 'ROLE', :$context);

        for '$?CLASS', '::?CLASS' {
            $resolver.declare-lexical(
              RakuAST::Type::Capture.new(RakuAST::Name.from-identifier($_)).to-begin-time($resolver, $context)
            );
        }
    }

    method install-in-scope(RakuAST::Resolver $resolver, str $scope, RakuAST::Name $name, RakuAST::Name $full-name) {
        # Find an appropriate existing role group
        my $group := $resolver.resolve-name-constant($full-name, :current-scope-only(self.scope eq 'my'));
        if $group && !nqp::istype($group.compile-time-value.HOW, Perl6::Metamodel::PackageHOW) {
            $group := $group.compile-time-value;
            $resolver.panic(
                $resolver.build-exception('X::Redeclaration', :symbol(self.name.canonicalize))
            ) unless nqp::can($group.HOW, 'add_possibility');
        }

        # No existing one found - create a role group
        else {
            my $group-name := $full-name.canonicalize(:colonpairs(0));
            $group := Perl6::Metamodel::ParametricRoleGroupHOW.new_type(
              :name($group-name), :repr(self.repr)
            );
            self.IMPL-INSTALL-PACKAGE(
              $resolver, $scope, $name, $resolver.current-package,
              :meta-object($group),
            );
        }
        # Add ourselves to the role group
        my $type-object := self.stubbed-meta-object;
        $type-object.HOW.set_group($type-object, $group);
        nqp::bindattr(self,RakuAST::Package::Attachable,'$!role-group',$group);
    }

    method install-extra-declarations(RakuAST::Resolver $resolver) {
        # We might have declarations from our signature. Need to push them on
        # to the outer scope as the role itself won't generate code for its
        # declarations.
        for self.IMPL-UNWRAP-LIST(self.generated-lexical-declarations) {
            $resolver.current-scope.add-generated-lexical-declaration($_);
        }
        $resolver.current-scope.add-generated-lexical-declaration(self.body.fixup) if self.body.fixup;
    }

    method additional-body-lexicals(:$resolver, :$context) {
        self.meta-object-as-body-lexicals('ROLE', :$resolver, :$context);

        self.body.add-generated-lexical-declaration(
            # $?CONCRETIZATION is actually a run-time symbol because it's being initialized when role is
            # getting specialized. But we make it ?-twigilled to stay in line with $?ROLE, $?CLASS, etc.,
            # and to reduce pollution of lexcial namespace.
            RakuAST::VarDeclaration::Implicit::RoleConcretization.new(:name('$?CONCRETIZATION'), :scope('my'))
        );

        # Flush any instantiation-lexicals that nested generic packages
        # queued during their own compose. The role's body has been
        # replaced by now (in Raku/Actions.nqp), so adding the lexical
        # declarations here puts them on the body that will actually be
        # emitted, and registering them on $!instantiation-lexicals gets
        # them rebound by resolve_instantiations per specialization.
        if $!pending-ins-lexicals {
            for $!pending-ins-lexicals {
                self.body.add-generated-lexical-declaration($_);
                self.IMPL-ADD-GENERIC-LEXICAL($_);
            }
            nqp::bindattr(self, RakuAST::Role, '$!pending-ins-lexicals', Array);
        }
    }

    # Queue an instantiation-lexical declaration requested by a nested
    # generic-typed package that is being composed before our body has
    # been replaced. additional-body-lexicals (called during the role's
    # apply-implicit-block-semantics) drains the queue. Guards against
    # double-registration by name in case the same package enters
    # IMPL-COMPOSE more than once, which should not happen in practice
    # but would otherwise leak duplicate entries into
    # $!instantiation-lexicals and duplicate lexical decls into the
    # role body.
    method IMPL-QUEUE-INSTANTIATION-LEXICAL(Mu $decl) {
        unless nqp::defined($!pending-ins-lexicals) {
            nqp::bindattr(self, RakuAST::Role, '$!pending-ins-lexicals', []);
        }
        my str $name := $decl.name;
        for $!pending-ins-lexicals {
            return Nil if $_.name eq $name;
        }
        if $!instantiation-lexicals {
            for $!instantiation-lexicals {
                return Nil if $_.name eq $name;
            }
        }
        nqp::push($!pending-ins-lexicals, $decl);
        Nil
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $type := self.stubbed-meta-object(:$resolver, :$context);

        unless self.is-stub {
            my $how := $type.HOW;
            self.PRODUCE-META-ATTACHABLES($type, $how);
            $how.set_body_block($type, self.body.meta-object(:$resolver, :$context));
            self.IMPL-COMPOSE-TYPE($type);
            CATCH {
                nqp::bindattr(self, RakuAST::Package, '$!compose-exception', $_)
            }
            my $group :=
              nqp::getattr(self, RakuAST::Package::Attachable, '$!role-group');
            $group.HOW.add_possibility($group, $type) unless $group =:= Mu;
        }

        $type
    }

    method IMPL-ADD-GENERIC-LEXICAL(Mu $lexical) {
        unless nqp::defined($!instantiation-lexicals) {
            nqp::bindattr(self, RakuAST::Role, '$!instantiation-lexicals', []);
        }
        nqp::push($!instantiation-lexicals, $lexical);
    }

    method IMPL-COMPOSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # See Package.IMPL-COMPOSE; $resolver/$context are mandatory and
        # the first meta-object access fills the cache.
        self.meta-object(:$resolver, :$context);
        self.body.IMPL-FINISH-ROLE-BODY($context);
    }
}

class RakuAST::Role::ResolveInstantiations
  is RakuAST::Statement
{
    has List $.instantiation-lexicals;
    has str $.type-env-var;

    method new(@instantiation-lexicals) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Role::ResolveInstantiations, '$!instantiation-lexicals', @instantiation-lexicals);
        nqp::bindattr_s($obj, RakuAST::Role::ResolveInstantiations, '$!type-env-var', QAST::Node.unique('__typeenv_'));
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::elems($!instantiation-lexicals) {
            my @names;
            for $!instantiation-lexicals {
                nqp::push(@names, $_.lexical-name);
            }
            $context.ensure-sc(@names);
            QAST::Op.new( :op<bind>,
                QAST::Var.new( :name($!type-env-var), :scope<local>, :decl<var> ),
                QAST::Op.new( :op<callmethod>, :name<resolve_instantiations>,
                    QAST::Op.new( :op<how>,
                        QAST::Var.new( :name<::?ROLE>, :scope<lexical> ) ),
                    QAST::Var.new( :name<::?ROLE>, :scope<lexical> ),
                    QAST::Op.new( :op<curlexpad> ),
                    QAST::WVal.new( :value(@names) )
                ))
        }
        else {
            QAST::Op.new(:op<null>)
        }
    }
}

class RakuAST::Role::TypeEnvVar
    is RakuAST::Expression
{
    has str $.type-env-var;

    method new(str $type-env-var) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Role::TypeEnvVar, '$!type-env-var', $type-env-var);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
      QAST::Var.new( :name($!type-env-var), :scope<local> )
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle classes and grammars

class RakuAST::Class
  is RakuAST::Package::Attachable
{
    method declarator()  { "class"             }
    method default-how() { Metamodel::ClassHOW }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $type := self.stubbed-meta-object(:$resolver, :$context);
        self.PRODUCE-META-ATTACHABLES($type, $type.HOW);

        {
            self.IMPL-COMPOSE-TYPE($type);
            CATCH {
                nqp::bindattr(self, RakuAST::Package, '$!compose-exception', $_)
            }
        }

        $type
    }

    # Produce any accessor methods as well as the POPULATE method from
    # the attributes that are known at this time, and add them as methods
    # for later processing
    method PRODUCE-ACCESSORS-POPULATE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {

        # Make the resolver aware the invocant class
        $resolver.push-scope(self.body);
        $resolver.push-package(self);

#- initializations -------------------------------------------------------------

        # Set up lookup for existing methods
        my $methods := nqp::hash();
        for self.attached-methods {
            my str $name := $_.name.canonicalize;
            nqp::bindkey($methods,$name,1);
        }

        # The body of the POPULATE method to be
        my $statements := RakuAST::StatementList.new;
        my $ast-self   := RakuAST::Term::Self.new;
        my $ast-type   := RakuAST::Var::Compiler::Lookup.new('$?CLASS');
        my $ast-is-raw := RakuAST::Trait::Is.new(
          name => RakuAST::Name.from-identifier("raw")
        );

#- helper subs -----------------------------------------------------------------

        # Helper sub for adding statements
        my sub add-statement($expression) {
            $statements.add-statement(
              RakuAST::Statement::Expression.new(:$expression)
            )
        }

        # Helper sub to create AST representation of a name
        my sub makeName(str $name) {
            nqp::index($name,"::") == -1
              ?? RakuAST::Name.from-identifier($name)
              !! RakuAST::Name.from-identifier-parts(|nqp::split("::",$name))
        }

        # Helper sub to create AST representation of a type by name
        my sub makeType(str $name) {
            RakuAST::Type::Simple.new(makeName($name))
        }

        # Helper sub to create a :foo(bar) named argument
        my sub makeColonPairValue(str $name, $ast) {
            RakuAST::ColonPair::Value.new(key => $name, value => $ast)
        }

        # Helper sub to create the default value for a given type
        my sub makeDefault($attribute) {
            (my int $primspec := nqp::objprimspec($attribute.IMPL-OF-TYPE))
              ?? RakuAST::Literal.from-value(
                   $primspec == 3 ?? "" !! $primspec == 2 ?? 0e0 !! 0
                 )
              !! makeType("Nil")
        }

#- process all attributes ------------------------------------------------------
        my int $are-built;
        my @attributes := self.attached-attributes;

        for @attributes -> $attribute {
            my str $name   := $attribute.name;
            my str $sigil  := $attribute.sigil;
            my str $twigil := $attribute.twigil;
            my str $key    := $attribute.desigilname.canonicalize;

            my int $is-built     := $twigil eq ".";
            my int $has-accessor := $is-built;
            my int $is-bound;
            my int $is-rw;

            my $default;
            for self.IMPL-UNWRAP-LIST($attribute.traits) -> $trait {
                my str $trait-name := $trait.IMPL-TRAIT-NAME;

                # Has some default value logic
                if $trait-name eq 'will' {
                    my $expr := $trait.expr;
                    $default := nqp::istype($expr,RakuAST::Method::Initializer)
                      ?? $expr.body.statement-list.statements.head.expression
                      !! $expr;
                }

                # Some type of "is" trait
                elsif $trait-name eq 'is' {
                    my str $name := $trait.name.canonicalize;

                    # is required
                    if $name eq 'required' {
                        my $required := (my $argument := $trait.argument)
                          ?? $argument.semilist.head
                          !! RakuAST::IntLiteral.new(1);

                        # X::Attribute::Required.new(:$name, :$why).throw
                        $default := RakuAST::ApplyPostfix.new(
                          operand => RakuAST::ApplyPostfix.new(
                            operand => makeType("X::Attribute::Required"),
                            postfix => RakuAST::Call::Method.new(
                              name => makeName("new"),
                              args => RakuAST::ArgList.new(
                                makeColonPairValue(
                                  "name", RakuAST::StrLiteral.new($name)
                                ),
                                makeColonPairValue(
                                  "why", RakuAST::Literal.from-value($required)
                                )
                              )
                            )
                          ),
                          postfix => RakuAST::Call::Method.new(
                            name => makeName("throw")
                          )
                        )
                    }

                    # is built
                    elsif $name eq 'built' {

                        # is built(foo)
                        if (my $argument := $trait.argument) {
                            my $expression :=
                              $argument.semilist.statements.head.expression;
                            if nqp::istype($expression,RakuAST::Term::Enum) {
                                my str $name := $expression.name.canonicalize;
                                $name eq 'False'
                                  ?? ($is-built := 0)
                                  !! $name eq 'True'
                                    ?? ($is-built := 1)
                                    !! nqp::die("Unknown value in 'is built' trait: $name");
                            }
                            elsif nqp::istype(
                                    $expression,RakuAST::ColonPair::True
                                  ) {
                                my str $key := $expression.key;
                                $key eq 'bind'
                                  ?? ($is-bound := 1)
                                  !! nqp::die("Unknown colonpair in 'is built' trait: :$key");
                            }
                            elsif nqp::istype(
                                    $expression,RakuAST::ColonPair::False
                                  ) {
                                my str $key := $expression.key;
                                $key eq 'bind'
                                  ?? ($is-bound := 0)
                                  !! nqp::die("Unknown colonpair in 'is built' trait: :!$key");
                            }
                            else {
                                nqp::die("Unknown value in 'built' trait");
                            }
                        }

                        # is built
                        else {
                            $is-built := 1;
                        }
                    }

                    # is rw
                    elsif $name eq 'rw' {
                        $is-rw := 1;
                    }
                }
            }

            # need to create an accessor
            if $has-accessor  && nqp::not_i(nqp::existskey($methods,$key)) {

                # method $key () { $!attribute }
                my $method := RakuAST::Method.new(
                  name   => makeName($key),
                  traits => $is-rw ?? ($ast-is-raw,) !! (),
                  body   => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                      RakuAST::Statement::Expression.new(
                       expression => RakuAST::Var::Attribute.new(
                                       $sigil ~ '!' ~ $key
                                     )
                      )
                    )
                  )
                );
                $method.IMPL-BEGIN($resolver, $context);
            }

            # attribute can be specified as named argument
            if $is-built {
                ++$are-built;

                # $!a = nqp::ifnull(nqp::atkey($nameds,$key),$default)
                add-statement(RakuAST::ApplyInfix.new(
                  left  => RakuAST::Var::Attribute.new(
                             $attribute.sigil ~ '!' ~ $key
                           ),
                  infix => $is-bound
                    ?? RakuAST::Infix.new(":=")
                    !! RakuAST::Assignment.new,
                  right => RakuAST::Nqp.new(
                             "ifnull",
                             RakuAST::Nqp.new(
                               "atkey",
                               RakuAST::Var::Lexical.new('$nameds'),
                               RakuAST::StrLiteral.new($key)
                             ),
                             $default // makeDefault($attribute)
                           )
                ));
            }
        }

#- wrap it up ------------------------------------------------------------------

        # Mu.POPULATE for now already contains a POPULATE method,
        # and some naughty people may have added their own.
        unless nqp::existskey($methods,'POPULATE') {

            # at least one attribute to be built
            if $are-built {
                # Try to do everything as low level as possible, so extract the
                # NQP hash from the positional argument
                # my $nameds := nqp::getattr(%nameds,Map,'$!storage')
                $statements.unshift-statement(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::VarDeclaration::Simple.new(
                      sigil       => '$',
                      desigilname => makeName("nameds"),
                      initializer => RakuAST::Initializer::Bind.new(
                        # my $nameds := nqp::getattr(%nameds,Map,'$!storage')
                        RakuAST::Nqp.new(
                          "getattr",
                          RakuAST::Var::Lexical.new('%nameds'),
                          makeType("Map"),
                          RakuAST::StrLiteral.new('$!storage')
                        )
                      )
                    )
                  )
                );
            }

            # self
            add-statement($ast-self);

            # method POPULATE(%nameds) { $statements }
            my $method := RakuAST::Method.new(
              name      => makeName("POPULATE"),
              signature => RakuAST::Signature.new(
                parameters => (
                  RakuAST::Parameter.new(
                    target   => RakuAST::ParameterTarget::Var.new(
                      name => "\%nameds"
                    ),
                    optional => False
                  )
                )
              ),
              body      => RakuAST::Blockoid.new($statements)
            );

            $method.IMPL-BEGIN($resolver, $context);
        }

        # Remove knowledge of invocant class from the resolver
        $resolver.pop-package();
        $resolver.pop-scope();
    }

    method PERFORM-CHECK(
      RakuAST::Resolver          $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::findmethod(RakuAST::Package::Attachable, 'PERFORM-CHECK')(
          self, $resolver, $context
        );

        # Create accessors and POPULATE now
        self.PRODUCE-ACCESSORS-POPULATE($resolver, $context);
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle grammars

class RakuAST::Grammar
  is RakuAST::Class
  is RakuAST::CheckTime
{
    method declarator()  { "grammar"             }
    method default-how() { Metamodel::GrammarHOW }

    method PERFORM-CHECK(
      RakuAST::Resolver          $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::findmethod(RakuAST::Class, 'PERFORM-CHECK')(
          self, $resolver, $context
        );
        my $sanity-check := self.HOW.find_method(self,"check-sanity");
        $sanity-check(self) if $sanity-check;
        True;
    }
}

#-------------------------------------------------------------------------------
# Specific logic to handle modules

class RakuAST::Module
  is RakuAST::Package
{
    method declarator()  { "module"              }
    method default-how() { Metamodel::ModuleHOW  }

    method declare-lexicals(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.meta-object-as-lexicals($resolver, 'PACKAGE', :$context);
        self.meta-object-as-lexicals($resolver, 'MODULE', :$context);
    }

    method additional-body-lexicals(:$resolver, :$context) {
        self.meta-object-as-body-lexicals('MODULE', :$resolver, :$context);
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
