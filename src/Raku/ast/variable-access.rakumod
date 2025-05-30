# Marker for different variable-like things.
class RakuAST::Var
  is RakuAST::Term
{
    method sigil() { '' }
}

# A typical lexical variable lookup (e.g. $foo).
class RakuAST::Var::Lexical
  is RakuAST::Var
  is RakuAST::Lookup
  is RakuAST::ParseTime
  is RakuAST::Sinkable
{
    has str $.sigil;
    has str $.twigil;
    has RakuAST::Name $.desigilname;

    method new(str $name?, Str :$sigil, Str :$twigil, RakuAST::Name :$desigilname) {
        my $obj := nqp::create(self);
        if $name {
            nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!sigil', nqp::substr($name, 0, 1));
            nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!twigil', '');
            nqp::bindattr($obj, RakuAST::Var::Lexical, '$!desigilname',
                RakuAST::Name.from-identifier(nqp::substr($name, 1)))
        }
        else {
            nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!sigil', $sigil);
            nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!twigil', $twigil);
            nqp::bindattr($obj, RakuAST::Var::Lexical, '$!desigilname', $desigilname);
        }
        $obj
    }

    method postdeclaration-exception-name() { 'X::Redeclaration::Outer' }

    method name() {
        ($!sigil // '') ~ ($!twigil // '') ~ $!desigilname.canonicalize
    }

    method can-be-bound-to() {
        self.is-resolved ?? self.resolution.can-be-bound-to !! False
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        self.is-resolved
            ?? self.resolution.build-bind-exception($resolver)
            !! nqp::findmethod(RakuAST::Node, 'build-bind-exception')(self, $resolver)
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless self.is-resolved {
            my $resolved := $resolver.resolve-lexical(self.name);
            if $resolved {
                self.set-resolution($resolved);
            }
        }
        if self.is-resolved && nqp::istype(self.resolution, RakuAST::VarDeclaration::AttributeAlias) {
            my $self := RakuAST::Term::Self.new.to-begin-time($resolver, $context);
            unless $self.is-resolved {
                self.add-sorry($resolver.build-exception('X::Syntax::NoSelf', :variable(self.name)));
            }
        }
        self.add-sunk-worry($resolver, self.name) if self.sunk;
    }

    method undeclared-symbol-details() {
        $!sigil eq '&'
            ?? RakuAST::UndeclaredSymbolDescription::Routine.new($!desigilname.canonicalize)
            !! Nil
    }

    method is-topic() {
        self.name eq '$_'
    }

    method return-type() {
        self.is-resolved
            ?? self.resolution.return-type
            !! Mu
    }

    method maybe-compile-time-value() {
        self.resolution.compile-time-value
    }

    method IMPL-IS-META-OP() {
        ($!sigil eq '&' || $!sigil eq '')
            && (my $cp := $!desigilname.IMPL-UNWRAP-LIST($!desigilname.colonpairs))
            && nqp::istype($cp[0], RakuAST::QuotedString)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        self.resolution.IMPL-BIND-QAST($context, $source-qast)
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.resolution.compile-time-value
    }

    method dump-markers() {
        '【' ~ self.name ~ '】'
    }
}

# A lexical variable lookup, but assumed to resolve to a compile time
# value.
class RakuAST::Var::Lexical::Constant
  is RakuAST::Var::Lexical
{
    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical-constant(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
    }
}

# A lexical looked up in the setting (used for when we really want the setting
# version of a routine).
class RakuAST::Var::Lexical::Setting
  is RakuAST::Var::Lexical
{
    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical-constant-in-setting(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
    }
}

# A dynamic variable lookup (e.g. $*foo).
class RakuAST::Var::Dynamic
  is RakuAST::Var
  is RakuAST::Lookup
  is RakuAST::ParseTime
  is RakuAST::CheckTime
{
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Dynamic, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method can-be-bound-to() { True }

    method needs-resolution() { False }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical($!name, :current-scope-only);
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method postdeclaration-exception-name() { 'X::Dynamic::Postdeclaration' }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless self-is-resolved {
            my $resolved := $resolver.resolve-lexical($!name, :current-scope-only);
            if $resolved {
                self.set-resolution($resolved);
            }
        }

        self.add-sorry(
          $resolver.build-exception:
            'X::Dynamic::Package', :symbol($!name)
        ) if nqp::index($!name, '::') >= 0;
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # If it's resolved in the current scope, just a lexical access.
        if self.is-resolved {
            my $name := self.resolution.lexical-name;
            QAST::Var.new( :$name, :scope<lexical> )
        }
        else {
            my $with-star := QAST::SVal.new( :value($!name) );
            my $without-star := QAST::SVal.new( :value(nqp::replace($!name, 1, 1, '')) );
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new( :op('getlexdyn'), $with-star),
                QAST::Op.new(
                    :op('callstatic'), :name('&DYNAMIC-FALLBACK'),
                    $with-star, $without-star
                )
            )
        }
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        # If it's resolved in the current scope, just a lexical bind.
        if self.is-resolved {
            my $name := self.resolution.lexical-name;
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :$name, :scope<lexical> ),
                $source-qast
            )
        }
        else {
            my $complain := QAST::Op.new(
                :op('die_s'),
                QAST::SVal.new( :value('Dynamic variable ' ~ $!name ~ ' not found') )
            );
            QAST::Op.new(
                :op('bind'),
                QAST::VarWithFallback.new(
                    :name($!name), :scope('contextual'), :fallback($complain)
                ),
                $source-qast
            )
        }
    }
}

# A (private) attribute access (e.g. $!foo).
class RakuAST::Var::Attribute
  is RakuAST::Var
  is RakuAST::ImplicitLookups
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has str $.name;
    has RakuAST::Package $!package;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Attribute, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method can-be-bound-to() {
        # stubbed-meta-object gives you the type object without trying to compose it first.
        my $package := $!package.stubbed-meta-object;
        if $package.HOW.has_attribute($package, $!name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
            return nqp::objprimspec($attr-type) ?? False !! True
        }

        # Return True so we don't have multi error for a non-existent attribute.
        True
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $package := $resolver.find-attach-target('package');
        if $package {
            # We can't check attributes exist until we compose the
            # package, since they may come from roles. Thus we need to
            # attach them to the package.
            $package.ATTACH-ATTRIBUTE-USAGE(self);
            nqp::bindattr(self, RakuAST::Var::Attribute, '$!package', $package);
        }
        else {
            # TODO check-time error
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # stubbed-meta-object gives you the type object without trying to compose it first.
        if $!package {
            my $package := $!package.stubbed-meta-object;

            self.add-sorry: $resolver.build-exception: 'X::Attribute::Undeclared',
                :symbol($!name), :package-kind($!package.declarator),
                :package-name($package.HOW.name), :what('attribute')
                unless $package.HOW.has_attribute($package, $!name);
        }

        self.add-sorry: $resolver.build-exception: 'X::Syntax::NoSelf', :variable($!name)
            unless self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].is-resolved;
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Term::Self.new,
            RakuAST::Var::Compiler::Lookup.new('$?CLASS'),
        ]
    }

    method IMPL-QAST-PACKAGE-LOOKUP(RakuAST::Impl::QASTContext $context) {
        my $class := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1];
        if $class.is-resolved
          && nqp::istype($class.resolution, RakuAST::CompileTimeValue) {
            my $type-object := $class.resolution.compile-time-value;
            my $how := $type-object.HOW;
            unless nqp::can($how, 'archetypes') && nqp::can($how.archetypes, 'generic') && $how.archetypes.generic {
                return QAST::WVal.new(:value($type-object))
            }
        }
        return QAST::Var.new(:scope<lexical>, :name('$?CLASS'))
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $package := $!package.meta-object;
        if $package.HOW.has_attribute($package, $!name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
            return QAST::Var.new(
                :scope(nqp::objprimspec($attr-type) ?? 'attributeref' !! 'attribute'),
                :name($!name), :returns($attr-type),
                self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context),
                self.IMPL-QAST-PACKAGE-LOOKUP($context)
            )
        }
        QAST::Op.new(:op<null>)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        my $package := $!package.meta-object;
        if $package.HOW.has_attribute($package, $!name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
            unless nqp::eqaddr($attr-type, Mu) {
                $context.ensure-sc($attr-type);
                $source-qast := QAST::Op.new(
                    :op('p6bindassert'),
                    $source-qast,
                    QAST::WVal.new( :value($attr-type) )
                );
            }
            return QAST::Op.new(
                :op('bind'),
                QAST::Var.new(
                    :scope('attribute'), :name($!name), :returns($attr-type),
                    self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context),
                    self.IMPL-QAST-PACKAGE-LOOKUP($context)
                ),
                $source-qast
            )
        }
        QAST::Op.new(:op<null>)
    }
}

# Wrapper for $.foo "attribute" accesses
class RakuAST::Var::Attribute::Public
  is RakuAST::Term
{
    has str                   $.name;
    has RakuAST::ApplyPostfix $!expression;

    # Did we already get our args replaced once?
    # Important so we don't swallow call chains like $.foo()(1)
    has Bool $.has-args;

    method new(str :$name, RakuAST::ArgList :$args) {
        my str $sigil := nqp::substr($name,0,1);

        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Attribute::Public, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Var::Attribute::Public, '$!has-args', False);
        nqp::bindattr(  $obj, RakuAST::Var::Attribute::Public, '$!expression',
          # self.foo.item
          RakuAST::ApplyPostfix.new(
            operand => RakuAST::ApplyPostfix.new(
              operand => RakuAST::Term::Self.new(:variable($obj)),
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier(nqp::substr($name,2))
              )
            ),
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier(
                $sigil eq '@' ?? 'list' !! $sigil eq '%' ?? 'hash' !! 'item'
              )
            )
          )
        );
        $obj.replace-args($args) if $args;
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }

    method replace-args(RakuAST::Args $args) {
        nqp::bindattr(self, RakuAST::Var::Attribute::Public, '$!has-args', True);
        $!expression.operand.postfix.replace-args($args);
    }

    method args() {
        self.IMPL-WRAP-LIST($!expression.operand.postfix.args)
    }

    method creates-block() {
        $!expression.creates-block;
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $block := $resolver.find-attach-target('block');
        if nqp::istype($block, RakuAST::Method::Initializer) {
            self.add-sorry:
                $resolver.build-exception: 'X::Syntax::VirtualCall', call => $!name;
            return False;
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!expression.IMPL-EXPR-QAST($context)
    }
}

# The base for special compiler variables ($?FOO).
class RakuAST::Var::Compiler
  is RakuAST::Var { }

# The $?LANG variable which refers to the cursor at that point of the parse.
class RakuAST::Var::Compiler::Lang
  is RakuAST::Var::Compiler
{
    has Mu $.cursor;

    method new(Mu $cursor) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Compiler::Lang, '$!cursor', $cursor);
        $obj
    }

    method sigil() { '$' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!cursor;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) { $!cursor }
}

# The $?FILE variable, which is created pre-resolved to a string value.
class RakuAST::Var::Compiler::File
  is RakuAST::Var::Compiler
{
    has Str $.file;

    method new(Str $file) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Compiler::File, '$!file', $file);
        $obj
    }

    method sigil() { '$' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!file;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) { $!file }
}

# The $?LINE variable, which is created pre-resolved to an integer value.
class RakuAST::Var::Compiler::Line
  is RakuAST::Var::Compiler
{
    has Int $.line;

    method new(Int $line) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Compiler::Line, '$!line', $line);
        $obj
    }

    method sigil() { '$' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!line;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) { $!line }
}

class RakuAST::Var::Compiler::Block
  is RakuAST::Var::Compiler
{
    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
    }
}

class RakuAST::Var::Compiler::Routine
  is RakuAST::Var::Compiler
  is RakuAST::Var::Lexical
  is RakuAST::ParseTime
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!sigil', '&');
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!twigil', '?');
        nqp::bindattr($obj, RakuAST::Var::Lexical, '$!desigilname', RakuAST::Name.from-identifier('ROUTINE'));
        $obj
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $routine := $resolver.find-attach-target('routine');
        if nqp::isconcrete($routine) {
            $routine.set-need-routine-variable();
        }

        my $resolved := $resolver.resolve-lexical('&?ROUTINE');
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(:name<&?ROUTINE>, :scope<lexical>)
    }
}

class RakuAST::Var::Compiler::Resources
  is RakuAST::Var::Compiler
  is RakuAST::Var::Lexical
  is RakuAST::ImplicitLookups
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!sigil', '%');
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!twigil', '?');
        nqp::bindattr($obj, RakuAST::Var::Lexical, '$!desigilname', RakuAST::Name.from-identifier('RESOURCES'));
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts('Distribution', 'Resources')),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $resources := nqp::getlexdyn('$*RESOURCES');
        unless $resources {
            my $Resources := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
            $resources := $Resources.from-precomp();
        }
        if $resources {
            $context.ensure-sc($resources);
            QAST::WVal.new( :value($resources) );
        }
        else {
            QAST::WVal.new( :value(Nil) );
        }
    }
}

class RakuAST::Var::Compiler::Distribution
  is RakuAST::Var::Compiler
  is RakuAST::Var::Lexical
  is RakuAST::ImplicitLookups
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!sigil', '$');
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!twigil', '?');
        nqp::bindattr($obj, RakuAST::Var::Lexical, '$!desigilname', RakuAST::Name.from-identifier('DISTRIBUTION'));
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts('CompUnit', 'Repository', 'Distribution')),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $distribution := nqp::getlexdyn('$*DISTRIBUTION');
        unless $distribution {
            my $Distribution := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
            $distribution := $Distribution.from-precomp();
        }
        if $distribution {
            $context.ensure-sc($distribution);
            QAST::WVal.new( :value($distribution) );
        }
        else {
            QAST::WVal.new( :value(Nil) );
        }
    }
}

# A special compiler variable that resolves to a lookup, such as $?PACKAGE.
class RakuAST::Var::Compiler::Lookup
  is RakuAST::Var::Compiler
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Compiler::Lookup, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# The base for Rakudoc variable lookup
class RakuAST::Var::Doc
  is RakuAST::Var
{
    has str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Doc, '$!name', $name);
        $obj
    }
    method sigil()  { '$' }
    method twigil() { '=' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(:name(self.sigil ~ self.twigil ~ $!name), :scope<lexical>)
    }
}

# A regex positional capture variable (e.g. $0).
class RakuAST::Var::PositionalCapture
  is RakuAST::Var
  is RakuAST::ImplicitLookups
{
    has Int $.index;

    method new(Int $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::PositionalCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('&postcircumfix:<[ ]>'),
            RakuAST::Var::Lexical.new('$/'),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $index := $!index;
        $context.ensure-sc($index);
        QAST::Op.new(
            :op('call'),
            :name($lookups[0].is-resolved ?? $lookups[0].resolution.lexical-name !! '&postcircumfix:<[ ]>'),
            $lookups[1].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($index) )
        )
    }
}

# A regex named capture variable (e.g. $<foo>).
class RakuAST::Var::NamedCapture
  is RakuAST::Var
  is RakuAST::ImplicitLookups
{
    has RakuAST::QuotedString $.index;

    method new(RakuAST::QuotedString $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::NamedCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('&postcircumfix:<{ }>'),
            RakuAST::Var::Lexical.new('$/'),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $op := QAST::Op.new(
            :op('call'),
            :name($lookups[0].resolution.lexical-name),
            $lookups[1].IMPL-TO-QAST($context),
        );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        $op
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
    }
}

# A package variable, i.e. $Foo::bar
class RakuAST::Var::Package
  is RakuAST::Var
  is RakuAST::Lookup
  is RakuAST::ParseTime
  is RakuAST::CheckTime
{
    has str $.sigil;
    has RakuAST::Name $.name;

    method new(RakuAST::Name :$name!, :$sigil) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Package, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Var::Package, '$!sigil', $sigil);
        $obj
    }

    method sigil() { $!sigil }

    method can-be-bound-to() {
        self.is-resolved ?? self.resolution.can-be-bound-to !! $!name.is-pseudo-package
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        self.is-resolved
            ?? self.resolution.build-bind-exception($resolver)
            !! nqp::findmethod(RakuAST::Node, 'build-bind-exception')(self, $resolver)
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-name(RakuAST::Name.new($!name.root-part))
            unless $!name.is-empty || nqp::istype($!name.root-part, RakuAST::Name::Part::Empty);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if !self.is-resolved && ($!name.is-empty || $!name.is-anonymous) {
            my $name := $!name.canonicalize;
            self.add-sorry:
                $resolver.build-exception: 'X::Undeclared', :symbol($!sigil ~ $name),
                    :suggestions($resolver.suggest-lexicals($name));
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $sigil := $!sigil;
        if $!name.is-simple {
            if $!name.is-pseudo-package {
                $!name.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP($context, :$sigil);
            }
            else {
                my @parts := $!name.IMPL-LOOKUP-PARTS;
                my $final := @parts[nqp::elems(@parts) - 1];
                my $result;
                if self.is-resolved {
                    my $name := self.resolution.lexical-name;
                    nqp::shift(@parts);
                    $result := nqp::istype(self.resolution, RakuAST::CompileTimeValue)
                        ?? QAST::WVal.new(:value(self.resolution.compile-time-value))
                        !! QAST::Var.new(:$name, :scope<lexical>);
                }
                else {
                    $result := QAST::Op.new(:op<getcurhllsym>, QAST::SVal.new(:value<GLOBAL>));
                }
                for @parts {
                    $result := QAST::Op.new( :op('who'), $result );
                    $result := $_.IMPL-QAST-PACKAGE-LOOKUP-PART($context, $result, $_ =:= $final, :$sigil);
                }
                $result
            }
        }
        else {
            $!name.IMPL-QAST-INDIRECT-LOOKUP($context, :$sigil)
        }
    }

    method IMPL-ADJUST-QAST-FOR-LVALUE(Mu $qast) {
        my $last := $qast.list[-1];
        $qast.pop if nqp::istype($last, QAST::SpecialArg) && $last.named eq 'global_fallback';
        $qast
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        my $qast := $!name.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP($context, :sigil($!sigil));
        $source-qast.named('BIND');
        $qast.push($source-qast);
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

class RakuAST::Var::Slang
  is RakuAST::Var
  is RakuAST::ImplicitLookups
{
    has Mu $!grammar;
    has Mu $!actions;

    method new(Mu :$grammar!, Mu :$actions!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Slang, '$!grammar', $grammar);
        nqp::bindattr($obj, RakuAST::Var::Slang, '$!actions', $actions);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Slang')),
        ]
    }

    method sigil() { '$' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := QAST::Op.new(
            :op<callmethod>, :name<new>, :returns(self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value),
            QAST::Var.new( :name<Slang>, :scope<lexical> ));
        my $g := $!grammar;
        $context.ensure-sc($g);
        my $a := $!actions;
        if !nqp::isnull($g) {
            my $wval := QAST::WVal.new( :value($g) );
            $wval.named('grammar');
            $qast.push($wval);
            $wval := QAST::WVal.new( :value($a) );
            $wval.named('actions');
            $qast.push($wval);
        }
        $qast
    }
}
