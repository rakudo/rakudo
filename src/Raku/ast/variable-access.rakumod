# Marker for different variable-like things.
class RakuAST::Var is RakuAST::Term {
    method sigil() { '' }
}

# A typical lexical variable lookup (e.g. $foo).
class RakuAST::Var::Lexical is RakuAST::Var is RakuAST::Lookup {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method can-be-bound-to() {
        self.is-resolved ?? self.resolution.can-be-bound-to !! False
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, RakuAST::Expression $source) {
        self.resolution.IMPL-BIND-QAST($context, $source)
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.resolution.compile-time-value
    }
}

# A lexical variable lookup, but assumed to resolve to a compile time
# value.
class RakuAST::Var::Lexical::Constant is RakuAST::Var::Lexical {
    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical-constant(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }
}

# A lexical looked up in the setting (used for when we really want the setting
# version of a routine).
class RakuAST::Var::Lexical::Setting is RakuAST::Var::Lexical is RakuAST::Lookup {
    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical-constant-in-setting(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }
}

# A dynamic variable lookup (e.g. $*foo).
class RakuAST::Var::Dynamic is RakuAST::Var is RakuAST::Lookup {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Dynamic, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method can-be-bound-to() { True }

    method needs-resolution() { False }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name, :current-scope-only);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
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

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, RakuAST::Expression $source) {
        # If it's resolved in the current scope, just a lexical bind.
        my $source-qast := $source.IMPL-TO-QAST($context);
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

# An attribute access (e.g. $!foo).
class RakuAST::Var::Attribute is RakuAST::Var is RakuAST::ImplicitLookups
                              is RakuAST::Attaching {
    has str $.name;
    has RakuAST::Package $!package;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Attribute, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method can-be-bound-to() {
        my $package := $!package.meta-object;
        my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
        nqp::objprimspec($attr-type) ?? False !! True
    }

    method attach(RakuAST::Resolver $resolver) {
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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Term::Self.new,
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $package := $!package.meta-object;
        my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
        QAST::Var.new(
            :scope('attribute'), :name($!name), :returns($attr-type),
            @lookups[0].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($package) ),
        )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, RakuAST::Expression $source) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $package := $!package.meta-object;
        my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
        my $source-qast := $source.IMPL-TO-QAST($context);
        unless nqp::eqaddr($attr-type, Mu) {
            $context.ensure-sc($attr-type);
            $source-qast := QAST::Op.new(
                :op('p6bindassert'),
                $source-qast,
                QAST::WVal.new( :value($attr-type) )
            );
        }
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new(
                :scope('attribute'), :name($!name), :returns($attr-type),
                @lookups[0].IMPL-TO-QAST($context),
                QAST::WVal.new( :value($package) ),
            ),
            $source-qast
        )
    }
}

# The base for special compiler variables ($?FOO).
class RakuAST::Var::Compiler is RakuAST::Var {
}

# The $?FILE variable, which is created pre-resolved to a string value.
class RakuAST::Var::Compiler::File is RakuAST::Var::Compiler {
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
class RakuAST::Var::Compiler::Line is RakuAST::Var::Compiler {
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

# A special compiler variable that resolves to a lookup, such as $?PACKAGE.
class RakuAST::Var::Compiler::Lookup is RakuAST::Var::Compiler is RakuAST::Lookup {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Compiler::Lookup, '$!name', $name);
        $obj
    }

    method sigil() { nqp::substr($!name, 0, 1) }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# The base for POD variables ($=foo).
class RakuAST::Var::Pod is RakuAST::Var {
}

# The Pod $=finish variable.
class RakuAST::Var::Pod::Finish is RakuAST::Var is RakuAST::Attaching {
    has RakuAST::CompUnit $!cu;

    method new() {
        nqp::create(self)
    }

    method sigil() { '$' }

    method attach(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Var::Pod::Finish, '$!cu',
            $resolver.find-attach-target('compunit'));
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $finish-content := $!cu.finish-content;
        $context.ensure-sc($finish-content);
        QAST::WVal.new( :value($finish-content) )
    }
}

# A regex positional capture variable (e.g. $0).
class RakuAST::Var::PositionalCapture is RakuAST::Var is RakuAST::ImplicitLookups {
    has Int $.index;

    method new(Int $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::PositionalCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<[ ]>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $index := $!index;
        $context.ensure-sc($index);
        QAST::Op.new(
            :op('call'),
            :name(@lookups[0].resolution.lexical-name),
            @lookups[1].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($index) )
        )
    }
}

# A regex named capture variable (e.g. $<foo>).
class RakuAST::Var::NamedCapture is RakuAST::Var is RakuAST::ImplicitLookups {
    has RakuAST::QuotedString $.index;

    method new(RakuAST::QuotedString $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::NamedCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<{ }>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $op := QAST::Op.new(
            :op('call'),
            :name(@lookups[0].resolution.lexical-name),
            @lookups[1].IMPL-TO-QAST($context),
        );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        $op
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
    }
}

# A package variable, i.e. $Foo::bar
class RakuAST::Var::Package is RakuAST::Var is RakuAST::Lookup {
    has str $.sigil;
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name, :$sigil) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::Package, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Var::Package, '$!sigil', $sigil);
        $obj
    }

    method sigil() { $!sigil }

    method can-be-bound-to() {
        self.is-resolved ?? self.resolution.can-be-bound-to !! False
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my @parts := self.IMPL-UNWRAP-LIST($!name.parts);
        my $resolved := $resolver.resolve-name(RakuAST::Name.new(@parts[0]));
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $sigil := $!sigil;
        if $!name.is-simple {
            my @parts := nqp::clone(self.IMPL-UNWRAP-LIST($!name.parts));
            my $final := @parts[nqp::elems(@parts) - 1];
            my $result;
            if self.is-resolved {
                my $name := self.resolution.lexical-name;
                nqp::shift(@parts);
                $result := QAST::Var.new(:$name, :scope<lexical>);
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
        else {
            $!name.IMPL-QAST-INDIRECT-LOOKUP($context, :$sigil)
        }
    }
}
