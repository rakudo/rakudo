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

    method DEPARSE() { $!name }

    method sigil() { nqp::substr($!name, 0, 1) }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
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

    method DEPARSE() { $!name }

    method sigil() { nqp::substr($!name, 0, 1) }

    method needs-resolution() { False }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name, :current-scope-only);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method DEPARSE() { $!name }

    method sigil() { nqp::substr($!name, 0, 1) }

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

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $package := $!package.meta-object;
        my $attr-type := $package.HOW.get_attribute_for_usage($package, $!name).type;
        QAST::Var.new(
            :scope('attribute'), :name($!name), :returns($attr-type),
            @lookups[0].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($package) ),
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

    method DEPARSE() { '$?FILE' }

    method sigil() { '$' }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method DEPARSE() { '$?LINE' }

    method sigil() { '$' }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method DEPARSE() { $!name }

    method sigil() { nqp::substr($!name, 0, 1) }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
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

    method DEPARSE() { '$' ~ $!index }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<[ ]>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method DEPARSE() { '$<' ~ $!index.DEPARSE ~ '>' }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<{ }>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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
