# We want to compile Raku code by building up an AST, and thanks to macros,
# and other compile-time functionality, that AST needs to be something that
# is visible to Raku code. Thus, it should be made up of objects that are
# Raku-like - that is, we can introspect them just like any other Raku object.
# This means we need to build them up using the Raku MOP. The most convenient
# way to use *that* would be to write Raku code - but we can't, because we
# can't compile Raku code without the Raku AST!
#
# Thus, we need to piece together the Raku AST objects using the MOP. That is
# very tedious to do by hand. Thus this boring little compiler, which lets us
# write things that look like classes with attributes and methods, but with
# NQP bodies. These are then turned into code that uses the MOP to piece the
# AST nodes together - giving us rather easier to write/maintain code.

# Parser

grammar RakuASTParser {
    rule TOP {
        <?> <package>*
    }

    proto rule package {*}
    rule package:sym<class> {
        'class' <name> {}
        :my $*CLASS := ~$<name>;
        [ 'is' <parent=.name> ]?
        '{'
        [ <attribute-decl> | <method-decl> ]*
        [ '}' || <.panic('Missing }')> ]
    }

    rule attribute-decl {
        'has' <type=.name> [<attribute> || $<public-attribute>=['$.' <.identifier>]]
        [ ';' || <.panic('Missing ; after attribute declaration')> ]
    }

    token attribute {
        '$!' <.identifier>
    }

    rule method-decl {
        'method' <name=.identifier> <signature>?
        [ '{' || <.panic('Missing opening { of method')> ]
        <nqp-code>
        [ '}' || <.panic('Missing }')> ]
    }

    rule signature {
        '(' <parameter>* % [',' ] ')'
    }

    rule parameter {
        <type=.name>? [$<named>=':']?$<name>=['$'<.identifier>] [$<optional>=<[?!]>]?
    }

    token nqp-code {
        # We want to do some minor transforms on the NQP code, so just sorta
        # tokenize it. If it's good enough for the C preproc... :-)
        (
        | <name>
        | <attribute>
        | $<variable>=['$' <.identifier>]
        | <string>
        | $<numeric>=[ \d+ ['.' \d*]? [<[eE]> \d+]? ]
        | $<paren>='(' <nqp-code> [ ')' || <.panic('Missing )')> ]
        | $<brace>='{' <nqp-code> [ '}' || <.panic('Missing }')> ]
        | <?[\s#]> <ws>
        || $<other>=[<-[{}()'"\s\w$]>+] # don't include in LTM as it'd win too much
        )*
    }

    token string {
        | "'" [<-[\\']>+ | "\\'" | "\\"]* ["'" || <.panic('Unterminated string')> ]
        | '"' [<-[\\"]>+ | '\\"' | "\\"]* ['"' || <.panic('Unterminated string')> ]
    }

    token name {
        <identifier>+ % '::'
    }

    token identifier {
        <.ident> [<[-']><.ident>]*
    }

    token ws {
        <!ww>
        [
        | \s+
        | '#' \N+
        ]*
    }

    method panic($message) {
        nqp::die("$message near '" ~ nqp::substr(self.orig, self.pos, 20) ~ "'")
    }
}

# AST

role Node { }

class CompUnit {
    has @!packages;
    method packages() { @!packages }
}

class Package {
    has $!name;
    has $!parent;
    has @!attributes;
    has @!methods;
    method name() { $!name }
    method parent() { $!parent }
    method attributes() { @!attributes }
    method methods() { @!methods }
}

class Attribute {
    has $!type;
    has $!name;
    has $!has-accessor;
    method type() { $!type }
    method name() { $!name }
    method has-accessor() { $!has-accessor }
}

class Method {
    has $!name;
    has @!parameters;
    has $!body;
    method name() { $!name }
    method parameters() { @!parameters }
    method body() { $!body }
}

class Parameter {
    has $!type;
    has $!named;
    has $!name;
    has $!optional;
    method type() { $!type }
    method named() { $!named }
    method name() { $!name }
    method optional() { $!optional }
}

# AST-building actions

class RakuASTActions {
    method TOP($/) {
        my @packages;
        for $<package> {
            @packages.push($_.ast);
        }
        make CompUnit.new(:@packages);
    }

    method package:sym<class>($/) {
        my $name := ~$<name>;
        my $parent := $<parent> ?? ~$<parent> !! NQPMu;
        my @attributes;
        for $<attribute-decl> {
            @attributes.push($_.ast);
        }
        my @methods;
        for $<method-decl> {
            @methods.push($_.ast);
        }
        make Package.new(:$name, :$parent, :@attributes, :@methods);
    }

    method attribute-decl($/) {
        my $type := ~$<type>;
        if $<attribute> {
            my $name := ~$<attribute>;
            make Attribute.new(:$type, :$name, :!has-accessor);
        }
        else {
            my $name := nqp::replace(~$<public-attribute>, 1, 1, '!');
            make Attribute.new(:$type, :$name, :has-accessor);
        }
    }

    method method-decl($/) {
        my $name := ~$<name>;
        my @parameters := $<signature> ?? $<signature>.ast !! [];
        my $body := $<nqp-code>.ast;
        make Method.new(:$name, :@parameters, :$body);
    }

    method signature($/) {
        my @parameters;
        for $<parameter> {
            @parameters.push($_.ast);
        }
        make @parameters;
    }

    method parameter($/) {
        my $type := $<type> ?? ~$<type> !! NQPMu;
        my $named := ?$<named>;
        my $name := ~$<name>;
        my $optional := $named
            ?? ($<optional> eq '!' ?? 0 !! 1)
            !! ($<optional> eq '?' ?? 1 !! 0);
        make Parameter.new(:$type, :$named, :$name, :$optional);
    }

    method nqp-code($/) {
        my @chunks;
        for $/[0] -> $/ {
            if $<name> {
                # Rewrite `self` into `$SELF`, and True/False also.
                my $name := ~$<name>;
                if $name eq 'self' {
                    @chunks.push('$SELF');
                }
                elsif $name eq 'True' || $name eq 'False' {
                    @chunks.push('(Bool.WHO)<' ~ $name ~ '>');
                }
                else {
                    @chunks.push($name);
                }
            }
            elsif $<attribute> {
                my $name := ~$<attribute>;
                @chunks.push("nqp::getattr(\$SELF, $*CLASS, '$name')");
            }
            elsif $<string> {
                @chunks.push($<string>.ast);
            }
            elsif $<paren> {
               @chunks.push('(' ~ $<nqp-code>.ast ~ ')');
            }
            elsif $<brace> {
               @chunks.push('{' ~ $<nqp-code>.ast ~ '}');
            }
            else {
                @chunks.push(~$/);
            }
        }
        make nqp::join("", @chunks);
    }

    method string($/) {
        make ~$/;
    }
}

# Code-gen


# Frontend

sub MAIN(*@files) {
    # Parse everything.
    my @compunits;
    nqp::shift(@files); # first arg is this script
    for @files {
        my $source := slurp($_);
        @compunits.push(RakuASTParser.parse($source, actions => RakuASTActions).ast);
    }

    # Geneate code
    say('# Generated by tools/build/raku-ast-compiler.nqp');
    say('');
    emit-stubs(@compunits);
    say('BEGIN {');
    emit-mop-utils();
    for @compunits {
        for $_.packages {
            emit-package($_);
        }
    }
    say('    EXPORT::DEFAULT.WHO<RakuAST> := RakuAST;');
    say('}');
}

# Code-gen

sub emit-stubs(@compunits) {
    say('stub RakuAST metaclass Perl6::Metamodel::PackageHOW { ... };');
    say('BEGIN { Perl6::Metamodel::PackageHOW.add_stash(RakuAST); }');
    for @compunits -> $cu {
        for $cu.packages -> $package {
            say('stub ' ~ $package.name ~ ' metaclass Perl6::Metamodel::ClassHOW { ... };');
        }
    }
    say('');
}

sub emit-mop-utils() {
    say('
    ##
    ## Various utility subs to help us produce types that look Raku-like.
    ##

    sub parent($class, $parent) {
        $class.HOW.add_parent($class, $parent);
    }

    sub add-attribute($class, $type, $name) {
        $class.HOW.add_attribute($class, Attribute.new(
            :$name, :$type, :package($class)
        ));
    }

    sub add-method($class, $name, @parameters, $impl) {
        my $static-code := nqp::getstaticcode($impl);
        nqp::setcodename($static-code, $name);
        $class.HOW.add_method($class, $name, $static-code);
    }

    sub compose($type) {
        $type.HOW.compose_repr($type)
    }
');
}

sub emit-package($package) {
    my $name := $package.name;

    my $parent := $package.parent || 'Any';
    say("    parent($name, $parent);");

    my %need-accessor;
    for $package.attributes -> $attr {
        my $type := $attr.type;
        my $attr-name := $attr.name;
        say("    add-attribute($name, $type, '$attr-name');");
        if $attr.has-accessor {
            %need-accessor{nqp::substr($attr-name, 2)} := $attr-name;
        }
    }

    for $package.methods -> $method {
        nqp::deletekey(%need-accessor, $method.name);
        emit-method($name, $method);
    }

    for %need-accessor {
        my $method-name := $_.key;
        my $attr-name := $_.value;
        say("    add-method($name, '$method-name', [], sub (\$self) \{");
        say("        nqp::getattr(nqp::decont(\$self), $name, '$attr-name')");
        say("    });");
    }

    say("    compose($name);");
    say("");
}

sub emit-method($package, $method) {
    my @parameters := $method.parameters;
    my @params-in;
    my @params-desc;
    my @params-decont;
    for @parameters {
        my $param-name := $_.name;
        my $type := $_.type || 'Any';
        my $named := $_.named ?? ':' !! '';
        my $opt := $_.optional ?? '?' !! '!';
        @params-in.push(", $named$param-name$opt");
        @params-desc.push("$type, '$param-name'");
        @params-decont.push("$param-name := nqp::decont($param-name);");
    }
    my $params-in := nqp::join("", @params-in);
    my $params-desc := nqp::join(", ", @params-desc);

    my $name := $method.name;
    say("    add-method($package, '$name', [$params-desc], sub (\$SELF_CONT$params-in) \{");
    say("        my \$SELF := nqp::decont(\$SELF_CONT);");
    for @params-decont {
        say("        $_");
    }
    say("        " ~ $method.body);
    say("    });");
}
