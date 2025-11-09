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
        <?> <package>* [$ || <.panic("Confused")>]
    }

    proto rule package {*}
    rule package:sym<class> { <sym> <package-def('class')> }

    rule package-def($*PKGDECL) {
        <name> {}
        :my $*PACKAGE-NAME := ~$<name>;
        :my %*ATTRS;
        [ 'is' <parent=.name> ]*
        [ '{' || <.panic("Missing block in $*PKGDECL $*PACKAGE-NAME declaration")> ]
        [ <attribute-decl> | <method-decl> ]*
        [ '}' || <.panic("Missing '}' in $*PKGDECL $*PACKAGE-NAME declaration")> ]
    }

    rule attribute-decl {
        'has' <type=.name> [<attribute> || $<public-attribute>=['$.' <.identifier>]]
        [ ';' || <.panic('Missing ; after attribute declaration')> ]
    }

    token attribute {
        '$!' <.identifier>
    }

    rule method-decl {
        'method' <name=.identifier> {}
        :my $*METHOD-NAME := ~$<name>;
         <signature>?
         <method-body>
    }

    token method-body {
        [ '{' || <.panic("Missing block in method '$*METHOD-NAME' declaration")> ]
        <nqp-code>
        [ '}' || <.panic("Missing '}' in method '$*METHOD-NAME' declaration")> ]
    }

    rule signature {
        '(' <parameter>* % [',' ] ')'
    }

    rule parameter {
        <type=.name>?
        [$<named>=':'|$<slurpy>='*']?$<name>=[<[$@%]><.identifier>][$<optional>=<[?!]>]?
        [$<raw>=[is raw]]?
    }

    token sigil {
        '$' | '@' | '%'
    }

    token nqp-code {
        # We want to do some minor transforms on the NQP code, so just sorta
        # tokenize it. If it's good enough for the C preproc... :-)
        (
        | <name>
        | <attribute>
        | $<variable>=[<.sigil> '*'? <.identifier>]
        | <string>
        | ['/' <-[/]>+ '/' || '//' || '/' <?before \s* [\d | '$']>] # regex or // operator
        | $<numeric>=[ \d+ ['.' \d*]? [<[eE]> \d+]? ]
        | $<paren>='(' {} <nqp-code> [ ')' || {} <.panic('Missing ) for opening ( at line ' ~ self.line-of($<paren>))> ]
        | $<brace>='{' {} <nqp-code> [ '}' || {} <.panic('Missing } for opening { at line ' ~ self.line-of($<brace>))> ]
        | $<brckt>='[' {} <nqp-code> [ ']' || {} <.panic('Missing ] for opening [ at line ' ~ self.line-of($<brckt>))> ]
        | <?[\s#]> <ws>
        || $<other>=[<-[{}()\[\]'"\s\w$/]>+] # don't include in LTM as it'd win too much
        )*
    }

    token string {
        | "'" [<-[\\']>+ | "\\'" | "\\\\"]* ["'" || <.panic('Unterminated string')> ]
        | '"' [<-[\\"]>+ | '\\"' | "\\".]* ['"' || <.panic('Unterminated string')> ]
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
        nqp::die( "$message near '" ~ nqp::substr(self.orig, self.pos, 20) ~ "' at "
            ~ $*CURRENT-FILE ~ ":" ~ HLL::Compiler.lineof(self.target, self.pos, :cache))
    }

    method line-of($whatever) {
        -1
    }
}

# AST

role Node {
    has $!line;
    method line() { $!line }
    method set-line($line) { $!line := $line; }
}

class CompUnit does Node {
    has @!packages;
    has $!filename;
    method packages() { @!packages }
    method filename() { $!filename }
}

class Package does Node {
    has $!type; # only 'class' for now, could be 'role' too
    has $!name;
    has @!parents;
    has @!attributes;
    has @!methods;
    method name() { $!name }
    method parents() { @!parents }
    method attributes() { @!attributes }
    method methods() { @!methods }
}

class Attribute does Node {
    has $!type;
    has $!name;
    has $!has-accessor;
    method type() { $!type }
    method name() { $!name }
    method has-accessor() { $!has-accessor }
    method getattr-op() {
        $!type eq 'int' ?? 'getattr_i' !!
        $!type eq 'num' ?? 'getattr_n' !!
        $!type eq 'str' ?? 'getattr_s' !!
                           'getattr'
    }
}

class Method does Node {
    has $!name;
    has @!parameters;
    has $!body;
    method name() { $!name }
    method parameters() { @!parameters }
    method body() { $!body }
}

class Parameter does Node {
    has $!type;
    has $!slurpy;
    has $!named;
    has $!name;
    has $!optional;
    has $!raw;
    method type() { $!type }
    method named() { $!named }
    method slurpy() { $!slurpy }
    method name() { $!name }
    method optional() { $!optional }
    method raw() { $!raw }
}

class NQPCode does Node {
    has $!body;
    method body() { $!body }
    method Str() { $!body }
}

# AST-building actions

class RakuASTActions {
    method attach($/, $node) {
        $node.set-line(HLL::Compiler.lineof($/.target, $/.from, :cache));
        make $node;
    }

    method TOP($/) {
        my @packages;
        for $<package> {
            @packages.push($_.ast);
        }
        self.attach($/, CompUnit.new(:@packages, :filename($*CURRENT-FILE)));
    }

    method package:sym<class>($/) { make $<package-def>.ast }

    method package-def($/) {
        my $name := ~$<name>;
        my @parents;
        for $<parent> {
            nqp::push(@parents, ~$_);
        }
        my @attributes;
        for $<attribute-decl> {
            @attributes.push($_.ast);
        }
        my @methods;
        for $<method-decl> {
            @methods.push($_.ast);
        }
        self.attach($/, Package.new(:type($*PKGDECL), :$name, :@parents, :@attributes, :@methods));
    }

    method attribute-decl($/) {
        my $type := ~$<type>;
        my $attr;
        if $<attribute> {
            my $name := ~$<attribute>;
            $attr := Attribute.new(:$type, :$name, :!has-accessor);
        }
        else {
            my $name := nqp::replace(~$<public-attribute>, 1, 1, '!');
            $attr := Attribute.new(:$type, :$name, :has-accessor);
        }
        %*ATTRS{$attr.name} := $attr;
        self.attach($/, $attr);
    }

    method method-decl($/) {
        my $name := ~$<name>;
        my @parameters := $<signature> ?? $<signature>.ast !! [];
        my $body := $<method-body>.ast;
        self.attach($/, Method.new(:$name, :@parameters, :$body));
    }

    method method-body($/) {
        make $<nqp-code>.ast
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
        my $slurpy := ?$<slurpy>;
        my $name := ~$<name>;
        my $optional := $named
            ?? ($<optional> eq '!' ?? 0 !! 1)
            !! ($<optional> eq '?' ?? 1 !! 0);
        my $raw := ?$<raw>;
        self.attach($/, Parameter.new(:$type, :$named, :$slurpy, :$name, :$optional, :$raw));
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
                if %*ATTRS{$name} -> $attr {
                    @chunks.push("nqp::" ~ $attr.getattr-op ~ "(\$SELF, $*PACKAGE-NAME, '$name')");
                }
                else {
                    $/.panic("No such attribute $name in $*PACKAGE-NAME");
                }
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
            elsif $<brckt> {
               @chunks.push('[' ~ $<nqp-code>.ast ~ ']');
            }
            else {
                @chunks.push(~$/);
            }
        }
        self.attach($/, NQPCode.new(:body(nqp::join("", @chunks))));
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
        my $*CURRENT-FILE := $_;
        my $*LINEPOSCACHE;
        my $source := slurp($_);
        @compunits.push(RakuASTParser.parse($source, actions => RakuASTActions).ast);
    }

    # Geneate code
    say('# Generated by tools/build/raku-ast-compiler.nqp');
    say('');
    emit-stubs(@compunits);
    say('BEGIN {');
    emit-nqp('src/Raku/ast/rakuast-prologue.nqp');
    for @compunits {
        my $*CU := $_;
        for $_.packages {
            emit-package($_);
        }
    }
    emit-nqp('src/Raku/ast/rakuast-epilogue.nqp');
    say('}');
}

# Code-gen.

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

sub emit-nqp($nqp-file) {
    say('#line 1 ' ~ $nqp-file);
    say(slurp($nqp-file));
}

sub emit-package($package) {
    my $name := $package.name;

    my @parents := $package.parents;
    for @parents || ['Any'] {
        say("    parent($name, $_);");
    }

    my %need-accessor;
    for $package.attributes -> $attr {
        my $type := $attr.type;
        my $attr-name := $attr.name;
        say("    add-attribute($name, $type, '$attr-name');");
        if $attr.has-accessor {
            %need-accessor{nqp::substr($attr-name, 2)} := $attr;
        }
    }

    for $package.methods -> $method {
        nqp::deletekey(%need-accessor, $method.name);
        emit-method($name, $method);
    }

    for sorted_keys(%need-accessor) -> $method-name {
        my $attr-node := %need-accessor{$method-name};
        my $attr-name := $attr-node.name;
        my $decl-line := $attr-node.line;
        my $op := $attr-node.getattr-op;
        say("#line ", $decl-line, " ", $*CU.filename);
        say("    add-method($name, '$method-name', [], anon sub $method-name (\$self) \{",
            " nqp::" ~ $op ~ "(nqp::decont(\$self), $name, '$attr-name')",
            " });");
    }

    say("    compose($name);");
}

sub emit-method($package, $method) {
    my str $bodystr := $method.body.Str;

    my @parameters := $method.parameters;
    my @params-in;
    my @params-desc := ["$package, '', 0, 0"];
    my @params-decont;
    for @parameters {
        my $param-name := $_.name;
        my $type := $_.type || 'Any';
        my $named := $_.named ?? ':' !! '';
        my $slurpy := $_.slurpy ?? '*' !! '';
        my $opt := $slurpy ?? '' !! ($_.optional ?? '?' !! '!');
        @params-in.push(", $named$slurpy$param-name$opt");
        @params-desc.push("$type, '$param-name', " ~ ($_.named ?? '1, ' !! '0, ') ~
            ($_.optional ?? '1' !! '0'));
        unless $_.raw {
            # If we don't mention the parameter anywhere in the code,
            # no need to decont it!
            if nqp::index($bodystr, $param-name) != -1 {
                @params-decont.push("$param-name := nqp::decont($param-name);");
            }
        }
    }
    my $params-in := nqp::join("", @params-in);
    my $params-desc := nqp::join(", ", @params-desc);

    my $name := $method.name;
    say("    add-method($package, '$name', [$params-desc], anon sub $name (\$SELF_CONT$params-in) \{");

    # If we don't use the $SELF variable anywhere in the body, don't create it
    if nqp::index($bodystr, '$SELF') != -1 {
        say("        my \$SELF := nqp::decont(\$SELF_CONT);");
    }

    for @params-decont {
        say("        $_");
    }

    say("#line " ~ $method.body.line ~ " " ~ $*CU.filename);

    # Cheekily save some chars by leaving out lines that are only spaces
    # and similar wastes of tiny amounts of text

    # If the first char in our $bodystr is whitespace, find the
    # first nowhitespace and cut everything before that off.
    my int $first-nonwhite :=
        nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $bodystr, 0)
            ?? nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, $bodystr,
                           0, nqp::chars($bodystr))
            !! -1;
    if $first-nonwhite != -1 {
        $bodystr := nqp::substr($bodystr, $first-nonwhite, nqp::chars($bodystr) - 1);
    }

    # From the end of the bodystr, remove all whitespace until we hit the very
    # last nonwhitespace.
    my int $endpos := nqp::chars($bodystr) - 1;
    my int $orig-endpos := $endpos;
    while nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $bodystr, $endpos) {
        $endpos--;
    }
    if $endpos != nqp::chars($bodystr) - 1 {
        $bodystr := nqp::substr($bodystr, 0, $endpos + 1);
    }

    # Very importantly, don't b0rk the #? magic comments,
    # they seem to need to go right at the first character in a line.
    # We must not indent them!
    if nqp::eqat($bodystr, "#?", 0) {
        say($bodystr);
    }
    else {
        say("        " ~ $bodystr);
    }

    say("    });");
}
