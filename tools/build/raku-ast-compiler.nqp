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
        | '#' \N*
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

    # Every type name a declaration may use: the classes declared here plus
    # the natives, bootstrap types and QAST types their signatures name.
    my %*KNOWN-TYPES;
    for @compunits -> $cu {
        for $cu.packages -> $package {
            %*KNOWN-TYPES{$package.name} := 1;
        }
    }
    for <Mu Any str int num Str Int Bool Code List Array Hash Scalar Signature ContainerDescriptor QAST::Node QAST::Block QAST::Op QAST::Stmts> {
        %*KNOWN-TYPES{$_} := 1;
    }

    # An unknown type name would otherwise compile to a lookup that yields
    # NQPMu. All names are checked before anything is emitted, so one run
    # reports every unknown name.
    my @*UNKNOWN-TYPES;
    for @compunits {
        my $*CU := $_;
        for $_.packages {
            check-package-types($_);
        }
    }
    if @*UNKNOWN-TYPES {
        nqp::die("Unknown types in RakuAST declarations:\n  " ~ nqp::join("\n  ", @*UNKNOWN-TYPES));
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

sub check-type-name($type, $where) {
    unless %*KNOWN-TYPES{$type} {
        nqp::push(@*UNKNOWN-TYPES, "$type in $where (" ~ $*CU.filename ~ ")");
    }
}

sub check-package-types($package) {
    my $name := $package.name;
    for $package.parents {
        check-type-name($_, "parent of $name");
    }
    for $package.attributes -> $attr {
        check-type-name($attr.type, "attribute " ~ $attr.name ~ " of $name");
    }
    for $package.methods -> $method {
        for $method.parameters {
            check-type-name($_.type || 'Any', "parameter " ~ $_.name ~ " of $name." ~ $method.name);
        }
    }
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

sub type-is-native($type) {
    $type eq 'str' || $type eq 'int' || $type eq 'num'
}

# Whether a declared type needs an object type check. Mu and Any accept
# anything, including NQP values, and a native parameter is enforced by the
# unbox when the argument is bound.
sub type-is-checked($type) {
    !($type eq 'Any' || $type eq 'Mu' || type-is-native($type))
}

# The compiler passes VM strings, integers, arrays, hashes and closures
# where user code passes Str, Int, List, Hash and Code objects, and a bare
# adverb, which is a VM integer, where user code passes a Bool. NQP cannot
# know those satisfy the type, so these checks are emitted here. A VM integer
# for a Bool becomes a Bool on entry, and NQPMu for an optional flag becomes
# the Bool type object. An omitted optional of the other five stays
# undefined, which the bodies treat as absent. Every other type goes on the
# NQP parameter itself, and NQP checks it, deconts the argument and gives an
# omitted optional the type object. NQP does not check a slurpy, so the
# elements of a typed slurpy are checked here too.
sub type-is-vm-shaped($type) {
    $type eq 'Str' || $type eq 'Int' || $type eq 'Bool' || $type eq 'Code' || $type eq 'List' || $type eq 'Hash'
}

# The NQP expression that decides whether a value satisfies a declared type,
# for the checks emitted here. For the types type-is-vm-shaped names, an
# undefined value passes only as the type object itself, or as the NQPMu
# that NQP code passes for an absent value. A required flag refuses NQPMu,
# which is also what a name NQP cannot resolve evaluates to.
sub type-check-expr($type, $value, $absent-ok = 1) {
    return "nqp::istype($value, $type)" unless type-is-vm-shaped($type);
    my $concrete :=
      $type eq 'Str'  ?? "nqp::isstr($value) || nqp::istype($value, Str)" !!
      $type eq 'Int'  ?? "nqp::isint($value) || nqp::istype($value, Int)" !!
      $type eq 'Bool' ?? "nqp::isint($value) || nqp::istype($value, Bool)" !!
      $type eq 'Code' ?? "nqp::isinvokable($value)" !!
      $type eq 'List' ?? "nqp::islist($value) || nqp::istype($value, List)" !!
                         "nqp::ishash($value) || nqp::istype($value, Hash)";
    my $absent := $absent-ok ?? "nqp::eqaddr($value, NQPMu) || " !! '';
    "(nqp::isconcrete($value) ?? ($concrete) !! ({$absent}nqp::istype($value, $type)))"
}

# The call that reports a failed check, through the hook NQP uses for the
# parameters it checks itself.
sub type-check-fail($name, $type, $value) {
    "nqp::gethllsym('nqp', 'parameter-type-check-failure')($value, $type, '$name', nqp::curcode())"
}

sub emit-method($package, $method) {
    my @parameters := $method.parameters;
    my @params-in;
    my @params-desc := ["$package, '', 0, 0"];
    my @params-decont;
    my $name := $method.name;
    for @parameters {
        my $param-name := $_.name;
        my $type := $_.type || 'Any';
        my $named := $_.named ?? ':' !! '';
        my $slurpy := $_.slurpy ?? '*' !! '';
        my $opt := $slurpy ?? '' !! ($_.optional ?? '?' !! '!');
        my $checked := type-is-checked($type);
        my $here := $checked && (type-is-vm-shaped($type) || $slurpy);
        # The type goes on the NQP parameter unless it is checked here. A
        # native one makes binding unbox the argument and refuse one that
        # cannot be unboxed.
        my $typed := type-is-native($type) || ($checked && !$here) ?? "$type " !! '';
        if $type eq 'Bool' && ($slurpy || $_.raw) {
            nqp::die("A Bool parameter cannot be slurpy or raw: $param-name of $package.$name (" ~ $*CU.filename ~ ")");
        }
        my $raw := $_.raw && $typed ?? ' is raw' !! '';
        my $default := $type eq 'Bool' && !$slurpy && $_.optional ?? ' = Bool' !! '';
        @params-in.push(", $typed$named$slurpy$param-name$opt$raw$default");
        @params-desc.push("$type, '$param-name', " ~ ($_.named ?? '1, ' !! '0, ') ~
            ($_.optional ?? '1' !! '0'));
        unless $_.raw || $typed {
            @params-decont.push("$param-name := nqp::decont($param-name);");
        }
        if $here && $slurpy {
            my $value := $_.named ?? 'nqp::decont(nqp::iterval($_))' !! 'nqp::decont($_)';
            @params-decont.push("for $param-name \{ " ~ type-check-expr($type, $value)
                ~ " || " ~ type-check-fail($param-name, $type, $value) ~ " }");
        }
        elsif $here {
            my $value := $_.raw ?? "nqp::decont($param-name)" !! $param-name;
            @params-decont.push(type-check-expr($type, $value, $type ne 'Bool' || $_.optional)
                ~ " || " ~ type-check-fail($param-name, $type, $value) ~ ";");
        }
        if $type eq 'Bool' {
            @params-decont.push("$param-name := nqp::isint($param-name) ?? (nqp::unbox_i($param-name) ?? (Bool.WHO)<True> !! (Bool.WHO)<False>) !! nqp::eqaddr($param-name, NQPMu) ?? Bool !! $param-name;");
        }
    }
    my $params-in := nqp::join("", @params-in);
    my $params-desc := nqp::join(", ", @params-desc);

    say("    add-method($package, '$name', [$params-desc], anon sub $name (\$SELF_CONT$params-in) \{");
    say("        my \$SELF := nqp::decont(\$SELF_CONT);");
    for @params-decont {
        say("        $_");
    }
    say("#line " ~ $method.body.line ~ " " ~ $*CU.filename);
    say("        " ~ $method.body);
    say("    });");
}
