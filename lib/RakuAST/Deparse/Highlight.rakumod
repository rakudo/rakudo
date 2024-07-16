use v6.e.PREVIEW;

# just needed until Raku::Actions/Raku::DEPARSE become default
use experimental :rakuast;
use nqp;
my constant RakuGrammar = nqp::gethllsym('Raku','Grammar');
my constant RakuActions = nqp::gethllsym('Raku','Actions');
my constant RakuDEPARSE = nqp::gethllsym('Raku','DEPARSE');

my constant cleaner = / <-[-]>+ $/;

# Helper subs (perhaps this should become more general)
multi sub postcircumfix:<{ }>(RakuGrammar:D $/, Str:D $key) {
    $/.hash.AT-KEY($key)
}
multi sub prefix:<~>(RakuGrammar:D $/) {
    $/.Str
}

#- RakuAST::LanguageVersion ----------------------------------------------------
# A dummy class to give a language version specification (e.g. use v6.d)
# a place in the statement list, so it can be properly deparsed later

my class RakuAST::LanguageVersion is RakuAST::Node {
    has Version $.version is built(:bind);

    method new(Version:D $version) { self.bless: :$version }

    method raku() { self.^name ~ ".new($!version.gist())" }
}

#- Actions ---------------------------------------------------------------------
my class Actions is RakuActions {
    has str $.source;  # source being parsed, type object if no comments seen
    has     $!version; # language version seen
    has     @.eol;     # indices of line endings
    has     @.soc;     # indices of start of comment on associated line endings
    has     %!seen;    # lookup hash to prevent double registrations

    # Get any version specification to be added later
    method lang-setup(Mu $/) {

        # Appear to have a language version specification, safe it for later
        with $<version> {
            self.SET-NODE-ORIGIN(
              $_,
              $!version := RakuAST::LanguageVersion.new((~$_).substr(1).Version)
            );
        }
        nextsame;
    }

    # Tweak compunit handling by inserting any language version in place
    method comp-unit(Mu $/) {

        # We appear to have a language version specification.  Add our
        # dummy object as the first element after any Doc::Blocks.  The
        # reasoning is: if there was code before any Doc::Blocks, it will
        # be inserted before the code.  In a file where there are only
        # Doc::Blocks before any code, the most logical place is after
        # those Doc::Blocks and before any code there.
        if $!version -> $version {
            my $stmt-list :=  $<statementlist>.ast;
            with $stmt-list.statements.first(!(* ~~ RakuAST::Doc::Block), :k) {
                $stmt-list.insert-doc-block($_, $version);
            }
            else {
                $stmt-list.add-doc-block($version);
            }
        }

        nextsame;
    }

    # Handle a comment in the source
    method comment:sym<#>(Mu $/) {
        my str $source = self!check-source($/);

        my uint $from = $/.from;
        unless %!seen{$from}++ {
            my uint $to  = $/.to;

            # Find the start of the comment if not at start of line
            unless !$from || $source.substr-eq("\n",$from - 1) {
                my int $start = nqp::rindex($source,"\n",--$from);
                ++$start if $start < 0;
                Nil while --$from > $start
                    && $source.substr-eq(" " | "\t",$from);

                # Consider as whole line if just whitespace before #
                ++$from if $from;
            }

            # Associate the comment with the correct line
            @!soc[@!eol.first($to, :k)] = $from;
        }
    }

    # Return the source being parsed, and vivifies structures on first call
    method !check-source(Mu $/) {
        unless $!source {
            my @eol = ($!source = $/.orig).indices("\n");
            @eol.push($!source.chars) unless @eol;
            @eol.unshift(-1);
            @!eol := @eol.List;
        }
        $!source
    }

    # Mark empty lines as being comments for a better deparsing experience
    method commentize-empty-lines() {
        for ^@!eol.end -> uint $i {
            my int $index = @!eol[$i];
            @!soc[$i + 1] = $index if @!eol[$i + 1] == $index + 1;
        }
    }

    # Return any comment at the given line number and remove it, unless
    # it is indicated it should be kept
    method comment(uint $index, :$keep --> Str:D) {
        with @!soc[$index] -> $soc {
            @!soc[$index] := Any unless $keep;
            $!source.substr($soc, @!eol[$index] - $soc) but True
        }
        else {
            Nil
        }
    }

    # Return any *full line comment* at the given line number and remove it,
    # unless it is indicated it should be kept.  If there is a comment at
    # that line, but not a full line comment, it will be ignored
    method whole-line-comment(uint $index, :$keep) {
        with @!eol[$index - 1] -> $previous {
            if @!eol[$index] == $previous + 1 {
                return "" but True;
            }
            orwith @!soc[$index] -> $soc {
                # is it a whole line?
                if $soc == $previous + 1 {
                    @!soc[$index] := Any unless $keep;
                    return $!source.substr($soc, @!eol[$index] - $soc);
                }
                elsif $soc == $previous {
                    @!soc[$index] := Any unless $keep;
                    return "" but True;
                }
            }
        }
        Nil
    }

    # Return any full line comments *before* the given line number and
    # remove them, unless it is indicated they should be kept.
    method comments-preceding(
      uint $index is copy,
      :$keep,
      :$partial  # also add any partial comment
    --> Str:D) {
        my str @parts;
        while --$index
          && self.whole-line-comment($index, :$keep) -> $comment {
            @parts.unshift($comment)
        }

        @parts.unshift(self.comment($index) // "") if $partial;

        @parts ?? @parts.join("\n") but True !! Nil
    }

    # Return any full line comments *after* the given line number and
    # remove them, unless it is indicated they should be kept.
    method comments-following(uint $index is copy, :$keep --> Str:D) {
        my str @parts;
        while ++$index
          && self.whole-line-comment($index, :$keep) -> $comment {
            @parts.push($comment)
        }

        @parts ?? @parts.join("\n") !! Nil
    }
}

#- SafeActions -----------------------------------------------------------------

my class SafeActions is Actions {

    my class X::NotAllowedHighlighting {
        has $.what;
        method message() {
            "$!what not allowed in safe syntax highlighting"
        }
    }

    method statement-prefix:sym<BEGIN>(Mu $/ is raw) {
        $/.typed-panic("X::NotAllowedHighlighting", :what("BEGIN phaser"));
    }

    method type-declarator:sym<constant>(Mu $/ is raw) {
        $/.typed-panic("X::NotAllowedHighlighting", :what("constant definition"));
    }

    method statement-control:sym<use>(Mu $/) {
        my str $name = ~<$module-name>;
        if RakuAST::Pragma.IS-PRAGMA($/.pragma2str($name)) {
            ($name eq 'nqp' | 'MONKEY' | 'MONKEY-GUTS')
              ?? $/.typed-panic(
                   "X::NotAllowedHighlighting", :what("use $name")
                 )
              !! (nextsame);
        }
        else {
            $/.typed-panic(
              "X::NotAllowedHighlighting", :what("module loading")
            );
        }
    }
}

#- Deparse----------------------------------------------------------------------
my class Deparse is RakuDEPARSE {
    has $.actions;

    multi method deparse(RakuAST::StatementList:D $ast --> Str:D) {
        my $actions := $!actions;

        if $ast.statements -> @statements {
            my str @outer;
            my str $spaces = $*INDENT;
            my $last-statement := @statements.first({
                !($_ ~~ RakuAST::Doc::Block)
            }, :end) // @statements.tail;

            my $code;
            my $*DELIMITER;
            for @statements -> $statement {
                my str @parts;

                # Determine the line numbers this statement is at
                my $origin := $statement.origin;
                my $source := $origin.source;
                my $first-line := $source.original-line($origin.from);
                my $last-line  := $source.original-line($origin.to - 1);

                # Standard deparsing
                $*DELIMITER = $statement === $last-statement
                  ?? $.last-statement
                  !! $.end-statement;
                my $deparsed := self.deparse($statement);
                $deparsed := $deparsed.chop(2) if $deparsed.ends-with("};\n");

                @parts.push($spaces);
                @parts.push($deparsed);

               # Add any comment at last line
                if $deparsed.ends-with('}') {
                    if $actions.comments-preceding(
                      $last-line + 1, :partial
                    ) -> $comment {
                        @parts.push(self.hsyn('comment',$comment));
                    }
                    @parts.push("\n");
                }

                # Split what we produced in lines so we can add any comments
                @parts = @parts.join.split("\n");

                # RakuDoc never has comments
                unless $statement ~~ RakuAST::Doc::Block {

                    # Add any comment on first line
                    if $actions.comment($first-line) -> $comment {
                        @parts[0] ~= self.hsyn('comment',$comment);
                    }

                    # Add any full line comments preceding the first line
                    if $actions.comments-preceding($first-line) -> $preceding {
                        @parts.unshift(self.hsyn('comment',$preceding));
                    }

                    # Add any full line comments after this line
                    if $last-line > $first-line
                      && $actions.comments-following($first-line) -> $following {
                        @parts.pop if (my $empty := !@parts.tail);
                        @parts.push(self.hsyn('comment', $following));
                        @parts.push("") if $empty;
                    }
                }

                @outer.push(@parts.join("\n"));
            }

            @outer.join
        }

        else {
            ''
        }
    }

    # Adds deparsing logic for our special language version handling
    multi method deparse(RakuAST::LanguageVersion:D $ast --> Str:D) {
        self.hsyn('use-use', self.xsyn('use', 'use'))
         ~ ' '
         ~ self.hsyn('version', $ast.version.gist)
         ~ ";\n"
    }
}

#- convenience subsets ---------------------------------------------------------

subset comment            of Str:D where * eq 'comment';
subset invocant           of Str:D where * eq 'invocant';
subset label              of Str:D where * eq 'label';
subset literal            of Str:D where * eq 'literal';
subset param              of Str:D where * eq 'param';
subset stub               of Str:D where * eq 'stub';
subset version            of Str:D where * eq 'version';

subset adverb-qs     of Str:D where *.starts-with("adverb-q-");
subset arrows        of Str:D where *.starts-with("arrow-");
subset blocks        of Str:D where *.starts-with("block-");
subset captures      of Str:D where *.starts-with("capture-");
subset constraints   of Str:D where *.starts-with("constraint-");
subset cores         of Str:D where *.starts-with("core-");
subset docs          of Str:D where *.starts-with("doc-");
subset infixes       of Str:D where *.starts-with("infix-");
subset markups       of Str:D where *.starts-with("markup-");
subset metas         of Str:D where *.starts-with("meta-");
subset modifiers     of Str:D where *.starts-with("modifier-");
subset multis        of Str:D where *.starts-with("multi-");
subset nameds        of Str:D where *.starts-with("named-");
subset nqps          of Str:D where *.starts-with("nqp-");
subset packages      of Str:D where *.starts-with("package-");
subset phasers       of Str:D where *.starts-with("phaser-");
subset postfixes     of Str:D where *.starts-with("postfix-");
subset pragmas       of Str:D where *.starts-with("pragma-");
subset prefixes      of Str:D where *.starts-with("prefix-");
subset quote-langs   of Str:D where *.starts-with("quote-lang-");
subset rakudocs      of Str:D where *.starts-with("rakudoc-");
subset routines      of Str:D where *.starts-with("routine-");
subset scopes        of Str:D where *.starts-with("scope-");
subset smileys       of Str:D where *.starts-with("smiley-");
subset stmt-prefixes of Str:D where *.starts-with("stmt-prefix-");
subset systems       of Str:D where *.starts-with("system-");
subset ternaries     of Str:D where *.starts-with("ternary-");
subset trait-ises    of Str:D where *.starts-with("trait-is-");
subset traitmods     of Str:D where *.starts-with("traitmod-");
subset typers        of Str:D where *.starts-with("typer-");
subset types         of Str:D where *.starts-with("type-");
subset uses          of Str:D where *.starts-with("use-");
subset vars          of Str:D where *.starts-with("var-");

#- highlight - basic role interface --------------------------------------------

my proto sub highlight(|) is export {*}
my multi sub highlight(Str:D $source, *@roles is copy, :$unsafe --> Str:D) {
    my $actions := nqp::create($unsafe ?? Actions !! SafeActions);
    my $ast     := $source.AST(:$actions);

    # Post process empty lines
    $actions.commentize-empty-lines;

    # Set up initial deparser
    my $deparser := Deparse.new(:$actions);

    # Make sure we actually have roles to mix in and mix them in
    for @roles {
        if $_ ~~ Str {
            my $class := "RakuAST::Deparse::Highlight::$_";
            $_ = "use experimental :rakuast; use $class; $class".EVAL;
        }
        $deparser.^mixin($_);
    }

    # Not sure why this is needed, but without it the deparse fails with
    # a "getlex: outer index out of range" error message
    my $*INDENT = "";

    # Do the actual deparse: if nothing was returned, it is just
    # comments, so highlight the original source as a comment
    $ast.DEPARSE($deparser) || $deparser.hsyn('comment', $source)
}

#- highlight - color based interface -------------------------------------------

my constant %default = <
  adverb-q-      red
  arrow-         yellow
  block-         yellow
  capture-       cyan
  comment        blue
  constraint-    magenta
  core-          yellow
  doc-           blue
  infix-         yellow
  invocant       cyan
  label          yellow
  literal        red
  markup-        magenta
  meta-          yellow
  modifier-      yellow
  multi-         yellow
  named-         yellow
  nqp-           none
  package-       yellow
  param          cyan
  phaser-        magenta
  postfix-       yellow
  pragma-        magenta
  prefix-        yellow
  quote-lang-    red
  rakudoc-       yellow
  routine-       yellow
  scope-         magenta
  smiley-        red
  stmt-prefix-   magenta
  stub           none
  system-        none
  ternary-       yellow
  trait-is-      magenta
  traitmod-      magenta
  type-          green
  typer-         yellow
  use-           magenta
  var-           cyan
  version        red
>;

my multi sub highlight(Str:D $source, %sub-mapper, *%_) {
    my $unsafe := %_<unsafe>:delete;
    highlight($source, %sub-mapper, color-mapper(%_), :$unsafe)
}

my multi sub highlight(Str:D $source, %sub-mapper, %color-mapper, :$unsafe) {

    my role ColorMapper {
        method hsyn(Str:D $key, Str:D $content) {
            if %color-mapper{$key}
              // %color-mapper{$key.subst(cleaner)} -> $color {
                if %sub-mapper{$color} -> &highlighter {
                    highlighter $content
                }
                else {
                    $content
                }
            }
            else {
               $content
            }
        }
    }

    highlight($source, ColorMapper, :$unsafe)
}

#- color-mapper ----------------------------------------------------------------

my proto sub color-mapper(|) is export {*}
my multi sub color-mapper() is default { %default }
my multi sub color-mapper(*%_) { color-mapper(%_) }
my multi sub color-mapper(%_) {
    my $color-mapper := %default.Hash;
    $color-mapper{.key} = .value for %_;
    $color-mapper
}

#- hsyn-key2color --------------------------------------------------------------

my proto sub hsyn-key2color(|) is export {*}
my multi sub hsyn-key2color(Str:D $key) {
    %default{$key} // %default{$key.subst(cleaner)}
}
my multi sub hsyn-key2color(Str:D $key, %color-mapper) {
    %color-mapper{$key} // %color-mapper{$key.subst(cleaner)}
}

#-------------------------------------------------------------------------------
=begin rakudoc

=head1 NAME

RakuAST::Deparse::Highlight - provide Raku-based syntax highlighting

=head1 SYNOPSIS

=begin code :lang<raku>

use RakuAST::Deparse::Highlight;

say highlight("say 'hello world'", "HTML");
# <span style="color:yellow;">say</span>
# <span style="color:red;">"hello world"</span>

my %color2sub = red => sub ($t) { "==$t==" }
say highlight("say 'hello world'", %color2sub);
# say =="hello world"==

role LiteralAsterisked {
    multi method hsyn(literal, Str:D $t) { "**$t**" }
}
say highlight("say 'hello world'", LiteralAsterisked);
# say **"hello world"**

=end code

=head1 DESCRIPTION

The C<RakuAST::Deparse::Highlight> module exports a number of subroutines
to provide easily customizable Raku-based syntax highlighting, based on
parsing source code and deparsing of the created RakuAST objects.

It basically provides 3 modes of syntax highlighting:

=head2 using pre-installed modules

Highlighting provided by modules named C<RakuAST::Deparse::Highlight::xxx>
can be activated by specifying C<"xxx">.  There is no need to pre-load that
module, it will be automatically loaded when necessary.

=head2 using default color mapping

By specifying a hash that maps color names to subroutines to be called to
perform the indicated highlighting.  Which can be extended to provide
custom syntax object type to color mapping as well.

=head2 using a custom role

All syntax highlighting is based on injecting customized methods named
"hsyn" into the class doing the deparsing.  You can supply one or more
roles that have these methods in them for full control of syntax highlighting.

To ease this way of using syntax highlight also exports a large number of C<subsets> that can be used to handle
specific situations in syntax highlighting by creating C<multi> C<hsyn>
methods that use such a subset as the first argument.

=head1 THEORY OF OPERATION

Syntax highlighting is based on first parsing the given source code to RakuAST
objects using the Raku grammar with a dedicated set of actions that B<also>
process any inline comments.

The resulting RakuAST nodes are then deparsed with a subclass of the
basic RakuAST deparsing class that takes the additional comments
information and provides additional deparsing logic for the inline
comments.

The disadvantage of this approach, is that the format of the code may
change, because deparsing has no knowledge of the original source code.

The advantages are:

=item the code is sure to be syntactically correct

If the code doesn't parse, it cannot be highlighted.  This could be considered
a disadvantage for use in editors and IDE's.  But that's not what this syntax
highlighting is intended for.

=item much more information available

Because the syntax highlighting is based on a Abstract Syntax Tree, much
more information is available to adapt highlighting to (instead of just on
string matches.  This can e.g. be used in tooltips that have extra information
and/or links to documentation in HTML.

=item ready for localization

The syntax highlighting will also work on any localized version of the Raku
Programming Language B<because> it is based on RakuAST, which is ignorant of
the actual localization being used in the source code.

=head1 SUBROUTINES

=head2 highlight

The first positional argument to C<highlight> is B<always> the Raku source
code that needs to be highlighted.

=begin code :lang<raku>

say highlight("file".IO.slurp, "HTML", :unsafe);

=end code

By default, compile time actions (such as C<BEGIN> blocks, C<constant>
definitions and C<use> statements that would actually load a module)
are B<not> allowed because they (could potentially) execute malignant
code.  If you are sure of your environment and I<would> like to allow
these operations, you can always pass the C<:unsafe> named argument.

The advantage of allowing compile time actions, is that it will allow
to also properly highlight custom operators, traits or any other
syntax changes that can be implemented from module space.

=begin code :lang<raku>

# using predefined classes
say highlight("say 'hello world'", "HTML");
# <span style="color:yellow;">say</span>
# <span style="color:red;">"hello world"</span>

=end code

The simplest way to use C<highlight> is to specify the name of the module
that you would like to use for highlighting.  This module should exist
in the C<RakuAST::Deparse::Highlight::> namespace.  Simple support for
C<HTML> is provided by default.

=begin code :lang<raku>

# using default color mapping
my %color2handler = red => -> $t { "==$t==" }
say highlight("say 'hello world'", %color2handler);
# say =="hello world"==

=end code

A slightly more complex way to use highlighting, is to supply a C<Map>
that maps color names to handlers that will be called if a part of the
source needs to be highlighted in that color.  The handler will be given
the source to be highlighted: the handler is expected to return the properly
highlighted text.

If there is no handler for a given color name, then the original text will be
produced.

The mapping of C<hsyn> keys to color names is (by default) based on the
syntax highlighting provided by C<vim>.  The following color names are
recognized: C<black>, C<blue>, C<cyan>, C<green>, C<magenta>, C<none>,
C<red>, C<yellow> and C<white>.

=begin code :lang<raku>

# show comments in green, and BEGIN in blue
say highlight "say 'hello world'", %color2handler,
  comments     => "green",
  phaser-BEGIN => "blue"
);

=end code

If the C<highlight> subroutine is called in this matter, it's also possible
to specify tweaks in the mapping of C<hsyn> keys to color names by specifying
one or more pairs with additional mappings or tweaks.

=begin code :lang<raku>

# show comments in green, and BEGIN in blue from pre-tweaked hash
my constant %color-mapper = color-mapper(
  comments     => "green",
  phaser-BEGIN => "blue",
);
say highlight "say 'hello world'", %color2handler, %color-mapper);

=end code

Because creating the lookup hash with a given set of tweaks is relatively
expensive, it's also possible to tweak this beforehand with the C<color-mapper>
subroutine, and specify the resulting hash as the second positional argument.

=begin code :lang<raku>

# using custom roles
role LiteralAsterisked {
    multi method hsyn(literal, Str:D $t) { "**$t**" }
}
say highlight("say 'hello world'", LiteralAsterisked);
# say **"hello world"**

=end code

To give complete control of the highlighting process, it is also possible
to provide one or more roles as additional positional arguments to
C<highlight>.  This allows one to create one or more C<hsyn> method
candidates, each of which has complete freedom to do what is necessary.

While deparsing a RakuAST tree of objects, the deparser will call a
C<hsyn> method with a string identifier as the first argument, and the
actual string to be highlighted as the second argument.

By default, the C<hsyn> method returns the second argument unchanged.
By mixing in one or more roles (as additional positional arguments in
the call to the C<highlight> subroutine), on can customize the highlighting
behaviour to your liking.

=begin code :lang<raku>

role LiteralAsterisked {
    multi method hsyn(literal, Str:D $t) { "**$t**" }
}
say highlight("say 'hello world'", LiteralAsterisked);
# say **"hello world"**

=end code

To make it easier to create C<hsyn> multi methods candidates for customization,
the following subsets are provided by this module to be used as the first
argument in any custom C<hsyn> method signature (in alphabetical order).

Note that the subsets of which the name ends with C<s>, are in fact a
collection of possible values for the first argument, so further selection
is potentially possible.

Note that the second (content) argument contains the localized version of the
information.  So e.g. a C<hsyn>  method that uses the C<invocant> subset, would
receive "selbst" (as opposed to "self") as the second argument in a German
localization.

=item adverb-qs

Adverbs on quoting constructs, e.g. C<to> in C<q:to/Foo/>.

=item arrows

Syntax elements C«->» and C«-->».

=item blocks

Statements that take blocks, e.g. C<if>, C<elsif>, C<with> etc.

=item captures

Capture variables, such as C<$0> and C<$<foo>>.

=item comment

An inline comment: anything after a C<#> on a line, including its preceding
whitespace.

=item constraints

A constraint indication such as C<where> in a C<subset> specfication.

=item cores

Any core subroutine / method, such as C<say>, C<put>, C<take>, etc.

=item docs

Any declarator documentation, aka the text after C<#|> or C<#=>.

=item infixes

Any infix operator, such as C<+>, C<->, C<eq>, etc.

=item invocant

The invocant, aka C<self>.

=item label

Any label of a loop structure, including the C<:>.

=item literal

Any literal value, such as C<42>, C<"foo">, etc.

=item markups

The letters in markup in rakudoc.

=item metas

Meta operators, such as C<R> in C<Req>, C<!> in C<!(elem)>, C<Z> in C<Z,>, etc.

=item modifiers

Any conditional / loop modifier, such as postfix C<if>, C<while>, C<for> in
constructs such as C<.say for ^10>.

=item multis

Dispatch scope indicator to C<sub> and C<method>: C<only>, C<proto>,
C<multi> or C<anon>.

=item nameds

The name part of named arguments.

=item nqps

Any C<nqp::> function call, e.g. C<nqp::say>.

=item packages

Package declarators such as C<class>, C<grammar>, C<module>, etc.

=item param

Any parameter in a signature.

=item phasers

Phasers such as C<BEGIN>, C<CATCH>, C<ENTER>, C<LEAVE>, etc.

=item pragmas

The name part of pragmas such as C<use nqp>, C<no precompilation>, etc.

=item postfixes

Any postfix operator, such as C<++>, C<-->, etc.

=item prefixes

Any prefix operator, such as C<+>, C<->, C<so>, etc.

=item rakudocs

Any RakuDoc element.

=item routines

Indicators of named code blocks, such as C<sub>, C<method>, C<token>, etc.

=item quote-langs

Quoting language indicators, such as C<Q>, C<q>, C<qqx>, etc.

=item scopes

Scope indicator: C<my>, C<our>, C<state>, C<has>.

=item smileys

Type smiley indicator: C<D>, C<U>.

=item stmt-prefixes

Statement prefix, such as C<do>, C<hyper>, C<lazy>, etc.

=item stub

A code stub: C<...>, C<!!!>, C<???>.

=item systems

Methods that have a special meaning in Raku, such as C<TWEAK>, C<BUILD>,
C<DESTROY>, etc.

=item ternaries

The ternary operator: C<??> and C<!!>.

=item traitmods

Types of traitmods: C<is>, C<does>, C<returns>, C<handles>, etc.

=item types

Any type specification, such as C<Int>, C<Str>.

=item typers

Statements that creat types, such as S<subset> and C<enum>.

=item uses

Use-like statements, such as C<use>, C<no>, C<need>, C<require>.

=item vars

Any variable.

=item version

Any version literal.

=head2 color-mapper

=begin code :lang<raku>

# the default mapping logic
my %default = color-mapper;

# tweaked from named args
my %tweaked = color-mapper(comment => "green");

# tweaked from hash
my %tweaks = comment => "green", literal => "cyan";
my %tweaked = color-mapper(%tweaks);

=end code

The C<color-mapper> subroutine either returns the default color mapper, or
a tweaked version of the color mapper.  This can than be used as the third
positional argument to C<highlight>, or as the second positional argument
to C<hsyn-key2color>.

Tweaking can be indicated by any number of named arguments, or a hash
containing the tweaks.

=head2 hsyn-key2color

=begin code :lang<raku>

say hsyn-key2color("phaser-BEGIN");            # magenta

my constant %tweaked = color-mapper(phaser-BEGIN => "cyan");
say hsyn-key2color("phaser-BEGIN", %tweaked);  # cyan

=end code

The C<hsyn-key2color> subroutine converts a given C<hsyn> key to the
associated color.  If the second argument is omitted, then the default
C<hsyn> key to color mapping will be assumed.

=head1 STATUS

This is alpha quality code: it's intended to be used in rendering of the Raku
documentation source code examples, will no doubt expose problems in this logic
and/or the general deparsing logic.

=end rakudoc

# vim: expandtab shiftwidth=4
