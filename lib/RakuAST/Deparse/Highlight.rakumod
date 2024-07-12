use v6.e.PREVIEW;

# just needed until Raku::Actions/Raku::DEPARSE become default
use experimental :rakuast;
use nqp;
my constant RakuGrammar = nqp::gethllsym('Raku','Grammar');
my constant RakuActions = nqp::gethllsym('Raku','Actions');
my constant RakuDEPARSE = nqp::gethllsym('Raku','DEPARSE');

# Helper subs< perhaps this should become more general
multi sub postcircumfix:<{ }>(RakuGrammar:D $/, Str:D $key) {
    $/.hash.AT-KEY($key)
}
multi sub prefix:<~>(RakuGrammar:D $/) {
    $/.Str
}

#- Actions ---------------------------------------------------------------------
my class Actions is RakuActions {
    has str $.source;  # source being parsed, type object if no comments seen
    has     @.eol;     # indices of line endings
    has     @.soc;     # indices of start of comment on associated line endings
    has     %!seen;    # lookup hash to prevent double registrations

    # Return the source being parsed, and vivifies structures on first call
    method !check-source(Mu $/) {
        unless $!source {
            my @eol = ($!source = $/.orig).indices("\n");
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
        my int $previous = @!eol[$index - 1];
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
        RakuAST::Pragma.IS-PRAGMA($/.pragma2str(~$<module-name>))
          ?? (nextsame)
          !! $/.typed-panic("X::NotAllowedHighlighting", :what("module loading"));
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

                # Add any comment on first line
                if $actions.comment($first-line) -> $comment {
                    @parts[0] ~= self.hsyn('comment',$comment);
                }

                # Add any full line comments preceding the first line
                if $actions.comments-preceding($first-line) -> $preceding {
                    @parts.unshift(self.hsyn('comment', $preceding));
                }

                # Add any full line comments after this line
                if $last-line > $first-line
                  && $actions.comments-following($first-line) -> $following {
                    @parts.pop if (my $empty := !@parts.tail);
                    @parts.push(self.hsyn('comment', $following));
                    @parts.push("") if $empty;
                }

                @outer.push(@parts.join("\n"));
            }

            @outer.join
        }

        else {
            ''
        }
    }
}

#- convenience subsets ---------------------------------------------------------

subset comment            of Str:D where * eq 'comment';
subset invocant           of Str:D where * eq 'invocant';
subset label              of Str:D where * eq 'label';
subset literal            of Str:D where * eq 'literal';
subset param              of Str:D where * eq 'param';
subset stub               of Str:D where * eq 'stub';
subset ternary            of Str:D where * eq 'ternary';
subset type               of Str:D where * eq 'type';

subset adverb-qs     of Str:D where *.starts-with("adverb-q-");
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
subset rakudocs      of Str:D where *.starts-with("rakudoc-");
subset routines      of Str:D where *.starts-with("routine-");
subset quote-langs   of Str:D where *.starts-with("quote-lang-");
subset scopes        of Str:D where *.starts-with("scope-");
subset stmt-prefixes of Str:D where *.starts-with("stmt-prefix-");
subset systems       of Str:D where *.starts-with("system-");
subset trait-ises    of Str:D where *.starts-with("trait-is-");
subset traitmods     of Str:D where *.starts-with("traitmod-");
subset typers        of Str:D where *.starts-with("typer-");
subset uses          of Str:D where *.starts-with("use-");
subset vars          of Str:D where *.starts-with("var-");

#-------------------------------------------------------------------------------
# The external interface

my sub highlight(str $source, *@roles is copy, :$unsafe --> Str:D) is export {
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

=begin rakudoc

=head1 NAME

RakuAST::Deparse::Highlight - provide Raku-based syntax highlighting

=head1 SYNOPSIS

=begin code :lang<raku>

use RakuAST::Deparse::Highlight;

say highlight("say 'hello world'", "HTML");
# <span class="raku-core-say">say</span>
# <span class="raku-literal">"hello world"</span>

role LiteralAsterisked {
    multi method hsyn(literal, Str:D $t) { "**$t**" }
}
say highlight("say 'hello world'", LiteralAsterisked);
# say **"hello world"**

=end code

=head1 DESCRIPTION

The C<RakuAST::Deparse::Highlight> module exports a single subroutine
C<highlight> which provides Raku-based syntax highlighting.

It also exports a large number of C<subsets> that can be used to handle
specific situations in syntax highlighting by creating C<multi> methods
that use such a subset as the first argument.

=head1 SUBROUTINES

=head2 highlight

=begin code :lang<raku>

say highlight("say 'hello world'", "HTML");
# <span class="raku-core-say">say</span>
# <span class="raku-literal">"hello world"</span>

role LiteralAsterisked {
    multi method hsyn(literal, Str:D $t) { "**$t**" }
}
say highlight("say 'hello world'", LiteralAsterisked);
# say **"hello world"**

=end code

The C<highlight> subroutine takes 2 or more positional arguments: the
first positional argument is the Raku source code for which to provide
syntax highlighting.

The other arguments indicate the type of highlighting wanted.  They
can either be a string (e.g. "HTML", which will be converted to the
module name C<RakuAST::Deparse::Highlight::HTML> and as such will
attempt to load a module by that name).  Or it can be one or more
custom C<role>s that should be mixed in into the standard deparsing
logic.

=begin code :lang<raku>

say highlight("file".IO.slurp, "HTML", :unsafe);

=end code

By default, compile time actions (such as C<BEGIN> blocks, C<constant>
definitions and C<use> statements that would actually load a module)
are B<not> allowed because they (could potentially) execute malignant
code.  If you are sure of your environment and I<would> like to allow
these operations, you can pass the C<:unsafe> named argument.

=head1 CONVENIENCE SUBSETS

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

=head2 adverb-qs

Adverbs on quoting constructs, e.g. C<to> in C<q:to/Foo/>.

=head2 blocks

Statements that take blocks, e.g. C<if>, C<elsif>, C<with> etc.

=head2 captures

Capture variables, such as C<$0> and C<$<foo>>.

=head2 comment

An inline comment: anything after a C<#> on a line, including its preceding
whitespace.

=head2 constraints

A constraint indication such as C<where> in a C<subset> specfication.

=head2 cores

Any core subroutine / method, such as C<say>, C<put>, C<take>, etc.

=head2 docs

Any declarator documentation, aka the text after C<#|> or C<#=>.

=head2 infixes

Any infix operator, such as C<+>, C<->, C<eq>, etc.

=head2 invocant

The invocant, aka C<self>.

=head2 label

Any label of a loop structure, including the C<:>.

=head2 literal

Any literal value, such as C<42>, C<"foo">, etc.

=head2 markups

The letters in markup in rakudoc.

=head2 metas

Meta operators, such as C<R> in C<Req>, C<!> in C<!(elem)>, C<Z> in C<Z,>, etc.

=head2 modifiers

Any conditional / loop modifier, such as postfix C<if>, C<while>, C<for> in
constructs such as C<.say for ^10>.

=head2 multis

Dispatch scope indicator to C<sub> and C<method>: C<only>, C<proto>,
C<multi> or C<anon>.

=head2 nameds

The name part of named arguments.

=head2 nqps

Any C<nqp::> function call, e.g. C<nqp::say>.

=head2 packages

Package declarators such as C<class>, C<grammar>, C<module>, etc.

=head2 param

Any parameter in a signature.

=head2 phasers

Phasers such as C<BEGIN>, C<CATCH>, C<ENTER>, C<LEAVE>, etc.

=head2 pragmas

The name part of pragmas such as C<use nqp>, C<no precompilation>, etc.

=head2 postfixes

Any postfix operator, such as C<++>, C<-->, etc.

=head2 prefixes

Any prefix operator, such as C<+>, C<->, C<so>, etc.

=head2 rakudocs

Any RakuDoc element.

=head2 routines

Indicators of named code blocks, such as C<sub>, C<method>, C<token>, etc.

=head2 quote-langs

Quoting language indicators, such as C<Q>, C<q>, C<qqx>, etc.

=head2 scopes

Scope indicator: C<my>, C<our>, C<state>, C<has>.

=head2 stmt-prefixes

Statement prefix, such as C<do>, C<hyper>, C<lazy>, etc.

=head2 stub

A code stub: C<...>, C<!!!>, C<???>.

=head2 systems

Methods that have a special meaning in Raku, such as C<TWEAK>, C<BUILD>,
C<DESTROY>, etc.

=head2 ternary

The ternary operator: C<??> and C<!!>.

=head2 traitmods

Types of traitmods: C<is>, C<does>, C<returns>, C<handles>, etc.

=head2 type

Any type specification, such as C<Int>, C<Str>.

=head2 typers

Statements that creat types, such as S<subset> and C<enum>.

=head2 uses

Use-like statements, such as C<use>, C<no>, C<need>, C<require>.

=head2 vars

Any variable.

=head1 THEORY OF OPERATION

Syntax highlighting us based on first parsing the given code to RakuAST
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

=head1 STATUS

This is alpha quality code: it's intended to be used in rendering of the Raku
documentation source code examples, will no doubt expose problems in this logic
and/or the general deparsing logic.

=end rakudoc

# vim: expandtab shiftwidth=4
