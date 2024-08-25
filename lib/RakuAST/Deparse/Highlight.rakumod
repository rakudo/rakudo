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
    has     $.finish;  # char pos of =finish, if any
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

    # Catch any =finish, for later processing
    method doc-block:sym<finish>(Mu $/) {
        $!finish := $/.from;
        nextsame;
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
        my str $name = ~$<module-name>;
        if RakuAST::Pragma.IS-PRAGMA($name) {
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
        self.hsyn('pragma-use', self.xsyn('use', 'use'))
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
subset regexes       of Str:D where *.starts-with("regex-");
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
my multi sub highlight(
  Str:D  $source is copy,
        *@roles  is copy,
        :%allow,
        :$unsafe
--> Str:D) {
    my $class := $unsafe ?? Actions !! SafeActions;
    my $actions;

    # Remove any allowed RakuDoc markup
    my @substs;
    if %allow {
        my %seen;

        $source = RakuAST::Doc::Paragraph.from-string($source).atoms.map({

            # Nothing to do
            if nqp::istype($_,Str) {
                $_
            }

            # Need to handle this markup
            elsif %allow{.letter} {
                my $atoms := .atoms.join;

                # Not yet seen, so find the locations
                unless %seen{$atoms} {
                    %seen{$atoms} := 1;

                    my @indices := $source.indices($atoms);
                    my $replacements;

                    # More than one occurrence, find out which ones we need
                    # to actually substitute later
                    if @indices > 1 {
                        my $prefix := .letter ~ '<';
                        for ^@indices {
                            $replacements.push($_ + 1)
                              if $source.substr-eq($prefix, @indices[$_] - 2);
                        }
                        $replacements := $replacements.List;
                    }

                    # Save AST for later substitution work
                    @substs.push: Pair.new: $_, $replacements;
                }

                $atoms
            }

            # Markup that should not be handled
            else {
                .Str
            }
        }).join;
    }

    # Helper sub to perform an actual compilation
    my sub compile() {
        CATCH { return .Failure }
        $actions := nqp::create($class);
        quietly $source.AST(:$actions)
    }

    my int $skip = 1;
    $source = "no strict;\n$source\n;";

    my $ast := compile;
    while nqp::istype($ast,Failure) {
        my $exception := $ast.exception;
        if nqp::istype($exception,X::Inheritance::UnknownParent) {
            $source = "my class $exception.parent() \{ }\n" ~ $source;
            ++$skip;
            $ast := compile;
        }
        elsif nqp::istype($exception,X::Undeclared::Symbols) {
            if $exception.unk_types.keys -> @classes {
                $source = @classes.map({ "my class $_ \{ }\n" }).join ~ $source;
                $skip += @classes;
                $ast := compile;
            }
            elsif $exception.unk_routines.keys -> @subs {
                $source = @subs.map({ "my sub $_\(|) \{ }\n" }).join ~ $source;
                $skip += @subs;
                $ast := compile;
            }
            else {
                return $ast;
            }
        }
        else {
            return $ast;
        }
    }

    # Post process empty lines
    $actions.commentize-empty-lines;

    # Set up initial deparser
    my $deparser := Deparse.new(:$actions);

    # If there are no roles, highlight as text if someone is watching, or
    # as debug information if output is being piped somewhere else
    @roles.push($*OUT.t ?? "Text" !! "DEBUG") unless @roles;

    # Make sure we actually have roles to mix in and mix them in
    for @roles {
        if $_ ~~ Str {
            my $class  := "RakuAST::Deparse::Highlight::$_";
            my $lookup := ::($class);
            $_ = $lookup ~~ Failure
              ?? "use experimental :rakuast; use $class; $class".EVAL
              !! $lookup
        }
        $deparser.^mixin($_);
    }

    # Not sure why this is needed, but without it the deparse fails with
    # a "getlex: outer index out of range" error message
    my $*INDENT = "";

    # Any text from =finish onward
    my str $finish;
    with $actions.finish {
        my ($before, $after) = $source.substr($_).split("\n", 2);
        $finish = $deparser.hsyn('rakudoc-directive', $before)
          ~ "\n"
          ~ $deparser.hsyn('rakudoc-content', $after)
          ~ "\n";
    }

    # Do the actual deparse
    my $highlighted = do if $ast.DEPARSE($deparser) -> $deparsed {
        ($deparsed ~ $finish).lines(:!chomp).skip($skip).head(*-1).join.chomp
    }

    # Nothing deparsed, but we have a =finish
    elsif $finish {
        $deparser.hsyn('comment', $source.substr(0, $actions.finish)) ~ $finish
    }

    # Nothing deparsed, and no =finish either, assume all comment
    else {
        $deparser.hsyn('comment', $source) ~ "\n"
    }

    # Put back any substitutions caused by allowable markup
    for @substs -> (:key($ast), :value($occurrences)) {
        my $before := $ast.atoms;
        my $after  := %allow{$ast.letter}($ast);

        # Multiple occurrences found, only subst the correct ones,
        # last ones first to prevent confusion
        if $occurrences {
            for $occurrences.reverse {
                $highlighted .= subst: $before, $after, :th($_);
            }
        }

        # Only one occurrence, go for it!
        else {
            $highlighted .= subst: $before, $after
        }
    }

    $highlighted
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
  pragma-        green
  prefix-        yellow
  quote-lang-    red
  rakudoc-           yellow
  rakudoc-code       magenta
  rakudoc-config     red
  rakudoc-content    magenta
  rakudoc-divider    blue
  rakudoc-directive  green
  rakudoc-id         cyan
  rakudoc-type       red
  regex-         blue
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

my multi sub highlight(Str:D $source, :$unsafe, :$default, *%_) {
    highlight($source, mapper(%_), :$unsafe, :$default)
}

my multi sub highlight(Str:D $source, %mapper, :$default, *%_) {

    my role ColorMapper {
        method hsyn(Str:D $key is copy, Str:D $content) {
            while %mapper{$key} // %mapper{$key.subst(cleaner)} -> $value {
                $value ~~ Callable
                  ?? (return $value($content))
                  !! ($key = $value)
            }
            $default
              ?? $default($key, $content)
              !! $content
        }
    }

    highlight($source, ColorMapper, |%_)
}

#- mapper ----------------------------------------------------------------------

my proto sub mapper(|) is export {*}
my multi sub mapper() is default { %default }
my multi sub mapper(*%_) { mapper(%_) }
my multi sub mapper(%_) {
    my $mapper := %default.Hash;
    $mapper{.key} = .value for %_;
    $mapper
}

#- map-hsyn-key ----------------------------------------------------------------

my proto sub map-hsyn-key(|) is export {*}
my multi sub map-hsyn-key(Str:D $key) {
    %default{$key} // %default{$key.subst(cleaner)} // $key
}
my multi sub map-hsyn-key(Str:D $key is copy, %mapper) {
    while %mapper{$key} // %mapper{$key.subst(cleaner)} -> $value {
        $value ~~ Callable
          ?? (return $value)
          !! ($key = $value)
    }
    $key
}

#- RakuAST::Deparse::Highlight::Text -------------------------------------------

my constant %color =
  black   => "\e[30m",
  blue    => "\e[34m",
  cyan    => "\e[36m",
  green   => "\e[32m",
  magenta => "\e[35m",
  red     => "\e[31m",
  white   => "\e[37m",
  yellow  => "\e[33m",
;
my constant RESET = "\e[0m";

my role RakuAST::Deparse::Highlight::Text {
    method hsyn(str $key, str $content) {
        if map-hsyn-key($key) -> $color {
            if %color{$color} -> $ansi-color {
                $ansi-color ~ $content.subst(RESET, $ansi-color, :g) ~ RESET
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

#- RakuAST::Deparse::Highligh::HTML --------------------------------------------

my role RakuAST::Deparse::Highlight::HTML {
    method hsyn(str $key, str $content) {
        if map-hsyn-key($key) -> $color {
            qq|<span style="color:$color;">$content\</span>|
        }
        else {
            $content
        }
    }
}

#- RakuAST::Deparse::Highlight::DEBUG ------------------------------------------

my role RakuAST::Deparse::Highlight::DEBUG {
    method hsyn(Str:D $key, Str:D $content) {
        $content
    }
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
C<Text>, C<HTML> and C<DEBUG> is provided by default.

If no roles are specified, C<Text> will be assumed if there is someone
watching, or C<DEBUG> if the output is being redirected.

=begin code :lang<raku>

# using default color mapping
my %mapping = red => -> $t { "==$t==" }
say highlight("say 'hello world'", %mapping);
# say =="hello world"==

=end code

A slightly more complex way to use highlighting, is to supply a C<Map>
that provides mapping that will ultimately result in finding a C<Callable>
handler.  The following mappings are allowed:

=item hsyn key to color name
=item hsyn key to Callable handler
=item color name to other color name
=item color name to Callable handler

If the mapping results in finding a C<Callable> handler, then that handler
will be called with the source to be highlighted.  The handler is supposed
to return the properly highlighted text.

=begin code :lang<raku>

# using default handler
sub default(str $key, str $source) {
    "<$key>$source</$key>"
}
say highlight("say 'hello world'", :&default);
# <yellow>say</yellow> <red>"hello world"</red>

=end code

If no C<Callable> handler can be found, the optional default handler will be
called with the last key mapping found (which can be a hsyn key, or a color
name) and the source to be highlighted.

If there is no handler found and there is no default handler, then the
original text will be produced unchanged.

=head3 allowing markup

The C<highlight> subroutine also accepts a C<:allow> named argument.  It
should contain a hash with the allowable markup letters as the key, and a
C<Callable> as the value.  This C<Callable> should accept the
C<RakuAST::Doc::Markup> object as its only positional argument, and
should return the string that should be substituted.

=begin code :lang<raku>

my $source = Q:to/CODE/
my B<$a> = L<42|https://the-answer.com>;
CODE

my %allow =
  L => { qq|<a href="$_.meta()">$_.atoms()\</a>| },
  B => { '<bold>' ~ .atoms ~ '</bold>' }
;

say highlight($source, 'HTML', :%allow);
# <span style="color:magenta;">my</span>
# <span style="color:cyan;"><bold>$a</bold></span>
# <span style="color:yellow;">=</span>
# <span style="color:red;"><a href="https://the-answer.com">42</a></span>;

=end code

=head3 mapping

The mapping of C<hsyn> keys to color names is (by default) loosely based on
the syntax highlighting provided by C<vim>.  The following color names are
recognized: C<black>, C<blue>, C<cyan>, C<green>, C<magenta>, C<none>,
C<red>, C<yellow> and C<white>.

=begin code :lang<raku>

# show comments in green, and BEGIN in blue
say highlight "say 'hello world'",
  comments     => "green",
  phaser-BEGIN => "blue"
);

=end code

If the C<highlight> subroutine is called in this matter, it's also possible
to specify tweaks in the mapping of C<hsyn> keys to color names / handler
by specifying one or more pairs with additional mappings or tweaks.

=begin code :lang<raku>

# show comments in green, and BEGIN in blue from pre-tweaked hash
my constant %mapper = mapper(
  comments     => "green",
  phaser-BEGIN => "blue",
);
say highlight "say 'hello world'", %mapper);

=end code

Because creating the lookup hash with a given set of tweaks is relatively
expensive, it's also possible to tweak this beforehand with the C<mapper>
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

=item regexes

Any element to do with regexes.

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

=head2 mapper

=begin code :lang<raku>

# the default mapping logic
my %default = mapper;

# tweaked from named args
my %tweaked = mapper(comment => "green");

# tweaked from hash
my %tweaks = comment => "green", literal => "cyan";
my %tweaked = mapper(%tweaks);

=end code

The C<mapper> subroutine either returns the default mapper, or a tweaked
version of the mapper.  This can than be used as the second positional
argument to C<highlight>, or as the second positional argument to
C<map-hsyn-key>.

Tweaking can be indicated by any number of named arguments, or a hash
containing the tweaks.

=head2 map-hsyn-key

=begin code :lang<raku>

say map-hsyn-key("phaser-BEGIN");            # magenta

my constant %tweaked = mapper(phaser-BEGIN => "cyan");
say map-hsyn-key("phaser-BEGIN", %tweaked);  # cyan

=end code

The C<map-hsyn-key> subroutine converts a given C<hsyn> key to the
associated color.  If the second argument is omitted, then the default
C<hsyn> key to color mapping will be assumed.

=head1 STATUS

This is alpha quality code: it's intended to be used in rendering of the Raku
documentation source code examples, will no doubt expose problems in this logic
and/or the general deparsing logic.

=end rakudoc

# vim: expandtab shiftwidth=4
