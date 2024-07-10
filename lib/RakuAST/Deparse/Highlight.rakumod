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

# vim: expandtab shiftwidth=4
