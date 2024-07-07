use v6.e.PREVIEW;

use nqp;  # just needed until Raku::Actions/Raku::DEPARSE become default
my constant RakuActions = nqp::gethllsym('Raku','Actions');
my constant RakuDEPARSE = nqp::gethllsym('Raku','DEPARSE');

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
            $!source.substr($soc, @!eol[$index] - $soc)
        }
        else {
            Nil
        }
    }

    # Return any *full line comment* at the given line number and remove it,
    # unless it is indicated it should be kept.  If there is a comment at
    # that line, but not a full line comment, it will be ignored
    method whole-line-comment(uint $index, :$keep) {
        with @!soc[$index] -> $soc {
            # is it a whole line?
            if $soc == @!eol[$index - 1] + 1 {
                @!soc[$index] := Any unless $keep;
                return $!source.substr($soc, @!eol[$index] - $soc);
            }
        }
        Nil
    }

    # Return any full line comments *before* the given line number and
    # remove them, unless it is indicated they should be kept.
    method comments-preceding(
      uint $index is copy,
      :$keep,
      :$partial
    --> Str:D) {
        my str @parts;
        while --$index
          && self.whole-line-comment($index, :$keep) -> $comment {
            @parts.unshift($comment)
        }

        if $partial && self.comment($index) -> $comment {
            @parts.unshift($comment);
        }

        @parts ?? @parts.join("\n") !! Nil
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

my sub highlight(str $source, *@roles is copy --> Str:D) is export {
    my $actions := nqp::create(Actions);
    my $ast     := $source.AST(:$actions);
    for @roles {
        if $_ ~~ Str {
            my $class := "RakuAST::Deparse::Highlight::$_";
            $_ = "use experimental :rakuast; use $class; $class".EVAL;
        }
    }

    # Not sure why this is needed, but without it the deparse fails with
    # a "getlex: outer index out of range" error message
    my $*INDENT = "";
    $ast.DEPARSE(Deparse.new(:$actions), |@roles)
}

# vim: expandtab shiftwidth=4
