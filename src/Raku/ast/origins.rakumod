#- RakuAST::Origin -------------------------------------------------------------
class RakuAST::Origin {
    has int $.from;
    has int $.to;

    # Basically the source from which Raku code is parsed, with
    # additional functionality
    has RakuAST::Origin::Source $.source;

    # List of nodes representing the nested key positions in the source.
    # For example, for:
    # {
    #     say "123";
    #     my $foo;
    # }
    # the block node nesting will contain nodes for `say` and for `my`.
    # Normally nestings would be defined by the <statement> token.
    has Mu $.nestings;

    method new(int :$from, int :$to, Mu :$nestings, RakuAST::Origin::Source :$source) {
        my $obj := nqp::create(self);
        nqp::bindattr_i($obj, RakuAST::Origin, '$!from', $from);
        nqp::bindattr_i($obj, RakuAST::Origin, '$!to', $to);
        if nqp::isconcrete($nestings) {
            nqp::bindattr($obj, RakuAST::Origin, '$!nestings', $nestings);
        }
        if nqp::isconcrete($source) || nqp::isconcrete($*ORIGIN-SOURCE) {
            nqp::bindattr($obj, RakuAST::Origin, '$!source', ($source // $*ORIGIN-SOURCE))
        }
        $obj
    }

    method set-nestings(Mu $nestings) {
        nqp::bindattr(self, RakuAST::Origin, '$!nestings', $nestings);
    }

    method is-key() { nqp::isconcrete($!nestings) ?? True !! False }

    method as-match() { $!source.match-from(self) }

    method Str() {
        nqp::substr($!source.orig, $!from, $!to - $!from)
    }
}

#- RakuAST::Origin::Match ------------------------------------------------------
# The class is supposed to mimic NQPMatch up to the level to make it
# usable as QAST::Node.node value.  For this it would suffice to
# provide methods .orig, .from, and coercer .Str.

# TODO after merging rakuast and master branches into main it would
# make sense to add support for .file in NQP backend compilation code.
# It currently relies upon HLL::Compiler.linefileof method which might
# be unreliable.
class RakuAST::Origin::Match {
    has str $.file;
    has str $.orig;
    has int $.line;
    has int $.orig-line; # Original line number, unaffected by #line directive
    has int $.from;
    has int $.to;

    method new(str :$file, str :$orig, int :$line, int :$orig-line, int :$from, int :$to) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Origin::Match, '$!file', $file);
        nqp::bindattr_s($obj, RakuAST::Origin::Match, '$!orig', $orig);
        nqp::bindattr_i($obj, RakuAST::Origin::Match, '$!line', $line);
        nqp::bindattr_i($obj, RakuAST::Origin::Match, '$!orig-line', $orig-line);
        nqp::bindattr_i($obj, RakuAST::Origin::Match, '$!from', $from);
        nqp::bindattr_i($obj, RakuAST::Origin::Match, '$!to', $to);
        $obj
    }

    method target { $!orig }
    method chars { $!to - $!from }
    method Str { nqp::substr($!orig, $!from, $!to - $!from) }
}

#- RakuAST::Origin::Source -----------------------------------------------------
class RakuAST::Origin::Source {

    # Our source, as fed into the grammar. Better be bound to $/.target
    # because it's an optimized string.
    has str $.orig;

    # List of positions in $!orig pointing at line ends.
    has Mu $!line-ends;

    # List of lists of #line directives. Each entry is a triplet of
    # [original-directive-line, delta, file-name]
    # `original-directive-line` is where #line was encountered;
    # `delta` is what to add to a line number to get its value
    # relative to the start of file `file-name`.
    # The first entry is always [1, 0, <source-file-name>]
    has Mu $!line-file;

    method new(str :$orig) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Origin::Source, '$!orig', $orig);

        my $file := %*COMPILING<%?OPTIONS><source-name>;
        if !nqp::isconcrete($file) {
            if nqp::isnull($file := nqp::getlexdyn('$?FILES')) {
                $file := '<unknown file>';
            }
        }
        nqp::bindattr($obj, RakuAST::Origin::Source, '$!line-file', nqp::list(nqp::list(0, 0, $file)));

        my $line-ends := nqp::list;
        my int $chars := nqp::chars($orig);
        my int $nl-pos;
        while ($nl-pos := nqp::findcclass(nqp::const::CCLASS_NEWLINE, $orig, $nl-pos, $chars)) < $chars {
            my int $ord := nqp::ord($orig, $nl-pos);
            nqp::push($line-ends, ++$nl-pos);

            # Treat \r\n as a single logical newline. Note that NFG
            # implementations, we should check it really is a lone \r,
            # not the first bit of a \r\n grapheme.
            if $ord == 13 && nqp::eqat($orig, "\r", $nl-pos - 1)
                && $nl-pos < $chars && nqp::ord($orig, $nl-pos) == 10
            {
                ++$nl-pos;
            }
        }
        nqp::bindattr($obj, RakuAST::Origin::Source, '$!line-ends', $line-ends);

        $obj
    }

    method register-line-directive(int $orig-line, int $directive-line, $filename) {
        my $registered := nqp::elems($!line-file);
        # Make sure we're not trying to re-register an existing
        # directive. This can happen when grammar retracts and
        # the same line directive gets parsed repeatedly.
        if $registered == 1 || $!line-file[$registered - 1][0] < $orig-line {
            if nqp::isconcrete($filename) {
                $filename := $filename.Str;
            }
            else {
                $filename := $!line-file[0][2];
            }
            $!line-file.push([$orig-line, $directive-line - $orig-line - 1, $filename]);
        }
    }

    method original-file() {
        $!line-file[0][2]
    }

    method original-line-column(int $pos) {
        my @line-ends := $!line-ends;
        my int $lo;
        my int $hi := nqp::elems(@line-ends);
        my int $line;
        while $lo < $hi {
            $line := nqp::div_i(($lo + $hi), 2);
            if @line-ends[$line] > $pos {
                $hi := $line;
            }
            else {
                $lo := $line + 1;
            }
        }
        my $column := $lo == 0 ?? $pos !! ($pos - @line-ends[$lo - 1]);
        [$lo + 1, $column + 1]
    }

    method original-line(int $pos) {
        self.original-line-column($pos)[0]
    }

    # Get current line, column, and file as a triplet with #line
    # directives taken into account
    method location-of-pos(int $pos) {
        my @orig-line-col := self.original-line-column($pos);
        my $orig-line := @orig-line-col[0];
        my $column := @orig-line-col[1];
        my int $hi := nqp::elems($!line-file);
        my int $lo;
        my int $idx;
        while $lo < $hi {
            $idx := nqp::div_i($lo + $hi, 2);
            if $!line-file[$idx][0] > $orig-line {
                $hi := $idx;
            }
            else {
                $lo := $idx + 1;
            }
        }
        --$lo;
        [ $orig-line + $!line-file[$lo][1], $column, $!line-file[$lo][2] ]
    }

    method file-of-pos(int $pos) {
        return $!line-file[0][2] if nqp::elems($!line-file) == 1;
        self.location-of-pos($pos)[2]
    }

    method line-of-pos(int $pos) {
        self.location-of-pos($pos)[0]
    }

    method column-of-pos(int $pos) {
        self.location-of-pos($pos)[1]
    }

    # $from-to can be either NQPMatch, or Match, or RakuAST::Origin,
    # or anything else with .from/.to methods available
    method match-from($from-to) {
        my $from := $from-to.from();
        my @location := self.location-of-pos($from);
        RakuAST::Origin::Match.new(
            :from($from), :to($from-to.to()), :orig($!orig),
            :line(@location[0]), :file(@location[2]),
            :orig-line(self.original-line($from)))
    }
}
