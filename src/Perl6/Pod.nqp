# TODO BEFORE RELEASE TO PR:
#
# + fix balance rows
# + remove spec test 7b
# + ensure tests in 7a are for my three bug reports
# + make detailed log from a git diff of this branch since inception
# + check all TODOs in code
# + add test for single column table

# various helper methods for Pod parsing and processing
my $debug := 1;
class Perl6::Pod {

    # some rules for processing tables
    # row separator
    my $is_row_sep   := /^ <[-+_|=\h]>* $/;
    # legal row cell separators
    my $col_sep_pipe := /\h '|' \h/; # visible
    my $col_sep_plus := /\h '+' \h/; # visible
    my $col_sep_ws   := /\h \h/;     # invisible: double-space

    my $has_vis_col_sep := / $col_sep_pipe | $col_sep_plus /;
    my $has_ws_col_sep  := / $col_sep_ws /;

    # some vars for telling caller about table attributes, warnings, or exceptions
    # these need resetting upon each call to sub table
    my @table_line_info := []; # save info on each incoming line
    my $num_row_cells   := 0;  # all table rows must have the same number of cells
    my $error_msg       := '';
    my $table_has_vis_col_seps := 0;
    my $table_has_ws_col_seps  := 0;
    my $unbalanced_row_cells   := 0; # set true if all rows don't have same number of cells

    our sub document($/, $what, $with, :$leading, :$trailing) {
        if $leading && $trailing || !$leading && !$trailing {
            nqp::die("You must provide one of leading or trailing to Perl6::Pod::document");
        }
        if ~$with ne '' {
            if $leading {
                $*W.apply_trait($/, '&trait_mod:<is>', $what, :leading_docs($with));
            } else { # trailing
                $*W.apply_trait($/, '&trait_mod:<is>', $what, :trailing_docs($with));
            }
        }
    }

    our sub any_block($/) {
        my @children := [];
        my $type;
        my $leveled;
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;

        if $<type>.Str ~~ /^item \d*$/ {
            $type    := 'Pod::Item';
            $leveled := 1;
        } elsif $<type>.Str ~~ /^head \d+$/ {
            $type    := 'Pod::Heading';
            $leveled := 1;
        } else {
            $type := 'Pod::Block::Named';
        }

        for $<pod_content> {
            my $array := $_.ast;
            my $i     := 0;
            my $elems := $array.elems;

            while $i < $elems {
                @children.push($array.AT-POS($i));
                $i++;
            }
        }

        my $contents := serialize_array(@children);
        if $leveled {
            my $level := nqp::substr($<type>.Str, 4);
            my $level_past;
            if $level eq '' {
                $level := "1";
            }
            $level_past := $*W.add_constant(
                'Int', 'int', +$level,
            ).compile_time_value;

            my $past := serialize_object(
                $type, :level($level_past), :config($config),
                :contents($contents.compile_time_value)
            );
            return $past.compile_time_value;
        }

        my $name := $*W.add_constant('Str', 'str', $<type>.Str);
        my $past := serialize_object(
            'Pod::Block::Named', :name($name.compile_time_value),
            :config($config), :contents($contents.compile_time_value),
        );
        return $past.compile_time_value;
    }

    our sub raw_block($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;
        my $str := $*W.add_constant('Str', 'str', ~$<pod_content>);
        my $contents := serialize_array([$str.compile_time_value]);
        my $past := serialize_object(
            'Pod::Block::Comment', :config($config),
            :contents($contents.compile_time_value),
        );
        return $past.compile_time_value;
    }

    our sub config($/) {
        my $type := $*W.add_constant('Str', 'str', ~$<type>);
        return serialize_object(
            'Pod::Config', :type($type.compile_time_value),
            :config($<pod_configuration>.ast)
        ).compile_time_value
    }

    our sub make_config($/) {
        my @pairs;
        for $<colonpair> -> $colonpair {
            my $key := $colonpair<identifier>;
            my $val;
            # This is a cheaty and evil hack. This is also the only way
            # I can obtain this information without reimplementing
            # <colonpair> entirely
            if $colonpair<coloncircumfix><circumfix> {
                $val := $colonpair<coloncircumfix><circumfix>;
                if $val<nibble> {
                    $val := $*W.colonpair_nibble_to_str($/, $val<nibble>);
                } else {
                    $val := ~$val<semilist>;
                }

                $val := $*W.add_constant('Str', 'str', $val).compile_time_value;
            } else {
                # and this is the worst hack of them all.
                # Hide your kids, hide your wife!
                my $truth := !nqp::eqat($colonpair, '!', 1);

                $val := $*W.add_constant('Bool', 'int', $truth).compile_time_value;
            }

            if $key eq "allow" {
                my $chars := nqp::chars($val);
                my $pos := 0;
                while $pos < $chars {
                    my $char := nqp::substr($val, $pos, 1);
                    if $char eq " " {
                        $pos := $pos + 1;
                    } else {
                        my $bitval := nqp::ord($char) - nqp::ord("A");
                        if $bitval >= 0 && $bitval <= 25 {
                            $*POD_ALLOW_FCODES := $*POD_ALLOW_FCODES +| (2 ** $bitval);
                        }
                        $pos := $pos + 2;
                    }
                }
            }

            $key := $*W.add_constant('Str', 'str', $key).compile_time_value;
            @pairs.push(
                serialize_object(
                    'Pair', :key($key), :value($val)
                ).compile_time_value
            );
        }
        return serialize_object('Hash', |@pairs).compile_time_value;
    }

    our sub normalize_text($a) {
	# given a string of text, possibly including newlines, reduces
	# contiguous whitespace to a single space and trim leading and
	# trailing whitespace from all logical lines
        my $r := subst($a, /\s+/, ' ', :global);
        $r    := subst($r, /^^\s*/, '');
        $r    := subst($r, /\s*$$/, '');
        return $r;
    }

    our sub remove_inline_comments($S) {
	# removes all Z<> comments from string $S
        # (note any use of char '>' inside the intended comment will not be handled as intended)
	# this could (should?) be handled by the grammar
	my $s   := $S; # the raw string to clean of comments
	my $ret := ''; # the cleaned string (i.e., without comments)
	my $idx := nqp::index($s, 'Z<'); # find the beginning of a comment
	while $idx > -1 {
            my $idx2 := nqp::index($s, '>', $idx+2); # and the end
            nqp::die("FATAL:  Non-existent closing '>' for inline pod comment in string '$s'") if $idx2 < 0;
            my $s0 := nqp::substr($s, 0, $idx); # the leading chunk (which may be empty)
            # assemble the cleaned string by parts
            $ret := nqp::concat($ret, $s0);

            # throw away the orig string up to the end of the found comment
            $s := nqp::substr($s, $idx2+1); # the trailing chunk
            # look for another comment in the remaining string
            $idx := nqp::index($s, 'Z<');
	}

	# make sure we use up a non-empty string end
	if $s {
            $ret := nqp::concat($ret, $s);
	}
	return $ret;
    }

    our sub chomp($s) {
	# remove ending newline from a string
        return subst($s, /\n$/, '');
    }

    our sub merge_twines(@twines) {
        my @ret := @twines.shift.ast;
        for @twines {
            my @cur   := $_.ast;
            @ret.push(
                $*W.add_constant(
                    'Str', 'str',
                    nqp::unbox_s(@ret.pop) ~ ' ' ~ nqp::unbox_s(@cur.shift)
                ).compile_time_value,
            );
            nqp::splice(@ret, @cur, +@ret, 0);
        }
        return @ret;
    }

    our sub build_pod_string(@content) {
        return $*POD_IN_CODE_BLOCK
            ?? build_pod_code_string(@content)
            !! build_pod_regular_string(@content)
    }

    our sub build_pod_regular_string(@content) {
        sub push_strings(@strings, @where) {
            my $s := subst(nqp::join('', @strings), /\s+/, ' ', :global);
            my $t := $*W.add_constant(
                'Str', 'str', $s
            ).compile_time_value;
            @where.push($t);
        }

        my @res  := [];
        my @strs := [];
        for @content -> $elem {
            if nqp::isstr($elem) {
                # don't push the leading whitespace
                if +@res + @strs == 0 && $elem eq ' ' {

                } else {
                    @strs.push($elem);
                }
            } else {
                push_strings(@strs, @res);
                @strs := [];
                @res.push($elem);
            }
        }
        push_strings(@strs, @res);

        return @res;
    }

    # Code strings need to be handled differently:
    # Formatting codes need to be saved, but everything
    # else should be verbatim
    our sub build_pod_code_string(@content) {
        sub push_strings(@strings, @where) {
            my $s := nqp::join('', @strings);
            my $t := $*W.add_constant('Str', 'str', $s).compile_time_value;
            @where.push($t);
        }

        my @res  := [];
        my @strs := [];
        for @content -> $elem {
            if nqp::isstr($elem) {
                @strs.push($elem);
            } else {
                push_strings(@strs, @res);
                @strs := [];
                @res.push($elem);
            }
        }
        push_strings(@strs, @res);

        return @res;
    }

    # serializes the given array
    our sub serialize_array(@arr) {
        return $*W.add_constant('Array', 'type_new', |@arr);
    }

    # serializes an array of strings
    our sub serialize_aos(@arr) {
        my @cells := [];
        for @arr -> $cell {
            my $p := $*W.add_constant('Str', 'str', ~$cell);
            @cells.push($p.compile_time_value);
        }
        return serialize_array(@cells);
    }

    # serializes an array of arrays of strings
    our sub serialize_aoaos(@rows) {
        my @content := [];
        for @rows -> $row {
            my $p := serialize_aos($row);
            @content.push($*W.scalar_wrap($p.compile_time_value));
        }
        return serialize_array(@content);
    }

    # serializes object of the given type
    our sub serialize_object($type, *@pos, *%named) {
        return $*W.add_constant($type, 'type_new', |@pos, |%named);
    }

    our sub table($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;

	# reset global table vars for each table
	@table_line_info := []; # save info on each incoming line
	$num_row_cells   := 0;  # all table rows must have the same number of cells
	$error_msg       := '';
	$table_has_vis_col_seps := 0;
	$table_has_ws_col_seps  := 0;
	$unbalanced_row_cells   := 0; # set true if all rows don't have same number of cells

	# form the rows from the pod table parse match
        my @rows := [];
        nqp::say("===DEBUG NEW TABLE") if $debug;
        for $<table_row> {
            # stringify the row for analysis and further handling
            my $row := $_.ast;
            # remove inline pod comment (Z<some comment to be ignored>)
            $row := remove_inline_comments($row);
            $row := chomp($row);
            nqp::say("DEBUG RAW ROW: '$row'") if $debug;
            unless $row ~~ $is_row_sep {
                # Test the row for type of col seps. If a vis type is
                # found, then if a ws type also exists it shouldn't affect further
                # analysis. But then check for a ws type if a vis type is not found.
                if $row ~~ $has_vis_col_sep {
                    ++$table_has_vis_col_seps;
                    nqp::say("      VIS ROW: '$row'") if $debug;
                } elsif $row ~~ $has_ws_col_sep {
                    ++$table_has_ws_col_seps;
                    nqp::say("      WS ROW: '$row'") if $debug;
                }
            }
            @rows.push($row);
        }
        nqp::say("===DEBUG END OF TABLE") if $debug;

        # we have an invalid table if we have both visible and invisible
        # col sep types
        if $table_has_vis_col_seps && $table_has_ws_col_seps {
            nqp::say("===FATAL: Table has a mixture of visible and invisible column-separator types.");
            nqp::say($_) for @rows;
            nqp::die("===FATAL: Table has a mixture of visible and invisible column-separator types.");
        }

        # remove trailing blank lines
        @rows.pop while @rows && @rows[+@rows - 1] ~~ /^ \s* $/;

        # TODO: see if this is needed only for ws col sep tables
        @rows := trim_row_leading_ws(@rows);

        # break the data rows into cells
        if $table_has_vis_col_seps {
            @rows := process_rows(@rows);
        } elsif $table_has_ws_col_seps {
            @rows := splitrows(@rows);
        }

        # TODO: move this code to inside loop below
        # multi-line rows are not yet merged, but we may need to
        # add empty cells to some rows
        if 0 && $unbalanced_row_cells {
            my $i := 0;
            while $i < +@rows {
                if nqp::islist(@rows[$i]) {
                    my $nc := nqp::elems(@rows[$i]);
                    if $nc != $num_row_cells {
                        # pad row with needed empty cells
                        my $n := $num_row_cells - $nc;
                        while $n > 0 {
                            nqp::push(@rows[$i], ' ');
                            --$n;
                        }
                    }
                }
                ++$i;
            }
        }

	# Now separate the table into headers (if any) and content.
	#
        # We need to know 3 things about the row separators:
        #   + is there more than one?
        #   + where is the first one?
        #   + are they different from each other?
        #
        # Given no separators, our table is just an ordinary table
        #   of single lines.
        # If there is one separator, the table has a header and
        #   the actual content. If the separator is further than on the
        #   second row, then the header is multi-lined.
        # If there's more than one separator, the table has a multi-line
        #   header and multi-line content.
        #
        # Tricky, isn't it? Let's try to handle it sanely.
        my $sepnum        := 0;
        my $firstsepindex := 0;
        my $differentseps := 0;
        my $firstsep;
        my $i := 0;
        while $i < +@rows {
            unless nqp::islist(@rows[$i]) {
                $sepnum := $sepnum + 1;
                unless $firstsepindex { $firstsepindex := $i }
                if $firstsep {
                    if $firstsep ne @rows[$i] { $differentseps := 1 }
                } else {
                    $firstsep := @rows[$i];
                }
            }
            # TODO put balance row code here--saves a loop
            ++$i;
        }

        my $headers := [];
        my $content := [];

        if $sepnum == 0 {
            # ordinary table, no headers, one-lined rows
            $content := @rows;
        } elsif $sepnum == 1 {
            if $firstsepindex == 1 {
                # one-lined header, one-lined rows
                $headers := @rows.shift;
                @rows.shift; # remove the row separator
                $content := @rows;
            } else {
                # multi-line header, one-lined rows
                my $i := 0;
                my @hlines := [];
                while $i < $firstsepindex {
                    @hlines.push(@rows.shift);
                    ++$i;
                }
                $headers := merge_rows(@hlines);
                @rows.shift; # remove the row separator
                $content := @rows;
            }
        } else {
            my @hlines := [];
            my $i := 0;
            if $differentseps {
                while $i < $firstsepindex {
                    @hlines.push(@rows.shift);
                    ++$i;
                }
                @rows.shift; # remove the row separator
                $headers := merge_rows(@hlines);
            }
            # let's go through the rows and merge the multi-line ones
            my @newrows := [];
            my @tmp  := [];
            $i       := 0;
            while $i < +@rows {
                if nqp::islist(@rows[$i]) {
                    @tmp.push(@rows[$i]);
                } else {
                    @newrows.push(merge_rows(@tmp));
                    @tmp := [];
                }
                ++$i;
            }
            if +@tmp > 0 {
                @newrows.push(merge_rows(@tmp));
            }
            $content := @newrows;
        }

        # the table data are separated properly, now pack up
        # everything into a table object
        my $past := serialize_object(
            'Pod::Block::Table', :config($config),
            :headers(serialize_aos($headers).compile_time_value),
            :contents(serialize_aoaos($content).compile_time_value),
        );

        make $past.compile_time_value;
    } # end sub table

    #===== TABLE-SPECIFIC SUBROUTINES =====
    my sub trim_row_leading_ws(@Rows) {
        # find the shortest leading whitespace and strip it
        # from every row, also remove trailing \n
        my $w := 999999; # the shortest leading whitespace
        my @rows := @Rows;
        for @rows -> $row {
	    next if $row ~~ /^\s*$/;
	    my $match := $row ~~ /^\s+/;
	    my $n := 0;
	    $n := $match.to if $match;
	    if $n < $w {
                $w := $n;
	    }
        }

        my $i := 0;
        while $i < +@rows {
	    unless @rows[$i] ~~ /^\s*$/ {
                if $w != -1 {
		    @rows[$i] := nqp::substr(@rows[$i], $w);
                }
	    }
	    ++$i;
        }
        return @rows;
    }

    my sub process_rows(@rows) {
        # split the row between cells
        my @res;
        my $i := 0;
        while $i < +@rows {
	    my $v := @rows[$i];
	    if $v ~~ $is_row_sep {
                @res[$i] := $v;
	    } elsif $v ~~ /\h'|'\h/ {
                my $m := $v ~~ /
                    :ratchet
			 ([<!before [\h+ || ^^] '|' [\h+ || $$]> .]*)+
			 % [ [\h+ || ^^] '|' [\h || $$] ]
			 /;
                @res[$i] := [];
                for $m[0] { @res[$i].push(normalize_text($_)) }
                check_num_row_cells(+@res[$i]);
	    } elsif $v ~~ /\h'+'\h/ {
                my $m := $v ~~ /
                    :ratchet
			 ([<!before [\h+ || ^^] '+' [\h+ || $$]> .]*)+
			 % [ [\h+ || ^^] '+' [\h+ || $$] ]
			 /;
                @res[$i] := [];
                for $m[0] { @res[$i].push(normalize_text($_)) }
                check_num_row_cells(+@res[$i]);
	    } else {
                # SHOULDN'T GET HERE NOW
                nqp::die("FATAL: SHOULD NOT GET HERE!!");
                # now way to easily split rows with non-visual separators
                #return splitrows(@rows);
	    }
	    ++$i;
        }
        return @res;
    }

    my sub merge_rows(@rows) {
        my @result := @rows[0];
        my $i := 1;
        while $i < +@rows {
	    my $j := 0;
	    while $j < +@rows[$i] {
                if @rows[$i][$j] {
		    @result[$j] := normalize_text(
                        ~@result[$j] ~ ' ' ~ ~@rows[$i][$j]
		    );
                    }
                ++$j;
	    }
	    ++$i;
        }
        return @result;
    }

    # takes an array of strings (rows of a table)
    # returns array of arrays of strings (cells)
    # NOTE: this only works for tables with double-space cell
    # separators!
    my sub splitrows(@Rows) {
        my @rows := @Rows;

        my @suspects := [];  #positions that might be cell delimiters
        # values: 1     - impossible!
        #         unset - maybe

        # collect cell delimiters per row
        my $i := 0;
        while $i < +@rows {
	    unless @rows[$i] ~~ $is_row_sep {
                my @line := nqp::split('', @rows[$i]);
                my $j := 0;
                while $j < +@line {
		    unless @suspects[$j] {
                        if @line[$j] ne ' ' {
			    @suspects[$j] := 1;
                        }
		    }
		    ++$j;
                }
	    }
	    ++$i;
        }

        # now let's skip the single spaces by marking them impossible
        $i := 0;
        while $i < +@suspects {
	    unless @suspects[$i] {
                if @suspects[$i-1] && @suspects[$i+1] {
		    @suspects[$i] := 1;
                }
	    }
	    ++$i;
        }

        # now we're doing some magic which will
        # turn those positions into cell ranges
        # so, e.g., for 1 values in positions: 13 14 15   30 31 32 33
        # we get [0, 13, 16, 30, 34, 0] (last 0 as a guard)

        my $wasone := 1;
        $i := 0;
        my @ranges := [];
        @ranges.push(0);

        while $i < +@suspects {
	    if !$wasone && @suspects[$i] == 1 {
                @ranges.push($i);
                $wasone := 1;
	    } elsif $wasone && @suspects[$i] != 1 {
                @ranges.push($i);
                $wasone := 0;
	    }
	    ++$i;
        }
        @ranges.push(0); # guard

        my @ret := [];
        for @rows -> $row {
	    if $row ~~ $is_row_sep {
                @ret.push($row);
                next;
	    }
	    my @tmp := [];
	    for @ranges -> $a, $b {
                next if $a > nqp::chars($row);
                if $b {
		    @tmp.push(
                        normalize_text(nqp::substr($row, $a, $b - $a))
		    );
                } else {
		    @tmp.push(
                        normalize_text(nqp::substr($row, $a))
		    );
                }
	    }
	    @ret.push(@tmp);
            check_num_row_cells(+@tmp);
        }
        return @ret;
    }

    my sub check_num_row_cells($num_cells) {
        # checks a row's number of cells against the global value
        if !$num_row_cells {
            $num_row_cells := $num_cells;
        } elsif $num_cells > $num_row_cells {
            $num_row_cells := $num_cells;
            $unbalanced_row_cells := 1;
        } elsif $num_cells < $num_row_cells {
            $unbalanced_row_cells := 1;
        } 
    }
    #===== END OF TABLE-SPECIFIC SUBROUTINES =====

} # end class Perl6::Pod

# vim: ft=perl6
