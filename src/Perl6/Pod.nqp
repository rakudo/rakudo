class Perl6::Pod {

    # various helper methods for Pod parsing and processing

    # enable use of env vars for debug selections
    # TODO: track down possible nqp bug: inconsistent handling of the debug values
    my $debug    := 0; # for dev use
    my $udebug   := 0; # for users via an environment variable
    my $ddenvvar := 'RAKUDO_POD6_TABLE_DEBUG_DEV';
    my $duenvvar := 'RAKUDO_POD6_TABLE_DEBUG';
    my %env      := nqp::getenvhash();
    if nqp::existskey(%env, $ddenvvar) {
        my $val := nqp::atkey(%env, $ddenvvar);
        $debug := $val;
    }
    if nqp::existskey(%env, $duenvvar) {
        my $val := nqp::atkey(%env, $duenvvar);
        $udebug := $val;
    }

    my $show_warning :=  1; # flag used to track the first warning so no repeated warnings are given
    my $table_num    := -1; # for user debugging, incremented by one on each call to sub table

    our sub document($/, $what, $with, :$leading, :$trailing) {
        if $leading && $trailing || !$leading && !$trailing {
            nqp::die("You must provide one of leading or trailing to Perl6::Pod::document");
        }
        if ~$with ne '' {
            if $leading {
                $*W.apply_trait($/, '&trait_mod:<is>', $what, :leading_docs($with));
            }
            else { # trailing
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
        }
        elsif $<type>.Str ~~ /^head \d+$/ {
            $type    := 'Pod::Heading';
            $leveled := 1;
        }
        else {
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
            # TODO This is a cheaty and evil hack. This is also the only way
            # I can obtain this information without reimplementing
            # <colonpair> entirely
            if $colonpair<coloncircumfix><circumfix> {
                $val := $colonpair<coloncircumfix><circumfix>;
                if $val<nibble> {
                    $val := $*W.colonpair_nibble_to_str($/, $val<nibble>);
                }
                else {
                    $val := ~$val<semilist>;
                }

                $val := $*W.add_constant('Str', 'str', $val).compile_time_value;
            }
            else {
                # and this is the worst hack of them all.
                # TODO Hide your kids, hide your wife!
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
                    }
                    else {
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
            nqp::die("FATAL:  Non-existent closing '>' for inline pod comment in string '$s' in Table $table_num") if $idx2 < 0;
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

    our sub trim_right($a) {
        # given a string of text, removes all whitespace from the end
        return subst($a, /\s*$/, '');
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
        sub push_regular_strings(@strings, @where) {
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

                }
                else {
                    @strs.push($elem);
                }
            }
            else {
                push_regular_strings(@strs, @res);
                @strs := [];
                @res.push($elem);
            }
        }
        push_regular_strings(@strs, @res);

        return @res;
    }

    # Code strings need to be handled differently:
    # Formatting codes need to be saved, but everything
    # else should be verbatim
    our sub build_pod_code_string(@content) {
        sub push_code_strings(@strings, @where) {
            my $s := nqp::join('', @strings);
            my $t := $*W.add_constant(
                'Str', 'str', $s
            ).compile_time_value;
            @where.push($t);
        }

        my @res  := [];
        my @strs := [];
        for @content -> $elem {
            if nqp::isstr($elem) {
                @strs.push($elem);
            }
            else {
                push_code_strings(@strs, @res);
                @strs := [];
                @res.push($elem);
            }
        }
        push_code_strings(@strs, @res);

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

    # TODO This sub is for future work on pod formatting issues.
    # It isn't currently used.
    sub string2twine($S) {
        # takes a simple string with unhandled formatting code
        # and converts it into a twine data structure. primarily
        # designed for a table cell.
        # note all Z<> comments have already been removed.
        my $s   := $S; # the raw string to be converted to a twine
        my $ret := ''; # the cleaned string (i.e., without comments)
        my $idx := nqp::index($s, '<'); # find the possible beginning of a formatting code
        if $idx < 0 {
            # no '<' found so return the simple string
            return $s;
        }

        while $idx > -1 {
            my $idx2 := nqp::index($s, '>', $idx+2); # and the end
            nqp::die("FATAL:  Couldn't find terminator '>' for inline pod format code in string '$s' in Table $table_num") if $idx2 < 0;
            my $s0 := nqp::substr($s, 0, $idx); # the leading chunk (which may be empty)
            # assemble the cleaned string by parts
            $ret := nqp::concat($ret, $s0);

            # throw away the orig string up to the end of the found comment
            $s := nqp::substr($s, $idx2+1); # the trailing chunk
            # look for another comment in the remaining string
            $idx := nqp::index($s, 'Z<');
        }

        # make sure we use up a non-empty string end
        $ret := nqp::concat($ret, $s) if $s;

        return $ret;
    }

    our sub table($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;

        # increment the table number for user debugging and reporting
        ++$table_num;

        # some rules for processing tables
        # row separator
        my $is_row_sep   := /^ <[-+_|=\h]>* $/;
        # legal row cell separators
        my $col_sep_pipe := /\h '|' \h/; # visible
        my $col_sep_plus := /\h '+' \h/; # visible
        my $col_sep_ws   := /\h \h/;     # invisible: double-space

        my $has_vis_col_sep  := / $col_sep_pipe | $col_sep_plus /;
        my $has_ws_col_sep   := / $col_sep_ws /;
        my $has_col_sep      := / $col_sep_pipe | $col_sep_plus | $col_sep_ws /;
        my $has_col_sep_plus := / $col_sep_plus /;


        # string literals for substitutions
        my $col_sep_pipe_literal := ' | '; # visible
        my $pipe  := '|';
        my $plus  := '+';
        my $space := ' ';

        # some vars for telling caller about table attributes, warnings, or exceptions
        # these are reset upon each call to sub table (i.e., for each table)
        my $table_has_no_col_seps   := 0;
        my @orig_rows               := [];
        my $has_shown_table_matrix  := 0;

        my $table_has_col_sep_plus      := 0;
        #=== for fatal conditions
        my $fatals := 0;
        my $table_has_multiple_row_seps := 0; # set true if multiple consecutive interior row seps
        my $table_has_vis_col_seps      := 0;
        my $table_has_ws_col_seps       := 0;
        my $table_has_data              := 0; # die if not set true
        #=== for warning conditions
        my $warns := 0;
        my $unbalanced_row_cells    := 0; # set true if all rows don't have same number of cells
        my $num_row_cells           := 0; # all table rows must have the same number of cells
        my $table_has_leading_row_seps             := 0;
        my $table_has_trailing_row_seps            := 0;
        my $table_has_border_vis_col_seps          := 0;
        my $table_has_leading_border_vis_col_seps  := 0;
        my $table_has_trailing_border_vis_col_seps := 0;

        # form the rows from the pod table parse match
        my @rows := [];
        nqp::say("===DEBUG NEW TABLE $table_num") if $debug;

        my $first_line            := 0; # set true when first non-row-sep line is seen
        my $last_line_was_row_sep := 0;
        for $<table_row> {
            # stringify the row for analysis and further handling
            my $row := $_.ast;
            $row := chomp($row);
            @orig_rows.push($row);
            if $row ~~ $is_row_sep {
                # leading row sep lines are deleted and warned about
                # two consecutive interior row sep lines are illegal
                if !$first_line {
                    # ignore it for now but create a warning
                    ++$table_has_leading_row_seps;
                    next;
                }
                if $last_line_was_row_sep {
                    # an invalid table if inside it
                    ++$table_has_multiple_row_seps;
                }
                else {
                    $last_line_was_row_sep := 1;
                }
            }
            else {
                $last_line_was_row_sep := 0;
                # this is a table data line
                $first_line := 1;
            }

            nqp::say("DEBUG RAW ROW (after chomp): '$row'") if $debug;
            # remove inline pod comment (Z<some comment to be ignored>)
            $row := remove_inline_comments($row) if $row ~~ /'Z<'/;
            unless $row ~~ $is_row_sep {
                ++$table_has_data;
                if $row ~~ $has_vis_col_sep {
                    #nqp::say("      VIS COL SEP ROW: '$row'") if $debug;
                    ++$table_has_col_sep_plus if $row ~~ $has_col_sep_plus;
                }
            }
            @rows.push($row);
        }
        nqp::say("===DEBUG END OF TABLE INPUT for table $table_num") if $debug;

        # remove trailing blank lines BEFORE checking for col separators
        @rows.pop while @rows && @rows[+@rows - 1] ~~ /^ \s* $/;

        # check for trailing row sep lines
        if @rows && @rows[+@rows - 1] ~~ $is_row_sep {
            ++$table_has_trailing_row_seps;
            @rows.pop while @rows && @rows[+@rows - 1] ~~ $is_row_sep;
        }

        # check for col seps AFTER trailing row separator lines have been removed
        for @rows -> $row {
            unless $row ~~ $is_row_sep {
                # Test the row for type of col seps. If a vis type is
                # found, then if a ws type also exists it shouldn't affect further
                # analysis. But then check for a ws type if a vis type is not found.
                if $row ~~ $has_vis_col_sep {
                    ++$table_has_vis_col_seps;
                }
                elsif $row ~~ $has_ws_col_sep {
                    ++$table_has_ws_col_seps;
                }
            }
        }

        # note warnings and fatal conditions are handled in a sub AFTER the next phases:

        # all tables have excess leading ws trimmed evenly
        @rows := trim_row_leading_ws(@rows);

        # break the data rows into cells
        if $table_has_vis_col_seps {
            @rows := split_vis_col_sep_rows(@rows);
        }
        elsif $table_has_ws_col_seps {
            @rows := split_ws_col_sep_rows(@rows);
        }
        else {
            # don't forget the single-column tables!
            @rows := split_ws_col_sep_rows(@rows);
        }

        # all warnings and invalid conditions should be known by now
        # time to warn or throw as appropriate
        handle_table_issues(@orig_rows);

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
                unless $firstsepindex {
                    $firstsepindex := $i
                }
                if $firstsep {
                    if $firstsep ne @rows[$i] {
                        $differentseps := 1
                    }
                }
                else {
                    $firstsep := @rows[$i];
                }
            }
            # multi-line rows are not yet merged, but we may need to
            # add empty cells to some rows
            if $unbalanced_row_cells && nqp::islist(@rows[$i]) {
                my $ncells := nqp::elems(@rows[$i]);
                if $ncells != $num_row_cells {
                    # pad row with needed empty cells
                    my $n := $num_row_cells - $ncells;
                    while $n > 0 {
                        nqp::push(@rows[$i], '');
                        --$n;
                    }
                }
            }
            ++$i;
        }

        my $headers := [];
        my $content := [];

        if $sepnum == 0 {
            # ordinary table, no headers, one-lined rows
            $content := @rows;
        }
        elsif $sepnum == 1 {
            if $firstsepindex == 1 {
                # one-lined header, one-lined rows
                $headers := @rows.shift;
                @rows.shift; # remove the row separator
                $content := @rows;
            }
            else {
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
        }
        else {
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
                }
                else {
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

        # show final table matrix
        if $debug || $udebug {
            # show final table matrix
            show_final_table_matrix($headers, $content);
        }

        # the table data are separated properly, now pack up
        # everything into a table object
        my $past := serialize_object(
            'Pod::Block::Table', :config($config),
            :headers(serialize_aos($headers).compile_time_value),
            :contents(serialize_aoaos($content).compile_time_value),
        );

        # must return here before the sub subs begin
        return $past.compile_time_value;

        #===== TABLE-SPECIFIC SUBROUTINES =====
        sub unescape-col-seps($S) {
            my $s := subst($S, /'\+'/, '+', :global);
            $s    := subst($s, /'\|'/, '|', :global);
            return $s;
        }

        sub handle_table_issues(@rows) {
            my $t := $table_num;
            #=== invalid tables generate exceptions
            if $table_has_vis_col_seps && $table_has_ws_col_seps {
                ++$fatals;
                nqp::say("===FATAL: Table $t has a mixture of visible and invisible column-separator types.");
            }
            if $table_has_multiple_row_seps {
                ++$fatals;
                nqp::say("===FATAL: Table $t has multiple interior row separator lines.");
            }
            if !$table_has_data {
                ++$fatals;
                nqp::say("===FATAL: Table $t has no data.");
            }

            # bail out with data for a fatal
            if $fatals {
                nqp::say($_) for @rows; # the original table as input
                nqp::say("===end FATAL table $t input");
                nqp::die(1);
            }

            #=== warnings (only if $udebug)
            if $table_has_leading_row_seps || $table_has_trailing_row_seps {
                ++$warns;
                if $udebug {
                    nqp::say("===WARNING: Table $t has unneeded leading or trailing row separators.");
                }
            }

            if $table_has_border_vis_col_seps {
                ++$warns;
                if $udebug {
                    nqp::say("===WARNING: Table $t has unneeded border vis col separators.");
                }
            }

            if $warns && $udebug {
                nqp::say($_) for @rows; # the original table as input
                nqp::say("===end WARNING table $t input rows");
            }
            elsif $warns && $show_warning {
                    nqp::say("===WARNING: One or more tables evidence bad practice.");
                    nqp::say("==          Set environment variable 'RAKUDO_POD6_TABLE_DEBUG' for more details.");
                    $show_warning := 0;
            }
        }

        sub normalize_vis_col_sep_rows(@Rows) {
            # leading and trailing column separators are handled and warned about
            my @rows     := @Rows;
            my $nlp      := 0; # number of leading pipes
            my $ntp      := 0; # number of trailing pipes
            my $nr       := 0; # number of data rows
            my $leading  := 0;
            my $trailing := 0;
            my $i        := 0;
            while $i < +@rows {
                unless @rows[$i] ~~ $is_row_sep {
                    ++$nr;
                    @rows[$i] := normalize_row_cells(@rows[$i]);
                    # check for leading pipes
                    if @rows[$i] ~~ /^ \s* '|' / {
                        ++$nlp;
                        ++$table_has_leading_border_vis_col_seps;
                        ++$table_has_border_vis_col_seps;
                    }
                    # check for trailing pipes
                    if @rows[$i] ~~ / '|' \s* $/ {
                        ++$ntp;
                        ++$table_has_trailing_border_vis_col_seps;
                        ++$table_has_border_vis_col_seps;
                    }
                    say("DEBUG: i $i, nlp $nlp, ntp $ntp, nr $nr") if $debug;
                }
                ++$i;
            }
            ++$leading if $nlp == $nr;
            ++$trailing if $ntp == $nr;
            if $leading || $trailing {
                say("DEBUG: REMOVING BORDER PIPES") if $debug;
                # all data rows have the leading or trailng pipe, so
                # remove all including surrounding ws
                @rows := remove_border_pipes(@rows, $leading, $trailing);
            }
            return @rows;
        }

        sub remove_border_pipes(@Rows, $leading, $trailing) {
            my @rows :=  @Rows;
            my $i := 0; # BUG: nqp did NOT warn about missing $i
            while $i < +@rows {
                unless @rows[$i] ~~ $is_row_sep {
                    if $leading || $trailing {
                        say("DEBUG BEFORE rm border: '@rows[$i]'") if $debug;
                    }
                    if $leading {
                        # remove the leading pipe and surrounding ws
                        @rows[$i] := subst(@rows[$i], /^ \h* '|' \h* /, '');
                    }
                    if $trailing {
                        # remove the trailing pipe and surrounding ws
                        @rows[$i] := subst(@rows[$i], / \h* '|' \h* $/, '');
                    }
                    if $leading || $trailing {
                        say("DEBUG AFTER rm border: '@rows[$i]'") if $debug;
                    }
                }
                ++$i;
            }
            return @rows;
        }

        sub trim_row_leading_ws(@Rows) {
            # find the shortest leading whitespace and strip it
            # from every row
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

        sub split_vis_col_sep_rows(@Rows) {
            my @rows := normalize_vis_col_sep_rows(@Rows);

            # split the vis col sep rows between cells
            # note we don't merge multiple rows yet
            my @res;
            my $i := 0;
            for @rows -> $row {
                if $row ~~ $is_row_sep {
                    @res.push($row);
                }
                elsif nqp::isstr($row) {
                    # just split the row
                    nqp::say("VIS BEFORE SPLIT: '$row'") if $debug;
                    # TODO split on ' | '
                    # TODO unescape | and +
                    my @t := nqp::split(' | ', $row);
                    my @tmp := [];
                    my $j := 0;
                    for @t {
                        my $c := $_;
                        nqp::say("  CELL $j: '$c'") if $debug;
                        $c := normalize_text(unescape-col-seps($c));
                        nqp::say("  CELL $j normalized: '$c'") if $debug;
                        @tmp.push($c);
                        ++$j;
                    }
                    # this is the check for vis col sep rows
                    my $n := +@tmp;
                    check_num_row_cells($n);
                    @res.push(@tmp);
                }
                else {
                    nqp::say("WEIRD ROW number $i '$row'") if $debug;
                }
                ++$i;
            }
            return @res;
        }

        sub merge_rows(@rows) {
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
        sub split_ws_col_sep_rows(@Rows) {
            my @rows := @Rows;

            my @suspects := [];  # positions that might be cell delimiters
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
                }
                elsif $wasone && @suspects[$i] != 1 {
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
                            # TODO unescape | and +
                            normalize_text(unescape-col-seps(nqp::substr($row, $a, $b - $a)))
                        );
                    }
                    else {
                        @tmp.push(
                            # TODO unescape | and +
                            normalize_text(unescape-col-seps(nqp::substr($row, $a)))
                        );
                    }
                }
                # this is the check for ws col sep rows
                check_num_row_cells(+@tmp);
                @ret.push(@tmp);
            }
            return @ret;
        }

        sub check_num_row_cells($num_cells) {
            if !$num_cells {return}
            # checks a row's number of cells against the global value
            if !$num_row_cells {
                # the first row checked sets the mark
                $num_row_cells := $num_cells;
            }
            elsif $num_cells > $num_row_cells {
                $num_row_cells := $num_cells;
                ++$unbalanced_row_cells;
            }
            elsif $num_cells < $num_row_cells {
                ++$unbalanced_row_cells;
            }
        }

        sub normalize_row_cells($row) {
            # forces a pipe separator as the row cell separator
            my $s := $row;
            # add trailing ws to ensure correct split later
            $s := nqp::concat($s, ' ');
            $s := subst($s, /\h'+'\h/, $col_sep_pipe_literal, :global);
            return $s;
        }

        sub show_final_table_matrix($headers, $content) {
            nqp::say("===DEBUG: final cell layout for table $table_num.");
            nqp::say("=== cell contents are enclosed in single quotes");
            nqp::say("=== cell separators are shown as pipes ('|')");
            nqp::say("=== headers");
            if $headers {
                my $i := 0;
                for $headers -> $h {
                    nqp::print(" | ") if $i;
                    nqp::print("'$h'");
                    ++$i;
                }
                nqp::print("\n");
            }
            else {
                nqp::print(" (no headers)\n");
            }
            nqp::say("=== contents");
            for $content -> $row {
                my $i := 0;
                for $row -> $cell {
                    nqp::print(" | ") if $i;
                    nqp::print("'$cell'");
                    ++$i;
                }
                nqp::print("\n");
            }
            nqp::say("===DEBUG: end final cell layout for table $table_num.");
        }
        #===== END OF TABLE-SPECIFIC SUBROUTINES =====

    } # end sub table

} # end class Perl6::Pod

# vim: ft=perl6
