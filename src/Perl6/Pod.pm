# various helper methods for Pod parsing and processing
class Perl6::Pod {
    our sub document($what, $with) {
        if ~$with ne '' {
            my $true := $*W.add_constant('Int', 'int', 1)<compile_time_value>;
            $*W.apply_trait('&trait_mod:<is>', $what, $*DOCEE, :docs($true));
            # don't reset it if it already holds docs for another element
            if $*DECLARATOR_DOCS && $*DOC.to == $*DECLARATOR_DOCS.to {
                $*DECLARATOR_DOCS := '';
            }
        }
    }

    our sub any_block($/) {
        my @children := [];
        my $type;
        my $leveled;
        my $config := make_config($/);

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
            @children.push($_.ast);
        }

        my $content := serialize_array(@children);
        if $leveled {
            my $level := nqp::substr($<type>.Str, 4);
            my $level_past;
            if $level ne '' {
                $level_past := $*W.add_constant(
                    'Int', 'int', +$level,
                )<compile_time_value>;
            } else {
                $level_past := $*W.find_symbol(['Mu']);
            }

            my $past := serialize_object(
                $type, :level($level_past), :config($config),
                :content($content<compile_time_value>)
            );
            return $past<compile_time_value>;
        }

        my $name := $*W.add_constant('Str', 'str', $<type>.Str);
        my $past := serialize_object(
            'Pod::Block::Named', :name($name<compile_time_value>),
            :config($config), :content($content<compile_time_value>),
        );
        return $past<compile_time_value>;
    }

    our sub raw_block($/) {
        my $config := make_config($/);
        my $str := $*W.add_constant('Str', 'str', ~$<pod_content>);
        my $content := serialize_array([$str<compile_time_value>]);
        my $type := $<type>.Str eq 'code' ?? 'Pod::Block::Code'
                                          !! 'Pod::Block::Comment';
        my $past := serialize_object(
            $type, :config($config),
            :content($content<compile_time_value>),
        );
        return $past<compile_time_value>;
    }

    our sub config($/) {
        my $type := $*W.add_constant('Str', 'str', ~$<type>);
        return serialize_object(
            'Pod::Config', :type($type<compile_time_value>),
            :config(make_config($/))
        )<compile_time_value>
    }

    our sub make_config($/) {
        my @pairs;
        for $<colonpair> -> $colonpair {
            my $key := $colonpair<identifier>;
            my $val;
            # This is a cheaty and evil hack. This is also the only way
            # I can obtain this information without reimplementing
            # <colonpair> entirely
            if $colonpair<circumfix> {
                $val := $colonpair<circumfix>;
                if $val<quote_EXPR> {
                    $val := pir::join('', $val<quote_EXPR><quote_delimited><quote_atom>);
                } else {
                    $val := ~$val<semilist>;
                }
            } else {
                # and this is the worst hack of them all.
                # Hide your kids, hide your wife!
                my $truth := pir::substr($colonpair, 1, 1) ne '!';

                $val := $*W.add_constant('Int', 'int', $truth)<compile_time_value>;
            }
            $key := $*W.add_constant('Str', 'str', $key)<compile_time_value>;
            $val := $*W.add_constant('Str', 'str', $val)<compile_time_value>;
            @pairs.push(
                serialize_object(
                    'Pair', :key($key), :value($val)
                )<compile_time_value>
            );
        }
        return serialize_object('Hash', |@pairs)<compile_time_value>;
    }

    our sub formatted_text($a) {
        my $r := subst($a, /\s+/, ' ', :global);
        $r    := subst($r, /^^\s*/, '');
        $r    := subst($r, /\s*$$/, '');
        return $r;
    }
    our sub table($/) {
        my $config := make_config($/);

        my @rows := [];
        for $<table_row> {
            @rows.push($_.ast);
        }
        @rows := process_rows(@rows);
        # we need to know 3 things about the separators:
        #   is there more than one
        #   where is the first one
        #   are they different from each other
        # Given no separators, our table is just an ordinary, one-lined
        # table.
        # If there is one separator, the table has a header and
        # the actual content. If the first header is further than on the
        # second row, then the header is multi-lined.
        # If there's more than one separator, the table has a multi-line
        # header and a multi-line content.
        # Tricky, isn't it? Let's try to handle it sanely
        my $sepnum        := 0;
        my $firstsepindex := 0;
        my $differentseps := 0;
        my $firstsep;
        my $i := 0;
        while $i < +@rows {
            unless pir::isa(@rows[$i], 'ResizablePMCArray') {
                $sepnum := $sepnum + 1;
                unless $firstsepindex { $firstsepindex := $i }
                if $firstsep {
                    if $firstsep ne @rows[$i] { $differentseps := 1 }
                } else {
                    $firstsep := @rows[$i];
                }
            }
            $i := $i + 1;
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
                @rows.shift; # remove the separator
                $content := @rows;
            } else {
                # multi-line header, one-lined rows
                my $i := 0;
                my @hlines := [];
                while $i < $firstsepindex {
                    @hlines.push(@rows.shift);
                    $i := $i + 1;
                }
                $headers := merge_rows(@hlines);
                @rows.shift; # remove the separator
                $content := @rows;
            }
        } else {
            my @hlines := [];
            my $i := 0;
            if $differentseps {
                while $i < $firstsepindex {
                    @hlines.push(@rows.shift);
                    $i := $i + 1;
                }
                @rows.shift;
                $headers := merge_rows(@hlines);
            }
            # let's go through the rows and merge the multi-line ones
            my @newrows := [];
            my @tmp  := [];
            $i       := 0;
            while $i < +@rows {
                if pir::isa(@rows[$i], 'ResizablePMCArray') {
                    @tmp.push(@rows[$i]);
                } else {
                    @newrows.push(merge_rows(@tmp));
                    @tmp := [];
                }
                $i := $i + 1;
            }
            if +@tmp > 0 {
                @newrows.push(merge_rows(@tmp));
            }
            $content := @newrows;
        }

        my $past := serialize_object(
            'Pod::Block::Table', :config($config),
            :headers(serialize_aos($headers)<compile_time_value>),
            :content(serialize_aoaos($content)<compile_time_value>),
        );
        make $past<compile_time_value>;
    }

    our sub process_rows(@rows) {
        # remove trailing blank lines
        @rows.pop while @rows[+@rows - 1] ~~ /^ \s* $/;
        # find the longest leading whitespace and strip it
        # from every row, also remove trailing \n
        my $w := -1; # the longest leading whitespace
        for @rows -> $row {
            next if $row ~~ /^^\s*$$/;
            my $match := $row ~~ /^^\s+/;
            my $n := $match.to;
            if $n < $w || $w == -1 {
                $w := $n;
            }
        }
        my $i := 0;
        while $i < +@rows {
            unless @rows[$i] ~~ /^^\s*$$/ {
                @rows[$i] := nqp::substr(@rows[$i], $w);
            }
            # chomp
            @rows[$i] := subst(@rows[$i], /\n$/, '');
            $i := $i + 1;
        }
        # split the row between cells
        my @res;
        $i := 0;
        while $i < +@rows {
            my $v := @rows[$i];
            if $v ~~ /^'='+ || ^'-'+ || ^'_'+ || ^\h*$/ {
                @res[$i] := $v;
            } elsif $v ~~ /\h'|'\h/ {
                my $m := $v ~~ /
                    :ratchet ([<!before [\h+ || ^^] '|' [\h+ || $$]> .]*)
                    ** [ [\h+ || ^^] '|' [\h || $$] ]
                /;
                @res[$i] := [];
                for $m[0] { @res[$i].push(formatted_text($_)) }
            } elsif $v ~~ /\h'+'\h/ {
                my $m := $v ~~ /
                    :ratchet ([<!before [\h+ || ^^] '+' [\h+ || $$]> .]*)
                    ** [ [\h+ || ^^] '+' [\h+ || $$] ]
                /;
                @res[$i] := [];
                for $m[0] { @res[$i].push(formatted_text($_)) }
            } else {
                # now way to easily split rows
                return splitrows(@rows);
            }
            $i := $i + 1;
        }
        return @res;
    }

    our sub merge_rows(@rows) {
        my @result := @rows[0];
        my $i := 1;
        while $i < +@rows {
            my $j := 0;
            while $j < +@rows[$i] {
                if @rows[$i][$j] {
                    @result[$j] := formatted_text(
                        ~@result[$j] ~ ' ' ~ ~@rows[$i][$j]
                    );
                }
                $j := $j + 1;
            }
            $i := $i + 1;
        }
        return @result;
    }

    our sub merge_twines(@twines) {
        my @ret := @twines.shift.ast;
        for @twines {
            my @cur   := $_.ast;
            @ret.push(
                $*W.add_constant(
                    'Str', 'str',
                    nqp::unbox_s(@ret.pop) ~ ' ' ~ nqp::unbox_s(@cur.shift)
                )<compile_time_value>,
            );
            nqp::splice(@ret, @cur, +@ret, 0);
        }
        return @ret;
    }

    our sub build_pod_string(@content) {
        sub push_strings(@strings, @where) {
            my $s := subst(pir::join('', @strings), /\s+/, ' ', :global);
            my $t := $*W.add_constant(
                'Str', 'str', $s
            )<compile_time_value>;
            @where.push($t);
        }

        my @res  := [];
        my @strs := [];
        for @content -> $elem {
            if pir::typeof($elem) eq 'String' {
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


    # takes an array of strings (rows of a table)
    # returns array of arrays of strings (cells)
    our sub splitrows(@rows) {
        my @suspects := []; #positions that might be cell delimiters
                            # values: 1     – impossibru!
                            #         unset – mebbe

        my $i := 0;
        while $i < +@rows {
            unless @rows[$i] ~~ /^'='+ || ^'-'+ || ^'_'+ || ^\h*$ / {
                my @line := pir::split('', @rows[$i]);
                my $j := 0;
                while $j < +@line {
                    unless @suspects[$j] {
                        if @line[$j] ne ' ' {
                            @suspects[$j] := 1;
                        }
                    }
                    $j := $j + 1;
                }
            }
            $i := $i + 1;
        }

        # now let's skip the single spaces
        $i := 0;
        while $i < +@suspects {
            unless @suspects[$i] {
                if @suspects[$i-1] && @suspects[$i+1] {
                    @suspects[$i] := 1;
                }
            }
            $i := $i + 1;
        }

        # now we're doing some magic which will
        # turn those positions into cell ranges
        # so for values: 13 14 15   30 31 32 33
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
            $i := $i + 1;
        }
        @ranges.push(0); # guard

        my @ret := [];
        for @rows -> $row {
            if $row ~~ /^'='+ || ^'-'+ || ^'_'+ || ^\h*$/ {
                @ret.push($row);
                next;
            }
            my @tmp := [];
            for @ranges -> $a, $b {
                next if $a > nqp::chars($row);
                if $b {
                    @tmp.push(
                        formatted_text(nqp::substr($row, $a, $b - $a))
                    );
                } else {
                    @tmp.push(
                        formatted_text(nqp::substr($row, $a))
                    );
                }
            }
            @ret.push(@tmp);
        }
        return @ret;
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
            @cells.push($p<compile_time_value>);
        }
        return serialize_array(@cells);
    }

    # serializes an array of arrays of strings
    our sub serialize_aoaos(@rows) {
        my @content := [];
        for @rows -> $row {
            my $p := serialize_aos($row);
            @content.push($*W.scalar_wrap($p<compile_time_value>));
        }
        return serialize_array(@content);
    }

    # serializes object of the given type
    our sub serialize_object($type, *@pos, *%named) {
        return $*W.add_constant($type, 'type_new', |@pos, |%named);
    }
}

# vim: ft=perl6
