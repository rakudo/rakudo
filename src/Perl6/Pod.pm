# various helper methods for Pod parsing and processing
class Perl6::Pod {
    our sub any_block($/) {
        my @children := [];
        for $<pod_content> {
            # not trivial, for it can be either an array or a pod node
            # and we can't really flatten a list in nqp
            # I hope there's a better way,
            # but let's settle on this for now
            if pir::isa($_.ast, 'ResizablePMCArray') {
                for $_.ast {
                    @children.push($_);
                }
            } else {
                @children.push($_.ast);
            }
        }
        my $content := $*ST.add_constant(
            'Array', 'type_new',
            |@children,
        );
        if $<type>.Str ~~ /^item \d*$/ {
            my $level      := nqp::substr($<type>.Str, 4);
            my $level_past;
            if $level ne '' {
                $level_past := $*ST.add_constant(
                    'Int', 'int', +$level,
                )<compile_time_value>;
            } else {
                $level_past := $*ST.find_symbol(['Mu']);
            }
            my $past := $*ST.add_constant(
                'Pod::Item', 'type_new',
                :level($level_past),
                :content($content<compile_time_value>),
            );
            return $past<compile_time_value>;
        }
        my $name := $*ST.add_constant('Str', 'str', $<type>.Str);
        my $past := $*ST.add_constant(
            'Pod::Block::Named', 'type_new',
            :name($name<compile_time_value>),
            :content($content<compile_time_value>),
        );
        return $past<compile_time_value>;
    }

    our sub raw_block($/) {
        my $type;
        my $str := $*ST.add_constant(
            'Str', 'str',
            pir::isa($<pod_content>, 'ResizablePMCArray')
                ?? pir::join('', $<pod_content>) !! ~$<pod_content>,
        );
        my $content := $*ST.add_constant(
            'Array', 'type_new',
            $str<compile_time_value>
        );
        my $past := $*ST.add_constant(
            $<type>.Str eq 'code' ?? 'Pod::Block::Code'
                                  !! 'Pod::Block::Comment',
            'type_new',
            :content($content<compile_time_value>),
        );
        return $past<compile_time_value>;
    }

    our sub formatted_text($a) {
        my $r := subst($a, /\s+/, ' ', :global);
        $r    := subst($r, /^^\s*/, '');
        $r    := subst($r, /\s*$$/, '');
        return $r;
    }
    our sub table($/) {
        my @rows := [];
        for $<table_row> {
            @rows.push($_.ast);
        }
        @rows := process_rows(@rows);
        # we need 3 informations about the separators:
        #   how many of them are
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

        my $past;
        if $sepnum == 0 {
            my $content := build_table_content(@rows);
            $past := $*ST.add_constant(
                'Pod::Block::Table', 'type_new',
                :content($content<compile_time_value>),
            );
        } elsif $sepnum == 1 {
            $past := $*ST.add_constant('Pod::Block::Table', 'type_new');
            if $firstsepindex == 1 {
                my @head := [];
                for @rows.shift {
                    my $p := $*ST.add_constant('Str', 'str', ~$_);
                    @head.push($p<compile_time_value>);
                }
                my $headpast := $*ST.add_constant(
                    'Array', 'type_new', |@head
                );
                @rows.shift; # remove the separator
                my $content := build_table_content(@rows);
                $past := $*ST.add_constant(
                    'Pod::Block::Table', 'type_new',
                    :headers($headpast<compile_time_value>),
                    :content($content<compile_time_value>),
                );
            } else {
                my $i := 0;
                my @hlines := [];
                while $i < $firstsepindex {
                    @hlines.push(@rows.shift);
                    $i := $i + 1;
                }
                my @newrows := merge_rows(@hlines);
                my @hpast   := [];
                for @newrows {
                    my $p := $*ST.add_constant('Str', 'str', $_);
                    @hpast.push($p<compile_time_value>);
                }
                my $headers := $*ST.add_constant(
                    'Array', 'type_new', |@hpast
                );
                @rows.shift; # remove the separator
                my $content := build_table_content(@rows);

                $past := $*ST.add_constant(
                    'Pod::Block::Table', 'type_new',
                    :headers($headers<compile_time_value>),
                    :content($content<compile_time_value>),
                );
            }
        } else {
            my @headers := [];
            my $i := 0;
            while $i < $firstsepindex {
                @headers.push(@rows.shift);
                $i := $i + 1;
            }
            @rows.shift;
            @headers  := merge_rows(@headers);
            my @hpast := [];
            for @headers {
                my $p := $*ST.add_constant('Str', 'str', $_);
                @hpast.push($p<compile_time_value>);
            }
            my $headpast := $*ST.add_constant(
                    'Array', 'type_new', |@hpast
            );
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
            my $content := build_table_content(@newrows);
            $past := $*ST.add_constant(
                'Pod::Block::Table', 'type_new',
                :headers($headpast<compile_time_value>),
                :content($content<compile_time_value>),
            );
        }
        make $past<compile_time_value>;
    }

    our sub build_table_content(@rows) {
        my @content := [];
        for @rows -> $row {
            my @cells := [];
            for $row -> $cell {
                my $p := $*ST.add_constant('Str', 'str', ~$cell);
                @cells.push($p<compile_time_value>);
            }
            my $p := $*ST.add_constant('Array', 'type_new', |@cells);
            @content.push($*ST.scalar_wrap($p<compile_time_value>));
        }
        return $*ST.add_constant('Array', 'type_new', |@content);
    }

    our sub process_rows(@rows) {
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
                    :ratchet
                    ([<!before \h+'|'\h+> .]*)
                    ** [ [\h+ || ^^] '|' [\h+ || $$] ]
                /;
                @res[$i] := $m[0];
            } elsif $v ~~ /\h'+'\h/ {
                my $m := $v ~~ /
                    :ratchet
                    ([<!before \h+'+'\h+> .]*)
                    ** [ [\h+ || ^^] '+' [\h+ || $$] ]
                /;
                @res[$i] := $m[0];
            } else {
                # quite a special case
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
    # takes an array of strings (rows of a table)
    # returns array of arrays of strings (cells)
    our sub splitrows(@rows) {
        my @suspects := []; #positions that might be cell delimiters
                            # values: 1     – impossibru!
                            #         unset – mebbe

        my $i := 0;
        while $i < +@rows {
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
}

# vim: ft=perl6
