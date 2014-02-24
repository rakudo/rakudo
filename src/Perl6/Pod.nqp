# various helper methods for Pod parsing and processing
class Perl6::Pod {
    our sub document($/, $what, $with) {
        if ~$with ne '' {
            $*W.apply_trait($/, '&trait_mod:<is>', $what, :docs($*DOCEE));
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
            @children.push($_.ast);
        }

        my $content := serialize_array(@children);
        if $leveled {
            my $level := nqp::substr($<type>.Str, 4);
            my $level_past;
            if $level ne '' {
                $level_past := $*W.add_constant(
                    'Int', 'int', +$level,
                ).compile_time_value;
            } else {
                $level_past := $*W.find_symbol(['Mu']);
            }

            my $past := serialize_object(
                $type, :level($level_past), :config($config),
                :content($content.compile_time_value)
            );
            return $past.compile_time_value;
        }

        my $name := $*W.add_constant('Str', 'str', $<type>.Str);
        my $past := serialize_object(
            'Pod::Block::Named', :name($name.compile_time_value),
            :config($config), :content($content.compile_time_value),
        );
        return $past.compile_time_value;
    }

    our sub raw_block($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;
        my $str := $*W.add_constant('Str', 'str', ~$<pod_content>);
        my $content := serialize_array([$str.compile_time_value]);
        my $past := serialize_object(
            'Pod::Block::Comment', :config($config),
            :content($content.compile_time_value),
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
            } else {
                # and this is the worst hack of them all.
                # Hide your kids, hide your wife!
                my $truth := nqp::substr($colonpair, 1, 1) ne '!';

                $val := $*W.add_constant('Int', 'int', $truth).compile_time_value;
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
            $val := $*W.add_constant('Str', 'str', $val).compile_time_value;
            @pairs.push(
                serialize_object(
                    'Pair', :key($key), :value($val)
                ).compile_time_value
            );
        }
        return serialize_object('Hash', |@pairs).compile_time_value;
    }

    our sub formatted_text($a) {
        my $r := subst($a, /\s+/, ' ', :global);
        $r    := subst($r, /^^\s*/, '');
        $r    := subst($r, /\s*$$/, '');
        return $r;
    }
    our sub table($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;

        if +@*text-pieces {
            for @*text-pieces[0] {
                my @t := [];
                @t.push($_.shift) for @*text-pieces;
                @*content.push(@t);
            }
        }
        make serialize_object(
            'Pod::Block::Table', :config($config),
            :headers(@*headers), :content(@*content),
        ).compile_time_value;
    }

    our sub insert_column_part($content, $column, $columns-fixed = 0) {
        my $idx := 0;
        for @*columns -> $pos {
            if $pos == $column {
                @*text-pieces[$idx].push: $content;
                return;
            } elsif $pos > $column && !$columns-fixed {
                @*text-pieces.splice($idx, 0, [$content]);
                @*columns.splice($idx, 0, $column);
                return;
            } elsif $pos > $column && $columns-fixed {
                # maybe the user wanted to use multiple spaces to align.
                # that's no problem if our columns are already fixed,
                # because we already know the exact layout of columns.
                my @lr := @*text-pieces[$idx - 1];
                @lr[@lr - 1].push($_) for $content;
                return;
            }
        }
        if !$columns-fixed {
            @*text-pieces.push([$content]);
            @*columns.push($column);
        }
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

    # takes an array of strings (rows of a table)
    # returns array of arrays of strings (cells)
    our sub splitrows(@rows) {
        my @suspects := []; #positions that might be cell delimiters
                            # values: 1     - impossibru!
                            #         unset - mebbe

        my $i := 0;
        while $i < +@rows {
            unless @rows[$i] ~~ /^'='+ || ^'-'+ || ^'_'+ || ^\h*$ / {
                my @line := nqp::split('', @rows[$i]);
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
}

# vim: ft=perl6
