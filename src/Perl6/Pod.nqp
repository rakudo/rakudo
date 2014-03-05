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
            unless nqp::islist(@rows[$i]) {
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
                if nqp::islist(@rows[$i]) {
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
            :headers(serialize_aos($headers).compile_time_value),
            :content(serialize_aoaos($content).compile_time_value),
        );
        make $past.compile_time_value;
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
                    :ratchet
                    ([<!before [\h+ || ^^] '|' [\h+ || $$]> .]*)+
                    % [ [\h+ || ^^] '|' [\h || $$] ]
                /;
                @res[$i] := [];
                for $m[0] { @res[$i].push(formatted_text($_)) }
            } elsif $v ~~ /\h'+'\h/ {
                my $m := $v ~~ /
                    :ratchet ([<!before [\h+ || ^^] '+' [\h+ || $$]> .]*)+
                    % [ [\h+ || ^^] '+' [\h+ || $$] ]
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

    # all-lowercase HTML5 character entities, derived from
    # https://github.com/w3c/html/blob/master/entities.json
    my %entities;
    # NQP doesn't support assignment of entire hashes :(
    # so we have to iterate
    for 'aacute',[225],'aacute',[225],'abreve',[259],'ac',[8766],'acd',[8767],'acirc',[226],
        'acirc',[226],'acute',[180],'acute',[180],'acy',[1072],'aelig',[230],'aelig',[230],
        'af',[8289],'afr',[120094],'agrave',[224],'agrave',[224],'alefsym',[8501],'aleph',[8501],
        'alpha',[945],'amacr',[257],'amalg',[10815],'amp',[38],'amp',[38],'andand',[10837],
        'and',[8743],'andd',[10844],'andslope',[10840],'andv',[10842],'ang',[8736],'ange',[10660],
        'angle',[8736],'angmsdaa',[10664],'angmsdab',[10665],'angmsdac',[10666],'angmsdad',[10667],'angmsdae',[10668],
        'angmsdaf',[10669],'angmsdag',[10670],'angmsdah',[10671],'angmsd',[8737],'angrt',[8735],'angrtvb',[8894],
        'angrtvbd',[10653],'angsph',[8738],'angst',[197],'angzarr',[9084],'aogon',[261],'aopf',[120146],
        'apacir',[10863],'ap',[8776],'ape',[8778],'apid',[8779],'apos',[39],'approx',[8776],
        'approxeq',[8778],'aring',[229],'aring',[229],'ascr',[119990],'ast',[42],'asymp',[8776],
        'asympeq',[8781],'atilde',[227],'atilde',[227],'auml',[228],'auml',[228],'awconint',[8755],
        'awint',[10769],'backcong',[8780],'backepsilon',[1014],'backprime',[8245],'backsim',[8765],'backsimeq',[8909],
        'barvee',[8893],'barwed',[8965],'barwedge',[8965],'bbrk',[9141],'bbrktbrk',[9142],'bcong',[8780],
        'bcy',[1073],'bdquo',[8222],'becaus',[8757],'because',[8757],'bemptyv',[10672],'bepsi',[1014],
        'bernou',[8492],'beta',[946],'beth',[8502],'between',[8812],'bfr',[120095],'bigcap',[8898],
        'bigcirc',[9711],'bigcup',[8899],'bigodot',[10752],'bigoplus',[10753],'bigotimes',[10754],'bigsqcup',[10758],
        'bigstar',[9733],'bigtriangledown',[9661],'bigtriangleup',[9651],'biguplus',[10756],'bigvee',[8897],'bigwedge',[8896],
        'bkarow',[10509],'blacklozenge',[10731],'blacksquare',[9642],'blacktriangle',[9652],'blacktriangledown',[9662],'blacktriangleleft',[9666],
        'blacktriangleright',[9656],'blank',[9251],'block',[9608],'bne',[61,8421],'bnequiv',[8801,8421],'bnot',[8976],
        'bopf',[120147],'bot',[8869],'bottom',[8869],'bowtie',[8904],'boxbox',[10697],'boxdl',[9488],
        'boxdr',[9484],'boxh',[9472],'boxhd',[9516],'boxhu',[9524],'boxminus',[8863],'boxplus',[8862],
        'boxtimes',[8864],'boxul',[9496],'boxur',[9492],'boxv',[9474],'boxvh',[9532],'boxvl',[9508],
        'boxvr',[9500],'bprime',[8245],'breve',[728],'brvbar',[166],'brvbar',[166],'bscr',[119991],
        'bsemi',[8271],'bsim',[8765],'bsime',[8909],'bsolb',[10693],'bsol',[92],'bsolhsub',[10184],
        'bull',[8226],'bullet',[8226],'bump',[8782],'bumpe',[8783],'bumpeq',[8783],'cacute',[263],
        'capand',[10820],'capbrcup',[10825],'capcap',[10827],'cap',[8745],'capcup',[10823],'capdot',[10816],
        'caps',[8745,65024],'caret',[8257],'caron',[711],'ccaps',[10829],'ccaron',[269],'ccedil',[231],
        'ccedil',[231],'ccirc',[265],'ccups',[10828],'ccupssm',[10832],'cdot',[267],'cedil',[184],
        'cedil',[184],'cemptyv',[10674],'cent',[162],'cent',[162],'centerdot',[183],'cfr',[120096],
        'chcy',[1095],'check',[10003],'checkmark',[10003],'chi',[967],'circ',[710],'circeq',[8791],
        'circlearrowleft',[8634],'circlearrowright',[8635],'circledast',[8859],'circledcirc',[8858],'circleddash',[8861],'cir',[9675],
        'cire',[8791],'cirfnint',[10768],'cirmid',[10991],'cirscir',[10690],'clubs',[9827],'clubsuit',[9827],
        'colon',[58],'colone',[8788],'coloneq',[8788],'comma',[44],'commat',[64],'comp',[8705],
        'compfn',[8728],'complement',[8705],'complexes',[8450],'cong',[8773],'congdot',[10861],'conint',[8750],
        'copf',[120148],'coprod',[8720],'copy',[169],'copy',[169],'copysr',[8471],'crarr',[8629],
        'cross',[10007],'cscr',[119992],'csub',[10959],'csube',[10961],'csup',[10960],'csupe',[10962],
        'ctdot',[8943],'cudarrl',[10552],'cudarrr',[10549],'cuepr',[8926],'cuesc',[8927],'cularr',[8630],
        'cularrp',[10557],'cupbrcap',[10824],'cupcap',[10822],'cup',[8746],'cupcup',[10826],'cupdot',[8845],
        'cupor',[10821],'cups',[8746,65024],'curarr',[8631],'curarrm',[10556],'curlyeqprec',[8926],'curlyeqsucc',[8927],
        'curlyvee',[8910],'curlywedge',[8911],'curren',[164],'curren',[164],'curvearrowleft',[8630],'curvearrowright',[8631],
        'cuvee',[8910],'cuwed',[8911],'cwconint',[8754],'cwint',[8753],'cylcty',[9005],'dagger',[8224],
        'daleth',[8504],'darr',[8595],'dash',[8208],'dashv',[8867],'dbkarow',[10511],'dblac',[733],
        'dcaron',[271],'dcy',[1076],'ddagger',[8225],'ddarr',[8650],'dd',[8518],'ddotseq',[10871],
        'deg',[176],'deg',[176],'delta',[948],'demptyv',[10673],'dfisht',[10623],'dfr',[120097],
        'dharl',[8643],'dharr',[8642],'diam',[8900],'diamond',[8900],'diamondsuit',[9830],'diams',[9830],
        'die',[168],'digamma',[989],'disin',[8946],'div',[247],'divide',[247],'divide',[247],
        'divideontimes',[8903],'divonx',[8903],'djcy',[1106],'dlcorn',[8990],'dlcrop',[8973],'dollar',[36],
        'dopf',[120149],'dot',[729],'doteq',[8784],'doteqdot',[8785],'dotminus',[8760],'dotplus',[8724],
        'dotsquare',[8865],'doublebarwedge',[8966],'downarrow',[8595],'downdownarrows',[8650],'downharpoonleft',[8643],'downharpoonright',[8642],
        'drbkarow',[10512],'drcorn',[8991],'drcrop',[8972],'dscr',[119993],'dscy',[1109],'dsol',[10742],
        'dstrok',[273],'dtdot',[8945],'dtri',[9663],'dtrif',[9662],'duarr',[8693],'duhar',[10607],
        'dwangle',[10662],'dzcy',[1119],'dzigrarr',[10239],'eacute',[233],'eacute',[233],'easter',[10862],
        'ecaron',[283],'ecirc',[234],'ecirc',[234],'ecir',[8790],'ecolon',[8789],'ecy',[1101],
        'edot',[279],'ee',[8519],'efr',[120098],'eg',[10906],'egrave',[232],'egrave',[232],
        'egs',[10902],'egsdot',[10904],'el',[10905],'elinters',[9191],'ell',[8467],'els',[10901],
        'elsdot',[10903],'emacr',[275],'empty',[8709],'emptyset',[8709],'emptyv',[8709],'emsp',[8195],
        'eng',[331],'ensp',[8194],'eogon',[281],'eopf',[120150],'epar',[8917],'eparsl',[10723],
        'eplus',[10865],'epsi',[949],'epsilon',[949],'epsiv',[1013],'eqcirc',[8790],'eqcolon',[8789],
        'eqsim',[8770],'eqslantgtr',[10902],'eqslantless',[10901],'equals',[61],'equest',[8799],'equiv',[8801],
        'eqvparsl',[10725],'erarr',[10609],'escr',[8495],'esdot',[8784],'esim',[8770],'eta',[951],
        'eth',[240],'eth',[240],'euml',[235],'euml',[235],'euro',[8364],'excl',[33],
        'exist',[8707],'expectation',[8496],'exponentiale',[8519],'fallingdotseq',[8786],'fcy',[1092],'female',[9792],
        'ffilig',[64259],'fflig',[64256],'ffllig',[64260],'ffr',[120099],'filig',[64257],'fjlig',[102,106],
        'flat',[9837],'fllig',[64258],'fltns',[9649],'fnof',[402],'fopf',[120151],'forall',[8704],
        'fork',[8916],'forkv',[10969],'fpartint',[10765],'frasl',[8260],'frown',[8994],'fscr',[119995],
        'gacute',[501],'gamma',[947],'gammad',[989],'gap',[10886],'gbreve',[287],'gcirc',[285],
        'gcy',[1075],'gdot',[289],'ge',[8805],'gel',[8923],'geq',[8805],'geqq',[8807],
        'geqslant',[10878],'gescc',[10921],'ges',[10878],'gesdot',[10880],'gesdoto',[10882],'gesdotol',[10884],
        'gesl',[8923,65024],'gesles',[10900],'gfr',[120100],'gg',[8811],'ggg',[8921],'gimel',[8503],
        'gjcy',[1107],'gla',[10917],'gl',[8823],'glj',[10916],'gnap',[10890],'gnapprox',[10890],
        'gne',[10888],'gneq',[10888],'gneqq',[8809],'gnsim',[8935],'gopf',[120152],'grave',[96],
        'gscr',[8458],'gsim',[8819],'gsime',[10894],'gsiml',[10896],'gtcc',[10919],'gtcir',[10874],
        'gt',[62],'gt',[62],'gtdot',[8919],'gtquest',[10876],'gtrapprox',[10886],'gtrarr',[10616],
        'gtrdot',[8919],'gtreqless',[8923],'gtreqqless',[10892],'gtrless',[8823],'gtrsim',[8819],'gvertneqq',[8809,65024],
        'hairsp',[8202],'half',[189],'hamilt',[8459],'hardcy',[1098],'harrcir',[10568],'harr',[8596],
        'harrw',[8621],'hbar',[8463],'hcirc',[293],'hearts',[9829],'heartsuit',[9829],'hellip',[8230],
        'hercon',[8889],'hfr',[120101],'hksearow',[10533],'hkswarow',[10534],'hoarr',[8703],'homtht',[8763],
        'hookleftarrow',[8617],'hookrightarrow',[8618],'hopf',[120153],'horbar',[8213],'hscr',[119997],'hslash',[8463],
        'hstrok',[295],'hybull',[8259],'hyphen',[8208],'iacute',[237],'iacute',[237],'ic',[8291],
        'icirc',[238],'icirc',[238],'icy',[1080],'iecy',[1077],'iexcl',[161],'iexcl',[161],
        'iff',[8660],'ifr',[120102],'igrave',[236],'igrave',[236],'ii',[8520],'iiiint',[10764],
        'iiint',[8749],'iinfin',[10716],'iiota',[8489],'ijlig',[307],'imacr',[299],'image',[8465],
        'imagline',[8464],'imagpart',[8465],'imath',[305],'imof',[8887],'imped',[437],'incare',[8453],
        'in',[8712],'infin',[8734],'infintie',[10717],'inodot',[305],'intcal',[8890],'int',[8747],
        'integers',[8484],'intercal',[8890],'intlarhk',[10775],'intprod',[10812],'iocy',[1105],'iogon',[303],
        'iopf',[120154],'iota',[953],'iprod',[10812],'iquest',[191],'iquest',[191],'iscr',[119998],
        'isin',[8712],'isindot',[8949],'isins',[8948],'isinsv',[8947],'isinv',[8712],'it',[8290],
        'itilde',[297],'iukcy',[1110],'iuml',[239],'iuml',[239],'jcirc',[309],'jcy',[1081],
        'jfr',[120103],'jmath',[567],'jopf',[120155],'jscr',[119999],'jsercy',[1112],'jukcy',[1108],
        'kappa',[954],'kappav',[1008],'kcedil',[311],'kcy',[1082],'kfr',[120104],'kgreen',[312],
        'khcy',[1093],'kjcy',[1116],'kopf',[120156],'kscr',[120000],'lacute',[314],'laemptyv',[10676],
        'lagran',[8466],'lambda',[955],'lang',[10216],'langd',[10641],'langle',[10216],'lap',[10885],
        'laquo',[171],'laquo',[171],'larrb',[8676],'larrbfs',[10527],'larr',[8592],'larrfs',[10525],
        'larrhk',[8617],'larrlp',[8619],'larrpl',[10553],'larrsim',[10611],'larrtl',[8610],'latail',[10521],
        'lat',[10923],'late',[10925],'lates',[10925,65024],'lbarr',[10508],'lbbrk',[10098],'lbrace',[123],
        'lbrack',[91],'lbrke',[10635],'lbrksld',[10639],'lbrkslu',[10637],'lcaron',[318],'lcedil',[316],
        'lceil',[8968],'lcub',[123],'lcy',[1083],'ldca',[10550],'ldquo',[8220],'ldquor',[8222],
        'ldrdhar',[10599],'ldrushar',[10571],'ldsh',[8626],'le',[8804],'leftarrow',[8592],'leftarrowtail',[8610],
        'leftharpoondown',[8637],'leftharpoonup',[8636],'leftleftarrows',[8647],'leftrightarrow',[8596],'leftrightarrows',[8646],'leftrightharpoons',[8651],
        'leftrightsquigarrow',[8621],'leftthreetimes',[8907],'leg',[8922],'leq',[8804],'leqq',[8806],'leqslant',[10877],
        'lescc',[10920],'les',[10877],'lesdot',[10879],'lesdoto',[10881],'lesdotor',[10883],'lesg',[8922,65024],
        'lesges',[10899],'lessapprox',[10885],'lessdot',[8918],'lesseqgtr',[8922],'lesseqqgtr',[10891],'lessgtr',[8822],
        'lesssim',[8818],'lfisht',[10620],'lfloor',[8970],'lfr',[120105],'lg',[8822],'lhard',[8637],
        'lharu',[8636],'lharul',[10602],'lhblk',[9604],'ljcy',[1113],'llarr',[8647],'ll',[8810],
        'llcorner',[8990],'llhard',[10603],'lltri',[9722],'lmidot',[320],'lmoustache',[9136],'lmoust',[9136],
        'lnap',[10889],'lnapprox',[10889],'lne',[10887],'lneq',[10887],'lneqq',[8808],'lnsim',[8934],
        'loang',[10220],'loarr',[8701],'lobrk',[10214],'longleftarrow',[10229],'longleftrightarrow',[10231],'longmapsto',[10236],
        'longrightarrow',[10230],'looparrowleft',[8619],'looparrowright',[8620],'lopar',[10629],'lopf',[120157],'loplus',[10797],
        'lotimes',[10804],'lowast',[8727],'lowbar',[95],'loz',[9674],'lozenge',[9674],'lozf',[10731],
        'lpar',[40],'lparlt',[10643],'lrarr',[8646],'lrcorner',[8991],'lrhar',[8651],'lrhard',[10605],
        'lrm',[8206],'lrtri',[8895],'lsaquo',[8249],'lscr',[120001],'lsh',[8624],'lsim',[8818],
        'lsime',[10893],'lsimg',[10895],'lsqb',[91],'lsquo',[8216],'lsquor',[8218],'lstrok',[322],
        'ltcc',[10918],'ltcir',[10873],'lt',[60],'lt',[60],'ltdot',[8918],'lthree',[8907],
        'ltimes',[8905],'ltlarr',[10614],'ltquest',[10875],'ltri',[9667],'ltrie',[8884],'ltrif',[9666],
        'lurdshar',[10570],'luruhar',[10598],'lvertneqq',[8808,65024],'macr',[175],'macr',[175],'male',[9794],
        'malt',[10016],'maltese',[10016],'map',[8614],'mapsto',[8614],'mapstodown',[8615],'mapstoleft',[8612],
        'mapstoup',[8613],'marker',[9646],'mcomma',[10793],'mcy',[1084],'mdash',[8212],'measuredangle',[8737],
        'mfr',[120106],'mho',[8487],'micro',[181],'micro',[181],'midast',[42],'midcir',[10992],
        'mid',[8739],'middot',[183],'middot',[183],'minusb',[8863],'minus',[8722],'minusd',[8760],
        'minusdu',[10794],'mlcp',[10971],'mldr',[8230],'mnplus',[8723],'models',[8871],'mopf',[120158],
        'mp',[8723],'mscr',[120002],'mstpos',[8766],'mu',[956],'multimap',[8888],'mumap',[8888],
        'nabla',[8711],'nacute',[324],'nang',[8736,8402],'nap',[8777],'napid',[8779,824],'napos',[329],
        'napprox',[8777],'natural',[9838],'naturals',[8469],'natur',[9838],'nbsp',[160],'nbsp',[160],
        'nbump',[8782,824],'nbumpe',[8783,824],'ncap',[10819],'ncaron',[328],'ncedil',[326],'ncong',[8775],
        'ncongdot',[10861,824],'ncup',[10818],'ncy',[1085],'ndash',[8211],'nearhk',[10532],'nearr',[8599],
        'nearrow',[8599],'ne',[8800],'nedot',[8784,824],'nequiv',[8802],'nesear',[10536],'nesim',[8770,824],
        'nexist',[8708],'nexists',[8708],'nfr',[120107],'nge',[8817],'ngeq',[8817],'ngeqq',[8807,824],
        'ngeqslant',[10878,824],'nges',[10878,824],'ngsim',[8821],'ngt',[8815],'ngtr',[8815],'nharr',[8622],
        'nhpar',[10994],'ni',[8715],'nis',[8956],'nisd',[8954],'niv',[8715],'njcy',[1114],
        'nlarr',[8602],'nldr',[8229],'nle',[8816],'nleftarrow',[8602],'nleftrightarrow',[8622],'nleq',[8816],
        'nleqq',[8806,824],'nleqslant',[10877,824],'nles',[10877,824],'nless',[8814],'nlsim',[8820],'nlt',[8814],
        'nltri',[8938],'nltrie',[8940],'nmid',[8740],'nopf',[120159],'not',[172],'not',[172],
        'notin',[8713],'notindot',[8949,824],'notinva',[8713],'notinvb',[8951],'notinvc',[8950],'notni',[8716],
        'notniva',[8716],'notnivb',[8958],'notnivc',[8957],'nparallel',[8742],'npar',[8742],'nparsl',[11005,8421],
        'npart',[8706,824],'npolint',[10772],'npr',[8832],'nprcue',[8928],'nprec',[8832],'npreceq',[10927,824],
        'npre',[10927,824],'nrarrc',[10547,824],'nrarr',[8603],'nrarrw',[8605,824],'nrightarrow',[8603],'nrtri',[8939],
        'nrtrie',[8941],'nsc',[8833],'nsccue',[8929],'nsce',[10928,824],'nscr',[120003],'nshortmid',[8740],
        'nshortparallel',[8742],'nsim',[8769],'nsime',[8772],'nsimeq',[8772],'nsmid',[8740],'nspar',[8742],
        'nsqsube',[8930],'nsqsupe',[8931],'nsub',[8836],'nsube',[8840],'nsubset',[8834,8402],'nsubseteq',[8840],
        'nsubseteqq',[10949,824],'nsucc',[8833],'nsucceq',[10928,824],'nsup',[8837],'nsupe',[8841],'nsupset',[8835,8402],
        'nsupseteq',[8841],'nsupseteqq',[10950,824],'ntgl',[8825],'ntilde',[241],'ntilde',[241],'ntlg',[8824],
        'ntriangleleft',[8938],'ntrianglelefteq',[8940],'ntriangleright',[8939],'ntrianglerighteq',[8941],'nu',[957],'num',[35],
        'numero',[8470],'numsp',[8199],'nvap',[8781,8402],'nvdash',[8876],'nvge',[8805,8402],'nvgt',[62,8402],
        'nvinfin',[10718],'nvle',[8804,8402],'nvlt',[60,8402],'nvltrie',[8884,8402],'nvrtrie',[8885,8402],'nvsim',[8764,8402],
        'nwarhk',[10531],'nwarr',[8598],'nwarrow',[8598],'nwnear',[10535],'oacute',[243],'oacute',[243],
        'oast',[8859],'ocirc',[244],'ocirc',[244],'ocir',[8858],'ocy',[1086],'odash',[8861],
        'odblac',[337],'odiv',[10808],'odot',[8857],'odsold',[10684],'oelig',[339],'ofcir',[10687],
        'ofr',[120108],'ogon',[731],'ograve',[242],'ograve',[242],'ogt',[10689],'ohbar',[10677],
        'ohm',[937],'oint',[8750],'olarr',[8634],'olcir',[10686],'olcross',[10683],'oline',[8254],
        'olt',[10688],'omacr',[333],'omega',[969],'omicron',[959],'omid',[10678],'ominus',[8854],
        'oopf',[120160],'opar',[10679],'operp',[10681],'oplus',[8853],'orarr',[8635],'or',[8744],
        'ord',[10845],'order',[8500],'orderof',[8500],'ordf',[170],'ordf',[170],'ordm',[186],
        'ordm',[186],'origof',[8886],'oror',[10838],'orslope',[10839],'orv',[10843],'oscr',[8500],
        'oslash',[248],'oslash',[248],'osol',[8856],'otilde',[245],'otilde',[245],'otimesas',[10806],
        'otimes',[8855],'ouml',[246],'ouml',[246],'ovbar',[9021],'para',[182],'para',[182],
        'parallel',[8741],'par',[8741],'parsim',[10995],'parsl',[11005],'part',[8706],'pcy',[1087],
        'percnt',[37],'period',[46],'permil',[8240],'perp',[8869],'pertenk',[8241],'pfr',[120109],
        'phi',[966],'phiv',[981],'phmmat',[8499],'phone',[9742],'pi',[960],'pitchfork',[8916],
        'piv',[982],'planck',[8463],'planckh',[8462],'plankv',[8463],'plusacir',[10787],'plusb',[8862],
        'pluscir',[10786],'plus',[43],'plusdo',[8724],'plusdu',[10789],'pluse',[10866],'plusmn',[177],
        'plusmn',[177],'plussim',[10790],'plustwo',[10791],'pm',[177],'pointint',[10773],'popf',[120161],
        'pound',[163],'pound',[163],'prap',[10935],'pr',[8826],'prcue',[8828],'precapprox',[10935],
        'prec',[8826],'preccurlyeq',[8828],'preceq',[10927],'precnapprox',[10937],'precneqq',[10933],'precnsim',[8936],
        'pre',[10927],'precsim',[8830],'prime',[8242],'primes',[8473],'prnap',[10937],'prnsim',[8936],
        'prod',[8719],'profalar',[9006],'profline',[8978],'profsurf',[8979],'prop',[8733],'propto',[8733],
        'prsim',[8830],'prurel',[8880],'pscr',[120005],'psi',[968],'puncsp',[8200],'qfr',[120110],
        'qint',[10764],'qopf',[120162],'qprime',[8279],'qscr',[120006],'quaternions',[8461],'quatint',[10774],
        'quest',[63],'questeq',[8799],'quot',[34],'quot',[34],'race',[8765,817],'racute',[341],
        'radic',[8730],'raemptyv',[10675],'rang',[10217],'rangd',[10642],'range',[10661],'rangle',[10217],
        'raquo',[187],'raquo',[187],'rarrap',[10613],'rarrb',[8677],'rarrbfs',[10528],'rarrc',[10547],
        'rarr',[8594],'rarrfs',[10526],'rarrhk',[8618],'rarrlp',[8620],'rarrpl',[10565],'rarrsim',[10612],
        'rarrtl',[8611],'rarrw',[8605],'ratail',[10522],'ratio',[8758],'rationals',[8474],'rbarr',[10509],
        'rbbrk',[10099],'rbrace',[125],'rbrack',[93],'rbrke',[10636],'rbrksld',[10638],'rbrkslu',[10640],
        'rcaron',[345],'rcedil',[343],'rceil',[8969],'rcub',[125],'rcy',[1088],'rdca',[10551],
        'rdldhar',[10601],'rdquo',[8221],'rdquor',[8221],'rdsh',[8627],'real',[8476],'realine',[8475],
        'realpart',[8476],'reals',[8477],'rect',[9645],'reg',[174],'reg',[174],'rfisht',[10621],
        'rfloor',[8971],'rfr',[120111],'rhard',[8641],'rharu',[8640],'rharul',[10604],'rho',[961],
        'rhov',[1009],'rightarrow',[8594],'rightarrowtail',[8611],'rightharpoondown',[8641],'rightharpoonup',[8640],'rightleftarrows',[8644],
        'rightleftharpoons',[8652],'rightrightarrows',[8649],'rightsquigarrow',[8605],'rightthreetimes',[8908],'ring',[730],'risingdotseq',[8787],
        'rlarr',[8644],'rlhar',[8652],'rlm',[8207],'rmoustache',[9137],'rmoust',[9137],'rnmid',[10990],
        'roang',[10221],'roarr',[8702],'robrk',[10215],'ropar',[10630],'ropf',[120163],'roplus',[10798],
        'rotimes',[10805],'rpar',[41],'rpargt',[10644],'rppolint',[10770],'rrarr',[8649],'rsaquo',[8250],
        'rscr',[120007],'rsh',[8625],'rsqb',[93],'rsquo',[8217],'rsquor',[8217],'rthree',[8908],
        'rtimes',[8906],'rtri',[9657],'rtrie',[8885],'rtrif',[9656],'rtriltri',[10702],'ruluhar',[10600],
        'rx',[8478],'sacute',[347],'sbquo',[8218],'scap',[10936],'scaron',[353],'sc',[8827],
        'sccue',[8829],'sce',[10928],'scedil',[351],'scirc',[349],'scnap',[10938],'scnsim',[8937],
        'scpolint',[10771],'scsim',[8831],'scy',[1089],'sdotb',[8865],'sdot',[8901],'sdote',[10854],
        'searhk',[10533],'searr',[8600],'searrow',[8600],'sect',[167],'sect',[167],'semi',[59],
        'seswar',[10537],'setminus',[8726],'setmn',[8726],'sext',[10038],'sfr',[120112],'sfrown',[8994],
        'sharp',[9839],'shchcy',[1097],'shcy',[1096],'shortmid',[8739],'shortparallel',[8741],'shy',[173],
        'shy',[173],'sigma',[963],'sigmaf',[962],'sigmav',[962],'sim',[8764],'simdot',[10858],
        'sime',[8771],'simeq',[8771],'simg',[10910],'siml',[10909],'simne',[8774],'simplus',[10788],
        'simrarr',[10610],'slarr',[8592],'smallsetminus',[8726],'smashp',[10803],'smeparsl',[10724],'smid',[8739],
        'smile',[8995],'smt',[10922],'smte',[10924],'smtes',[10924,65024],'softcy',[1100],'solbar',[9023],
        'solb',[10692],'sol',[47],'sopf',[120164],'spades',[9824],'spadesuit',[9824],'spar',[8741],
        'sqcap',[8851],'sqcaps',[8851,65024],'sqcup',[8852],'sqcups',[8852,65024],'sqsub',[8847],'sqsube',[8849],
        'sqsubset',[8847],'sqsubseteq',[8849],'sqsup',[8848],'sqsupe',[8850],'sqsupset',[8848],'sqsupseteq',[8850],
        'square',[9633],'squarf',[9642],'squ',[9633],'squf',[9642],'srarr',[8594],'sscr',[120008],
        'ssetmn',[8726],'ssmile',[8995],'sstarf',[8902],'star',[9734],'starf',[9733],'straightepsilon',[1013],
        'straightphi',[981],'strns',[175],'sub',[8834],'subdot',[10941],'sube',[8838],'subedot',[10947],
        'submult',[10945],'subne',[8842],'subplus',[10943],'subrarr',[10617],'subset',[8834],'subseteq',[8838],
        'subseteqq',[10949],'subsetneq',[8842],'subsetneqq',[10955],'subsim',[10951],'subsub',[10965],'subsup',[10963],
        'succapprox',[10936],'succ',[8827],'succcurlyeq',[8829],'succeq',[10928],'succnapprox',[10938],'succneqq',[10934],
        'succnsim',[8937],'succsim',[8831],'sum',[8721],'sung',[9834],'sup',[8835],'supdot',[10942],
        'supdsub',[10968],'supe',[8839],'supedot',[10948],'suphsol',[10185],'suphsub',[10967],'suplarr',[10619],
        'supmult',[10946],'supne',[8843],'supplus',[10944],'supset',[8835],'supseteq',[8839],'supseteqq',[10950],
        'supsetneq',[8843],'supsetneqq',[10956],'supsim',[10952],'supsub',[10964],'supsup',[10966],'swarhk',[10534],
        'swarr',[8601],'swarrow',[8601],'swnwar',[10538],'szlig',[223],'szlig',[223],'target',[8982],
        'tau',[964],'tbrk',[9140],'tcaron',[357],'tcedil',[355],'tcy',[1090],'tdot',[8411],
        'telrec',[8981],'tfr',[120113],'therefore',[8756],'theta',[952],'thetasym',[977],'thetav',[977],
        'thickapprox',[8776],'thicksim',[8764],'thinsp',[8201],'thkap',[8776],'thksim',[8764],'thorn',[254],
        'thorn',[254],'tilde',[732],'timesbar',[10801],'timesb',[8864],'times',[215],'times',[215],
        'timesd',[10800],'tint',[8749],'toea',[10536],'topbot',[9014],'topcir',[10993],'top',[8868],
        'topf',[120165],'topfork',[10970],'tosa',[10537],'tprime',[8244],'trade',[8482],'triangle',[9653],
        'triangledown',[9663],'triangleleft',[9667],'trianglelefteq',[8884],'triangleq',[8796],'triangleright',[9657],'trianglerighteq',[8885],
        'tridot',[9708],'trie',[8796],'triminus',[10810],'triplus',[10809],'trisb',[10701],'tritime',[10811],
        'trpezium',[9186],'tscr',[120009],'tscy',[1094],'tshcy',[1115],'tstrok',[359],'twixt',[8812],
        'twoheadleftarrow',[8606],'twoheadrightarrow',[8608],'uacute',[250],'uacute',[250],'uarr',[8593],'ubrcy',[1118],
        'ubreve',[365],'ucirc',[251],'ucirc',[251],'ucy',[1091],'udarr',[8645],'udblac',[369],
        'udhar',[10606],'ufisht',[10622],'ufr',[120114],'ugrave',[249],'ugrave',[249],'uharl',[8639],
        'uharr',[8638],'uhblk',[9600],'ulcorn',[8988],'ulcorner',[8988],'ulcrop',[8975],'ultri',[9720],
        'umacr',[363],'uml',[168],'uml',[168],'uogon',[371],'uopf',[120166],'uparrow',[8593],
        'updownarrow',[8597],'upharpoonleft',[8639],'upharpoonright',[8638],'uplus',[8846],'upsi',[965],'upsih',[978],
        'upsilon',[965],'upuparrows',[8648],'urcorn',[8989],'urcorner',[8989],'urcrop',[8974],'uring',[367],
        'urtri',[9721],'uscr',[120010],'utdot',[8944],'utilde',[361],'utri',[9653],'utrif',[9652],
        'uuarr',[8648],'uuml',[252],'uuml',[252],'uwangle',[10663],'vangrt',[10652],'varepsilon',[1013],
        'varkappa',[1008],'varnothing',[8709],'varphi',[981],'varpi',[982],'varpropto',[8733],'varr',[8597],
        'varrho',[1009],'varsigma',[962],'varsubsetneq',[8842,65024],'varsubsetneqq',[10955,65024],'varsupsetneq',[8843,65024],'varsupsetneqq',[10956,65024],
        'vartheta',[977],'vartriangleleft',[8882],'vartriangleright',[8883],'vcy',[1074],'vdash',[8866],'veebar',[8891],
        'vee',[8744],'veeeq',[8794],'vellip',[8942],'verbar',[124],'vert',[124],'vfr',[120115],
        'vltri',[8882],'vnsub',[8834,8402],'vnsup',[8835,8402],'vopf',[120167],'vprop',[8733],'vrtri',[8883],
        'vscr',[120011],'vsubne',[8842,65024],'vsupne',[8843,65024],'vzigzag',[10650],'wcirc',[373],'wedbar',[10847],
        'wedge',[8743],'wedgeq',[8793],'weierp',[8472],'wfr',[120116],'wopf',[120168],'wp',[8472],
        'wr',[8768],'wreath',[8768],'wscr',[120012],'xcap',[8898],'xcirc',[9711],'xcup',[8899],
        'xdtri',[9661],'xfr',[120117],'xharr',[10231],'xi',[958],'xlarr',[10229],'xmap',[10236],
        'xnis',[8955],'xodot',[10752],'xopf',[120169],'xoplus',[10753],'xotime',[10754],'xrarr',[10230],
        'xscr',[120013],'xsqcup',[10758],'xuplus',[10756],'xutri',[9651],'xvee',[8897],'xwedge',[8896],
        'yacute',[253],'yacute',[253],'yacy',[1103],'ycirc',[375],'ycy',[1099],'yen',[165],
        'yen',[165],'yfr',[120118],'yicy',[1111],'yopf',[120170],'yscr',[120014],'yucy',[1102],
        'yuml',[255],'yuml',[255],'zacute',[378],'zcaron',[382],'zcy',[1079],'zdot',[380],
        'zeetrf',[8488],'zeta',[950],'zfr',[120119],'zhcy',[1078],'zigrarr',[8669],'zopf',[120171],
        'zscr',[120015],'zwj',[8205],'zwnj',[8204]
    -> $k, $v { %entities{$k} := $v }
    our sub str_from_entity($e) {
        my $r := '';
        for %entities{$e} // [] {
            $r := $r ~ nqp::chr($_);
        }
        $r
    }
}

# vim: ft=perl6
