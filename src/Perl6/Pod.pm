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
}

# vim: ft=perl6
