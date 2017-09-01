unit class Pod::To::Text;

method render($pod) {
    pod2text($pod)
}

my &colored = sub ($text, $) {$text }
if %*ENV<POD_TO_TEXT_ANSI> {
    (try require Terminal::ANSIColor <&colored>) !=== Nil
        and &OUTER::colored = &colored
};

sub pod2text($pod) is export {
    given $pod {
        when Pod::Heading      { heading2text($pod)             }
        when Pod::Block::Code  { code2text($pod)                }
        when Pod::Block::Named { named2text($pod)               }
        when Pod::Block::Para  { twrap( $pod.contents.map({pod2text($_)}).join("") ) }
        when Pod::Block::Table { table2text($pod)               }
        when Pod::Block::Declarator { declarator2text($pod)     }
        when Pod::Item         { item2text($pod).indent(2)      }
        when Pod::FormattingCode { formatting2text($pod)        }
        when Positional        { .flat».&pod2text.grep(?*).join: "\n\n" }
        when Pod::Block::Comment { '' }
        when Pod::Config       { '' }
        default                { $pod.Str                       }
    }
}

sub heading2text($pod) {
    given $pod.level {
        when 1  {          pod2text($pod.contents)  }
        when 2  { '  '   ~ pod2text($pod.contents)  }
        default { '    ' ~ pod2text($pod.contents)  }
    }
}

sub code2text($pod) {
    "    " ~ $pod.contents>>.&pod2text.subst(/\n/, "\n    ", :g)
}

sub item2text($pod) {
    '* ' ~ pod2text($pod.contents).chomp.chomp
}

sub named2text($pod) {
    given $pod.name {
        when 'pod'  { pod2text($pod.contents)     }
        when 'para' { para2text($pod.contents[0]) }
        when 'defn' { pod2text($pod.contents[0]) ~ "\n"
                    ~ pod2text($pod.contents[1..*-1]) }
        when 'config' { }
        when 'nested' { }
        default     { $pod.name ~ "\n" ~ pod2text($pod.contents) }
    }
}

sub para2text($pod) {
    twine2text($pod.contents)
}

sub table2text($pod) {
    my @rows = $pod.contents;
    @rows.unshift($pod.headers.item) if $pod.headers;
    my @maxes;
    my $cols = [max] @rows.map({ .elems });
    for 0..^$cols -> $i {
        @maxes.push([max] @rows.map({ $i < $_ ?? $_[$i].chars !! 0 }));
    }
    my $ret;
    if $pod.config<caption> {
        $ret = $pod.config<caption> ~ "\n"
    }
    for @rows -> $row {
        for 0..($row.elems - 1) -> $i {
            $ret ~= $row[$i].fmt("%-{@maxes[$i]}s") ~ "  ";
        }
        $ret ~= "\n";
    }
    $ret
}

sub declarator2text($pod) {
    next unless $pod.WHEREFORE.WHY;
    my $what = do given $pod.WHEREFORE {
        when Method {
            my @params=$_.signature.params[1..*];
              @params.pop if @params.tail.name eq '%_';
            'method ' ~ $_.name ~ signature2text(@params)
        }
        when Sub {
            'sub ' ~ $_.name ~ signature2text($_.signature.params)
        }
        when .HOW ~~ Metamodel::ClassHOW {
            'class ' ~ $_.perl
        }
        when .HOW ~~ Metamodel::ModuleHOW {
            'module ' ~ $_.perl
        }
        when .HOW ~~ Metamodel::PackageHOW {
            'package ' ~ $_.perl
        }
        default {
            ''
        }
    }
    "$what\n{$pod.WHEREFORE.WHY.contents}"
}

sub signature2text($params) {
      $params.elems ??
      "(\n\t" ~ $params.map(&param2text).join("\n\t") ~ "\n)"
      !! "()";
}
sub param2text($p) {
    $p.perl ~ ',' ~ ( $p.WHY ?? ' # ' ~ $p.WHY !! ' ')
}

my %formats =
  C => "bold",
  L => "underline",
  D => "underline",
  R => "inverse"
;

sub formatting2text($pod) {
    my $text = $pod.contents>>.&pod2text.join;
    $pod.type ~~ %formats
      ?? colored($text, %formats{$pod.type})
      !! $text
}

sub twine2text($_) {
    .map({ when Pod::Block { twine2text .contents }; .&pod2text }).join
}

sub twrap($text is copy, :$wrap=75 ) {
    $text ~~ s:g/(. ** {$wrap} <[\s]>*)\s+/$0\n/;
    $text
}

# vim: ft=perl6
