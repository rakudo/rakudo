class Pod::To::Text;

method render($pod) {
    pod2text($pod)
}

my &colored;
if %*ENV<POD_TO_TEXT_ANSI> {
    &colored = try { EVAL q{ use Term::ANSIColor; &colored } } // sub ($text, $color) { $text }
} else {
    &colored = sub ($text, $color) { $text }
}

sub pod2text($pod) is export {
    given $pod {
        when Pod::Heading      { heading2text($pod)             }
        when Pod::Block::Code  { code2text($pod)                }
        when Pod::Block::Named { named2text($pod)               }
        when Pod::Block::Para  { $pod.contents.map({pod2text($_)}).join("") }
        when Pod::Block::Table { table2text($pod)               }
        when Pod::Block::Declarator { declarator2text($pod)     }
        when Pod::Item         { item2text($pod).indent(2)      }
        when Pod::FormattingCode { formatting2text($pod)        }
        when Positional        { $pod.map({pod2text($_)}).join("\n\n")}
        when Pod::Block::Comment { }
        when Pod::Config       { }
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
    for 0..(@rows[1].elems - 1) -> $i {
        @maxes.push([max] @rows.map({ $_[$i].chars }));
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
    return $ret;
}

sub declarator2text($pod) {
    next unless $pod.WHEREFORE.WHY;
    my $what = do given $pod.WHEREFORE {
        when Method {
            my @params=$_.signature.params[1..*];
              @params.pop if @params[*-1].name eq '%_';
            'method ' ~ $_.name ~ signature2text(@params)
        }
        when Sub {
            'sub ' ~ $_.name ~ signature2text($_.signature.params)
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::ClassHOW)) {
            'class ' ~ $_.perl
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::ModuleHOW)) {
            'module ' ~ $_.perl
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::PackageHOW)) {
            'package ' ~ $_.perl
        }
        default {
            ''
        }
    }
    return "$what\n{$pod.WHEREFORE.WHY.contents}"
}

sub signature2text($params) {
      $params.elems ??
      "(\n\t" ~ $params.map({ $_.perl }).join(", \n\t") ~ "\n)" 
      !! "()";
}

my %formats =
  C => "bold",
  L => "underline",
  D => "underline",
  R => "inverse"
;

sub formatting2text($pod) {
    my $text = $pod.contents>>.&pod2text.join;
    if $pod.type ~~ %formats {
        return colored($text, %formats{$pod.type});
    } elsif $pod.type ~~ 'P' {
        my $path;
        my $contents=$pod.contents;
        my $sth = $pod.contents.subst(/['doc' | 'file']\:/,"");
        if $contents ~~ /doc/ {
            $path = @*INC.>>.candidates($sth).path;
        } else {
            $path = $sth;
        }
        return qqx {$*EXECUTABLE_NAME --doc $path};
    }
    $text
}

sub twine2text($twine) {
    return '' unless $twine.elems;
    my $r = $twine[0];
    for $twine[1..*] -> $f, $s {
        $r ~= twine2text($f.contents);
        $r ~= $s;
    }
    return $r;
}

# vim: ft=perl6
