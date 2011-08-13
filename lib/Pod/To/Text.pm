module Pod::To::Text;

sub pod2text($pod) is export {
    my @declarators;
    given $pod {
        when Pod::Heading      { heading2text($pod)             }
        when Pod::Block::Code  { code2text($pod)                }
        when Pod::Block::Named { named2text($pod)               }
        when Pod::Block::Para  { para2text($pod)                }
        when Pod::Block::Table { table2text($pod)               }
        when Pod::Block::Declarator { declarator2text($pod)     }
        when Pod::Item         { item2text($pod)                }
        when Positional        { $pod.map({pod2text($_)}).join("\n\n")}
        when Pod::Block::Comment { }
        default                { $pod.Str                       }
    }
}

sub heading2text($pod) {
    given $pod.level {
        when 1  {          pod2text($pod.content)  }
        when 2  { '  '   ~ pod2text($pod.content)  }
        default { '    ' ~ pod2text($pod.content)  }
    }
}

sub code2text($pod) {
    "    " ~ $pod.content.subst(/\n/, "\n    ", :g)
}

sub item2text($pod) {
    ' * ' ~ pod2text($pod.content).chomp.chomp
}

sub named2text($pod) {
    given $pod.name {
        when 'pod'  { pod2text($pod.content)     }
        when 'para' { para2text($pod.content[0]) }
        when 'defn' { pod2text($pod.content[0]) ~ "\n"
                    ~ pod2text($pod.content[1..*-1]) }
        when 'config' { }
        when 'nested' { }
        default     { $pod.name ~ "\n" ~ pod2text($pod.content) }
    }
}

sub para2text($pod) {
    $pod.content.join("\n")
}

sub table2text($pod) {
    ($pod.caption // '')
    ~ ($pod.headers ?? $pod.headers.join("\t") ~ "\n" !! '')
    ~ ($pod.content.map({ $_.join("\t") }).join("\n"))
}

sub declarator2text($pod) {
    next unless $pod.WHEREFORE.WHY;
    my $type = do given $pod.WHEREFORE {
        when Method {
            'method'
        }
        when Sub {
            'sub'
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::ClassHOW)) {
            'class'
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::ModuleHOW)) {
            'module'
        }
        when nqp::p6bool(nqp::istype($_.HOW, Metamodel::PackageHOW)) {
            'package'
        }
        default {
            ''
        }
    }
    return "$type {$pod.WHEREFORE.perl}: {$pod.WHEREFORE.WHY}"
}

# vim: ft=perl6
