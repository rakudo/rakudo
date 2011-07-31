module Pod::To::Text;

sub pod2text($pod) is export {
    given $pod {
        when Pod::Heading      { heading2text($pod)             }
        when Pod::Block::Code  { code2text($pod)                }
        when Pod::Block::Named { named2text($pod)               }
        when Pod::Item         { item2text($pod)                }
        when Positional        { $pod.map({pod2text($_)}).join  }
        default                { $pod.Str ~ "\n\n"              }
    }
}

sub heading2text($pod) {
    given $pod.level {
        when 1  {          pod2text($pod.content) }
        when 2  { '  '   ~ pod2text($pod.content) }
        default { '    ' ~ pod2text($pod.content) }
    }
}

sub code2text($pod) {
    "    " ~ $pod.content.subst(/\n/, "\n    ", :g) ~ "\n\n"
}

sub item2text($pod) {
    ' * ' ~ $pod.content.Str ~ "\n"
}

sub named2text($pod) {
    $pod.name eq 'pod'
        ?? pod2text($pod.content)
        !! $pod.content ~ "\n"
}

# vim: ft=perl6
