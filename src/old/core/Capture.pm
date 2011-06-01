augment class Capture {
    method perl {
        '\('  ~   join(', ', @(self)>>.perl, %(self)>>.perl ) ~ ')'
    }
}
