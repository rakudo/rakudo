class CompUnit::Repository::Spec {
    has $.short-id;
    has %.options;
    has $.path;
    has $.Str;
    method from-string(Str:D $spec, :$default-short-id = 'file') {
        return unless $spec.chars;
        # something we understand
        if $spec.contains('#') {
            if $spec ~~ /^
              <before .>
              [
                $<type>=[ <.ident>+ % '::' ]
                $<options>=[ '#' $<option-name>=\w+
                  <[ < ( [ { ]> $<option-value>=<[\w-]>+? <[ > ) \] } ]>
                ]*
                '#'
              ]?
              $<path>=.*
            $/ {
                my $short-id := ~($<type> // $default-short-id);
                my $path := $*SPEC.canonpath(~$<path>);
                self.new(
                    :$short-id,
                    :options(%($<option-name>>>.Str Z=> $<option-value>>>.Str)),
                    :$path,
                    :Str($short-id ~ $<options> ~ '#' ~ $path)
                );
            }
        }
        else {
            my $path := $*SPEC.canonpath($spec);
            self.new(:short-id($default-short-id), :$path, :Str($default-short-id ~ '#' ~ $path))
        }
    }
}

# vim: expandtab shiftwidth=4
