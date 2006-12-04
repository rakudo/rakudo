module lib-0.0.1;
use v6;

our @ORIG_INC = @*INC;  # take a copy of the original

sub import (Str $pkg: *@_paths) returns Void {
    my @paths = @_paths.reverse();
    for @paths -> $path {
        if ($path eq '') {
            $*ERR.say("Empty compile time value given to lib.import()");
        }
        if (-e $path && !-d $path) {
            $*ERR.say("Parameter to lib.import() must be directory, not file");
        }
        # add to the @*INC, but do not allow duplicates
        @*INC.unshift($path) unless $path eq any(@*INC);
    }
}

# I am purposfully leaving out the archname, version_dir, version_arch_dir
# and inc_version_list code from lib.pm because I am hoping that perl6
# and the new module functionality will not need it. Althoug this may be a 
# niave understanding of what this stuff is for, so if it is, I invite 
# others to fix it.

=kwid

= NAME

lib - A pragma for addition of paths to @*INC

= SYNOPSIS

  require lib;
  import('lib': @paths);
  
  # no load your other modules ...

= DESCRIPTION

This is a small simple module which simplifies the 
manipulation of `@*INC`.

It is typically used to add extra directories to Pugs's 
search path so that later `use` or `require` statements 
will find modules which are not located on Pugs's default
search path.

= LIMITATIONS & CAVEATS

Currently this will only work during runtime since Pugs 
does not yet support `BEGIN{}` or `use`. Modules are 
loaded in a /first-come-first-served/ basis, so just be
sure (as with perl5 lib.pm) to use this module first.

= SEE ALSO

`perldoc lib`

= AUTHOR

Stevan Little <stevan@iinteractive.com>

= COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
