# This module exports a single subroutine named "SL" (for Spesh Log). When
# this sub is called, returns a MoarVM::Spesh object that can be queried
# for the result of the Spesh Log for the executed program.  Or it returns
# Nil when it is being run inside the program producing the Spesh Log.
#
# When introspection of the MoarVM::Spesh object is done, the "exit" method
# should be called on the object to prevent the code of the program actually
# running twice.
# 
# A typical use case would be:
#
#  use MoarVM::SL;
#
#  if SL() -> $SL {
#      # do testing, or just show the report:
#      say $SL.report;
#      $SL.exit;
#  }

use MoarVM::Spesh;

sub SL is export {

    my $filename = $*SPEC.tmpdir.add("speshlog-$*PID");
    LEAVE unlink $filename;

    my $proc := Rakudo::Internals.RERUN-WITH(MVM_SPESH_LOG => $filename);
    return Nil unless $proc;

    my $out := $*OUT;
    my $err := $*ERR;
    my $status;

    react {
        whenever $proc.stdout.lines {
            $out.say($_);
        }
        whenever $proc.stderr.lines {
            $err.say($_);
        }
        whenever $proc.start(:%*ENV) {
            $status = .exitcode;
        }
    }

    MoarVM::Spesh.new($filename) but role {
        method exit() { exit $status }
    }
}

# vim: expandtab shiftwidth=4
