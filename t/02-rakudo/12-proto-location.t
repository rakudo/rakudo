use Test;

plan 4;

# A proto with an onlystar body compiles to a frame whose only code is the
# dispatch instruction, so the code object's reported location comes from
# the position attached to that body.
proto sub zp(|) {*}
multi sub zp() { 1 }

is &zp.file.IO.absolute, $?FILE.IO.absolute,
    'an onlystar proto reports the file it was declared in';
is &zp.line, 8, 'an onlystar proto reports the line it was declared on';

# The setting's own onlystar protos carry the SETTING:: file prefix that
# Routine.IS-SETTING-ONLY-D relies on, which the smartmatch dispatcher
# consults to keep junction autothreading on definite type matches.
ok Str.^find_method('ACCEPTS').IS-SETTING-ONLY-D,
    'the ACCEPTS proto is recognized as setting-only';
is-deeply <a b c>.all ~~ Str:D, True,
    'a junction smartmatched against a definite type autothreads';

# vim: expandtab shiftwidth=4
