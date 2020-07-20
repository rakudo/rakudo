use v6;

use lib <lib>;
use Test;

plan 26;

sub test($string,$from,$to,$result) {
    is Rakudo::Internals.TRANSPOSE($string,$from,$to),$result,
      "TRANSPOSE $string,$from,$to  -> $result";
}

test "foof","f","a",      "aooa";
test "foof","f","ab",   "abooab";
test "foof","f","ff",   "ffooff";
test "foof","f","",         "oo";

test "offo","f","a",      "oaao";
test "offo","f","ab",   "oababo";
test "offo","f","ff",   "offffo";
test "offo","f","",         "oo";

test "foof","fo","a",      "aof";
test "foof","fo","ab",    "abof";
test "foof","fo","ff",    "ffof";
test "foof","fo","",        "of";

test "offo","fo","a",      "ofa";
test "offo","fo","ab",    "ofab";
test "offo","fo","ff",    "offf";
test "offo","fo","",        "of";

test "ofof","fo","a",      "oaf";
test "ofof","fo","ab",    "oabf";
test "ofof","fo","ff",    "offf";
test "ofof","fo","",        "of";

test "oooo","o","",           "";
test "oooo","o","x",      "xxxx";
test "oooo","o","xx", "xxxxxxxx";
test "oooo","x","",       "oooo";
test "oooo","x","y",      "oooo";
test "oooo","x","yy",     "oooo";

# vim: expandtab shiftwidth=4
