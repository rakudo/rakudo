use v6;

use Test;

plan 28;

for &TRANSPOSE, &TRANSPOSE-ONE -> &trans {
    my $name = &trans.name;
    is trans("foof","f","a"),    "aooa", "$name foof,f,a  -> aooa";
    is trans("foof","f","ab"), "abooab", "$name foof,f,ab -> abooab";
    is trans("foof","f","ff"), "ffooff", "$name foof,f,ff -> ffooff";
    is trans("foof","f",""),       "oo", "$name foof,f,   -> oo";

    is trans("offo","f","a"),    "oaao", "$name offo,f,a  -> oaao";
    is trans("offo","f","ab"), "oababo", "$name offo,f,ab -> oababo";
    is trans("offo","f","ff"), "offffo", "$name offo,f,ff -> offffo";
    is trans("offo","f",""),       "oo", "$name offo,f,   -> oo";
}

my $name = "TRANSPOSE";
is TRANSPOSE("foof","fo","a"),   "aof", "$name foof,fo,a  -> aof";
is TRANSPOSE("foof","fo","ab"), "abof", "$name foof,fo,ab -> abof";
is TRANSPOSE("foof","fo","ff"), "ffof", "$name foof,fo,ff -> ffof";
is TRANSPOSE("foof","fo",""),     "of", "$name foof,fo,   -> of";

is TRANSPOSE("offo","fo","a"),   "ofa", "$name offo,fo,a  -> ofa";
is TRANSPOSE("offo","fo","ab"), "ofab", "$name offo,fo,ab -> ofab";
is TRANSPOSE("offo","fo","ff"), "offf", "$name offo,fo,ff -> offf";
is TRANSPOSE("offo","fo",""),     "of", "$name offo,fo,   -> of";

is TRANSPOSE("ofof","fo","a"),   "oaf", "$name ofof,fo,a  -> oaf";
is TRANSPOSE("ofof","fo","ab"), "oabf", "$name ofof,fo,ab -> oabf";
is TRANSPOSE("ofof","fo","ff"), "offf", "$name ofof,fo,ff -> offf";
is TRANSPOSE("ofof","fo",""),     "of", "$name offo,fo,   -> of";
