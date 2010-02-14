.sub '&prefix:<~>'
    .param string a
    $P0 = new ['Str']
    assign $P0, a
    .return ($P0)
.end

.sub '&infix:<~|>'
    .param string a
    .param string b
    $S0 = bors a, b
    .return ($S0)
.end

.sub '&infix:<~&>'
    .param string a
    .param string b
    $S0 = bands a, b
    .return ($S0)
.end

.sub '&infix:<~^>'
    .param string a
    .param string b
    $S0 = bxors a, b
    .return ($S0)
.end
