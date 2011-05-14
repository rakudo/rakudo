.sub '&prefix:<~>'
    .param string a
    x_enter_sublog
    $P0 = new ['Str']
    assign $P0, a
    .return ($P0)
.end
