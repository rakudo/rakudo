# method cheats for Mu

# most of these can potentially go into CORE, when we have 'augment' working.

.namespace ['Mu']

.sub 'print' :method
    $P0 = get_hll_global '&print'
    .tailcall $P0(self)
.end

.sub 'say' :method
    $P0 = get_hll_global '&say'
    .tailcall $P0(self)
.end

.namespace []

.sub '&prefix:<defined>'
    .param pmc x
    $I0 = defined x
    .return ($I0)
.end


