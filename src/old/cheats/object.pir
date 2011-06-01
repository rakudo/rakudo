# method cheats for Mu

.namespace []

.sub '&prefix:<defined>'
    .param pmc x
    $I0 = defined x
    .tailcall '&prefix:<?>'($I0)
.end


