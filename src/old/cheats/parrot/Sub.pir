.HLL 'parrot'

.namespace ['Sub']
.sub '!get_code' :method
    .param pmc codetype
    .param pmc lazysig         :optional
    .param pmc multi           :optional

    .local pmc code
    code = getprop '$!p6code', self
    unless null code goto code_done
    $P0 = codetype.'HOW'()
    $P0 = getattribute $P0, 'parrotclass'
    code = new $P0
    transform_to_p6opaque code
    setattribute code, '$!do', self
    setprop self, '$!p6code', code
    if null lazysig goto lazysig_done
    setprop self, '$!lazysig', lazysig
  lazysig_done:
    if null multi goto multi_done
    setprop self, '$!multi', multi
    setattribute code, '$!multi', multi
    if multi != 2 goto multi_done
    $P0 = box 1
    setprop code, 'proto', $P0
  multi_done:
  code_done:
    .return (code)
.end

.namespace ['Sub']
.sub '!get_closure' :method
    .param pmc codetype
    .param pmc lazysig         :optional
    .param pmc multi           :optional

    .local pmc code
    code = getprop '$!p6code', self
    unless null code goto code_done
    code = self.'!get_code'(codetype, lazysig, multi)
  code_done:
    .local pmc closure, do
    $P0 = typeof code
    closure = new $P0
    transform_to_p6opaque closure
    do = getattribute code, '$!do'
    do = clone do
    $P0 = prophash self
    x_setprophash do, $P0
    setattribute closure, '$!do', do
    $P0 = getattribute code, '$!multi'
    setattribute closure, '$!multi', $P0
    $P0 = getprop 'proto', code
    setprop closure, 'proto', $P0
    .return (closure)
.end

.namespace ['Sub']
.sub '!llsig' :method
    .local pmc llsig, lazysig
    llsig = getprop '$!llsig', self
    unless null llsig goto done
    lazysig = getprop '$!lazysig', self
    if null lazysig goto err_llsig
    llsig = lazysig()
    setprop self, '$!llsig', llsig
  done:
    .return (llsig)
  err_llsig:
    $P0 = root_new ['parrot';'ResizableStringArray']
    push $P0, 'Sub'
    push $P0, self
    push $P0, ' (subid "'
    $S1 = self.'get_subid'()
    push $P0, $S1
    push $P0, '") has no $!llsig and no $!lazysig'
    $S0 = join '', $P0
    die $S0
.end


.namespace ['Sub']
.sub '!signature' :method
    .local pmc llsig, signature
    signature = getprop '$!signature', self
    unless null signature goto done
    llsig = self.'!llsig'()
    $P0 = get_root_global [.RAKUDO_HLL], 'Signature'
    signature = $P0.'new'('llsig'=>llsig)
    setprop self, '$!signature', signature
  done:
    .return (signature)
.end
