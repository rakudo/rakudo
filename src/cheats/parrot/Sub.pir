.HLL 'parrot'

.namespace ['Sub']
.sub '!llsig' :method
    .local pmc llsig, lazysig
    llsig = getprop '$!llsig', self
    unless null llsig goto done
    lazysig = getprop '$!lazysig', self
    llsig = lazysig()
    setprop self, '$!llsig', llsig
  done:
    .return (llsig)
.end


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
