# Invoke a method on a possibly foreign object.  If the object
# supports the requested method, we use it, otherwise we assume
# the object is foreign and try using the corresponding method
# from C<Any>.

.namespace []
.sub '!CALLMETHOD'
    .param string method
    .param pmc obj
    $I0 = can obj, method
    unless $I0 goto any_method
    .tailcall obj.method()
  any_method:
    .local pmc anyobj
    anyobj = get_global '$!ANY'
    unless null anyobj goto any_method_1
    anyobj = new ['Any']
    set_global '$!ANY', anyobj
  any_method_1:
    $P0 = find_method anyobj, method
    .tailcall obj.$P0()
.end
