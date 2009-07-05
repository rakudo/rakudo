
multi trait_mod:<is>(Object $child, Object $parent) {
    Q:PIR {
    .local pmc child, parent
    child = find_lex '$child'
    parent = find_lex '$parent'

    # Do we have a role here?
    $I0 = isa parent, 'Role'
    if $I0 goto need_to_pun
    $I0 = isa parent, 'Perl6Role'
    if $I0 goto need_to_pun_role
    goto have_class
  need_to_pun_role:
    parent = parent.'!select'()
  need_to_pun:
    parent = parent.'!pun'()
  have_class:

    # Now the the real parrot class and add parent.
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parent = p6meta.'get_parrotclass'(parent)
    child.'add_parent'(parent)
    };
}

multi trait_mod:<is>(Code $block, $arg?, :$export!) {
    # Maybe we can re-write some more of this out of PIR and into Perl 6.
    Q:PIR {
    .local pmc block, arg
    block = find_lex '$block'
    arg = find_lex '$arg'

    # Multis that are exported need to be whole-sale exported as multis.
    .local pmc blockns
    .local string blockname
    block = descalarref block
    blockns = block.'get_namespace'()
    blockname = block
    $P0 = blockns[blockname]
    $I0 = isa $P0, 'MultiSub'
    unless $I0 goto multi_handled
    block = $P0
  multi_handled:

    .local pmc exportns
    exportns = blockns.'make_namespace'('EXPORT')
    unless arg goto default_export
    .local pmc it
    $I0 = arg.'elems'()
    if $I0 goto have_arg
  default_export:
    $P0 = get_hll_global 'Pair'
    $P0 = $P0.'new'('key' => 'DEFAULT', 'value' => 1)
    arg = 'list'($P0)
  have_arg:
    it = iter arg
  arg_loop:
    unless it goto arg_done
    .local pmc tag, ns
    tag = shift it
    $I0 = isa tag, ['Perl6Pair']
    unless $I0 goto arg_loop
    $S0 = tag.'key'()
    ns = exportns.'make_namespace'($S0)
    ns[blockname] = block
    goto arg_loop
  arg_done:
    ns = exportns.'make_namespace'('ALL')
    ns[blockname] = block
    }
}

multi trait_mod:<is>(Code $block, :$default!) {
    Q:PIR {
        $P0 = find_lex '$block'
        $P0 = descalarref $P0
        $P1 = box 1
        setprop $P0, 'default', $P1
    };
}

multi trait_mod:<does>(Object $class is rw, Object $role) {
    Q:PIR {
    .local pmc metaclass, role
    metaclass = find_lex "$class"
    metaclass = descalarref metaclass
    role = find_lex "$role"
    role = descalarref role

    # XXX For now, we can't multi-dispatch differentiate classes
    # and scalars. :-( So we'll have to do check here.
    $I0 = isa metaclass, 'Class'
    if $I0 goto is_class
    $I0 = isa metaclass, 'P6role'
    if $I0 goto is_class
    $I0 = isa metaclass, 'Perl6Role'
    if $I0 goto is_class
    'infix:does'(metaclass, role)
    .return ()
  is_class:

    # If it's an un-selected role, do so.
    $I0 = isa role, 'P6role'
    if $I0 goto have_role
    role = role.'!select'()
  have_role:
    # Now add it to the list of roles to compose into the class.
    .local pmc role_list
    role_list = getprop '@!roles', metaclass
    unless null role_list goto have_role_list
    role_list = root_new ['parrot';'ResizablePMCArray']
    setprop metaclass, '@!roles', role_list
  have_role_list:
    push role_list, role
    };
}

multi trait_mod:<does>(Array $var, $role) {
    $var does $role;
}

multi trait_mod:<does>(Hash $var, $role) {
    $var does $role;
}

multi trait_mod:<of>(Code $block is rw, Object $type is rw) {
    $block does Callable[$type];
}

multi trait_mod:<of>(Array $var is rw, Object $type is rw) {
    $var does Positional[$type];
}

multi trait_mod:<of>(Hash $var is rw, Object $type is rw) {
    $var does Associative[$type];
}

multi trait_mod:<of>(Object $var is rw, Object $type is rw) {
    Q:PIR {
    .local pmc var, type
    var = find_lex '$var'
    type = find_lex '$type'
    setprop var, 'type', type
    };
}

multi trait_mod:<returns>(Code $block is rw, Object $type) {
    &trait_mod:<of>($block, $type);
}

multi trait_mod:<will>($declarand, &arg, *%name) {
    &trait_mod:<is>($declarand, &arg, |%name);
}
