# XXX This wants to be more general eventually, when we support custom
# metaclasses.
subset Class of Object where ClassHOW | RoleHOW;

multi trait_mod:<is>(Class $child, Object $parent) {
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
    child = getattribute child, 'parrotclass'
    child.'add_parent'(parent)
    };
}

multi trait_mod:<is>(Class $child, :$hidden!) {
    $child.hidden = True;
}

multi trait_mod:<is>(Code $block, :$default!) {
    Q:PIR {
        $P0 = find_lex '$block'
        $P0 = descalarref $P0
        $P1 = box 1
        setprop $P0, 'default', $P1
    };
}

multi trait_mod:<is>(ContainerDeclarand $c, :$rw!) {
    # The default anyway, so nothing to do.
}

multi trait_mod:<does>(Class $class is rw, Object $role) {
    Q:PIR {
    .local pmc metaclass, role
    metaclass = find_lex "$class"
    metaclass = descalarref metaclass
    role = find_lex "$role"
    role = descalarref role

    # If it's an un-selected role, do so.
    $I0 = isa role, 'P6role'
    if $I0 goto have_role
    role = role.'!select'()
  have_role:
    # Now add it to the list of roles to compose into the class.
    .local pmc role_list
    metaclass = getattribute metaclass, 'parrotclass'
    role_list = getprop '@!roles', metaclass
    unless null role_list goto have_role_list
    role_list = root_new ['parrot';'ResizablePMCArray']
    setprop metaclass, '@!roles', role_list
  have_role_list:
    push role_list, role
    };
}

multi trait_mod:<does>(ContainerDeclarand $c, Object $role) {
    $c.container does $role;
}

multi trait_mod:<of>(Code $block is rw, Object $type is rw) {
    $block does Callable[$type];
}

multi trait_mod:<of>(ContainerDeclarand $c, Object $type is rw) {
    given $c.container {
        when Array { $_ does Positional[$type] }
        when Hash { $_ does Associative[$type] }
        default { VAR($_).of($type) }
    }
}

multi trait_mod:<returns>(Code $block is rw, Object $type) {
    &trait_mod:<of>($block, $type);
}

multi trait_mod:<will>($declarand, &arg, *%name) {
    &trait_mod:<is>($declarand, &arg, |%name);
}

multi trait_mod:<hides>(Class $child, Object $parent) {
    &trait_mod:<is>($child, $parent);
    $child.hides.push($parent)
}
