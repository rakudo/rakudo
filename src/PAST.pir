## $Id$

=head1 NAME

Perl6::PAST - Abstract syntax tree nodes for Perl6

=head1 DESCRIPTION

This file implements the various abstract syntax tree nodes
needed for Perl 6.  The currently defined ast nodes:

    Perl6::PAST::Node       - base class for all ast nodes

=head1 METHODS

=over 4

=cut

.namespace [ 'Perl6::PAST' ]

.sub '__onload' :load :init
    .local pmc base
    base = newclass 'Perl6::PAST::Node'
    addattribute base, '@.children'
    addattribute base, '$.source'
    addattribute base, '$.pos'
    addattribute base, '$.name'

    $P0 = subclass base, 'Perl6::PAST::Op'

    $P0 = subclass base, 'Perl6::PAST::Val'
    addattribute $P0, '$.valtype'

    $P0 = subclass base, 'Perl6::PAST::Var'
    addattribute $P0, '$.scope'

    $P0 = subclass base, 'Perl6::PAST::Block'
    addattribute $P0, '$.outer'
    addattribute $P0, '$.blocktype'
    addattribute $P0, '%.vardecl'

    $P0 = subclass base, 'Perl6::PAST::Exp'
    $P0 = subclass base, 'Perl6::PAST::Stmt'
    $P0 = subclass base, 'Perl6::PAST::Stmts'

    $P0 = new .Integer
    $P0 = 10
    store_global '$!serno', $P0
    .return ()
.end


.namespace [ 'Perl6::PAST::Node' ]

.sub 'attr' :method
    .param string attrname
    .param pmc value
    .param int setvalue
    if setvalue goto set
    value = getattribute self, attrname
    unless null value goto end
    value = new .Undef
  set:
    setattribute self, attrname, value
  end:
    .return (value)
.end


.sub 'init' :method
    .param pmc children        :slurpy
    .param pmc adverbs         :slurpy :named

    unless null children goto set_children
    children = new .ResizablePMCArray
  set_children:
    setattribute self, '@.children', children

    if null adverbs goto end
    .local pmc iter
    iter = new .Iterator, adverbs
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    if $S0 == 'XXX' goto iter_loop
    $P0 = iter[$S0]
    $P1 = find_method self, $S0
    self.$P1($P0)
    goto iter_loop
  iter_end:
  end:
    .return ()
.end


.sub 'new' :method
    .param string class
    .param pmc children        :slurpy
    .param pmc adverbs         :slurpy :named

    $I0 = find_type class
    $P0 = new $I0
    $P0.'init'(children :flat, 'node'=>self, 'XXX'=>1, adverbs :flat :named)
    .return ($P0)
.end


.sub 'add_child' :method
    .param pmc child
    .local pmc array
    array = getattribute self, '@.children'
    push array, child
    .return ()
.end


.sub 'add_child_new' :method
    .param string class
    .param pmc children        :slurpy
    .param pmc adverbs         :slurpy :named
    $P0 = self.'new'(class, children :flat, 'XXX'=>0, adverbs :flat :named)
    self.'add_child'($P0)
    .return ($P0)
.end


.sub 'source' :method
    .param string source       :optional
    .param int has_source      :opt_flag
    .return self.'attr'('$.source', source, has_source)
.end


.sub 'pos' :method
    .param int pos             :optional
    .param int has_pos         :opt_flag
    .return self.'attr'('$.pos', pos, has_pos)
.end


.sub 'name' :method
    .param string name         :optional
    .param int has_name        :opt_flag
    .return self.'attr'('$.name', name, has_name)
.end


.sub 'node' :method
    .param pmc node
    $I0 = isa node, 'Perl6::PAST::Node'
    if $I0 goto clone_past
  clone_pge:
    $S0 = node
    self.'source'($S0)
    $I0 = node.'from'()
    self.'pos'($I0)
    .return ()
  clone_past:
    $S0 = node.'source'()
    self.'source'($S0)
    $I0 = node.'pos'()
    self.'pos'($I0)
    .return ()
.end


.sub 'child_iter' :method
    $P0 = getattribute self, '@.children'
    $P1 = new .Iterator, $P0
    $P1 = 0
    .return ($P1)
.end


=item C<Perl6::PAST::Node::unique([string fmt])>

Each call to C<unique> returns a unique number, or if a C<fmt>
parameter is given it returns a unique string beginning with
C<fmt>.  (This may eventually be generalized to allow
uniqueness anywhere in the string.)  The function starts
counting at 10 (so that the values 0..9 can be considered "safe").

=cut

.sub 'unique' :method
    .param string fmt          :optional
    .param int has_fmt         :opt_flag

    if has_fmt goto unique_1
    fmt = ''
  unique_1:
    $P0 = find_global 'Perl6::PAST', '$!serno'
    $S0 = $P0
    $S0 = concat fmt, $S0
    inc $P0
    .return ($S0)
.end


.sub '__elements' :method
    $P0 = getattribute self, '@.children'
    $I0 = elements $P0
    .return ($I0)
.end


.sub '__get_pmc_keyed_int' :method
    .param int key
    $P0 = getattribute self, '@.children'
    $P0 = $P0[key]
    .return ($P0)
.end


.sub '__set_pmc_keyed_int' :method
    .param int key
    .param pmc val
    $P0 = getattribute self, '@.children'
    $P0[key] = val
    .return ()
.end


.sub '__dumplist' :method
    .return ('$.pos $.name @.children')
.end


.sub '__dump' :method
    .param pmc dumper
    .param string label
    .local string indent, subindent

    (subindent, indent) = dumper.'newIndent'()
    print '=> { '
    .local pmc attrlist, iter
    $S0 = self.'__dumplist'()
    attrlist = split ' ', $S0
    iter = new .Iterator, attrlist
  iter_loop:
    unless iter goto iter_end
    .local string attrname
    .local pmc val
    attrname = shift iter
    val = getattribute self, attrname
    print "\n"
    print subindent
    print attrname
    print ' => '
    dumper.'dump'(label, val)
    goto iter_loop
  iter_end:
    print "\n"
    print indent
    print '}'
    dumper.'deleteIndent'()
    .return ()
.end


.namespace [ 'Perl6::PAST::Op' ]


.sub '__dumplist' :method
    .return ('$.name @.children')
.end


.namespace [ 'Perl6::PAST::Val' ]

.sub 'valtype' :method
    .param string valtype      :optional
    .param int has_valtype     :opt_flag
    .return self.'attr'('$.valtype', valtype, has_valtype)
.end

.sub '__dumplist' :method
    .return ('$.name $.valtype')
.end


.namespace [ 'Perl6::PAST::Var' ]

.sub 'scope' :method
    .param string scope      :optional
    .param int has_scope     :opt_flag
    .return self.'attr'('$.scope', scope, has_scope)
.end

.sub '__dumplist' :method
    .return ('$.name $.scope')
.end


.namespace [ 'Perl6::PAST::Block' ]

.sub '__init' :method
    null $P0
    setattribute self, '$.outer', $P0
    $P0 = new .Hash
    setattribute self, '%.vardecl', $P0
    .return ()
.end

.sub 'outer' :method
    .param pmc outer           :optional
    .param int has_outer       :opt_flag
    .return self.'attr'('$.outer', outer, has_outer)
.end

.sub 'blocktype' :method
    .param pmc blocktype       :optional
    .param int has_blocktype   :opt_flag
    .return self.'attr'('$.blocktype', blocktype, has_blocktype)
.end

.sub 'vardecl' :method
    .param pmc name            :optional
    .param int has_name        :opt_flag
    .param pmc value           :optional
    .param int has_value       :opt_flag
    .local pmc vardecl
    vardecl = getattribute self, '%.vardecl'
    if has_value goto with_value
    if has_name goto with_name
    .return (vardecl)
  with_name:
    value = vardecl[name]
    .return (value)
  with_value:
    vardecl[name] = value
    .return (value)
.end

.sub '__dumplist' :method
    .return ('$.name $.outer $.blocktype @.children %.vardecl')
.end
