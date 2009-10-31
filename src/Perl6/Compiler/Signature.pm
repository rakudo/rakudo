# Copyright (C) 2009, The Perl Foundation.
# $Id$

class Perl6::Compiler::Signature;

# This class represents a signature in the compiler. It takes care of
# producing an AST that will generate the signature, based upon all of
# the various bits of information it is provided up to that point. The
# motivation for this is making actions.pm simpler, but also to allow
# the underlying signature construction mechanism to change more easily.
# It will also allow more efficient code generation.

# Note that NQP does not yet support accessing attributes or declaring
# them, so we have a little inline PIR and also we create this class at
# first elsewhere.

has $!entries;
has $!default_type;


# Adds a parameter to the signature.
method add_parameter($new_entry) {
    my @entries := self.entries;
    if $new_entry.var_name eq '$_' {
        @entries.unshift($new_entry); # Always comes first, e.g. before slurpies.
    }
    else {
        @entries.push($new_entry);
    }
}


# Sets the default type of the parameters.
method set_default_parameter_type($type_name) {
    $!default_type := $type_name;
}


# Gets the default type of the parameters.
method get_default_parameter_type() {
    $!default_type || 'Object'
}


# Gets a PAST::Op node with children being PAST::Var nodes that declare the
# various variables mentioned within the signature, with a valid viviself to
# make sure they are initialized either to the default value or an empty
# instance of the correct type.
method get_declarations() {
    my $result := PAST::Op.new( :pasttype('stmts') );
    my @entries := self.entries;
    for @entries {
        # If the parameter has a name, add it.
        if $_.var_name {
            my $var := PAST::Var.new(
                :name($_.var_name),
                :scope('lexical'),
                :isdecl(1)
            );
            $result.push($var);
        }

        # Check any sub-signatures.
        # XXX TODO
    }
    return $result;
}


# Produces an AST for generating a low-level signature object. Optionally can
# instead produce code to generate a high-level signature object.
method ast($high_level?) {
    my $ast     := PAST::Stmts.new();
    my @entries := self.entries;
    my $SIG_ELEM_BIND_CAPTURE       := 1;
    my $SIG_ELEM_BIND_PRIVATE_ATTR  := 2;
    my $SIG_ELEM_BIND_PUBLIC_ATTR   := 4;
    my $SIG_ELEM_SLURPY_POS         := 8;
    my $SIG_ELEM_SLURPY_NAMED       := 16;
    my $SIG_ELEM_SLURPY_BLOCK       := 32;
    my $SIG_ELEM_INVOCANT           := 64;
    my $SIG_ELEM_MULTI_INVOCANT     := 128;
    my $SIG_ELEM_IS_RW              := 256;
    my $SIG_ELEM_IS_COPY            := 512;
    my $SIG_ELEM_IS_REF             := 1024;
    my $SIG_ELEM_IS_OPTIONAL        := 2048;
    my $SIG_ELEM_ARRAY_SIGIL        := 4096;
    my $SIG_ELEM_HASH_SIGIL         := 8192;
    
    # Allocate a signature and stick it in a register.
    my $sig_var := PAST::Var.new( :name($ast.unique('signature_')), :scope('register') );
    $ast.push(PAST::Op.new(
        :pasttype('bind'),
        PAST::Var.new( :name($sig_var.name()), :scope('register'), :isdecl(1) ),
        PAST::Op.new( :inline('    %r = allocate_signature ' ~ +@entries) )
    ));

    # We'll likely also find a register holding a null value helpful to have.
    $ast.push(PAST::Op.new( :inline('    null $P0') ));
    my $null_reg := PAST::Var.new( :name('$P0'), :scope('register') );

    # For each of the parameters, emit a call to add the parameter.
    my $i := 0;
    for @entries {
        # First, compute flags.
        my $flags := 0;
        if $_.optional                  { $flags := $flags + $SIG_ELEM_IS_OPTIONAL; }
        if $_.pos_slurpy                { $flags := $flags + $SIG_ELEM_SLURPY_POS; }
        if $_.named_slurpy              { $flags := $flags + $SIG_ELEM_SLURPY_NAMED; }
        if $_.sigil eq '@'              { $flags := $flags + $SIG_ELEM_ARRAY_SIGIL; }
        if $_.sigil eq '%'              { $flags := $flags + $SIG_ELEM_HASH_SIGIL; }
        if $_.invocant                  { $flags := $flags + $SIG_ELEM_INVOCANT; }
        #if $_<multi_invocant> ne "0"    { $flags := $flags + $SIG_ELEM_MULTI_INVOCANT; }
        #if $_<slurpy> && $sigil ne '@' && $sigil ne '%' { } # XXX TODO: Slurpy block.
        #if $_<read_type> eq 'rw'        { $flags := $flags + $SIG_ELEM_IS_RW; }
        #if $_<read_type> eq 'copy'      { $flags := $flags + $SIG_ELEM_IS_COPY; }
        #if $_<twigil> eq '!'            { $flags := $flags + $SIG_ELEM_BIND_PRIVATE_ATTR }
        #if $_<twigil> eq '.'            {
        #    # Set flag, and we'll pull the sigil and twigil off to leave us
        #    # with the method name.
        #    $flags := $flags + $SIG_ELEM_BIND_PUBLIC_ATTR;
        #    $_<var_name> := substr($_<var_name>, 2);
        #}

        # Fix up nominal type.
        my $nom_type := $null_reg;
        if $_.pos_slurpy || $_.named_slurpy || $_.invocant {
            $nom_type := PAST::Var.new( :name('Object'), :scope('package') );
        }
        elsif $_.sigil eq "$" {
            if !$_.nom_type {
                $nom_type := PAST::Var.new(
                    :name(self.get_default_parameter_type()),
                    :scope('package')
                );
            }
             else {
                $nom_type := $_.nom_type;
             }
        }
#        elsif $sigil ne "" && !$_<invocant> {
#            # May well be a parametric role based type.
#            my $role_name;
#            if    $sigil eq "@" { $role_name := "Positional" }
#            elsif $sigil eq "%" { $role_name := "Associative" }
#            elsif $sigil ne ":" { $role_name := "Callable" }
#            if $role_name {
#                my $role_type := PAST::Var.new( :name($role_name), :namespace(list()), :scope('package') );
#                if !$_<nom_type> {
#                    $_<nom_type> := $role_type;
#                }
#                else {
#                    $_<nom_type> := PAST::Op.new(
#                        :pasttype('callmethod'),
#                        :name('!select'),
#                        $role_type,
#                        $_<nom_type>
#                    );
#                }
#            }
#        }

        # Constraints list needs to build a ResizablePMCArray.
        my $constraints := $null_reg;
        if +@($_.cons_types) {
            $constraints := PAST::Op.new( );
            my $pir := "    %r = root_new ['parrot'; 'ResizablePMCArray']\n";
            my $i := 0;
            for @($_.cons_types) {
                $pir := $pir ~ "    push %r, %" ~ $i ~ "\n";
                $constraints.push($_);
            }
            $constraints.inline($pir);
        }

        # Names and type capture lists needs to build a ResizableStringArray.
        my $names := $null_reg;
        if +@($_.names) {
            my $pir := "    %r = root_new ['parrot'; 'ResizableStringArray']\n";
            for @($_.names) { $pir := $pir ~ '    push %r, unicode:"' ~ ~$_ ~ "\"\n"; }
            $names := PAST::Op.new( :inline($pir) );
        }
        my $type_captures := $null_reg;
        if +@($_.type_captures) {
            my $pir := "    %r = root_new ['parrot'; 'ResizableStringArray']\n";
            for @($_.type_captures) { $pir := $pir ~ '    push %r, unicode:"' ~ ~$_ ~ "\"\n"; }
            $type_captures := PAST::Op.new( :inline($pir) );
        }

        # Fix up sub-signature AST.
        my $sub_sig := $null_reg;
        if pir::defined__IP($_.sub_signature) {
            $sub_sig := PAST::Stmts.new();
            $sub_sig.push( $_.sub_signature.ast );
            $sub_sig.push( PAST::Var.new( :name('signature'), :scope('register') ) );
        }

        # Emit op to build signature element.
        $ast.push(PAST::Op.new(
            :pirop('set_signature_elem vPisiPPPPPP'),
            $sig_var,
            $i,
            ~$_.var_name,
            $flags,
            $nom_type,
            $constraints,
            $names,
            $type_captures,
            ($_.default ?? $_.default !! $null_reg),
            $sub_sig
        ));
        $i := $i + 1;
    }

    # If we had to build a high-level signature, do so.
    if ($high_level) {
        $ast.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            PAST::Var.new( :name('Signature'),, :scope('package') ),
            PAST::Var.new( :name($sig_var.name()), :scope('register'), :named('ll_sig') )
        ));
    }
    else {
        $ast.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('signature'), :scope('register'), :isdecl(1) ),
            $sig_var
        ));
    }

    return $ast;
}


# Accessor for entries in the signature object.
method entries() {
    unless $!entries { $!entries := Q:PIR { %r = new ['ResizablePMCArray'] } }
    $!entries
}



#############################################################################
####################### CODE TO RE-WRITE IN NG BRANCH #######################
#############################################################################

# As for add_parameter, but puts it into position relative to the other
# positional parameters.
method add_placeholder_parameter(*%new_entry) {
    my @entries := self.entries;
    if +@entries == 0 { @entries.push(%new_entry); return 1; }
    my @temp := list();
    while +@entries && @entries[0]<var_name> lt %new_entry<var_name> && !@entries[0]<names> && !@entries[0]<slurpy> {
        @temp.unshift(@entries.shift);
    }
    @entries.unshift(%new_entry);
    for @temp { @entries.unshift($_); }
}


# Adds an invocant to the signature, if it does not already have one.
method add_invocant() {
    my @entries := self.entries;
    if +@entries == 0 || !@entries[0]<invocant> {
        my $param := Q:PIR{ %r = new ['Hash'] };
        $param<var_name> := "self";
        $param<invocant> := 1;
        $param<multi_invocant> := 1;
        $param<names> := list();
        @entries.unshift($param);
    }
}


# Sets all parameters without an explicit read type to default to rw.
method set_rw_by_default() {
    my @entries := self.entries;
    for @entries {
        unless $_<read_type> {
            $_<read_type> := 'rw';
        }
    }
}


# Checks if the signature contains a named slurpy parameter.
method has_named_slurpy() {
    my @entries := self.entries;
    unless +@entries { return 0; }
    my $last := @entries[ +@entries - 1 ];
    return $last<slurpy> && $last<names> ?? 1 !! 0;
}


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
