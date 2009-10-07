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


# Adds a parameter to the signature.
#  - var_name is the name of the lexical that we bind to, if any
#  - nom_type is the main, nominal type of the parameter
#  - cons_type is (at least for now) a junction of other type constraints
#  - type_captures is a list of any lexical type names bound to the type of
#    the incoming parameter
#  - optional sets if the parameter is optional
#  - slurpy sets if the parameter is slurpy
#  - names is a list of one or more names, if this parameter is a named
#    parameter
# - invocant is set to a true value if the parameter is a method invocant
# - multi_invocant is set to a true value if the parameter should be considered
#   by the multi dispatcher
# - read_type should be one of readonly (the default), rw, copy or ref
# - default should be a PAST::Block that when invoked computes the default
#   value.
method add_parameter(*%new_entry) {
    my @entries := self.entries;
    @entries.push(%new_entry);
}


# Adds an invocant to the signature, if it does not already have one.
method add_invocant() {
    my @entries := self.entries;
    if +@entries == 0 || !@entries[0]<invocant> {
        my $param := Q:PIR{ %r = new ['Hash'] };
        $param<var_name> := "self";
        $param<invocant> := 1;
        $param<multi_invocant> := 1;
        @entries.unshift($param);
    }
}


# Sets the default type of the parameters.
method set_default_parameter_type($type_name) {
    Q:PIR {
        $P0 = find_lex "$type_name"
        setattribute self, '$!default_type', $P0
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


# Produces an AST for generating a low-level signature object. Optionally can
# instead produce code to generate a high-level signature object.
# XXX Or it will when we have the new signature object support. For now, this
# is generating the normal high level signatures, just a tad more efficiently
# than we used to (e.g. we don't go back and add the invocant or tweak things
# again at runtime when building the signature). This is the method that will
# change substantially in the future.
method ast($high_level?) {
    my $ast := PAST::Stmts.new();
    
    # Instantiate signature and stick it in a register.
    $ast.push(PAST::Op.new(
        :pasttype('bind'),
        PAST::Var.new( :name('signature'), :scope('register'), :isdecl(1) ),
        PAST::Op.new( :inline('    %r = new ["Signature"]') )
    ));
    my $sig_var := PAST::Var.new( :name('signature'), :scope('register') );

    # Set default type, if any.
    Q:PIR {
        $P0 = getattribute self, '$!default_type'
        if null $P0 goto default_type_done
    };
    $ast.push(PAST::Op.new(
        :pasttype('callmethod'),
        :name('!set_default_param_type'),
        $sig_var,
        PAST::Var.new( :name(Q:PIR { %r = $P0 }), :namespace(list()), :scope('package') )
    ));
    Q:PIR {
      default_type_done:
    };

    # For each of the parameters, emit a call to add the parameter.
    for self.entries {
        my $add_param := PAST::Op.new(
            :pasttype('callmethod'),
            :name('!add_param'),
            $sig_var,
            ~$_<var_name>
        );
        if $_<optional> { $add_param.push(PAST::Val.new( :value(1), :named('optional') )); }
        if $_<slurpy>   { $add_param.push(PAST::Val.new( :value(1), :named('slurpy') )); }
        if $_<invocant> { $add_param.push(PAST::Val.new( :value(1), :named('invocant') )); }
        if $_<multi_invocant> eq "0" { $add_param.push(PAST::Val.new( :value(0), :named('multi_invocant') )); }
        if $_<nom_type> && !$_<slurpy> {
            $_<nom_type>.named('nom_type');
            $add_param.push($_<nom_type>);
        }
        if $_<cons_type> {
            $_<cons_type>.named('cons_type');
            $add_param.push($_<cons_type>);
        }
        if $_<read_type> {
            $add_param.push(PAST::Val.new( :value(~$_<read_type>), :named('readtype') ));
        }
        if $_<names> {
            if $_<names> eq "1" && $_<slurpy> {
                $add_param.push(PAST::Val.new( :value(1), :named('named') ));
            }
            elsif !$_<slurpy> {
                # Current signatures only support one name.
                $add_param.push(PAST::Val.new( :value($_<names>[0]), :named('named') ));
            }
        }
        $ast.push($add_param);
    }

    return $ast;
}


# Accessor for entries in the signature object.
method entries() {
    Q:PIR {
        %r = getattribute self, '$!entries'
        unless null %r goto have_entries
        %r = new ['ResizablePMCArray']
        setattribute self, '$!entries', %r
      have_entries:
    };
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
