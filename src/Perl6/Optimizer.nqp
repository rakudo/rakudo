# This file contains a bunch of classes related to static optimization of Perl
# 6 programs. It takes place after we've done all of the stuff in the grammar
# and actions, which means CHECK time is over. Thus we're allowed to assume that
# lexpads are immutable, declarations are over and done with, multi candidate
# lists won't change and so forth.

use NQPP6QRegex;
use QAST;
use Perl6::Ops;

# A null QAST node, inserted when we want to eliminate something.
my $NULL := QAST::Op.new( :op<null> );

# Represents the current set of blocks we're in and thus the symbols they
# make available, and allows for queries over them.
my class Symbols {
    # The nested blocks we're in; it's the lexical chain, essentially.
    has @!block_stack;

    # Some interesting scopes.
    has $!GLOBALish;
    has $!UNIT;
    has $!SETTING;

    # Cached setting lookups.
    has %!SETTING_CACHE;

    # Some interesting symbols.
    has $!Mu;
    has $!Any;
    has $!Block;
    has $!PseudoStash;
    has $!Routine;
    has $!Nil;
    has $!Failure;
    has $!Seq;

    # Top routine, for faking it when optimizing post-inline.
    has $!fake_top_routine;

    # Constructs a new instance of the symbols handling class.
    method new($compunit) {
        my $obj := nqp::create(self);
        $obj.BUILD($compunit);
        $obj
    }
    method BUILD($compunit) {
        @!block_stack   := [$compunit[0]];
        $!GLOBALish     := $compunit.ann('GLOBALish');
        $!UNIT          := $compunit.ann('UNIT');
        %!SETTING_CACHE := {};
        unless nqp::istype($!UNIT, QAST::Block) {
            nqp::die("Optimizer could not find UNIT");
        }
        nqp::push(@!block_stack, $!UNIT);
        $!Mu          := self.find_lexical('Mu');
        $!Any         := self.find_lexical('Any');
        $!Block       := self.find_lexical('Block');
        $!PseudoStash := self.find_lexical('PseudoStash');
        $!Routine     := self.find_lexical('Routine');
        $!Nil         := self.find_lexical('Nil');
        $!Failure     := self.find_lexical('Failure');
        $!Seq         := self.find_lexical('Seq');
        nqp::pop(@!block_stack);
    }

    # Block handling.
    method push_block($block) {
        nqp::push(@!block_stack, $block);
    }
    method pop_block() {
        nqp::pop(@!block_stack)
    }
    method top_block() {
        @!block_stack[+@!block_stack - 1]
    }
    method top_routine() {
        return $!fake_top_routine if $!fake_top_routine;
        my int $i := nqp::elems(@!block_stack);
        while $i > 0 {
            $i := $i - 1;
            my $co := @!block_stack[$i].ann('code_object');
            if nqp::istype($co, $!Routine) {
                return $co;
            }
        }
        NQPMu
    }
    method faking_top_routine($fake, $run) {
        $!fake_top_routine := $fake;
        my $result := $run();
        $!fake_top_routine := NQPMu;
        $result;
    }
    
    # Accessors for interesting symbols/scopes.
    method GLOBALish()   { $!GLOBALish }
    method UNIT()        { $!UNIT }
    method Mu()          { $!Mu }
    method Any()         { $!Any }
    method Block()       { $!Block }
    method PseudoStash() { $!PseudoStash }
    method Nil()         { $!Nil }
    method Failure()     { $!Failure }
    method Seq()         { $!Seq }

    # The following function is a nearly 1:1 copy of World.find_symbol.
    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_symbol(@name) {
        # Make sure it's not an empty name.
        unless +@name { nqp::die("Cannot look up empty name"); }

        # GLOBAL is current view of global.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return $!GLOBALish;
        }

        # If it's a single-part name, look through the lexical
        # scopes.
        if +@name == 1 {
            my $final_name := @name[0];
            my int $i := +@!block_stack;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!block_stack[$i].symbol($final_name);
                if +%sym {
                    return self.force_value(%sym, $final_name, 1);
                }
            }
        }

        # If it's a multi-part name, see if the containing package
        # is a lexical somewhere. Otherwise we fall back to looking
        # in GLOBALish.
        my $result := $!GLOBALish;
        if +@name >= 2 {
            my $first := @name[0];
            my int $i := +@!block_stack;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!block_stack[$i].symbol($first);
                if +%sym {
                    $result := self.force_value(%sym, $first, 1);
                    @name := nqp::clone(@name);
                    @name.shift();
                    $i := 0;
                }
            }
        }

        # Try to chase down the parts of the name.
        for @name {
            if nqp::existskey($result.WHO, ~$_) {
                $result := ($result.WHO){$_};
            }
            else {
                nqp::die("Could not locate compile-time value for symbol " ~
                    join('::', @name));
            }
        }

        $result;
    }

    # Locates a lexical symbol and returns its compile time value. Dies if
    # it does not exist.
    method find_lexical($name) {
        my int $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                return self.force_value(%sym, $name, 1);
            }
        }
        nqp::die("Optimizer: No lexical $name found");
    }

    method find_lexical_symbol($name) {
        my int $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            return %sym if +%sym;
        }
        nqp::die("Optimizer: No lexical $name found");
    }

    # Checks if a given lexical is declared, though it needn't have a compile
    # time known value.
    method is_lexical_declared($name) {
        my int $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                return 1;
            }
        }
        0
    }

    # Forces a value to be made available.
    method force_value(%sym, $key, int $die) {
        if nqp::existskey(%sym, 'value') {
            %sym<value>
        }
        elsif nqp::existskey(%sym, 'lazy_value_from') {
            %sym<value> := nqp::atkey(nqp::atkey(%sym, 'lazy_value_from'), $key)
        }
        else {
            $die ?? nqp::die("No compile-time value for $key") !! NQPMu
        }
    }

    # Works out how many scopes in from the outermost a given name is. A 0
    # from this means the nearest declaration is from the setting; a 1 means
    # it is in the mainline, etc.
    method scopes_in($name) {
        my int $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                return $i;
            }
        }
        nqp::die("Symbol $name not found");
    }

    method is_from_core($name) {
        my int $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym && nqp::existskey(%sym, 'value') {
                my %sym := $block.symbol("!CORE_MARKER");
                if +%sym {
                    return 1;
                }
                return 0;
            }
        }
        return 0;
    }

    method find_in_setting($symbol) {
        if !nqp::defined($!SETTING) {
            my int $i := +@!block_stack;
            while $i > 0 && !nqp::defined($!SETTING) {
                $i := $i - 1;
                my $block := @!block_stack[$i];
                my %sym := $block.symbol("!CORE_MARKER");
                if +%sym {
                    $!SETTING := $block;
                }
            }
            if !nqp::defined($!SETTING) {
                nqp::die("Optimizer couldn't find CORE while looking for $symbol.");
            }
        } else {
            if nqp::existskey(%!SETTING_CACHE, $symbol) {
                return %!SETTING_CACHE{$symbol};
            }
        }
        my %sym := $!SETTING.symbol($symbol);
        if +%sym {
            return %!SETTING_CACHE{$symbol} := self.force_value(%sym, $symbol, 1);
        }
    }
}

# Tracks problems (errors and warnings) discovered by the optimizer as it
# does its work.
my class Problems {
    # Symbols object.
    has $!symbols;

    # Things that should be warned about; keys are warnings, value is an array
    # of line numbers.
    has %!worrying;

    # Typed exceptions, these are all deadly currently
    has @!exceptions;

    method new($symbols) {
        my $obj := nqp::create(self);
        $obj.BUILD($symbols);
        $obj
    }
    method BUILD($symbols) {
        $!symbols    := $symbols;
        %!worrying   := nqp::hash();
        @!exceptions := [];
    }

    method add_exception(@name, $op, *%opts) {
        %opts<line>            := HLL::Compiler.lineof($op.node.orig, $op.node.from, :cache(1));
        %opts<modules>         := $*W.p6ize_recursive(@*MODULES);

        # get line numbers - we can't use $*W.locprepost here
        # because the cursor has .from as .pos
        # in contrast to node
        my $pos  := $op.node.from;
        my $orig := $op.node.orig;

        my $prestart := $pos - 40;
        $prestart := 0 if $prestart < 0;
        my $pre := nqp::substr($orig, $prestart, $pos - $prestart);
        $pre    := subst($pre, /.*\n/, "", :global);
        $pre    := '<BOL>' if $pre eq '';

        my $postchars := $pos + 40 > nqp::chars($orig) ?? nqp::chars($orig) - $pos !! 40;
        my $post := nqp::substr($orig, $pos, $postchars);
        $post    := subst($post, /\n.*/, "", :global);
        $post    := '<EOL>' if $post eq '';

        %opts<pre>             := nqp::box_s($pre, $!symbols.find_symbol(['Str']));
        %opts<post>            := nqp::box_s($post, $!symbols.find_symbol(['Str']));
        %opts<is-compile-time> := nqp::p6bool(1);

        for %opts -> $p {
            if nqp::islist($p.value) {
                my @a := [];
                for $p.value {
                    nqp::push(@a, nqp::hllizefor($_, 'perl6'));
                }
                %opts{$p.key} := nqp::hllizefor(@a, 'perl6');
            }
            else {
                %opts{$p.key} := nqp::hllizefor($p.value, 'perl6');
            }
        }
        my $file        := nqp::getlexdyn('$?FILES');
        %opts<filename> := nqp::box_s(
            (nqp::isnull($file) ?? '<unknown file>' !! $file),
            $!symbols.find_symbol(['Str'])
        );

        my $exsym  := $!symbols.find_symbol(@name);
        my $x_comp := $!symbols.find_symbol(['X', 'Comp']);
        unless nqp::istype($exsym, $x_comp) {
            $exsym := $exsym.HOW.mixin($exsym, $x_comp);
        }

        my $ex := $exsym.new(|%opts);
        nqp::push(@!exceptions, $ex);
    }

    method add_worry($past_node, $message, @extras?) {
        self.add_memo($past_node, $message, @extras, :type<worry>);
    }

    method add_memo($past_node, $message, @extras?, :$type!) {
        my $mnode := $past_node.node;
        if !nqp::can($mnode,'orig') {
            # note("[DISLOCATED MESSAGE] " ~ $message);
            return;
        }
        my $line := HLL::Compiler.lineof($mnode.orig, $mnode.from, :cache(1));
        my $key := $message ~ (+@extras ?? "\n" ~ join("\n", @extras) !! "");
        my %cont := %!worrying;
        unless %cont{$key} {
            %cont{$key} := [];
        }
        %cont{$key}.push($line);
    }

    method report() {
        if +@!exceptions {
            if +@!exceptions > 1 {
                my $x_comp_group_sym := $!symbols.find_symbol(['X', 'Comp', 'Group']);
                my @exs := [];
                for @!exceptions {
                    nqp::push(@exs, $_);
                }
                my $x_comp_group := $x_comp_group_sym.new(:sorrows(@exs));
                $x_comp_group.throw();
            }
            else {
                @!exceptions[0].throw();
            }
        }

        # We didn't die from any Exception, so we print warnings now.
        if +%!worrying {
            my $err := nqp::getstderr();
            nqp::sayfh($err, "WARNINGS for " ~ $*W.current_file ~ ":");
            my @fails;
            for %!worrying {
                nqp::printfh($err, $_.key ~ " (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
                    join(', ', $_.value) ~ ")\n");
            }
        }
    }
}

# Implements analysis related to variable declarations within a block, which
# includes lexical to local handling and deciding when immediate blocks may
# be flattened into their surrounding block.
my class BlockVarOptimizer {
    # Hash mapping variable names declared in the block to the QAST::Var
    # of its declaration.
    has %!decls;

    # Usages of variables in this block, or unioned in from an inlined
    # immediate block.
    has %!usages_flat;

    # Usages of variables in this block, or unioned in from a non-inlined
    # immediate block or a declaration block.
    has %!usages_inner;

    # Have we seen this block (or an inner one) making calls?
    has int $!calls;

    # Usages of getlexouter.
    has @!getlexouter_binds;

    # Setup and bind (hopefully one each) of %_.
    has @!autoslurpy_setups;
    has @!autoslurpy_binds;

    # The takedispatcher operation.
    has $!takedispatcher;

    # If lowering is, for some reason, poisoned.
    has int $!poisoned;

    # If p6bindsig is used.
    has int $!uses_bindsig;

    method add_decl($var) {
        my str $scope := $var.scope;
        if $scope eq 'lexical' || $scope eq 'lexicalref' {
            %!decls{$var.name} := $var;
        }
    }

    method add_usage($var) {
        my str $scope := $var.scope;
        if $scope eq 'lexical' || $scope eq 'lexicalref' {
            my $name   := $var.name;
            my @usages := %!usages_flat{$name};
            unless @usages {
                @usages := [];
                %!usages_flat{$name} := @usages;
            }
            nqp::push(@usages, $var);
        }
    }

    method register_call() { $!calls++; }

    method register_getlexouter_bind($node) {
        nqp::push(@!getlexouter_binds, $node);
    }

    method register_autoslurpy_setup($node) {
        nqp::push(@!autoslurpy_setups, $node);
    }

    method register_autoslurpy_bind($node) {
        nqp::push(@!autoslurpy_binds, $node);
    }

    method register_takedispatcher($node) {
        $!takedispatcher := $node;
    }

    method poison_lowering() { $!poisoned := 1; }

    method uses_bindsig() { $!uses_bindsig := 1; }

    method get_decls() { %!decls }

    method get_usages_flat() { %!usages_flat }

    method get_usages_inner() { %!usages_inner }

    method get_calls() { $!calls }

    method is_poisoned() { $!poisoned }

    method is_flattenable() {
        for %!decls {
            my $var := $_.value;
            my str $scope := $var.scope;
            return 0 if $scope eq 'lexical' || $scope eq 'lexicalref';
            return 0 if $var.decl eq 'param';
        }
        1
    }

    method incorporate_inner($vars_info, $flattened) {
        # We'll exclude anything that the inner or flattened thing has as
        # a declaration, since those are its own.
        my %decls := $vars_info.get_decls;

        # Inner ones always go into our inners set.
        add_to_set(%!usages_inner, $vars_info.get_usages_inner, %decls);

        # Flat ones depend on if we flattened this block into ourself.
        add_to_set($flattened ?? %!usages_flat !! %!usages_inner,
            $vars_info.get_usages_flat, %decls);

        # Add up call counts.
        $!calls := $!calls + $vars_info.get_calls;

        sub add_to_set(%set, %to_add, %exclude) {
            for %to_add {
                my $name := $_.key;
                next if nqp::existskey(%exclude, $name);
                my @existing := %set{$name};
                if @existing {
                    for $_.value { nqp::push(@existing, $_) }
                    #nqp::splice(@existing, $_.value, 0, 0);
                }
                else {
                    %set{$name} := $_.value;
                }
            }
        }
    }

    method delete_unused_magicals($block) {
        # Magicals are contextual, so if we call anything when we're done for.
        return 0 if $!calls || $!poisoned || $!uses_bindsig;

        # Otherwise see if there's any we can kill.
        my %kill;
        if nqp::existskey(%!decls, '$/') {
            if !nqp::existskey(%!usages_flat, '$/') && !nqp::existskey(%!usages_inner, '$/') {
                %kill<$/> := 1;
                nqp::deletekey(%!decls, '$/');
            }
        }
        if nqp::existskey(%!decls, '$!') {
            if !nqp::existskey(%!usages_flat, '$!') && !nqp::existskey(%!usages_inner, '$!') {
                %kill<$!> := 1;
                nqp::deletekey(%!decls, '$!');
            }
        }
        if nqp::existskey(%!decls, '$¢') {
            if !nqp::existskey(%!usages_flat, '$¢') && !nqp::existskey(%!usages_inner, '$¢') {
                %kill<$¢> := 1;
                nqp::deletekey(%!decls, '$¢');
            }
        }
        if nqp::existskey(%!decls, '$_') {
            my str $decl := %!decls<$_>.decl;
            if $decl eq 'var' || $decl eq 'contvar' {
                if !nqp::existskey(%!usages_flat, '$_') && !nqp::existskey(%!usages_inner, '$_') {
                    if !@!getlexouter_binds {
                        %kill<$_> := 1;
                        nqp::deletekey(%!decls, '$_');
                    }
                    elsif nqp::elems(@!getlexouter_binds) == 1 {
                        my $glob := @!getlexouter_binds[0];
                        if $glob[0].name eq '$_' && $glob[1][0].value eq '$_' {
                            $glob.op('null');
                            $glob.shift(); $glob.shift();
                            %kill<$_> := 1;
                            nqp::deletekey(%!decls, '$_');
                        }
                    }
                }
            }
        }

        # If we found things to eliminate, do so.
        if %kill {
            my @setups := @($block[0]);
            my int $i  := 0;
            my int $n  := nqp::elems(@setups);
            while $i < $n {
                my $consider := @setups[$i];
                if nqp::istype($consider, QAST::Var) && nqp::existskey(%kill, $consider.name) {
                    @setups[$i] := $NULL;
                }
                $i++;
            }
        }
    }

    method delete_unused_autoslurpy() {
        if !$!poisoned && !$!uses_bindsig && nqp::existskey(%!decls, '%_')
                && nqp::elems(@!autoslurpy_setups) == 1
                && nqp::elems(@!autoslurpy_binds) == 1 {
            if !nqp::existskey(%!usages_inner, '%_') && nqp::elems(%!usages_flat<%_>) == 1 {
                my $to_null := @!autoslurpy_setups[0];
                nqp::shift($to_null) while @($to_null);
                $to_null.op('null');
                $to_null := @!autoslurpy_binds[0];
                nqp::shift($to_null) while @($to_null);
                $to_null.op('null');
            }
        }
    }

    method simplify_takedispatcher() {
        unless $!calls || $!uses_bindsig {
            if $!takedispatcher {
                $!takedispatcher.op('cleardispatcher');
                $!takedispatcher.shift();
            }
        }
    }

    method lexical_vars_to_locals($block) {
        return 0 if $!poisoned || $!uses_bindsig;
        return 0 unless nqp::istype($block[0], QAST::Stmts);
        for %!decls {
            # We're looking for lexical var decls; these have no magical
            # vivification lexical behavior and so are safe to lower.
            my $qast := $_.value;
            my str $decl := $qast.decl;
            next unless $decl eq 'var';
            my str $scope := $qast.scope;
            next unless $scope eq 'lexical';

            # Also ensure not dynamic.
            my $dynamic := try nqp::getattr($qast.value, nqp::p6var($qast.value).WHAT, '$!descriptor').dynamic;
            next if $dynamic;

            # Consider name. Can't lower if it's used by any nested blocks.
            my str $name := $_.key;
            unless nqp::existskey(%!usages_inner, $name) {
                # Lowerable if it's a normal variable.
                next if nqp::chars($name) < 1;
                unless nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $name, 0) {
                    my str $sigil := nqp::substr($name, 0, 1);
                    next unless $sigil eq '$' || $sigil eq '@' || $sigil eq '%';
                    next unless nqp::chars($name) >= 2 &&
                                nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $name, 1);
                }

                # Also must not lexicalref it.
                my int $ref'd := 0;
                if %!usages_flat{$name} {
                    for %!usages_flat{$name} {
                        if $_.scope eq 'lexicalref' {
                            $ref'd := 1;
                            last;
                        }
                    }
                }
                next if $ref'd;

                # Seems good; lower it. Note we need to retain a lexical in
                # case of binder failover to generate errors.
                my $new_name := $qast.unique('__lowered_lex');
                $block[0].unshift(QAST::Var.new( :name($qast.name), :scope('lexical'),
                                                 :decl('var'), :returns($qast.returns) ));
                $qast.name($new_name);
                $qast.scope('local');
                if %!usages_flat{$name} {
                    for %!usages_flat{$name} {
                        $_.scope('local');
                        $_.name($new_name);
                    }
                }
            }
        }
    }
}

# Junction optimizer, responsible for flattening away simple junctions.
my class JunctionOptimizer {
    # Junctions we can fold, and what short-circuit operator they fold to.
    my %foldable_junction := nqp::hash(
        '&infix:<|>', '&infix:<||>',
        '&infix:<&>', '&infix:<&&>');

    # Contexts in which we can fold a junction.
    my %foldable_outer := nqp::hash(
        '&prefix:<?>',      1,  '&prefix:<!>',      1,
        '&prefix:<so>',     1,  '&prefix:<not>',    1,
        'if',               1,  'unless',           1,
        'while',            1,  'until',            1);

    # The main optimizer.
    has $!optimizer;

    # Symbols lookup object.
    has $!symbols;

    # Constructs a new instance of the junction optimizer.
    method new($optimizer, $symbols) {
        my $obj := nqp::create(self);
        $obj.BUILD($optimizer, $symbols);
        $obj
    }
    method BUILD($optimizer, $symbols) {
        $!optimizer := $optimizer;
        $!symbols   := $symbols;
    }
    
    # Check if the junction is in a context where we can optimize.
    method is_outer_foldable($op) {
        if $op.op eq "call" {
            if nqp::existskey(%foldable_outer, $op.name) && $!symbols.is_from_core($op.name) {
                return 1;
            }
        } elsif nqp::existskey(%foldable_outer, $op.op) {
            return 1;
        }
        return 0;
    }

    # Only if a chain operator handles Any, rather than Mu, in its signature
    # will autothreading actually happen.
    method chain_handles_Any($op) {
        my $obj;
        my int $found := 0;
        try {
            $obj := $!symbols.find_lexical($op);
            $found := 1;
        }
        if $found == 1 {
            my $signature := $!symbols.find_in_setting("Signature");
            my $iter := nqp::iterator(nqp::getattr($obj.signature, $signature, '@!params'));
            while $iter {
                my $p := nqp::shift($iter);
                unless nqp::istype($p.type, $!symbols.Any) {
                    return 0;
                }
            }
            return 1;
        } else {
            return 0;
        }
        return 0;
    }

    method can_chain_junction_be_warped($node) {
        sub has_core-ish_junction($node) {
            if nqp::istype($node, QAST::Op) && $node.op eq 'call' &&
                    nqp::existskey(%foldable_junction, $node.name) {
                if $!symbols.is_from_core($node.name) {
                    # TODO: special handling for any()/all(), because they create
                    #       a Stmts with a infix:<,> in it.
                    if +$node.list == 1 {
                        return 0;
                    }
                    return 1;
                }
            }
            return 0;
        }

        if has_core-ish_junction($node[0]) {
            return 0;
        } elsif has_core-ish_junction($node[1]) {
            return 1;
        }
        return -1;
    }

    method optimize($op) {
        if self.is_outer_foldable($op) && nqp::istype($op[0], QAST::Op) {
            my $proceed := 0;
            my $exp-side;
            if $op[0].op eq "chain" {
                $exp-side := self.can_chain_junction_be_warped($op[0]);
                $proceed := $exp-side != -1 && self.chain_handles_Any($op[0].name) == 1
            } elsif $op[0].op eq 'callmethod' && $op[0].name eq 'ACCEPTS' {
                $exp-side := self.can_chain_junction_be_warped($op[0]);
                # we should only ever find the 0nd child (the invocant) to be a junction anyway.
                $proceed := $exp-side == 0;
            }
            if $proceed {
                return self.apply_transform($op, $exp-side);
            }
        }
        return NQPMu;
    }

    method apply_transform($op, $exp-side) {
        # TODO chain_handles_Any may get more cleverness to check only the parameters that actually have
        # a junction passed to them, so that in some cases the unfolding may still happen.
        my str $juncop    := $op[0][$exp-side].name eq '&infix:<&>' ?? 'if' !! 'unless';
        my str $juncname  := %foldable_junction{$op[0][$exp-side].name};
        my str $chainop   := $op[0].op;
        my str $chainname := $op[0].name;
        my $values := $op[0][$exp-side];
        my $ovalue := $op[0][1 - $exp-side];

        # the first time $valop is refered to, create a bind op for a
        # local var, next time create a reference var op.
        my %reference;
        sub refer_to($valop) {
            my $id := nqp::where($valop);
            if nqp::existskey(%reference, $id) {
                QAST::Var.new(:name(%reference{$id}), :scope<local>);
            } else {
                %reference{$id} := $op.unique('junction_unfold');
                QAST::Op.new(:op<bind>,
                             QAST::Var.new(:name(%reference{$id}),
                                           :scope<local>,
                                           :decl<var>),
                             $valop);
            }
        }

        # create a comparison operation for the inner comparisons
        sub chain($value) {
            if $exp-side == 0 {
                QAST::Op.new(:op($chainop), :name($chainname),
                             $value,
                             refer_to($ovalue));
            } else {
                QAST::Op.new(:op($chainop), :name($chainname),
                             refer_to($ovalue),
                             $value);
            }
        }

        # create a chain of outer logical junction operators with inner comparisons
        sub create_junc() {
            my $junc := QAST::Op.new(:name($juncname), :op($juncop));

            $junc.push(chain($values.shift()));

            if +$values.list > 1 {
                $junc.push(create_junc());
            } else {
                $junc.push(chain($values.shift()));
            }
            return $junc;
        }

        $op.shift;
        $op.unshift(create_junc());
        return $!optimizer.visit_op($op);
    }
}

# Drives the optimization process overall.
class Perl6::Optimizer {
    # Symbols tracking object.
    has $!symbols;

    # Stack of block variable optimizers.
    has @!block_var_stack;

    # Junction optimizer.
    has $!junc_opt;

    # Track problems we encounter.
    has $!problems;

    # Optimizer configuration.
    has %!adverbs;

    # The optimization level.
    has $!level;

    # How deep a chain we're in, for chaining operators.
    has int $!chain_depth;

    # Are we in void context?
    has int $!void_context;

    # Are we in a declaration?
    has int $!in_declaration;

    # one shared QAST tree we'll put into every block we've eliminated
    has $!eliminated_block_contents;

    has $!debug;

    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        # Initialize.
        $!symbols                 := Symbols.new($past);
        @!block_var_stack         := [];
        $!junc_opt                := JunctionOptimizer.new(self, $!symbols);
        $!problems                := Problems.new($!symbols);
        $!chain_depth             := 0;
        $!in_declaration          := 0;
        $!void_context            := 0;
        $!debug                   := nqp::getenvhash<RAKUDO_OPTIMIZER_DEBUG>;
        my $*DYNAMICALLY_COMPILED := 0;
        my $*W                    := $past.ann('W');

        # Work out optimization level.
        $!level := nqp::existskey(%adverbs, 'optimize') ??
            +%adverbs<optimize> !! 2;
        %!adverbs := %adverbs;

        note("method optimize before\n" ~ $past.dump) if $!debug;

        $!eliminated_block_contents := QAST::Op.new( :op('die_s'),
            QAST::SVal.new( :value('INTERNAL ERROR: Execution of block eliminated by optimizer') ) );

        # Walk and optimize the program.
        self.visit_block($!symbols.UNIT);

        # Report any discovered problems.
        $!problems.report();

        note("method optimize after\n" ~ $past.dump) if $!debug;

        $past
    }
    
    # Called when we encounter a block in the tree.
    method visit_block($block) {
        # Push block onto block stack and create vars tracking.
        $!symbols.push_block($block);
        @!block_var_stack.push(BlockVarOptimizer.new);
        
        # Visit children.
        if $block.ann('DYNAMICALLY_COMPILED') {
            my $*DYNAMICALLY_COMPILED := 1;
            self.visit_children($block, :resultchild(+@($block) - 1),:void_default);
        }
        else {
            self.visit_children($block, :resultchild(+@($block) - 1),:void_default);
        }
        
        # Pop block from block stack and get computed block var info.
        $!symbols.pop_block();
        my $vars_info := @!block_var_stack.pop();

        # If this block is UNIT and we're in interactive mode, poison lexical
        # to local lowering, or else we may lose symbols we need to remember.
        if $block =:= $!symbols.UNIT && %!adverbs<interactive> {
            $vars_info.poison_lowering();
        }

        # We might be able to delete some of the magical variables when they
        # are trivially unused, and also simplify takedispatcher.
        if $!level >= 1 {
            $vars_info.delete_unused_magicals($block);
            $vars_info.delete_unused_autoslurpy();
            $vars_info.simplify_takedispatcher();
        }

        # If the block is immediate, we may be able to inline it.
        my int $flattened := 0;
        my $result        := $block;
        if $block.blocktype eq 'immediate' && $block.arity == 0
                && !$vars_info.is_poisoned && !$block.has_exit_handler {
            # Scan symbols for any non-interesting ones.
            my @sigsyms;
            for $block.symtable() {
                my $name := $_.key;
                if $name ne '$_' && $name ne '$*DISPATCHER' {
                    @sigsyms.push($name);
                }
            }

            # If we dynamically compiled it, then it must not contain
            # any nested blocks.
            if $*DYNAMICALLY_COMPILED {
                for $block[0].list {
                    if nqp::istype($_, QAST::Block) && $_.blocktype ne 'raw' {
                        @sigsyms.push('');
                    }
                }
            }
            
            # If we have no interesting ones, then we can inline the
            # statements.
            if +@sigsyms == 0 {
                if $!level >= 2 {
                    my $outer := $!symbols.top_block;
                    $result := self.inline_immediate_block($block, $outer,
                        nqp::existskey($vars_info.get_decls(), '$_'));
                }
            }
        }

        # Do any possible lexical => local lowering.
        if $!level >= 2 {
            $vars_info.lexical_vars_to_locals($block);
        }

        # Incorporate this block's info into outer block's info.
        @!block_var_stack[nqp::elems(@!block_var_stack) - 1].incorporate_inner($vars_info, $flattened)
            if @!block_var_stack;

        $result
    }

    # Gets the last statement if the thing passed as a QAST::Stmts or a
    # QAST::Stmt. Works recursively. Otherwise returns what was passed.
    sub get_last_stmt($op) {
        if nqp::istype($op, QAST::Stmt) || nqp::istype($op, QAST::Stmts) {
            my int $resultchild := $op.resultchild // +@($op) - 1;
            $resultchild >= 0 ?? get_last_stmt($op[$resultchild]) !! $op
        }
        else {
            $op
        }
    }
    
    # Range operators we can optimize into loops, and how to do it.
    sub get_bound($node) {
        if nqp::istype($node, QAST::Want) && $node[1] eq 'Ii' {
            my int $value := $node[2].value;
            if $value > -2147483648 && $value < 2147483647 {
                return [$value];
            }
        }
        []
    }
    my %range_bounds := nqp::hash(
        '&infix:<..>', -> $op {
            my @lb := get_bound($op[0]);
            my @ub := get_bound($op[1]);
            @lb && @ub ?? [@lb[0], @ub[0]] !! []
        },
        '&infix:<..^>', -> $op {
            my @lb := get_bound($op[0]);
            my @ub := get_bound($op[1]);
            @lb && @ub ?? [@lb[0], @ub[0] - 1] !! []
        },
        '&infix:<^..>', -> $op {
            my @lb := get_bound($op[0]);
            my @ub := get_bound($op[1]);
            @lb && @ub ?? [@lb[0] + 1, @ub[0]] !! []
        },
        '&infix:<^..^>', -> $op {
            my @lb := get_bound($op[0]);
            my @ub := get_bound($op[1]);
            @lb && @ub ?? [@lb[0] + 1, @ub[0] - 1] !! []
        },
        '&prefix:<^>', -> $op {
            my @ub := get_bound($op[0]);
            @ub ?? [0, @ub[0] - 1] !! []
        },
        );

    # Poisonous calls.
    my %poison_calls := nqp::hash(
        'EVAL',     NQPMu, '&EVAL',     NQPMu,
        'EVALFILE', NQPMu, '&EVALFILE', NQPMu,
        'callwith', NQPMu, '&callwith', NQPMu,
        'callsame', NQPMu, '&callsame', NQPMu,
        'nextwith', NQPMu, '&nextwith', NQPMu,
        'nextsame', NQPMu, '&nextsame', NQPMu,
        'samewith', NQPMu, '&samewith', NQPMu);

    # Called when we encounter a QAST::Op in the tree. Produces either
    # the op itself or some replacement opcode to put in the tree.
    method visit_op($op) {
        note("method visit_op $!void_context\n" ~ $op.dump) if $!debug;
        # If it's a QAST::Op of type handle, needs some special attention.
        my str $optype := $op.op;
        if $optype eq 'handle' || $optype eq 'handlepayload' {
            return self.visit_handle($op);
        }
        elsif $optype eq 'locallifetime' {
            return self.visit_children($op,:first);
        }

        # If it's a for 1..1000000 { } we can simplify it to a while loop. We
        # check this here, before the tree is transformed by call inline opts.
        if $optype eq 'p6for' && $op.sunk && @($op) == 2 {
            my $theop := $op[0];
            if nqp::istype($theop, QAST::Stmts) { $theop := $theop[0] }

            if nqp::istype($theop, QAST::Op) && nqp::existskey(%range_bounds, $theop.name) && $!symbols.is_from_core($theop.name) {
                self.optimize_for_range($op, $op[1], $theop);
                self.visit_op_children($op);
                return $op;
            }
        }

        # It could also be that the user explicitly spelled out the for loop
        # with a method call to "map".
        if $optype eq 'callmethod' && $op.name eq 'sink' &&
              nqp::istype($op[0], QAST::Op) && $op[0].op eq 'callmethod' && $op[0].name eq 'map' && @($op[0]) == 2 &&
                (nqp::istype((my $c1 := $op[0][0]), QAST::Op) &&
                        nqp::existskey(%range_bounds, $c1.name)
                 || nqp::istype($op[0][0], QAST::Stmts) &&
                        nqp::istype(($c1 := $op[0][0][0]), QAST::Op) &&
                        nqp::existskey(%range_bounds, $c1.name)) &&
              $!symbols.is_from_core($c1.name) {
            self.optimize_for_range($op, $op[0][1], $c1);
            self.visit_op_children($op);
            return $op;
        }
        elsif $optype eq 'callmethod' && $op.name eq 'new' && $!void_context {
            if $op.node {
                my str $op_txt := nqp::escape($op.node.Str);
                my $warning := qq[Useless use of "$op_txt" in sink context];
                note($warning) if $!debug;
                $!problems.add_worry($op, $warning);
            }
            elsif nqp::istype($op[0],QAST::Var) && $op[0].scope eq 'lexical' {
                my str $op_txt := $op[0].name;
                my $warning := qq[Useless use of "$op_txt" in sink context];
                note($warning) if $!debug;
                $!problems.add_worry($op, $warning);
            }
        }

        # Let's see if we can catch a type mismatch in assignment at compile-time.
        # Especially with Num, Rat, and Int there's often surprises at run-time.
        if $optype eq 'assign' && nqp::istype($op[0], QAST::Var) && $op[0].scope eq 'lexical' {
            if nqp::istype($op[1], QAST::Want) {
                # grab the var's symbol from our blocks
                my $varsym := $!symbols.find_lexical_symbol($op[0].name);
                my $type := $varsym<type>;

                my $want_type := $op[1][1];
                my $varname := $op[0].name;

                if $want_type eq 'Ii' {
                    if $type =:= $!symbols.find_in_setting("Num") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($type), :value($op[1][2].value), :suggestiontype<Real>,
                                :valuetype<Int>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Rat") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($type), :value($op[1][2].value), :suggestiontype<Real>,
                                :valuetype<Int>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Complex") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($type), :value($op[1][2].value), :suggestiontype<Numeric>,
                                :valuetype<Int>
                            );
                    }
                } elsif $want_type eq 'Nn' {
                    my $value := $op[1][2];
                    if $value.HOW.name($value) eq 'QAST::NVal' {
                        if $type =:= $!symbols.find_in_setting("Int") {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($type), :value($value.value), :suggestiontype<Real>,
                                    :valuetype<Num>
                                );
                        } elsif $type =:= $!symbols.find_in_setting("Rat") {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($type), :value($value.value), :suggestiontype<Real>,
                                    :valuetype<Num>
                                );
                        } elsif $type =:= $!symbols.find_in_setting("Complex") {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($type), :value($value.value), :suggestiontype<Numeric>,
                                    :valuetype<Num>
                                );
                        }
                    }
                }
            }
            elsif nqp::istype($op[1], QAST::WVal) {
                my $varsym := $!symbols.find_lexical_symbol($op[0].name);
                my $type := $varsym<type>;

                my $value := $op[1].value;
                my $val_type := $value.HOW.name($value);
                my $varname := $op[0].name;
                if $val_type eq 'Rat' || $val_type eq 'RatStr' {
                    if $type =:= $!symbols.find_in_setting("Int") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value(+$value), :suggestiontype<Real>,
                                :valuetype<Rat>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Num") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value(+$value), :suggestiontype<Real>,
                                :valuetype<Rat>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Complex") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value(+$value), :suggestiontype<Numeric>,
                                :valuetype<Rat>
                            );
                    }
                }
                elsif $val_type eq 'Complex' || $val_type eq 'ComplexStr' {
                    if $type =:= $!symbols.find_in_setting("Int") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value($value.Numeric), :suggestiontype<Numeric>,
                                :valuetype<Complex>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Num") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value($value.Numeric), :suggestiontype<Numeric>,
                                :valuetype<Complex>
                            );
                    } elsif $type =:= $!symbols.find_in_setting("Rat") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value($value.Numeric), :suggestiontype<Numeric>,
                                :valuetype<Complex>
                            );
                    }
                }
            }
        }

        # A chain with exactly two children can become the op itself.
        if $optype eq 'chain' {
            $!chain_depth := $!chain_depth + 1;
            $optype := 'call' if $!chain_depth == 1 &&
                !(nqp::istype($op[0], QAST::Op) && $op[0].op eq 'chain') &&
                !(nqp::istype($op[1], QAST::Op) && $op[1].op eq 'chain');
        }

        # We may be able to unfold a junction at compile time.
        if $!level >= 2 {
            my $jo_result := $!junc_opt.optimize($op);
            if $jo_result {
                return $jo_result;
            }
        }

        # Visit the children.
        self.visit_op_children($op);

        # Some ops are significant for variable analysis/lowering.
        if $optype eq 'bind' {
            if nqp::istype($op[1], QAST::Op) && $op[1].op eq 'getlexouter' {
                @!block_var_stack[nqp::elems(@!block_var_stack) - 1].register_getlexouter_bind($op);
            }
            elsif nqp::istype($op[0], QAST::Var) && $op[0].name eq '%_' {
                @!block_var_stack[nqp::elems(@!block_var_stack) - 1].register_autoslurpy_bind($op);
            }
            elsif $op.ann('autoslurpy') {
                @!block_var_stack[nqp::elems(@!block_var_stack) - 1].register_autoslurpy_setup($op);
            }
        }
        elsif $optype eq 'p6bindsig' {
            @!block_var_stack[nqp::elems(@!block_var_stack) - 1].uses_bindsig();
        }
        elsif $optype eq 'call' || $optype eq 'callmethod' || $optype eq 'chain' {
            @!block_var_stack[nqp::elems(@!block_var_stack) - 1].register_call();
            if nqp::existskey(%poison_calls, $op.name) {
                self.poison_var_lowering();
            }
        }

        # May be able to eliminate some decontrv operations.
        if $optype eq 'p6decontrv' {
            # If it's rw, don't need to decont at all.
            my $value := $op[1];
            return $value if $op[0].value.rw;

            # Boolifications don't need it, nor do _I/_i/_n/_s ops, with
            # the exception of native assignment, which can decont_[ins]
            # as appropriate, which may avoid a boxing.
            my $last_stmt := get_last_stmt($value);
            if nqp::istype($last_stmt, QAST::Op) {
                my str $last_op := $last_stmt.op;
                if $last_op eq 'p6bool' || nqp::eqat($last_op, 'I', -1) {
                    return $value;
                }
                if nqp::eqat($last_op, 'assign_', 0) {
                    if    $last_op eq 'assign_i' { $op.op('decont_i') }
                    elsif $last_op eq 'assign_n' { $op.op('decont_n') }
                    elsif $last_op eq 'assign_s' { $op.op('decont_s') }
                    $op.shift; # The QAST::WVal of the routine
                    return $op;
                }
                if nqp::eqat($last_op, '_i', -2) ||
                      nqp::eqat($last_op, '_n', -2) ||
                      nqp::eqat($last_op, '_s', -2) {
                    return $value;
                }
            }
            elsif nqp::istype($last_stmt, QAST::Var) {
                my str $scope := $last_stmt.scope;
                if $scope eq 'lexicalref' {
                    $last_stmt.scope('lexical');
                    return $value;
                }
                if $scope eq 'attributeref' {
                    $last_stmt.scope('attribute');
                    return $value;
                }
            }
        }

        # Also some return type checks.
        elsif $optype eq 'p6typecheckrv' {
            my $optres := self.optimize_p6typecheckrv($op);
            return $optres if $optres;
        }

        # Some ops have first boolean arg, and we may be able to get rid of
        # a p6bool if there's already an integer result behind it.
        elsif $optype eq 'if' || $optype eq 'unless' || $optype eq 'while' || $optype eq 'until' {
            my $update := $op;
            my $target := $op[0];
            while (nqp::istype($target, QAST::Stmt) || nqp::istype($target, QAST::Stmts)) && +@($target) == 1 {
                $update := $target;
                $target := $target[0];
            }
            if nqp::istype($target, QAST::Op) && $target.op eq 'p6bool' {
                if nqp::objprimspec($target[0].returns) == nqp::objprimspec(int) {
                    $update[0] := $target[0];
                }
            }
            elsif nqp::istype($target,QAST::Var) && $target.scope eq 'lexicalref' && nqp::objprimspec($target.returns) == 1 {
                # turn $i into $i != 0
                $update[0] := QAST::Op.new( :op('isne_i'), :returns($target.returns), $target, QAST::IVal.new( :value(0) ));
            }
        }

        # May be able to simplify takedispatcher ops.
        elsif $optype eq 'takedispatcher' {
            @!block_var_stack[nqp::elems(@!block_var_stack) - 1].register_takedispatcher($op);
        }

        # Calls are especially interesting as we may wish to do some
        # kind of inlining.
        elsif $optype eq 'call' && $op.name ne '' {
            my $opt_result := self.optimize_call($op);
            return $opt_result if $opt_result;
        } elsif $optype eq 'call' && $op.name eq '' {
            my $opt_result := self.optimize_nameless_call($op);
            return $opt_result if $opt_result;
        }
        
        # If it's a private method call, we can sometimes resolve it at
        # compile time. If so, we can reduce it to a sub call in some cases.
        elsif $!level >= 2 && $optype eq 'callmethod' && $op.name eq 'dispatch:<!>' {
            self.optimize_private_method_call($op);
        }
        
        # If we end up here, just leave op as is.
        if $op.op eq 'chain' {
            $!chain_depth := $!chain_depth - 1;
        }
        $op
    }
    
    method visit_op_children($op) {
        my int $orig_void := $!void_context;
        $!void_context    := $op.op eq 'callmethod' && $op.name eq 'sink';
        self.visit_children($op);
        $!void_context := $orig_void;
    }

    method optimize_p6typecheckrv($op) {
        try {
            my $rettype        := $!symbols.top_routine.returns;
            my int $rettype_ps := nqp::objprimspec($rettype);
            if $rettype =:= $!symbols.Mu {
                return $op[0];
            }
            elsif $rettype_ps && $rettype_ps == nqp::objprimspec($op[0].returns) {
                return $op[0];
            }
        }
    }

    method optimize_call($op) {
        # See if we can find the thing we're going to call.
        my $obj;
        my int $found := 0;
        note("method optimize_call $!void_context\n" ~ $op.dump) if $!debug;
        try {
            $obj := $!symbols.find_lexical($op.name);
            $found := 1;
        }
        if $found {
            # Pure operators can be constant folded.
            if nqp::can($obj, 'IS_PURE') && $obj.IS_PURE {
                # First ensure we're not in void context; warn if so.
                sub widen($m) {
                    my int $from := $m.from;
                    my int $to   := $m.to;
                    for $m.list {
                        $from := $_.from if $_.from < $from;
                        $to   := $_.to   if $_.to   > $to;
                    }
                    nqp::substr($m.orig, $from, $to - $from);
                }
                if $op.name eq '&infix:<,>' {
                    if +@($op) {
                        # keep void setting to distribute sink warnings
                        try self.visit_children($op);
                    }
                    elsif $!void_context {
                        my $suggest := ($op.okifnil ?? ' (use Nil instead to suppress this warning)' !! '');
                        my $warning := qq[Useless use of () in sink context$suggest];
                        note($warning) if $!debug;
                        $!problems.add_worry($op, $warning);
                    }
                }
                elsif $op.node && $!void_context {
                    my str $op_txt := nqp::escape($op.node.Str);
                    my str $expr   := nqp::escape(widen($op.node));
                    unless $op_txt eq '/' && $op[1].has_compile_time_value && $op[1].compile_time_value == 0 {
                        my $warning := qq[Useless use of "$op_txt" in expression "$expr" in sink context];
                        note($warning) if $!debug;
                        $!problems.add_worry($op, $warning);
                    }
                }
                # check if all arguments are known at compile time
                my int $all_args_known := 1;
                my @args := [];
                for @($op) {
                    if nqp::istype($_, QAST::Node)
                            && $_.has_compile_time_value
                            && !$_.named {
                        nqp::push(@args, $_.compile_time_value);
                    }
                    else {
                        $all_args_known := 0;
                        last;
                    }
                }

                # Don't constant fold the 'x' operator if the resulting string would be too big.
                # 1024 is just a heuristic, measuring might show a bigger value would be fine.
                if $all_args_known && $op.name eq '&infix:<x>' && $!symbols.is_from_core('&infix:<x>') {
                    my int $survived := 0;
                    my int $size;
                    try {
                        $size := @args[0].chars * @args[1];
                        $survived := 1;
                    }

                    return $op if $survived && $size > 1024;
                }

                # If so, attempt to constant fold.
                if $all_args_known {
                    my int $survived := 0;
                    my $ret_value;
                    try {
                        $ret_value := $obj(|@args);
                        $survived  := 1 ;
                        CONTROL {
                            $survived := 0;
                        }
                    }
                    if $survived && self.constant_foldable_type($ret_value) {
                        return $NULL if $!void_context && !$!in_declaration;
                        $*W.add_object($ret_value);
                        my $wval := QAST::WVal.new(:value($ret_value));
                        if $op.named {
                            $wval.named($op.named);
                        }
                        # if it's an Int, Num or Str, we can create a Want
                        # from it with an int, num or str value.
                        my $want;
                        if !nqp::isconcrete($ret_value) {
# can we create a Want with a type object???  XXX
                        } elsif nqp::istype($ret_value, $!symbols.find_in_setting("Int")) && !nqp::isbig_I(nqp::decont($ret_value)) {
                            $want := QAST::Want.new($wval,
                                "Ii", QAST::IVal.new(:value(nqp::unbox_i($ret_value))));
                        } elsif nqp::istype($ret_value, $!symbols.find_in_setting("Num")) {
                            $want := QAST::Want.new($wval,
                                "Nn", QAST::NVal.new(:value(nqp::unbox_n($ret_value))));
                        } elsif nqp::istype($ret_value, $!symbols.find_in_setting("Str")) {
                            $want := QAST::Want.new($wval,
                                "Ss", QAST::SVal.new(:value(nqp::unbox_s($ret_value))));
                        }
                        if nqp::defined($want) {
                            if $op.named {
                                $want.named($op.named);
                            }
                            return $want;
                        }
                        return $wval;
                    } elsif $survived && nqp::istype($ret_value, $!symbols.Failure) {
                        # Disarm the failure so it doesn't output its warning during GC.
                        $ret_value.Bool();
                    }
                }
            }
            elsif $!level >= 2 && ($op.name eq '&prefix:<++>' || $op.name eq '&postfix:<++>' ||
                                   $op.name eq '&prefix:<-->' || $op.name eq '&postfix:<-->') {
                my $var := $op[0];
                if nqp::istype($var,QAST::Var) && $var.scope eq 'lexicalref' && nqp::objprimspec($var.returns) == 1 {
                    my $add := nqp::eqat($op.name,'++', -3) ?? 'add_i' !! 'sub_i';
                    my $sub := nqp::eqat($op.name,'++', -3) ?? 'sub_i' !! 'add_i';
                    my $one := QAST::Want.new(
                                QAST::WVal.new( :value($!symbols.find_lexical('Int')) ),
                                'Ii',
                                QAST::IVal.new( :value(1) ));
                    if $!void_context || nqp::eqat($op.name,'&pre',0) {  # can use simple add?
                        # $i = $i + 1  (or $i = $i - 1)
                        return QAST::Op.new( :op('assign_i'), :returns($var.returns), $var, QAST::Op.new(:op($add), $var, $one));
                    }
                    else {             # can't, but still much faster to subtract 1 back out than to allocate temp
                        # ($i = $i + 1) - 1  (or ($i = $i - 1) + 1)
                        return QAST::Op.new( :op($sub), :returns($var.returns),
                            QAST::Op.new( :op('assign_i'), :returns($var.returns), $var, QAST::Op.new(:op($add), $var, $one)),
                            $one);
                    }
                }
            }
            # If it's an onlystar proto, we have a couple of options.
            # The first is that we may be able to work out what to
            # call at compile time. Failing that, we can at least inline
            # the proto.
            my $dispatcher;
            try { if $obj.is_dispatcher { $dispatcher := 1 } }
            if $dispatcher && $obj.onlystar {
                # Try to do compile-time multi-dispatch. Need to consider
                # both the proto and the multi candidates.
                my @ct_arg_info := self.analyze_args_for_ct_call($op);
                if +@ct_arg_info {
                    my @types := @ct_arg_info[0];
                    my @flags := @ct_arg_info[1];
                    my $ct_result_proto := nqp::p6trialbind($obj.signature, @types, @flags);
                    my @ct_result_multi := $obj.analyze_dispatch(@types, @flags);
                    if $ct_result_proto == 1 && @ct_result_multi[0] == 1 {
                        my $chosen := @ct_result_multi[1];
                        if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                        if $!level >= 2 {
                            return nqp::can($chosen, 'inline_info') && nqp::istype($chosen.inline_info, QAST::Node)
                                ?? self.inline_call($op, $chosen)
                                !! self.call_ct_chosen_multi($op, $obj, $chosen);
                        }
                    }
                    elsif $ct_result_proto == -1 || @ct_result_multi[0] == -1 {
                        self.report_inevitable_dispatch_failure($op, @types, @flags, $obj,
                            :protoguilt($ct_result_proto == -1));
                    }
                }
                if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
            }
            elsif !$dispatcher && nqp::can($obj, 'signature') {
                # If we know enough about the arguments, do a "trial bind".
                my @ct_arg_info := self.analyze_args_for_ct_call($op);
                if +@ct_arg_info {
                    my @types := @ct_arg_info[0];
                    my @flags := @ct_arg_info[1];
                    my $sig := $obj.signature;
                    my $ct_result := nqp::p6trialbind($sig, @types, @flags);
                    if $ct_result == 1 {
                        if $op.op eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                        if $!level >= 2 {
                            if nqp::can($obj, 'inline_info') && nqp::istype($obj.inline_info, QAST::Node) {
                                return self.inline_call($op, $obj);
                            }
                            else {
                                self.simplify_refs($op, $sig);
                            }
                            copy_returns($op, $obj);
                        }
                    }
                    elsif $ct_result == -1 {
                        self.report_inevitable_dispatch_failure($op, @types, @flags, $obj);
                    }
                }
            }

            # If we get here, no inlining or compile-time decision was
            # possible, but we may still be able to make it a callstatic,
            # which is cheaper on some backends.
            my $scopes := $!symbols.scopes_in($op.name);
            if $scopes == 0 || $scopes == 1 && nqp::can($obj, 'soft') && !$obj.soft {
                $op.op('callstatic');
            }
        }
        else {
            # We really should find routines; failure to do so is a CHECK
            # time error. Check that it's not just compile-time unknown,
            # however (shows up in e.g. sub foo(&x) { x() }).
            unless $!symbols.is_lexical_declared($op.name) {
                # Shouldn't be able to hit this due to checks done in the parse
                # phase, but this will catch anything that sneaks past it.
                $!problems.add_exception(['X', 'Undeclared'], $op, :symbol($op.name));
            }
        }
        return NQPMu;
    }

    method constant_foldable_type($value) {
        !(
            nqp::istype($value, $!symbols.Seq) ||
            nqp::istype($value, $!symbols.Failure)
        )
    }

    my @native_assign_ops := ['', 'assign_i', 'assign_n', 'assign_s'];
    method optimize_nameless_call($op) {
        if +@($op) > 0 {
            # if we know we're directly calling the result, we can be smarter
            # about METAOPs
            my int $is_var := 0;
            if nqp::istype((my $metaop := $op[0]), QAST::Op) && ($metaop.op eq 'call' || $metaop.op eq 'callstatic') {
                if $metaop.name eq '&METAOP_ASSIGN' && $!symbols.is_from_core('&METAOP_ASSIGN') {
                    if nqp::istype($metaop[0], QAST::Var) {
                        if (nqp::istype($op[1], QAST::Var) && ($is_var := 1))
                            # instead of $foo += 1 we may have $foo.bar += 1, which we really want to
                            # unpack here as well. this second branch of the if statement achieves this.
                            || (nqp::istype($op[1], QAST::Op)
                                && ($op[1].op eq 'callmethod'
                                    || ($op[1].op eq 'hllize'
                                        && nqp::istype($op[1][0], QAST::Op) && $op[1][0].op eq 'callmethod')
                                    || $op[1].op eq 'call' || $op[1].op eq 'callstatic')) {
                            my str $assignop;
                            my $assignee;
                            my $assignee_var;
                            my int $is-always-definite;
                            if $is_var {
                                my str $sigil := nqp::substr($op[1].name, 0, 1);

                                if nqp::objprimspec($op[1].returns) -> $spec {
                                    $assignop := @native_assign_ops[$spec];
                                    $is-always-definite := 1;
                                } elsif $sigil eq '$' {
                                    $assignop := 'assign';
                                } else {
                                    # TODO support @ and % sigils
                                    # TODO check what else we need to "copy" from assign_op in Actions
                                    return NQPMu;
                                }

                                $assignee := $assignee_var := $op[1];
                            } else {
                                $assignop := "assign";

                                # We want to be careful to only call $foo.bar once,
                                # so we bind to a local var and assign to that.
                                my $lhs_ast := $op[1];
                                my $target_name := QAST::Node.unique('METAOP_assign_');
                                $assignee := QAST::Op.new( :op('bind'),
                                    QAST::Var.new(:name($target_name), :scope('local'), :decl('var')),
                                    $lhs_ast);
                                $assignee_var := QAST::Var.new(:name($target_name), :scope('local'));

                            }

                            # since the optimizer will only ever walk the first
                            # branch of a Want node, we have to make sure to change
                            # the node in place, since it's most likely shared with
                            # the other branch.
                            $op.op($assignop);
                            my $operand := $op[2];

                            $op.pop;
                            $op.pop;
                            $op.pop;

                            $op.push($assignee);
                            if ($is-always-definite) {
                                $op.push(QAST::Op.new( :op('call'), :name($metaop[0].name),
                                    $assignee_var,
                                    $operand));
                            } else {
                                $op.push(QAST::Op.new( :op('call'), :name($metaop[0].name),
                                    QAST::Op.new( :op('if'),
                                        QAST::Op.new( :op('p6definite'), $assignee_var),
                                        $assignee_var,
                                        QAST::Op.new( :op('call'), :name($metaop[0].name) ) ),
                                    $operand));
                            }

                            if $assignop ne 'assign' && nqp::objprimspec($assignee.returns) {
                                $op.returns($assignee.returns);
                            }
                        }
                    }
                } elsif $metaop.name eq '&METAOP_NEGATE' && $!symbols.is_from_core('&METAOP_NEGATE') {
                    return NQPMu unless nqp::istype($metaop[0], QAST::Var);
                    return QAST::Op.new( :op('call'), :name('&prefix:<!>'),
                            QAST::Op.new( :op('call'), :name($metaop[0].name),
                                $op[1],
                                $op[2]) );
                } elsif $metaop.name eq '&METAOP_REVERSE' && $!symbols.is_from_core('&METAOP_REVERSE') {
                    return NQPMu unless nqp::istype($metaop[0], QAST::Var) && +@($op) == 3;
                    return QAST::Op.new( :op('call'), :name($metaop[0].name),
                                $op[2],
                                $op[1]);
                }
            }
        }
        NQPMu;
    }

    method optimize_private_method_call($op) {
        if $op[1].has_compile_time_value && $op[2].has_compile_time_value &&
                nqp::istype($op[1], QAST::Want) && $op[1][1] eq 'Ss' {
            my str $name := $op[1][2].value; # get raw string name
            my $pkg  := $op[2].returns;      # actions sets this unless in role
            my $meth := $pkg.HOW.find_private_method($pkg, $name);
            if nqp::defined($meth) && $meth {
                unless nqp::isnull(nqp::getobjsc($meth)) {
                    my $call := QAST::WVal.new( :value($meth) );
                    my $inv  := $op.shift;
                    $op.shift; $op.shift; # name, package (both pre-resolved now)
                    $op.unshift($inv);
                    $op.unshift($call);
                    $op.op('call');
                    $op.name(NQPMu);
                }
            }
            else {
                $!problems.add_exception(['X', 'Method', 'NotFound'], $op, 
                    :private(nqp::p6bool(1)), :method($name),
                    :typename($pkg.HOW.name($pkg)));
            }
        }
    }

    method optimize_for_range($op, $callee, $c2) {
        my $code    := $callee.ann('code_object');
        my $count   := $code.count;
        my $block   := $!symbols.Block;
        my $phasers := nqp::istype($code, $block)
          ?? nqp::getattr($code, $block, '$!phasers')
          !! nqp::null();
        if $count == 1 && nqp::isnull($phasers) && %range_bounds{$c2.name}($c2) -> @bounds {
            my $it_var     := QAST::Node.unique('range_it_');
            my $callee_var := QAST::Node.unique('range_callee_');
            $op.shift while $op.list;
            $op.op('stmts');
            $op.push(QAST::Stmts.new(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($it_var), :scope('local'), :decl('var'), :returns(int) ),
                    QAST::IVal.new( :value(@bounds[0]) )
                ),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($callee_var), :scope('local'), :decl('var') ),
                    $callee
                ),
                QAST::Op.new(
                    :op('while'),
                    QAST::Op.new(
                        :op('isle_i'),
                        QAST::Var.new( :name($it_var), :scope('local'), :returns(int) ),
                        QAST::IVal.new( :value(@bounds[1]) )
                    ),
                    QAST::Op.new(
                        :op('call'),
                        QAST::Var.new( :name($callee_var), :scope('local') ),
                        QAST::Var.new( :name($it_var), :scope('local'), :returns(int) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($it_var), :scope('local'), :returns(int) ),
                        QAST::Op.new(
                            :op('add_i'),
                            QAST::Var.new( :name($it_var), :scope('local'), :returns(int) ),
                            QAST::IVal.new( :value(1) )
                        )
                    )
                ),
                QAST::WVal.new( :value($!symbols.Nil) )
            ));
        }
    }

    # Handles visiting a QAST::Op :op('handle').
    method visit_handle($op) {
        my int $orig_void := $!void_context;
        $!void_context    := 0;
        self.visit_children($op, :skip_selectors);
        $!void_context := $orig_void;
        $op
    }
    
    # Handles visiting a QAST::Want node.
    method visit_want($want) {
        note("method visit_want $!void_context\n" ~ $want.dump) if $!debug;

        # If it's the sink context void node, then only visit the first
        # child. Otherwise, see all.
        if +@($want) == 3 && $want[1] eq 'v' {
            my $sinker := $want[2];
            my int $tweak_sinkee := nqp::istype($sinker, QAST::Op)
                && $sinker.op eq 'p6sink'
                && $sinker[0] =:= $want[0];
            self.visit_children($want, :first);
            if $tweak_sinkee {
                $want[2][0] := $want[0];
            }
        }
        else {
            self.visit_children($want, :skip_selectors);
        }

        # (Check the following after we've checked children, since they may have useless bits too.)

        # Any literal in void context deserves a warning.
        if $!void_context && +@($want) == 3 && $want.node {

            my str $warning;
            if $want[1] eq 'Ss' && nqp::istype($want[2], QAST::SVal) {
                $warning := qq[Useless use of constant string "]
                         ~ nqp::escape($want[2].value)
                         ~ qq[" in sink context];
            }
            elsif $want[1] eq 'Ii' && nqp::istype($want[2], QAST::IVal) {
                $warning := qq[Useless use of constant integer ]
                         ~ ~$want[2].value
                         ~ qq[ in sink context];
            }
            elsif $want[1] eq 'Nn' && nqp::istype($want[2], QAST::NVal) {
                $warning := qq[Useless use of constant floating-point number ]
                         ~ ~$want[2].value
                         ~ qq[ in sink context];
            }
            if $warning {
                $warning := $warning ~ ' (use Nil instead to suppress this warning)' if $want.okifnil;
                note($warning) if $!debug;
                $!problems.add_worry($want, $warning);
            }
        }

        $want;
    }
    
    # Handles visit a variable node.
    method visit_var($var) {
        # Track usage.
        my str $scope := $var.scope;
        my str $decl := $var.decl;
        if $scope eq 'attribute' || $scope eq 'attributeref' || $scope eq 'positional' || $scope eq 'associative' {
            self.visit_children($var);
        } else {
            my int $top := nqp::elems(@!block_var_stack) - 1;
            if $decl {
                @!block_var_stack[$top].add_decl($var);
                if $decl eq 'param' {
                    self.visit_children($var);
                    my $default := $var.default;
                    if $default {
                        my $stmts_def := QAST::Stmts.new( $default );
                        self.visit_children($stmts_def);
                        $var.default($stmts_def[0]);
                    }
                }
            }
            else {
                @!block_var_stack[$top].add_usage($var);
            }
        }

        # Warn about usage of variable in void context.
        if $!void_context && !$decl && $var.name && !$var.sinkok {
            # stuff like Nil is also stored in a QAST::Var, but
            # we certainly don't want to warn about that one.
            my str $name  := try $!symbols.find_lexical_symbol($var.name)<descriptor>.name;
            $name         := $var.name unless $name;
            my str $sigil := nqp::substr($name, 0, 1);
            if $name ne "Nil" && $name ne 'ctxsave' {  # (comes from nqp, which doesn't know about wanted)
                my $suggest := ($var.okifnil ?? ' (use Nil instead to suppress this warning)' !! '');
                my $warning := nqp::index(' $@%&', $sigil) < 1
                    ?? "Useless use of $name symbol in sink context$suggest"
                    !! $sigil eq $name
                        ?? "Useless use of unnamed $sigil variable in sink context$suggest"
                        !! "Useless use of $name in sink context$suggest";
                note($warning) if $!debug;
                $!problems.add_worry($var, $warning);
            }
        }

        $var;
    }
    
    # Checks arguments to see if we're going to be able to do compile
    # time analysis of the call.
    my @allo_map := ['', 'Ii', 'Nn', 'Ss'];
    my %allo_rev := nqp::hash('Ii', 1, 'Nn', 2, 'Ss', 3);
    my @prim_names := ['', 'int', 'num', 'str'];
    my int $ARG_IS_LITERAL := 32;
    method analyze_args_for_ct_call($op) {
        my @types;
        my @flags;
        my @allomorphs;
        my int $num_prim := 0;
        my int $num_allo := 0;
        
        # Initial analysis.
        for @($op) {
            if !nqp::can($_,'flat') {
                note("Weird node in analyze: " ~ $_.HOW.name($_));
                return [];
            }
            # Can't cope with flattening or named.
            if $_.flat || $_.named ne '' {
                return [];
            }
            
            # See if we know the node's type; if so, check it.
            my $type := $_.returns();
            if $type =:= NQPMu {
                if nqp::istype($_, QAST::Want) && nqp::istype($_[0], QAST::WVal) {
                    $type := $_[0].value.WHAT;
                }
            }
            my $ok_type := 0;
            try $ok_type := nqp::istype($type, $!symbols.Mu) &&
                $type.HOW.archetypes.nominal();
            unless $ok_type {
                # nqp::ops end up labeled with nqp primitive types; we swap
                # those out for their Perl 6 equivalents.
                my int $ps := nqp::objprimspec($type);
                if $ps >= 1 && $ps <= 3 {
                    $type := $!symbols.find_lexical(@prim_names[$ps]);
                    $ok_type := 1;
                }
            }
            if $ok_type {
                my $prim := nqp::objprimspec($type);
                my str $allo := $_.has_compile_time_value && nqp::istype($_, QAST::Want)
                    ?? $_[1] !! '';
                @types.push($type);
                @flags.push($prim);
                @allomorphs.push($allo);
                $num_prim := $num_prim + 1 if $prim;
                $num_allo := $num_allo + 1 if $allo;
            }
            else {
                return [];
            }
        }
        
        # See if we have an allomorphic constant that may allow us to do
        # a native dispatch with it; takes at least one declaratively
        # native argument to make this happen.
        if @types == 2 && $num_prim == 1 && $num_allo == 1 {
            my int $prim_flag := @flags[0] || @flags[1];
            my int $allo_idx := @allomorphs[0] ?? 0 !! 1;
            if @allomorphs[$allo_idx] eq @allo_map[$prim_flag] {
                @flags[$allo_idx] := $prim_flag +| $ARG_IS_LITERAL;
            }
        }
        
        # Alternatively, a single arg that is allomorphic will prefer
        # the literal too.
        if @types == 1 && $num_allo == 1 {
            my $rev := %allo_rev{@allomorphs[0]};
            @flags[0] := nqp::defined($rev) ?? $rev +| $ARG_IS_LITERAL !! 0;
        }
        
        [@types, @flags]
    }

    method report_inevitable_dispatch_failure($op, @types, @flags, $obj, :$protoguilt) {
        my @arg_names;
        my int $i := 0;
        while $i < +@types {
            @arg_names.push(
                @flags[$i] == 1 ?? 'int' !!
                @flags[$i] == 2 ?? 'num' !!
                @flags[$i] == 3 ?? 'str' !!
                @types[$i].HOW.name(@types[$i]));
            $i := $i + 1;
        }

        my %opts := nqp::hash();
        %opts<protoguilt> := $protoguilt // nqp::p6bool(0);
        %opts<arguments> := @arg_names;
        %opts<objname> := $obj.name;
        %opts<signature> := nqp::can($obj, 'is_dispatcher') && $obj.is_dispatcher && !$protoguilt ??
                multi_sig_list($obj) !!
                [try $obj.signature.gist];

        $!problems.add_exception(['X', 'TypeCheck', 'Argument'], $op, |%opts);
    }
    
    # Signature list for multis.
    sub multi_sig_list($dispatcher) {
        my @sigs := [];
        for $dispatcher.dispatchees {
            @sigs.push("\n    " ~ $_.signature.gist);
        }
        @sigs
    }
    
    # Visits all of a nodes children, and dispatches appropriately.
    method visit_children($node, :$skip_selectors, :$resultchild, :$first, :$void_default) {
        my int $r := $resultchild // -1;
        my int $i := 0;
        my int $n := +@($node);
        while $i < $n {
            my int $outer_void := $!void_context;
            my int $outer_decl := $!in_declaration;
            unless $skip_selectors && $i % 2 {
                $!void_context   := $void_default && ($outer_void || ($r != -1 && $i != $r));
                $!in_declaration := $outer_decl || ($i == 0 && nqp::istype($node, QAST::Block));
                my $visit := $node[$i];
                if nqp::can($visit,'ann') {
                    if $visit.wanted { $!void_context := 0 }
                    elsif $visit.sunk { $!void_context := 1 }
                    elsif $visit.final {
                        note("Undecided final " ~ $node.HOW.name($node)) if $!debug;
                        $!void_context := 0;  # assume wanted
                    }
                }
                else {
                    note("Non-QAST node visited " ~ $visit.HOW.name($visit)) if $!debug;
                }
                if nqp::istype($visit, QAST::Op) {
                    $node[$i] := self.visit_op($visit)
                }
                elsif nqp::istype($visit, QAST::Want) {
                    $node[$i] := self.visit_want($visit);
                }
                elsif nqp::istype($visit, QAST::Var) {
                    $node[$i] := self.visit_var($visit);
                }
                elsif nqp::istype($visit, QAST::Block) {
                    $node[$i] := self.visit_block($visit);
                }
                elsif nqp::istype($visit, QAST::Stmts) && nqp::elems($visit.list) == 1 && !$visit.named {
                    self.visit_children($visit,:void_default);
                    $node[$i] := $visit[0];
                }
                elsif nqp::istype($visit, QAST::Stmt) || nqp::istype($visit, QAST::Stmts) {
                    my int $resultchild := $visit.resultchild // +@($visit) - 1;
                    if $resultchild >= 0 {
                        self.visit_children($visit, :$resultchild,:void_default);
                        if !nqp::can($visit,'returns') {
                            note("Child can't returns! " ~ $visit.HOW.name($visit)) if $!debug;
                        }
                        elsif !nqp::can($visit[$resultchild],'returns') {
                            note("Resultchild $resultchild can't returns! " ~
                                $visit[$resultchild].HOW.name($visit[$resultchild]) ~
                                "\n" ~ $node.dump)
                                if $!debug;
                        }
                        else {
                            $visit.returns($visit[$resultchild].returns);
                            if nqp::istype($visit[0], QAST::Op) && $visit[0].op eq 'lexotic' {
                                if @!block_var_stack[nqp::elems(@!block_var_stack) - 1].get_calls() == 0 {
                                    # Lexotic in something making no calls, which
                                    # means there's no way to use it.
                                    $node[$i] := $visit[0][0];
                                }
                            }
                        }
                    }
                }
                elsif nqp::istype($visit, QAST::Regex) {
                    self.poison_var_lowering();
                    QRegex::Optimizer.new().optimize($visit, $!symbols.top_block, |%!adverbs);
                }
                elsif nqp::istype($visit, QAST::WVal) {
                    if $!void_context && $visit.has_compile_time_value && $visit.node {
                        my $value := ~$visit.node;
                        $value := '""' if $value eq '';
                        my $suggest := ($visit.okifnil ?? ' (use Nil instead to suppress this warning)' !! '');
                        unless $value eq 'Nil' {
                            my $warning := qq[Useless use of constant value {~$visit.node} in sink context$suggest];
                            note($warning) if $!debug;
                            $!problems.add_worry($visit, $warning)
                        }
                    }
                    if $visit.value =:= $!symbols.PseudoStash {
                        self.poison_var_lowering();
                    }
                }
                elsif nqp::istype($visit, QAST::ParamTypeCheck) ||
                      nqp::istype($visit, QAST::SVal) ||
                      nqp::istype($visit, QAST::IVal) ||
                      nqp::istype($visit, QAST::NVal) ||
                      nqp::istype($visit, QAST::VM)
                { }
                else {
                    note("Weird node visited: " ~ $visit.HOW.name($visit)) if $!debug;
                }
            }
            $i               := $first ?? $n !! $i + 1;
            $!void_context   := $outer_void;
            $!in_declaration := $outer_decl;
        }
        $node;
    }

    # Inlines an immediate block.
    method inline_immediate_block($block, $outer, $preserve_topic) {
        # Sanity check.
        return $block if +@($block) != 2;
        return $block unless nqp::istype($outer[0], QAST::Stmts);

        # Extract interesting parts of block.
        my $decls := $block.shift;
        my $stmts := $block.shift;

        # Turn block into an "optimized out" stub (deserialization or fixup
        # will still want it to be there). Marked as "raw" to avoid any kind
        # of lexical capture/closure happening.
        $block.blocktype('raw');
        $block[0] := $!eliminated_block_contents;
        $outer[0].push($block);

        # Copy over interesting stuff in declaration section.
        for @($decls) {
            if nqp::istype($_, QAST::Op) && ($_.op eq 'p6bindsig' || 
                    $_.op eq 'bind' && $_[0].name eq 'call_sig') {
                # Don't copy this binder call or setup.
            }
            elsif nqp::istype($_, QAST::Op) && $_.op eq 'bind' && $_[0].name eq '$_' {
                # Don't copy the $_ initialization from outer.
            }
            elsif nqp::istype($_, QAST::Var) && ($_.name eq '$/' || $_.name eq '$!' ||
                    $_.name eq '$_' || $_.name eq '$*DISPATCHER') {
                # Don't copy this variable node.
            }
            elsif nqp::istype($_, QAST::Op) && $_.op eq 'takedispatcher' {
                # Don't copy the dispatcher take, since the $*DISPATCHER is
                # also not copied.
            }
            else {
                $outer[0].push($_);
            }
        }

        # Hand back the statements, but be sure to preserve $_ around them if
        # the block uses it.
        if $preserve_topic {
            my $pres_topic_name := QAST::Node.unique('pres_topic_');
            $outer[0].push(QAST::Var.new( :scope('local'), :name($pres_topic_name), :decl('var') ));
            return QAST::Stmts.new(
                :resultchild(1),
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($pres_topic_name), :scope('local') ),
                    QAST::Var.new( :name('$_'), :scope('lexical') )
                ),
                $stmts,
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name('$_'), :scope('lexical') ),
                    QAST::Var.new( :name($pres_topic_name), :scope('local') )
                )
            );
        }
        else {
            return $stmts;
        }
    }
    
    # Inlines a call to a sub.
    method inline_call($call, $code_obj) {
        # If the code object is marked soft, can't inline it.
        if nqp::can($code_obj, 'soft') && $code_obj.soft {
            return $call;
        }
        
        # Bind the arguments to temporaries, if they are used more than once.
        my $inlined := QAST::Stmts.new();
        my @subs;
        my @usages;
        $code_obj.inline_info.count_inline_placeholder_usages(@usages);
        my int $idx := 0;
        for $call.list {
            if @usages[$idx] == 1 {
                nqp::push(@subs, $_);
            }
            else {
                my $temp_name := QAST::Node.unique('_inline_arg_');
                my $temp_type := $_.returns;
                $inlined.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($temp_name), :scope('local'), :returns($temp_type), :decl('var') ),
                    $_));
                nqp::push(@subs, QAST::Var.new( :name($temp_name), :scope('local'), :returns($temp_type) ));
            }
            $idx++;
        }
        
        # Now do the inlining.
        $inlined.push($code_obj.inline_info.substitute_inline_placeholders(@subs));
        if $call.named -> $name {
            $inlined.named($name);
        }
        $inlined.node($call.node);

        # Do an optimization pass over the inlined code.
        $!symbols.faking_top_routine($code_obj,
            { self.visit_children($inlined) });

        # Annotate return type.
        $inlined.returns($code_obj.returns);

        $inlined
    }
    
    # If we decide a dispatch at compile time, this emits the direct call.
    # Note that we do not do this on MoarVM, since it can actually make a
    # much better job of these than we are able to here and we don't have a
    # way to convey the choice. We also simplify any lexicalref/attributeref
    # we may be passing.
    method call_ct_chosen_multi($call, $proto, $chosen) {
        self.simplify_refs($call, $chosen.signature);
        if nqp::getcomp('perl6').backend.name ne 'moar' {
            my @cands := $proto.dispatchees();
            my int $idx := 0;
            for @cands {
                if $_ =:= $chosen {
                    $call.unshift(QAST::Op.new(
                        :op('atpos'),
                        QAST::Var.new(
                            :name('@!dispatchees'), :scope('attribute'),
                            QAST::Var.new( :name($call.name), :scope('lexical') ),
                            QAST::WVal.new( :value($!symbols.find_lexical('Routine')) )
                        ),
                        QAST::IVal.new( :value($idx) )
                    ));
                    $call.name(NQPMu);
                    $call.op('call');
                    #say("# Compile-time resolved a call to " ~ $proto.name);
                    last;
                }
                $idx := $idx + 1;
            }
            $call := copy_returns($call, $chosen);
        }
        else {
            my $scopes := $!symbols.scopes_in($call.name);
            if $scopes == 0 || $scopes == 1 && nqp::can($proto, 'soft') && !$proto.soft {
                $call.op('callstatic');
            }
        }
        $call
    }

    # Looks through positional args for any lexicalref or attributeref, and
    # if we find them check if the expectation is for an non-rw argument.
    method simplify_refs($call, $sig) {
        if $sig.arity == $sig.count {
            my @args   := $call.list;
            my int $i  := $call.name eq '' ?? 1 !! 0;
            my int $n  := nqp::elems(@args);
            my int $p  := 0;
            while $i < $n {
                my $arg := @args[$i];
                unless $arg.named || $arg.flat {
                    if nqp::istype($arg, QAST::Var) {
                        my str $scope := $arg.scope;
                        my int $lref  := $scope eq 'lexicalref';
                        my int $aref  := $scope eq 'attributeref';
                        if $lref || $aref {
                            my $Signature := $!symbols.find_in_setting("Signature");
                            my $param := nqp::getattr($sig, $Signature, '@!params')[$p];
                            if nqp::can($param, 'rw') {
                                unless $param.rw {
                                    $arg.scope($lref ?? 'lexical' !! 'attribute');
                                }
                            }
                        }
                    }
                    $p++;
                }
                $i++;
            }
        }
    }
    
    my @prim_spec_ops := ['', 'p6box_i', 'p6box_n', 'p6box_s'];
    my @prim_spec_flags := ['', 'Ii', 'Nn', 'Ss'];
    sub copy_returns($to, $from) {
        if nqp::can($from, 'returns') {
            my $ret_type := $from.returns();
            if nqp::objprimspec($ret_type) -> $primspec {
                $to := QAST::Want.new(
                    :named($to.named),
                    QAST::Op.new( :op(@prim_spec_ops[$primspec]), $to ),
                    @prim_spec_flags[$primspec], $to);
            }
            $to.returns($ret_type);
        }
        $to
    }

    method poison_var_lowering() {
        for @!block_var_stack {
            $_.poison_lowering();
        }
    }
}

# vim: ft=perl6 expandtab sw=4
