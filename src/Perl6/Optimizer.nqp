# This file contains a bunch of classes related to static optimization of Raku
# programs. It takes place after we've done all of the stuff in the grammar
# and actions, which means CHECK time is over. Thus we're allowed to assume that
# lexpads are immutable, declarations are over and done with, multi candidate
# lists won't change and so forth.

use NQPP6QRegex;
use QAST;
use Perl6::Ops;
use Perl6::Metamodel;

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
    has @!CORES;

    # Cached setting lookups.
    has %!SETTING_CACHE;

    # Some interesting symbols.
    has $!Mu;
    has $!Mu'U;
    has $!Junction;
    has $!Any;
    has $!Block;
    has $!PseudoStash;
    has $!Code;
    has $!Routine;
    has $!Method;
    has $!Signature;
    has $!Pair;
    has $!Regex;
    has $!Nil;
    has $!Failure;
    has $!False;
    has $!True;
    has $!Seq;
    has $!AST;
    has $!LoweredAwayLexical;

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
        @!CORES         := [];
        $!GLOBALish     := $compunit.ann('GLOBALish');
        $!UNIT          := $compunit.ann('UNIT');
        %!SETTING_CACHE := {};
        unless nqp::istype($!UNIT, QAST::Block) {
            nqp::die("Optimizer could not find UNIT");
        }
        nqp::push(@!block_stack, $!UNIT);
        $!Mu          := self.find_in_setting('Mu');
        $!Mu'U        := nqp::gethllsym('Raku', 'Mu:U');
        $!Junction    := self.find_in_setting('Junction');
        $!Any         := self.find_in_setting('Any');
        $!Block       := self.find_in_setting('Block');
        $!PseudoStash := self.find_in_setting('PseudoStash');
        $!Code        := self.find_in_setting('Code');
        $!Routine     := self.find_in_setting('Routine');
        $!Method      := self.find_in_setting('Method');
        $!Signature   := self.find_in_setting('Signature');
        $!Pair        := self.find_in_setting('Pair');
        $!Regex       := self.find_in_setting('Regex');
        $!Nil         := self.find_in_setting('Nil');
        $!Failure     := self.find_in_setting('Failure');
        $!False       := self.find_in_setting('False');
        $!True        := self.find_in_setting('True');
        $!Seq         := self.find_in_setting('Seq');
        $!AST         := self.find_in_setting('AST');
        $!LoweredAwayLexical := self.find_symbol(['Rakudo', 'Internals', 'LoweredAwayLexical']);
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
    method Mu'U()        { $!Mu'U }
    method Junction()    { $!Junction }
    method Any()         { $!Any }
    method Block()       { $!Block }
    method Regex()       { $!Regex }
    method PseudoStash() { $!PseudoStash }
    method Code()        { $!Code }
    method Pair()        { $!Pair }
    method Routine()     { $!Routine }
    method Method()      { $!Method }
    method Signature()   { $!Signature }
    method Nil()         { $!Nil }
    method Failure()     { $!Failure }
    method False()       { $!False }
    method True()        { $!True }
    method Seq()         { $!Seq }
    method AST()         { $!AST }
    method LoweredAwayLexical() { $!LoweredAwayLexical }

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
            if +%sym && (nqp::existskey(%sym, 'value') || nqp::existskey(%sym, 'lazy_value_from')) {
                my %isym := $block.symbol("CORE-SETTING-REV");
                if +%isym {
                    return 1;
                }
                return 0;
            }
        }
        return 0;
    }

    method find_in_setting($symbol) {
        if nqp::existskey(%!SETTING_CACHE, $symbol) {
            return %!SETTING_CACHE{$symbol};
        }
        if !nqp::elems(@!CORES) {
            my int $i := +@!block_stack;
            while $i > 0 {
                $i := $i - 1;
                my $block := @!block_stack[$i];
                my %sym := $block.symbol("CORE-SETTING-REV");
                if +%sym {
                    nqp::push(@!CORES, $block);
                }
            }
            if !nqp::elems(@!CORES) {
                nqp::die("Optimizer couldn't find CORE while looking for $symbol.");
            }
        }
        for @!CORES -> $core {
            my %sym := $core.symbol($symbol);
            if +%sym {
                return %!SETTING_CACHE{$symbol} := self.force_value(%sym, $symbol, 1);
            }

        }
        nqp::die("Optimizer couldn't find $symbol in SETTING.");
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
        %opts<is-compile-time> := nqp::hllboolfor(1, "Raku");

        for %opts -> $p {
            if nqp::islist($p.value) {
                my @a := [];
                for $p.value {
                    nqp::push(@a, nqp::hllizefor($_, 'Raku'));
                }
                %opts{$p.key} := nqp::hllizefor(@a, 'Raku');
            }
            else {
                %opts{$p.key} := nqp::hllizefor($p.value, 'Raku');
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
            my $err := stderr();
            $err.say("WARNINGS for " ~ $*W.current_file ~ ":");
            my @fails;
            for %!worrying {
                $err.print($_.key ~ " (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
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
    # Retain the order of variable declarations.
    has @!decls;

    # Usages of variables in this block, or unioned in from an inlined
    # immediate block.
    has %!usages_flat;

    # Usages of variables in this block, or unioned in from a non-inlined
    # immediate block or a declaration block.
    has %!usages_inner;

    # Have we seen this block (or an inner one) making calls?
    has int $!calls;

    # Usages of getlexouter.
    has @!getlexouter_usages;

    # Setup and bind (hopefully one each) of %_.
    has @!autoslurpy_setups;
    has @!autoslurpy_binds;

    # The takedispatcher operation.
    has $!takedispatcher;

    # If lowering is, for some reason, poisoned.
    has int $!poisoned;

    # If topic lowering is, for some reason, poisoned.
    has int $!topic_poisoned;

    # If p6bindsig is used.
    has int $!uses_bindsig;

    # If p6return is used
    has int $!uses_p6return;

    has $!debug;

    # If we're currently inside a handler argument of an nqp::handle. These
    # are code-gen'd with an implicit block around them, so we mustn't lower
    # lexicals referenced in them to locals.
    has int $!in_handle_handler;
    has %!used_in_handle_handler;

    method add_decl($var) {
        my str $scope := $var.scope;
        if $scope eq 'lexical' || $scope eq 'lexicalref' {
            %!decls{$var.name} := $var;
            nqp::push(@!decls, $var);
        }
    }

    method remove_decl(str $name) {
        nqp::deletekey(%!decls, $name);
        my int $i := 0;
        for @!decls {
            if $_.name eq $name {
                nqp::splice(@!decls, [], $i, 1);
                return;
            }
            $i++;
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
            if $!in_handle_handler {
                %!used_in_handle_handler{$name} := 1;
            }
        }
    }

    method register_call() { $!calls++; }

    method unregister_call() { $!calls--; }

    method register_getlexouter_usage($node) {
        nqp::push(@!getlexouter_usages, $node);
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

    method poison_topic_lowering() { $!topic_poisoned := 1; }

    method uses_bindsig() { $!uses_bindsig := 1; }

    method uses_p6return() { $!uses_p6return := 1; }

    method entering_handle_handler() { $!in_handle_handler++; }

    method leaving_handle_handler() { $!in_handle_handler--; }

    method get_decls() { %!decls }

    method get_usages_flat() { %!usages_flat }

    method get_usages_inner() { %!usages_inner }

    method get_getlexouter_usages() { @!getlexouter_usages }

    method get_calls() { $!calls }

    method is_poisoned() { $!poisoned }

    method is_inlinable() {
        !($!poisoned || $!uses_p6return)
    }

    method get_escaping_handler_vars() {
        my @esc;
        for %!used_in_handle_handler {
            my $name := $_.key;
            unless %!decls{$name} {
                @esc.push($name);
            }
        }
        return @esc;
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

        # If the inner block uses getlexouter then we need to store those as
        # usages.
        for $vars_info.get_getlexouter_usages() {
            my $name;
            if nqp::istype($_, QAST::Op) {
                # The bind case; note some may be rewritten away.
                next unless nqp::istype($_[1], QAST::Op) && $_[1].op eq 'getlexouter';
                $name := $_[1][0].value;
            }
            else {
                # The parameter default case
                $name := $_[0][0].value;
            }
            my %target := $flattened ?? %!usages_flat !! %!usages_inner;
            %target{$name} := [] unless %target{$name};
            nqp::push(%target{$name}, $_);
        }

        # Also need to copy handler usages.
        for $vars_info.get_escaping_handler_vars() {
            %!used_in_handle_handler{$_} := 1;
        }

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

    method delete_unused_magicals($block, $can_lower_topic) {
        # Can't if we're poisoned or have to use the slow-path binder.
        return 0 if $!poisoned || $!uses_bindsig || $*DYNAMICALLY_COMPILED;

        # We handle $_ first, because it is lexical rather than dynamic. We
        # need to ensure topic lowering is OK; if it's not, there might be a
        # late-bound access.
        my %kill;
        if $can_lower_topic && !$!topic_poisoned {
            if nqp::existskey(%!decls, '$_') {
                my str $decl := %!decls<$_>.decl;
                if $decl eq 'var' || $decl eq 'contvar' {
                    unless nqp::existskey(%!usages_flat, '$_') || nqp::existskey(%!usages_inner, '$_') {
                        if !@!getlexouter_usages {
                            %kill<$_> := 1;
                            self.remove_decl('$_');
                        }
                        elsif nqp::elems(@!getlexouter_usages) == 1 {
                            my $glob := @!getlexouter_usages[0];
                            if nqp::istype($glob, QAST::Op) && $glob[0].name eq '$_' &&
                                    $glob[1][0].value eq '$_' {
                                $glob.op('null');
                                $glob.shift(); $glob.shift();
                                %kill<$_> := 1;
                                self.remove_decl('$_');
                            }
                        }
                    }
                }
            }
        }

        # Other magicals are contextual, so only consider those if we've no
        # calls (will be true in really simple builtins).
        unless $!calls {
            if nqp::existskey(%!decls, '$/') {
                if !nqp::existskey(%!usages_flat, '$/') && !nqp::existskey(%!usages_inner, '$/') {
                    %kill<$/> := 1;
                    self.remove_decl('$/');
                }
            }
            if nqp::existskey(%!decls, '$!') {
                if !nqp::existskey(%!usages_flat, '$!') && !nqp::existskey(%!usages_inner, '$!') {
                    %kill<$!> := 1;
                    self.remove_decl('$!');
                }
            }
            if nqp::existskey(%!decls, '$¢') {
                if !nqp::existskey(%!usages_flat, '$¢') && !nqp::existskey(%!usages_inner, '$¢') {
                    %kill<$¢> := 1;
                    self.remove_decl('$¢');
                }
            }
        }

        # If we found things to eliminate, do so.
        if %kill {
            my @setups := @($block[0]);
            my int $n  := nqp::elems(@setups);
            my int $i  := -1;
            while ++$i < $n {
                my $consider := @setups[$i];
                if nqp::istype($consider, QAST::Var) && nqp::existskey(%kill, $consider.name) {
                    @setups[$i] := $NULL;
                }
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

    method lexical_vars_to_locals($block, $LoweredAwayLexical, $can_lower_topic) {
        return 0 if $!poisoned || $!uses_bindsig;
        return 0 unless nqp::istype($block[0], QAST::Stmts);
        for @!decls -> $qast {
            # We're looking for lexical var/contvar decls.
            my str $scope := $qast.scope;
            next unless $scope eq 'lexical';
            my str $decl := $qast.decl;
            my int $is_contvar := $decl eq 'contvar';
            next unless $is_contvar || $decl eq 'var' || $decl eq 'param';

            # Also ensure not dynamic or with an implicit lexical usage.
            next if $qast.ann('lexical_used_implicitly');
            my $qv := $qast.value;
            my $dynamic := nqp::isconcrete_nd($qv) &&
                try nqp::getattr($qv, nqp::what_nd($qv), '$!descriptor').dynamic;
            next if $dynamic;

            # If it's a contvar, then the value should be a concrete P6opaque
            # for us to lower its initialization.
            if $is_contvar {
                next unless nqp::isconcrete_nd($qv) &&
                    nqp::reprname(nqp::what_nd($qv)) eq 'P6opaque';
            }

            # Consider name. Can't lower if it's used by any nested blocks or
            # in an nqp::handlers handler.
            my str $name := $qast.name;
            unless nqp::existskey(%!usages_inner, $name) ||
                    nqp::existskey(%!used_in_handle_handler, $name) {
                # Lowerable if it's a normal variable, including $_ if we're in a
                # Raku version that allows lowering that.
                next if nqp::chars($name) < 1;
                unless nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $name, 0) {
                    my str $sigil := nqp::substr($name, 0, 1);
                    next unless $sigil eq '$' || $sigil eq '@' || $sigil eq '%';
                    next unless nqp::chars($name) >= 2 &&
                                (nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $name, 1) ||
                                 $can_lower_topic && !$!topic_poisoned && nqp::eqat($name, '_', 1));
                }

                # Also must not lexicalref it.
                my int $ref'd := 0;
                if %!usages_flat{$name} {
                    for %!usages_flat{$name} {
                        if nqp::istype($_, QAST::Var) && $_.scope eq 'lexicalref' {
                            $ref'd := 1;
                            last;
                        }
                    }
                }
                next if $ref'd;

                # Seems good; lower it. Note we need to retain a lexical in
                # case of binder failover to generate errors for parameters,
                # and also to preserve behavior of CALLER::foo dying when we
                # try to access a lexical that was lowered. We install the
                # symbol Rakudo::Internals::LoweredAwayLexical for the latter
                # purpose.
                my $new_name := $qast.unique('__lowered_lex');
                if nqp::objprimspec($qast.returns) {
                    $block[0].unshift(QAST::Var.new(
                        :name($qast.name), :scope('lexical'), :decl('var'),
                        :returns($qast.returns)
                    ));
                }
                else {
                    $block[0].unshift(QAST::Var.new(
                        :name($qast.name), :scope('lexical'), :decl('static'),
                        :value($LoweredAwayLexical)
                    ));
                }
                $qast.name($new_name);
                $qast.scope('local');
                if $is_contvar {
                    # Instead of the vivify on first read, we instead set up
                    # the variable's container. The naive way to do that would
                    # be a clone of the prototype value, but to explicitly
                    # bindattr is much more analyzable by VM-level optimizers,
                    # such as MoarVM's spesh. We skip this for the `our` case,
                    # as it is bound immediately.
                    $qast.decl('var');
                    unless $qast.ann('our_decl') {
                        my $type := nqp::what_nd($qv);
                        my $setup := QAST::Op.new(
                            :op('create'),
                            QAST::WVal.new( :value($type) )
                        );
                        for $type.HOW.mro($type) -> $mro_entry {
                            for $mro_entry.HOW.attributes($mro_entry, :local) -> $attr {
                                my str $name := $attr.name;
                                if $name eq '$!descriptor' || $name eq '$!value' {
                                    $setup := QAST::Op.new(
                                        :op('p6bindattrinvres'),
                                        $setup,
                                        QAST::WVal.new( :value($mro_entry) ),
                                        QAST::SVal.new( :value($name) ),
                                        QAST::WVal.new( :value(nqp::getattr($qv, $mro_entry, $name)) )
                                    );
                                }
                            }
                        }
                        $block[0].push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($new_name), :scope('local') ),
                            $setup
                        ));
                    }
                }
                if %!usages_flat{$name} {
                    for %!usages_flat{$name} {
                        if nqp::istype($_, QAST::Var) {
                            $_.scope('local');
                            $_.name($new_name);
                        }
                        elsif nqp::istype($_, QAST::Op) && $_.op eq 'bind' &&
                                nqp::istype($_[1], QAST::Op) && $_[1].op eq 'getlexouter' {
                            $_[1] := QAST::Var.new( :name($new_name), :scope('local') );
                        }
                        elsif nqp::istype($_, QAST::Stmts) && nqp::istype($_[0], QAST::Op) &&
                                $_[0].op eq 'getlexouter' {
                            $_[0] := QAST::Var.new( :name($new_name), :scope('local') );
                        }
                        else {
                            nqp::die("Unexpected node in usages_flat");
                        }
                    }
                }

                # Stash the name we lowered it to, and add a debug mapping.
                $block.symbol($name, :lowered($new_name));
                $block.add_local_debug_mapping($new_name, $name);
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
            my @candidates;
            if $obj.is_dispatcher {
                @candidates := nqp::getattr($obj, $!symbols.Routine, '@!dispatchees');
            }
            else {
                @candidates := nqp::list($obj);
            }
            my $signature := $!symbols.find_in_setting("Signature");
            my $canditer := nqp::iterator(@candidates);
            while $canditer {
                my $cand := nqp::shift($canditer);
                my $iter := nqp::iterator(nqp::getattr($cand.signature, $signature, '@!params'));
                while $iter {
                    my $p := nqp::shift($iter);
                    unless nqp::istype($p.type, $!symbols.Any) {
                        return 0;
                    }
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

# LHS/RHS operand classification by value
my $OPERAND_VALUE_VAR      := 0;
my $OPERAND_VALUE_CONST    := 1;
my $OPERAND_VALUE_RETURN   := 2;

my class Operand {
    has $!ast;
    has $!unwrapped;
    # Invocations can be wrapped into hllize op. Here we will keep the actuall nqp::call* branch of AST
    has $!invocation-ast;
    has $!is-want;
    has $!value;
    has $!value-kind;
    has $!is-instantiation;
    has $!routine;
    has $!routine-name;
    has $!analyzed;
    has $!optimizer;
    has $!symbols;
    has $!debug;
    has $!name;
    has $!can-be-junction;

    # Localized version of operand stores its value in a local variable, so that when it's needed more than once
    # we don't call it again and again when it comes to non-Var or non-WVal QAST node types.
    # This status is advisable. Methods orig-ast and localize ignore it though try to set appropriate mode. But
    # method ast respect the mode and returns appropriate form of AST node.
    has $!local-var-name;
    has $!localizable;

    method new($ast, $optimizer, $symbols, :$name = nqp::null(), :$localizable = nqp::null()) {
        my $obj := nqp::create(self);
        $obj.BUILD($ast, $optimizer, $symbols, :$name, :$localizable);
        $obj
    }

    method new-from-self($ast, :$name = nqp::null(), :$localizable = nqp::null()) {
        self.new($ast, $!optimizer, $!symbols, :$name, :$localizable)
    }

    method BUILD($ast, $optimizer, $symbols, :$name, :$localizable) {
        $!ast := $ast;
        $!unwrapped := ($!is-want := nqp::istype($ast, QAST::Want))
                        ?? $ast[0]
                        !! $ast;
        $!invocation-ast := nqp::null();
        $!analyzed := 0;
        $!value := nqp::null();
        $!value-kind := nqp::null();
        $!routine := nqp::null();
        $!local-var-name := nqp::null();
        $!symbols := $symbols;
        $!optimizer := $optimizer;
        $!debug := nqp::getenvhash<RAKUDO_OPTIMIZER_DEBUG> || nqp::getenvhash<RAKUDO_OPTIMIZER_SM_DEBUG>;
        $!name := $name;
        $!localizable := $localizable;
        $!can-be-junction := nqp::null();
        $!is-instantiation := nqp::null();
    }

    # $!localizable can be set only once.
    method localizable($set = nqp::null()) {
        $!localizable := nqp::istrue($set) if nqp::isnull($!localizable) && !nqp::isnull($set);
        $!localizable
    }

    method orig-ast() {
        # Set non-localizable mode unless set already.
        self.localizable(0);
        $!ast
    }
    # Method ast() may return a QAST node of a localized variable bound to LHS.
    method ast(:$decl = nqp::null()) {
        self.localizable(nqp::defined($decl))
            ?? self.localized(:$decl)
            !! $!ast
     }
    method is-want() { $!is-want }
    method unwrapped() { $!unwrapped }

    method value-analyze() {
        unless $!analyzed {
            if nqp::istype($!unwrapped, QAST::Var) {
                my $value := $!ast.returns;
                if nqp::isnull($value) || nqp::istype($value, NQPMu) {
                    my %sym := $!symbols.find_lexical_symbol($!unwrapped.name);
                    $value := $!symbols.force_value(%sym, $!unwrapped.name, 0);
                }
                $!value := $value unless nqp::isnull($value) || nqp::istype($value, NQPMu);
                $!value-kind := $OPERAND_VALUE_VAR;
            }
            elsif nqp::istype($!unwrapped, QAST::WVal)
                || nqp::istype($!unwrapped, QAST::IVal)
                || nqp::istype($!unwrapped, QAST::SVal)
                || nqp::istype($!unwrapped, QAST::NVal)
            {
                $!value := $!unwrapped.value;
                # Whatever is in XVal – it is a value, even if it's not concrete.
                $!value-kind := $OPERAND_VALUE_CONST;
            }
            elsif nqp::istype($!unwrapped, QAST::Op) {
                my $unwrapped := $!unwrapped.op eq 'hllize' ?? $!unwrapped[0] !! $!unwrapped;
                my $optype := $unwrapped.op;
                if $optype eq 'call' || $optype eq 'callstatic' {
                    # If for whatever reason routine is not there yet or we can't infer its return value then assume it
                    # can be anything.
                    $!value := $!symbols.Mu;
                    $!value-kind := $OPERAND_VALUE_RETURN;
                    $!invocation-ast := $unwrapped;
                    $!routine-name := $!invocation-ast.name;
                    $!routine := $!symbols.find_lexical($!routine-name)
                        if nqp::isconcrete($!routine-name) && nqp::chars($!routine-name);
                }
                elsif $optype eq 'callmethod' {
                    $!value := $!symbols.Mu;
                    $!value-kind := $OPERAND_VALUE_RETURN;
                    $!invocation-ast := $unwrapped;
                    $!routine-name := $!invocation-ast.name;
                    my $invocant := self.new-from-self($!invocation-ast[0]);
                    if $invocant.has-value && $invocant.value-kind == $OPERAND_VALUE_CONST {
                        my $invocant_type := $invocant.value-type;
                        $!routine := $invocant_type.HOW.find_method($invocant_type, $!routine-name);
                        note("Found method '", $!routine-name, "': ", $!routine.HOW.name($!routine)) if $!debug;
                    }
                }
                elsif $!debug {
                    note("Unsupported op '", $!ast.op, "' for ", ($!name ?? $!name ~ " " !! ""), "operand");
                    note($!ast.dump(4));
                }

                if nqp::isconcrete($!routine)
                    && nqp::istype($!routine, $!symbols.Code)
                    && nqp::isconcrete($!routine.signature)
                    && $!routine.signature.has_returns
                {
                    $!value := $!routine.signature.returns;
                }
            }
            else {
                note("Unsupported ", $!ast.HOW.name($!ast),
                    " node for ",
                    ($!name ?? $!name ~ " " !! ""),
                    "operand"
                ) if $!debug;
            }
            $!analyzed := 1;
        }
    }

    method has-value() {
        self.value-analyze;
        nqp::not_i(nqp::isnull($!value))
    }

    method value() {
        self.value-analyze;
        $!value
    }

    method value-type() {
        self.value-analyze;
        # We need this ternary for JVM where otherwise it may throw on VMNull in $!value
        nqp::isnull($!value) ?? nqp::null() !! nqp::what($!value)
    }

    method value-kind() {
        self.value-analyze;
        $!value-kind
    }

    method value-type-name() {
        $!value.HOW.name($!value)
    }

    method is-type-object($type = nqp::null(), :$strict = 0) {
        self.value-analyze;
        $!value-kind == $OPERAND_VALUE_CONST
            && !nqp::isconcrete($!value)
            && (nqp::isnull($type)
                || ($strict ?? nqp::eqaddr($!value, $type) !! nqp::istype($!value, $type)))
    }

    method str() {
        self.value-analyze;
        if $!value-kind == $OPERAND_VALUE_CONST {
            # A string constant
            if nqp::istype($!ast, QAST::Want) && $!ast[1] eq 'Ss' {
                return $!ast[2].value;
            }
            if nqp::objprimspec($!unwrapped.value) {
                return nqp::stringify($!unwrapped.value);
            }
            if nqp::can($!unwrapped.value, 'Str') {
                return nqp::unbox_s($!unwrapped.value.Str);
            }
        }
        elsif $!value-kind == $OPERAND_VALUE_RETURN && nqp::isconcrete($!value) {
            return nqp::stringify($!value) if nqp::objprimspec($!value);
            return nqp::unbox_s($!value.Str) if nqp::can($!value, 'Str');
        }
        return nqp::null();
    }

    method int() {
        self.value-analyze;
        if $!value-kind == $OPERAND_VALUE_CONST {
            # A string constant
            if nqp::istype($!ast, QAST::Want) && $!ast[1] eq 'Ii' {
                return $!ast[2].value;
            }
            if nqp::objprimspec($!unwrapped.value) {
                return nqp::intify($!unwrapped.value);
            }
            if nqp::can($!unwrapped.value, 'Int') {
                return nqp::unbox_i($!unwrapped.value.Int);
            }
        }
        elsif $!value-kind == $OPERAND_VALUE_RETURN && nqp::isconcrete($!value) {
            return nqp::intify($!value) if nqp::objprimspec($!value);
            return nqp::unbox_i($!value.Int) if nqp::can($!value, 'Int');
        }
        elsif nqp::istype($!ast, QAST::Op) && $!ast.op eq 'hllbool' {
            return self.new-from-self($!ast[0]).bool
        }
        return nqp::null();
    }

    # Produce a "native bool", i.e. 0/1 value.
    method bool() {
        self.value-analyze;
        if $!value-kind == $OPERAND_VALUE_CONST {
            # A string constant
            if nqp::istype($!ast, QAST::Want) {
                return nqp::istrue($!ast[2].value);
            }
            if nqp::objprimspec($!unwrapped.value) {
                return nqp::istrue($!unwrapped.value);
            }
            if nqp::can($!unwrapped.value, 'Bool') {
                return nqp::istrue($!unwrapped.value.Bool);
            }
        }
        elsif $!value-kind == $OPERAND_VALUE_RETURN && nqp::isconcrete($!value) {
            return nqp::istrue($!value) if nqp::objprimspec($!value);
            return nqp::istrue($!value.Bool) if nqp::can($!value, 'Bool');
        }
        elsif nqp::istype($!ast, QAST::Op) && $!ast.op eq 'hllbool' {
            return self.new-from-self($!ast[0]).bool
        }
        return nqp::null();

    }

    # If operand has a concrete compile-time value then get a native out of it if possible.
    method native-value() {
        return nqp::null() unless self.is-literal && (my $spec := nqp::objprimspec($!value));
        return nqp::unbox_i($!value) if $spec == nqp::const::BIND_VAL_INT;
        return nqp::unbox_u($!value) if $spec == nqp::const::BIND_VAL_UINT;
        return nqp::unbox_n($!value) if $spec == nqp::const::BIND_VAL_NUM;
        return nqp::unbox_s($!value);
    }

    method invocation-args() {
        return nqp::null() unless self.is-invocation;
        my @args;
        for @($!invocation-ast) {
            @args.push(self.new-from-self($_));
        }
        @args
    }

    method is-junction-static() {
        (self.has-value && nqp::isconcrete($!value) && nqp::istype($!value, $!symbols.Junction))
            || (self.is-instantiation && nqp::istype(self.instantiation-type, $!symbols.Junction));
    }

    my %junction-routines := nqp::hash(
        '&all', 1, '&any', 1, '&one', 1, '&none', 1,
        '&infix:<&>', 1, '&infix:<|>', 1, '&infix:<^>', 1
    );
    method is-junction() {
        self.is-junction-static
            || (self.is-invocation
                && nqp::existskey(%junction-routines, $!routine-name)
                && $!optimizer.is_routine_setting_only($!routine))
    }

    # To find out if the operand could result in a junction we check its type in first place. But for an invocation we
    # must also consider the possibility of autothreading. For this purpose we iterate over invocation arguments and
    # check if any of them could result in a junction.
    method can-be-junction() {
        return $!can-be-junction unless nqp::isnull($!can-be-junction);
        return 1 if $!can-be-junction := self.can-be($!symbols.Junction);
        if self.is-invocation {
            my $arg_i := 0;
            for @($!invocation-ast) -> $arg_ast {
                if self.new-from-self($arg_ast).can-be-junction {
                    return $!can-be-junction := 1;
                }
            }
        }
        0
    }

    method can-be($type) {
        return 1 if nqp::isnull(my $op-type := self.infer-type(:guaranteed));
        # 'Can be' condition is a two-way thing. If we ask if something can be Int then if operand type is Real it can
        # potentially be Int; yet, if it is IntStr then it is already an Int and the answer to 'can be' is 'yes' too.
        (nqp::istype($type, $op-type)
            || nqp::istype($op-type, $type)
            || (($!value-kind == $OPERAND_VALUE_VAR || $!value-kind == $OPERAND_VALUE_RETURN)
                && nqp::eqaddr($!value, $!symbols.Mu)))
    }

    method routine() {
        self.value-analyze;
        $!routine
    }

    method is-op($opname) {
        nqp::istype($!ast, QAST::Op) && $!ast.op eq $opname
    }

    method is-invocation() {
        self.value-analyze;
        nqp::defined($!invocation-ast)
    }

    method is-instantiation($type = nqp::null()) {
        return $!is-instantiation unless nqp::isnull($!is-instantiation);
        $!is-instantiation :=
            self.is-invocation && ($!invocation-ast.name eq 'new')
                && (nqp::isnull($type)
                    || (nqp::istype($!invocation-ast[0], QAST::WVal)
                        && nqp::eqaddr($!invocation-ast[0].value, $type)))
    }

    # With :guaranteed the method will make sure that type returned by .new will not be different from the invocant
    # type. This kind of situation is possible if new/bless methods are overriden and their implementation allows
    # returning something different from their invocant. For example, some iterators may return Empty iterator if they
    # see there will no elements to iterate over.
    method instantiation-type(:$guaranteed = 0) {
        return nqp::null() unless self.is-instantiation;
        my $invocant := self.invocation-args()[0];
        return nqp::null() unless $invocant.value-kind == $OPERAND_VALUE_CONST;
        my $type := $invocant.value-type;
        if $guaranteed {
            # List of core types for which .new is guaranteed to return an instance of the same type.
            my @ok-types := [ $!symbols.Mu,
                              $!symbols.Junction,
                              $!symbols.Failure,
                              $!symbols.Pair,
                              $!symbols.find_in_setting('Range'),
                              $!symbols.find_in_setting('Int'),
                              $!symbols.find_in_setting('Num'),
                              $!symbols.find_in_setting('Str'),
                              $!symbols.find_in_setting('Label') ];
            my $is-guaranteed := 1;
            my $new-type;
            for ['new', 'bless'] -> $constructor {
                if nqp::defined(my $meth := nqp::tryfindmethod($type, $constructor)) {
                    for $!optimizer.routine_candidates($meth) -> $cand {
                        next unless nqp::can($cand, 'package');
                        my $cand-pkg := $cand.package;
                        my $cand-ok := 0;
                        for @ok-types -> $ok-type {
                            last if $cand-ok := nqp::eqaddr($cand-pkg, $ok-type);
                        }
                        last unless $is-guaranteed := $cand-ok;
                    }
                }
                last unless $is-guaranteed;
            }

            unless $is-guaranteed {
                # The type of returned instance can't be guaranteed. In this case .new return type is the only what we
                # can report back.
                $type := self.value-type;
            }
        }
        $type
    }

    method infer-type(:$guaranteed = 0) {
        self.is-instantiation ?? self.instantiation-type(:$guaranteed) !! self.value-type
    }

    method is-literal() {
        self.has-value
            && ($!value-kind == $OPERAND_VALUE_CONST
                && nqp::defined($!value))
    }

    method dump($level = 0) {
        $!ast.dump($level)
    }

    method localized(:$decl = 0) {
        # If this method is invoked first then finalize localizable status unless it is set already.
        self.localizable(1);
        # Var and WVal do not need localization
        return $!ast if nqp::istype($!unwrapped, QAST::Var) || nqp::istype($!unwrapped, QAST::WVal);

        $!local-var-name := QAST::Node.unique('__local_sm_operand_') if nqp::isnull($!local-var-name);

        return QAST::Op.new(
            :op<bind>,
            QAST::Var.new(:name($!local-var-name), :scope<local>, :decl<var>),
            $!ast
        ) if $decl;

        QAST::Var.new(:name($!local-var-name), :scope<local>)
    }

    method is-method-default($method-name, :$u-invocant = 0) {
        my $type := self.is-instantiation ?? self.instantiation-type !! self.value-type;
        return 0 if nqp::isnull($type);
        # No need to check for core types
        unless $!symbols.is_from_core($type.HOW.name($type)) {
            # When a non-core type is instantiated we cannot guarantee the resulting instance will have the same type,
            # as .new invocator.
            return 0 if $!is-instantiation;

            my $meth := nqp::tryfindmethod($type, $method-name);
            return nqp::defined($meth) && $!optimizer.is_routine_setting_only($meth, :$u-invocant);
        }
        1
    }

    method is-ACCEPTS-default(:$u-invocant = 0) {
        self.is-method-default('ACCEPTS', :$u-invocant)
    }
}

my class SmartmatchOptimizer {
    has $!symbols;
    has $!optimizer;
    has $!debug;

    # Don't try compile-time known literals optimization for types on this list. They may rely on lexical/dynamic outers.
    my @literal-rhs-exceptions;

    method new($optimizer, $symbols) {
        my $obj := nqp::create(self);
        $obj.BUILD($optimizer, $symbols);
        $obj
    }

    method BUILD($optimizer, $symbols) {
        $!symbols := $symbols;
        $!optimizer := $optimizer;
        $!debug   := nqp::getenvhash<RAKUDO_OPTIMIZER_DEBUG> || nqp::getenvhash<RAKUDO_OPTIMIZER_SM_DEBUG>;
        @literal-rhs-exceptions := nqp::list($!symbols.Code, $!symbols.Signature) unless +@literal-rhs-exceptions;
    }

    method respect_junctions($op, $fallback, $lhs?) {
        my $topic;
        my $topic_decl;
        if nqp::isconcrete($lhs) {
            if nqp::istype($lhs, Operand) {
                $topic_decl := $lhs.ast(:decl);
                $topic := $lhs.ast;
            }
            else {
                $topic_decl := $topic := $lhs;
            }
        }
        else {
            $topic_decl := $topic := QAST::Var.new( :name('$_'), :scope('lexical') ),
        }
        # If LHS is a concrete Junction then use unoptimized code
        QAST::Op.new(
            :op<if>,
            QAST::Op.new(
                :op<if>,
                QAST::Op.new( :op<istype>, $topic_decl, QAST::WVal.new( :value($!symbols.Junction) )),
                QAST::Op.new( :op<isconcrete>, $topic )),
            $fallback,
            $op
        )
    }

    method maybe_respect_junctions($lhs, $rhs, $optimized_ast, :$negated = 0) {
        return $optimized_ast unless $lhs.can-be-junction;
        # typematching against a Junction doesn't need a fallback
        return $optimized_ast if $optimized_ast.ann('sm_typematch') && $rhs.is-type-object($!symbols.Junction);
        my $fallback_ast;
        if $rhs.has-value && $rhs.value-kind == $OPERAND_VALUE_CONST && $rhs.is-ACCEPTS-default {
            # When we can guarantee that RHS uses CORE's ACCEPTS then instead of doing .ACCEPTS.Bool chain where all
            # Junction eigenstates will be evaluated we can try BOOLIFY-ACCEPTS which would shortcut whenever possible.
            $fallback_ast :=
                QAST::Op.new(
                    :op<callmethod>,
                    :name<BOOLIFY-ACCEPTS>,
                    $lhs.localized,
                    $rhs.ast,
                    QAST::WVal.new( :value(nqp::hllboolfor($negated, 'Raku')) )
                );
        }
        else {
            my $bool-method := $negated ?? 'not' !! 'Bool';
            $fallback_ast :=
                QAST::Op.new(
                    :op<callmethod>,
                    :name($bool-method),
                    QAST::Op.new(
                        :op<callmethod>,
                        :name<ACCEPTS>,
                        $rhs.ast,
                        $lhs.localized
                    )
                )
        }
        self.respect_junctions( $optimized_ast, $fallback_ast, $lhs )
    }

    my $Mu'U := nqp::null;

    method maybe_typematch($lhs, $rhs, :$in-when = 0, :$negated = 0) {
        my $sm_type;
        # Don't try if RHS is not a compile-time known type object or it has user-defined ACCEPTS method. In the latter
        # case we can't guarantee that method behavior is independent of run-time conditions, contrary to core-provided
        # versions of it.
        return nqp::null()
            unless $rhs.is-type-object
                    && $rhs.is-ACCEPTS-default(:u-invocant)
                    && ($in-when || !$lhs.is-junction);

        $sm_type := $rhs.value;
        my $sm_type_how := nqp::how($sm_type);

        # We wouldn't be able to shortcut typematching when:
        # - can't determine type's archetype
        # - the type is a generic
        return nqp::null()
            if !nqp::can($sm_type_how, 'archetypes')
                || $sm_type_how.archetypes($sm_type).generic;

        # This edge case muddies up the visual waters quite a bit in hopes of keeping the optimizer speedy
        return QAST::Op.new( :op<callmethod>, :name<Bool>,
                QAST::Op.new( :op<istype>, $lhs.ast, $rhs.ast )
        )   if $lhs.value-kind == $OPERAND_VALUE_VAR
            && nqp::eqaddr($sm_type, nqp::ifnull($Mu'U, $Mu'U := $!symbols.Mu'U))
            && ! ($sm_type_how.archetypes($sm_type).definite && $sm_type_how.definite($sm_type));

        my $sm_is_subset :=
            $sm_type_how.archetypes($sm_type).nominalizable
            && !nqp::isnull($sm_type_how.wrappee-lookup($sm_type, :subset));

        note("Try typematch over RHS ", $sm_type_how.name($sm_type)) if $!debug;

        my $sm_ast;

        # When ~~ LHS is known at compile time we sometimes can reduce down to a plain boolean constant.
        # Doesn't work for 'when' statement and for subsets. The latter may depend on run-time conditions in their
        # `when` code.
        if !$sm_is_subset && !$in-when && $lhs.has-value {
            note("  . typematch: LHS has a value (",
                $lhs.value-type-name, ", ",
                (nqp::isconcrete($lhs.value) ?? "concrete" !! "undefined"),
                ", ast returns: ", $lhs.ast.returns.HOW.name($lhs.ast.returns),
                "), constant substitution is possible") if $!debug;

            return nqp::null() if $lhs.value.HOW.archetypes($lhs.value).generic;

            # Wrap into try because for if there a user-defined `where`-block involved into typematching it might throw.
            # Consider this case a failed typematch and proceed further.
            my $matches := try nqp::istype($lhs.value, $rhs.value);
            # If LHS is an invocation or a variable then we actually check their (return) type. In this case non-match
            # means nothing because their eventual value could still match at runtime. Yet true means that any of their
            # value will always match. Also, if routine returns a constant then we can always use it too.
            if $matches
                || $lhs.value-kind == $OPERAND_VALUE_CONST
                || ($lhs.value-kind == $OPERAND_VALUE_RETURN && nqp::isconcrete($lhs.value))
            {
                $matches := !$matches if $negated;
                return QAST::WVal.new( :value($matches ?? $!symbols.True !! $!symbols.False) )
            }
        }

        # See if we can reduce a call to ACCEPTS into a basic `nqp::istype`. By doing so we must keep in mind that a
        # user class may have their own ACCEPTS candidate(s) for this purpose. Therefore make sure that no candidate
        # overrides CORE's methods.
        note("Typematch over ", $sm_type.HOW.name($sm_type), " can be reduced to nqp::istype") if $!debug;

        $sm_ast := QAST::Op.new( :op<istype>, $lhs.ast, $rhs.ast );
        $sm_ast := QAST::Op.new( :op<not_i>, $sm_ast ) if $negated;
        # For 'when' statement it is sufficient to get native 0 or 1. Otherwise we need to boolify.
        ($in-when
            ?? $sm_ast
            !! QAST::Op.new( :op<hllbool>, $sm_ast )).annotate_self('sm_typematch', 1)
    }

    method maybe_literal($lhs, $rhs, :$in-when = 0, :$negated) {
        return nqp::null()
            unless $rhs.is-literal && !$lhs.is-junction;

        note("Try literal smartmatch") if $!debug;

        # Both sides are literal values. Thus we can produce True/False right away.
        # Doesn't work for 'when' statement.
        if !$in-when
            && $rhs.is-ACCEPTS-default
            && ($lhs.is-literal
                || ($lhs.value-kind == $OPERAND_VALUE_RETURN && nqp::isconcrete($lhs.value)))
        {
            my $try-it := 1;
            my $rhs-type := nqp::what($rhs.value);
            for @literal-rhs-exceptions {
                last unless $try-it := $try-it && !try nqp::istype($rhs-type, $_);
                note($rhs-type.HOW.name($rhs-type), " is not ", $_.HOW.name($_)) if $!debug;
            }
            if $try-it {
                note("Both sides are literal, try static match of ",
                        $lhs.value-type-name,
                        " vs. ",
                        $rhs.value-type-name)
                    if $!debug;
                my $is_eq := nqp::null();
                try {
                    $is_eq := $rhs.value.ACCEPTS($lhs.value);
                    $is_eq := $negated ?? $is_eq.not !! $is_eq.Bool;
                }
                note("  = is eq? ", (nqp::isnull($is_eq) ?? "match thrown" !! $is_eq.WHICH)) if $!debug;
                # Fallback to dynamic path if try block failed.
                return QAST::WVal.new( :value($is_eq) ) unless nqp::isnull($is_eq);
            }
        }

        return nqp::null() unless $rhs.is-want;

        my $op_type;
        my $method;
        my $rhs_val := $rhs.ast[2];
        my $topic_name;
        my $topic_scope;

        if $in-when {
            $topic_name := '$_';
            $topic_scope := 'lexical';
        }
        else {
            $topic_name := QAST::Node.unique('__local_sm_LHS_');
            $topic_scope := 'local';
        }

        if nqp::istype($rhs_val, QAST::IVal) {
            $op_type := '==';
            $method  := 'Numeric';
        }
        elsif nqp::istype($rhs_val, QAST::SVal) {
            $op_type := 'eq';
            $method  := 'Stringy';
        }
        elsif nqp::istype($rhs_val, QAST::NVal) {
            if nqp::isnanorinf($rhs_val.value) {
                # If we know that RHS is NaN or Inf then the only time we can optimize is when LHS is a Num. Otherwise
                # only the slow path is possible as, say, Complex cannot be equal to NaN.
                return nqp::null()
                    unless $lhs.has-value
                            && nqp::istype($lhs.value, $!symbols.find_in_setting('Num'));
                $op_type := '===';
            }
            else {
                $op_type := '==';
            }
            $method  := 'Numeric';
        }
        else {
            die("Unknown QAST::Want type " ~ $rhs_val.HOW.name($rhs_val));
        }

        my $op_name := "&infix:<$op_type>";

        # Coercion method to call on the given value
        my $method_call :=
            QAST::Op.new(
                :op<p6fatalize>,
                :name($method),
                :wanted(1),
                QAST::Op.new(
                    :op<callmethod>,
                    :name($method),
                    QAST::Var.new( :name($topic_name), :scope($topic_scope), :wanted(1) ) ),
                QAST::WVal.new( :value($!symbols.Failure) ));

        # We don't need/want `val()` to `fail()` if `Numeric()` ends up calling it
        # and it doesn't succeed, that creates an expensive Backtrace that we just
        # end up throwing away
        if $method eq 'Numeric' {
            my $fail-or-nil := QAST::Op.new( :op('hllbool'), QAST::IVal.new( :value(1) ) );
            $fail-or-nil.named('fail-or-nil');
            $method_call[0].push($fail-or-nil);

            # Rewrite the `$LHS.Numeric` into `my $tmp := $LHS.Numeric(:fail-or-nil); $LHS := nqp::istype($tmp, Nil) ?? NaN !! $tmp;`
            # Since we already explicitly handle the RHS being NaN above, this is safe because NaN isn't == to anything, so
            # when we do `$LHS == $RHS` it will always be False
            my $tmp := QAST::Node.unique('nilee');
            $method_call[0] :=
                QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                        $method_call[0]
                    ),
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('istype'),
                            QAST::Var.new( :name($tmp), :scope('local') ),
                            QAST::WVal.new( :value($!symbols.Nil) ),
                        ),
                        QAST::NVal.new( :value(nqp::nan) ),
                        QAST::Var.new( :name($tmp), :scope('local') )
                    ))
        }

        # Make sure we're not comparing against a type object, since those could
        # coerce to the value, so gen the equivalent of
        # `isconcrete($_) && <literal> ==|eq $_."$method"`
        my $topic_var;
        # With 'when' statement we always have $_. But with smartmatch we don't need it, thus use LHS directly.
        if $in-when {
            $topic_var := QAST::Var.new( :name($topic_name), :scope($topic_scope) );
        }
        else {
            $topic_var := QAST::Op.new(
                :op<bind>,
                QAST::Var.new( :name($topic_name), :scope($topic_scope), :decl<var> ),
                $lhs.ast
            );
        }
        my $is_eq_op;

        if $op_type eq '===' {
            $is_eq_op := QAST::Op.new(
                            :op<eqaddr>,
                            QAST::Op.new( :op<decont>, $method_call ),
                            QAST::WVal.new( :value($rhs.value) ));
            $is_eq_op := QAST::Op.new( :op<not_i>, $is_eq_op ) if $negated;
            $is_eq_op := QAST::Op.new( :op<hllbool>, $is_eq_op );
        }
        else {
            $is_eq_op := QAST::Op.new( :op<call>, :name($op_name), $rhs_val, $method_call );
            $is_eq_op := QAST::Op.new( :op<call>, :name('&prefix:<!>'), $is_eq_op ) if $negated;
        }

        $is_eq_op :=
            QAST::Op.new(
                :op<if>,
                QAST::Op.new( :op<isconcrete>, $topic_var ),
                QAST::Op.new(
                    :op<p6fatalize>,
                    :name($op_name),
                    $is_eq_op,
                    QAST::WVal.new( :value($!symbols.Failure) )));

        # For a ~~ we expect a Bool, not a native 0
        $is_eq_op.push(QAST::WVal.new( :value($!symbols.False) )) unless $in-when;

        # This is the equivalent of sticking a `try` before the genned `iseq_*`, which is
        # needed because otherwise it'll die with an error when the `$_` is a different type.
        QAST::Op.new(
            :op('handle'),
            # Success path evaluates to the block.
            $is_eq_op,
            # On failure, just evaluate to False
            'CATCH',
            QAST::WVal.new( :value($!symbols.False) ))
    }

    # If we do have a 'pair ~~ pair' case then we better always return some AST as this would signal the upstream code
    # that no extra checks are needed and AST nodes like run-time checking for RHS to be a Regex can be dropped off.
    method pair_to_pair($lhs, $rhs) {
        my $lhs_type := $lhs.is-instantiation ?? $lhs.instantiation-type !! $lhs.value-type;

        return nqp::null() unless nqp::istype($lhs_type, $!symbols.Pair);

        my sub default-ast() {
            QAST::Op.new(
                :op<callmethod>,
                :name<Bool>,
                QAST::Op.new(
                    :op<callmethod>,
                    :name<ACCEPTS>,
                    $rhs.orig-ast,
                    $lhs.orig-ast ))
        }

        unless $lhs.is-ACCEPTS-default && $rhs.is-ACCEPTS-default {
            # With a user-defined ACCEPTS method no optimization is possible.
            return default-ast();
        }

        # "Prefix" ast nodes.
        my @past;

        my $rhs_key;
        my $rhs_value;
        if $rhs.is-literal {
            $rhs_key := $rhs.value.key;
            $rhs_value := $rhs.value.value;
        }
        elsif $rhs.is-instantiation {
            my @inst-args := $rhs.invocation-args;
            $rhs_key := @inst-args[1].has-value ?? @inst-args[1].value !! @inst-args[1];
            $rhs_value := @inst-args[2].has-value ?? @inst-args[2].value !! @inst-args[2];
        }
        else {
            # For run-time only RHS values anything but the default ACCEPTS is an overcomplication of things.
            return default-ast();
        }

        my $lhs_key;
        my $lhs_value;
        if $lhs.is-literal {
            $lhs_key := $lhs.value.key;
            $lhs_value := $lhs.value.value;
        }
        elsif $lhs.is-instantiation {
            my @inst-args := $lhs.invocation-args;
            $lhs_key := @inst-args[1].has-value ?? @inst-args[1].value !! @inst-args[1];
            $lhs_value := @inst-args[2].has-value ?? @inst-args[2].value !! @inst-args[2];
        }
        else {
            # See the note for RHS above.
            return default-ast();
        }

        if nqp::objprimspec($rhs_key) && nqp::objprimspec($rhs_value)
            && nqp::objprimspec($lhs_key) && nqp::objprimspec($lhs_value)
        {
            return QAST::WVal.new(
                :value(
                    nqp::iseq_s(nqp::stringify($rhs_key), nqp::stringify($lhs_key))
                    && nqp::iseq_s(nqp::stringify($rhs_value), nqp::stringify($lhs_value))
                        ?? $!symbols.True
                        !! $!symbols.False
                )
            )
        }
    }

    method maybe_pair($lhs, $rhs, $sm_accepts_ast, :$negated = 0) {
        return nqp::null()
            unless ($rhs.has-value && nqp::istype($rhs.value, $!symbols.Pair))
                    || ($rhs.is-instantiation && nqp::istype($rhs.instantiation-type, $!symbols.Pair));

        # Note that this method only uses $lhs for analysis. Since smartmatching on Pair is always topicalized we must
        # use the topic. If all goes well it will later be lowered anyway.

        my $res_ast;

        # Pair ~~ Pair case
        return $res_ast unless nqp::isnull($res_ast := self.pair_to_pair($lhs, $rhs));

        my $rhs_ast := $rhs.orig-ast;

        my $method_name := nqp::null();
        my $bool_value;

        my $pair_type_ast;

        if $rhs.is-instantiation($!symbols.Pair) {
            my @inst-args  := $rhs.invocation-args;
            $pair_type_ast := @inst-args[0].ast;
            my $key-arg    := @inst-args[1];
            my $value-arg  := @inst-args[2];

            $method_name := nqp::ifnull($key-arg.str, $key-arg);
            $bool_value := nqp::ifnull($value-arg.bool, $value-arg);
        }

        if $rhs.is-literal {
            $method_name := nqp::unbox_s($rhs.value.key.Str);
            $bool_value := nqp::istrue($rhs.value.value.Bool);
        }

        unless nqp::isnull($method_name) {
            my $method_ast;
            my $bool_value_ast :=
                nqp::objprimspec($bool_value)
                    ?? QAST::Op.new( :op<hllbool>, QAST::IVal.new( :value($bool_value) ) )
                    !! $bool_value.ast;

            if nqp::objprimspec($method_name) {
                # We have method name in clear text form. Now, if we know LHS concrete value and it doesn't have the
                # method then no optimization makes sense as X::Method::NotFound will be thrown anyway.
                return nqp::null() if $lhs.has-value
                                        && $lhs.value-kind == $OPERAND_VALUE_CONST
                                        && !nqp::can($lhs.value-type, $method_name);

                $method_ast := QAST::Op.new(
                    :op<callmethod>,
                    :name($method_name),
                    QAST::Var.new( :name('$_'), :scope<lexical> )
                );
            }
            else {
                # When we can't infer method name directly (i.e. it is most likely computed at run-time), we still can
                # help the compiler by reducing the smartmatch code by one method call and one Pair allocation on when
                # the topic can the requested method. Failure route becomes somewhat more expensive then, but  after
                # all, exceptions are expensive anyway and throwing one is barely a hot path.
                $method_ast := QAST::Op.new(
                    :op<callmethod>,
                    QAST::Var.new( :name('$_'), :scope<lexical> ),
                    QAST::Op.new(
                        :op<unbox_s>,
                        QAST::Op.new( :op<callmethod>, :name<Stringy>, $method_name.localized )
                    )
                );

                # If topic can the method then we invoke it. Otherwise fall back to ACCEPTS to throw an exception
                # TODO Code for new-disp
                $method_ast := QAST::Op.new(
                    :op<if>,
                    QAST::Op.new(
                        :op<can>,
                        QAST::Var.new( :name('$_'), :scope<lexical> ),
                        $method_name.localized(:decl)
                    ),
                    $method_ast,
                    QAST::Op.new(
                        :op<callmethod>,
                        :name<ACCEPTS>,
                        QAST::Op.new(
                            :op<callmethod>,
                            :name<new>,
                            $pair_type_ast,
                            $method_name.localized,
                            $bool_value_ast
                        ),
                        QAST::Var.new( :name('$_'), :scope<local> )
                    )
                )
            }

            if nqp::objprimspec($bool_value) {
                my $boolify_method := nqp::xor($bool_value, $negated) ?? "Bool" !! "not";
                $res_ast := QAST::Op.new( :op<callmethod>, :name($boolify_method), $method_ast );
            }
            else {
                $res_ast := QAST::Op.new(
                    :op<hllbool>,
                    QAST::Op.new(
                        :op<iseq_i>,
                        QAST::Op.new( :op<istrue>, $method_ast ),
                        QAST::Op.new( :op<istrue>, $bool_value_ast )
                    )
                );
            }

            # If there is a chance that LHS can result in an Associative then a fallback to the default ACCEPTS+Bool
            # must be provided.
            my $Associative := $!symbols.find_in_setting('Associative');
            if $lhs.can-be($Associative) {
                my $accepts_bool_method := $sm_accepts_ast.ann('smartmatch_negated') ?? 'not' !! 'Bool';
                $res_ast := QAST::Op.new(
                    :op<if>,
                    QAST::Op.new(
                        :op<istype>,
                        QAST::Var.new( :name('$_'), :scope<lexical> ),
                        QAST::WVal.new( :value($Associative) )
                    ),
                    QAST::Op.new(
                        :op<callmethod>,
                        :name($accepts_bool_method),
                        $sm_accepts_ast
                    ),
                    $res_ast
                );
            }
        }

        $res_ast
    }

    method optimize_when($op) {
        return nqp::null() if nqp::getenvhash<RAKUDO_OPTIMIZE_NOSM>;
        note("Optimizing 'when':\n", $op.dump(4)) if $!debug;
        my $subop1;
        my $accepts_op;
        # Ensure integrity of the node we're about to modify.
        #return $op
        unless nqp::iseq_s($op.op, 'if')
                && nqp::istype(($subop1 := $op[0]), QAST::Op)
                && nqp::iseq_s($subop1.op, 'callmethod')
                && nqp::iseq_s($subop1.name, 'Bool')
                && nqp::istype(($accepts_op := $subop1[0]), QAST::Op)
                && (nqp::iseq_s($accepts_op.op, 'callmethod')
                    || nqp::iseq_s($accepts_op.op, 'p6fatalize'))
                && nqp::iseq_s($accepts_op.name, 'ACCEPTS')
        {
            nqp::die("Unexpected structure of 'when' statement QAST node:\n" ~ $op.dump)
        }

        # Make sure we won't get into a deep recursion.
        $op.annotate('when_block', 0);

        # At this pass it is not safe to collect variable statistics because we may replace a whole block with var
        # references. A second pass of visit_op_children will be needed to get all this fixed later.
        $!optimizer.block_var_stack.enter-dry-run;
        $!optimizer.visit_op_children($op);
        $!optimizer.block_var_stack.exit-dry-run;

        note("Post-visit 'when' AST:\n", $op.dump(4)) if $!debug;

        if nqp::iseq_s($accepts_op.op, 'p6fatalize') {
            $accepts_op := $accepts_op[0];
        }

        my $sm_exp := Operand.new($accepts_op[0], $!optimizer, $!symbols, :name('when expression'));
        my $topic  := Operand.new($accepts_op[1], $!optimizer, $!symbols, :name('when topic'));

        my $m_op;
        if nqp::defined($m_op := self.maybe_typematch($topic, $sm_exp, :in-when))
            || nqp::defined($m_op := self.maybe_literal($topic, $sm_exp, :in-when))
        {
            $op[0] := self.respect_junctions($m_op, $op[0]);
        }

        note("FINAL 'when':\n", $op.dump(4)) if $!debug;

        $op
    }

    method is_chain($op) {
        nqp::istype($op, QAST::Op) && ($op.op eq 'chain' || $op.op eq 'chainstatic')
    }

    method optimize_chain($op) {
        return nqp::null()
            if nqp::getenvhash<RAKUDO_OPTIMIZE_NOSM>
                || $op.ann('smartmatch_optimized');
        note("Try optimizing chained smartmatch:\n", $op.dump(2)) if $!debug;

        my $negated := 0;
        return nqp::null()
            unless self.is_chain($op)
                    && ($op.name eq '&infix:<~~>'
                        || ($negated := $op.name eq '&infix:<!~~>'));

        # Don't optimize long chains. If either we're in a chain, or the first child is a chain op then fall back.
        return nqp::null() if $!optimizer.chain_depth > 1 || self.is_chain($op[0]);

        $!optimizer.block_var_stack.enter-dry-run;
        $!optimizer.visit_op_children($op);
        $!optimizer.block_var_stack.exit-dry-run;

        my $lhs := Operand.new($op[0], $!optimizer, $!symbols, :name<LHS>);
        my $rhs := Operand.new($op[1], $!optimizer, $!symbols, :name<RHS>);

        $lhs.localizable($lhs.can-be-junction && !$rhs.is-type-object($!symbols.Junction));

        note("- LHS:\n", $lhs.dump(2), "- RHS:\n", $rhs.dump(2)) if $!debug;

        my $sm_ast;
        my $result := nqp::null();

        if $lhs.is-junction && !$rhs.is-type-object($!symbols.Junction) {
            $result := QAST::Op.new(
                :op<callmethod>,
                :name<BOOLIFY-ACCEPTS>,
                $lhs.orig-ast,
                $rhs.ast,
                QAST::WVal.new( :value(nqp::hllboolfor($negated, 'Raku')) )
            );
        }

        if nqp::isnull($result)
            && (nqp::defined($sm_ast := self.maybe_typematch($lhs, $rhs, :$negated))
                || nqp::defined($sm_ast := self.maybe_literal($lhs, $rhs, :$negated)))
        {
            if $!debug {
                note("Optimization possible, ensure we don't lose a junction");
                note("  - LHS can be a junction? ", $lhs.can-be-junction,
                    ", is junction? ", $lhs.is-junction,
                    ", type: ", $lhs.value-type-name);
                note("  - RHS is a Junction type object? ", $rhs.is-type-object($!symbols.Junction));
                note($lhs.dump(2));
            }
            $result := self.maybe_respect_junctions($lhs, $rhs, $sm_ast, :$negated);
        }

        if nqp::isnull($result) {
#?if !moar
        $result := $op;
#?endif
#?if moar
        $result := QAST::Op.new(
            :op<dispatch>,
            QAST::SVal.new( :value<raku-smartmatch> ),
            $lhs.orig-ast,
            $rhs.orig-ast,
            QAST::IVal.new( :value($negated ?? -1 !! 1) )
        );
#?endif
        }

        $result.annotate('smartmatch_optimized', 1);
        $!optimizer.visit_op($result) if nqp::istype($result, QAST::Op);

        $result
    }

    method locate_smartmatch_ACCEPTS($ast) {
        for @($ast) -> $child {
            next if nqp::istype($child, QAST::Want);
            return $child if nqp::istype($child, QAST::Op) && $child.ann('smartmatch_accepts');
            nqp::die("AST child is not a QAST node! AST:\n" ~ $ast.dump(4)) unless nqp::istype($child, QAST::Node);
            my $found := self.locate_smartmatch_ACCEPTS($child);
            return $found if nqp::defined($found);
        }
        return nqp::null();
    }

    # This method returns nqp::null() to indicate inability to optimize.
    method optimize_topicalized($op) {
        return nqp::null()
            if nqp::getenvhash<RAKUDO_OPTIMIZE_NOSM>
                || $op.ann('smartmatch_optimized');

        note("Try optimizing topicalized smartmatch:\n",
            (nqp::isconcrete($op.node) ?? "    " ~ $op.node[0] ~ " " ~ $op.node ~ " " ~ $op.node[1] ~ "\n" !! ""),
            $op.dump(4)) if $!debug;
        return nqp::null() unless nqp::istype($op, QAST::Op)
                                    && nqp::iseq_s($op.op, 'locallifetime')
                                    && $op.ann('smartmatch');

        $!optimizer.block_var_stack.enter-dry-run;
        $!optimizer.visit_children($op, :first);
        $!optimizer.block_var_stack.exit-dry-run;

        my $sm_accepts := self.locate_smartmatch_ACCEPTS($op[0][2]);

        note("Located the ACCEPTS call in QAST tree? ", nqp::defined($sm_accepts)) if $!debug;

        my $result := nqp::null();

        # After visit_children was invoked we have to return some kind of concrete AST node or it will result in a
        # duplicate call to visit_children.
        if nqp::defined($sm_accepts) {
            my $lhs := Operand.new($op[0][1][1], $!optimizer, $!symbols, :name<LHS>);
#?if !moar
            my $rhs := Operand.new($sm_accepts[0][1], $!optimizer, $!symbols, :name<RHS>);
#?endif
#?if moar
            my $rhs := Operand.new($sm_accepts[2], $!optimizer, $!symbols, :name<RHS>);
#?endif
            my $negated := $sm_accepts.ann('smartmatch_negated');
            my $boolified := $op[0][2].ann('smartmatch_boolified');

            $lhs.localizable($lhs.can-be-junction && !$rhs.is-type-object($!symbols.Junction));

            note("Topicalized smartmatch:\n    TOPIC:\n", $lhs.dump(11), "\n      RHS:\n", $rhs.dump(11)) if $!debug;

            # We don't try literals optimization because they never make it into topicalized form of SM.
            my $sm_op;
            if nqp::defined($sm_op := self.maybe_typematch($lhs, $rhs, :$negated)) {
                $result := self.maybe_respect_junctions($lhs, $rhs, $sm_op);
            }

            note("Post-typematch attempt result is ", $result.HOW.name($result)) if $!debug;

            if nqp::isnull($result) && ($sm_op := self.maybe_pair($lhs, $rhs, $sm_accepts, :$negated)) {
#?if !moar
                # When maybe_pair succeeds it means the RHS is certainly not a Regex. Hence we can remove the check and
                # replace QAST::Stmts wrapper with the binding to sm_result_<n> local.
                # Pull out the binding op and replace stmts; not needed in case of !~~
                $op[0][2] := $op[0][2][0] unless $negated;
#?endif
                $op[0][2][1] := $sm_op;    # Replace the second binding argument with the optimized AST.
                $result := $op; # No further optimizations are possible
            }

#?if !moar
            # If we know RHS type then we can possibly simplify the actual SM op withing `locallifetime` to plain
            # .ACCEPTS(...).Bool unless RHS is or can be a Regex
            if nqp::isnull($result)
                && $boolified
                && !$rhs.can-be($!symbols.Regex)
            {
                # Get binding into sm_result local
                my $bind_ast := $op[0][2][0];
                # Since RHS would only be used by ACCEPTS now, we can get rid of extra bind into sm_rhs local
                $sm_accepts[0] := $rhs.orig-ast;
                # We boolify unconditionally and this is what goes into sm_result. So, replace the second arg of bind
                $bind_ast[1] := QAST::Op.new(
                    :op<callmethod>,
                    :name<Bool>,
                    $sm_accepts
                );
                # Where previously we had stmts with ACCEPTS + Regex check, it will only bee boolified ACCEPTS now
                $op[0][2] := $bind_ast;
                $result := $op;
            }
#?endif
        }

        $result := $op unless nqp::defined($result);
        $result.annotate('smartmatch_optimized', 1);

        $!optimizer.visit_op($result);

        note("FINAL topicalized:\n", $result.dump(4)) if $!debug;

        $result
    }
}

# Drives the optimization process overall.
class Perl6::Optimizer {
    my class BlockVarStack {
        has $!debug;
        has @!block_var_stack;
        has $!top;
        # If set then any request to the actual BlockVarOptimizer on the stack top would be ignored by method 'do'
        has $!dry-run;

        method push() {
            $!top := BlockVarOptimizer.new(:$!debug);
            @!block_var_stack.push($!top);
            $!top
        }

        method pop() {
            my $old-top := @!block_var_stack.pop();
            my $count := +@!block_var_stack;
            $!top := $count ?? @!block_var_stack[+@!block_var_stack - 1] !! nqp::null();
            $old-top
        }

        method top() { $!top }

        method enter-dry-run() {
            $!dry-run := ($!dry-run // 0) + 1;
        }

        method exit-dry-run() {
            nqp::die("Exiting block var stack dry run mode without a previous enter") unless $!dry-run;
            --$!dry-run
        }

        method in-dry-run() { nqp::istrue($!dry-run) }

        method do($method, *@pos, *%named) {
            return nqp::null() if $!dry-run;
            $!top."$method"(|@pos, |%named)
        }

        method elems() { +@!block_var_stack }

        method poison-var-lowering() {
            for @!block_var_stack {
                $_.poison_lowering();
            }
        }
    }

    # Symbols tracking object.
    has $!symbols;

    # Stack of block variable optimizers.
    has $!block_var_stack;

    # Junction optimizer.
    has $!junc_opt;

    # Smartmatch optimizer
    has $!smartmatch_opt;

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

    # Are we allowed to lower the topic ($_) to a local?
    has $!can_lower_topic;

    # Are we in debug mode?
    has $!debug;

    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        # Initialize.
        $!symbols                 := Symbols.new($past);
        $!block_var_stack         := BlockVarStack.new(:$!debug, :!dry-run);
        $!junc_opt                := JunctionOptimizer.new(self, $!symbols);
        $!smartmatch_opt          := SmartmatchOptimizer.new(self, $!symbols);
        $!problems                := Problems.new($!symbols);
        $!chain_depth             := 0;
        $!in_declaration          := 0;
        $!void_context            := 0;
        $!can_lower_topic         := $past.ann('CAN_LOWER_TOPIC');
        $!debug                   := nqp::getenvhash<RAKUDO_OPTIMIZER_DEBUG>;
        my $*DYNAMICALLY_COMPILED := 0;
        my $*OPTIMIZER-SYMBOLS    := $!symbols;
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
        $!block_var_stack.push();

        # we don't want any "blah in sink context" warnings emitted here
        get_last_stmt($block[1]).annotate: 'sink-quietly', 1
            if $block.ann: 'WANTMEPLEASE';

        # Visit children.
        if $block.ann('DYNAMICALLY_COMPILED') {
            my $*DYNAMICALLY_COMPILED := 1;
            self.visit_children($block, :resultchild(nqp::elems($block) - 1),
                :void_default, :block_structure);
        }
        else {
            self.visit_children($block, :resultchild(nqp::elems($block) - 1),
                :void_default, :block_structure);
        }

        # Pop block from block stack and get computed block var info.
        $!symbols.pop_block();
        my $vars_info := $!block_var_stack.pop();

        # If block var stack is in dry run mode no work on the data it provides is possible because all stats are
        # incorrect at the best. Don't worry, there will be second pass done later on this very block.
        return $block if $!block_var_stack.in-dry-run;

        # If this block is UNIT and we're in interactive mode, poison lexical
        # to local lowering, or else we may lose symbols we need to remember.
        if $block =:= $!symbols.UNIT && %!adverbs<interactive> {
            $vars_info.poison_lowering();
        }

        # We might be able to delete some of the magical variables when they
        # are trivially unused, and also simplify takedispatcher.
        if $!level >= 1 {
            $vars_info.delete_unused_magicals($block, $!can_lower_topic);
            $vars_info.delete_unused_autoslurpy();
            $vars_info.simplify_takedispatcher();
        }

        # Do any possible lexical => local lowering.
        if $!level >= 2 {
            $vars_info.lexical_vars_to_locals($block, $!symbols.LoweredAwayLexical, $!can_lower_topic);
        }

        # If the block is immediate, we may be able to inline it. This is possible
        # when we have lowered any lexicals that it declares into locals, and so
        # it can be said that the scope doesn't have any runtime significance.
        my int $flattened := 0;
        my $result := $block;
        if $!level >= 2 && $block.blocktype eq 'immediate' && $block.arity == 0
                && $vars_info.is_inlinable && !$block.has_exit_handler {
            # Scan symbols for any non-lowered ones.
            my $impossible := 0;
            for $block.symtable() {
                my %sym := $_.value;
                if %sym<scope> eq 'lexical' && !%sym<lowered> {
                    $impossible := 1;
                    last;
                }
            }

            # If we dynamically compiled it, then it must not contain any
            # nested blocks. Otherwise, just make sure there's no parameters
            # we can't understand in there.
            if !$impossible {
                for $block[0].list {
                    if nqp::istype($_, QAST::Var) && $_.decl eq 'param' {
                        $impossible := 1;
                        last;
                    }
                    elsif $*DYNAMICALLY_COMPILED {
                        if nqp::istype($_, QAST::Block) && $_.blocktype ne 'raw' {
                            $impossible := 1;
                            last;
                        }
                    }
                }
            }

            # If we have nothing blocking us, do the immediate inlining of it.
            unless $impossible {
                my $outer := $!symbols.top_block;
                $result := self.inline_immediate_block($block, $outer);
                $flattened := 1 unless $result =:= $block;
            }
        }

        # Incorporate this block's info into outer block's info.
        $!block_var_stack.do('incorporate_inner', $vars_info, $flattened)
            if $!block_var_stack.elems;

        $result
    }

    # Gets the last statement if the thing passed as a QAST::Stmts or a
    # QAST::Stmt. Works recursively. Otherwise returns what was passed.
    sub get_last_stmt($op) {
        if nqp::istype($op, QAST::Stmt) || nqp::istype($op, QAST::Stmts) {
            my int $resultchild := $op.resultchild // nqp::elems($op) - 1;
            $resultchild >= 0 ?? get_last_stmt($op[$resultchild]) !! $op
        }
        else {
            $op
        }
    }

    # Range operators we can optimize into loops, and how to do it.
    sub get_bound($node,$extra) {

        # 0
        if nqp::istype($node, QAST::Want) && $node[1] eq 'Ii' {
            my int $value := $node[2].value + $extra;
            if $value > -2147483648 && $value < 2147483647 {
                return [QAST::IVal.new( :value($value) )];
            }
        }

        # my constant \foo = 0
        elsif nqp::istype($node, QAST::WVal) {
            try {
                if nqp::istype($node.value, $!symbols.find_in_setting("Int")) {
                    my int $value := $node.value + $extra;
                    if $value > -2147483648 && $value < 2147483647 {
                        return [QAST::IVal.new( :value($value) )];
                    }
                }
            }
        }

        # my int $foo
        elsif nqp::istype($node, QAST::Var)
          && nqp::objprimspec($node.returns) == nqp::const::BIND_VAL_INT {  # XXX uint?
            return [$extra
              ?? QAST::Op.new( :op<add_i>, :returns($node.returns),
                   $node,
                   QAST::IVal.new( :value($extra) )
                 )
              !! $node
            ]
        }

        # No way we can make this faster
        []
    }

    # A hash for range operators mapping to a block that returns an array
    # with [first, last, step] if it is possible to optimize, or [] if not.
    my %range_bounds := nqp::hash(
        '&infix:<..>', -> $op {
            my @lt := get_bound($op[0],  0);
            my @rt := get_bound($op[1],  0);
            @lt && @rt ?? [@lt[0], @rt[0], 1] !! []
        },
        '&infix:<..^>', -> $op {
            my @lt  := get_bound($op[0],  0);
            my @rt := get_bound($op[1], -1);
            @lt && @rt ?? [@lt[0], @rt[0], 1] !! []
        },
        '&infix:<^..>', -> $op {
            my @lt  := get_bound($op[0],  1);
            my @rt := get_bound($op[1],  0);
            @lt && @rt ?? [@lt[0], @rt[0], 1] !! []
        },
        '&infix:<^..^>', -> $op {
            my @lt  := get_bound($op[0],  1);
            my @rt := get_bound($op[1], -1);
            @lt && @rt ?? [@lt[0], @rt[0], 1] !! []
        },
        '&prefix:<^>', -> $op {
            (my @rt := get_bound($op[0], -1))
              ?? [QAST::IVal.new( :value(0) ), @rt[0], 1]
              !! []
        },
        '&infix:<...>', -> $op {
            my @result;
            if get_bound($op[0], 0) -> @lt {                # left side ok
                if nqp::istype(@lt[0],QAST::IVal) {         # and is an int
                    if get_bound($op[1], 0) -> @rt {        # right side ok
                        if nqp::istype(@rt[0],QAST::IVal) { # and is an int
                            @result.push(@lt[0]);
                            @result.push(@rt[0]);
                            @result.push(@lt[0].value > @rt[0].value ?? -1 !! 1)
                        }
                    }
                }
            }
            elsif nqp::istype($op[0],QAST::Op)
              && $op[0].name eq '&infix:<,>'          # left side is a list
              && nqp::elems(my $list := $op[0]) == 2  # with 2 elements
              && nqp::istype($list[0],QAST::Want) && nqp::istype($list[0][2],QAST::IVal) # 1st = Int
              && nqp::istype($list[1],QAST::Want) && nqp::istype($list[1][2],QAST::IVal) # 2nd = int
              && get_bound($op[1], 0) -> @rt {        # right side is ok
                if nqp::istype(@rt[0],QAST::IVal) {   # and it is an Int
                    @result.push($list[0][2]);
                    @result.push(@rt[0]);
                    @result.push($list[1][2].value - $list[0][2].value);
                }
            }
            @result
        }
    );

    # Poisonous calls.
    my %poison_calls := nqp::hash(
        'EVAL',     NQPMu, '&EVAL',     NQPMu,
        'EVALFILE', NQPMu, '&EVALFILE', NQPMu,
        'callwith', NQPMu, '&callwith', NQPMu,
        'callsame', NQPMu, '&callsame', NQPMu,
        'nextwith', NQPMu, '&nextwith', NQPMu,
        'nextsame', NQPMu, '&nextsame', NQPMu,
        'samewith', NQPMu, '&samewith', NQPMu,
        # This is a hack to compensate for Test.rakumod using unspecified
        # behavior. The EVAL form of it should be deprecated and then
        # removed, at which point this can go away.
        '&throws-like', NQPMu,
        '&repl', NQPMu);

    # Called when we encounter a QAST::Op in the tree. Produces either
    # the op itself or some replacement opcode to put in the tree.
    method visit_op($op) {
        note("method visit_op $!void_context\n" ~ $op.dump) if $!debug;
        # If it's a QAST::Op of type handle, needs some special attention.
        my str $optype := $op.op;
        if $optype eq 'handle' || $optype eq 'handlepayload' {
            return self.visit_handle($op);
        }

        my $visit_children_mode := 'default'; # Can also be 'sm_chain', or 'sm_when'

        my str $opname := $op.name;
        my int $level := $!level;
        my $sm_ast;
        if $optype eq 'locallifetime' {
            note("ENCOUNTERED locallifetime, is smartmatch? ", ($op.ann('smartmatch') ?? "YES" !! "NO")) if $!debug;
            if $level >= 2
                && $op.ann('smartmatch')
                && nqp::defined($sm_ast := $!smartmatch_opt.optimize_topicalized($op))
            {
                return $sm_ast;
            }
            return self.visit_children($op,:first);
        }

        if $level >= 2
            && ($optype eq 'chain' || $optype eq 'chainstatic')
            && ($opname eq '&infix:<~~>' || $opname eq '&infix:<!~~>')
        {
            $visit_children_mode := 'sm_chain';
        }

        my $symbols := $!symbols;
        # If it's a for 1..1000000 { } we can simplify it to a while loop. We
        # check this here, before the tree is transformed by call inline opts.
        if ($optype eq 'p6for' || $optype eq 'p6forstmt') && $op.sunk && nqp::elems($op) == 2 {
            my $reverse := 0;
            my $theop := $op[0];
            if nqp::istype($theop, QAST::Op)
              && $theop.op   eq 'callmethod'
              && $theop.name eq 'reverse' {
                $reverse := 1;
                $theop := $theop[0];
            }
            if nqp::istype($theop, QAST::Stmts) {
                $theop := $theop[0]
            }

            if nqp::istype($theop, QAST::Op)
            && nqp::existskey(%range_bounds, $theop.name)
            && $symbols.is_from_core($theop.name)
            && $op[1].has_ann('code_object') {
                self.optimize_for_range($op, $op[1], $theop, :$reverse);
                self.visit_op_children($op);
                return $op;
            }
        }

        # It could also be that the user explicitly spelled out the for loop
        # with a method call to "map".
        if $optype eq 'callmethod' && $opname eq 'sink'
        && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'callmethod'
        && $op[0].name eq 'map' && nqp::elems($op[0]) == 2
        && $op[0][1].has_ann('code_object')
        && (
               nqp::istype((my $c1 := $op[0][0]), QAST::Op)
            && nqp::existskey(%range_bounds, $c1.name)
            || nqp::istype($op[0][0], QAST::Stmts)
            && nqp::istype(($c1 := $op[0][0][0]), QAST::Op)
            && nqp::existskey(%range_bounds, $c1.name)
        ) && $symbols.is_from_core($c1.name)
        {
            self.optimize_for_range($op, $op[0][1], $c1);
            self.visit_op_children($op);
            return $op;
        }
        elsif $optype eq 'callmethod' && $opname eq 'new' && $!void_context {
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
        if ($optype eq 'p6assign' || $optype eq 'assign_n' || $optype eq 'assign_i')
            && nqp::istype($op[0], QAST::Var)
            && ($op[0].scope eq 'lexical' || $op[0].scope eq 'lexicalref') {
            if nqp::istype($op[1], QAST::Want) {
                # grab the var's symbol from our blocks
                my $varsym := $symbols.find_lexical_symbol($op[0].name);
                my $type := $varsym<type>;

                my $want_type := $op[1][1];
                my $varname := $op[0].name;

                if $want_type eq 'Ii' {
                    if $type =:= (my $numtype := $symbols.find_in_setting("Num"))
                      || nqp::objprimspec($type) == nqp::const::BIND_VAL_NUM {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($numtype), :value($op[1][2].value), :suggestiontype<Real>,
                                :valuetype<Int>,
                                :native(!($type =:= $numtype))
                            );
                    } elsif $type =:= $symbols.find_in_setting("Rat") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($type), :value($op[1][2].value), :suggestiontype<Real>,
                                :valuetype<Int>
                            );
                    } elsif $type =:= $symbols.find_in_setting("Complex") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                :$varname, :vartype($type), :value($op[1][2].value), :suggestiontype<Numeric>,
                                :valuetype<Int>
                            );
                    }
                } elsif $want_type eq 'Nn' {
                    my $value := $op[1][2];
                    if $value.HOW.name($value) eq 'QAST::NVal' {
                        if $type =:= (my $inttype := $symbols.find_in_setting("Int"))
                          || nqp::objprimspec($type) == nqp::const::BIND_VAL_INT {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($inttype), :value($value.value), :suggestiontype<Real>,
                                    :valuetype<Num>,
                                    :native(!($type =:= $inttype))
                                );
                        } elsif $type =:= $symbols.find_in_setting("Rat") {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($type), :value($value.value), :suggestiontype<Real>,
                                    :valuetype<Num>
                                );
                        } elsif $type =:= $symbols.find_in_setting("Complex") {
                            $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[1],
                                    :$varname, :vartype($type), :value($value.value), :suggestiontype<Numeric>,
                                    :valuetype<Num>
                                );
                        }
                    }
                }
            }
            elsif nqp::istype($op[1], QAST::WVal) {
                my $varsym := $symbols.find_lexical_symbol($op[0].name);
                my $type := $varsym<type>;

                my $value := $op[1].value;
                my $val_type := $value.HOW.name($value);
                my $varname := $op[0].name;
                if $val_type eq 'Rat' || $val_type eq 'RatStr' {
                    if $type =:= (my $inttype := $symbols.find_in_setting("Int"))
                          || nqp::objprimspec($type) == nqp::const::BIND_VAL_INT {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($inttype), :value($value), :suggestiontype<Real>,
                                :valuetype<Rat>,
                                :native(!($type =:= $inttype))
                            );
                    } elsif $type =:= (my $numtype := $symbols.find_in_setting("Num"))
                        || nqp::objprimspec($type) == nqp::const::BIND_VAL_NUM {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($numtype), :value($value), :suggestiontype<Real>,
                                :valuetype<Rat>,
                                :native(!($type =:= $numtype))
                            );
                    } elsif $type =:= $symbols.find_in_setting("Complex") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value($value), :suggestiontype<Numeric>,
                                :valuetype<Rat>
                            );
                    }
                }
                elsif $val_type eq 'Complex' || $val_type eq 'ComplexStr' {
                    if $type =:= (my $inttype := $symbols.find_in_setting("Int"))
                      || nqp::objprimspec($type) == nqp::const::BIND_VAL_INT {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($inttype), :value($value), :suggestiontype<Numeric>,
                                :valuetype<Complex>,
                                :native(!($type =:= $inttype))
                            );
                    } elsif $type =:= (my $numtype := $symbols.find_in_setting("Num"))
                        || nqp::objprimspec($type) == nqp::const::BIND_VAL_NUM {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($numtype), :value($value), :suggestiontype<Numeric>,
                                :valuetype<Complex>,
                                :native(!($type =:= $numtype))
                            );
                    } elsif $type =:= $symbols.find_in_setting("Rat") {
                        $!problems.add_exception(['X', 'Syntax', 'Number', 'LiteralType'], $op[0],
                                :$varname, :vartype($type), :value($value), :suggestiontype<Numeric>,
                                :valuetype<Complex>
                            );
                    }
                }
            }
        }


        if $optype eq 'chain' {
            $!chain_depth := $!chain_depth + 1;

            # A chain with exactly two children can become the op itself.
            $optype := 'call' if $!chain_depth == 1 &&
                !(nqp::istype($op[0], QAST::Op) && $op[0].op eq 'chain') &&
                !(nqp::istype($op[1], QAST::Op) && $op[1].op eq 'chain');
        }

        if $level >= 2 && $optype eq 'if' && $op.ann('when_block') {
            $visit_children_mode := 'sm_when';
        }

        # We may be able to unfold a junction at compile time.
        if $level >= 2 {
            my $jo_result := $!junc_opt.optimize($op);
            if $jo_result {
                return $jo_result;
            }
        }

        # Visit the children.
        if ($visit_children_mode eq 'sm_chain'
            && nqp::defined(my $sm_op := $!smartmatch_opt.optimize_chain($op)))
        {
            # Smartmatch in a chain is only optimizable if chain depth is 1.
            $!chain_depth := 0;
            return $sm_op;
        }
        $!smartmatch_opt.optimize_when($op) if $visit_children_mode eq 'sm_when';
        self.visit_op_children($op);

        # Some ops are significant for variable analysis/lowering.
        if $optype eq 'bind' {
            if nqp::istype($op[1], QAST::Op) && $op[1].op eq 'getlexouter' {
                $!block_var_stack.do('register_getlexouter_usage', $op);
            }
            elsif nqp::istype($op[0], QAST::Var) && $op[0].name eq '%_' {
                $!block_var_stack.do('register_autoslurpy_bind', $op);
            }
            elsif $op.ann('autoslurpy') {
                $!block_var_stack.do('register_autoslurpy_setup', $op);
            }
        }
        elsif $optype eq 'p6bindsig' || $optype eq 'p6bindcaptosig' {
            $!block_var_stack.do('uses_bindsig');
        }
        elsif $optype eq 'p6return' {
            $!block_var_stack.do('uses_p6return');
        }
        elsif $optype eq 'call' || $optype eq 'callmethod' || $optype eq 'chain' {
            $!block_var_stack.do('register_call');
            if nqp::existskey(%poison_calls, $opname) {
                $!block_var_stack.poison-var-lowering();
            }
        }

        # May be able to eliminate some decontrv operations.
        if $optype eq 'p6decontrv' || $optype eq 'p6decontrv_6c' {
            # If it's rw, don't need to decont at all.
            my $value := $op[1];
            return $value if $op[0].value.rw;

            # Boolifications don't need it, nor do _I/_i/_n/_s/_u ops, with
            # the exception of native assignment, which can decont_[insu]
            # as appropriate, which may avoid a boxing. Same for QAST::WVal
            # if we can see the value is not containerized.
            my $last_stmt := get_last_stmt($value);
            if nqp::istype($last_stmt, QAST::Want) {
                $last_stmt := $last_stmt[0];
            }
            if nqp::istype($last_stmt, QAST::Op) {
                my str $last_op := $last_stmt.op;
                if $last_op eq 'hllbool' || nqp::eqat($last_op, 'I', -1) {
                    return $value;
                }
                if nqp::eqat($last_op, 'assign_', 0) {
                    if    $last_op eq 'assign_i' { $op.op('decont_i') }
                    elsif $last_op eq 'assign_n' { $op.op('decont_n') }
                    elsif $last_op eq 'assign_s' { $op.op('decont_s') }
                    elsif $last_op eq 'assign_u' { $op.op('decont_u') }
                    $op.shift; # The QAST::WVal of the routine
                    return $op;
                }
                if nqp::eqat($last_op, '_i', -2) ||
                      nqp::eqat($last_op, '_u', -2) ||
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
                if $scope eq 'lexical' && $last_stmt.name eq 'self' {
                    return $value; # The invocant is always decontainerized
                }
            }
            elsif nqp::istype($last_stmt, QAST::WVal) {
                return $value unless nqp::iscont($last_stmt.value);
            }
        }

        # Also some return type checks.
        elsif $optype eq 'p6typecheckrv' {
            my $optres := self.optimize_p6typecheckrv($op);
            return $optres if $optres;
        }

        # Some ops have first boolean arg, and we may be able to get rid of
        # a hllbool if there's already an integer result behind it. For if/unless,
        # we can only do that when we have the `else` branch, since otherwise we
        # might return the no-longer-Bool value from the conditional.
        elsif ((nqp::elems($op) == 3 || $!void_context) && ($optype eq 'if' || $optype eq 'unless'))
        || $optype eq 'while' || $optype eq 'until' {
            my $update := $op;
            my $target := $op[0];
            while (nqp::istype($target, QAST::Stmt) || nqp::istype($target, QAST::Stmts)) && nqp::elems($target) == 1 {
                $update := $target;
                $target := $target[0];
            }
            if nqp::istype($target, QAST::Op) && $target.op eq 'hllbool' {
                if nqp::objprimspec($target[0].returns) == nqp::const::BIND_VAL_INT {
                    $update[0] := $target[0];
                }
            }
            elsif nqp::istype($target,QAST::Var) && ($target.scope eq 'lexicalref' || $target.scope eq 'attributeref' || $target.scope eq "localref") && nqp::objprimspec($target.returns) == nqp::const::BIND_VAL_INT {
                # turn $i into $i != 0
                $target.scope($target.scope eq 'lexicalref' ?? 'lexical' !! $target.scope eq 'attributeref' ?? 'attribute' !! 'local');
                $update[0] := QAST::Op.new( :op('isne_i'), :returns($target.returns), $target, QAST::IVal.new( :value(0) ));
            }
        }

        # May be able to simplify takedispatcher ops.
        elsif $optype eq 'takedispatcher' {
            $!block_var_stack.do('register_takedispatcher', $op);
        }

        # Make array variable initialization cheaper
        elsif
            $level > 0
            && $optype eq 'callmethod'
            && $opname eq 'STORE'
            && nqp::elems($op.list()) == 3
            && nqp::istype($op[0], QAST::Var)
            && nqp::substr($op[0].name, 0, 1) eq '@'
            && nqp::istype($op[1], QAST::Op)
            && ($op[1].op eq 'call' || $op[1].op eq 'callstatic')
            && $op[1].name eq '&infix:<,>'
            && $symbols.is_from_core($op[1].name)
            && nqp::istype($op[2], QAST::WVal)
            && nqp::istype($op[2], QAST::SpecialArg)
        {
            return self.optimize_array_variable_initialization($op);
        }

        # Turn `@a[1, 3]` into `@a.AT-POS(1), @a.AT-POS(3)`
        elsif
            $level > 0
            && ($optype eq 'call' || $optype eq 'callstatic')

            # workaround because op_eq_core needs to be "primed"
            && ((try $symbols.find_lexical($opname)) || 1)

            && self.op_eq_core($op, '&postcircumfix:<[ ]>')

            # if it's 3 that means the slice is on the LHS of an assignment
            && nqp::elems($op) == 2

            && nqp::istype($op[0], QAST::Var)
            && nqp::substr($op[0].name, 0, 1) eq '@'
            && nqp::istype($op[1], QAST::WVal)
            && nqp::istype($op[1].value, $symbols.find_in_setting("List"))
        {
            return self.optimize_array_slice($op);
        }

        # Calls are especially interesting as we may wish to do some
        # kind of inlining.
        elsif $optype eq 'call' {
            my $opt_result := $opname eq ''
                ?? self.optimize_nameless_call($op)
                !! self.optimize_call($op);
            if $opt_result {
                 $!chain_depth := 0
                    if $optype eq 'chain' && $!chain_depth == 1;
                 return $opt_result;
            }
        }
        # Some .dispatch:<....> calls can be simplified
        elsif $level >= 2 && $optype eq 'callmethod'
        && nqp::eqat($opname, 'dispatch:<', 0) {
            if $opname eq 'dispatch:<!>' {
                # If it's a private method call, we can sometimes resolve
                # it at compile time. If so, we can reduce it to a
                # sub call in some cases.
                self.optimize_private_method_call: $op;
            }
            elsif $opname eq 'dispatch:<.=>' {
                # .= calls can be unpacked entirely
                return self.optimize_dot_equals_method_call: $op;
            }
            elsif $opname eq 'dispatch:<::>' {
                return self.optimize_qual_method_call: $op;
            }
            elsif $opname eq 'dispatch:<.?>' {
                return self.optimize_maybe_method_call: $op;
            }
        }

        if $optype eq 'chain' {
            $!chain_depth := $!chain_depth - 1;

            # See if we can staticalize this op
            my $obj;
            my $found;
            try {
                $obj   := $symbols.find_lexical($opname);
                $found := 1;
            }
            if $found {
                if nqp::can($obj, 'is-pure') {
                    $op.op: 'chainstatic'
                }
                else {
                    my $scopes := $symbols.scopes_in: $opname;
                    $op.op: 'chainstatic'
                      if $scopes <= 1 && nqp::can($obj, 'soft') && ! $obj.soft;
                }
            }
        }
        $op
    }

    method optimize_dot_equals_method_call($call) {
        my $target := $call[0];
        my $qast;
        $call.name: ''; # second kid already is the method name the op will use

        if nqp::istype($target, QAST::Var) {
            # we have a plain variable as target. Safe to Just Use It™
            $qast := QAST::Op.new: :op<p6store>, $target, $call
        }
        else {
            # we have something more complex as target. Use a temp var to
            # save the result of it into and then to call method on it
            $target := $call.shift;
            my $name := QAST::Node.unique: 'make_dot_equals_temp_';
            $call.unshift: QAST::Var.new: :$name, :scope<local>;
            $qast := QAST::Stmts.new:
              QAST::Op.new(:op<bind>,
                QAST::Var.new(:$name, :scope<local>, :decl<var>),
                $target),
              QAST::Op.new: :op<p6store>,
                QAST::Var.new(:$name, :scope<local>),
                $call
        }

        $qast
    }

    method visit_op_children($op) {
        my int $orig_void := $!void_context;
        $!void_context    := $op.op eq 'callmethod' && $op.name eq 'sink';
        self.visit_children($op);
        $!void_context := $orig_void;
    }

    method optimize_p6typecheckrv($op) {
        my $tr := $!symbols.top_routine;
        if nqp::can($tr, "returns") {
            my $rettype        := $tr.returns;
            my int $rettype_ps := nqp::objprimspec($rettype);
            if $rettype =:= $!symbols.Mu {
                return $op[0];
            }
            elsif $rettype_ps && $rettype_ps == nqp::objprimspec($op[0].returns) {
                return $op[0];
            }
        }
    }

    method optimize_array_slice($op) {
        unless nqp::isconcrete($op[1].value) {
            return $op
        }

        my $reified := nqp::getattr($op[1].value, $!symbols.find_in_setting("List"), '$!reified');

        unless $reified.HOW.name($reified) eq 'BOOTArray' {
            return $op
        }

        my $new_op    := QAST::Op.new(:op('callstatic'), :name('&infix:<,>'));
        my $array_var := $op[0];

        for $reified -> $var {
            if nqp::istype($var,$!symbols.find_in_setting("Int")) {
                $new_op.push: QAST::Op.new(:op('callmethod'),
                  :name('AT-POS'),
                  $array_var,
                  QAST::WVal.new(:value($var))
                );
            }
            else {
                return $op;  # alas, too complex for now
            }
        }

        $new_op;
    }

    method optimize_array_variable_initialization($op) {
        my $Positional := $!symbols.find_in_setting('Positional');
        if $op[0].returns =:= $Positional {
            # Turns the @var.STORE(infix:<,>(...)) into:
            # nqp::getattr(
            #     nqp::p6bindattrinvres(
            #         nqp::p6bindattrinvres(
            #             @var,
            #             List,
            #             $!reified,
            #             nqp::create(IterationBuffer),
            #         ),
            #         List,
            #         $!todo,
            #         nqp::p6bindattrinvres(
            #             nqp::p6bindattrinvres(
            #                 nqp::p6bindattrinvres(
            #                     $reifier,
            #                     List::Reifier,
            #                     '$!reified',
            #                     nqp::getattr(@var, List, $!reified),
            #                 ),
            #                 List::Reifier,
            #                 '$!reification-target',
            #                 @var.reification-target()
            #             ),
            #             List::Reifier,
            #             '$!future',
            #             nqp::list(...),
            #         ),
            #     ),
            #     List,
            #     '$!todo'
            # ).reify-until-lazy();

            $op.op('callmethod');
            $op.name('reify-until-lazy');

            my $array_var := $op[0];
            my $comma_op  := $op[1];
            $comma_op.op('getattr');
            $comma_op.name(NQPMu);

            my $List            := $!symbols.find_in_setting('List');
            my $Reifier         := $!symbols.find_in_setting('List').WHO<Reifier>;
            my $IterationBuffer := $!symbols.find_in_setting('IterationBuffer');

            my $list := QAST::Op.new(:op<list>);
            $list.set_children(@($comma_op));
            $comma_op.set_children([
                QAST::Op.new(
                    :op<p6bindattrinvres>,
                    QAST::Op.new(
                        :op<p6bindattrinvres>,
                        $array_var,
                        QAST::WVal.new(:value($List)),
                        QAST::SVal.new(:value('$!reified')),
                        QAST::Op.new(:op<create>, QAST::WVal.new(:value($IterationBuffer))),
                    ),
                    QAST::WVal.new(:value($List)),
                    QAST::SVal.new(:value('$!todo')),
                    QAST::Op.new(
                        :op<p6bindattrinvres>,
                        QAST::Op.new(
                            :op<p6bindattrinvres>,
                            QAST::Op.new(
                                :op<p6bindattrinvres>,
                                QAST::Op.new(:op<create>, QAST::WVal.new(:value($Reifier))),
                                QAST::WVal.new(:value($Reifier)),
                                QAST::SVal.new(:value('$!reified')),
                                QAST::Op.new(
                                    :op<getattr>,
                                    $array_var,
                                    QAST::WVal.new(:value($List)),
                                    QAST::SVal.new(:value('$!reified')),
                                )
                            ),
                            QAST::WVal.new(:value($Reifier)),
                            QAST::SVal.new(:value('$!reification-target')),
                            QAST::Op.new(
                                :op<callmethod>,
                                :name('reification-target'),
                                $array_var,
                            )
                        ),
                        QAST::WVal.new(:value($Reifier)),
                        QAST::SVal.new(:value('$!future')),
                        $list,
                    ),
                ),
                QAST::WVal.new(:value($List)),
                QAST::SVal.new(:value('$!todo')),
            ]);
            $op.set_children([$comma_op]);

            return QAST::Stmts.new: $op, $array_var;
        }
        $op
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
            if nqp::can($obj, 'is-pure') {
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
                        try self.visit_children($op, :void_default($!void_context));
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
                    unless $op_txt eq '/' && $op[1].has_compile_time_value && $op[1].compile_time_value == 0 {
                        my str $expr   := nqp::escape(widen($op.node));
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
                if $all_args_known && self.op_eq_core($op, '&infix:<x>') {
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
                        my $*FOLDING := 1;
                        $ret_value := $obj(|@args);
                        $survived  := 1 ;
                        CONTROL {
                            $survived := 0;
                        }
                    }
                    if $survived && self.constant_foldable_type($ret_value) {
                        return $NULL if $!void_context && !$!in_declaration;
                        $*W.add_object_if_no_sc($ret_value);
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
                            $!block_var_stack.do('unregister_call');
                            return $want;
                        }
                        $!block_var_stack.do('unregister_call');
                        return $wval;
                    } elsif $survived && nqp::istype($ret_value, $!symbols.Failure) {
                        # Disarm the failure so it doesn't output its warning during GC.
                        $ret_value.Bool();
                    }
                }
            }
            elsif $!level >= 2 && (
                 $op.name eq '&prefix:<++>' || $op.name eq '&postfix:<++>'
              || $op.name eq '&prefix:<-->' || $op.name eq '&postfix:<-->'
            ) && $!symbols.is_from_core($op.name)
            && self.optimize-post-pre-inc-dec-ops($op) -> $qast {
                return $qast;
            }

            # Re-write `$foo ** 2` into `$foo * $foo`. Literals will be constant folded and
            # we can't do it for non-VARs, since side effects could be duplicated (e.g.,
            # `my $a = 3; say ($a += 3) ** 2; say $a` would give `89␤9` instead of `36␤6`).
            if $!level >= 2 && ($op.name eq '&infix:<**>' || $op.name eq '&postfix:<ⁿ>') && $!symbols.is_from_core($op.name) &&
              nqp::istype($op[0], QAST::Var) && $op[1].has_compile_time_value &&
              nqp::istype((my $p := $op[1].compile_time_value), $!symbols.find_in_setting("Int")) && $p == 2 {
                return QAST::Op.new(:op($op.op), :name('&infix:<*>'), $op[0], $op[0]);
            }

            # If it's an onlystar proto, we have a couple of options.
            # The first is that we may be able to work out what to
            # call at compile time. Failing that, we can at least inline
            # the proto.
            my $dispatcher := nqp::can($obj, "is_dispatcher") && $obj.is_dispatcher;
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
                        self.report_inevitable_dispatch_failure(
                            $op, @types, @flags, $obj,
                            :protoguilt($ct_result_proto == -1)
                        ) unless $*NO-COMPILE-TIME-THROWAGE;
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
                        self.report_inevitable_dispatch_failure(
                            $op, @types, @flags, $obj
                        ) unless $*NO-COMPILE-TIME-THROWAGE;
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

    method optimize-post-pre-inc-dec-ops($op) {
        my $var  := $op[0];
        my $node := $op.node;
        $op.op: 'callstatic'; # by now we know 'tis a core op

        # if we got a native int/num, we can rewrite into nqp ops
        if nqp::istype($var,QAST::Var) && ($var.scope eq 'lexicalref' || $var.scope eq 'attributeref')
        && ((my $primspec := nqp::objprimspec($var.returns)) == nqp::const::BIND_VAL_INT
          || $primspec == nqp::const::BIND_VAL_NUM || $primspec == 4 || $primspec == 5) # native num or "emulated" 64bit int
        {
            my $returns := $var.returns;
            my $is-dec := nqp::eqat($op.name, '--', -3);

            if $primspec == nqp::const::BIND_VAL_INT {
                $!block_var_stack.do('unregister_call');
                my $one := QAST::IVal.new: :value(1);
                if $!void_context || nqp::eqat($op.name, '&pre', 0) {
                    # we can just use (or ignore) the result
                    return QAST::Op.new: :op<assign_i>, :$node, :$returns, $var,
                      QAST::Op.new: :op($is-dec ?? 'sub_i' !! 'add_i'),
                        :$returns, $var, $one
                }
                else {
                    # need to assign original value; it's cheaper to just
                    # do the reverse operation than to use a temp var
                    return QAST::Op.new: :op($is-dec ?? 'add_i' !! 'sub_i'),
                          :$node, :$returns,
                        QAST::Op.new(:op<assign_i>, :$returns, $var,
                          QAST::Op.new: :op($is-dec ?? 'sub_i' !! 'add_i'),
                           :$returns, $var, $one),
                        $one
                }
            }
            elsif $primspec == nqp::const::BIND_VAL_NUM {
                $!block_var_stack.do('unregister_call');
                my $one := QAST::NVal.new: :value(1.0);
                if $!void_context || nqp::eqat($op.name, '&pre', 0) {
                    # we can just use (or ignore) the result
                    return QAST::Op.new: :op<assign_n>, :$node, :$returns, $var,
                      QAST::Op.new: :op($is-dec ?? 'sub_n' !! 'add_n'),
                        :$returns, $var, $one
                }
                else {
                    # need to assign original value; it's cheaper to just
                    # do the reverse operation than to use a temp var
                    return QAST::Op.new: :op($is-dec ?? 'add_n' !! 'sub_n'),
                        :$node, :$returns,
                      QAST::Op.new(:op<assign_n>, :$returns, $var,
                        QAST::Op.new: :op($is-dec ?? 'sub_n' !! 'add_n'),
                          :$returns, $var, $one),
                      $one
                }
            }
            elsif $primspec == 4 || $primspec == 5 { # 64bit int on 32bit backends
                $!block_var_stack.do('unregister_call');
                my str $assign_op := $primspec == 4 ?? 'assign_i64' !! 'assign_u64';
                my $one := QAST::IVal.new: :value(1);
                if $!void_context || nqp::eqat($op.name, '&pre', 0) {
                    # we can just use (or ignore) the result
                    return QAST::Op.new: :op($assign_op), :$node, :$returns, $var,
                      QAST::Op.new: :op($is-dec ?? 'sub_i64' !! 'add_i64'),
                        :$returns, $var, $one
                }
                else {
                    # need to assign original value; it's cheaper to just
                    # do the reverse operation than to use a temp var
                    return QAST::Op.new: :op($is-dec ?? 'add_i64' !! 'sub_i64'),
                          :$node, :$returns,
                        QAST::Op.new(:op($assign_op), :$returns, $var,
                          QAST::Op.new: :op($is-dec ?? 'sub_i64' !! 'add_i64'),
                           :$returns, $var, $one),
                        $one
                }
            }
        }

        # XXX TODO: my tests show the opt below makes things 25% slower.
        # Even without the temp var business, and unoptimized version:
        # my $i = 1; my $z; { for ^10000_000 { $z = 1 + ++$i }; say now - ENTER now }
        # comes out slower than
        # my $i = 1; my $z; { for ^10000_000 { $z = 1 + $i++ }; say now - ENTER now }
        #
        # Don't know what's going on. Leaving the code below commented out
        # Filed as https://github.com/rakudo/rakudo/issues/1491

        # elsif nqp::eqat($op.name, 'post', 1) { # try to optimize postfix ops
            # just rewrite to prefix-assign
            # $op.name: '&prefix:<' ~ nqp::substr($op.name, -3);

            # if original value isn't needed, we're done here
            # return $op if $!void_context;

            # use a local temp var to store original value
            # my $temp := QAST::Var.new: :$node, :scope<local>,
            #   :name(QAST::Node.unique: 'post_pre_op_rewrite_');
            #
            # return QAST::Stmts.new: :$node,
            #   QAST::Op.new(:$node, :op<bind>, $temp.decl_as('var'),
            #     QAST::Op.new: :op<decont>, $var),
            #   $op, $temp
        # }

        NQPMu
    }

    method constant_foldable_type($value) {
        !(
            nqp::istype($value, $!symbols.Seq) ||
            nqp::istype($value, $!symbols.Failure)
        )
    }

    # The _i64 and _u64 are only used on backends that emulate int64/uint64
    my @native_assign_ops := ['', 'assign_i', 'assign_n', 'assign_s', 'assign_i64', 'assign_u64', '', '', '', '', 'assign_u'];

    method optimize_nameless_call($op) {
      return NQPMu
        unless nqp::elems($op)
        && nqp::istype((my $metaop := $op[0]), QAST::Op)
        && ($metaop.op eq 'call' || $metaop.op eq 'callstatic');

      # if we know we're directly calling the result, we can be smarter
      # about METAOPs
      if self.op_eq_core($metaop, '&METAOP_ASSIGN') {
        if nqp::istype($metaop[0], QAST::Var) # lexical sub
        || (nqp::istype($metaop[0], QAST::Op) # or a REVERSE metaop with lex sub
          && self.op_eq_core($metaop[0], '&METAOP_REVERSE') # and nothing else
          && nqp::istype($metaop[0][0], QAST::Var)
          && nqp::elems($metaop[0]) == 1
          && (my $is-reverse := 1)) {
          if $is-reverse
          || (nqp::istype($op[1], QAST::Var) && (my int $is_var := 1))
          || $op[1].has_ann('METAOP_opt_result') # previously-optimized nested METAOP
          # instead of $foo += 1 we may have $foo.bar += 1, which
          # we really want to unpack here as well. this second branch
          # of the if statement achieves this.
          || (nqp::istype($op[1], QAST::Op)
            && ($op[1].op eq 'callmethod'
              || ($op[1].op eq 'hllize' && nqp::istype($op[1][0], QAST::Op)
                && $op[1][0].op eq 'callmethod')
              || $op[1].op eq 'call' || $op[1].op eq 'callstatic')
          ) {
            my str $assignop;
            my $assignee;
            my $assignee_var;
            my int $is-always-definite;

            if $is_var {
              my str $sigil := nqp::substr($op[1].name, 0, 1);
              if nqp::objprimspec($op[1].returns) -> $spec {
                $assignop := @native_assign_ops[$spec];
                $is-always-definite := 1;
              }
              elsif $sigil eq '$' {
                $assignop := 'p6assign';
              }
              elsif $sigil eq '@' || $sigil eq '%' {
                $assignop := 'p6store';
              }
              else {
                return NQPMu;
              }
              $assignee := $op[1];
              # other optimizations might want to change this independently
              $assignee_var := nqp::clone($op[1]);
            }
            else {
              $assignop := 'p6store';
              # We want to be careful to only call $foo.bar once, if that's what
              # we have, so we bind to a local var and assign to that. The
              # var is also needed when we're unpacking a REVERSE op, since
              # we'd still need to assign into the variable on LHS w/o REVERSE
              my $lhs_ast := $op[1];
              my $target_name := QAST::Node.unique: 'METAOP_assign_';
              $assignee     :=
              QAST::Op.new: :op<bind>,
                QAST::Var.new(:name($target_name), :scope<local>, :decl<var>),
                $lhs_ast;
              $assignee_var :=
              QAST::Var.new: :name($target_name),:scope<local>;
            }

            # since the optimizer will only ever walk the first branch of a Want
            # node, we have to make sure to change the node in place, since it's
            # most likely shared with the other branch.
            $op.op: $assignop;
            my $operand := $op[2];
            $op.pop; $op.pop; $op.pop;
            $op.push: $assignee;

            if $is-always-definite {
              $op.push: QAST::Op.new: :op<call>, :name($metaop[0].name),
                          $assignee_var, $operand;
            }
            elsif $is-reverse {
              # We end up with two calls of the op if var is not definite.
              # This is by design:
              # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2018-01-12#l208
              $op.push:
              QAST::Op.new: :op<call>, :name($metaop[0][0].name),
                $operand,
                QAST::Op.new(:op<if>,
                  QAST::Op.new(:op<p6definite>, $assignee_var),
                  $assignee_var,
                  QAST::Op.new(:op<call>, :name($metaop[0][0].name)));
            }
            else {
              # We end up with two calls of the op if var is not definite.
              # This is by design:
              # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2018-01-12#l208
              $op.push:
              QAST::Op.new: :op<call>, :name($metaop[0].name),
                QAST::Op.new(:op<if>,
                  QAST::Op.new(:op<p6definite>, $assignee_var),
                  $assignee_var,
                  QAST::Op.new(:op<call>, :name($metaop[0].name))),
                $operand;
            }

            $op.annotate_self: 'METAOP_opt_result', 1;
            $op.returns: $assignee.returns
                if $assignop ne 'p6assign'
                && nqp::objprimspec($assignee.returns);

            my $*NO-COMPILE-TIME-THROWAGE := 1;
            $!block_var_stack.do('unregister_call');
            return self.visit_op: $op;
          }
        }
      }
      elsif self.op_eq_core($metaop, '&METAOP_NEGATE') {
        return NQPMu unless nqp::istype($metaop[0], QAST::Var);

        my %lookup := nqp::hash(
          '&infix:<==>', 1,
          '&infix:<eq>', 1,
          # ('ne' ne 'troublesome') eq False
          #'&infix:<ne>', 1,
          '&infix:<===>', 1,
          '&infix:<eqv>', 1,
        );

        return nqp::not_i(nqp::isnull($metaop[0]))
            && nqp::not_i(nqp::isnull(my $raku-comp := nqp::getcomp('Raku')))
            && nqp::not_i(nqp::isnull(my $revision := nqp::findmethod($raku-comp, 'language_revision')))
            && nqp::islt_i(2, $revision($raku-comp))
            && nqp::not_i(nqp::isnull(my $op-name := $metaop[0].name))
            && nqp::existskey(%lookup, $op-name)
          ??
            QAST::Op.new(:op<call>, :name($metaop[0].name),
              QAST::WVal.new(value => $!symbols.find_in_setting("False")),
              QAST::Op.new(:op<call>, :name($metaop[0].name), $op[1], $op[2]))
          !!
            QAST::Op.new(:op<call>, :name('&prefix:<!>'),
              QAST::Op.new(:op<call>, :name($metaop[0].name), $op[1], $op[2]));
      }
      elsif self.op_eq_core($metaop, '&METAOP_REVERSE') {
        return NQPMu unless nqp::istype($metaop[0], QAST::Var)
          && nqp::elems($op) == 3;
        my $opt_result := QAST::Op.new(:op<call>, :name($metaop[0].name),
          $op[2], $op[1]).annotate_self: 'METAOP_opt_result', 1;
        if $op.named { $opt_result.named($op.named) }
        if $op.flat { $opt_result.flat($op.flat) }
        return self.visit_op: $opt_result;
      }
      NQPMu
    }

    method op_eq_core($op, $name) {
        $op.name eq $name && $!symbols.is_from_core: $name
    }

    method optimize_private_method_call($op) {
        # We can only optimize if we have a compile-time-known name.
        my $name_node := $op[1];
        if nqp::istype($name_node, QAST::Want) && $name_node[1] eq 'Ss' {
            $name_node := $name_node[2];
        }
        unless (nqp::istype($name_node, QAST::SVal)) {
            return 0;
        }

        # For private method calls within a class, we can try to resolve them
        # at this point. If they are missing, that's an error. In a role, this
        # optimization won't help.
        my $pkg_node := $op[2];
        if $pkg_node.has_compile_time_value {
            my str $name := $name_node.value; # get raw string name
            my $pkg := $pkg_node.returns;     # actions sets this unless in role
            if nqp::can($pkg.HOW, 'find_private_method') {
                my $meth := $pkg.HOW.find_private_method($pkg, $name);
                if nqp::defined($meth) && $meth {
                    if nqp::isnull(nqp::getobjsc($meth)) {
                        try $*W.add_object_if_no_sc($meth);
                    }
                    unless nqp::isnull(nqp::getobjsc($meth)) {
                        my $call := QAST::WVal.new( :value($meth) );
                        my $inv  := $op.shift;
                        $op.shift; $op.shift; # name, package (both pre-resolved now)
                        $op.unshift($inv);
                        $op.unshift($call);
                        $op.op('call');
                        $op.name(NQPMu);
                        return 1;
                    }
                }
                else {
                    $!problems.add_exception(['X', 'Method', 'NotFound'], $op,
                        :private(nqp::hllboolfor(1, "Raku")), :method($name),
                        :typename($pkg.HOW.name($pkg)), :invocant($pkg),
                        :in-class-call(nqp::hllboolfor(1, "Raku")));
                    return 1;
                }
            }
        }

#?if moar
        # If resolution didn't work out this way, and we're on the MoarVM
        # backend, use a dispatcher to speed it up. Give it the package and
        # name ahead of the invocant, as this makes the calling far more
        # optimal.

        my $inv := $op.shift;
        my $name := $op.shift;
        my $pkg := $op.shift;
        $op.unshift($inv);
        $op.unshift($name_node);
        $op.unshift($pkg);
        $op.unshift(QAST::SVal.new( :value('raku-meth-private') ));
        $!block_var_stack.do('unregister_call');
        $op.op('dispatch');
        $op.name(NQPMu);
        return 1;
#?endif
    }

    method optimize_qual_method_call($op) {
        # Dispatch only available on MoarVM for now.
#?if !moar
        $op;
#?endif
#?if moar
        # We can only optimize if we have a compile-time-known name.
        my $name_node := $op[1];
        if nqp::istype($name_node, QAST::Want) && $name_node[1] eq 'Ss' {
            $name_node := $name_node[2];
        }
        return $op unless nqp::istype($name_node, QAST::SVal);

        # We need to evaluate the invocant only once, but pass it both as is
        # and decontainerized, so will bind it into a temporary.
        my $inv := $op.shift;
        my $name := $op.shift;
        my $type := $op.shift;
        my @args;
        while $op.list {
            nqp::push(@args, $op.shift);
        }
        my $temp := QAST::Node.unique('inv_once');
        $op.op('stmts');
        $op.push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($temp), :scope('local'), :decl('var') ),
            $inv
        ));
        $op.push(QAST::Op.new(
            :op('dispatch'),
            QAST::SVal.new( :value('raku-meth-call-qualified') ),
            QAST::Op.new(
                :op('decont'),
                QAST::Var.new( :name($temp), :scope('local') ),
            ),
            $name_node,
            $type,
            QAST::Var.new( :name($temp), :scope('local') ),
            |@args
        ));
        $!block_var_stack.do('unregister_call');
        $op;
#?endif
    }

    method optimize_maybe_method_call($op) {
        # Spesh plugins only available on MoarVM.
#?if !moar
        $op;
#?endif
#?if moar
        # We can only optimize if we have a compile-time-known name.
        my $name_node := $op[1];
        if nqp::istype($name_node, QAST::Want) && $name_node[1] eq 'Ss' {
            $name_node := $name_node[2];
        }
        return $op unless nqp::istype($name_node, QAST::SVal);

        # We need to evaluate the invocant only once, so will bind it into
        # a temporary.
        my $inv := $op.shift;
        my $name := $op.shift;
        my @args;
        while $op.list {
            nqp::push(@args, $op.shift);
        }
        my $temp := QAST::Node.unique('inv_once');
        $op.op('stmts');
        $op.push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($temp), :scope('local'), :decl('var') ),
            $inv
        ));
        $op.push(QAST::Op.new(
            :op('dispatch'),
            QAST::SVal.new( :value('raku-meth-call-me-maybe') ),
            QAST::Op.new(
                :op('decont'),
                QAST::Var.new( :name($temp), :scope('local') )
            ),
            $name_node,
            QAST::Var.new( :name($temp), :scope('local') ),
            |@args
        ));
        $!block_var_stack.do('unregister_call');
        $op;
#?endif
    }

    method generate_optimized_for($op,$callee,$start,$end,$step) {
        my $it_var     := QAST::Node.unique('range_it_');
        my $last_var   := QAST::Node.unique('range_last_');
        my $callee_var := QAST::Node.unique('range_callee_');

        $op.op('stmts');
        $op.push(QAST::Stmts.new(

# my int $it := $start - $step
          QAST::Op.new( :op<bind>,
            QAST::Var.new(
              :name($it_var), :scope<local>, :decl<var>, :returns(int)
            ),
            nqp::istype($start, QAST::IVal)
                ?? QAST::IVal.new( :value($start.value - $step) )
                !! QAST::Op.new( :op<sub_i>, $start, QAST::IVal.new( :value($step)))
          ),

# my int $last := $end
          QAST::Op.new( :op<bind>,
            QAST::Var.new(
              :name($last_var), :scope<local>, :decl<var>, :returns(int)
            ),
            $end
          ),

# my $callee := { };
          QAST::Op.new( :op<bind>,
            QAST::Var.new(:name($callee_var), :scope<local>, :decl<var>),
            $callee
          ),

# nqp::while(
#   $step < 0 ?? nqp::isge_i !! nqp::isle_i(
#     nqp::bind($it, nqp::add_i($it,$step)),
#     $last_var
#   )
          QAST::Op.new( :op<while>,
            QAST::Op.new(
              op => $step < 0 ?? "isge_i" !! "isle_i",
              QAST::Op.new( :op<bind>,
                QAST::Var.new(:name($it_var), :scope<local>, :returns(int)),
                QAST::Op.new( :op<add_i>,
                  QAST::Var.new(:name($it_var),:scope<local>,:returns(int)),
                  QAST::IVal.new( :value($step) )
                )
              ),
              QAST::Var.new(:name($last_var), :scope<local>, :returns(int))
            ),

#   nqp::call($callee, $it)
            QAST::Op.new( :op<call>,
              QAST::Var.new(:name($callee_var), :scope<local> ),
              QAST::Var.new(:name($it_var), :scope<local>, :returns(int))
            )
          ),

# return Nil
          QAST::WVal.new( :value($!symbols.Nil) )
        ));
    }

    method optimize_for_range($op, $callee, $c2, :$reverse) {
        my $code    := $callee.ann('code_object');
        my $count   := $code.count;
        my $block   := $!symbols.Block;
        my $phasers := nqp::istype($code, $block)
          ?? nqp::getattr($code, $block, '$!phasers')
          !! nqp::null();
        if $count == 1
          && !nqp::isconcrete($phasers)
          && %range_bounds{$c2.name}($c2) -> @fls {
            if $reverse {
                my $tmp := @fls[0];
                @fls[0] := @fls[1];
                @fls[1] := $tmp;
                @fls[2] := -@fls[2];
            }
            $op.shift while $op.list;
            self.generate_optimized_for($op,$callee,@fls[0],@fls[1],@fls[2])
        }
    }


    # Handles visiting a QAST::Op :op('handle').
    method visit_handle($op) {
        my int $orig_void := $!void_context;
        $!void_context    := 0;
        self.visit_children($op, :skip_selectors, :handle);
        $!void_context := $orig_void;
        $op
    }

    # Handles visiting a QAST::Want node.
    method visit_want($want) {
        note("method visit_want $!void_context\n" ~ $want.dump) if $!debug;

        # If it's the sink context void node, then only visit the first
        # child. Otherwise, see all.
        if nqp::elems($want) == 3 && $want[1] eq 'v' {
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
        if $!void_context && nqp::elems($want) == 3 && $want.node
           && !$want.ann('sink-quietly') {
            my str $warning;
            my $no-sink;

            if $want[1] eq 'Ss' && nqp::istype($want[2], QAST::SVal) {
                $warning := 'constant string "'
                  ~ nqp::escape($want[2].node // $want[2].value)
                  ~ '"'
            }
            elsif $want[1] eq 'Ii' && nqp::istype($want[2], QAST::IVal) {
                $warning := 'constant integer '
                  ~ ($want[2].node // $want[2].value);
            }
            elsif $want[1] eq 'Nn' && nqp::istype($want[2], QAST::NVal) {
                $warning := 'constant floating-point number '
                  ~ ($want[2].node // $want[2].value);
            }
            elsif $want[1] eq 'v' && nqp::istype($want[2], QAST::Op) {
# R#2040
# - QAST::Op(p6capturelex)
#   - QAST::Op(callmethod clone)
#     - QAST::WVal(Sub...)
                if $want[0].op eq 'p6capturelex' {
                    my $op := $want[0][0];
                    if $op.op eq 'callmethod' && $op.name eq 'clone' {
                        $op := $op[0];
                        if nqp::istype($op, QAST::WVal) && nqp::istype(
                          $op.value,
                          $!symbols.find_in_setting("Sub")
                        ) && !$op.value.name {
                            $warning := qq[anonymous sub, did you forget to provide a name?];
                            $no-sink := 1;
                        }
                    }
                }
            }

            if $warning {
                $warning := "Useless use of " ~ $warning;
                $warning := $warning ~ qq[ in sink context] unless $no-sink;
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
            if $decl {
                $!block_var_stack.do('add_decl', $var);
                if $decl eq 'param' {
                    self.visit_children($var);
                    my $default := $var.default;
                    if $default {
                        my $stmts_def := QAST::Stmts.new( $default );
                        if nqp::istype($default, QAST::Op) && $default.op eq 'getlexouter' {
                            $!block_var_stack.do('register_getlexouter_usage', $stmts_def);
                            # Don't modify the declaration in dry run, only collect the data.
                            $var.default($stmts_def) unless $!block_var_stack.in-dry-run;
                        }
                        else {
                            self.visit_children($stmts_def);
                            $var.default($stmts_def[0]);
                        }
                    }
                }
            }
            else {
                $!block_var_stack.do('add_usage', $var);
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
    my @prim_names := ['', 'int', 'num', 'str', '', '', '', '', '', '', 'uint'];
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
                # those out for their Raku equivalents.
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
        if nqp::elems(@types) == 2 && $num_prim == 1 && $num_allo == 1 {
            my int $prim_flag := @flags[0] || @flags[1];
            my int $allo_idx := @allomorphs[0] ?? 0 !! 1;
            if @allomorphs[$allo_idx] eq @allo_map[$prim_flag] {
                @flags[$allo_idx] := $prim_flag +| $ARG_IS_LITERAL;
            }
        }

        # Alternatively, a single arg that is allomorphic will prefer
        # the literal too.
        if nqp::elems(@types) == 1 && $num_allo == 1 {
            my $rev := %allo_rev{@allomorphs[0]};
            @flags[0] := nqp::defined($rev) ?? $rev +| $ARG_IS_LITERAL !! 0;
        }

        [@types, @flags]
    }

    method report_inevitable_dispatch_failure($op, @types, @flags, $obj, :$protoguilt) {
        my @arg_names;
        my int $i := -1;
        while ++$i < +@types {
            @arg_names.push(
                @flags[$i] == 1  ?? 'int' !!
                @flags[$i] == 2  ?? 'num' !!
                @flags[$i] == 3  ?? 'str' !!
                @flags[$i] == 10 ?? 'uint' !!
                @types[$i].HOW.name(@types[$i]));
        }

        my %opts := nqp::hash();
        %opts<protoguilt> := $protoguilt // nqp::hllboolfor(0, "Raku");
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

    # Visits all of a node's children, and dispatches appropriately.
    method visit_children($node, :$skip_selectors, :$resultchild, :$first, :$void_default,
                          :$handle, :$block_structure) {
        note("method visit_children $!void_context\n" ~ $node.dump(4)) if $!debug;
        my $symbols := $!symbols;
        my int $r := $resultchild // -1;
        my int $i := 0;
        my int $n := nqp::elems($node);
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
                if $handle && $i > 0 {
                    $!block_var_stack.do('entering_handle_handler');
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
                elsif !$block_structure && nqp::istype($visit, QAST::Stmts) &&
                        nqp::elems($visit.list) == 1 && !$visit.named {
                    self.visit_children($visit,:void_default);
                    $node[$i] := $visit[0];
                }
                elsif nqp::istype($visit, QAST::Stmt) || nqp::istype($visit, QAST::Stmts) {
                    my int $resultchild := $visit.resultchild // nqp::elems($visit) - 1;
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
                        }
                    }
                }
                elsif nqp::istype($visit, QAST::Regex) {
                    $!block_var_stack.poison-var-lowering();
                    QRegex::Optimizer.new().optimize($visit, $symbols.top_block, |%!adverbs);
                }
                elsif nqp::istype($visit, QAST::WVal) {
                    if $!void_context {
                        if $visit.ann: 'ok_to_null_if_sunk' {
                            $node[$i] := $NULL;
                        }
                        elsif $visit.has_compile_time_value && $visit.node {
                            my $value := ~$visit.node;
                            $value := '""' if $value eq '';
                            my $suggest := ($visit.okifnil ?? ' (use Nil instead to suppress this warning)' !! '');
                            unless $value eq 'Nil'
                            || $visit.ann('sink-quietly') {
                                my $thing :=    nqp::istype($visit.value,
                                  $symbols.find_in_setting: 'Int')
                                ?? 'integer' !! nqp::istype($visit.value,
                                  $symbols.find_in_setting: 'Rational')
                                ?? 'rational' !! 'value';
                                my $warning := qq[Useless use of constant $thing $value in sink context$suggest];
                                note($warning) if $!debug;
                                $!problems.add_worry($visit, $warning)
                            }
                        }
                    }
                    if $visit.value =:= $symbols.PseudoStash {
                        $!block_var_stack.poison-var-lowering();
                    }
                    elsif nqp::istype($visit.value, $symbols.Regex) {
                        $!block_var_stack.do('poison_topic_lowering');
                    }
                    elsif nqp::istype($visit.value, $symbols.AST) {
                        $!block_var_stack.do('poison_lowering');
                    }
                }
                elsif nqp::istype($visit, QAST::ParamTypeCheck) {
                  self.optimize-param-typecheck: $visit;
                }
                elsif nqp::istype($visit, QAST::SVal) ||
                      nqp::istype($visit, QAST::IVal) ||
                      nqp::istype($visit, QAST::NVal) ||
                      nqp::istype($visit, QAST::VM)
                { }
                else {
                    note("Weird node visited: " ~ $visit.HOW.name($visit)) if $!debug;
                }
                if $handle && $i > 0 {
                    $!block_var_stack.do('leaving_handle_handler');
                }
            }
            $i               := $first ?? $n !! $i + 1;
            $!void_context   := $outer_void;
            $!in_declaration := $outer_decl;
        }
        $node;
    }

    # See if we can simplify QAST::ParamTypeCheck
    method optimize-param-typecheck($node) {
        # We're looking for a structure like this:
        # - QAST::ParamTypeCheck  :code-post-constraint<?>
        #   - QAST::Op(istrue)
        #     - QAST::Op(callmethod ACCEPTS)
        #       - QAST::Op(p6capturelex)
        #         - QAST::Op(callmethod clone)
        #           - QAST::WVal(Block)
        #       - QAST::Var(local __lowered_param__16753)

        return NQPMu unless $node.has_ann('code-post-constraint');
        my $wv-block   := $node[0][0][0][0][0];
        my $param-var  := $node[0][0][1];
        my $qast-block := nqp::getattr($wv-block.value,
            $!symbols.find_symbol(['Code']), '@!compstuff')[0];

        if nqp::istype($qast-block[1], QAST::Stmts) && @($qast-block[1]) == 1 {
            $qast-block[1] := $qast-block[1][0];
        }

        # do we have an "any" Junction we can inline?
        if nqp::istype($qast-block[1], QAST::Op)
        && $qast-block[1].op eq 'callmethod' && $qast-block[1].name eq 'ACCEPTS'
        && @($qast-block[1]) == 2
        && nqp::istype($qast-block[1][0], QAST::WVal)
        && nqp::istype((my $j := $qast-block[1][0].value),
            (my $symJunction := $!symbols.find_symbol: ['Junction']))
        && nqp::getattr($j, $symJunction, '$!type') eq 'any'
        {
            my @types := nqp::getattr($j, $symJunction, '$!eigenstates');
            return NQPMu if nqp::isconcrete($_) for @types;

            my $op := my $qast := QAST::Stmts.new;
            for @types {
                $op.push: my $new-op := QAST::Op.new: :op<unless>,
                    QAST::Op.new: :op<istype>, $param-var,
                      QAST::WVal.new: :value($_);
                $op := $new-op;
            }
            # rewrite last `unless` into `istype` in the second branch of
            # parent `unless` (or just the top node, if we only got one WVal)
            $op.op: 'istype'; $op.push: $op[0][1]; $op[0] := $op[0][0];
            $qast := $qast[0]; # toss Stmts, we no longer need 'em;

            if $node.has_ann('no-autothread') {
                # Our param won't autothread; inject special handling of
                # Junction arguments to make them go through the slower
                # path of using the original ACCEPTS call
                $node[0] := QAST::Op.new: :op<if>,
                  QAST::Op.new(:op<istype>,
                    $param-var, QAST::WVal.new: :value($symJunction)),
                  $node[0], $qast;
            }
            else {
                $node[0] := $qast;
            }
        }
    }

    # Inlines an immediate block.
    method inline_immediate_block($block, $outer) {
        # Sanity check.
        return $block if nqp::elems($block) != 2;
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

        # Extract interesting stuff in declaration section.
        my @copy_decls;
        for @($decls) {
            if nqp::istype($_, QAST::Op) && ($_.op eq 'p6bindsig' ||
                    $_.op eq 'bind' && $_[0].name eq 'call_sig') {
                # Don't copy this binder call or setup.
            }
            elsif nqp::istype($_, QAST::Op) && $_.op eq 'bind' &&
                    nqp::istype($_[1], QAST::Op) && $_[1].op eq 'getlexouter' {
                # Initialization from outer $_ simply becomes reading the $_ in
                # the block we're flattening into and binding it to the inner
                # lowered one.
                $_[1] := QAST::Var.new( :name($_[1][0].value), :scope('lexical') );
                $!block_var_stack.do('add_usage', $_[1]);
                @copy_decls.push($_);
            }
            elsif nqp::istype($_, QAST::Var) && $_.scope eq 'lexical' {
                # It's a lexical. If the outer we're merging into already has such
                # a symbol, do nothing (we just need it for "fallback" purposes).
                # Otherwise, copy it and register it in the outer.
                my $name := $_.name;
                unless $name eq '$*DISPATCHER' || $name eq '$_' || $outer.symbol($name) {
                    @copy_decls.push($_);
                    $outer.symbol($name, :scope('lexical'));
                }
            }
            elsif nqp::istype($_, QAST::Op) && $_.op eq 'takedispatcher' {
                # Don't copy the dispatcher take, since the $*DISPATCHER is
                # also not copied.
            }
            else {
                @copy_decls.push($_);
            }
        }

        # Copy local debug name mappings.
        for $block.local_debug_map {
            $outer.add_local_debug_mapping($_.key, $_.value);
        }

        # Hand back the decls and statements that we're inlining.
        return QAST::Stmts.new( |@copy_decls, $stmts );
    }

    # Inlines a call to a sub.
    method inline_call($call, $code_obj) {
        # If the code object is marked soft, can't inline it.
        if nqp::can($code_obj, 'soft') && $code_obj.soft {
            return $call;
        }

        $!block_var_stack.do('unregister_call');

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
#?if jvm
        my @cands := $proto.dispatchees();
        my int $idx := 0;
        for @cands {
            if $_ =:= $chosen {
                $call.unshift(QAST::Op.new(
                    :op('atpos'),
                    QAST::Var.new(
                        :name('@!dispatchees'), :scope('attribute'),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($call.name), :scope('lexical') )
                        ),
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
#?endif
#?if !jvm
        my $scopes := $!symbols.scopes_in($call.name);
        if $scopes == 0 || $scopes == 1 && nqp::can($proto, 'soft') && !$proto.soft {
            $call.op('callstatic');
        }
#?endif
        $call
    }

    # Looks through positional args for any lexicalref or attributeref, and
    # if we find them check if the expectation is for an non-rw argument.
    method simplify_refs($call, $sig) {
        if nqp::iseq_n($sig.arity, $sig.count) {
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
                            my $param := nqp::getattr($sig, $!symbols.Signature, '@!params')[$p];
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

    my @prim_spec_ops := ['', 'p6box_i', 'p6box_n', 'p6box_s', '', '', '', '', '', '', 'p6box_u'];
    my @prim_spec_flags := ['', 'Ii', 'Nn', 'Ss', '', '', '', '', '', '', 'Ii']; #FIXME maybe need Iu or even Uu here?
    sub copy_returns($to, $from) {
        if nqp::can($from, 'returns') {
            my $ret_type := $from.returns;
            $to.returns($ret_type) unless nqp::can($from, 'rw') && $from.rw;
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

    method chain_depth() { $!chain_depth }

    method block_var_stack() { $!block_var_stack }

    method routine_candidates($routine, @candidates = [], :$proto = 1) {
        @candidates.push: $routine if $proto || !$routine.is_dispatcher;
        if nqp::can($routine, 'is-wrapped') && $routine.is-wrapped {
            for @($routine.WRAPPERS) -> $wrapper {
                self.routine_candidates($wrapper, @candidates);
            }
        }
        elsif nqp::can($routine, 'is_dispatcher') && $routine.is_dispatcher {
            my @dispatchees := nqp::istype($routine, NQPRoutine)
                                ?? nqp::getattr($routine, NQPRoutine, '$!dispatchees')
                                !! nqp::getattr($routine, $!symbols.Routine, '@!dispatchees');
            for @dispatchees {
                self.routine_candidates($_, @candidates);
            }
        }
        @candidates
    }

    # Find out if a routine belongs to the core. For a multi it means the proto and all its candidates are from the
    # core.
    # With $u-invocant only the candidates which are methods with :U invocant specification are taken into account
    method is_routine_setting_only($routine, :$u-invocant = 0) {
        my @candidates := self.routine_candidates($routine);
        for @candidates -> $cand {
            if $u-invocant && nqp::istype($cand, $!symbols.Method) {
                my $signature := $cand.signature;

                # Skip NQP candidates as we consider them default by definition. The only reason we ever iterate
                # over NQPRoutine proto is if somehow Raku-land code would install its candidate.
                next if nqp::istype($signature, NQPSignature);

                my $invocant_type := $signature.params.AT-POS(0).type;
                # Skip a method with :D if requested
                next if $invocant_type.HOW.archetypes($invocant_type).definite
                        && $invocant_type.HOW.definite($invocant_type);
            }

            return 0 unless nqp::istype($cand, NQPRoutine) || nqp::index($cand.file, 'SETTING::') == 0;
        }
        1
    }
}

# vim: expandtab sw=4
