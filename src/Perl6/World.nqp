use NQPHLL;
use QAST;
use Perl6::ModuleLoader;
use Perl6::Pod;
use Perl6::Ops;

# Binder constants.
# XXX Want constant syntax in NQP really.
my $SIG_ELEM_BIND_CAPTURE        := 1;
my $SIG_ELEM_BIND_PRIVATE_ATTR   := 2;
my $SIG_ELEM_BIND_PUBLIC_ATTR    := 4;
my $SIG_ELEM_SLURPY_POS          := 8;
my $SIG_ELEM_SLURPY_NAMED        := 16;
my $SIG_ELEM_SLURPY_LOL          := 32;
my $SIG_ELEM_INVOCANT            := 64;
my $SIG_ELEM_MULTI_INVOCANT      := 128;
my $SIG_ELEM_IS_RW               := 256;
my $SIG_ELEM_IS_COPY             := 512;
my $SIG_ELEM_IS_RAW              := 1024;
my $SIG_ELEM_IS_OPTIONAL         := 2048;
my $SIG_ELEM_ARRAY_SIGIL         := 4096;
my $SIG_ELEM_HASH_SIGIL          := 8192;
my $SIG_ELEM_DEFAULT_FROM_OUTER  := 16384;
my $SIG_ELEM_IS_CAPTURE          := 32768;
my $SIG_ELEM_UNDEFINED_ONLY      := 65536;
my $SIG_ELEM_DEFINED_ONLY        := 131072;
my $SIG_ELEM_NOMINAL_GENERIC     := 524288;
my $SIG_ELEM_DEFAULT_IS_LITERAL  := 1048576;
my $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
my $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
my $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
my $SIG_ELEM_SLURPY_ONEARG       := 16777216;
my $SIG_ELEM_CODE_SIGIL          := 33554432;

sub p6ize_recursive($x) {
    if nqp::islist($x) {
        my @copy := [];
        for $x {
            nqp::push(@copy, p6ize_recursive($_));
        }
        return nqp::hllizefor(@copy, 'perl6');
    }
    elsif nqp::ishash($x) {
        my %copy := nqp::hash();
        for $x {
            %copy{$_.key} := p6ize_recursive($_.value);
        }
        return nqp::hllizefor(%copy, 'perl6').item;
    }
    nqp::hllizefor($x, 'perl6');
}

# Helper sub that turns a list of items into an NQPArray. This is needed when
# using e.g. $*W.find_symbol from Perl 6 code (example: slangs).
sub nqplist(*@arr) { @arr }
nqp::bindcurhllsym('nqplist', &nqplist);

# this levenshtein implementation is used to suggest good alternatives
# when deriving from an unknown/typo'd class.
sub levenshtein($a, $b) {
    my %memo;
    my $alen := nqp::chars($a);
    my $blen := nqp::chars($b);

    return 0 if $alen eq 0 || $blen eq 0;

    # the longer of the two strings is an upper bound.
    #my $bound := $alen < $blen ?? $blen !! $alen;

    sub changecost($ac, $bc) {
        sub issigil($_) { nqp::index('$@%&|', $_) != -1 };
        return 0 if $ac eq $bc;
        return 0.1 if nqp::fc($ac) eq nqp::fc($bc);
        return 0.5 if issigil($ac) && issigil($bc);
        return 1;
    }

    sub levenshtein_impl($apos, $bpos, $estimate) {
        my $key := join(":", ($apos, $bpos));

        return %memo{$key} if nqp::existskey(%memo, $key);

        # if either cursor reached the end of the respective string,
        # the result is the remaining length of the other string.
        sub check($pos1, $len1, $pos2, $len2) {
            if $pos2 == $len2 {
                return $len1 - $pos1;
            }
            return -1;
        }

        my $check := check($apos, $alen, $bpos, $blen);
        return $check unless $check == -1;
        $check := check($bpos, $blen, $apos, $alen);
        return $check unless $check == -1;

        my $achar := nqp::substr($a, $apos, 1);
        my $bchar := nqp::substr($b, $bpos, 1);

        my $cost := changecost($achar, $bchar);

        # hyphens and underscores cost half when adding/deleting.
        my $addcost := 1;
        $addcost := 0.5 if $bchar eq "-" || $bchar eq "_";

        my $delcost := 1;
        $delcost := 0.5 if $achar eq "-" || $achar eq "_";

        my $ca := levenshtein_impl($apos+1, $bpos,   $estimate+$delcost) + $delcost; # what if we remove the current letter from A?
        my $cb := levenshtein_impl($apos,   $bpos+1, $estimate+$addcost) + $addcost; # what if we add the current letter from B?
        my $cc := levenshtein_impl($apos+1, $bpos+1, $estimate+$cost) + $cost; # what if we change/keep the current letter?

        # the result is the shortest of the three sub-tasks
        my $distance;
        $distance := $ca if $ca <= $cb && $ca <= $cc;
        $distance := $cb if $cb <= $ca && $cb <= $cc;
        $distance := $cc if $cc <= $ca && $cc <= $cb;

        # switching two letters costs only 1 instead of 2.
        if $apos + 1 <= $alen && $bpos + 1 <= $blen &&
           nqp::eqat($a, $bchar, $apos + 1) && nqp::eqat($b, $achar, $bpos + 1) {
            my $cd := levenshtein_impl($apos+2, $bpos+2, $estimate+1) + 1;
            $distance := $cd if $cd < $distance;
        }

        %memo{$key} := $distance;
        return $distance;
    }

    my $result := levenshtein_impl(0, 0, 0);
    return $result;
}

sub make_levenshtein_evaluator($orig_name, @candidates) {
    my $Str-obj := $*W.find_symbol(["Str"]);
    my $find-count := 0;
    my $try-count := 0;
    sub inner($name, $hash) {
        # difference in length is a good lower bound.
        $try-count := $try-count + 1;
        return 0 if $find-count > 20 || $try-count > 1000;
        my $parlen := nqp::chars($orig_name);
        my $lendiff := nqp::chars($name) - $parlen;
        $lendiff := -$lendiff if $lendiff < 0;
        return 1 if $lendiff >= $parlen * 0.3;

        my $dist := levenshtein($orig_name, $name) / $parlen;
        my $target := -1;
        $target := @candidates[0] if $dist <= 0.1;
        $target := @candidates[1] if 0.1 < $dist && $dist <= 0.2;
        $target := @candidates[2] if 0.2 < $dist && $dist <= 0.35;
        if $target != -1 {
            my $name-str := nqp::box_s($name, $Str-obj);
            nqp::push($target, $name-str);
            $find-count := $find-count + 1;
        }
        1;
    }
    return &inner;
}

sub levenshtein_candidate_heuristic(@candidates, $target) {
    # only take a few suggestions
    my $to-add := 5;
    for @candidates[0] {
        $target.push($_) if $to-add > 0;
        $to-add := $to-add - 1;
    }
    $to-add := $to-add - 1 if +@candidates[0] > 0;
    for @candidates[1] {
        $target.push($_) if $to-add > 0;
        $to-add := $to-add - 1;
    }
    $to-add := $to-add - 2 if +@candidates[1] > 0;
    for @candidates[2] {
        $target.push($_) if $to-add > 0;
        $to-add := $to-add - 1;
    }
}

# This builds upon the HLL::World to add the specifics needed by Rakudo Perl 6.
class Perl6::World is HLL::World {
    my class Perl6CompilationContext is HLL::World::CompilationContext {
        # The stack of lexical pads, actually as QAST::Block objects. The
        # outermost frame is at the bottom, the latest frame is on top.
        has @!BLOCKS;

        # The stack of code objects; phasers get attached to the top one.
        has @!CODES;

        # Mapping of sub IDs to their code objects; used for fixing up in
        # dynamic compilation.
        has %!sub_id_to_code_object;

        # Mapping of sub IDs to any code objects that were cloned during
        # compilation before we had chance to compile the code. These are
        # not true closures (in those cases the surrounding scope that it
        # would close over is also compiled), but rather are clones for
        # things like proto method derivation.
        has %!sub_id_to_cloned_code_objects;

        # Mapping of sub IDs to SC indexes of code stubs.
        has %!sub_id_to_sc_idx;

        # Array of stubs to check and the end of compilation.
        has @!stub_check;

        # Array of protos that can have their candidates pre-sorted at CHECK
        # time.
        has @!protos_to_sort;

        # Cached constants that we've built.
        has %!const_cache;

        # Cached * and ** instances.
        has $!the_whatever;
        has $!the_hyper_whatever;

        # List of CHECK blocks to run.
        has @!CHECKs;

        # Clean-up tasks, to do after CHECK time.
        has @!cleanup_tasks;

        # Cache of container info and descriptor for magicals.
        has %!magical_cds;

        method BUILD(:$handle, :$description) {
            @!BLOCKS := [];
            @!CODES := [];
            @!stub_check := [];
            @!protos_to_sort := [];
            @!CHECKs := [];
            %!sub_id_to_code_object := {};
            %!sub_id_to_cloned_code_objects := {};
            %!sub_id_to_sc_idx := {};
            %!const_cache := {};
            @!cleanup_tasks := [];
            %!magical_cds := {};
        }

        method blocks() {
            @!BLOCKS
        }

        # Creates a new lexical scope and puts it on top of the stack.
        method push_lexpad($/) {
            # Create pad, link to outer, annotate with creating statement, and add to stack.
            my $pad := QAST::Block.new( QAST::Stmts.new( :node($/) ) );
            if $*WANTEDOUTERBLOCK {  # (outside of 1st push/pop pass)
                $pad.annotate('outer', $*WANTEDOUTERBLOCK);
            }
            elsif +@!BLOCKS {
                $pad.annotate('outer', @!BLOCKS[+@!BLOCKS - 1]);
            }
            $pad.annotate('statement_id', $*STATEMENT_ID);
            $pad.annotate('in_stmt_mod', $*IN_STMT_MOD);
            @!BLOCKS[+@!BLOCKS] := $pad;
            $pad
        }

        # Pops a lexical scope off the stack.
        method pop_lexpad() {
            @!BLOCKS.pop()
        }

        # Gets the top lexpad.
        method cur_lexpad() {
            @!BLOCKS[+@!BLOCKS - 1]
        }

        # Marks the current lexpad as being a signatured block.
        method mark_cur_lexpad_signatured() {
            @!BLOCKS[+@!BLOCKS - 1].annotate('signatured', 1);
        }

        # Finds the nearest signatured block and checks if it declares
        # a certain symbol.
        method nearest_signatured_block_declares(str $symbol) {
            my $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                if @!BLOCKS[$i].ann('signatured') {
                    return +@!BLOCKS[$i].symbol($symbol);
                }
            }
        }

        # Hunts through scopes to find the type of a lexical.
        method find_lexical_container_type(str $name) {
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($name);
                if +%sym {
                    if nqp::existskey(%sym, 'type') {
                        return %sym<type>;
                    }
                    else {
                        $i := 0;
                    }
                }
            }
            nqp::die("Could not find container descriptor for $name");
        }

        # Hunts through scopes to find a lexical and returns if it is
        # known to be read-only.
        method is_lexical_marked_ro(str $name) {
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($name);
                if %sym {
                    return nqp::existskey(%sym, 'ro');
                }
            }
            0;
        }

        # Checks if the given name is known anywhere in the lexpad
        # and with lexical scope.
        method is_lexical(str $name) {
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($name);
                if +%sym {
                    return %sym<scope> eq 'lexical';
                }
            }
            0;
        }

        # Checks if the symbol is really an alias to an attribute.
        method is_attr_alias(str $name) {
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($name);
                if +%sym {
                    return %sym<attr_alias>;
                }
            }
        }

        # Gets top code object in the code objects stack, or optionally the
        # one the specified number of scopes down.
        method get_code_object(int :$scopes = 0) {
            $scopes < nqp::elems(@!CODES)
                ?? @!CODES[nqp::elems(@!CODES) - ($scopes + 1)]
                !! NQPMu
        }

        method push_code_object($code) {
            @!CODES[+@!CODES] := $code
        }

        method pop_code_object() {
            @!CODES.pop()
        }

        method cur_code_object() {
            @!CODES[+@!CODES - 1]
        }

        # Pushes a stub on the "stubs to check" list.
        method add_stub_to_check($stub) {
            nqp::push(@!stub_check, $stub);
        }

        method stub_check() {
            @!stub_check
        }

        # Adds a proto to be sorted at CHECK time.
        method add_proto_to_sort($proto) {
            nqp::push(@!protos_to_sort, $proto);
        }

        method protos_to_sort() {
            @!protos_to_sort
        }

        method magical_cds() {
            %!magical_cds
        }

        method sub_id_to_code_object() {
            %!sub_id_to_code_object
        }

        method sub_id_to_sc_idx() {
            %!sub_id_to_sc_idx
        }

        method const_cache() {
            %!const_cache
        }

        method add_cleanup_task($task) {
            nqp::push(@!cleanup_tasks, $task)
        }

        method cleanup_tasks() {
            @!cleanup_tasks
        }

        method add_clone_for_cuid($clone, $cuid) {
            unless %!sub_id_to_cloned_code_objects{$cuid} {
                %!sub_id_to_cloned_code_objects{$cuid} := [];
            }
            %!sub_id_to_cloned_code_objects{$cuid}.push($clone);
        }

        method sub_id_to_cloned_code_objects() {
            %!sub_id_to_cloned_code_objects
        }

        method whatever() {
            $!the_whatever
        }

        method set_whatever($whatever) {
            $!the_whatever := $whatever
        }

        method hyper_whatever() {
            $!the_hyper_whatever
        }

        method set_hyper_whatever($hyper_whatever) {
            $!the_hyper_whatever := $hyper_whatever
        }

        method add_check($check) {
            @!CHECKs := [] unless @!CHECKs;
            @!CHECKs.unshift($check);
        }

        method checks() {
            @!CHECKs
        }
    }

    method context_class() {
        Perl6CompilationContext
    }

    # Mapping of QAST::Stmts node containing fixups, keyed by sub ID. If
    # we do dynamic compilation then we do the fixups immediately and
    # then clear this list.
    # Doesn't need to be shared - is used for BEGIN blocks
    has %!code_object_fixup_list;

    # Cached compiler services object, if any.
    has $!compiler_services;

    # are we module debugging?
    has $!RAKUDO_MODULE_DEBUG;

    has $!record_precompilation_dependencies;

    method BUILD(*%adv) {
        %!code_object_fixup_list := {};
        $!record_precompilation_dependencies := 1;
    }

    method create_nested() {
        Perl6::World.new(:handle(self.handle), :context(self.context()))
    }

    method RAKUDO_MODULE_DEBUG() {
        if nqp::isconcrete($!RAKUDO_MODULE_DEBUG) {
            $!RAKUDO_MODULE_DEBUG
        }
        elsif !$*COMPILING_CORE_SETTING {
            $!RAKUDO_MODULE_DEBUG :=
              self.find_symbol(['&DYNAMIC'])('$*RAKUDO_MODULE_DEBUG')
        }
        else {
            $!RAKUDO_MODULE_DEBUG := False
        }
    }

    method loading_and_symbol_setup($/) {
        my $setting_name;

        # Create unit outer (where we assemble any lexicals accumulated
        # from e.g. REPL) and the real UNIT.
        $*UNIT_OUTER := self.push_lexpad($/);
        $*UNIT       := self.push_lexpad($/);
        my $in_eval  := 0;

        # If we already have a specified outer context, then that's
        # our setting. Otherwise, load one.
        my $have_outer := nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>);
        if $have_outer {
            $setting_name := '';
            $*UNIT.annotate('IN_DECL', 'eval');
            $in_eval := 1;
        }
        else {
            $setting_name := %*COMPILING<%?OPTIONS><setting> // 'CORE';
            $*COMPILING_CORE_SETTING := 1 if $setting_name eq 'NULL';
            self.load_setting($/,$setting_name);
            $*UNIT.annotate('IN_DECL', 'mainline');
        }
        $/.CURSOR.unitstart();

        try {
            my $EXPORTHOW := self.find_symbol(['EXPORTHOW']);
            for self.stash_hash($EXPORTHOW) {
                %*HOW{$_.key} := $_.value;
            }
        }

        # Create GLOBAL(ish), unless we were given one.
        if nqp::existskey(%*COMPILING<%?OPTIONS>, 'global') {
            $*GLOBALish := %*COMPILING<%?OPTIONS><global>;
        }
        elsif $have_outer && $*UNIT_OUTER.symbol('GLOBALish') {
            $*GLOBALish :=
              self.force_value($*UNIT_OUTER.symbol('GLOBALish'),'GLOBALish',1);
        }
        else {
            $*GLOBALish :=
              self.pkg_create_mo($/,%*HOW<package>,:name('GLOBAL'));
            self.pkg_compose($/, $*GLOBALish);
        }

        # Create or pull in existing EXPORT.
        if $have_outer && $*UNIT_OUTER.symbol('EXPORT') {
            $*EXPORT :=
              self.force_value($*UNIT_OUTER.symbol('EXPORT'), 'EXPORT', 1);
        }
        else {
            $*EXPORT := self.pkg_create_mo($/, %*HOW<package>, :name('EXPORT'));
            self.pkg_compose($/, $*EXPORT);
        }

        # If there's a self in scope, set $*HAS_SELF.
        if $have_outer && $*UNIT_OUTER.symbol('self') {
            $*HAS_SELF := 'complete';
        }

        # Take current package from outer context if any, otherwise for a
        # fresh compilation unit we start in GLOBAL.
        if $have_outer && $*UNIT_OUTER.symbol('$?PACKAGE') {
            $*PACKAGE :=
              self.force_value($*UNIT_OUTER.symbol('$?PACKAGE'),'$?PACKAGE',1);
        }
        else {
            $*PACKAGE := $*GLOBALish;
        }

        # If we're eval'ing in the context of a %?LANG, set up our own
        # %*LANG based on it.
        if $have_outer && $*UNIT_OUTER.symbol('%?LANG') {
            for self.force_value(
              $*UNIT_OUTER.symbol('%?LANG'), '%?LANG', 1).FLATTENABLE_HASH() {
                %*LANG{$_.key} := $_.value;
            }
        }
        if $have_outer && $*UNIT_OUTER.symbol('$*MAIN') {
            $*MAIN :=
              self.force_value($*UNIT_OUTER.symbol('$*MAIN'), '$*MAIN', 1);
        }
        if $have_outer && $*UNIT_OUTER.symbol('$?STRICT') {
            $*STRICT :=
              self.force_value($*UNIT_OUTER.symbol('$*STRICT'), '$*STRICT', 1);
        }
        else {
            $*STRICT  := 1;
        }

        # Bootstrap
        if $setting_name eq 'NULL' {
            my $name   := "Perl6::BOOTSTRAP";
            my $module := self.load_module_early($/, $name, {}, $*GLOBALish);
            my $EXPORT := $module<EXPORT>.WHO;
            my @to_import := ['MANDATORY', 'DEFAULT'];
            for @to_import -> $tag {
                if nqp::existskey($EXPORT, $tag) {
                    self.import($/, self.stash_hash($EXPORT{$tag}), $name);
                }
            }
            for $module<EXPORTHOW>.WHO {
                my str $key := $_.key;
                %*HOW{$key} := nqp::decont($_.value);
            }
        }

        # Install as we've no setting, in which case we've likely no
        # static lexpad class yet either. Also, UNIT needs a code object.
        else {
            self.install_lexical_symbol($*UNIT, 'GLOBALish', $*GLOBALish);
            self.install_lexical_symbol($*UNIT, 'EXPORT', $*EXPORT);
            self.install_lexical_symbol($*UNIT, '$?PACKAGE', $*PACKAGE);
            self.install_lexical_symbol($*UNIT, '::?PACKAGE', $*PACKAGE);
            $*CODE_OBJECT := $*DECLARAND := self.stub_code_object('Block');

            unless $in_eval {
                self.install_lexical_symbol(
                  $*UNIT,'$=finish',self.find_symbol(['Mu']));
            }
        }

        unless $in_eval {
            my $M := %*COMPILING<%?OPTIONS><M>;
            if nqp::defined($M) {
                for nqp::islist($M) ?? $M !! [$M] -> $longname {
                    self.do_pragma_or_load_module($/,1,$longname);
                }
            }
        }
    }

    method mop_up_and_check($/) {

        # Install POD-related variables.
        $*POD_PAST := self.add_constant(
            'Array', 'type_new', :nocache, |$*POD_BLOCKS
        );
        self.install_lexical_symbol(
            $*UNIT, '$=pod', $*POD_PAST.compile_time_value
        );

        # Tag UNIT with a magical lexical. Also if we're compiling CORE,
        # give it such a tag too.
        my $name := $*COMPILING_CORE_SETTING
          ?? '!CORE_MARKER'
          !! '!UNIT_MARKER';
        my $marker := self.pkg_create_mo($/, %*HOW<package>, :$name);
        $marker.HOW.compose($marker);
        self.install_lexical_symbol($*UNIT, $name, $marker);

        # CHECK time.
        self.CHECK();

        # Clean up compiler services.
        if $!compiler_services {
            my $cs := $!compiler_services;
            nqp::bindattr($cs, $cs.WHAT, '$!compiler', nqp::null());
        }
    }

    # Creates a new lexical scope and puts it on top of the stack.
    method push_lexpad($/) {
        self.context().push_lexpad($/)
    }

    # Pops a lexical scope off the stack.
    method pop_lexpad() {
        self.context().pop_lexpad()
    }

    # Gets the top lexpad.
    method cur_lexpad() {
        self.context().cur_lexpad()
    }

    # Marks the current lexpad as being a signatured block.
    method mark_cur_lexpad_signatured() {
        self.context().mark_cur_lexpad_signatured()
    }

    # Finds the nearest signatured block and checks if it declares
    # a certain symbol.
    method nearest_signatured_block_declares(str $symbol) {
        self.context().nearest_signatured_block_declares($symbol)
    }

    # Gets top code object in the code objects stack, or optionally the
    # one the specified number of scopes down.
    method get_code_object(int :$scopes = 0) {
        self.context().get_code_object(:$scopes)
    }

    # Pushes a stub on the "stubs to check" list.
    method add_stub_to_check($stub) {
        self.context().add_stub_to_check($stub)
    }

    # Adds a proto to be sorted at CHECK time.
    method add_proto_to_sort($proto) {
        self.context().add_proto_to_sort($proto)
    }

    # Checks for any stubs that weren't completed.
    method assert_stubs_defined($/) {
        my @incomplete;
        for self.context().stub_check {
            unless $_.HOW.is_composed($_) {
                @incomplete.push($_.HOW.name($_));
            }
        }
        if +@incomplete {
            self.throw($/, 'X::Package::Stubbed', packages => @incomplete);
        }
    }

    # Sorts all protos.
    method sort_protos() {
        for self.context().protos_to_sort() {
            if nqp::can($_, 'sort_dispatchees') {
                $_.sort_dispatchees();
            }
        }
    }

    # Loads a setting.
    method load_setting($/, $setting_name) {
        # Do nothing for the NULL setting.
        if $setting_name ne 'NULL' {
            # Load it immediately, so the compile time info is available.
            # Once it's loaded, set it as the outer context of the code
            # being compiled.
            my $setting := %*COMPILING<%?OPTIONS><outer_ctx>
                        := Perl6::ModuleLoader.load_setting($setting_name);

            # Add a fixup and deserialization task also.
            my $fixup := QAST::Stmt.new(
                self.perl6_module_loader_code(),
                QAST::Op.new(
                    :op('forceouterctx'),
                    QAST::BVal.new( :value($*UNIT_OUTER) ),
                    QAST::Op.new(
                        :op('callmethod'), :name('load_setting'),
                        QAST::Op.new(
                            :op('getcurhllsym'),
                            QAST::SVal.new( :value('ModuleLoader') )
                        ),
                        QAST::SVal.new( :value($setting_name) )
                    )
                )
            );
            self.add_load_dependency_task(:deserialize_ast($fixup), :fixup_ast($fixup));

            return nqp::ctxlexpad($setting);
        }
    }

    method import_EXPORTHOW($/, $handle) {
        my $EXPORTHOW := $handle.export-how-package;
        if nqp::defined($EXPORTHOW) {
            $EXPORTHOW.pairs.map(-> $pair {
                my str $key := $pair.key;
                if $key eq 'SUPERSEDE' {
                    my %SUPERSEDE := self.stash_hash($pair.value);
                    for %SUPERSEDE {
                        my str $pdecl := $_.key;
                        my $meta  := nqp::decont($_.value);
                        unless nqp::existskey(%*HOW, $pdecl) {
                            $/.CURSOR.typed_panic('X::EXPORTHOW::NothingToSupersede',
                                declarator => $pdecl);
                        }
                        if nqp::existskey(%*HOWUSE, $pdecl) {
                            $/.CURSOR.typed_panic('X::EXPORTHOW::Conflict',
                                declarator => $pdecl, directive => $key);
                        }
                        %*HOW{$pdecl}    := $meta;
                        %*HOWUSE{$pdecl} := nqp::hash('SUPERSEDE', $meta);
                    }
                }
                elsif $key eq 'DECLARE' {
                    my %DECLARE := self.stash_hash($pair.value);
                    for %DECLARE {
                        my str $pdecl := $_.key;
                        my $meta  := nqp::decont($_.value);
                        if nqp::existskey(%*HOW, $pdecl) {
                            $/.CURSOR.typed_panic('X::EXPORTHOW::Conflict',
                                declarator => $pdecl, directive => $key);
                        }
                        %*HOW{$pdecl}    := $meta;
                        %*HOWUSE{$pdecl} := nqp::hash('DECLARE', $meta);
                        self.add_package_declarator($/,$pdecl);
                    }
                }
                elsif $key eq 'COMPOSE' {
                    my %COMPOSE := self.stash_hash($pair.value);
                    $/.CURSOR.NYI('EXPORTHOW::COMPOSE');
                }
                else {
                    if $key eq nqp::lc($key) {
                        # Support legacy API, which behaves like an unchecked
                        # supersede.
                        # XXX Can give deprecation warning in the future, remove
                        # before 6.0.0.
                        %*HOW{$key} := nqp::decont($pair.value);
                    }
                    else {
                        $/.CURSOR.typed_panic('X::EXPORTHOW::InvalidDirective', directive => $key);
                    }
                }
            }).eager;
        }
    }

    method add_package_declarator($/, str $pdecl) {
        my $cursor := $/.CURSOR;

        # Compute name of grammar/action entry.
        my $canname := 'package_declarator:sym<' ~ $pdecl ~ '>';

        # Add to grammar if needed.
        unless nqp::can($cursor, $canname) {
            my role PackageDeclarator[$meth_name, $declarator] {
                token ::($meth_name) {
                    :my $*OUTERPACKAGE := $*PACKAGE;
                    :my $*PKGDECL := $declarator;
                    :my $*LINE_NO := HLL::Compiler.lineof($cursor.orig(), $cursor.from(), :cache(1));
                    $<sym>=[$declarator] <.end_keyword> <package_def>
                }
            }
            $cursor.HOW.mixin($cursor, PackageDeclarator.HOW.curry(PackageDeclarator, $canname, $pdecl));

            # This also becomes the current MAIN. Also place it in %?LANG.
            %*LANG<MAIN> := $cursor.WHAT;
        }

        my $actions := %*LANG<MAIN-actions>;
        # Add action method if needed.
        unless nqp::can($actions, $canname) {
            my role PackageDeclaratorAction[$meth] {
                method ::($meth)($/) {
                    make $<package_def>.ast;
                }
            };
            %*LANG<MAIN-actions> := $actions.HOW.mixin($actions,
                PackageDeclaratorAction.HOW.curry(PackageDeclaratorAction, $canname));
        }
        self.install_lexical_symbol(self.cur_lexpad(), '%?LANG', self.p6ize_recursive(%*LANG));
    }

    method do_import($/, $handle, $package_source_name, $arglist?) {
        my $EXPORT := $handle.export-package;
        if nqp::defined($EXPORT) {
            $EXPORT := $EXPORT.FLATTENABLE_HASH();
            my @to_import := ['MANDATORY'];
            my @positional_imports := [];
            if nqp::defined($arglist) {
                my $Pair := self.find_symbol(['Pair']);
                for $arglist -> $tag {
                    if nqp::istype($tag, $Pair) {
                        $tag := nqp::unbox_s($tag.key);
                        if nqp::existskey($EXPORT, $tag) {
                            self.import($/, self.stash_hash($EXPORT{$tag}), $package_source_name);
                        }
                        else {
                            self.throw($/, ['X', 'Import', 'NoSuchTag'],
                                source-package => $package_source_name, :$tag)
                        }
                    }
                    else {
                        nqp::push(@positional_imports, $tag);
                    }
                }
            }
            else {
                nqp::push(@to_import, 'DEFAULT');
            }
            for @to_import -> $tag {
                if nqp::existskey($EXPORT, $tag) {
                    self.import($/, self.stash_hash($EXPORT{$tag}), $package_source_name);
                }
            }
            my &EXPORT := $handle.export-sub;
            if nqp::defined(&EXPORT) {
                my $result := &EXPORT(|@positional_imports);
                my $Map := self.find_symbol(['Map']);
                if nqp::istype($result, $Map) {
                    my $storage := $result.hash.FLATTENABLE_HASH();
                    self.import($/, $storage, $package_source_name);
                }
                else {
                    nqp::die("&EXPORT sub did not return an Map");
                }
            }
            else {
                if +@positional_imports {
                    self.throw($/, ['X', 'Import', 'Positional'],
                        source-package => $package_source_name)
                }
            }
        }
    }

    # pragmas without args
    my %no_args_pragma := nqp::hash(
      'fatal',              1,
      'internals',          1,
      'MONKEY-TYPING',      1,
      'MONKEY-SEE-NO-EVAL', 1,
      'MONKEY-BRAINS',      1,
      'MONKEY-GUTS',        1,
      'MONKEY-BUSINESS',    1,
      'MONKEY-TRAP',        1,
      'MONKEY-SHINE',       1,
      'MONKEY-WRENCH',      1,
      'MONKEY-BARS',        1,
      'nqp',                1,
      'precompilation',     1,
      'strict',             1,
      'trace',              1,
      'worries',            1,
    );

    # pragmas without args that just set %*PRAGMAS
    my %just_set_pragma := nqp::hash(
      'fatal',              1,
      'internals',          1,
      'MONKEY-TYPING',      1,
      'MONKEY-SEE-NO-EVAL', 1,
      'MONKEY-BRAINS',      1,
      'MONKEY-GUTS',        1,
      'MONKEY-BUSINESS',    1,
      'MONKEY-TRAP',        1,
      'MONKEY-SHINE',       1,
      'MONKEY-WRENCH',      1,
      'MONKEY-BARS',        1,
      'nqp',                1,
      'trace',              1,
      'worries',            1,
    );

    # not yet implemented pragmas
    my %nyi_pragma := nqp::hash(
      'internals',  1,
      'invocant',   1,
      'parameters', 1,
    );

    method do_pragma($/,$name,$on,$arglist) {

        my $RMD := self.RAKUDO_MODULE_DEBUG;
        $RMD("Attempting '$name' as a pragma") if $RMD;

        if %nyi_pragma{$name} {
            self.throw($/,
              'X::NYI',
              :feature(($on ?? 'use' !! 'no') ~ " $name"),
            );
        }
        elsif %no_args_pragma{$name} {
            if nqp::islist($arglist) {
                self.throw($/, 'X::Pragma::NoArgs', :$name)
            }
        }

        if %just_set_pragma{$name} {
            %*PRAGMAS{$name} := $on;
        }
        elsif $name eq 'MONKEY' {
            %*PRAGMAS{$_.key} := $on if nqp::eqat($_.key,'MONKEY',0) for %just_set_pragma;
        }
        elsif $name eq 'strict' {
            if nqp::islist($arglist) {
                self.throw($/, 'X::Pragma::NoArgs', :$name)
            }
            $*STRICT  := $on;
        }
        elsif $name eq 'soft' {
            # This is an approximation; need to pay attention to
            # argument list really.
            %*PRAGMAS<soft> := $on;
        }
        elsif $name eq 'precompilation' {
            if $on {
                self.throw($/, 'X::Pragma::CannotWhat', :what<use>, :$name);
            }
            if self.is_precompilation_mode {
                my $automatic_precomp := nqp::ifnull(nqp::atkey(nqp::getenvhash, 'RAKUDO_PRECOMP_WITH'), 0);
                nqp::exit(0) if $automatic_precomp;
                self.throw($/, 'X::Pragma::CannotPrecomp');
            }
            # no further action needed
        }
        elsif $name eq 'invocant' || $name eq 'parameters' || $name eq 'variables' || $name eq 'attributes' {
            unless $on {
                self.throw($/, 'X::Pragma::CannotWhat', :what<no>, :$name);
            }
            unless nqp::defined($arglist) {
                self.throw($/, 'X::Pragma::MustOneOf', :$name, :alternatives(':D, :U or :_'));
            }

            my $Pair := self.find_symbol(['Pair']);
            my $Bool := self.find_symbol(['Bool']);
            my $type;
            for $arglist -> $arg {
                if $type {
                    self.throw($/, 'X::Pragma::OnlyOne', :$name);
                }
                elsif nqp::istype($arg,$Pair) {
                    my $value := $arg.value;
                    if nqp::istype($value,$Bool) && $value {
                        $type := $arg.key;
                        if $type eq 'D' || $type eq 'U' {
                            %*PRAGMAS{$name} := $type;
                            next;
                        }
                        elsif $type eq '_' {
                            nqp::deletekey(%*PRAGMAS,$name);
                            next;
                        }
                    }
                    self.throw($/, 'X::InvalidTypeSmiley', :name($arg.key));
                }
                self.throw($/, 'X::Pragma::UnknownArg', :$name, :$arg);
            }
        }
        elsif $name eq 'lib' {
            unless $on {
                self.throw($/, 'X::Pragma::CannotWhat', :what<no>, :$name);
            }
            if self.is_precompilation_mode {
                self.throw($/, 'X::Pragma::CannotPrecomp', :what("'use lib'") );
            }
            elsif $*PKGDECL {
                self.throw($/, 'X::Package::UseLib', :what($*PKGDECL) );
            }
            if nqp::islist($arglist) {
                my $registry := self.find_symbol(['CompUnit', 'RepositoryRegistry']);
                for $arglist -> $arg {
                    $registry.use-repository($registry.repository-for-spec($arg));
                }
            }
            else {
                self.throw($/, 'X::LibNone');
            }
        }
        else {
            $RMD("  '$name' is not a valid pragma") if $RMD;
            return 0;                        # go try module
        }

        $RMD("Successfully handled '$name' as a pragma") if $RMD;
        1;
    }

    method DEPRECATED($/,$alternative,$from,$removed,:$what,:$line,:$file) {
        my $DEPRECATED := self.find_symbol(['&DEPRECATED']);
        unless nqp::isnull($DEPRECATED) {
            $DEPRECATED($alternative,$from,$removed,
              :$what,
              :file($file // self.current_file),
              :line($line // self.current_line($/)),
            );
        }
    }

    method handle_OFTYPE_for_pragma($/, $pragma) {
        my $colonpairs := $*OFTYPE<colonpairs>;
        if $colonpairs && ($colonpairs.ast<D> || $colonpairs.ast<U> || $colonpairs.ast<_>) {
            # This is handled in typename and value:sym<name> directly.
        }

        # no specific smiley found, check for default
        elsif %*PRAGMAS{$pragma} -> $default {
            my class FakeOfType { has $!type; method ast() { $!type } }
            if $default ne '_' {
                if $*OFTYPE {
                    $*OFTYPE.make(
                        self.create_definite_type($*W.resolve_mo($/, 'definite'),
                            $*OFTYPE.ast, $default eq 'D')
                    )
                }
                else {
                    $*OFTYPE := FakeOfType.new(type => self.create_definite_type(
                        $*W.resolve_mo($/, 'definite'), self.find_symbol(['Any']),
                        $default eq 'D'));
                }
            }
        }
        1
    }

    method current_file() {
        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isnull($file) {
            $file := '<unknown file>';
        }
        elsif !nqp::eqat($file,'/',0) && !nqp::eqat($file,'-',0) && !nqp::eqat($file,':',1) {
            $file := nqp::cwd ~ '/' ~ $file;
        }
        $file;
    }

    method current_line($/) {
        HLL::Compiler.lineof($/.orig,$/.from,:cache(1));
    }

    method arglist($/) {
        my $arglist;
        if $<arglist><EXPR> -> $expr {
            $arglist := self.compile_time_evaluate($/,$expr.ast).List.FLATTENABLE_LIST;
        }
        $arglist;
    }

    method do_pragma_or_load_module($/,$use,$thisname?) {
        my $name;
        my %cp;
        my $arglist;

        my $RMD := self.RAKUDO_MODULE_DEBUG;

        if $thisname {
            $name := $thisname;
        }
        else {
            my $lnd  := self.dissect_longname($<module_name><longname>);
            $name    := $lnd.name;
            %cp      := $lnd.colonpairs_hash($use ?? 'use' !! 'no');
            $arglist := self.arglist($/);
        }

        unless %cp {
            if self.do_pragma($/,$name,$use,$arglist) { return }
        }

        if $use {
            $RMD("Attempting to load '$name'") if $RMD;
            my $comp_unit := self.load_module($/, $name, %cp, self.cur_lexpad);
            $RMD("Performing imports for '$name'") if $RMD;
            self.do_import($/, $comp_unit.handle, $name, $arglist);
            self.import_EXPORTHOW($/, $comp_unit.handle);
            $RMD("Imports for '$name' done") if $RMD;
        }
        else {
            nqp::die("Don't know how to 'no $name' just yet");
        }
    }

    # Loads a module immediately, and also makes sure we load it
    # during the deserialization.
    method load_module_early($/, $module_name, %opts, $cur_GLOBALish) {
        my $RMD := self.RAKUDO_MODULE_DEBUG;
        $RMD("  Early loading '$module_name'") if $RMD;

        # Immediate loading.
        my $line   := self.current_line($/);
        my $module := nqp::gethllsym('perl6', 'ModuleLoader').load_module($module_name, %opts,
            $cur_GLOBALish, :$line);

        # During deserialization, ensure that we get this module loaded.
        if self.is_precompilation_mode() {
            $RMD("  Pre-compiling '$module_name'") if $RMD;
            my $opt_hash := QAST::Op.new( :op('hash') );
            for %opts {
                self.add_object($_.value);
                $opt_hash.push(QAST::SVal.new( :value($_.key) ));
                my $Str := self.find_symbol(['Str']);
                if nqp::isstr($_.value) || nqp::istype($_.value, $Str) {
                    $opt_hash.push(QAST::SVal.new( :value($_.value) ));
                }
                else {
                    $opt_hash.push(QAST::WVal.new( :value($_.value) ));
                }
            }
            self.add_load_dependency_task(:deserialize_ast(QAST::Stmts.new(
                self.perl6_module_loader_code(),
                QAST::Op.new(
                   :op('callmethod'), :name('load_module'),
                   QAST::Op.new( :op('getcurhllsym'),
                        QAST::SVal.new( :value('ModuleLoader') ) ),
                   QAST::SVal.new( :value($module_name) ),
                   $opt_hash,
                   QAST::IVal.new(:value($line), :named('line'))
                ))));
        }

        return $module;
    }

    # Loads a module immediately, and also makes sure we load it
    # during the deserialization.
    method load_module($/, $module_name, %opts, $cur_GLOBALish) {
        my $RMD := self.RAKUDO_MODULE_DEBUG;
        $RMD("  Late loading '$module_name'") if $RMD;

        # Immediate loading.
        my $line   := self.current_line($/);
        my $true := self.find_symbol(['True']);
        my $spec := self.find_symbol(['CompUnit', 'DependencySpecification']).new(
            :short-name($module_name),
            :from(%opts<from> // 'Perl6'),
            :auth-matcher(%opts<auth> // $true),
            :version-matcher(%opts<ver> // $true),
            :source-line-number($line)
        );
        self.add_object($spec);
        my $registry := self.find_symbol(['CompUnit', 'RepositoryRegistry']);
        my $comp_unit := $registry.head.need($spec);
        my $globalish := $comp_unit.handle.globalish-package;
        nqp::gethllsym('perl6','ModuleLoader').merge_globals_lexically(self, $cur_GLOBALish, $globalish);

        return $comp_unit;
    }

    # Uses the NQP module loader to load Perl6::ModuleLoader, which
    # is a normal NQP module.
    method perl6_module_loader_code() {
        QAST::Stmt.new(
            QAST::Op.new(
                :op('loadbytecode'),
                QAST::VM.new(
                    :jvm(QAST::SVal.new( :value('ModuleLoader.class') )),
                    :moar(QAST::SVal.new( :value('ModuleLoader.moarvm') ))
                )),
            QAST::Op.new(
                :op('callmethod'), :name('load_module'),
                QAST::Op.new(
                    :op('gethllsym'),
                    QAST::SVal.new( :value('nqp') ),
                    QAST::SVal.new( :value('ModuleLoader') )
                ),
                QAST::SVal.new( :value('Perl6::ModuleLoader') )
            ))
    }

    # Imports symbols from the specified stash into the current lexical scope.
    method import($/, %stash, $source_package_name) {
        # What follows is a two-pass thing for historical reasons.
        my $target := self.cur_lexpad();

        # First pass: QAST::Block symbol table installation. Also detect any
        # outright conflicts, and handle any situations where we need to merge.
        my %to_install;
        my @clash;
        my @clash_onlystar;
        for %stash {
            if $target.symbol($_.key) -> %sym {
                # There's already a symbol. However, we may be able to merge
                # if both are multis and have onlystar dispatchers.
                my $installed := %sym<value>;
                my $foreign := $_.value;
                if $installed =:= $foreign {
                    next;
                }
                if nqp::can($installed, 'is_dispatcher') && $installed.is_dispatcher
                && nqp::can($foreign, 'is_dispatcher') && $foreign.is_dispatcher {
                    # Both dispatchers, but are they onlystar? If so, we can
                    # go ahead and merge them.
                    if $installed.onlystar && $foreign.onlystar {
                        # Replace installed one with a derived one, to avoid any
                        # weird action at a distance.
                        $installed := self.derive_dispatcher($installed);
                        self.install_lexical_symbol($target, $_.key, $installed, :clone(1));

                        # Incorporate dispatchees of foreign proto, avoiding
                        # duplicates.
                        my %seen;
                        for $installed.dispatchees {
                            %seen{$_.static_id} := $_;
                        }
                        for $foreign.dispatchees {
                            unless nqp::existskey(%seen, $_.static_id) {
                                self.add_dispatchee_to_proto($installed, $_);
                            }
                        }
                    }
                    else {
                        nqp::push(@clash_onlystar, $_.key);
                    }
                }
                else {
                    nqp::push(@clash, $_.key);
                }
            }
            else {
                $target.symbol($_.key, :scope('lexical'), :value($_.value));
                $target[0].push(QAST::Var.new(
                    :scope('lexical'), :name($_.key), :decl('static'), :value($_.value)
                ));
                %to_install{$_.key} := $_.value;
            }
        }

        if +@clash_onlystar {
            self.throw($/, 'X::Import::OnlystarProto',
                symbols             => @clash_onlystar,
                source-package-name => $source_package_name,
            );
        }

        if +@clash {
            self.throw($/, 'X::Import::Redeclaration',
                symbols             => @clash,
                source-package-name => $source_package_name,
            );
        }

        # Second pass: make sure installed things are in an SC and handle
        # categoricals.
        for %to_install {
            my $v := $_.value;
            self.add_object_if_no_sc($v);
            my $categorical := match($_.key, /^ '&' (\w+) [ ':<' (.+) '>' | ':«' (.+) '»' ] $/);
            if $categorical {
                $/.CURSOR.add_categorical(~$categorical[0], ~$categorical[1],
                    ~$categorical[0] ~ self.canonicalize_pair('sym',$categorical[1]),
                    nqp::substr($_.key, 1), $v);
            }
        }
    }

    # Installs something package-y in the right place, creating the nested
    # packages as needed.
    method install_package($/, @name_orig, $scope, $pkgdecl, $package, $outer, $symbol) {
        if $scope eq 'anon' || +@name_orig == 0 { return 1 }
        my @parts := nqp::clone(@name_orig);
        my $name  := @parts.pop();
        my $create_scope := $scope;
        my $cur_pkg := $package;
        my $cur_lex := $outer;

        # Can only install packages as our or my scope.
        $create_scope := "our" if $create_scope eq 'unit';
        unless $create_scope eq 'my' || $create_scope eq 'our' {
            self.throw($/, 'X::Declaration::Scope',
                scope       => $*SCOPE,
                declaration => $pkgdecl,
            );
        }

        # Can't install our-scoped packages inside parametric types.
        if $create_scope eq 'our' {
            if nqp::can($cur_pkg.HOW, 'archetypes') && $cur_pkg.HOW.archetypes.parametric {
                self.throw($/, 'X::Declaration::OurScopeInRole',
                    declaration => $pkgdecl,
                );
            }
        }

        # If we have a multi-part name, see if we know the opening
        # chunk already. If so, use it for that part of the name.
        my $longname := '';
        if +@parts {
            try {
                $cur_pkg := self.find_symbol([@parts[0]], :upgrade_to_global($create_scope ne 'my'));
                $cur_lex := 0;
                $create_scope := 'our';
                $longname := @parts.shift();
            }
        }

        # Chase down the name, creating stub packages as needed.
        while +@parts {
            my $part := @parts.shift;
            $longname := $longname ?? $longname ~ '::' ~ $part !! $part;
            if nqp::existskey($cur_pkg.WHO, $part) {
                $cur_pkg := ($cur_pkg.WHO){$part};
            }
            else {
                my $new_pkg := self.pkg_create_mo($/, self.resolve_mo($/, 'package'),
                    :name($longname));
                self.pkg_compose($/, $new_pkg);
                if $create_scope eq 'my' || $cur_lex {
                    self.install_lexical_symbol($cur_lex, $part, $new_pkg);
                }
                if $create_scope eq 'our' {
                    self.install_package_symbol_unchecked($cur_pkg, $part, $new_pkg);
                }
                $cur_pkg := $new_pkg;
                $create_scope := 'our';
                $cur_lex := 0;
            }
        }

        # Install final part of the symbol.
        if $create_scope eq 'my' || $cur_lex {
            # upgrade a lexically imported package stub to package scope if it exists
            try { self.find_symbol([$name], :upgrade_to_global); }

            self.install_lexical_symbol($cur_lex, $name, $symbol);
        }
        if $create_scope eq 'our' {
            if nqp::existskey($cur_pkg.WHO, $name) {
                self.steal_WHO($symbol, ($cur_pkg.WHO){$name});
            }
            self.install_package_symbol_unchecked($cur_pkg, $name, $symbol);
        }

        1;
    }

    # If we declare class A::B { }, then class A { }, then A.WHO must be the
    # .WHO we already created for the stub package A.
    method steal_WHO($thief, $victim) {
        nqp::setwho($thief, $victim.WHO);
    }

    # Installs a lexical symbol. Takes a QAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, str $name, $obj, :$clone) {
        # Install the object directly as a block symbol.
        if nqp::isnull(nqp::getobjsc($obj)) {
            self.add_object($obj);
        }
        if $block.symbol($name) {
            for @($block[0]) {
                if nqp::istype($_, QAST::Var) && $_.name eq $name {
                    $_.decl('static');
                    $_.value($obj);
                    last;
                }
            }
        }
        else {
            $block[0].push(QAST::Var.new(
                :scope('lexical'), :name($name), :decl('static'), :value($obj)
            ));
        }
        $block.symbol($name, :scope('lexical'), :value($obj));

        # Add a clone if needed.
        if $clone {
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($name), :scope('lexical') ),
                QAST::Op.new(
                    :op('p6capturelex'),
                    QAST::Op.new(
                        :op('callmethod'), :name('clone'),
                        QAST::Var.new( :name($name), :scope('lexical') )
                    ))));
        }
    }

    # Installs a lexical symbol. Takes a QAST::Block object, name and
    # the type of container to install.
    method install_lexical_container($block, str $name, %cont_info, $descriptor, :$scope, :$package, :$cont = self.build_container_and_add_to_sc(%cont_info, $descriptor)) {
        # Add to block, if needed. Note that it doesn't really have
        # a compile time value.
        my $var;
        if $block.symbol($name) {
            for @($block[0]) {
                if nqp::istype($_, QAST::Var) && $_.name eq $name {
                    $var := $_;
                    $var.returns(%cont_info<bind_constraint>);
                    last;
                }
            }
        }
        else {
            $var := QAST::Var.new(
                :scope('lexical'), :name($name), :decl('var'),
                :returns(%cont_info<bind_constraint>)
            );
            $block[0].unshift($var);
        }
        $block.symbol($name, :scope('lexical'), :type(%cont_info<bind_constraint>), :descriptor($descriptor));

        # If it's a native type, no container as we inline natives straight
        # into registers. Do need to take care of initial value though.
        my $prim := %cont_info<sigil> eq '$' && nqp::objprimspec($descriptor.of);
        if $prim {
            if $scope eq 'state' { nqp::die("Natively typed state variables not yet implemented") }
            if $prim == 1 {
                $block[0].push(QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    QAST::IVal.new( :value(0) ) ))
            }
            elsif $prim == 2 {
                $block[0].push(QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    QAST::Op.new( :op('nan') )));
            }
            elsif $prim == 3 {
                $block[0].push(QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    QAST::SVal.new( :value('') ) ))
            }
            return nqp::null();
        }

        $block.symbol($name, :value($cont));
        self.install_package_symbol_unchecked($package, $name, $cont) if $scope eq 'our';

        # Tweak var to have container.
        $var.value($cont);
        $var.decl($scope eq 'state' ?? 'statevar' !! 'contvar');

        # Evaluate to the container.
        $cont
    }

    # Creates a new container descriptor and adds it to the SC.
    method create_container_descriptor($of, $rw, $name, $default = $of, $dynamic = nqp::chars($name) > 2 && nqp::eqat($name, '*', 1)) {
        my $cd_type := self.find_symbol(['ContainerDescriptor']);
        my $cd := $cd_type.new( :$of, :$rw, :$name, :$default, :$dynamic );
        self.add_object($cd);
        $cd
    }

    # Builds a container.
    method build_container(%cont_info, $descriptor) {
        my $cont;
        my $cont_type := %cont_info<container_type>;
        if %cont_info<build_ast> {
            $cont := $cont_type;
        }
        elsif nqp::istype($cont_type, self.find_symbol(['Scalar'])) {
            $cont := nqp::create($cont_type);
            nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
            if nqp::existskey(%cont_info, 'scalar_value') {
                nqp::bindattr($cont, %cont_info<container_base>, '$!value',
                    %cont_info<scalar_value>);
            }
        }
        elsif nqp::istype($cont_type, self.find_symbol(['Array'])) ||
              nqp::istype($cont_type, self.find_symbol(['Hash'])) {
            $cont := nqp::create($cont_type);
            nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
        }
        else {
            $cont := $cont_type.new;
            try nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
        }
        $cont
    }

    # Builds a container and adds it to the SC.
    method build_container_and_add_to_sc(%cont_info, $descriptor) {
        my $cont := self.build_container(%cont_info, $descriptor);
        self.add_object($cont);
        $cont;
    }

    # Given a sigil and the value type specified, works out the
    # container type (what should we instantiate and bind into the
    # attribute/lexpad), bind constraint (what could we bind to this
    # slot later), and if specified a constraint on the inner value
    # and a default value.
    method container_type_info($/, $sigil, @value_type, @cont_type, $shape?, :@post) {
        my %info;
        %info<sigil> := $sigil;

        @value_type[0] := nqp::decont(@value_type[0]) if @value_type;
        @cont_type[0] := nqp::decont(@cont_type[0]) if @cont_type;

        for @post -> $con {
            @value_type[0] := self.create_subset(self.resolve_mo($/, 'subset'),
                @value_type ?? @value_type[0] !! self.find_symbol(['Mu']),
                $con);
        }
        if $sigil eq '@' {
            if @cont_type {
                %info<bind_constraint> := @cont_type[0];
                %info<container_base> := @cont_type[0];
            }
            else {
                %info<bind_constraint> := self.find_symbol(['Positional']);
                my $base_type_name     := nqp::objprimspec(@value_type[0]) ?? 'array' !! 'Array';
                %info<container_base>  := self.find_symbol([$base_type_name]);
            }
            if @value_type {
                my $vtype              := @value_type[0];
                %info<container_type>  := self.parameterize_type_with_args($/,
                    %info<container_base>, [$vtype], nqp::hash());
                %info<bind_constraint> := self.parameterize_type_with_args($/,
                    %info<bind_constraint>, [$vtype], nqp::hash());
                %info<value_type>      := $vtype;
                %info<default_value>   := $vtype;
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := self.find_symbol(['Mu']);
                %info<default_value>  := self.find_symbol(['Any']);
            }
            if $shape || @cont_type {
                my $ast := QAST::Op.new(
                    :op('callmethod'), :name('new'),
                    QAST::WVal.new( :value(%info<container_type>) )
                );
                if $shape {
                    my $shape_ast := $shape[0].ast;
                    $shape_ast.named('shape');
                    $ast.push($shape_ast);
                }
                %info<build_ast> := $ast;
            }
        }
        elsif $sigil eq '%' {
            if @cont_type {
                %info<container_base>  := @cont_type[0];
                %info<bind_constraint> := @cont_type[0];
            }
            else {
                %info<container_base>  := self.find_symbol(['Hash']);
                %info<bind_constraint> := self.find_symbol(['Associative']);
            }
            if $shape {
                @value_type[0] := self.find_symbol(['Any']) unless +@value_type;
                my $shape_ast := $shape[0].ast;
                if $shape_ast.isa(QAST::Stmts) {
                    if +@($shape_ast) == 1 {
                        if $shape_ast[0].has_compile_time_value {
                            @value_type[1] := $shape_ast[0].compile_time_value;
                        } elsif (my $op_ast := $shape_ast[0]).isa(QAST::Op) {
                            if $op_ast.op eq "call" && +@($op_ast) == 2 {
                                if !nqp::isconcrete($op_ast[0].value) && !nqp::isconcrete($op_ast[1].value) {
                                    self.throw($/, 'X::Comp::NYI',
                                        feature => "coercive type declarations");
                                }
                            }
                        } else {
                            self.throw($/, "X::Comp::AdHoc",
                                payload => "Invalid hash shape; type expected");
                        }
                    } elsif +@($shape_ast) > 1 {
                        self.throw($/, 'X::Comp::NYI',
                            feature => "multidimensional shaped hashes");
                    }
                } else {
                    self.throw($/, "X::Comp::AdHoc",
                        payload => "Invalid hash shape; type expected");
                }
            }
            if @value_type {
                if nqp::objprimspec(@value_type[0]) {
                    self.throw($/, 'X::Comp::NYI',
                      feature => "native value types for hashes");
                }
                %info<container_type>  := self.parameterize_type_with_args($/,
                    %info<container_base>, @value_type, nqp::hash());
                %info<bind_constraint> := self.parameterize_type_with_args($/,
                    %info<bind_constraint>, @value_type, nqp::hash());
                %info<value_type>      := @value_type[0];
                %info<default_value>   := @value_type[0];
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := self.find_symbol(['Mu']);
                %info<default_value>  := self.find_symbol(['Any']);
            }
            if @cont_type {
                %info<build_ast> := QAST::Op.new(
                    :op('callmethod'), :name('new'),
                    QAST::WVal.new( :value(%info<container_type>) )
                );
            }
        }
        elsif $sigil eq '&' {
            if @cont_type {
                self.throw($/, 'X::NYI', :feature('is trait on &-sigil variable'));
            }
            %info<container_base>  := self.find_symbol(['Scalar']);
            %info<container_type>  := %info<container_base>;
            %info<bind_constraint> := self.find_symbol(['Callable']);
            if @value_type {
                %info<bind_constraint> := self.parameterize_type_with_args($/,
                    %info<bind_constraint>, [@value_type[0]], nqp::hash());
            }
            %info<value_type>     := %info<bind_constraint>;
            %info<default_value>  := self.find_symbol(['Callable']);
            %info<scalar_value>   := self.find_symbol(['Callable']);
        }
        else {
            if @cont_type {
                self.throw($/, 'X::NYI', :feature('is trait on $-sigil variable'));
            }
            %info<container_base>     := self.find_symbol(['Scalar']);
            %info<container_type>     := %info<container_base>;
            if @value_type {
                %info<bind_constraint> := @value_type[0];
                %info<value_type>      := @value_type[0];
                %info<default_value>   := @value_type[0];
            }
            else {
                %info<bind_constraint> := self.find_symbol(['Mu']);
                %info<value_type>      := self.find_symbol(['Mu']);
                %info<default_value>   := self.find_symbol(['Any']);
            }
            %info<scalar_value> := %info<default_value>;
        }
        %info
    }

    # Installs one of the magical lexicals ($_, $/ and $!). Uses a cache to
    # avoid massive duplication of containers and container descriptors.
    method install_lexical_magical($block, $name) {
        my %magical_cds := self.context().magical_cds();

        if nqp::existskey(%magical_cds, $name) {
            my $mcd := nqp::atkey(%magical_cds, $name);
            self.install_lexical_container($block, $name, $mcd[0], $mcd[1], :cont($mcd[2]));
        }
        else {
            my $Mu     := self.find_symbol(['Mu']);
            my $WHAT   := self.find_symbol([ $name eq '$_' ?? 'Any' !! 'Nil' ]);
            my $Scalar := self.find_symbol(['Scalar']);

            my %info := nqp::hash(
                'container_base',  $Scalar,
                'container_type',  $Scalar,
                'bind_constraint', $Mu,
                'value_type',      $Mu,
                'default_value',   $WHAT,
                'scalar_value',    $WHAT,
            );
            my $desc :=
              self.create_container_descriptor($Mu, 1, $name, $WHAT, 1);

            my $cont := self.build_container_and_add_to_sc(%info, $desc);

            %magical_cds{$name} := [%info, $desc, $cont];
            self.install_lexical_container($block, $name, %info, $desc, :cont($cont));
        }
    }

    # Builds PAST that constructs a container.
    method build_container_past(%cont_info, $descriptor) {
        # Create container and set descriptor.
        my $tmp := QAST::Node.unique('cont');
        my $cont_code := QAST::Stmts.new(
            :resultchild(0),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                QAST::Op.new(
                    :op('create'),
                    QAST::WVal.new( :value(%cont_info<container_type>) ))),
            QAST::Op.new(
                :op('bindattr'),
                QAST::Var.new( :name($tmp), :scope('local') ),
                QAST::WVal.new( :value(%cont_info<container_base>) ),
                QAST::SVal.new( :value('$!descriptor') ),
                QAST::WVal.new( :value($descriptor) )));

        # Default contents, if applicable (note, slurpy param as we can't
        # use definedness here, as it's a type object we'd be checking).
        if nqp::existskey(%cont_info, 'scalar_value') {
            $cont_code.push(QAST::Op.new(
                :op('bindattr'),
                QAST::Var.new( :name($tmp), :scope('local') ),
                QAST::WVal.new( :value(%cont_info<container_base>) ),
                QAST::SVal.new( :value('$!value') ),
                QAST::WVal.new( :value(%cont_info<scalar_value>) )));
        }

        $cont_code
    }

    # Hunts through scopes to find the type of a lexical.
    method find_lexical_container_type(str $name) {
        self.context().find_lexical_container_type($name)
    }

    # Hunts through scopes to find a lexical and returns if it is
    # known to be read-only.
    method is_lexical_marked_ro(str $name) {
        self.context().is_lexical_marked_ro($name)
    }

    # Installs a symbol into the package.
    method install_package_symbol($/, $package, $name, $obj, $declaration) {
        if nqp::can($package.HOW, 'archetypes') && $package.HOW.archetypes.parametric {
            self.throw($/, 'X::Declaration::OurScopeInRole', :$declaration);
        }
        self.install_package_symbol_unchecked($package, $name, $obj);
    }
    method install_package_symbol_unchecked($package, $name, $obj) {
        ($package.WHO){$name} := $obj;
        1;
    }

    method auto_declare_var($var) {
        my $varast     := $var.ast;
        my $name       := $varast.name;
        my $BLOCK      := self.cur_lexpad();
        self.handle_OFTYPE_for_pragma($/,'variables');
        my %cont_info  := self.container_type_info(NQPMu, $var<sigil>,
            $*OFTYPE ?? [$*OFTYPE.ast] !! [], []);
        my $descriptor := self.create_container_descriptor(%cont_info<value_type>, 1, $name);

        self.install_lexical_container($BLOCK, $name, %cont_info, $descriptor,
            :scope('our'), :package($*PACKAGE));

        if $varast.isa(QAST::Var) {
            $varast.scope('lexical');
            $varast.returns(%cont_info<bind_constraint>);
            if %cont_info<bind_constraint>.HOW.archetypes.generic {
                $varast := QAST::Op.new(
                    :op('callmethod'), :name('instantiate_generic'),
                    QAST::Op.new( :op('p6var'), $varast ),
                    QAST::Op.new( :op('curlexpad') ));
            }

            $BLOCK[0].push(QAST::Op.new( :op('bind'),
                $varast,
                self.symbol_lookup([$name], NQPMu, :package_only(1), :lvalue(1))
            ));
        }
    }

    # Creates a parameter object.
    method create_parameter($/, %param_info) {
        # Create parameter object now.
        my $par_type  := self.find_symbol(['Parameter'], :setting-only);
        my $parameter := nqp::create($par_type);
        self.add_object($parameter);

        # Calculate flags.
        my int $flags := 0;
        if %param_info<optional> {
            $flags := $flags + $SIG_ELEM_IS_OPTIONAL;
        }
        if %param_info<is_invocant> {
            $flags := $flags + $SIG_ELEM_INVOCANT;
        }
        if %param_info<is_multi_invocant> {
            $flags := $flags + $SIG_ELEM_MULTI_INVOCANT;
        }
        if %param_info<is_rw> {
            $flags := $flags + $SIG_ELEM_IS_RW;
        }
        if %param_info<is_raw> {
            $flags := $flags + $SIG_ELEM_IS_RAW;
        }
        if %param_info<pos_onearg> {
            $flags := $flags + $SIG_ELEM_SLURPY_ONEARG;
        }
        if %param_info<is_capture> {
            $flags := $flags + $SIG_ELEM_IS_CAPTURE + $SIG_ELEM_IS_RAW;
        }
        if %param_info<undefined_only> {
            $flags := $flags + $SIG_ELEM_UNDEFINED_ONLY;
        }
        if %param_info<defined_only> {
            $flags := $flags + $SIG_ELEM_DEFINED_ONLY;
        }
        if %param_info<pos_slurpy> {
            $flags := $flags + $SIG_ELEM_SLURPY_POS;
        }
        if %param_info<named_slurpy> {
            $flags := $flags + $SIG_ELEM_SLURPY_NAMED;
        }
        if %param_info<pos_lol> {
            $flags := $flags + $SIG_ELEM_SLURPY_LOL;
        }
        if %param_info<bind_attr> {
            $flags := $flags + $SIG_ELEM_BIND_PRIVATE_ATTR;
        }
        if %param_info<bind_accessor> {
            $flags := $flags + $SIG_ELEM_BIND_PUBLIC_ATTR;
        }
        if %param_info<sigil> eq '@' {
            $flags := $flags + $SIG_ELEM_ARRAY_SIGIL;
        }
        elsif %param_info<sigil> eq '%' {
            $flags := $flags + $SIG_ELEM_HASH_SIGIL;
        }
        elsif %param_info<sigil> eq '&' {
            $flags := $flags + $SIG_ELEM_CODE_SIGIL;
        }
        if %param_info<default_from_outer> {
            $flags := $flags + $SIG_ELEM_DEFAULT_FROM_OUTER;
        }
        if %param_info<nominal_generic> {
            $flags := $flags + $SIG_ELEM_NOMINAL_GENERIC;
        }
        if %param_info<default_is_literal> {
            $flags := $flags + $SIG_ELEM_DEFAULT_IS_LITERAL;
        }
        my $primspec := nqp::objprimspec(%param_info<nominal_type>);
        if $primspec == 1 {
            $flags := $flags + $SIG_ELEM_NATIVE_INT_VALUE;
        }
        elsif $primspec == 2 {
            $flags := $flags + $SIG_ELEM_NATIVE_NUM_VALUE;
        }
        elsif $primspec == 3 {
            $flags := $flags + $SIG_ELEM_NATIVE_STR_VALUE;
        }

        # Populate it.
        if nqp::existskey(%param_info, 'variable_name') {
            nqp::bindattr_s($parameter, $par_type, '$!variable_name', %param_info<variable_name>);
        }
        nqp::bindattr($parameter, $par_type, '$!nominal_type', %param_info<nominal_type>);
        nqp::bindattr_i($parameter, $par_type, '$!flags', $flags);
        if %param_info<named_names> {
            my @names := %param_info<named_names>;
            nqp::bindattr($parameter, $par_type, '@!named_names', @names);
        }
        if %param_info<type_captures> {
            my @type_names := %param_info<type_captures>;
            nqp::bindattr($parameter, $par_type, '@!type_captures', @type_names);
        }
        if %param_info<post_constraints> {
            nqp::bindattr($parameter, $par_type, '@!post_constraints',
                %param_info<post_constraints>);
        }
        if nqp::existskey(%param_info, 'default_value') {
            nqp::bindattr($parameter, $par_type, '$!default_value', %param_info<default_value>);
        }
        if nqp::existskey(%param_info, 'container_descriptor') {
            nqp::bindattr($parameter, $par_type, '$!container_descriptor', %param_info<container_descriptor>);
        }
        if nqp::existskey(%param_info, 'attr_package') {
            nqp::bindattr($parameter, $par_type, '$!attr_package', %param_info<attr_package>);
        }
        if nqp::existskey(%param_info, 'sub_signature') {
            nqp::bindattr($parameter, $par_type, '$!sub_signature', %param_info<sub_signature>);
        }
        if nqp::existskey(%param_info, 'coerce_type') {
            $parameter.set_coercion(%param_info<coerce_type>);
        }

        if nqp::existskey(%param_info, 'dummy') {
            my $dummy := %param_info<dummy>;
            my $why   := $dummy.WHY;
            if $why {
                $parameter.set_why($why);
            }
        }

        if nqp::existskey(%param_info, 'docs') {
            Perl6::Pod::document($/, $parameter, %param_info<docs>, :leading);
            if ~%param_info<docs> ne '' {
                %param_info<docs>.set_docee($parameter);
            }
        }
        $*PRECEDING_DECL := $parameter;
        # Return created parameter.
        $parameter
    }

    # Create Parameter objects, along with container descriptors if needed,
    # for all of the given parameter descriptors. Then make a Signature
    # object wrapping them.
    method create_signature_and_params($/, %signature_info, $lexpad, $default_type_name,
            :$no_attr_check, :$rw, :$method, :$invocant_type) {
        # If it's a method, add auto-slurpy.
        my $*PRECEDING_DECL;      # prevent parameter(s) created here from
        my $*PRECEDING_DECL_LINE; # clobbering code declaration for trailing comments

        my @params := %signature_info<parameters>;
        if $method {
            unless @params[0]<is_invocant> {
                @params.unshift(hash(
                    nominal_type => $invocant_type,
                    is_invocant => 1,
                    is_multi_invocant => 1
                ));
            }
            unless has_named_slurpy_or_capture(@params) {
                unless nqp::can($*PACKAGE.HOW, 'hidden') && $*PACKAGE.HOW.hidden($*PACKAGE) {
                    @params.push(hash(
                        variable_name => '%_',
                        nominal_type => self.find_symbol(['Mu']),
                        named_slurpy => 1,
                        is_multi_invocant => 1,
                        sigil => '%'
                    ));
                    $lexpad[0].unshift(QAST::Var.new( :name('%_'), :scope('lexical'), :decl('var') ));
                    $lexpad.symbol('%_', :scope('lexical'));
                }
            }
        }

        # Walk parameters, setting up parameter objects.
        my $default_type := self.find_symbol([$default_type_name]);
        my @param_objs;
        my %seen_names;
        for @params {
            # Set default nominal type, if we lack one.
            unless nqp::existskey($_, 'nominal_type') {
                $_<nominal_type> := $default_type;
            }

            # Default to rw if needed.
            if $rw {
                $_<is_rw> := 1;
            }

            # Check we don't have duplicated named parameter names.
            if $_<named_names> {
                for $_<named_names> {
                    if %seen_names{$_} {
                        self.throw($/, ['X', 'Signature', 'NameClash'],
                            name => $_
                        );
                    }
                    %seen_names{$_} := 1;
                }
            }

            # If it's !-twigil'd, ensure the attribute it mentions exists unless
            # we're in a context where we should not do that.
            if $_<bind_attr> && !$no_attr_check {
                self.get_attribute_meta_object($/, $_<variable_name>, QAST::Var.new);
            }

            # If we have a sub-signature, create that.
            if nqp::existskey($_, 'sub_signature_params') {
                $_<sub_signature> := self.create_signature_and_params($/,
                    $_<sub_signature_params>, $lexpad, $default_type_name);
            }

            # Add variable as needed.
            my $varname := $_<variable_name>;
            if $varname {
                my %sym := $lexpad.symbol($varname);
                if +%sym && !nqp::existskey(%sym, 'descriptor') {
                    $_<container_descriptor> := self.create_container_descriptor(
                        $_<nominal_type>, $_<is_rw> ?? 1 !! 0, $varname);
                    $lexpad.symbol($varname, :descriptor($_<container_descriptor>));
                }
            }

            # Create parameter object and apply any traits.
            my $param_obj := self.create_parameter($/, $_);
            self.apply_traits($_<traits>, $param_obj) if $_<traits>;

            # If it's natively typed and we got "is rw" set, need to mark the
            # container as being a lexical ref.
            if $varname && nqp::objprimspec($_<nominal_type>) {
                my $param_type := self.find_symbol(['Parameter'], :setting-only);
                my int $flags := nqp::getattr_i($param_obj, $param_type, '$!flags');
                if $flags +& $SIG_ELEM_IS_RW {
                    for @($lexpad[0]) {
                        if nqp::istype($_, QAST::Var) && $_.name eq $varname {
                            $_.scope('lexicalref');
                        }
                    }
                }
                elsif !($flags +& $SIG_ELEM_IS_COPY) {
                    $lexpad.symbol($varname, :ro(1));
                }
            }

            # Add it to the signature.
            @param_objs.push($param_obj);
        }
        %signature_info<parameter_objects> := @param_objs;
        self.create_signature(%signature_info)
    }

    sub has_named_slurpy_or_capture(@params) {
        for @params {
            return 1 if $_<named_slurpy> || $_<is_capture>;
        }
        0
    }

    # Creates a signature object from a set of parameters.
    method create_signature(%signature_info) {
        # Create signature object now.
        my $sig_type   := self.find_symbol(['Signature'], :setting-only);
        my $signature  := nqp::create($sig_type);
        my @parameters := %signature_info<parameter_objects>;
        self.add_object($signature);

        # Set parameters.
        nqp::bindattr($signature, $sig_type, '@!params', @parameters);
        if nqp::existskey(%signature_info, 'returns') {
            nqp::bindattr($signature, $sig_type, '$!returns', %signature_info<returns>);
        }

        # Compute arity and count.
        my $p_type    := self.find_symbol(['Parameter'], :setting-only);
        my int $arity := 0;
        my int $count := 0;
        my int $i     := 0;
        my int $n     := nqp::elems(@parameters);
        while $i < $n {
            my $param := @parameters[$i];
            my int $flags := nqp::getattr_i($param, $p_type, '$!flags');
            if $flags +& ($SIG_ELEM_IS_CAPTURE +| $SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_LOL +| $SIG_ELEM_SLURPY_ONEARG) {
                $count := -1;
            }
            elsif !($flags +& $SIG_ELEM_SLURPY_NAMED) &&
                    nqp::isnull(nqp::getattr($param, $p_type, '@!named_names')) {
                $count++;
                $arity++ unless $flags +& $SIG_ELEM_IS_OPTIONAL;
            }
            $i++;
        }
        nqp::bindattr_i($signature, $sig_type, '$!arity', $arity);
        if $count == -1 {
            nqp::bindattr($signature, $sig_type, '$!count',
                self.add_constant('Num', 'num', nqp::inf()).value);
        } else {
            nqp::bindattr($signature, $sig_type, '$!count',
                self.add_constant('Int', 'int', $count).value);
        }

        # Return created signature.
        $signature
    }

    method compile_time_evaluate($/, $ast) {
        return $ast.compile_time_value if $ast.has_compile_time_value;
        my $thunk := self.create_thunk($/, $ast);
        $thunk();
    }

    # Turn a QAST tree into a code object, to be called immediately.
    method create_thunk($/, $to_thunk) {
        my $block := self.push_lexpad($/);
        $block.push($to_thunk);
        self.pop_lexpad();
        self.create_simple_code_object($block, 'Code');
    }

    # Creates a simple code object with an empty signature
    method create_simple_code_object($block, $type) {
        if $*WANTEDOUTERBLOCK {
            $*WANTEDOUTERBLOCK[0].push($block);
        }
        else {
            self.cur_lexpad()[0].push($block);
        }
        my $sig := self.create_signature(nqp::hash('parameter_objects', []));
        return self.create_code_object($block, $type, $sig);
    }

    # Creates a code object of the specified type, attached the passed signature
    # object and sets up dynamic compilation thunk.
    method create_code_object($code_past, $type, $signature, $is_dispatcher = 0, :$yada) {
        my $code := self.stub_code_object($type);
        self.attach_signature($code, $signature);
        self.finish_code_object($code, $code_past, $is_dispatcher, :yada($yada));
        self.add_phasers_handling_code($code, $code_past);
        $code
    }

    # Stubs a code object of the specified type.
    method stub_code_object($type) {
        my $type_obj := self.find_symbol([$type], :setting-only);
        my $code     := nqp::create($type_obj);
        self.context().push_code_object($code);
        self.add_object($code);
        $code
    }

    # Attaches a signature to a code object, and gives the
    # signature its backlink to the code object.
    method attach_signature($code, $signature) {
        my $code_type := self.find_symbol(['Code'], :setting-only);
        my $sig_type := self.find_symbol(['Signature'], :setting-only);
        nqp::bindattr($code, $code_type, '$!signature', $signature);
        nqp::bindattr($signature, $sig_type, '$!code', $code);
    }

    # Takes a code object and the QAST::Block for its body. Finalizes the
    # setup of the code object, including populated the @!compstuff array.
    # This contains 3 elements:
    #   0 = the QAST::Block object
    #   1 = the compiler thunk
    #   2 = the clone callback
    method finish_code_object($code, $code_past, $is_dispatcher = 0, :$yada) {
        my $fixups := QAST::Stmts.new();
        my $des    := QAST::Stmts.new();

        # Remove it from the code objects stack.
        self.context().pop_code_object();

        # Locate various interesting symbols.
        my $code_type    := self.find_symbol(['Code'], :setting-only);
        my $routine_type := self.find_symbol(['Routine'], :setting-only);

        # Attach code object to QAST node.
        $code_past.annotate('code_object', $code);

        # Associate QAST block with code object, which will ensure it is
        # fixed up as needed.
        $code_past.code_object($code);

        # Stash it under the QAST block unique ID.
        my str $cuid := $code_past.cuid();
        self.context().sub_id_to_code_object(){$cuid} := $code;

        # Create the compiler stuff array and stick it in the code object.
        # Also add clearup task to remove it again later.
        my @compstuff;
        nqp::bindattr($code, $code_type, '@!compstuff', @compstuff);
        self.context().add_cleanup_task(sub () {
            nqp::bindattr($code, $code_type, '@!compstuff', nqp::null());
        });

        # For now, install stub that will dynamically compile the code if
        # we ever try to run it during compilation.
        my $precomp;
        my $compiler_thunk := {
            # Fix up GLOBAL.
            nqp::bindhllsym('perl6', 'GLOBAL', $*GLOBALish);

            # Compile the block.
            $precomp := self.compile_in_context($code_past, $code_type);

            # Also compile the candidates if this is a proto.
            if $is_dispatcher {
                for nqp::getattr($code, $routine_type, '@!dispatchees') {
                    my $cs := nqp::getattr($_, $code_type, '@!compstuff');
                    my $past := $cs[0] unless nqp::isnull($cs);
                    if $past {
                        self.compile_in_context($past, $code_type);
                    }
                }
            }
        };
        my $stub := nqp::freshcoderef(sub (*@pos, *%named) {
            unless $precomp {
                $compiler_thunk();
            }
            $precomp(|@pos, |%named);
        });
        @compstuff[1] := $compiler_thunk;
        nqp::setcodename($stub, $code_past.name);
        nqp::bindattr($code, $code_type, '$!do', $stub);

        # Tag it as a static code ref and add it to the root code refs set.
        nqp::markcodestatic($stub);
        nqp::markcodestub($stub);
        my $code_ref_idx := self.add_root_code_ref($stub, $code_past);
        self.context().sub_id_to_sc_idx(){$cuid} := $code_ref_idx;

        # If we clone the stub, need to mark it as a dynamic compilation
        # boundary.
        if self.is_precompilation_mode() {
            @compstuff[2] := sub ($orig, $clone) {
                my $do := nqp::getattr($clone, $code_type, '$!do');
                nqp::markcodestub($do);
                self.context().add_cleanup_task(sub () {
                    nqp::bindattr($clone, $code_type, '@!compstuff', nqp::null());
                });
                self.context().add_clone_for_cuid($clone, $cuid);
            };
        }

        # Fixup will install the real thing, unless we're in a role, in
        # which case pre-comp will have sorted it out.
        unless $*PKGDECL eq 'role' {
            unless self.is_precompilation_mode() || self.is_nested() {
                $fixups.push(self.set_attribute($code, $code_type, '$!do',
                    QAST::BVal.new( :value($code_past) )));

                # If we clone the stub, then we must remember to do a fixup
                # of it also.
                @compstuff[2] := sub ($orig, $clone) {
                    self.add_object($clone);
                    self.context().add_cleanup_task(sub () {
                        nqp::bindattr($clone, $code_type, '@!compstuff', nqp::null());
                    });
                    my $tmp := $fixups.unique('tmp_block_fixup');
                    $fixups.push(QAST::Stmt.new(
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                            QAST::Op.new( :op('clone'), QAST::BVal.new( :value($code_past) ) )
                        ),
                        self.set_attribute($clone, $code_type, '$!do',
                            QAST::Var.new( :name($tmp), :scope('local') )),
                        QAST::Op.new(
                            :op('setcodeobj'),
                            QAST::Var.new( :name($tmp), :scope('local') ),
                            QAST::WVal.new( :value($clone) )
                        )));
                };

                # Also stash fixups so we can know not to do them if we
                # do dynamic compilation.
                %!code_object_fixup_list{$cuid} := $fixups;
            }

            # Stash the QAST block in the comp stuff.
            @compstuff[0] := $code_past;
        }

        # If this is a dispatcher, install dispatchee list that we can
        # add the candidates too.
        if $is_dispatcher {
            nqp::bindattr($code, $routine_type, '@!dispatchees', []);
        }

        # Set yada flag if needed.
        if $yada {
            nqp::bindattr_i($code, $routine_type, '$!yada', 1);
        }

        # If it's a routine, store the package to make backtraces nicer.
        if nqp::istype($code, $routine_type) {
            nqp::bindattr($code, $routine_type, '$!package', $*PACKAGE);
        }

        if self.is_nested() {
            $compiler_thunk();
        }
        else {
            self.add_fixup_task(:deserialize_ast($des), :fixup_ast($fixups));
        }
        $code;
    }

    method add_quasi_fixups($quasi_ast, $block) {
        $quasi_ast := nqp::decont($quasi_ast);
        self.add_object($quasi_ast);
        unless $quasi_ast.is_quasi_ast {
            return "";
        }
        my $fixups := QAST::Op.new(:op<forceouterctx>,
           QAST::BVal.new(:value($block)),
           QAST::Op.new(
                :op<p6getouterctx>,
                QAST::Var.new(
                    :scope<attribute>,
                    :name<$!quasi_context>,
                    QAST::WVal.new( :value($quasi_ast) ),
                    QAST::WVal.new( :value(self.find_symbol(['AST'])) )
                )
           )
        );
        self.add_fixup_task(:fixup_ast($fixups), :deserialize_ast($fixups));
    }

    # Generates code for running phasers.
    method run_phasers_code($code, $code_past, $block_type, $type) {
        my @phasers := nqp::atkey(nqp::getattr($code, $block_type, '$!phasers'), $type);
        my @results := $code_past.ann('phaser_results') || [];
        my $result  := QAST::Stmts.new();
        for @phasers -> $phaser {
            self.add_object_if_no_sc($phaser);
            my $call_code := QAST::Op.new( :op('call'), QAST::WVal.new( :value($phaser) ) );
            for @results -> $pcheck, $res {
                if $pcheck =:= $phaser {
                    $call_code := QAST::Op.new( :op('bind'), $res, $call_code );
                }
            }
            $result.push($call_code);
        }
        $result
    }

    # Adds any extra code needing for handling phasers.
    method add_phasers_handling_code($code, $code_past) {
        my $block_type := self.find_symbol(['Block'], :setting-only);
        if nqp::istype($code, $block_type) {
            my %phasers := nqp::getattr($code, $block_type, '$!phasers');
            unless nqp::isnull(%phasers) {
                if nqp::existskey(%phasers, 'PRE') {
                    $code_past[0].push(QAST::Op.new( :op('p6setpre') ));
                    $code_past[0].push(self.run_phasers_code($code, $code_past, $block_type, 'PRE'));
                    $code_past[0].push(QAST::Op.new( :op('p6clearpre') ));
                }
                if nqp::existskey(%phasers, 'FIRST') {
                    $code_past[0].push(QAST::Op.new(
                        :op('if'),
                        QAST::Op.new( :op('p6takefirstflag') ),
                        self.run_phasers_code($code, $code_past, $block_type, 'FIRST')));
                }
                if nqp::existskey(%phasers, 'ENTER') {
                    $code_past[0].push(self.run_phasers_code($code, $code_past, $block_type, 'ENTER'));
                }
                if nqp::existskey(%phasers, '!LEAVE-ORDER') || nqp::existskey(%phasers, 'POST') {
                    $code_past.has_exit_handler(1);
                    if nqp::existskey(%phasers, 'KEEP') || nqp::existskey(%phasers,'UNDO') {
                        $code_past.annotate('WANTMEPLEASE',1);
                    }
                }
                if nqp::existskey(%phasers, 'LAST') || nqp::existskey(%phasers, 'NEXT') ||
                        nqp::existskey(%phasers, 'QUIT') {
                    $code_past[0].push(QAST::Op.new(
                        :op('callmethod'), :name('!capture_phasers'),
                        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
                    ));
                }
            }
        }
    }

    # Gives the current block what's needed for "let"/"temp" support.
    method give_cur_block_let($/) {
        my $block := self.cur_lexpad();
        unless $block.symbol('!LET-RESTORE') {
            self.setup_let_or_temp($/, '!LET-RESTORE', 'UNDO');
        }
    }
    method give_cur_block_temp($/) {
        my $block := self.cur_lexpad();
        unless $block.symbol('!TEMP-RESTORE') {
            self.setup_let_or_temp($/, '!TEMP-RESTORE', 'LEAVE');
        }
    }
    method setup_let_or_temp($/, $value_stash, $phaser) {
        # Add variable to current block.
        my $block := self.cur_lexpad();
        $block[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($value_stash), :scope('lexical'), :decl('var') ),
            QAST::Op.new( :op('list') )));
        $block.symbol($value_stash, :scope('lexical'));

        # Create a phaser block that will do the restoration.
        my $phaser_block := self.push_lexpad($/);
        self.pop_lexpad();
        $phaser_block.push(QAST::Op.new(
            :op('while'),
            QAST::Var.new( :name($value_stash), :scope('lexical') ),
            QAST::Op.new(
                :op('p6store'),
                QAST::Op.new(
                    :op('shift'),
                    QAST::Var.new( :name($value_stash), :scope('lexical') )
                ),
                QAST::Op.new(
                    :op('shift'),
                    QAST::Var.new( :name($value_stash), :scope('lexical') )
                ))));

        # Add as phaser.
        $block[0].push($phaser_block);
        self.add_phaser($/, $phaser,
            self.create_code_object($phaser_block, 'Code',
                self.create_signature(nqp::hash('parameter_objects', []))));
    }

    # Adds a multi candidate to a proto/dispatch.
    method add_dispatchee_to_proto($proto, $candidate) {
        $proto.add_dispatchee($candidate);
    }

    # Derives a proto to get a dispatch.
    method derive_dispatcher($proto) {
        # Immediately do so and add to SC.
        my $derived := $proto.derive_dispatcher();
        self.add_object($derived);
        return $derived;
    }

    # Helper to make PAST for setting an attribute to a value. Value should
    # be a PAST tree.
    method set_attribute($obj, $class, $name, $value_past) {
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new(
                :name($name), :scope('attribute'),
                QAST::WVal.new( :value($obj) ),
                QAST::WVal.new( :value($class) )
            ),
            $value_past
        )
    }

    # Wraps a value in a scalar container
    method scalar_wrap($obj) {
        my $scalar_type := self.find_symbol(['Scalar']);
        my $scalar      := nqp::create($scalar_type);
        self.add_object($scalar);
        nqp::bindattr($scalar, $scalar_type, '$!value', $obj);
        $scalar;
    }

    # Takes a QAST::Block and compiles it for running during "compile time".
    # We need to do this for BEGIN but also for things that get called in
    # the compilation process, like user defined traits.
    method compile_in_context($past, $code_type) {
        # Ensure that we have the appropriate op libs loaded and correct
        # HLL.
        my $wrapper := QAST::Block.new(QAST::Stmts.new(), $past);
        self.add_libs($wrapper);

        # Create outer lexical contexts with all symbols visible. Maybe
        # we can be a bit smarter here some day. But for now we just make a
        # single frame and copy all the visible things into it.
        $wrapper.annotate('DYN_COMP_WRAPPER', 1);
        my %seen;
        my $mu        := try { self.find_symbol(['Mu']) };
        my $cur_block := $past;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            for %symbols {
                my str $name := $_.key;
                # For now, EVALed code run during precomp will not get the
                # outer lexical context's symbols as those may contain or
                # reference unserializable objects leading to compilation
                # failures. Needs a smarter approach as noted above.
                unless self.is_nested() || %seen{$name} {
                    # Add symbol.
                    my %sym   := $_.value;
                    my $value := nqp::existskey(%sym, 'value') || nqp::existskey(%sym, 'lazy_value_from')
                        ?? self.force_value(%sym, $name, 0)
                        !! $mu;
                    if nqp::isnull(nqp::getobjsc($value)) {
                        $value := self.try_add_to_sc($value, $mu);
                    }
                    $wrapper[0].push(QAST::Var.new(
                        :name($name), :scope('lexical'),
                        :decl(%sym<state> ?? 'statevar' !! 'static'), :$value
                    ));
                    $wrapper.symbol($name, :scope('lexical'));

                }
                %seen{$name} := 1;
            }
            $cur_block := $cur_block.ann('outer');
        }

        # Compile it, set wrapper's static lexpad, then invoke the wrapper,
        # which fixes up the lexicals.
        my $compunit := QAST::CompUnit.new(
            :hll('perl6'),
            :sc(self.sc()),
            :compilation_mode(0),
            $wrapper
        );
        my $comp := nqp::getcomp('perl6');
        my $precomp := $comp.compile($compunit, :from<optimize>, :compunit_ok(1),
             :lineposcache($*LINEPOSCACHE));
        my $mainline := $comp.backend.compunit_mainline($precomp);
        $mainline();

        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out. Also set up their static
        # lexpads, if they have any.
        my @coderefs := $comp.backend.compunit_coderefs($precomp);
        my int $num_subs := nqp::elems(@coderefs);
        my int $i := 0;
        my $result;
        while $i < $num_subs {
            my $subid := nqp::getcodecuid(@coderefs[$i]);
            my %sub_id_to_code_object := self.context().sub_id_to_code_object();
            if nqp::existskey(%sub_id_to_code_object, $subid) {
                my $code_obj := %sub_id_to_code_object{$subid};
                nqp::setcodeobj(@coderefs[$i], $code_obj);
                nqp::bindattr($code_obj, $code_type, '$!do', @coderefs[$i]);
                nqp::bindattr($code_obj, $code_type, '@!compstuff', nqp::null());
            }
            my %sub_id_to_cloned_code_objects := self.context().sub_id_to_cloned_code_objects();
            if nqp::existskey(%sub_id_to_cloned_code_objects, $subid) {
                for %sub_id_to_cloned_code_objects{$subid} -> $code_obj {
                    my $clone := nqp::clone(@coderefs[$i]);
                    nqp::setcodeobj($clone, $code_obj);
                    nqp::bindattr($code_obj, $code_type, '$!do', $clone);
                    nqp::bindattr($code_obj, $code_type, '@!compstuff', nqp::null());
                }
            }
            my %sub_id_to_sc_idx := self.context().sub_id_to_sc_idx();
            if nqp::existskey(%sub_id_to_sc_idx, $subid) {
                nqp::markcodestatic(@coderefs[$i]);
                self.update_root_code_ref(%sub_id_to_sc_idx{$subid}, @coderefs[$i]);
            }
            if nqp::existskey(%!code_object_fixup_list, $subid) {
                my $fixups := %!code_object_fixup_list{$subid};
                $fixups.pop() while $fixups.list;
            }
            if $subid eq $past.cuid {
                $result := @coderefs[$i];
            }
            $i := $i + 1;
        }

        # Flag block as dynamically compiled.
        $past.annotate('DYNAMICALLY_COMPILED', 1);

        # Return the VM coderef that maps to the thing we were originally
        # asked to compile.
        $result
    }
    method try_add_to_sc($value, $fallback) {
        self.add_object($value);
        CATCH { $value := $fallback; }
        $value
    }

    # Adds a constant value to the constants table. Returns PAST to do
    # the lookup of the constant.
    method add_constant($type, $primitive, :$nocache, *@value, *%named) {
        # If we already built this, find it in the cache and
        # just return that.
        my %const_cache := self.context().const_cache();
        my str $cache_key;
        if !$nocache {
            my str $namedkey := '';
            for %named {
                $namedkey := $namedkey ~ $_.key ~ ',' ~ $_.value ~ ';'
                    if nqp::defined($_.value);
            }
            if $primitive eq 'bigint' {
                $cache_key := "$type,bigint," ~ nqp::tostr_I(@value[0]);
            } else {
                $cache_key := "$type,$primitive,"
                    ~ join(',', @value)
                    ~ $namedkey;
            }
            if nqp::existskey(%const_cache, $cache_key) {
                my $value := %const_cache{$cache_key};
                return QAST::WVal.new( :value($value), :returns($value.WHAT) );
            }
        }

        # Find type object for the box typed we'll create.
        my $type_obj := self.find_symbol(nqp::split('::', $type));

        # Go by the primitive type we're boxing. Need to create
        # the boxed value and also code to produce it.
        my $constant;
        if $primitive eq 'int' {
            $constant := nqp::box_i(@value[0], $type_obj);
        }
        elsif $primitive eq 'str' {
            $constant := nqp::box_s(@value[0], $type_obj);
        }
        elsif $primitive eq 'num' {
            $constant := nqp::box_n(@value[0], $type_obj);
        }
        elsif $primitive eq 'bigint' {
            $constant := @value[0];
        }
        elsif $primitive eq 'type_new' {
            $constant := $type_obj.new(|@value, |%named);
        }
        else {
            nqp::die("Don't know how to build a $primitive constant");
        }

        # Add to SC.
        self.add_object($constant);

        # Build QAST for getting the boxed constant from the constants
        # table, but also annotate it with the constant itself in case
        # we need it. Add to cache.
        my $qast := QAST::WVal.new( :value($constant), :returns($constant.WHAT) );
        if !$nocache {
            %const_cache{$cache_key} := $constant;
        }
        return $qast;
    }

    # Adds a numeric constant value (int or num) to the constants table.
    # Returns PAST to do  the lookup of the constant.
    method add_numeric_constant($/, $type, $value) {
        if $type eq 'Int' && (try $value.HOW.name($value)) eq 'Int' {
            if nqp::isbig_I($value) {
                # cannot unbox to int without loss of information
                return self.add_constant('Int', 'bigint', $value);
            }
            # since Int doesn't have any vtables yet (at least while compiling
            # the setting), it is inconvenient to work with, so unbox
            $value := nqp::unbox_i($value);
        }
        my $const := self.add_constant($type, nqp::lc($type), $value);
        my $past;
        if $type eq 'Int' {
            $past := QAST::Want.new($const, 'Ii', QAST::IVal.new( :value($value) ) );
        }
        else {
            $past := QAST::Want.new($const, 'Nn',
                $value eq 'Inf'  ?? QAST::Op.new( :op('inf') ) !!
                $value eq '-Inf' ?? QAST::Op.new( :op('neginf') ) !!
                $value eq 'NaN'  ?? QAST::Op.new( :op('nan') ) !!
                                    QAST::NVal.new( :value($value) ) );
        }
        $past.returns($const.returns);
        if $/ {
            $past.node($/);
        }
        $past;
    }

    # Adds a string constant value to the constants table.
    # Returns PAST to do the lookup of the constant.
    method add_string_constant($value) {
        my $const := self.add_constant('Str', 'str', $value);
        QAST::Want.new(
            $const, :returns($const.returns),
            'Ss', QAST::SVal.new( :value($value) ));
    }

    method whatever() {
        my $the_whatever := self.context().whatever();
        unless nqp::isconcrete($the_whatever) {
            $the_whatever := nqp::create(self.find_symbol(['Whatever']));
            self.add_object($the_whatever);
            self.context().set_whatever($the_whatever);
        }
        QAST::WVal.new( :value($the_whatever), :returns($the_whatever.WHAT) )
    }

    method hyper_whatever() {
        my $the_hyper_whatever := self.context().hyper_whatever();
        unless nqp::isconcrete($the_hyper_whatever) {
            $the_hyper_whatever := nqp::create(self.find_symbol(['HyperWhatever']));
            self.add_object($the_hyper_whatever);
            self.context().set_hyper_whatever($the_hyper_whatever);
        }
        QAST::WVal.new( :value($the_hyper_whatever), :returns($the_hyper_whatever.WHAT) )
    }

    # Adds the result of a constant folding operation to the SC and
    # returns a reference to it.
    method add_constant_folded_result($r) {
        self.add_object_if_no_sc($r);
        QAST::WVal.new( :value($r) )
    }

    # Takes a data structure of non-Perl 6 objects and wraps them up
    # recursively.
    method p6ize_recursive($data) {
        p6ize_recursive($data)
    }

    method nibble_to_str($/, $ast, $mkerr) {
        if (nqp::istype($ast, QAST::Stmts) || nqp::istype($ast, QAST::Stmt)) && +@($ast) == 1 {
            $ast := $ast[0];
        }

        $ast.wanted(1);
        if $ast.has_compile_time_value {
            return nqp::unbox_s($ast.compile_time_value);
        } elsif nqp::istype($ast, QAST::Op) {
            if $ast.name eq '&infix:<,>' {
                my @pieces;
                for @($ast) {
                    if $_.has_compile_time_value {
                        nqp::push(@pieces, nqp::unbox_s($_.compile_time_value));
                    }
                    else {
                        if nqp::istype($_, QAST::Var) {
                            my $result;
                            {
                                $result := self.compile_time_evaluate($/, $_);
                                CATCH {
                                    $/.CURSOR.panic($mkerr());
                                }
                            }
                            nqp::push(@pieces, nqp::unbox_s($result));
                        } else {
                            $/.CURSOR.panic($mkerr());
                        }
                    }
                }
                return join(' ', @pieces);
            } elsif $ast.name eq '&val' {
                # we ignore the val() here (since we clearly want a string
                # anyway) and focus on what's inside the val (which could be a
                # single thing or a list of things, as done above)
                self.nibble_to_str($/, $ast[0], $mkerr);
            } else {
                # check if there's something in $ast that probably wont work
                # as a compile time value
                my $inspect := $ast;
                while nqp::istype($inspect, QAST::Op) {
                    $inspect := $inspect[0];
                }
                if nqp::istype($inspect, QAST::WVal) && !nqp::istype($inspect.value, self.find_symbol(["Block"], :setting-only)) {
                    $/.CURSOR.panic($mkerr());
                }
                else {
                    my $result;
                    {
                        $result := ~self.compile_time_evaluate($/, $ast);
                        CONTROL {
                            # we might get a warning from evaluating a Block like
                            # "undefined value ..." which is reason enough to die
                            $/.CURSOR.panic($mkerr());
                        }
                        CATCH {
                            $/.CURSOR.panic($mkerr());
                        }
                    }
                    # if we have something here, it's probably a Slip,
                    # which stringifies fine but has to be split at ','
                    # and potentially whitespace-corrected
                    my @parts := nqp::split(' ', $result);
                    return nqp::join(" ", @parts);
                }
            }
        } elsif nqp::istype($ast, QAST::Var) {
            my $result;
            $result := self.compile_time_evaluate($/, $ast);
            CATCH {
                $/.CURSOR.panic($mkerr());
            }
            return nqp::unbox_s($result);
        } else {
            $/.CURSOR.panic($mkerr());
        }
    }

    method colonpair_nibble_to_str($/, $nibble) {
        self.nibble_to_str($/, $nibble.ast,
            -> {
                self.throw($/, ['X', 'Syntax', 'Extension', 'TooComplex'],
                  name       => ~$nibble,
                );
            });
    }

    # Takes a declarator name and locates the applicable meta-object for it.
    method resolve_mo($/, $declarator) {
        my %HOW := %*HOW;
        if nqp::existskey(%HOW, $declarator) {
            nqp::atkey(%HOW, $declarator)
        }
        elsif $declarator ~~ /'-attr'$/ {
            self.find_symbol(['Attribute'])
        }
        else {
            $/.CURSOR.panic("Cannot resolve meta-object for $declarator")
        }
    }

    # Creates a meta-object for a package, adds it to the root objects and
    # returns the created object.
    method pkg_create_mo($/, $how, :$name, :$repr, :$auth, :$ver, *%extra) {
        # Create the meta-object and add to root objects.
        my %args;
        if nqp::defined($name) { %args<name> := ~$name; }
        if nqp::defined($repr) { %args<repr> := ~$repr; }
        if nqp::defined($ver) { %args<ver> := $ver; }
        if nqp::defined($auth) { %args<auth> := $auth; }
        if nqp::existskey(%extra, 'base_type') {
            %args<base_type> := %extra<base_type>;
        }
        if nqp::existskey(%extra, 'group') {
            %args<group> := %extra<group>;
        }
        if nqp::existskey(%extra, 'signatured') {
            %args<signatured> := %extra<signatured>;
        }
        my $mo := $how.new_type(|%args);
        self.add_object($mo);

        # Result is just the object.
        return $mo;
    }

    # Constructs a meta-attribute and adds it to a meta-object. Expects to
    # be passed the meta-attribute type object, a set of literal named
    # arguments to pass and a set of name to object mappings to pass also
    # as named arguments, but where these passed objects also live in a
    # serialization context. The type would be passed in this way.
    method pkg_add_attribute($/, $obj, $meta_attr, %args,
            %cont_info, $descriptor) {
        # Create meta-attribute instance and add right away. Also add
        # it to the SC.
        my $cont := self.build_container(%cont_info, $descriptor);
        my $attr := $meta_attr.new(:auto_viv_container($cont), |%args);
        $obj.HOW.add_attribute($obj, $attr);
        self.add_object($attr);
        $attr
    }

    # Tries to locate an attribute meta-object; optionally panic right
    # away if we cannot, otherwise add it to the post-resolution list.
    method get_attribute_meta_object($/, $name, $later?) {
        unless nqp::can($*PACKAGE.HOW, 'get_attribute_for_usage') {
            $/.CURSOR.panic("Cannot understand $name in this context");
        }
        my $attr;
        my int $found := 0;
        try {
            $attr := $*PACKAGE.HOW.get_attribute_for_usage($*PACKAGE, $name);
            $found := 1;
        }
        unless $found {
            # Need to check later
            if $later {
                my $seen := %*ATTR_USAGES{$name};
                unless $seen {
                    %*ATTR_USAGES{$name} := $seen := nqp::list();
                    $later.node($/); # only need $/ for first error
                }
                $seen.push($later);
            }

            # Now is later
            else {
                self.throw($/, ['X', 'Attribute', 'Undeclared'],
                  symbol       => $name,
                  package-kind => $*PKGDECL,
                  package-name => $*PACKAGE.HOW.name($*PACKAGE),
                  what         => 'attribute',
                );
            }
        }
        $attr
    }

    # Adds a method to the meta-object.
    method pkg_add_method($/, $obj, $meta_method_name, $name, $code_object) {
        self.ex-handle($/, {
                $obj.HOW."$meta_method_name"($obj, $name, $code_object)
            }
        )
    }

    # Handles setting the body block code for a role.
    method pkg_set_role_body_block($/, $obj, $code_object, $past) {
        # Add it to the compile time meta-object.
        $obj.HOW.set_body_block($obj, $code_object);

        # Compile it immediately (we always compile role bodies as
        # early as possible, but then assume they don't need to be
        # re-compiled and re-fixed up at startup).
        self.compile_in_context($past, self.find_symbol(['Code'], :setting-only));
    }

    # Adds a possible role to a role group.
    method pkg_add_role_group_possibility($/, $group, $role) {
        $group.HOW.add_possibility($group, $role);
    }

    # Composes the package, and stores an event for this action.
    method pkg_compose($/, $obj) {
        my $compiler_services := self.get_compiler_services();
        if nqp::isconcrete($compiler_services) {
            self.ex-handle($/, { $obj.HOW.compose($obj, :$compiler_services) })
        }
        else {
            self.ex-handle($/, { $obj.HOW.compose($obj) })
        }
    }

    my class CompilerServices {
        has $!w;

        # We share one Signature object among accessors for a given package.
        has $!acc_sig_cache;
        has $!acc_sig_cache_type;

        method generate_accessor(str $meth_name, $package_type, str $attr_name, $type, int $rw) {
            my $native := nqp::objprimspec($type) != 0;
            my $acc := QAST::Var.new(
                :scope($native && $rw ?? 'attributeref' !! 'attribute'),
                :name($attr_name), :returns($type),
                QAST::Op.new(
                    :op('decont'),
                    QAST::Var.new( :name('self'), :scope('local') )
                ),
                QAST::WVal.new( :value($package_type) )
            );
            unless $native || $rw {
                $acc := QAST::Op.new( :op('decont'), $acc );
            }
            my $block := QAST::Block.new(
                :name($meth_name), :blocktype('declaration_static'),
                QAST::Stmts.new(
                    QAST::Var.new(
                        :decl('param'), :scope('local'), :name('self')
                    ),
                    QAST::Var.new(
                        :decl('param'), :scope('local'), :name('_'), :slurpy, :named
                    )
                ),
                QAST::Stmts.new($acc));
            $!w.cur_lexpad()[0].push($block);

            my $sig;
            if $package_type =:= $!acc_sig_cache_type {
                $sig := $!acc_sig_cache;
            }
            else {
                my %sig_info := nqp::hash('parameters', []);
                $sig := $!w.create_signature_and_params(NQPMu, %sig_info,
                    $block, 'Any', :method, invocant_type => $package_type);
                $!acc_sig_cache := $sig;
                $!acc_sig_cache_type := $package_type;
            }

            my $code := $!w.create_code_object($block, 'Method', $sig);
            $code.set_rw() if $rw;
            return $code;
        }
    }
    method get_compiler_services() {
        unless nqp::isconcrete($!compiler_services) {
            try {
                my $wtype   := self.find_symbol(['Rakudo', 'Internals', 'CompilerServices']);
                my $wrapped := CompilerServices.new(w => self);
                my $wrapper := nqp::create($wtype);
                nqp::bindattr($wrapper, $wtype, '$!compiler', $wrapped);
                $!compiler_services := $wrapper;
            }
        }
        $!compiler_services
    }

    # Builds a curried role based on a parsed argument list.
    method parameterize_type($role, $arglist, $/) {
        # Build a list of compile time arguments to the role; whine if
        # we find something without one.
        my @pos_args;
        my %named_args;
        for @($arglist) {
            my $val;
            if $_.has_compile_time_value {
                $val := $_.compile_time_value;
            }
            else {
                $val := self.compile_time_evaluate($/, $_);
            }
            if $_.named {
                %named_args{$_.named} := $val;
            }
            else {
                @pos_args.push($val);
            }
        }

        self.parameterize_type_with_args($/, $role, @pos_args, %named_args);
    }

    # Curries a role with the specified arguments.
    method parameterize_type_with_args($/, $role, @pos_args, %named_args) {
        # Make the curry right away and add it to the SC.
        if @pos_args || %named_args {
            unless nqp::can($role.HOW, 'parameterize') {
                self.throw($/, 'X::NotParametric', type => $role);
            }
            my $curried := $role.HOW.parameterize($role, |@pos_args, |%named_args);
            self.add_object($curried);
            return $curried;
        }
        $role;
    }

    # Creates a subset type meta-object/type object pair.
    method create_subset($how, $refinee, $refinement, :$name) {
        # Create the meta-object and add to root objects.
        my %args := hash(:refinee($refinee), :refinement($refinement));
        if nqp::defined($name) { %args<name> := $name; }
        my $mo := $how.new_type(|%args);
        self.add_object($mo);
        return $mo;
    }

    # Gets a definite type (possibly freshly created, possibly an
    # interned one).
    method create_definite_type($how, $base_type, $definite) {
       # Create the meta-object and add to root objects.
        my $mo := $how.new_type(:$base_type, :$definite);
        self.add_object($mo);
        return $mo;
    }

    # Adds a value to an enumeration.
    method create_enum_value($enum_type_obj, $key, $value) {
        # Create directly.
        my $val := nqp::rebless(nqp::clone($value), $enum_type_obj);
        nqp::bindattr($val, $enum_type_obj, '$!key', $key);
        nqp::bindattr($val, $enum_type_obj, '$!value', $value);
        self.add_object($val);

        # Add to meta-object.
        $enum_type_obj.HOW.add_enum_value($enum_type_obj, $val);

        # Result is the value.
        $val
    }

    # Gets a coercion type (possibly freshly created, possibly an
    # interned one).
    method create_coercion_type($/, $target, $constraint) {
        self.ex-handle($/, {
            my $type := %*HOW<coercion>.new_type($target, $constraint);
            if nqp::isnull(nqp::getobjsc($type)) { self.add_object($type); }
            $type
        })
    }

    method suggest_typename($name) {
        my %seen;
        %seen{$name} := 1;
        my @candidates := [[], [], []];
        my &inner-evaluator := make_levenshtein_evaluator($name, @candidates);
        my @suggestions;

        sub evaluator($name, $object, $has_object, $hash) {
            # only care about type objects
            my $first := nqp::substr($name, 0, 1);
            return 1 if $first eq '$' || $first eq '%' || $first eq '@' || $first eq '&' || $first eq ':';
            return 1 if !$has_object || nqp::isconcrete($object);
            return 1 if nqp::existskey(%seen, $name);

            %seen{$name} := 1;
            return &inner-evaluator($name, $hash);
        }
        self.walk_symbols(&evaluator);

        levenshtein_candidate_heuristic(@candidates, @suggestions);

        return @suggestions;
    }

    # Applies a trait.
    method apply_trait($/, $trait_sub_name, *@pos_args, *%named_args) {
        my $trait_sub := self.find_symbol([$trait_sub_name]);
        my $ex;
        try {
            self.ex-handle($/, { $trait_sub(|@pos_args, |%named_args) });
            CATCH { $ex := $_; }
            CONTROL {
                if nqp::getextype($_) == nqp::const::CONTROL_WARN {
                    $/.CURSOR.worry(nqp::getmessage($_));
                    nqp::resume($_);
                }
                nqp::rethrow($_);
            }
        }
        if $ex {
            my $payload := nqp::getpayload($ex);
            if nqp::istype($payload, self.find_symbol(["X", "Inheritance", "UnknownParent"], :setting-only)) {
                my @suggestions := self.suggest_typename($payload.parent);
                for @suggestions {
                    $payload.suggestions.push($_)
                }
            }
            self.rethrow($/, $ex);
        }
    }

    # Applies a list of traits, by calling the apply method on each.
    method apply_traits($traits, $declarand) {
        for $traits {
            my $ast := $_.ast;
            $ast.apply($declarand) if $ast;
        }
    }

    # Some things get cloned many times with an outer lexical scope that
    # we never enter. This makes sure we capture them as needed.
    method create_lexical_capture_fixup() {
        # TODO: use the Moar code-path here on all backends. The issue was
        # discovered close to release, so only the Moar backend - that really
        # needs this - is being updated for now.
        my class FixupList {
            has $!list;
            has $!resolved;
            has $!resolver;
            method add_unresolved($code) {
                nqp::scwbdisable();
                nqp::push($!list, $code);
                nqp::scwbenable();
                if nqp::isconcrete($!resolved) {
                    my int $added_update := 0;
                    try {
                        my $W := $*W;
                        my $cur_handle := $W.handle;
                        if $cur_handle ne $!resolver {
                            $W.add_object($code);
                            $W.add_fixup_task(:deserialize_ast(QAST::Op.new(
                                :op('callmethod'), :name('update'),
                                QAST::WVal.new( :value(self) ),
                                QAST::WVal.new( :value($code) )
                            )));
                            $added_update := 1;
                        }
                    }
                    unless $added_update {
                        nqp::p6captureouters2([$code], $!resolved);
                    }
                }
            }
            method resolve($resolved) {
                nqp::scwbdisable();
                $!resolved := $resolved;
                nqp::scwbenable();
                nqp::p6captureouters2($!list, $resolved);
            }
            method update($code) {
                if !nqp::isnull($!resolved) && !nqp::istype($!resolved, NQPMu) {
                    nqp::p6captureouters2([$code],
                        nqp::getcomp('perl6').backend.name eq 'moar'
                            ?? nqp::getstaticcode($!resolved)
                            !! $!resolved);
                }
            }
        }

        # Create a list and put it in the SC.
        my $fixup_list := nqp::create(FixupList);
        self.add_object($fixup_list);
        nqp::bindattr($fixup_list, FixupList, '$!list', nqp::list());
        nqp::bindattr($fixup_list, FixupList, '$!resolver', self.handle());

        # Set up capturing code.
        my $capturer := self.cur_lexpad();
        my $c_block  := QAST::Block.new( :blocktype('declaration_static'),
                                         :name('!LEXICAL_FIXUP_CSCOPE') );
        self.create_simple_code_object($c_block, 'Code');
        $capturer[0].push(QAST::Op.new(
            :op('callmethod'), :name('resolve'),
            QAST::WVal.new( :value($fixup_list) ),
            QAST::Op.new( :op('takeclosure'), $c_block )));

        # Return a QAST node that we can push the dummy closure.
        return QAST::Op.new(
            :op('callmethod'), :name('add_unresolved'),
            QAST::WVal.new( :value($fixup_list) )
        );
    }

    # Handles addition of a phaser.
    method add_phaser($/, $phaser, $block, $phaser_past?) {
        if $phaser eq 'BEGIN' {
            # BEGIN phasers get run immediately.
            my $result := self.handle-begin-time-exceptions($/, 'evaluating a BEGIN', $block);
            return self.add_constant_folded_result($result);
        }
        elsif $phaser eq 'CHECK' {
            my $result_node := QAST::Stmt.new( QAST::Var.new( :name('Nil'), :scope('lexical') ) );
            self.context().add_check([$block, $result_node]);
            return $result_node;
        }
        elsif $phaser eq 'INIT' {
            unless $*UNIT.symbol('!INIT_VALUES') {
                my $mu := self.find_symbol(['Mu']);
                my %info;
                %info<container_type> := %info<container_base> := self.find_symbol(['Hash']);
                %info<bind_constraint> := self.find_symbol(['Associative']);
                %info<value_type> := $mu;
                self.install_lexical_container($*UNIT, '!INIT_VALUES', %info,
                    self.create_container_descriptor($mu, 1, '!INIT_VALUES'));
            }
            $*UNIT[0].push(QAST::Op.new(
                :op('callmethod'), :name('BIND-KEY'),
                QAST::Var.new( :name('!INIT_VALUES'), :scope('lexical') ),
                QAST::SVal.new( :value($phaser_past.cuid) ),
                QAST::Op.new(
                    :op('call'),
                    QAST::WVal.new( :value($block) )
                )));
            return QAST::Op.new(
                :op('callmethod'), :name('AT-KEY'),
                QAST::Var.new( :name('!INIT_VALUES'), :scope('lexical') ),
                QAST::SVal.new( :value($phaser_past.cuid) )
            );
        }
        elsif $phaser eq 'END' {
            $*UNIT[0].push(QAST::Op.new(
                :op('callmethod'), :name('unshift'),
                QAST::Op.new(
                    :op('getcurhllsym'),
                    QAST::SVal.new( :value('@END_PHASERS') ),
                ),
                QAST::WVal.new( :value($block) )
            ));
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
        }
        elsif $phaser eq 'PRE' || $phaser eq 'POST' {
            my $what := self.add_string_constant($phaser);
            $what.named('phaser');
            my $condition := self.add_string_constant(~$/<blorst>);
            $condition.named('condition');

            $phaser_past[1] := QAST::Op.new(
                :op('unless'),
                $phaser_past[1],
                QAST::Op.new(
                    :op('callmethod'), :name('throw'),
                    QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :value(self.find_symbol(['X', 'Phaser', 'PrePost'])) ),
                        $what,
                        $condition,
                    )
                ),
            );

            if $phaser eq 'POST' {
                # Needs $_ that can be set to the return value.
                $phaser_past.custom_args(1);
                $phaser_past[0].unshift(QAST::Op.new( :op('p6bindsig') ));
                if $phaser_past.symbol('$_') {
                    for @($phaser_past[0]) {
                        if nqp::istype($_, QAST::Op) && $_.op eq 'bind' && $_[0].name eq '$_' {
                            $_.op('null'); $_.shift(); $_.shift();
                        }
                    }
                }
                else {
                    $phaser_past[0].unshift(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
                }
                nqp::push(
                    nqp::getattr($block.signature, self.find_symbol(['Signature'], :setting-only), '@!params'),
                    self.create_parameter($/, hash(
                            variable_name => '$_', is_raw => 1,
                            nominal_type => self.find_symbol(['Mu'])
                        )));
            }

            self.context().cur_code_object().add_phaser($phaser, $block);
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
        }
        elsif $phaser eq 'ENTER' {
            self.context().cur_code_object().add_phaser($phaser, $block);
            my $enclosing := self.context().cur_lexpad();
            my $enter_tmp := $enclosing.unique('enter_result_');
            $enclosing[0].push(QAST::Var.new( :name($enter_tmp), :scope('local'), :decl('var') ));
            my @pres := $enclosing.ann('phaser_results') || $enclosing.annotate('phaser_results', []);
            @pres.push($block);
            @pres.push(my $var := QAST::Var.new( :name($enter_tmp), :scope('local') ));
            $var.wanted(1);  # don't really know if wanted, but suppress warning
            return $var;
        }
        else {
            self.context().cur_code_object().add_phaser($phaser, $block);
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
        }
    }

    # Runs the CHECK phasers and twiddles the QAST to look them up.
    method CHECK() {
        for self.context().checks() {
            my $result := $_[0]();
            $_[1][0] := self.add_constant_folded_result($result);
        }
    }

    # Does any cleanups needed after compilation.
    method cleanup() {
        for self.context().cleanup_tasks() { $_() }
    }

    # Adds required libraries to a compilation unit.
    method add_libs($comp_unit) {
        $comp_unit.push(QAST::VM.new(
            loadlibs => ['nqp_group', 'nqp_ops', 'perl6_ops',
                         'bit_ops', 'math_ops', 'trans_ops', 'io_ops',
                         'obscure_ops', 'os', 'file', 'sys_ops',
                         'nqp_bigint_ops', 'nqp_dyncall_ops' ],
            jvm => QAST::Op.new( :op('null') ),
            moar => QAST::Op.new( :op('null') )));
    }

    # Represents a longname after having parsed it.
    my class LongName {
        # a match object, so that error messages can get a proper line number
        has $!match;

        # Set of name components. Each one will be either a string
        # or a PAST node that represents an expression to produce it.
        has @!components;

        # The colonpairs, if any.
        has @!colonpairs;

        # Flag for if the name ends in ::, meaning we need to emit a
        # .WHO on the end.
        has int $!get_who;

        # Gets the textual name of the value.
        method text() {
            ~$!match
        }

        # Gets the name, by default without any adverbs.
        method name(:$decl, :$dba = '', :$with_adverbs) {
            my @parts := self.type_name_parts($dba, :$decl);
            unless $decl && $decl eq 'routine' {
                @parts.shift() while self.is_pseudo_package(@parts[0]);
            }
            join('::', @parts)
                ~ ($with_adverbs ?? self.canonical_pairs !! '');
        }

        # returns a QAST tree that represents the name
        # currently needed for 'require ::($modulename) <importlist>'
        # ignore adverbs for now
        method name_past() {
            if self.contains_indirect_lookup() {
                if @!components == 1 {
                    return @!components[0];
                }
                else {
                    my $past := QAST::Op.new(:op<call>, :name('&infix:<,>'));
                    for @!components {
                        $past.push: $_ ~~ QAST::Node ?? $_ !! QAST::SVal.new(:value($_));
                    }
                    return QAST::Op.new(:op<callmethod>, :name<join>,
                        $past,
                        QAST::SVal.new(:value<::>)
                    );
                }
            }
            else {
                my $value := join('::', @!components);
                QAST::SVal.new(:$value);
            }
        }

        method canonical_pairs() {
            return '' unless @!colonpairs;
            my $result := '';
            my $w := $*W;
            my $Bool := $w.find_symbol(['Bool']);
            for @!colonpairs {
                my $p := $w.compile_time_evaluate($_, $_.ast);
                if nqp::istype($p.value,$Bool) {
                    $result := $result ~ ':' ~ ($p.value ?? '' !! '!') ~ $p.key;
                }
                else {
                    $result := $result ~ $w.canonicalize_pair($p.key,$p.value);
                }
            }
            $result;
        }

        # Note, this permanently mutates the last component.
        method attach_adverbs() {
            if @!colonpairs {
                my $last := nqp::pop(@!components) ~ self.canonical_pairs;
                nqp::push(@!components,$last);
            }
            self;
        }

        # Gets the individual components, which may be PAST nodes for
        # unknown pieces.
        method components() {
            @!components
        }

        # Gets the individual components (which should be strings) but
        # taking a sigil and twigil and adding them to the last component.
        method variable_components($sigil, $twigil) {
            my @result;
            for @!components {
                @result.push($_);
            }
            @result
              ?? (@result[+@result-1] := $sigil ~ $twigil ~ @result[+@result-1])
              !! (@result[0]          := $sigil ~ $twigil);
            @result
        }

        # Checks if there is an indirect lookup required.
        method contains_indirect_lookup() {
            for @!components {
                if nqp::istype($_, QAST::Node) {
                    return 1;
                }
            }
            return 0;
        }

        # Fetches an array of components provided they are all known
        # or resolvable at compile time.
        method type_name_parts($dba, :$decl) {
            my @name;
            my int $beyond_pp;
            if $decl && $!get_who {
                my $name := self.text;
                nqp::die("Name $name ends with '::' and cannot be used as a $dba");
            }
            if +@!components == 1 && self.is_pseudo_package(@!components[0]) {
                my $c := @!components[0];
                if !$decl || ($decl eq 'routine') {
                    nqp::push(@name, $c);
                    return @name;
                }
                if $c eq 'GLOBAL' {
                    nqp::die("Cannot declare pseudo-package GLOBAL");
                }
                $*W.throw($!match, 'X::PseudoPackage::InDeclaration',
                    pseudo-package  => $c,
                    action          => $dba,
                );
            }
            for @!components {
                if nqp::istype($_, QAST::Node) {
                    if $_.has_compile_time_value {
                        for nqp::split('::', ~$_.compile_time_value) {
                            @name.push($_);
                        }
                    }
                    else {
                        my $name := self.text;
                        nqp::die("Name $name is not compile-time known, and can not serve as a $dba");
                    }
                }
                elsif $beyond_pp || !self.is_pseudo_package($_) {
                    nqp::push(@name, $_);
                    $beyond_pp := 1;
                }
                else {
                    if $decl {
                        if $_ ne 'GLOBAL' {
                            $*W.throw($!match, 'X::PseudoPackage::InDeclaration',
                                pseudo-package  => $_,
                                action          => $dba,
                            );
                        }
                    }
                    else {
                        nqp::push(@name, $_);
                    }
                }
            }
            @name
        }

        method colonpairs_hash($dba) {
            my %result;
            for @!colonpairs {
                if $_<identifier> {
                    my $pair := $*W.compile_time_evaluate($_, $_.ast);
                    %result{$pair.key} := $pair.value;
                }
                else {
                    $_.CURSOR.panic("Colonpair too complex in $dba");
                }
            }
            %result
        }

        method get_who() {
            $!get_who
        }

        # Gets the name qualified with the specified package.
        method fully_qualified_with($target_package) {
            my $fullname := self.name();
            unless $target_package =:= $*GLOBALish {
                try {
                    my str $tname := nqp::getattr_s(
                        nqp::decont($target_package.WHO),
                        $*W.find_symbol(['Stash']),
                        '$!longname');
                    $fullname := $tname ~ '::' ~ $fullname;
                }
            }
            $fullname
        }

        # Checks if a name component is a pseudo-package.
        my %pseudo := nqp::hash(
            'PROCESS', 1, 'GLOBAL', 1, 'OUR', 1, 'MY', 1,
            'CORE', 1, 'SETTING', 1, 'UNIT', 1,
            'OUTER', 1, 'OUTERS', 1, 'LEXICAL', 1,
            'CALLER', 1, 'CALLERS', 1, 'DYNAMIC', 1,
            'COMPILING', 1, 'PARENT', 1, 'CLIENT', 1);
        method is_pseudo_package($comp) {
            !nqp::istype($comp, QAST::Node) && %pseudo{$comp};
        }

        # Checks if the name starts with GLOBAL.
        method is_declared_in_global() {
            @!components > 1
              && !nqp::istype(@!components[0], QAST::Node)
              && @!components[0]   # need this to fix crash on OS X
              && @!components[0] eq 'GLOBAL';
        }
    }

    # Takes a longname and turns it into an object representing the
    # name.
    method dissect_longname($longname) {
        # Set up basic info about the long name.
        my $result := nqp::create(LongName);
        nqp::bindattr($result, LongName, '$!match', $longname);

        # Pick out the pieces of the name.
        my @components;
        my $name := $longname<name>;
        if $name<identifier> {
            @components.push(~$name<identifier>);
        }
        for $name<morename> {
            if $_<identifier> {
                @components.push(~$_<identifier>);
            }
            elsif $_<EXPR> {
                my $EXPR := $_<EXPR>.ast;
                @components.push($EXPR);
            }
            else {
                # Either it's :: as a name entirely, in which case it's anon,
                # or we're ending in ::, in which case it implies .WHO.
                if +@components {
                    nqp::bindattr_i($result, LongName, '$!get_who', 1);
                }
            }
        }
        nqp::bindattr($result, LongName, '@!components', @components);

        # Stash colon pairs with names; incorporate non-named one into
        # the last part of the name (e.g. for infix:<+>). Need to be a
        # little cheaty when compiling the setting due to bootstrapping.
        my @pairs;
        for $longname<colonpair> {
            if $_<coloncircumfix> && !$_<identifier> {
                my $cp_str;
                if $*COMPILING_CORE_SETTING {
                    my $ast := $_.ast;

                    # XXX hackish for dealing with <longname> stuff, which
                    # doesn't get handled in a consistent way like <deflongname>
                    # stuff. The better solution, in the long run, is to
                    # uniformly run longname through nibble_to_str before this
                    if nqp::istype($ast, QAST::Op) && $ast.name eq '&val' {
                        $ast := $ast[0];
                    }
                    if nqp::istype($ast, QAST::Want) && nqp::istype($ast[2], QAST::SVal) {
                        $cp_str := self.canonicalize_pair('',$ast[2].value);
                    }
                    elsif nqp::istype($ast, QAST::WVal) &&
                          nqp::istype($ast.value, $*W.find_symbol(['Str'], :setting-only)) {
                        $cp_str := self.canonicalize_pair('', $ast.value);
                    }
                    else {
                        $cp_str := ~$_;
                    }
                }

                else {
                    # Safe to evaluate it directly; no bootstrap issues.
                    $cp_str := self.canonicalize_pair('',self.compile_time_evaluate($_, $_.ast));
                }
                if +@components {
                    @components[+@components - 1] := @components[+@components - 1] ~ $cp_str;
                } else {
                    @components[0] := $cp_str;
                }
            }
            else {
                $_.ast.wanted(1);
                @pairs.push($_);
            }
        }
        nqp::bindattr($result, LongName, '@!colonpairs', @pairs);

        $result
    }
    method dissect_deflongname($deflongname) {
        # deflongname has the same capture structure as longname
        self.dissect_longname($deflongname);
    }

    # Checks if a name starts with a pseudo-package.
    method is_pseudo_package($comp) {
        LongName.is_pseudo_package($comp)
    }

    # Checks if a given symbol is declared.
    method is_name(@name) {
        my int $is_name := 0;
        if self.is_pseudo_package(@name[0]) {
            $is_name := 1;
        }
        else {
            try {
                # This throws if it's not a known name.
                self.find_symbol(@name);
                $is_name := 1;
            }
        }
        $is_name || +@name == 1 && self.is_lexical(@name[0])
    }

    method symbol_has_compile_time_value(@name) {
        my $has_ctv := 0;
        try {
            my $sym := self.find_symbol(@name);
            $has_ctv := !nqp::iscont($sym);
        }
        $has_ctv;
    }

    # Checks if a given symbol is declared and a type object.
    method is_type(@name) {
        my $is_name := 0;
        try {
            # This throws if it's not a known name.
            $is_name := !nqp::isconcrete(self.find_symbol(@name))
        }
        $is_name
    }

    # Checks if a symbol has already been declared in the current
    # scope, and thus may not be redeclared.
    method already_declared($scope, $curpackage, $curpad, @name) {
        my str $first_name := ~@name[0];
        if $scope eq 'my' && +@name == 1 {
            my %sym := $curpad.symbol($first_name);
            if %sym {
                my $value := self.force_value(%sym, $first_name, 0);
                return $value.HOW.HOW.name($value.HOW) ne 'Perl6::Metamodel::PackageHOW';
            }
            return 0;
        }
        else {
            # Does the current lexpad or package declare the first
            # part of the name? If not, we're in the clear.
            my $first_sym;
            if $curpad.symbol($first_name) {
                $first_sym := self.force_value($curpad.symbol($first_name), $first_name, 0);
            }
            elsif nqp::existskey($curpackage.WHO, $first_name) {
                $first_sym := ($curpackage.WHO){$first_name};
            }
            else {
                return 0;
            }

            # If we've more name, recursively check the next level
            # in the package. Otherwise, just go on if it's a
            # package or not.
            if +@name > 1 {
                my @restname := nqp::clone(@name);
                @restname.shift;
                return self.already_declared('our', $first_sym, QAST::Block.new(), @restname);
            }
            else {
                return $first_sym.HOW.HOW.name($first_sym.HOW) ne 'Perl6::Metamodel::PackageHOW';
            }
        }
    }

    # Checks if there is a regex in scope.
    method regex_in_scope($name) {
        my $result := 0;
        try {
            my $maybe_regex := self.find_symbol([$name]);
            $result := nqp::istype($maybe_regex, self.find_symbol(['Regex'], :setting-only));
        }
        $result
    }

    method walk_symbols($code) {
        # first, go through all lexical scopes
        sub walk_block($block) {
            my %symtable := $block.symtable();
            for %symtable -> $symp {
                my $key := $symp.key;
                my %sym := $symp.value;
                my $has_val := nqp::existskey(%sym, 'value') || nqp::existskey(%sym, 'lazy_value_from');
                my $val := self.force_value(%sym, $key, 0);
                return 0 if $code($key, $val, $has_val, %sym) == 0;
            }
            1;
        }

        for self.context().blocks() {
            return 0 if walk_block($_) == 0;
        }
        for self.stash_hash($*GLOBALish) {
            return 0 if $code($_.key, $_.value, 1, hash()) == 0;
        }
    }

    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_symbol(@name, :$setting-only, :$upgrade_to_global, :$cur-package = $*PACKAGE) {
        # Make sure it's not an empty name.
        unless +@name { nqp::die("Cannot look up empty name"); }

        # GLOBAL is current view of global.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return $*GLOBALish;
        }

        my @BLOCKS := self.context().blocks;

        # Work out where to start searching.
        my int $start_scope := $setting-only
            ?? ($*COMPILING_CORE_SETTING ?? 2 !! 1)
            !! nqp::elems(@BLOCKS);

        # If it's a single-part name, look through the lexical
        # scopes and try the current package.
        if +@name == 1 {
            my str $final_name := ~@name[0];
            if $*WANTEDOUTERBLOCK {
                my $scope := $*WANTEDOUTERBLOCK;
                while $scope {
                    my %sym := $scope.symbol($final_name);
                    if +%sym {
                        my $value := self.force_value(%sym, $final_name, 1);
                        if $upgrade_to_global {
                            ($*GLOBALish.WHO){$final_name} := $value;
                        }
                        return $value;
                    }
                    $scope := $scope.ann('outer');
                }
            }
            else {
                my int $i := $start_scope;
                while $i > 0 {
                    $i := $i - 1;
                    my %sym := @BLOCKS[$i].symbol($final_name);
                    if +%sym {
                        my $value := self.force_value(%sym, $final_name, 1);
                        if $upgrade_to_global {
                            ($*GLOBALish.WHO){$final_name} := $value;
                        }
                        return $value;
                    }
                }
            }
            if nqp::existskey($cur-package.WHO, $final_name) {
                return nqp::atkey($cur-package.WHO, $final_name);
            }
        }

        # If it's a multi-part name, see if the containing package
        # is a lexical somewhere or can be found in the current
        # package. Otherwise we fall back to looking in GLOBALish.
        my $result := $*GLOBALish;
        if +@name >= 2 {
            my str $first := ~@name[0];
            my int $i := $start_scope;
            my int $found := 0;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @BLOCKS[$i].symbol($first);
                if +%sym {
                    $result := self.force_value(%sym, $first, 1);
                    @name := nqp::clone(@name);
                    @name.shift();
                    $i := 0;
                    $found := 1;
                }
            }
            unless $found {
                if nqp::existskey($cur-package.WHO, $first) {
                    $result := nqp::atkey($cur-package.WHO, $first);
                    @name := nqp::clone(@name);
                    @name.shift();
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

    # Takes a name and compiles it to a lookup for the symbol.
    method symbol_lookup(@name, $/, :$package_only = 0, :$lvalue = 0) {
        # Catch empty names and die helpfully.
        if +@name == 0 { $/.CURSOR.panic("Cannot compile empty name"); }
        my $orig_name := join('::', @name);

        # Handle fetching GLOBAL.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return QAST::Op.new( :op('getcurhllsym'),
                QAST::SVal.new( :value('GLOBAL') ) );
        }

        # Handle things starting with pseudo-package.
        if self.is_pseudo_package(@name[0]) && @name[0] ne 'GLOBAL' && @name[0] ne 'PROCESS' {
            my $lookup;
            for @name {
                if $lookup {
                    $lookup := QAST::Op.new( :op('who'), $lookup );
                }
                else {
                    # Lookups start at the :: root.
                    $lookup := QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :value(self.find_symbol(['PseudoStash'])) )
                    );
                }
                $lookup := QAST::Op.new(
                    :op('call'), :name('&postcircumfix:<{ }>'),
                    $lookup,
                    self.add_string_constant($_));
            }
            return $lookup;
        }

        # If it's a single item, then go hunting for it through the
        # block stack.
        my @BLOCKS := self.context().blocks;
        if +@name == 1 && !$package_only {
            my int $i := +@BLOCKS;
            my str $first_name := ~@name[0];
            while $i > 0 {
                $i := $i - 1;
                my %sym := @BLOCKS[$i].symbol($first_name);
                if +%sym {
                    return QAST::Var.new( :name($first_name), :scope(%sym<scope>) );
                }
            }
        }

        # The final lookup will always be just an AT-KEY call on a Stash.
        @name := nqp::clone(@name);
        my $final_name := @name.pop();
        my $lookup := QAST::Op.new(
            :op('callmethod'), :name('AT-KEY'),
            self.add_constant('Str', 'str', $final_name));

        # If there's no explicit qualification, then look it up in the
        # current package, and fall back to looking in GLOBAL.
        if +@name == 0 {
            $lookup.unshift(QAST::Op.new(
                :op('who'),
                QAST::Var.new( :name('$?PACKAGE'), :scope('lexical') )
            ));
        }

        # Otherwise, see if the first part of the name is lexically
        # known. If not, it's in GLOBAL. Also, if first part is GLOBAL
        # then strip it off.
        else {
            my $path := self.is_lexical(@name[0]) ??
                QAST::Var.new( :name(@name.shift()), :scope('lexical') ) !!
                QAST::Op.new( :op('getcurhllsym'),
                    QAST::SVal.new( :value('GLOBAL') ) );
            if @name[0] eq 'GLOBAL' {
                @name.shift();
            }
            for @name {
                $path := QAST::Op.new(
                    :op('callmethod'), :name('package_at_key'),
                    QAST::Op.new( :op('who'), $path ),
                    QAST::SVal.new( :value(~$_) ));
            }
            $lookup.unshift(QAST::Op.new(:op('who'), $path));
        }

        unless $lvalue {
            $lookup.push(QAST::WVal.new(
                :value(self.find_symbol(['Bool', 'True'])),
                :named('global_fallback')
            ));
        }

        return $lookup;
    }

    # Checks if the given name is known anywhere in the lexpad
    # and with lexical scope.
    method is_lexical(str $name) {
        self.context().is_lexical($name)
    }

    method suggest_lexicals($name) {
        my @suggestions;
        my @candidates := [[], [], []];
        my &inner-evaluator := make_levenshtein_evaluator($name, @candidates);
        my %seen;
        %seen{$name} := 1;
        sub evaluate($name, $value, $has_value, $hash) {
            # the descriptor identifies variables.
            return 1 unless nqp::existskey($hash, "scope");
            return 1 unless $hash<scope> eq 'lexical';
            return 1 if nqp::existskey(%seen, $name);
            %seen{$name} := 1;
            return &inner-evaluator($name, $hash);
        }
        self.walk_symbols(&evaluate);

        levenshtein_candidate_heuristic(@candidates, @suggestions);
        return @suggestions;
    }

    method suggest_routines($name) {
        my $with_sigil := nqp::eqat($name, '&', 0);
        $name := '&' ~ $name unless $with_sigil;
        my @suggestions;
        my @candidates := [[], [], []];
        my &inner-evaluator := make_levenshtein_evaluator($name, @candidates);
        my %seen;
        %seen{$name} := 1;

        # RT 126264
        # Since there's no programmatic way to get a list of all phasers
        # applicable to the current scope, just check against this list
        # of all of them that aren't already the names of routines
        for <&BEGIN &CHECK &INIT &ENTER &LEAVE &KEEP &UNDO &PRE &POST &CATCH &CONTROL> -> $phaser {
            &inner-evaluator($phaser, %seen);
        }

        sub evaluate($name, $value, $has_value, $hash) {
            return 1 unless nqp::eqat($name, '&', 0);
            return 1 if nqp::existskey(%seen, $name);

            %seen{$name} := 1;
            return &inner-evaluator($name, $hash);
        }
        self.walk_symbols(&evaluate);

        levenshtein_candidate_heuristic(@candidates, @suggestions);
        if !$with_sigil {
            my @no_sigils;  # can't do in-place $_ alteration
            for @suggestions {
                nqp::push( @no_sigils, nqp::substr($_,1,nqp::chars($_) - 1) );
            }
            @suggestions := @no_sigils;
        }
        if $name eq '&length' {
            @suggestions.push: $with_sigil ?? '&elems'  !! 'elems';
            @suggestions.push: $with_sigil ?? '&chars'  !! 'chars';
            @suggestions.push: $with_sigil ?? '&graphs' !! 'graphs';
            @suggestions.push: $with_sigil ?? '&codes'  !! 'codes';
        }
        elsif $name eq '&bytes' {
            @suggestions.push: '.encode($encoding).bytes';
        }
        return @suggestions;
    }


    # Checks if the symbol is really an alias to an attribute.
    method is_attr_alias(str $name) {
        self.context().is_attr_alias($name)
    }

    # Checks if a symbol is lexically visible relative to a given scope.
    # Returns 0 if it's not, 1 if it is, 2 if it's a type.
    method is_lexically_visible($name, $scope) {
        my $cur_block := $scope;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            if nqp::existskey(%symbols, $name) {
                my %sym := %symbols{$name};
                self.force_value(%sym, $name, 0);
                return nqp::existskey(%sym, 'value') &&
                    !nqp::isconcrete(%sym<value>) ?? 2 !! 1;
            }
            $cur_block := $cur_block.ann('outer');
        }
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

    # Adds various bits of initialization that must always be done early on.
    method add_initializations() {
        if self.is_precompilation_mode() {
            self.add_load_dependency_task(:deserialize_ast(QAST::Op.new( :op('null') )));
        }
    }

    # Constructs and immediately throws a typed exception. Note that if there
    # are extra sorrows or worries it will put them into a group.
    method throw($/, $ex_type, *%opts) {
        my $ex := self.typed_exception($/, $ex_type, |%opts);
        if @*SORROWS || @*WORRIES {
            $ex := self.group_exception($ex);
        }
        $ex.throw
    }

    # Builds an exception group.
    method group_exception(*@panic) {
        my %opts;
        %opts<panic> := @panic[0] if @panic;
        %opts<sorrows> := p6ize_recursive(@*SORROWS) if @*SORROWS;
        %opts<worries> := p6ize_recursive(@*WORRIES) if @*WORRIES;
        %opts<filename> := nqp::box_s(self.current_file,self.find_symbol(['Str']));
        try {
            my $group_type := self.find_symbol(['X', 'Comp', 'Group'], :setting-only);
            return $group_type.new(|%opts);
            CATCH {
                nqp::print("Error while constructing error object:");
                nqp::say($_);
            }
        }
    }

    # Tries to construct a typed exception, incorporating all available compile
    # time information (such as about location). Returns it provided it is able
    # to construct it. If that fails, dies right away.
    method typed_exception($/, $ex_type, *%opts) {
        my int $type_found := 0;
        my $ex;
        my $x_comp;
        unless $*COMPILING_CORE_SETTING {
            try {
                CATCH {
                    $type_found := 0;
                    nqp::print("Error while constructing error object:");
                    nqp::say($_);
                };
                $type_found := 1;
                $ex := self.find_symbol(
                    nqp::islist($ex_type) ?? $ex_type !! nqp::split('::', $ex_type),
                    :setting-only);
                my $x_comp := self.find_symbol(['X', 'Comp'], :setting-only);
                unless nqp::istype($ex, $x_comp) {
                    $ex := $ex.HOW.mixin($ex, $x_comp);
                }
            }
        }

        if $type_found {
            # If the highwater is beyond the current position, force the cursor to
            # that location.  (Maybe.)
            my $c := $/.CURSOR;
            my @expected;
            my $high := $c.'!highwater'();
            if %opts<precursor> {
                $c := $/.PRECURSOR;
            }
            elsif %opts<expected> {
                @expected := %opts<expected>;
            }
            elsif $high >= $c.pos() {
                my @raw_expected := $c.'!highexpect'();
                $c.'!cursor_pos'($high);
                my %seen;
                for @raw_expected {
                    unless %seen{$_} {
                        my $end := +@expected;
                        while $end && @expected[$end-1] gt $_ { $end := $end - 1 }
                        nqp::splice(@expected, [$_], $end, 0);
                        %seen{$_} := 1;
                    }
                }
            }

            # Try and better explain "Confused".
            my @locprepost := self.locprepost($c);
            if $ex.HOW.name($ex) eq 'X::Syntax::Confused' {
                if @locprepost[1] ~~ / ^ \s* <[ } ) \] > » ]> / {
                    %opts<reason> := "Unexpected closing bracket";
                    @expected := [];
                }
                elsif @locprepost[0] ~~ / \} \s* $ / {
                    %opts<reason> := "Strange text after block (missing semicolon or comma?)";
                }
                else {
                    my $expected_infix := 0;
                    my $expected_term := 0;
                    for @expected {
                        if nqp::index($_, "infix") >= 0 {
                            $expected_infix := 1;
                        }
                        elsif nqp::index($_, "term") >= 0 {
                            $expected_term := 1;
                        }
                    }
                    if $expected_infix {
                        if $expected_term {
                            %opts<reason> := "Bogus term";
                        }
                        elsif $*IN_META {
                            %opts<reason> := "Bogus infix";
                        }
                        elsif $c.MARKED('baresigil') {
                            %opts<reason> := "Name must begin with alphabetic character";
                        }
                        elsif @locprepost[1] ~~ / ^ \s* <[ $ @ \w ' " ]> / ||
                              @locprepost[1] ~~ / ^ \s+ <[ ( [ { « . ]> / {
                            %opts<reason> := "Two terms in a row";
                        }
                        elsif @locprepost[1] ~~ / ^ '<EOL>' / {
                            %opts<reason> := "Two terms in a row across lines (missing semicolon or comma?)";
                        }
                        elsif @locprepost[1] ~~ / ^ \S / {
                            %opts<reason> := "Bogus postfix";
                        }
                        # "Confused" is already the default, so no "else" clause needed here.
                    }
                    # or here...
                }
                my $qs := $*LASTQUOTE[0];
                my $qe := $*LASTQUOTE[1];
                if HLL::Compiler.lineof($c.orig, $qe, :cache(1)) >= HLL::Compiler.lineof($c.orig, $c.pos, :cache(1)) - 1
                    && nqp::index(nqp::substr($c.orig, $qs, $qe - $qs), "\n") >= 0 {
                    my $quotes :=
                        nqp::substr($c.orig, $qs - 1 , 1) ~
                        nqp::substr($c.orig, $qe, 1);
                    $quotes := "<<>>" if $quotes eq '<>' && nqp::substr($c.orig, $qe + 1, 1) eq '>';
                    %opts<reason> := %opts<reason> ~ " (runaway multi-line " ~ $quotes ~
                        " quote starting at line " ~ HLL::Compiler.lineof($c.orig, $qs, :cache(1)) ~ " maybe?)";
                }
            }

            # Build and throw exception object.
            %opts<line>            := HLL::Compiler.lineof($c.orig, $c.pos, :cache(1));
            # only set <pos> if it's not already set:
            %opts<pos>             := $c.pos unless nqp::existskey(%opts, 'pos');
            %opts<modules>         := p6ize_recursive(@*MODULES // []);
            %opts<pre>             := @locprepost[0];
            %opts<post>            := @locprepost[1];
            %opts<highexpect>      := p6ize_recursive(@expected) if @expected;
            %opts<is-compile-time> := 1;
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
            %opts<filename> := nqp::box_s(self.current_file,self.find_symbol(['Str']));
            try { return $ex.new(|%opts) };
        }

        my $Str;
        my $Int;
        my $List;
        my int $has_str;
        my int $has_int;
        my int $has_list;

        try { $Str := self.find_symbol(["Str"]); $has_str := 1 }
        try { $Int := self.find_symbol(["Int"]); $has_int := 1 }
        try { $List := self.find_symbol(["List"]); $has_list := 1 }

        sub safely_stringify($target) {
            if $has_str && nqp::istype($target, $Str) {
                return ~nqp::unbox_s($target);
            } elsif $has_int && nqp::istype($target, $Int) {
                return ~nqp::unbox_i($target);
            } elsif $has_list && nqp::istype($target, $List) {
                my $storage := nqp::getattr($target, $List, '$!reified');
                my @result;
                for $storage {
                    nqp::push(@result, safely_stringify($_));
                }
                return "(" ~ join(", ", @result) ~ ")";
            } elsif nqp::ishash($target) {
                my @result;
                for $target -> $key {
                    @result.push("\n") if +@result != 0;
                    @result.push("        '" ~ $key ~ "'");
                    @result.push(": ");
                    @result.push(safely_stringify($target{$key}));
                }
                return join('', @result);
            } elsif nqp::islist($target) {
                my @result;
                @result.push("(");
                for $target -> $val {
                    @result.push(",") if +@result != 1;
                    @result.push(safely_stringify($val));
                }
                @result.push(")");
                return join('', @result);
            } else {
                return (try { ~$target } // try { "(unstringifiable " ~ $target.HOW.name($target) ~ ")" } // '(unstringifiable object)' );
            }
        }

        my @err := ['Error while compiling, type ', (nqp::islist($ex_type) ?? join('::', $ex_type) !! $ex_type),  "\n"];
        for %opts -> $key {
            @err.push: '  ';
            @err.push: ~$key;
            @err.push: ': ';
            @err.push: safely_stringify(%opts{$key});
            @err.push: "\n";
        }
        nqp::findmethod(HLL::Grammar, 'panic')($/.CURSOR, join('', @err));
    }

    method locprepost($c) {
        my $orig := $c.orig;
        my $marked := $c.MARKED('ws');
        my $pos  := $marked && nqp::index(" }])>»", nqp::substr($orig, $c.pos, 1)) < 0 ?? $marked.from !! $c.pos;

        my $prestart := $pos - 40;
        $prestart := 0 if $prestart < 0;
        my $pre := nqp::substr($orig, $prestart, $pos - $prestart);
        $pre    := subst($pre, /.*\n/, "", :global);
        $pre    := '<BOL>' if $pre eq '';

        my $postchars := $pos + 40 > nqp::chars($orig) ?? nqp::chars($orig) - $pos !! 40;
        my $post := nqp::substr($orig, $pos, $postchars);
        $post    := subst($post, /\n.*/, "", :global);
        $post    := '<EOL>' if $post eq '';

        [$pre, $post]
    }

    method stash_hash($pkg) {
        my $hash := $pkg.WHO;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }

    method add_additional_frames($frames) {
        if %*COMPILING<%?OPTIONS><mast_frames> {
            my %existing := %*COMPILING<%?OPTIONS><mast_frames>;
            my $iterator := nqp::iterator($frames);
            while $iterator {
                my $pair := nqp::shift($iterator);
                %existing{nqp::iterkey_s($pair)} := nqp::iterval($pair);
            }
        }
        else {
            %*COMPILING<%?OPTIONS><mast_frames> := $frames;
        }
    }

    method ex-handle($/, $code) {
        my $res;
        my $ex;
        my int $nok;
        try {
            $res := $code();
            CATCH {
                $nok := 1;
                $ex  := $_;
            }
        }
        if $nok {
            self.rethrow($/, $ex);
        } else {
            $res;
        }
    }

    method handle-begin-time-exceptions($/, $use-case, $code) {
        my $ex;
        my int $nok;
        try {
            return $code();
            CATCH { $ex := $_; }
        }

        my int $success := 0;
        my $coercer;
        try { $coercer := self.find_symbol(['&COMP_EXCEPTION']); ++$success; };
        nqp::rethrow($ex) unless $success;
        my $p6ex := $coercer($ex);

        my int $found_xcbt := 0;
        my $x_comp_bt;
        try {
            $x_comp_bt := self.find_symbol(['X', 'Comp', 'BeginTime'], :setting-only);
            $found_xcbt++;
        }
        if $found_xcbt {
            my $xcbt := $x_comp_bt.new(exception => $p6ex, :$use-case);
            $xcbt.SET_FILE_LINE(
                nqp::box_s(self.current_file,self.find_symbol(['Str'])),
                nqp::box_i(self.current_line($/),self.find_symbol(['Int'])),
            );
            $xcbt.throw;
        }
        else {
            self.rethrow($/, $ex);
        }
    }

    method rethrow($/, $err) {
        my int $success := 0;
        my $coercer;
        try { $coercer := self.find_symbol(['&COMP_EXCEPTION']); ++$success; };
        nqp::rethrow($err) unless $success;
        my $p6ex := $coercer($err);
        unless nqp::can($p6ex, 'SET_FILE_LINE') {
            try {
                my $x_comp := self.find_symbol(['X', 'Comp'], :setting-only);
                $p6ex.HOW.mixin($p6ex, $x_comp).BUILD_LEAST_DERIVED(nqp::hash());
            }
        }
        if nqp::can($p6ex, 'SET_FILE_LINE') {
            $p6ex.SET_FILE_LINE(
                nqp::box_s(self.current_file,self.find_symbol(['Str'])),
                nqp::box_i(self.current_line($/),self.find_symbol(['Int'])),
            );
        }
        $p6ex.rethrow();
    }

    # Adds an object to this SC if it isn't already in one.
    method add_object_if_no_sc($obj) {
        if nqp::isnull(nqp::getobjsc($obj)) {
            self.add_object($obj);
        }
        $obj
    }

    method canonicalize_pair($k,$v) {
        if $v ~~ /<[ < > ]>/ && !($v ~~ /<[ « » $ \\ " ' ]>/) {
            ':' ~ $k ~ '«' ~ $v ~ '»'
        }
        else {
            my $new := '';
            my int $i := 0;
            my int $e := nqp::chars($v);
            while $i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
                ++$i;
            }
            ':' ~ $k ~ '<' ~ $new ~ '>';
        }
    }

    method record_precompilation_dependencies() {
        self.is_precompilation_mode && $!record_precompilation_dependencies
    }

    method suspend_recording_precompilation_dependencies() {
        $!record_precompilation_dependencies := 0;
    }

    method resume_recording_precompilation_dependencies() {
        $!record_precompilation_dependencies := 1;
    }
}

# vim: ft=perl6 expandtab sw=4
