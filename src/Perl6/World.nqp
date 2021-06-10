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
my $SIG_ELEM_TYPE_GENERIC        := 524288;
my $SIG_ELEM_DEFAULT_IS_LITERAL  := 1048576;
my $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
my $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
my $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
my $SIG_ELEM_SLURPY_ONEARG       := 16777216;
my $SIG_ELEM_CODE_SIGIL          := 33554432;
my int $SIG_ELEM_IS_COERCIVE     := 67108864;

sub p6ize_recursive($x) {
    if nqp::islist($x) {
        my @copy := [];
        for $x {
            nqp::push(@copy, p6ize_recursive($_));
        }
        return nqp::hllizefor(@copy, 'Raku');
    }
    elsif nqp::ishash($x) {
        my %copy := nqp::hash();
        for $x {
            %copy{$_.key} := p6ize_recursive($_.value);
        }
        return nqp::hllizefor(%copy, 'Raku').item;
    }
    nqp::hllizefor($x, 'Raku');
}

# Helper sub that turns a list of items into an NQPArray. This is needed when
# using e.g. $*W.find_symbol from Raku code (example: slangs).
sub nqplist(*@arr) { @arr }
nqp::bindcurhllsym('nqplist', &nqplist);

# this levenshtein implementation is used to suggest good alternatives
# when deriving from an unknown/typo'd class.
sub levenshtein($a, $b) {
    my %memo;
    my int $alen := nqp::chars($a);
    my int $blen := nqp::chars($b);

    return 0 if $alen == 0 || $blen == 0;

    sub changecost(str $ac, str $bc) {
        sub issigil($_) { nqp::index('$@%&|', $_) != -1 };
        return 0 if $ac eq $bc;
        return 0.1 if nqp::fc($ac) eq nqp::fc($bc);
        return 0.5 if issigil($ac) && issigil($bc);
        1;
    }

    sub levenshtein_impl(int $apos, int $bpos, num $estimate) {
        my $key := "$apos:$bpos";

        return %memo{$key} if nqp::existskey(%memo, $key);

        # if either cursor reached the end of the respective string,
        # the result is the remaining length of the other string.
        sub check(int $pos1, int $len1, int $pos2, int $len2) {
            if $pos2 == $len2 {
                return $len1 - $pos1;
            }
            -1;
        }

        my int $check := check($apos, $alen, $bpos, $blen);
        return $check unless $check == -1;
        $check := check($bpos, $blen, $apos, $alen);
        return $check unless $check == -1;

        my str $achar := nqp::substr($a, $apos, 1);
        my str $bchar := nqp::substr($b, $bpos, 1);

        my num $cost := changecost($achar, $bchar);

        # hyphens and underscores cost half when adding/deleting.
        my num $addcost := 1;
        $addcost := 0.5 if $bchar eq "-" || $bchar eq "_";

        my num $delcost := 1;
        $delcost := 0.5 if $achar eq "-" || $achar eq "_";

        my num $ca := nqp::add_n(levenshtein_impl($apos+1, $bpos,   nqp::add_n($estimate, $delcost)), $delcost); # what if we remove the current letter from A?
        my num $cb := nqp::add_n(levenshtein_impl($apos,   $bpos+1, nqp::add_n($estimate, $addcost)), $addcost); # what if we add the current letter from B?
        my num $cc := nqp::add_n(levenshtein_impl($apos+1, $bpos+1, nqp::add_n($estimate, $cost)), $cost); # what if we change/keep the current letter?

        # the result is the shortest of the three sub-tasks
        my num $distance;
        $distance := $ca if nqp::isle_n($ca, $cb) && nqp::isle_n($ca, $cc);
        $distance := $cb if nqp::isle_n($cb, $ca) && nqp::isle_n($cb, $cc);
        $distance := $cc if nqp::isle_n($cc, $ca) && nqp::isle_n($cc, $cb);

        # switching two letters costs only 1 instead of 2.
        if $apos + 1 <= $alen && $bpos + 1 <= $blen &&
           nqp::eqat($a, $bchar, $apos + 1) && nqp::eqat($b, $achar, $bpos + 1) {
            my num $cd := nqp::add_n(levenshtein_impl($apos+2, $bpos+2, nqp::add_n($estimate, 1)), 1);
            $distance := $cd if nqp::islt_n($cd, $distance);
        }

        %memo{$key} := $distance;
    }

    return levenshtein_impl(0, 0, 0e0);
}

sub make_levenshtein_evaluator($orig_name, @candidates) {
    my $Str-obj := $*W.find_single_symbol("Str", :setting-only);
    my $find-count := 0;
    my $try-count := 0;
    sub inner($name, $hash) {
        # difference in length is a good lower bound.
        $try-count := $try-count + 1;
        return 0 if $find-count > 20 || $try-count > 1000;
        my $parlen := nqp::chars($orig_name);
        my $lendiff := nqp::chars($name) - $parlen;
        $lendiff := -$lendiff if $lendiff < 0;
        return 1 if nqp::isge_n($lendiff, nqp::mul_n($parlen, 0.3));

        my num $dist := nqp::div_n(levenshtein($orig_name, $name), $parlen);
        my $target := -1;
        $target := @candidates[0] if nqp::isle_n($dist, 0.1);
        $target := @candidates[1] if nqp::islt_n(0.1, $dist) && nqp::isle_n($dist, 0.2);
        $target := @candidates[2] if nqp::islt_n(0.2, $dist) && nqp::isle_n($dist, 0.35);
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

# This builds upon the HLL::World to add the specifics needed by Rakudo.
class Perl6::World is HLL::World {

    my class Perl6CompilationContext is HLL::World::CompilationContext {
        # The stack of lexical pads, actually as QAST::Block objects. The
        # outermost frame is at the bottom, the latest frame is on top.
        has @!PADS;

        # The stack of QAST::Blocks together with the ones that are not
        # lexpads.
        # The outermost block is at the bottom, the latest block is on top.
        has @!PADS_AND_THUNKS;

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

        # Clean-up tasks, to do after CHECK time.
        has @!cleanup_tasks;

        # Cache of container info and descriptor for magicals.
        has %!magical_cds;

        has @!herestub_queue;

        method BUILD(:$handle, :$description) {
            @!PADS := [];
            @!PADS_AND_THUNKS := [];
            @!CODES := [];
            @!stub_check := [];
            @!protos_to_sort := [];
            %!sub_id_to_code_object := {};
            %!sub_id_to_cloned_code_objects := {};
            %!sub_id_to_sc_idx := {};
            %!const_cache := {};
            @!cleanup_tasks := [];
            %!magical_cds := {};
            @!herestub_queue := [];
        }

        method blocks() {
            @!PADS
        }

        method create_block($/) {
            # Create pad, link to outer, annotate with creating statement.
            my $pad := QAST::Block.new( QAST::Stmts.new( :node($/) ) );
            if $*WANTEDOUTERBLOCK {  # (outside of 1st push/pop pass)
                $pad.annotate('outer', $*WANTEDOUTERBLOCK);
            }
            elsif +@!PADS {
                $pad.annotate('outer', @!PADS[+@!PADS - 1]);
            }
            $pad.annotate('statement_id', $*STATEMENT_ID);
            $pad.annotate('in_stmt_mod', $*IN_STMT_MOD);
            $pad
        }

        # Creates a new lexical scope and puts it on top of the stack.
        method push_lexpad($/) {
            my $pad := self.create_block($/);
            @!PADS[+@!PADS] := $pad;
            @!PADS_AND_THUNKS[+@!PADS_AND_THUNKS] := $pad;
            $pad;
        }

        # Pops a lexical scope off the stack.
        method pop_lexpad() {
            @!PADS_AND_THUNKS.pop();
            @!PADS.pop();
        }

        # Gets the top lexpad.
        method cur_lexpad() {
            @!PADS[+@!PADS - 1]
        }

        # Creates a new thunk and puts it on top of the stack
        method push_thunk($/) {
            my $thunk := self.create_block($/);
            @!PADS_AND_THUNKS[+@!PADS_AND_THUNKS] := $thunk;
            $thunk;
        }

        # Pops a thunk off the stack
        method pop_thunk() {
            @!PADS_AND_THUNKS.pop();
        }

        # Gets the top block or thunk.
        method cur_block_or_thunk() {
            @!PADS_AND_THUNKS[+@!PADS_AND_THUNKS - 1]
        }

        # Marks the current lexpad as being a signatured block.
        method mark_cur_lexpad_signatured() {
            @!PADS[+@!PADS - 1].annotate('signatured', 1);
        }

        # Finds the nearest signatured block and checks if it declares
        # a certain symbol.
        method nearest_signatured_block_declares(str $symbol) {
            my $i := +@!PADS;
            while $i > 0 {
                $i := $i - 1;
                if @!PADS[$i].ann('signatured') {
                    return +@!PADS[$i].symbol($symbol);
                }
            }
        }

        # Marks all blocks upto and including one declaring a $*DISPATCHER as
        # being no-inline.
        method mark_no_inline_upto_dispatcher() {
            my $i := +@!PADS_AND_THUNKS;
            while $i > 0 {
                $i := $i - 1;
                my $block := @!PADS_AND_THUNKS[$i];
                $block.no_inline(1);
                last if $block.symbol('$*DISPATCHER');
            }
        }

        # Hunts through scopes to find the type of a lexical.
        method find_lexical_container_type(str $name) {
            my int $i := +@!PADS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!PADS[$i].symbol($name);
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
            my int $i := +@!PADS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!PADS[$i].symbol($name);
                if %sym {
                    return nqp::existskey(%sym, 'ro');
                }
            }
            0;
        }

        # Checks if the given name is known anywhere in the lexpad
        # and with lexical scope.
        method is_lexical(str $name) {
            my int $i := +@!PADS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!PADS[$i].symbol($name);
                if +%sym {
                    return %sym<scope> eq 'lexical' &&
                        # Make sure it's now a lowered away lexical from a flattened
                        # inner scope in an EVAL in the REPL
                        ($i > 0 || nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $name, 0) ||
                         !nqp::istype(
                             $*W.force_value(%sym, $name, 0),
                             $*W.find_symbol(['Rakudo', 'Internals', 'LoweredAwayLexical'])));
                }
            }
            0;
        }

        # Checks if the symbol is really an alias to an attribute.
        method is_attr_alias(str $name) {
            my int $i := +@!PADS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!PADS[$i].symbol($name);
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


        method herestub_queue() {
            @!herestub_queue
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

    has %!quote_lang_cache;

    # To temporarily keep fixup task QAST.
    has $!setting_fixup_task;

    has int $!in_unit_parse;
    has int $!have_outer;
    has int $!setting_loaded;
    has $!setting_name;
    has $!setting_revision;

    # List of CHECK blocks to run.
    has @!CHECKs;

    method BUILD(*%adv) {
        %!code_object_fixup_list := {};
        $!record_precompilation_dependencies := 1;
        %!quote_lang_cache := {};
        $!setting_loaded := 0;
        $!in_unit_parse := 0;
        @!CHECKs := [];
    }

    method lang-rev-before(str $want) {
        nqp::chars($want) == 1 || nqp::die(
          'Version to $*W.lang-rev-before'
            ~ " must be 1 char long ('c', 'd', etc). Got `$want`.");
        nqp::cmp_s(
          nqp::substr(nqp::getcomp('Raku').language_version, 2, 1),
          $want
        ) == -1
    }

    method setting_revision() {
        $!setting_revision
    }

    method in_unit_parse() {
        $!in_unit_parse
    }

    method add_check($check) {
        @!CHECKs := [] unless @!CHECKs;
        @!CHECKs.unshift($check);
    }

    method checks() {
        @!CHECKs
    }

    method !check-version-modifier($ver-match, $rev, $modifier, $comp) {
        my %lang_rev := $comp.language_revisions;

        unless nqp::existskey(%lang_rev, $rev) &&
               (!$modifier || nqp::existskey(%lang_rev{$rev}<mods>, $modifier)) {
            $ver-match.typed_panic: 'X::Language::Unsupported', version => ~$ver-match;
        }

        # See if requested revision is not supported without a modifier. Most likely it'll be PREVIEW modifier for
        # unreleased revisions.
        if nqp::existskey(%lang_rev{$rev}, 'require') {
            if nqp::iseq_s(%lang_rev{$rev}<require>, $modifier) {
                return;
            }
            $ver-match.typed_panic: 'X::Language::ModRequired',
                                    version => ~$ver-match,
                                    modifier => %lang_rev{$rev}<require>;
        }

        if %lang_rev{$rev}<mods>{$modifier}<deprecate> {
            $ver-match.PRECURSOR.worry("$modifier modifier is deprecated for Raku.$rev");
        }
    }

    method load-lang-ver($ver-match, $comp) {
        if $*INSIDE-EVAL && $!have_outer {
            # XXX Calling typed_panic is the desirable behavior. But it breaks some code. Just ignore version change for
            # now.
            # TODO? EVAL might get :unit parameter and simulate unit compilation.
            #$ver-match.typed_panic: 'X::Language::TooLate';
            return
        }
        $*MAIN   := 'MAIN';
        $*STRICT := 1 if $*begin_compunit;

        my str $version := ~$ver-match;
        my @vparts := nqp::split('.', $version);
        my $vWhatever := nqp::isge_i(nqp::index($version, '*'), 0);
        my $vPlus := nqp::isge_i(nqp::index($version, '+'), 0);
        my $default_rev := nqp::substr(nqp::gethllsym('default', 'SysConfig').rakudo-build-config()<language-version>, 2, 1);

        # Do we have dot-splitted version string?
        if !($vWhatever || $vPlus) &&
            ( ((@vparts > 1) && nqp::iseq_s(@vparts[0], 'v6'))
              || ($version eq 'v6') ) {
            my $revision := @vparts[1] || $default_rev;
            my $lang_ver := '6.' ~ $revision;

            self."!check-version-modifier"($ver-match, $revision, @vparts[2] || '', $comp);

            $comp.set_language_version: $lang_ver;
            # fast-path the common cases
            if $revision eq 'c' {
                $*CAN_LOWER_TOPIC := 0;
                # # CORE.c is currently our lowest core, which we don't "load"
                $!setting_name := 'CORE.c';
                return;
            }

            # Speed up loading assuming that the default language version would be the most used one.
            if $lang_ver eq nqp::gethllsym('default', 'SysConfig').rakudo-build-config()<language-version> {
                $!setting_name := 'CORE.' ~ $revision;
                # self.load_setting: $ver-match, 'CORE.' ~ $revision;
                return;
            }
        }

        my $Version := self.find_symbol: ['Version'];
        my $vWant   := $ver-match.ast.compile_time_value;
        my $vWantParts := $vWant.parts;
        my %lang_rev := $comp.language_revisions;
        unless $vWhatever || $vWant.plus {
            # It makes no sense checking for modifier when something like v6.* or v6.c+ is wanted.
            my $rev := $vWantParts.AT-POS(1);
            my str $rev_mod := $vWantParts.elems > 2 ?? $vWantParts.AT-POS(2) !! '';
            self."!check-version-modifier"($ver-match, $rev, $rev_mod, $comp);
        }

        my @can_ver_reversed;
        for $comp.can_language_versions { nqp::unshift(@can_ver_reversed, $_) }

        for @can_ver_reversed -> $can-ver {
            # Skip if tried version doesn't match the wanted one
            next unless $vWant.ACCEPTS: my $vCan := $Version.new: nqp::box_s($can-ver, self.find_single_symbol('Str', :setting-only));

            my $vCanElems := $vCan.parts.elems;
            my $can_rev := $vCan.parts.AT-POS: 1;

            # Skip if 2-part version tried now has a required modifier
            next if nqp::iseq_i($vCanElems, 2) && nqp::existskey(%lang_rev{$can_rev}, 'require');

            $comp.set_language_version: '6.' ~ $can_rev;
            $comp.set_language_modifier: $vCan.parts.AT-POS: 2 if $vCanElems > 2;

            if $can_rev eq 'c' {
                $*CAN_LOWER_TOPIC := 0;
                # CORE.c is our lowest core, which we don't "load"
            }
            $!setting_name := 'CORE.' ~ $can_rev;
            return;
        }

        $ver-match.typed_panic: 'X::Language::Unsupported', :$version;
    }

    method RAKUDO_MODULE_DEBUG() {
        if nqp::isconcrete($!RAKUDO_MODULE_DEBUG) {
            $!RAKUDO_MODULE_DEBUG
        }
        elsif !$*COMPILING_CORE_SETTING {
            $!RAKUDO_MODULE_DEBUG :=
              self.find_single_symbol('&DYNAMIC')('$*RAKUDO_MODULE_DEBUG')
        }
        else {
            $!RAKUDO_MODULE_DEBUG := False
        }
    }

    method comp_unit_stage0($/) {

        # Create unit outer (where we assemble any lexicals accumulated
        # from e.g. REPL) and the real UNIT.
        $*UNIT_OUTER := self.push_lexpad($/);
        $*UNIT       := self.push_lexpad($/);

        my $comp := nqp::getcomp('Raku');

        # If we already have a specified outer context, then that's
        # our setting. Otherwise, load one.
        $!have_outer := nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>);
        my $default_revision := nqp::substr($comp.language_version, 2, 1);
        my $default_setting_name := 'CORE.' ~ $default_revision;
        if $!have_outer {
            $!setting_name := $default_setting_name;
            $*UNIT.annotate('IN_DECL', 'eval');
        }
        else {
            $!setting_name := %*COMPILING<%?OPTIONS><setting> // $default_setting_name;
            if nqp::eqat($!setting_name, 'NULL.', 0) {
                $*COMPILING_CORE_SETTING := 1;
                $*SET_DEFAULT_LANG_VER := 0;
                $!setting_revision := nqp::substr($!setting_name, 5, 1);
                # Compile core with default language version unless the core revision is higher. I.e. when 6.d is the
                # default only core.e will be compiled with 6.e compiler.
                my $lang_ver := '6.' ~ (nqp::isgt_s($!setting_revision, $default_revision)
                                            ?? $!setting_revision
                                            !! $default_revision);
                nqp::getcomp('Raku').set_language_version($lang_ver);
            }
            $*UNIT.annotate('IN_DECL', 'mainline');
        }

        # Unit compilation started
        $!in_unit_parse := 1;
    }

    method comp_unit_stage1 ($/) {
        unless $!have_outer {
            self.load_setting($/, $!setting_name);
        }
        $/.unitstart();

        $!setting_loaded := 1;

        try {
            my $EXPORTHOW := self.find_single_symbol('EXPORTHOW');
            for self.stash_hash($EXPORTHOW) {
                $*LANG.set_how($_.key, $_.value);
            }
        }

        # Create GLOBAL(ish), unless we were given one.
        if nqp::existskey(%*COMPILING<%?OPTIONS>, 'global') {
            $*GLOBALish := %*COMPILING<%?OPTIONS><global>;
        }
        elsif $!have_outer && $*UNIT_OUTER.symbol('GLOBALish') {
            $*GLOBALish :=
              self.force_value($*UNIT_OUTER.symbol('GLOBALish'),'GLOBALish',1);
        }
        else {
            $*GLOBALish :=
              self.pkg_create_mo($/,$/.how('package'),:name('GLOBAL'));
            self.pkg_compose($/, $*GLOBALish);
        }

        # Create or pull in existing EXPORT.
        if $!have_outer && $*UNIT_OUTER.symbol('EXPORT') {
            $*EXPORT :=
              self.force_value($*UNIT_OUTER.symbol('EXPORT'), 'EXPORT', 1);
        }
        else {
            $*EXPORT := self.pkg_create_mo($/, $/.how('package'), :name('EXPORT'));
            self.pkg_compose($/, $*EXPORT);
        }

        # If there's a self in scope, set $*HAS_SELF.
        if $!have_outer && $*UNIT_OUTER.symbol('self') {
            $*HAS_SELF := 'complete';
        }

        # Take current package from outer context if any, otherwise for a
        # fresh compilation unit we start in GLOBAL.
        my $package;
        if $!have_outer && $*UNIT_OUTER.symbol('$?PACKAGE') {
            $package :=
              self.force_value($*UNIT_OUTER.symbol('$?PACKAGE'),'$?PACKAGE',1);
        }
        else {
            $package := $*GLOBALish;
        }
        $*PACKAGE := $package;
        $/.set_package($package);

        # If we're eval'ing in the context of a %?LANG, set up our own
        # %*LANG based on it.
        if $!have_outer && $*UNIT_OUTER.symbol('%?LANG') {
            for self.force_value(
              $*UNIT_OUTER.symbol('%?LANG'), '%?LANG', 1).FLATTENABLE_HASH() {
                %*LANG{$_.key} := $_.value;
            }
        }
        if $!have_outer && $*UNIT_OUTER.symbol('$*MAIN') {
            $*MAIN :=
              self.force_value($*UNIT_OUTER.symbol('$*MAIN'), '$*MAIN', 1);
        }
        if $!have_outer && $*UNIT_OUTER.symbol('$?STRICT') {
            $*STRICT :=
              self.force_value($*UNIT_OUTER.symbol('$*STRICT'), '$*STRICT', 1);
        }
        else {
            $*STRICT  := 1;
        }

        # Bootstrap
        # if $!setting_name eq 'NULL' {
        if nqp::eqat($!setting_name, 'NULL.', 0) {
            my $name   := "Perl6::BOOTSTRAP::v6" ~ nqp::substr($!setting_name, 5, 1) ;
            my $module := self.load_module_early($/, $name, $*GLOBALish);
            my $EXPORT := $module<EXPORT>.WHO;
            my @to_import := ['MANDATORY', 'DEFAULT'];
            for @to_import -> $tag {
                if nqp::existskey($EXPORT, $tag) {
                    self.import($/, self.stash_hash($EXPORT{$tag}), $name);
                }
            }
            for $module<EXPORTHOW>.WHO {
                my str $key := $_.key;
                $*LANG.set_how($key, nqp::decont($_.value));
            }
        }

        # Install as we've no setting, in which case we've likely no
        # static lexpad class yet either. Also, UNIT needs a code object.
        else {
            self.install_lexical_symbol($*UNIT, 'GLOBALish', $*GLOBALish);
            self.install_lexical_symbol($*UNIT, 'EXPORT', $*EXPORT);
            self.install_lexical_symbol($*UNIT, '$?PACKAGE', $package);
            self.install_lexical_symbol($*UNIT, '::?PACKAGE', $package);
            $*CODE_OBJECT := $*DECLARAND := self.stub_code_object('Block');

            unless $!have_outer {
                self.install_lexical_symbol(
                  $*UNIT,'$=finish',self.find_single_symbol('Mu', :setting-only));
            }
        }

        unless $!have_outer {
            my $M := %*COMPILING<%?OPTIONS><M>;
            if nqp::defined($M) {
                for nqp::islist($M) ?? $M !! [$M] -> $longname {
                    self.do_pragma_or_load_module($/,1,$longname);
                }
            }
        }

        self.add_load_dependency_task(:deserialize_ast($!setting_fixup_task), :fixup_ast($!setting_fixup_task));
    }

    method add_unit_marker($/, $name) {
        my $marker := self.pkg_create_mo($/, $/.how('package'), :$name);
        $marker.HOW.compose($marker);
        self.install_lexical_symbol($*UNIT, $name, $marker);
    }

    method mop_up_and_check($/) {

        # Unit has been parsed. Some services are not available past this point.
        $!in_unit_parse := 0;

        # Install POD-related variables.
        $*POD_PAST := self.add_constant(
            'Array', 'type_new', :nocache, |$*POD_BLOCKS
        );
        self.install_lexical_symbol(
            $*UNIT, '$=pod', $*POD_PAST.compile_time_value
        );

        # Tag UNIT with a magical lexical unless it is CORE.
        self.add_unit_marker($/, '!UNIT_MARKER') unless $*COMPILING_CORE_SETTING;
        self.add_unit_marker($/, '!EVAL_MARKER') if $*INSIDE-EVAL;

        # CHECK time.
        self.CHECK();

        # Clean up compiler services.
        if $!compiler_services {
            my $cs := $!compiler_services;
            nqp::bindattr($cs, $cs.WHAT, '$!compiler', nqp::null());
            nqp::bindattr($cs, $cs.WHAT, '$!current-match', nqp::null());
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

    # Creates a new thunk and puts it on top of the stack
    method push_thunk($/) {
        self.context().push_thunk($/)
    }

    # Pops a thunk off the stack.
    method pop_thunk() {
        self.context().pop_thunk()
    }

    # Push inner block

    method push_inner_block($block) {
        self.context().cur_block_or_thunk()[0].push($block);
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

    # Marks all blocks upto and including one declaring a $*DISPATCHER as
    # being no-inline.
    method mark_no_inline_upto_dispatcher() {
        self.context().mark_no_inline_upto_dispatcher()
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
        # We don't load setting for EVAL
        if $*INSIDE-EVAL && $!have_outer {
            return
        }
        # Do nothing for the NULL.c setting.
        if $setting_name ne 'NULL.c' {
            $setting_name := Perl6::ModuleLoader.transform_setting_name($setting_name);
            # Load it immediately, so the compile time info is available.
            # Once it's loaded, set it as the outer context of the code
            # being compiled unless being loaded as another core dependency.
            my $setting := %*COMPILING<%?OPTIONS><outer_ctx> :=
                            Perl6::ModuleLoader.load_setting($setting_name);

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
            $!setting_fixup_task := $fixup;

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
                        unless $/.know_how($pdecl) {
                            $/.typed_panic('X::EXPORTHOW::NothingToSupersede',
                                declarator => $pdecl);
                        }
                        if $/.know_how("U:$pdecl") {
                            $/.typed_panic('X::EXPORTHOW::Conflict',
                                declarator => $pdecl, directive => $key);
                        }
                        $*LANG.set_how($pdecl, $meta);
                        $*LANG.set_how("U:$pdecl", nqp::hash('SUPERSEDE', $meta));
                    }
                }
                elsif $key eq 'DECLARE' {
                    my %DECLARE := self.stash_hash($pair.value);
                    for %DECLARE {
                        my str $pdecl := $_.key;
                        my $meta  := nqp::decont($_.value);
                        if $/.know_how($pdecl) {
                            $/.typed_panic('X::EXPORTHOW::Conflict',
                                declarator => $pdecl, directive => $key);
                        }
                        $*LANG.set_how($pdecl, $meta);
                        $*LANG.set_how("U:$pdecl", nqp::hash('DECLARE', $meta));
                        self.add_package_declarator($/,$pdecl);
                    }
                }
                elsif $key eq 'COMPOSE' {
                    my %COMPOSE := self.stash_hash($pair.value);
                    $/.NYI('EXPORTHOW::COMPOSE');
                }
                else {
                    if $key eq nqp::lc($key) {
                        # Support legacy API, which behaves like an unchecked
                        # supersede.
                        # XXX Can give deprecation warning in the future, remove
                        # before 6.0.0.
                        $*LANG.set_how($key, nqp::decont($pair.value));
                    }
                    else {
                        $/.typed_panic('X::EXPORTHOW::InvalidDirective', directive => $key);
                    }
                }
            }).eager;
        }
    }

    method add_package_declarator($/, str $pdecl) {
        my $cursor := $/;

        # Compute name of grammar/action entry.
        my $canname := 'package_declarator:sym<' ~ $pdecl ~ '>';

        # Add to grammar if needed.
        unless nqp::can($cursor, $canname) {
            my role PackageDeclarator[$meth_name, $declarator] {
                token ::($meth_name) {
                    :my $*OUTERPACKAGE := self.package;
                    :my $*PKGDECL := $declarator;
                    :my $*LINE_NO := HLL::Compiler.lineof($cursor.orig(), $cursor.from(), :cache(1));
                    $<sym>=[$declarator] <.end_keyword> <package_def>
                    <.set_braid_from(self)>
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
            $actions := %*LANG<MAIN-actions> := $actions.HOW.mixin($actions,
                PackageDeclaratorAction.HOW.curry(PackageDeclaratorAction, $canname));
        }
        $cursor.define_slang("MAIN", $cursor.WHAT, $actions);
        $cursor.set_actions($actions);
        self.install_lexical_symbol(self.cur_lexpad(), '%?LANG', self.p6ize_recursive(%*LANG, :dynamic));

        $*LANG := $cursor;
        $*LEAF := $cursor;
    }

    method do_import($/, $handle, $package_source_name, $arglist?) {
        my $EXPORT := $handle.export-package;
        if nqp::defined($EXPORT) {
            $EXPORT := $EXPORT.FLATTENABLE_HASH();
            my @to_import := ['MANDATORY'];
            my @positional_imports := [];
            if nqp::defined($arglist) {
                my $Pair := self.find_single_symbol('Pair', :setting-only);
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
                my $Map := self.find_single_symbol('Map', :setting-only);
                if nqp::istype($result, $Map) {
                    my $storage := $result.hash.FLATTENABLE_HASH();
                    self.import($/, $storage, $package_source_name, :need-decont(!(nqp::what($result) =:= $Map)));
#                    $/.check_LANG_oopsies("do_import");
                }
                else {
                    nqp::die("&EXPORT sub did not return a Map");
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

    # pragmas without args that just set_pragma to true
    my %just_set_pragma := nqp::hash(
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

    my %isms := nqp::hash(
      'Perl5',   'p5isms',
      'C++',     'c++isms',
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
            $*LANG.set_pragma($name, $on);
        }
        elsif $name eq 'MONKEY' {
            $*LANG.set_pragma($_.key, $on) if nqp::eqat($_.key,'MONKEY',0) for %just_set_pragma;
        }
        elsif $name eq 'strict' {
            if nqp::islist($arglist) {
                self.throw($/, 'X::Pragma::NoArgs', :$name)
            }
            $*STRICT  := $on;
        }
        elsif $name eq 'fatal' {
            if nqp::islist($arglist) {
                self.throw($/, 'X::Pragma::NoArgs', :$name)
            }
            $*FATAL  := $on;  # (have to hoist this out of its statementlist so blockoid actions see it)
            $*LANG.set_pragma($name, $on);
        }
        elsif $name eq 'soft' {
            # This is an approximation; need to pay attention to
            # argument list really.
            $*LANG.set_pragma('soft', $on);
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

            my $Pair := self.find_single_symbol('Pair', :setting-only);
            my $Bool := self.find_single_symbol('Bool', :setting-only);
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
                            $*LANG.set_pragma($name, $type);
                            next;
                        }
                        elsif $type eq '_' {
                            # XXX shouldn't know this
                            nqp::deletekey($*LANG.slangs,$name);
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
                my $registry := self.find_symbol(['CompUnit', 'RepositoryRegistry'], :setting-only);
                my $io-path  := self.find_symbol(['IO', 'Path'], :setting-only);
                for $arglist -> $arg {
                    if $arg {
                        $registry.use-repository($registry.repository-for-spec(
                            nqp::istype($arg, $io-path) ?? $arg.absolute !! $arg
                        ));
                    }
                    else {
                        self.throw($/, 'X::LibEmpty');
                    }
                }
            }
            else {
                self.throw($/, 'X::LibNone');
            }
        }
        elsif $name eq 'isms' {
            if nqp::islist($arglist) {
                my @huh;
                for $arglist -> $ism {
                    if nqp::atkey(%isms,$ism) -> $value {
                        $*LANG.set_pragma($value, $on);
                    }
                    else {
                        nqp::push(@huh,$ism)
                    }
                }
                if @huh {
                    self.throw($/, 'X::AdHoc',
                      payload => "Don't know how to handle: isms <"
                        ~ nqp::join(" ",@huh)
                        ~ ">"
                    )
                }
            }
            else {
                $*LANG.set_pragma($_.value, $on) for %isms;
            }
        }
        elsif $name eq 'dynamic-scope' {
            unless $on {
                self.throw($/, 'X::Pragma::CannotWhat', :what<no>, :$name);
            }
            if nqp::islist($arglist) && nqp::elems($arglist) {
                # Just some variables.
                my %dyn;
                for $arglist {
                    %dyn{$_} := 1;
                }
                $*LANG.set_pragma('dynamic-scope', sub ($var) { %dyn{$var} || 0 });
            }
            else {
                # All variables.
                $*LANG.set_pragma('dynamic-scope', sub ($var) { 1 });
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
        my $DEPRECATED := self.find_single_symbol('&DEPRECATED');
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
        elsif $/.pragma($pragma) -> $default {
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
                        $*W.resolve_mo($/, 'definite'), self.find_single_symbol('Any', :setting-only),
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
            my $result := self.compile_time_evaluate($/,$expr.ast);
            CATCH {
                $/.panic("Could not evaluate arguments");
            }
            $arglist := $result.List.FLATTENABLE_LIST;
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
            # Workaround: P5foo modules rely on dynamic scoping of either $_ or
            # of all the things.
            if nqp::eqat($name, 'P5', 0) {
                $*CAN_LOWER_TOPIC := 0;
                self.do_pragma($/, 'dynamic-scope', 1, NQPMu);
            }
        }
        else {
            nqp::die("Don't know how to 'no $name' just yet");
        }
    }

    # Loads a module immediately, and also makes sure we load it
    # during the deserialization.
    method load_module_early($/, $module_name, $cur_GLOBALish) {
        my $RMD := self.RAKUDO_MODULE_DEBUG;
        $RMD("  Early loading '$module_name'") if $RMD;

        # Immediate loading.
        my $line   := self.current_line($/);
        my $module := nqp::gethllsym('Raku', 'ModuleLoader').load_module($module_name, {},
            $cur_GLOBALish, :$line);

        # During deserialization, ensure that we get this module loaded.
        if self.is_precompilation_mode() {
            $RMD("  Pre-compiling '$module_name'") if $RMD;
            self.add_load_dependency_task(:deserialize_ast(QAST::Stmts.new(
                self.perl6_module_loader_code(),
                QAST::Op.new(
                   :op('callmethod'), :name('load_module'),
                   QAST::Op.new( :op('getcurhllsym'),
                        QAST::SVal.new( :value('ModuleLoader') ) ),
                   QAST::SVal.new( :value($module_name) ),
                   QAST::Op.new( :op('hash') ),
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
        my $true := self.find_single_symbol('True', :setting-only);
        my $spec := self.find_symbol(['CompUnit', 'DependencySpecification'], :setting-only).new(
            :short-name($module_name),
            :from(%opts<from> // 'Perl6'),
            :auth-matcher(%opts<auth> // $true),
            :api-matcher(%opts<api> // $true),
            :version-matcher(%opts<ver> // $true),
        );
        self.add_object_if_no_sc($spec);
        my $registry := self.find_symbol(['CompUnit', 'RepositoryRegistry'], :setting-only);
        my $comp_unit := $registry.head.need($spec);
        my $globalish := $comp_unit.handle.globalish-package;
        nqp::gethllsym('Raku','ModuleLoader').merge_globals_lexically(self, $cur_GLOBALish, $globalish);

        CATCH {
            self.rethrow($/, $_);
        }

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
                    :moar(QAST::SVal.new( :value('ModuleLoader.moarvm') )),
                    :js(QAST::SVal.new( :value('ModuleLoader') ))
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
    method import($/, %stash, $source_package_name, :$need-decont = 0) {
        # What follows is a two-pass thing for historical reasons.
        my $target := self.cur_lexpad();

        # First pass: QAST::Block symbol table installation. Also detect any
        # outright conflicts, and handle any situations where we need to merge.
        my %to_install;
        my @clash;
        my @clash_onlystar;
        for sorted_keys(%stash) -> $key {
            my $value := %stash{$key};
            if $need-decont && nqp::islt_i(nqp::index('$&', nqp::substr($key,0,1)),0) {
                $value := nqp::decont($value);
            }
            if $target.symbol($key) -> %sym {
                # There's already a symbol. However, we may be able to merge
                # if both are multis and have onlystar dispatchers.
                my $installed := %sym<value>;
                my $foreign := $value;
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
                        self.install_lexical_symbol($target, $key, $installed, :clone(1));

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
                        nqp::push(@clash_onlystar, $key);
                    }
                }
                else {
                    nqp::push(@clash, $key);
                }
            }
            else {
                $target.symbol($key, :scope('lexical'), :value($value));
                $target[0].push(QAST::Var.new(
                    :scope('lexical'), :name($key), :decl('static'), :value($value)
                ));
                %to_install{$key} := $value;
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
        for sorted_keys(%to_install) -> $key {
            my $v := %to_install{$key};
            self.add_object_if_no_sc($v);
            my $categorical := match($key, /^ '&' (\w+) [ ':<' (.+) '>' | ':«' (.+) '»' ] $/);
            if $categorical {
                $/.add_categorical(~$categorical[0], ~$categorical[1],
                    ~$categorical[0] ~ self.canonicalize_pair('sym',$categorical[1]),
                    nqp::substr($key, 1), $v);
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
        my $longname := $package =:= $*GLOBALish ?? '' !! $package.HOW.name($package);
        if +@parts {
            try {
                $cur_pkg := self.find_single_symbol(@parts[0], :upgrade_to_global($create_scope ne 'my'));
                $cur_lex := 0;
                $create_scope := 'our';
                $longname := $longname ?? $longname ~ '::' ~ @parts.shift() !! @parts.shift();
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
            try { self.find_single_symbol($name, :upgrade_to_global); }

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
            self.add_object_if_no_sc($obj);
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
    method install_lexical_container($block, str $name, %cont_info, $descriptor,
            :$scope, :$package, :$cont = self.build_container_and_add_to_sc(%cont_info, $descriptor),
            :$init_removal) {
        # Add to block, if needed. Note that it doesn't really have
        # a compile time value.
        my $var;
        if $block.symbol($name) {
            for @($block[0]) {
                if nqp::istype($_, QAST::Var) && $_.name eq $name {
                    $var := $_;
                    $var.returns(%cont_info<bind_constraint>);
                    $var.annotate('our_decl', 1) if $scope eq 'our';
                    last;
                }
            }
        }
        else {
            $var := QAST::Var.new(
                :scope('lexical'), :name($name), :decl('var'),
                :returns(%cont_info<bind_constraint>)
            );
            $var.annotate('our_decl', 1) if $scope eq 'our';
            $block[0].unshift($var);
        }
        $block.symbol($name, :scope('lexical'), :type(%cont_info<bind_constraint>), :descriptor($descriptor));

        # If it's a native type, no container as we inline natives straight
        # into registers. Do need to take care of initial value though.
        my $prim := %cont_info<sigil> eq '$' && nqp::objprimspec($descriptor.of);
        if $prim {
            if $scope eq 'state' { nqp::die("Natively typed state variables not yet implemented") }
            my $init;
            if $prim == 1 || $prim == 4 || $prim == 5 {
                $init := QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    QAST::IVal.new( :value(0) ) );
            }
            elsif $prim == 2 {
                $init := QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    $*W.lang-rev-before('d')
                      ?? QAST::Op.new(:op<nan>)
                      !! QAST::NVal.new(:value(0e0))
                );
            }
            elsif $prim == 3 {
                $init := QAST::Op.new( :op('bind'),
                    QAST::Var.new( :scope('lexical'), :name($name) ),
                    QAST::SVal.new( :value('') ) );
            }
            $block[0].push($init);
            if $init_removal {
                $init_removal.annotate('init_removal', -> {
                    $init.shift;
                    $init.shift;
                    $init.op('null');
                });
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

    # Mark a container as being implicitly used lexically (to prevent it
    # being optimized away).
    method mark_lexical_used_implicitly($block, str $name) {
        for @($block[0]) {
            if nqp::istype($_, QAST::Var) && $_.name eq $name {
                $_.annotate('lexical_used_implicitly', 1);
                last;
            }
        }
    }

    # Creates a new container descriptor and adds it to the SC.
    method create_container_descriptor($of, $name, $default = $of, $dynamic = is_dynamic($name)) {
        my $cd_type_name := nqp::eqaddr($of, self.find_single_symbol('Mu', :setting-only))
            ?? ['ContainerDescriptor', 'Untyped']
            !! ['ContainerDescriptor'];
        my $cd_type := self.find_symbol($cd_type_name, :setting-only);
        my $cd := $cd_type.new( :$of, :$name, :$default, :$dynamic );
        self.add_object_if_no_sc($cd);
        $cd
    }

    sub is_dynamic($name) {
        # Dynamic if has the * twigil.
        if nqp::chars($name) > 2 && nqp::eqat($name, '*', 1) {
            1
        }
        # Otherwise, check pragma.
        else {
            my $dynprag := $*LANG.pragma('dynamic-scope');
            $dynprag ?? $dynprag($name) !! 0
        }
    }

    # Builds a container.
    method build_container(%cont_info, $descriptor) {
        my $cont;
        my $cont_type := %cont_info<container_type>;
        if %cont_info<build_ast> {
            $cont := $cont_type;
        }
        elsif nqp::istype($cont_type, self.find_single_symbol('Scalar', :setting-only)) {
            $cont := nqp::create($cont_type);
            nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
            if nqp::existskey(%cont_info, 'scalar_value') {
                nqp::bindattr($cont, %cont_info<container_base>, '$!value',
                    %cont_info<scalar_value>);
            }
        }
        elsif nqp::istype($cont_type, self.find_single_symbol('Array', :setting-only)) {
            $cont := nqp::create($cont_type);
            nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
            my $List := self.find_single_symbol('List', :setting-only);
            my $Mu := self.find_single_symbol('Mu', :setting-only);
            nqp::bindattr($cont, $List, '$!reified', $Mu);
            nqp::bindattr($cont, $List, '$!todo', $Mu);
        }
        elsif nqp::istype($cont_type, self.find_single_symbol('Hash', :setting-only)) {
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
        self.add_object_if_no_sc($cont);
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
                @value_type ?? @value_type[0] !! self.find_single_symbol('Mu', :setting-only),
                $con);
        }
        if $sigil eq '@' {
            if @cont_type {
                %info<bind_constraint> := @cont_type[0];
                %info<container_base> := @cont_type[0];
            }
            else {
                %info<bind_constraint> := self.find_single_symbol('Positional', :setting-only);
                my $base_type_name     := nqp::objprimspec(@value_type[0]) ?? 'array' !! 'Array';
                %info<container_base>  := self.find_single_symbol($base_type_name, :setting-only);
            }
            if @value_type {
                my $vtype              := @value_type[0];
                %info<container_type>  := self.parameterize_type_with_args($/,
                    %info<container_base>, [$vtype], nqp::hash());
                %info<bind_constraint> := self.parameterize_type_with_args($/,
                    %info<bind_constraint>, [$vtype], nqp::hash());
                %info<value_type>      := $vtype;
                %info<default_value>   := self.maybe-nominalize: $vtype;
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := self.find_single_symbol('Mu', :setting-only);
                %info<default_value>  := self.find_single_symbol('Any', :setting-only);
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
                %info<container_base>  := self.find_single_symbol('Hash', :setting-only);
                %info<bind_constraint> := self.find_single_symbol('Associative', :setting-only);
            }
            if $shape {
                @value_type[0] := self.find_single_symbol('Any', :setting-only) unless +@value_type;
                my $shape_ast := $shape[0].ast;
                if nqp::istype($shape_ast, QAST::Stmts) {
                    if +@($shape_ast) == 1 {
                        if $shape_ast[0].has_compile_time_value {
                            @value_type[1] := $shape_ast[0].compile_time_value;
                        } elsif nqp::istype(
                          (my $op_ast := $shape_ast[0]), QAST::Op) {
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
                %info<default_value>
                    := self.maybe-nominalize: @value_type[0];
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := self.find_single_symbol('Mu', :setting-only);
                %info<default_value>  := self.find_single_symbol('Any', :setting-only);
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
            %info<container_base>  := self.find_single_symbol('Scalar', :setting-only);
            %info<container_type>  := %info<container_base>;
            %info<bind_constraint> := self.find_single_symbol('Callable', :setting-only);
            if @value_type {
                %info<bind_constraint> := self.parameterize_type_with_args($/,
                    %info<bind_constraint>, [@value_type[0]], nqp::hash());
            }
            %info<value_type>     := %info<bind_constraint>;
            %info<default_value>  := self.find_single_symbol('Callable', :setting-only);
            %info<scalar_value>   := self.find_single_symbol('Callable', :setting-only);
        }
        else {
            if @cont_type {
                self.throw($/, 'X::NYI',
                  :feature('is trait on $-sigil variable'),
                  :did-you-mean("my {@cont_type[0].HOW.name(@cont_type[0])} $sigil{~$<variable><desigilname>}")
                );
            }
            %info<container_base>     := self.find_single_symbol('Scalar', :setting-only);
            %info<container_type>     := %info<container_base>;
            if @value_type {
                %info<bind_constraint> := @value_type[0];
                %info<value_type>      := @value_type[0];
                %info<default_value>
                    := self.maybe-nominalize: @value_type[0];
            }
            else {
                %info<bind_constraint> := self.find_single_symbol('Mu', :setting-only);
                %info<value_type>      := self.find_single_symbol('Mu', :setting-only);
                %info<default_value>   := self.find_single_symbol('Any', :setting-only);
            }
            %info<scalar_value> := %info<default_value>;
        }
        %info
    }

    method maybe-definite-how-base($v) {
        # returns the value itself, unless it's a DefiniteHOW, in which case,
        # it returns its base type. Behaviour available in 6.d and later only.
        ! self.lang-rev-before('d') && nqp::eqaddr($v.HOW,
            self.find_symbol: ['Metamodel','DefiniteHOW'], :setting-only
        ) ?? $v.HOW.base_type: $v !! $v
    }

    method maybe-nominalize($v) {
        # If type does LanguageRevision then check what language it was created with. Otherwise base decision on the
        # current compiler.
        my $v-how := $v.HOW;
        !$v-how.archetypes.coercive
        && (nqp::can($v-how, 'lang-rev-before') ?? $v-how.lang-rev-before($v, 'e') !! self.lang-rev-before('e'))
            ?? self.maybe-definite-how-base($v)
            !! ($v.HOW.archetypes.nominalizable
                ?? $v.HOW.nominalize($v)
                !! $v)
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
            my $Mu     := self.find_single_symbol('Mu', :setting-only);
            my $WHAT   := self.find_single_symbol($name eq '$_' ?? 'Any' !! 'Nil', :setting-only);
            my $Scalar := self.find_single_symbol('Scalar', :setting-only);

            my %info := nqp::hash(
                'container_base',  $Scalar,
                'container_type',  $Scalar,
                'bind_constraint', $Mu,
                'value_type',      $Mu,
                'default_value',   $WHAT,
                'scalar_value',    $WHAT,
            );
            my $dynamic := $*W.lang-rev-before('d') || $name ne '$_';
            my $desc := self.create_container_descriptor($Mu, $name, $WHAT, $dynamic);

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
        self.handle_OFTYPE_for_pragma($var,'variables');
        my %cont_info  := self.container_type_info(NQPMu, $var<sigil>,
            $*OFTYPE ?? [$*OFTYPE.ast] !! [], []);
        %cont_info<value_type> := self.find_single_symbol('Any', :setting-only);
        my $descriptor := self.create_container_descriptor(%cont_info<value_type>, $name);

        nqp::die("auto_declare_var") unless nqp::objectid($*PACKAGE) == nqp::objectid($*LEAF.package);
        self.install_lexical_container($BLOCK, $name, %cont_info, $descriptor,
            :scope('our'), :package($*LANG.package));

        if nqp::istype($varast, QAST::Var) {
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
        my $par_type  := self.find_single_symbol('Parameter', :setting-only);
        my $parameter := nqp::create($par_type);
        self.add_object_if_no_sc($parameter);
        my $paramerter_type := %param_info<type>;

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
        if %param_info<type_generic> {
            $flags := $flags + $SIG_ELEM_TYPE_GENERIC;
        }
        if $paramerter_type.HOW.archetypes.coercive {
            $flags := $flags + $SIG_ELEM_IS_COERCIVE;
        }
        if %param_info<default_is_literal> {
            $flags := $flags + $SIG_ELEM_DEFAULT_IS_LITERAL;
        }
        my $primspec := nqp::objprimspec(%param_info<type>);
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
        nqp::bindattr($parameter, $par_type, '$!type', %param_info<type>);
        nqp::bindattr_i($parameter, $par_type, '$!flags', $flags);
        if %param_info<named_names> {
            nqp::bindattr($parameter, $par_type, '@!named_names', %param_info<named_names>);
        }
        if %param_info<type_captures> {
            nqp::bindattr($parameter, $par_type, '@!type_captures', %param_info<type_captures>);
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
            my $package := nqp::istype($/,NQPMu) ?? $*LEAF.package !! $/;
            unless @params[0]<is_invocant> {
                @params.unshift(hash(
                    type => $invocant_type,
                    is_invocant => 1,
                    is_multi_invocant => 1
                ));
            }
            unless has_named_slurpy_or_capture(@params) {
                unless nqp::can($package.HOW, 'hidden') && $package.HOW.hidden($package) {
                    @params.push(hash(
                        variable_name => '%_',
                        type => self.find_single_symbol('Mu', :setting-only),
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
        my $default_type := self.find_single_symbol($default_type_name);
        my $param_type := self.find_single_symbol('Parameter', :setting-only);
        my @param_objs;
        my %seen_names;
        for @params {
            # Set default nominal type, if we lack one.
            unless nqp::existskey($_, 'type') {
                $_<type> := $default_type;
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

            # Create parameter object and apply any traits.
            my $param_obj := self.create_parameter($/, $_);
            self.apply_traits($_<traits>, $param_obj) if $_<traits>;

            # Add variable as needed.
            my int $flags := nqp::getattr_i($param_obj, $param_type, '$!flags');
            my $varname := $_<variable_name>;
            if $varname && ($flags +& $SIG_ELEM_IS_RW || $flags +& $SIG_ELEM_IS_COPY) {
                my %sym := $lexpad.symbol($varname);
                if +%sym && !nqp::existskey(%sym, 'descriptor') {
                    my $type := $_<type>;
                    my $desc := self.create_container_descriptor($type, $varname);
                    $_<container_descriptor> := $desc;
                    nqp::bindattr($param_obj, $param_type, '$!container_descriptor', $desc);
                    $lexpad.symbol($varname, :descriptor($desc), :$type);
                }
            }

            # If it's natively typed and we got "is rw" set, need to mark the
            # container as being a lexical ref.
            if $varname && nqp::objprimspec($_<type>) {
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
        my $sig_type   := self.find_single_symbol('Signature', :setting-only);
        my $signature  := nqp::create($sig_type);
        my @parameters := %signature_info<parameter_objects>;
        self.add_object_if_no_sc($signature);

        # Set parameters.
        nqp::bindattr($signature, $sig_type, '@!params', @parameters);
        if nqp::existskey(%signature_info, 'returns') {
            nqp::bindattr($signature, $sig_type, '$!returns', %signature_info<returns>);
        }

        # Compute arity and count.
        my $p_type    := self.find_single_symbol('Parameter', :setting-only);
        my int $arity := 0;
        my int $count := 0;
        my int $n     := nqp::elems(@parameters);
        my int $i     := -1;
        while ++$i < $n {
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

    method compile_time_evaluate($/, $ast, :$mark-wanted) {
        return $ast.compile_time_value if $ast.has_compile_time_value;
        my $thunk := self.create_thunk($/, $ast, :$mark-wanted);
        $thunk();
    }

    # Turn a QAST tree into a code object, to be called immediately.
    method create_thunk($/, $to_thunk, $block = self.context().create_block($/), :$mark-wanted) {
        # XXX TODO: Wantedness fixes warnings in RT#131305, but perhaps
        # it's safe to not install the block in the first place? (old attempt
        # to do so caused JVM breakage mentioned in the ticket)
        $to_thunk.wanted: 1 if $mark-wanted;
        $block.push($to_thunk);
        self.create_code_obj_and_add_child($block, 'Code');
    }

    # Creates a simple code object with an empty signature
    # Also makes the passed block a child of outer block on lexpad
    method create_code_obj_and_add_child($block, $type) {
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
        my $type_obj := self.find_single_symbol($type, :setting-only);
        my $code     := nqp::create($type_obj);
        self.context().push_code_object($code);
        self.add_object_if_no_sc($code);
        $code
    }

    # Attaches a signature to a code object, and gives the
    # signature its backlink to the code object.
    method attach_signature($code, $signature) {
        my $code_type := self.find_single_symbol('Code', :setting-only);
        my $sig_type := self.find_single_symbol('Signature', :setting-only);
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
        my $code_type    := self.find_single_symbol('Code', :setting-only);
        my $routine_type := self.find_single_symbol('Routine', :setting-only);

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
            nqp::bindhllsym('Raku', 'GLOBAL', $*GLOBALish);

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

            unless nqp::getcomp('Raku').backend.name eq 'js' {
                # Temporarly disabled for js untill we figure the bug out
                my $code_obj := nqp::getcodeobj(nqp::curcode());
                unless nqp::isnull($code_obj) {
                    return $code_obj(|@pos, |%named);
                }
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
                    self.add_object_if_no_sc($clone);
                    self.context().add_cleanup_task(sub () {
                        nqp::bindattr($clone, $code_type, '@!compstuff', nqp::null());
                    });
                    self.context().add_clone_for_cuid($clone, $cuid);
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
        }

        # Stash the QAST block in the comp stuff.
        @compstuff[0] := $code_past;

        # If this is a dispatcher, install dispatchee list that we can
        # add the candidates too.
        if $is_dispatcher {
            nqp::bindattr($code, $routine_type, '@!dispatchees', []);
        }

        # Set yada flag if needed.
        if $yada {
            $code.set_yada;
        }

        # If it's a routine, store the package to make backtraces nicer.
        if nqp::istype($code, $routine_type) {
            nqp::die("finish_code_object") unless nqp::objectid($*PACKAGE) == nqp::objectid($*LEAF.package);
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
        self.add_object_if_no_sc($quasi_ast);
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
                    QAST::WVal.new( :value(self.find_single_symbol('AST', :setting-only)) )
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
        my $block_type := self.find_single_symbol('Block', :setting-only);
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
                        nqp::existskey(%phasers, 'QUIT') || nqp::existskey(%phasers, 'CLOSE') {
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
            QAST::Op.new(
              :op('create'),
              QAST::WVal.new( :value(self.find_single_symbol('IterationBuffer', :setting-only))),
            )));
        $block.symbol($value_stash, :scope('lexical'));

        # Create a phaser block that will do the restoration.
        my $phaser_block := self.push_lexpad($/);
        self.pop_lexpad();
        $phaser_block.push(QAST::Op.new(
            :op('while'),
            QAST::Op.new(
                :op('elems'),
                QAST::Var.new( :name($value_stash), :scope('lexical') )
            ),
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
        self.add_object_if_no_sc($derived);
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
        my $scalar_type := self.find_single_symbol('Scalar', :setting-only);
        my $scalar      := nqp::create($scalar_type);
        self.add_object_if_no_sc($scalar);
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

        # Create outer lexical contexts with all symbols visible. Maybe
        # we can be a bit smarter here some day. But for now we just make a
        # single frame and copy all the visible things into it.
        $wrapper.annotate('DYN_COMP_WRAPPER', 1);
        my %seen;
        my $mu        := try { self.find_single_symbol('Mu', :setting-only) };
        my $cur_block := $past;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            for sorted_keys(%symbols) -> str $name {
                # For now, EVALed code run during precomp will not get the
                # outer lexical context's symbols as those may contain or
                # reference unserializable objects leading to compilation
                # failures. Needs a smarter approach as noted above.
                if !%seen{$name} && ($name eq '$_' || !self.is_nested()) {
                    # Add symbol.
                    my %sym   := %symbols{$name};
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
            :hll('Raku'),
            :sc(self.sc()),
            :compilation_mode(0),
            $wrapper
        );
        my $comp := nqp::getcomp('Raku');
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
        my $result;
        my int $num_subs := nqp::elems(@coderefs);
        my int $i := -1;
        while ++$i < $num_subs {
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
        }

        # Flag block as dynamically compiled.
        $past.annotate('DYNAMICALLY_COMPILED', 1);

        # Return the VM coderef that maps to the thing we were originally
        # asked to compile.
        $result
    }
    method unstub_code_object($code, $code_type) {
        my @compstuff := nqp::getattr($code, $code_type, q<@!compstuff>);
        unless nqp::isnull(@compstuff) {
            my $subid := @compstuff[0].cuid;

            nqp::bindattr($code, $code_type, '@!compstuff', nqp::null());

            my %sub_id_to_sc_idx := self.context().sub_id_to_sc_idx();
            my $code_ref := nqp::getattr($code, $code_type, '$!do');
            if nqp::existskey(%sub_id_to_sc_idx, $subid) {
                nqp::markcodestatic($code_ref); # maybe $!do instead
                self.update_root_code_ref(%sub_id_to_sc_idx{$subid}, $code_ref);
            }
            if nqp::existskey(%!code_object_fixup_list, $subid) {
                my $fixups := %!code_object_fixup_list{$subid};
                $fixups.pop() while $fixups.list;
            }
            nqp::deletekey(self.context().sub_id_to_code_object(), $subid);
        }
    }
    method try_add_to_sc($value, $fallback) {
        if nqp::isnull($value) {
            return $fallback;
        }

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
            for sorted_keys(%named) -> $key {
                $namedkey := $namedkey ~ $key ~ ',' ~ %named{$key} ~ ';'
                    if nqp::defined(%named{$key});
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
        self.add_object_if_no_sc($constant);

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
        my $node := $/;
        if $type eq 'Int' && (try $value.HOW.name($value)) eq 'Int' {
            if nqp::isbig_I($value) {
                # cannot unbox to int without loss of information
                my $const := self.add_constant('Int', 'bigint', $value);
                $const.node: $node;
                return $const;
            }
            # since Int doesn't have any vtables yet (at least while compiling
            # the setting), it is inconvenient to work with, so unbox
            $value := nqp::unbox_i($value);
        }
        my $const := self.add_constant($type, nqp::lc($type), $value);
        my $past;
        if $type eq 'Int' {
            $past := QAST::Want.new: :$node, $const, 'Ii',
              QAST::IVal.new: :$node, :$value;
        }
        else {
            $past := QAST::Want.new: :$node, $const, 'Nn',
                       QAST::NVal.new: :$node, :$value;
        }
        $past.returns($const.returns);
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
            $the_whatever := nqp::create(self.find_single_symbol('Whatever', :setting-only));
            self.add_object_if_no_sc($the_whatever);
            self.context().set_whatever($the_whatever);
        }
        QAST::WVal.new( :value($the_whatever), :returns($the_whatever.WHAT) )
    }

    method hyper_whatever() {
        my $the_hyper_whatever := self.context().hyper_whatever();
        unless nqp::isconcrete($the_hyper_whatever) {
            $the_hyper_whatever := nqp::create(self.find_single_symbol('HyperWhatever', :setting-only));
            self.add_object_if_no_sc($the_hyper_whatever);
            self.context().set_hyper_whatever($the_hyper_whatever);
        }
        QAST::WVal.new( :value($the_hyper_whatever), :returns($the_hyper_whatever.WHAT) )
    }

    # Adds the result of a constant folding operation to the SC and
    # returns a reference to it.
    method add_constant_folded_result($r) {
        if nqp::isnull($r) {
            QAST::Op.new( :op<null> )
        }
        elsif nqp::isstr($r) {
            QAST::SVal.new( :value($r) )
        }
        elsif nqp::isint($r) {
            QAST::IVal.new( :value(nqp::isconcrete($r) ?? $r !! 0) )
        }
        else {
            self.add_object_if_no_sc($r);
            QAST::WVal.new( :value($r) )
        }
    }

    # Takes a data structure of non-Raku objects and wraps them up
    # recursively.
    # If :$dynamic is passed wraps hashes with dynamic Scalars
    method p6ize_recursive($data, :$dynamic) {
        # $data that's a NQP hash is wrapped in a Scalar
        if nqp::ishash($data) && $dynamic {
            my $scalar := p6ize_recursive($data);
            my $descriptor_type := self.find_single_symbol('ContainerDescriptor', :setting-only);
            my $descriptor := $descriptor_type.new( :$dynamic );
            nqp::bindattr($scalar, self.find_single_symbol('Scalar', :setting-only), '$!descriptor', $descriptor);
            $scalar;
        }
        else {
            p6ize_recursive($data)
        }
    }

    method nibble_to_str($/, $ast, $mkerr) {
        if (nqp::istype($ast, QAST::Stmts) || nqp::istype($ast, QAST::Stmt)) && +@($ast) == 1 {
            $ast := $ast[0];
        }

        $ast.wanted(1);
        if $ast.has_compile_time_value {
            my $value := $ast.compile_time_value;
            if nqp::istype($value, self.find_single_symbol('Str', :setting-only)) {
                return nqp::unbox_s($ast.compile_time_value);
            }
            else {
                $/.panic("Did not get a string but a " ~ $value.HOW.name($value));
            }
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
                                    $/.panic($mkerr());
                                }
                            }
                            nqp::push(@pieces, nqp::unbox_s($result));
                        } else {
                            $/.panic($mkerr());
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
                if nqp::istype($inspect, QAST::WVal) && !nqp::istype($inspect.value, self.find_single_symbol("Block", :setting-only)) {
                    $/.panic($mkerr());
                }
                else {
                    my $result;
                    {
                        $result := ~self.compile_time_evaluate($/, $ast);
                        CONTROL {
                            # we might get a warning from evaluating a Block like
                            # "undefined value ..." which is reason enough to die
                            $/.panic($mkerr());
                        }
                        CATCH {
                            $/.panic($mkerr());
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
                $/.panic($mkerr());
            }
            return nqp::unbox_s($result);
        } else {
            $/.panic($mkerr());
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
        if $/.know_how($declarator) {
            $/.how($declarator)
        }
        elsif $declarator ~~ /'-attr'$/ {
            self.find_single_symbol('Attribute', :setting-only)
        }
        else {
            $/.panic("Cannot resolve meta-object for $declarator")
        }
    }

    # Creates a meta-object for a package, adds it to the root objects and
    # returns the created object.
    method pkg_create_mo($/, $how, :$name, :$repr, :$auth, :$api, :$ver, *%extra) {
        # Create the meta-object and add to root objects.
        my %args;
        if nqp::defined($name) { %args<name> := ~$name; }
        if nqp::defined($repr) { %args<repr> := ~$repr; }
        if nqp::defined($ver) { %args<ver> := $ver; }
        if nqp::defined($api) { %args<api> := $api; }
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
        self.add_object_if_no_sc($mo);

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
        self.add_object_if_no_sc($attr);
        $attr
    }

    # Tries to locate an attribute meta-object; optionally panic right
    # away if we cannot, otherwise add it to the post-resolution list.
    method get_attribute_meta_object($/, $name, $later?) {
        my $package := nqp::istype($/,NQPMu) ?? $*LEAF.package !! $/;
        unless nqp::can($package.HOW, 'get_attribute_for_usage') {
            $/.panic("Cannot understand $name in this context");
        }
        my $attr;
        my int $found := 0;
        try {
            $attr := $package.HOW.get_attribute_for_usage($package, $name);
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
                  package-name => $package.HOW.name($package),
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
        self.compile_in_context($past, self.find_single_symbol('Code', :setting-only));
    }

    # Adds a possible role to a role group.
    method pkg_add_role_group_possibility($/, $group, $role) {
        $group.HOW.add_possibility($group, $role);
    }

    # Composes the package, and stores an event for this action.
    method pkg_compose($/, $obj) {
        my $compiler_services := self.get_compiler_services($/);
        if nqp::isconcrete($compiler_services) {
            self.ex-handle($/, { $obj.HOW.compose($obj, :$compiler_services) })
        }
        else {
            self.ex-handle($/, { $obj.HOW.compose($obj) })
        }
    }

    my class CompilerServices {

        # Instantiated World object
        has $!w;

        # We share one Signature object among accessors for a given package.
        has $!acc_sig_cache;
        has $!acc_sig_cache_type;

        # The generic BUILDALL method for empty BUILDPLANs
        has $!empty_buildplan_method;

        # Types we always need
        has $!Block;
        has $!DefiniteHOW;
        has $!Failure;
        has $!List;
        has $!Map;
        has $!X-Attribute-Required;

        # Parameters we always need
        has $!pself;
        has $!pauto;
        has $!pinit;
        has $!p_;

        # Declarations we always need
        has $!dinit;
        has $!dreturn;

        # References to variables we always need
        has $!self;
        has $!init;
        has $!hllinit;
        has $!return;

        # String values we always need
        has $!storage;

        # signature configuration hashes
        has %!sig_empty;
        has %!sig_init;

        method BUILD(:$w, :$acc_sig_cache, :$acc_sig_cache_type, :$empty_buildplan_method, :$Block, :$DefiniteHOW, :$Failure, :$List, :$Map, :$X-Attribute-Required) {
            $!w                      := $w;
            $!acc_sig_cache          := $acc_sig_cache;
            $!empty_buildplan_method := $empty_buildplan_method;
            $!Block                  := $Block;
            $!DefiniteHOW            := $DefiniteHOW;
            $!Failure                := $Failure;
            $!List                   := $List;
            $!Map                    := $Map;
            $!X-Attribute-Required   := $X-Attribute-Required;

            $!pself := QAST::Var.new(:decl<param>, :scope<local>, :name<self>);
            $!pauto := QAST::Var.new(:decl<param>, :scope<local>, :name<@auto>);
            $!pinit := QAST::Var.new(:decl<param>, :scope<local>, :name<%init>);
            $!p_    := QAST::Var.new(:decl<param>, :scope<local>, :name('_'),
              :slurpy, :named);

            # Declarations we always need
            $!dinit   := QAST::Var.new(:decl<var>, :scope<local>, :name<init>);
            $!dreturn := QAST::Var.new(:decl<var>, :scope<local>, :name<return>);

            # References to variables we always need
            $!self     := QAST::Var.new( :scope<local>, :name<self> );
            $!init     := QAST::Var.new( :scope<local>, :name<init> );
            $!hllinit  := QAST::Var.new( :scope<local>, :name<%init> );
            $!return   := QAST::Var.new( :scope<local>, :name<return> );

            # String values we always need
            $!storage := QAST::SVal.new( :value<$!storage> );

            # signature configuration hashes
            %!sig_empty := nqp::hash('parameters', []);  # :()
            %!sig_init := nqp::hash(
              'parameters', [
                nqp::hash('variable_name','@auto'),
                nqp::hash('variable_name','%init')
              ]
            );
        }

        # Generate an accessor method with the given method name, package,
        # attribute name, type of attribute and rw flag.  Returns a code
        # object that can be installed as a method.
        method generate_accessor(
          $/, str $meth_name, $package_type, str $attr_name, $type, int $rw
        ) {

            # Is it a native attribute? (primpspec != 0)
            my $native := nqp::objprimspec($type);

            # Set up the actual statements, starting with "self"
# nqp::attribute(self,$package_type,$attr_name)
            my $accessor  := QAST::Var.new(
              :scope($native && $rw ?? 'attributeref' !! 'attribute'),
              :name($attr_name),
              :returns($type),
              QAST::Op.new( :op<decont>, $!self ),
              QAST::WVal.new( :value($package_type) )
            );

            # Opaque and read-only accessors need a decont
            unless $native || $rw {
                $accessor := QAST::Op.new( :op<decont>, $accessor );
            }

            # Create the block
            my $block := QAST::Block.new(
              :name($meth_name),
              :blocktype('declaration_static'),
              QAST::Stmts.new( $!pself, $!p_, :node(nqp::decont($/)) ),
              $accessor
            );

            # Make sure the block has a SC
            $!w.cur_lexpad()[0].push($block);

            # Find/Create the type of the invocant
            my $invocant_type :=
              $!w.create_definite_type($!DefiniteHOW, $package_type, 1);

            # Seen accessors of this class before, so use existing signature
            my $sig;
            if $invocant_type =:= $!acc_sig_cache_type {
                $sig := $!acc_sig_cache;
            }

            # First time, create new signature and mark it cached
            else {
                $sig := $!w.create_signature_and_params(
                  NQPMu, %!sig_empty, $block, 'Any', :method, :$invocant_type);
                $!acc_sig_cache      := $sig;
                $!acc_sig_cache_type := $invocant_type;
            }

            # Create a code object of the block, make sure we can write if ok
            my $code := $!w.create_code_object($block, 'Method', $sig);
            $code.set_rw() if $rw;

            # That's it
            $code
        }

        # Mapping of primspec to attribute postfix
        my @psp := ('','_i','_n','_s');

        # Mapping of primspec to native numeric default value op
        my @psd := (nqp::null,
          QAST::IVal.new( :value(0) ),
          QAST::NVal.new( :value(0e0) )
        );

        # signature configuration hash for ":(%init)"

        # Generate a method for building a new object that takes a hash
        # with attribute => value pairs to be assigned to the object's
        # attributes.  Basically a flattened version of Mu.BUILDALL, which
        # iterates over the BUILDALLPLAN at runtime with fewer inlining
        # and JITting opportunities.
        method generate_buildplan_executor($/, $in_object, $in_build_plan) {

            # low level hash access
            my $dc_build_plan := nqp::decont($in_build_plan);
            my $build_plan := nqp::islist($dc_build_plan)
                ?? $dc_build_plan
                !! nqp::getattr($dc_build_plan, $!List, '$!reified');

            if nqp::elems($build_plan) -> $count {

                # The bare object
                my $object := nqp::decont($in_object);

                # Do we need to wrap an exception handler
                my int $needs_wrapping;

                # The basic statements for object initialization, to be
                # filled in later
                my $stmts := QAST::Stmts.new(:node(nqp::decont($/)));

                my $declarations :=
                  QAST::Stmts.new($!pself, $!pauto, $!pinit, $!dinit);

                # The block of the method
                my $block := QAST::Block.new(
                  :name<BUILDALL>, :blocktype<declaration_static>,
                  $declarations
                );

                # Register the block in its SC
                $!w.cur_lexpad()[0].push($block);

                # Create the invocant type we need
                my $invocant_type :=
                  $!w.create_definite_type($!DefiniteHOW, $object, 1);

                # Debugging
#                $stmts.push(
#                  QAST::Op.new( :op<say>,
#                    QAST::SVal.new( :value(
#                      $object.HOW.name($object) ~ '.BUILDALL called'
#                    ))
#                  ),
#                );
#                $stmts.push(
#                  QAST::Op.new( :op<say>,
#                    QAST::Op.new( :op<callmethod>, :name<perl>, $!hllinit )
#                  )
#                );

# my $init := nqp::getattr(%init,Map,'$!storage')
                $stmts.push(QAST::Op.new( :op<bind>,
                  $!init,
                  QAST::Op.new( :op<getattr>,
                    $!hllinit, QAST::WVal.new( :value($!Map) ), $!storage
                  )
                ));

                # Do all of the actions in the BUILDPLAN
                my int $i := -1;
                while nqp::islt_i($i := nqp::add_i($i, 1), $count) {

                    # We have some intricate action to do
                    if nqp::islist(my $task := nqp::atpos($build_plan,$i)) {

                        # Register the class in the SC if needed
                        $!w.add_object_if_no_sc( nqp::atpos($task,1) );

                        # We always need the class object & full attribute name
                        my $class :=
                          QAST::WVal.new( :value(nqp::atpos($task,1)) );
                        my $attr :=
                          QAST::SVal.new( :value(nqp::atpos($task,2)) );

                        my int $code := nqp::atpos($task,0);
                        # 0,11,12,13 = initialize opaque from %init
                        if $code == 0 || $code == 11 || $code == 12 || $code == 13 {

# 'a'
                            my $key :=
                              QAST::SVal.new( :value(nqp::atpos($task,3)) );

# nqp::getattr(self,Foo,'$!a')
                            my $getattr := QAST::Op.new( :op<getattr>,
                              $!self, $class, $attr
                            );

# nqp::unless(
#   nqp::isnull(my \tmp = nqp::atkey($!init,'a')),
                            my $tmp := QAST::Node.unique('buildall_tmp_');
                            my $if := QAST::Op.new( :op<unless>,
                              QAST::Op.new( :op<isnull>,
                                QAST::Op.new( :op<bind>,
                                  QAST::Var.new( :name($tmp), :scope<local>, :decl<var> ),
                                  QAST::Op.new( :op<atkey>, $!init, $key )
                                )
                              )
                            );

                            my $sigil := nqp::substr(nqp::atpos($task,2),0,1);

# nqp::getattr(self,Foo,'$!a').STORE(tmp, :INITIALIZE)
                            if $sigil eq '@' || $sigil eq '%' {
                                $if.push(
                                  QAST::Op.new( :op<callmethod>, :name<STORE>,
                                    $getattr,
                                    QAST::Var.new( :name($tmp), :scope<local> ),
                                    QAST::WVal.new(
                                      :value($!w.find_symbol(
                                        ['Bool','True'], :setting-only
                                      )),
                                      :named('INITIALIZE')
                                    )
                                  )
                                );
                            }

# nqp::bindattr(self,Foo,'$!a',tmp)
                            elsif $code == 13 {
                                my $arg := QAST::Var.new( :name($tmp), :scope<local> );
                                if nqp::elems($task) == 5 {
                                    $arg := QAST::Op.new(
                                      :op('p6bindassert'),$arg,
                                      QAST::WVal.new(:value(nqp::atpos($task,4)))
                                    );
                                }
                                $if.push(
                                  QAST::Op.new(
                                    :op('bindattr'),$!self,$class,$attr,$arg)
                                );
                            }

# nqp::getattr(self,Foo,'$!a') = tmp
                            else {
                                $if.push(
                                  QAST::Op.new(
                                    :op( $sigil eq '$' || $sigil eq '&'
                                           ?? 'p6assign' !! 'p6store'
                                    ),
                                    $getattr,
                                    QAST::Var.new( :name($tmp), :scope<local> )
                                  )
                                );
                            }

                            # 11,12
# bindattr(self,Foo,'$!a',nqp::list|hash)
                            if $code == 11 || $code == 12 {
                                $if.push(
                                  QAST::Op.new(:op<bindattr>,
                                    $!self, $class, $attr,
                                    QAST::Op.new(
                                      :op($code == 11 ?? 'list' !! 'hash')
                                    )
                                  )
                                );
                            }

# ),
                            $stmts.push($if);
                        }

                        # 1,2,3 = initialize native from %init
                        elsif $code < 4 {

# nqp::unless(
#   nqp::isnull(my \tmp := nqp::atkey($init,'a')),
#   nqp::bindattr_x(self,Foo,'$!a',nqp::decont(tmp))
# ),
                            my $tmp := QAST::Node.unique('buildall_tmp_');
                            $stmts.push(
                              QAST::Op.new(:op<unless>,
                                QAST::Op.new(:op<isnull>,
                                  QAST::Op.new(:op<bind>,
                                    QAST::Var.new(:decl<var>, :name($tmp), :scope<local>),
                                    QAST::Op.new(:op<atkey>,
                                      $!init, QAST::SVal.new( :value(nqp::atpos($task,3)))
                                    )
                                  )
                                ),
                                QAST::Op.new(:op('bindattr' ~ @psp[$code]),
                                  $!self, $class, $attr,
                                  QAST::Op.new(:op<decont>, QAST::Var.new(:name($tmp), :scope<local>))
                                )
                              )
                            );
                        }

                        # 4 = set opaque with default if not set yet
                        elsif $code == 4 || $code == 14 {

# nqp::unless(
#   nqp::attrinited(self,Foo,'$!a'),
                            my $unless := QAST::Op.new( :op<unless>,
                                QAST::Op.new( :op<attrinited>,
                                  $!self, $class, $attr
                                )
                            );

# nqp::getattr(self,Foo,'$!a')
                            my $getattr := QAST::Op.new( :op<getattr>,
                              $!self, $class, $attr
                            );

                            my $initializer := nqp::istype(
                              nqp::atpos($task,3),$!Block
# $code(self,nqp::getattr(self,Foo,'$!a'))
                            ) ?? QAST::Op.new( :op<call>,
                                   QAST::WVal.new(:value(nqp::atpos($task,3))),
                                   $!self, $getattr
                                 )
# $value
                              !! QAST::WVal.new(:value(nqp::atpos($task,3)));

                            my $sigil := nqp::substr(nqp::atpos($task,2),0,1);
# nqp::getattr(self,Foo,'$!a').STORE($code(self,nqp::getattr(self,Foo,'$!a')), :INITIALIZE)
                            if $sigil eq '@' || $sigil eq '%' {
                                $unless.push(
                                  QAST::Op.new( :op<callmethod>, :name<STORE>,
                                    $getattr, $initializer, QAST::WVal.new(
                                      :value($!w.find_symbol(
                                        ['Bool','True'], :setting-only
                                      )),
                                      :named('INITIALIZE')
                                    )
                                  )
                                );
                            }

                            elsif $code == 14 {
# (nqp::bindattr(self,Foo,'$!a',$code(self,nqp::getattr(self,Foo,'$!a'))))
                                if nqp::elems($task) == 5 {
                                    $initializer := QAST::Op.new(
                                      :op('p6bindassert'),$initializer,
                                      QAST::WVal.new(:value(nqp::atpos($task,4)))
                                    );
                                }
                                $unless.push(
                                  QAST::Op.new(
                                    :op('bindattr'),
                                    $!self, $class, $attr,
                                    $initializer
                                  )
                                )
                            }

                            else {
# (nqp::getattr(self,Foo,'$!a') = $code(self,nqp::getattr(self,Foo,'$!a')))
                                $unless.push(
                                  QAST::Op.new(
                                    :op( $sigil eq '$' || $sigil eq '&'
                                           ?? 'p6assign' !! 'p6store'
                                    ),
                                    $getattr, $initializer
                                  )
                                )
                            }

# ),
                            $stmts.push($unless);

                            $!w.add_object_if_no_sc(nqp::atpos($task,3));
                        }

                        # 5,6 = set native numeric with default if not set
                        elsif $code < 7 {
# nqp::if(
#   nqp::iseq_x(
#     nqp::getattr_x(self,Foo,'$!a'),
#     (native default value)
#   ),
#   nqp::bindattr_x(self,Foo,'$!a',
#     $initializer(self,nqp::getattr_x(self,Foo,'$!a')))
# ),
                            my $getattr := QAST::Op.new(
                              :op('getattr' ~ @psp[$code - 4]),
                              $!self, $class, $attr
                            );
                            $stmts.push(
                              QAST::Op.new( :op<if>,
                                QAST::Op.new( :op('iseq' ~ @psp[$code - 4]),
                                  $getattr,
                                  @psd[$code - 4],
                                ),
                                QAST::Op.new( :op('bindattr' ~ @psp[$code - 4]),
                                  $!self, $class, $attr,
                                  nqp::if(
                                    nqp::istype(nqp::atpos($task,3),$!Block),
                                    QAST::Op.new( :op<call>,
                                      QAST::WVal.new(:value(nqp::atpos($task,3))),
                                      $!self,
                                      $getattr
                                    ),
                                    nqp::if(
                                      nqp::iseq_i($code,5),
                                      QAST::IVal.new(:value(nqp::atpos($task,3))),
                                      QAST::NVal.new(:value(nqp::atpos($task,3)))
                                    )
                                  )
                                )
                              )
                            );

                            $!w.add_object_if_no_sc(nqp::atpos($task,3));
                        }

                        # 7 = set native string with default if not set
                        elsif $code == 7 {
# nqp::if(
#   nqp::isnull_s(nqp::getattr_s(self,Foo,'$!a')),
#   nqp::bindattr_s(self,Foo,'$!a',
#     $initializer(self,nqp::getattr_s(self,Foo,'$!a')))
# ),
                            my $getattr := QAST::Op.new( :op<getattr_s>,
                              $!self, $class, $attr
                            );
                            $stmts.push(
                              QAST::Op.new( :op<if>,
                                QAST::Op.new( :op<isnull_s>, $getattr),
                                QAST::Op.new( :op<bindattr_s>,
                                  $!self, $class, $attr,
                                  nqp::if(
                                    nqp::istype(nqp::atpos($task,3),$!Block),
                                    QAST::Op.new( :op<call>,
                                      QAST::WVal.new(:value(nqp::atpos($task,3))),
                                      $!self,
                                      $getattr
                                    ),
                                    QAST::SVal.new(:value(nqp::atpos($task,3)))
                                  )
                                )
                              )
                            );

                            $!w.add_object_if_no_sc(nqp::atpos($task,3));
                        }

                        # 8 = bail if opaque not yet initialized
                        elsif $code == 8 {

# nqp::unless(
#   nqp::attrinited(self,Foo,'$!a'),
#   X::Attribute::Required.new(name => '$!a', why => (value))
# ),
                            $stmts.push(
                              QAST::Op.new( :op<unless>,
                                QAST::Op.new( :op<attrinited>,
                                  $!self, $class, $attr
                                ),
                                QAST::Op.new( :op<callmethod>, :name<throw>,
                                  QAST::Op.new( :op<callmethod>, :name<new>,
                                    QAST::WVal.new(
                                      :value($!X-Attribute-Required)),
                                    QAST::SVal.new( :named('name'),
                                      :value(nqp::atpos($task,2))),
                                    QAST::WVal.new( :named('why'),
                                      :value(nqp::atpos($task,3)))
                                  )
                                )
                              )
                            );
                        }

                        # 9 = run attribute container initializer
                        elsif $code == 9 {

# nqp::bindattr(self,Foo,'$!a',$initializer())
                            $stmts.push(
                              QAST::Op.new( :op<bindattr>,
                                $!self, $class, $attr,
                                QAST::Op.new( :op<call>,
                                  QAST::WVal.new(:value(nqp::atpos($task,3)))
                                )
                              )
                            );

                            $!w.add_object_if_no_sc(nqp::atpos($task,3));
                        }

                        # 10 = set attrinited on attribute
                        elsif $code == 10 {
# nqp::getattr(self,Foo,'$!a')
                            $stmts.push(
                              QAST::Op.new(:op<getattr>, $!self, $class, $attr)
                            );
                        }

                        else {
                            nqp::die('Invalid ' ~ $object.HOW.name($object) ~ '.BUILDALL plan: ' ~ $code);
                        }
                    }

                    # BUILD/TWEAK
                    else {

                        # BUILD or TWEAK without BUILD (first seen)
                        unless $needs_wrapping {

# (my $return),
                            $declarations.push($!dreturn);
                            $needs_wrapping := 1
                        }

# nqp::if(
#   nqp::istype(
#     ($!return := nqp::if(
#       nqp::elems($init),
#       $task(self,|%init),
#       $task(self)
#     )),
#     Failure
#   ),
#   return $!return
# ),
                        $stmts.push(
                          QAST::Op.new( :op<if>,
                            QAST::Op.new( :op<istype>,
                              QAST::Op.new( :op<bind>,
                                $!return,
                                QAST::Op.new( :op<if>,
                                  QAST::Op.new( :op<elems>, $!init ),
                                  QAST::Op.new( :op<call>,
                                    QAST::WVal.new( :value($task) ),
                                    $!self,
                                    QAST::Var.new(
                                      :scope<local>,
                                      :name<init>,  # use nqp::hash directly
                                      :flat(1),
                                      :named(1)
                                    ),
                                  ),
                                  QAST::Op.new( :op<call>,
                                    QAST::WVal.new( :value($task) ),
                                    $!self,
                                  )
                                )
                              ),
                              QAST::WVal.new(:value($!Failure)),
                            ),
                            QAST::Op.new( :op<call>,
                              QAST::WVal.new(
                                :value($!w.find_single_symbol('&return'))),
                              QAST::Var.new(:scope<local>, :name<return>)
                            )
                          )
                        );

                        $!w.add_object_if_no_sc($task);
                    }
                }

                # Finally, add the return value
                $stmts.push($!self);

                # Need to wrap an exception handler around
                if $needs_wrapping {
                    $stmts := QAST::Op.new( :op<handlepayload>,
                      $stmts,
                      'RETURN',
                      QAST::Op.new( :op<lastexpayload> )
                    );
                }

                # Add the statements to the block
                $block.push($stmts);

# :(Foo:D: %init)
                my $sig := $!w.create_signature_and_params(
                  NQPMu, %!sig_init, $block, 'Any', :method, :$invocant_type
                );

                # Create the code object, hide it from backtraces and return it
                my $code := $!w.create_code_object($block, 'Submethod', $sig);
                my $trait_mod_is := $!w.find_single_symbol('&trait_mod:<is>');
                $trait_mod_is($code,:hidden-from-backtrace);
                $code
            }

            # Empty buildplan, and we already have an empty buildplan method
            elsif $!empty_buildplan_method {
                $!empty_buildplan_method
            }

            # Empty buildplan, still need to make an empty method
            else {

# submethod :: (Any:D:) { self }
                my $block := QAST::Block.new(
                  :name<BUILDALL>, :blocktype<declaration_static>,
                  QAST::Stmts.new($!pself, $!pauto, $!pinit),
                  $!self
                );

                # Register the block in its SC
                $!w.cur_lexpad()[0].push($block);

                my $invocant_type := $!w.create_definite_type(
                  $!DefiniteHOW, $!w.find_single_symbol('Any', :setting-only), 1);

                my $sig := $!w.create_signature_and_params(
                  NQPMu, %!sig_init, $block, 'Any', :method, :$invocant_type
                );

                # Create the code object, save and return it
                $!empty_buildplan_method :=
                  $!w.create_code_object($block, 'Submethod', $sig)
            }
        }
    }

    method get_compiler_services($/) {

        # Already have a CompilerServices object
        if nqp::isconcrete($!compiler_services) {

            # Update $/ for error reporting on generated methods
            nqp::bindattr(
              $!compiler_services,$!compiler_services.WHAT,'$!current-match',$/
            );
        }

        # Don't have a CompilerServices object yet
        else {
            try {

                # Find the HLL version, might fail early in setting compilation
                my $wtype :=
                  self.find_symbol(['Rakudo','Internals','CompilerServices'], :setting-only);
                my $wrapper := nqp::create($wtype);

                # Set up the base object
                my $wrapped := CompilerServices.new(
                  w           => self,
                  Block       => self.find_single_symbol('Block', :setting-only),
                  DefiniteHOW => self.find_symbol(['Metamodel','DefiniteHOW'], :setting-only),
                  Failure     => self.find_single_symbol('Failure', :setting-only),
                  List        => self.find_single_symbol('List', :setting-only),
                  Map         => self.find_single_symbol('Map', :setting-only),
                  X-Attribute-Required =>
                    self.find_symbol(['X','Attribute','Required'], :setting-only)
                );
                nqp::bindattr($wrapper, $wtype, '$!compiler', $wrapped);
                nqp::bindattr($wrapper, $wtype, '$!current-match', $/);

                # When we got here, we really got a full CompilerServices object
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
            if nqp::isconcrete($curried)
              && nqp::istype($curried, self.find_single_symbol("Str", :setting-only)) {
                self.throw($/, 'X::AdHoc', payload => $curried)
            }

            self.add_object_if_no_sc($curried);
            return $curried;
        }
        $role;
    }

    # Creates a subset type meta-object/type object pair.
    method create_subset($how, $refinee, $refinement, :$name) {
        # Create the meta-object and add to root objects.
        my %args := hash(:$refinee, :$refinement);
        if nqp::defined($name) { %args<name> := $name; }
        my $mo := $how.new_type(|%args);
        self.add_object_if_no_sc($mo);
        return $mo;
    }

    # Gets a definite type (possibly freshly created, possibly an
    # interned one).
    method create_definite_type($how, $base_type, $definite) {
       # Create the meta-object and add to root objects.
        my $mo := $how.new_type(:$base_type, :$definite);

        if nqp::isnull(nqp::getobjsc($mo)) { self.add_object_if_no_sc($mo); }

        return $mo;
    }

    # Adds a value to an enumeration.
    method create_enum_value($enum_type_obj, $key, $value, $index) {
        # Create directly.
        my $val := nqp::rebless(nqp::clone($value), $enum_type_obj);
        nqp::bindattr($val, $enum_type_obj, '$!key', $key);
        nqp::bindattr($val, $enum_type_obj, '$!value', $value);
        nqp::bindattr_i($val, $enum_type_obj, '$!index', $index);
        self.add_object_if_no_sc($val);

        # Add to meta-object.
        $enum_type_obj.HOW.add_enum_value($enum_type_obj, $val);

        # Result is the value.
        $val
    }

    # Gets a coercion type (possibly freshly created, possibly an
    # interned one).
    method create_coercion_type($/, $target, $constraint) {
        self.ex-handle($/, {
            my $type := $/.how('coercion').new_type($target, $constraint);
            if nqp::isnull(nqp::getobjsc($type)) { self.add_object_if_no_sc($type); }
            $type
        })
    }

    # Set up lookup for newbie type errors in typenames
    my $newbies := nqp::hash(
      'Integer',   ('Int',),
      'integer',   ('Int','int'),
      'Float',     ('Num',),
      'float',     ('Num','num'),
      'Number',    ('Num',),
      'number',    ('Num','num'),
      'String',    ('Str',),
      'string',    ('Str','str'),
    );

    method suggest_typename($name) {
        my %seen;
        %seen{$name} := 1;
        my @candidates := [[], [], []];
        my &inner-evaluator := make_levenshtein_evaluator($name, @candidates);
        my @suggestions;

        if nqp::atkey($newbies,$name) -> @alternates {
            for @alternates {
                nqp::push(@suggestions,$_);
            }
        }

        sub evaluator($name, $object, $has_object, $hash) {
            # only care about type objects
            my $first := nqp::substr($name, 0, 1);
            return 1 if $first eq '$' || $first eq '%' || $first eq '@' || $first eq '&' || $first eq ':';
            return 1 if !$has_object || (nqp::isconcrete($object) && !($object.HOW.HOW.name($object.HOW) eq 'Perl6::Metamodel::EnumHOW'));
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
        my $trait_sub := self.find_single_symbol($trait_sub_name);
        my $ex;
        try {
            self.ex-handle($/, { $trait_sub(|@pos_args, |%named_args) });
            CATCH { $ex := $_; }
            CONTROL {
                if nqp::getextype($_) == nqp::const::CONTROL_WARN {
                    $/.PRECURSOR.worry(nqp::getmessage($_));
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
    my %is-traits-to-warn-on-duplicate := nqp::hash(
        'tighter',  1,  'looser', 1,  'equiv', 1,  'rw',   1,  'default', 1,
        'readonly', 1,  'raw',    1,  'assoc', 1,  'pure', 1,  'export',  1
    );
    method apply_traits($traits, $declarand) {
        my %seen;
        for $traits {
            my $name := ~$_<trait_mod><longname>;
            %is-traits-to-warn-on-duplicate{$name}
                && %seen{$name}++
                && $_.PRECURSOR.worry: "Duplicate 'is $name' trait";

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
                            $W.add_object_if_no_sc($code);
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
                        nqp::getcomp('Raku').backend.name eq 'moar'
                            ?? nqp::getstaticcode($!resolved)
                            !! $!resolved);
                }
            }
        }

        # Create a list and put it in the SC.
        my $fixup_list := nqp::create(FixupList);
        self.add_object_if_no_sc($fixup_list);
        nqp::bindattr($fixup_list, FixupList, '$!list', nqp::list());
        nqp::bindattr($fixup_list, FixupList, '$!resolver', self.handle());

        # Set up capturing code.
        my $capturer := self.cur_lexpad();
        my $c_block  := QAST::Block.new( :blocktype('declaration_static'),
                                         :name('!LEXICAL_FIXUP_CSCOPE') );
        self.create_code_obj_and_add_child($c_block, 'Code');
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
            my $handled_block := -> {
                self.handle-begin-time-exceptions($/, 'evaluating a CHECK', $block);
            }
            my $result_node := QAST::Stmt.new( QAST::Var.new( :name('Nil'), :scope('lexical') ) );
            self.add_check([$handled_block, $result_node]);
            return $result_node;
        }
        elsif $phaser eq 'INIT' {
            unless $*UNIT.symbol('!INIT_VALUES') {
                my $mu := self.find_single_symbol('Mu', :setting-only);
                my %info;
                %info<container_type> := %info<container_base> := self.find_single_symbol('Hash', :setting-only);
                %info<bind_constraint> := self.find_single_symbol('Associative', :setting-only);
                %info<value_type> := $mu;
                self.install_lexical_container($*UNIT, '!INIT_VALUES', %info,
                    self.create_container_descriptor($mu, '!INIT_VALUES'));
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
                        QAST::WVal.new( :value(self.find_symbol(['X', 'Phaser', 'PrePost'], :setting-only)) ),
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
                    nqp::getattr($block.signature, self.find_single_symbol('Signature', :setting-only), '@!params'),
                    self.create_parameter($/, hash(
                            variable_name => '$_', is_raw => 1,
                            type => self.find_single_symbol('Mu', :setting-only)
                        )));
            }

            self.context().cur_code_object().add_phaser($phaser, $block);
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
        }
        elsif $phaser eq 'ENTER' {
            self.context().cur_code_object().add_phaser($phaser, $block);
            my $enclosing := self.context().cur_lexpad();
            my $enter_tmp := $enclosing.unique('enter_result_');
            $enclosing[0].push(QAST::Var.new( :name($enter_tmp), :scope('lexical'), :decl('var') ));
            my @pres := $enclosing.ann('phaser_results') || $enclosing.annotate('phaser_results', []);
            @pres.push($block);
            @pres.push(my $var := QAST::Var.new( :name($enter_tmp), :scope('lexical') ));
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
        for self.checks() {
            my $result := $_[0]();
            $_[1][0] := self.add_constant_folded_result($result);
        }
    }

    # Does any cleanups needed after compilation.
    method cleanup() {
        for self.context().cleanup_tasks() { $_() }

        self.finish;
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
                        $past.push: nqp::istype($_, QAST::Node) ?? $_ !! QAST::SVal.new(:value($_));
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
            my $Bool := $w.find_single_symbol('Bool', :setting-only);
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
                    $_.panic("Colonpair too complex in $dba");
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
                        $*W.find_single_symbol('Stash', :setting-only),
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
            nqp::elems(@!components) > 1
              && !nqp::istype(@!components[0], QAST::Node)
              && @!components[0]   # need this to fix crash on OS X
              && @!components[0] eq 'GLOBAL';
        }
    }

    method can_has_coercerz($/) {
        if $<accept> {
            $<accept>.ast
        }
        elsif $<accept_any> {
            self.find_symbol: ['Any'], :setting-only
        }
        elsif $<colonpairs> && ($<colonpairs>.ast<D> || $<colonpairs>.ast<U>) {
            my $val := $<longname><colonpair>[0].ast[2];
            nqp::istype($val, QAST::Op)
              # XXX TODO: the circumfix:<[ ]> path is a misparse of parameterization,
              # e.g. List:D[Int]. When parse is fixed, the circumfix branch likely can be removed
              ?? $val.op eq 'hllbool' || $val.op eq 'call' && $val.name eq '&circumfix:<[ ]>'
                ?? nqp::null # not a coercer, but just got a regular DefiniteHOW
                !! $val.name eq '&infix:<,>' && @($val) == 0
                  ?? self.find_symbol: ['Any'], :setting-only # empty coercer source type
                  !! self.throw: $/, ['X', 'Syntax', 'Coercer', 'TooComplex']
              !! nqp::istype($val, QAST::WVal)
                ?? $val.value
                !! self.throw: $/, ['X', 'Syntax', 'Coercer', 'TooComplex']
        }
        else {
            nqp::null # we didn't find any coercers
        }
    }

    method validate_type_smiley ($/, $colonpairs) {
        1 < nqp::elems($colonpairs) && self.throw: $/, ['X', 'MultipleTypeSmiley'];
        my %colonpairs;
        for $colonpairs {
            if $_<identifier> {
                my $name := $_<identifier>.Str;
                $name eq 'D' || $name eq 'U' || $name eq '_'
                    ?? (%colonpairs{$name} := 1)
                    !! self.throw: $/, ['X', 'InvalidTypeSmiley'], :$name
            }
        }
        %colonpairs
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
        if nqp::existskey($name, 'morename') && nqp::elems($name<morename>) {
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
                    if nqp::elems(@components) {
                        nqp::bindattr_i($result, LongName, '$!get_who', 1);
                    }
                }
            }
        }
        nqp::bindattr($result, LongName, '@!components', @components);

        # Stash colon pairs with names; incorporate non-named one into
        # the last part of the name (e.g. for infix:<+>). Need to be a
        # little cheaty when compiling the setting due to bootstrapping.
        my @pairs;
        if nqp::existskey($longname, 'colonpair') && nqp::elems($longname<colonpair>) {
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
                              nqp::istype($ast.value, $*W.find_single_symbol('Str', :setting-only)) {
                            $cp_str := self.canonicalize_pair('', $ast.value);
                        }
                        else {
                            $cp_str := ~$_;
                        }
                    }
                    else {
                        # Safe to evaluate it directly; no bootstrap issues.
                        $cp_str := self.canonicalize_pair('',self.compile_time_evaluate:
                          $_, $_.ast, :mark-wanted);
                    }
                    if nqp::elems(@components) {
                        @components[nqp::elems(@components) - 1] := @components[nqp::elems(@components) - 1] ~ $cp_str;
                    } else {
                        @components[0] := $cp_str;
                    }
                }
                else {
                    $_.ast.wanted(1);
                    @pairs.push($_);
                }
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
            $has_ctv := !(nqp::iscont($sym) && nqp::isconcrete_nd($sym));
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
            my $maybe_regex := self.find_single_symbol($name);
            $result := nqp::istype($maybe_regex, self.find_single_symbol('Regex', :setting-only));
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

    method find_single_symbol_in_setting($name) {
        my $setting_name := Perl6::ModuleLoader.transform_setting_name($!setting_name);
        my $ctx := Perl6::ModuleLoader.load_setting($setting_name);

        while $ctx {
            my $pad := nqp::ctxlexpad($ctx);
            if nqp::existskey($pad, $name) {
                return nqp::atkey($pad, $name);
            }
            $ctx := nqp::ctxouter($ctx);
        }
        nqp::die("Cannot find symbol $name in $setting_name");
    }
    method find_symbol_in_setting(@name) {
        my $no-outers := 0; # If 'true' then don't look in the outer contexts
        my $setting_name := $!setting_name;

        if nqp::iseq_s(@name[0], 'CORE') {          # Looking in CORE:: namespace
            nqp::shift(@name := nqp::clone(@name));
            if nqp::iseq_i(nqp::chars(@name[0]),3)
                && nqp::iseq_i(nqp::index(@name[0], 'v6'),0) {
                my $rev := nqp::substr(@name[0],2,1);
                # If a supported language revision requested
                if nqp::chars($rev) == 1 && nqp::existskey(nqp::getcomp('Raku').language_revisions,$rev) {
                    $no-outers := 1; # you don't see other COREs!
                    nqp::shift(@name);
                    $setting_name := 'CORE' ~ '.' ~ $rev;
                }
            }
        }

        $setting_name := Perl6::ModuleLoader.transform_setting_name($setting_name);
        my $ctx := Perl6::ModuleLoader.load_setting($setting_name);

        my str $fullname := nqp::join("::", @name);
        my $components := +@name;

        while $ctx {
            my $pad := nqp::ctxlexpad($ctx);
            if nqp::existskey($pad, @name[0]) {
                my $val := nqp::atkey($pad, @name[0]);
                if $components == 1 {
                    return $val;
                }
                my $i := 1;
                while $i < $components {
                    if nqp::existskey($val.WHO, @name[$i]) {
                        $val := ($val.WHO){@name[$i++]};
                        if $i == $components {
                            return $val;
                        }
                    }
                    else {
                        last;
                    }
                }
            }
            $ctx := $no-outers ?? nqp::null() !! nqp::ctxouter($ctx);
        }
        nqp::die("Cannot find symbol $fullname in $setting_name");
    }

    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_single_symbol(str $name, :$setting-only, :$upgrade_to_global, :$cur-package) {
        unless $!setting_loaded {
            return self.find_single_symbol_in_setting($name);
        }

        # GLOBAL is current view of global.
        if $name eq 'GLOBAL' {
            return $*GLOBALish
        }

        # Look through the lexical scopes and try the current package.

        my $scope := $*WANTEDOUTERBLOCK;
        if $scope {
            while $scope {
                my %sym := $scope.symbol($name);
                if nqp::isconcrete(%sym) && nqp::elems(%sym) {
                    my $value := self.force_value(%sym, $name, 1);
                    if $upgrade_to_global {
                        ($*GLOBALish.WHO){$name} := $value;
                    }
                    return $value;
                }
                $scope := $scope.ann('outer');
            }
        }
        else {
            my @BLOCKS := self.context().blocks;

            # Work out where to start searching.
            my int $start_scope := $setting-only
                ?? ($*COMPILING_CORE_SETTING ?? 2 !! 1)
                !! nqp::elems(@BLOCKS);
            while $start_scope > 0 {
                $start_scope := $start_scope - 1;
                my %sym := @BLOCKS[$start_scope].symbol($name);
                if nqp::isconcrete(%sym) && nqp::elems(%sym) {
                    my $value := self.force_value(%sym, $name, 1);
                    if $upgrade_to_global {
                        ($*GLOBALish.WHO){$name} := $value;
                    }
                    return $value;
                }
            }
        }
        nqp::die("find_symbol1") unless nqp::objectid($*PACKAGE) == nqp::objectid($*LEAF.package);
        $cur-package := $*LEAF.package unless $cur-package;
        if nqp::existskey($cur-package.WHO, $name) {
            return nqp::atkey($cur-package.WHO, $name);
        }

        # Fall back to looking in GLOBALish
        my $result := $*GLOBALish;
        # Try to chase down the parts of the name.
        if nqp::existskey($result.WHO, $name) {
            $result := ($result.WHO){$name};
        }
        else {
            nqp::die("Could not locate compile-time value for symbol " ~ $name);
        }

        $result;
    }
    method find_symbol(@name, :$setting-only, :$upgrade_to_global, :$cur-package) {
        # Make sure it's not an empty name.
        unless +@name { nqp::die("Cannot look up empty name"); }

        if +@name == 1 {
            #note("got a single element argument to find_symbol: " ~ @name[0]);
            return self.find_single_symbol(~@name[0], :$setting-only, :$upgrade_to_global, :$cur-package);
        }

        unless $!setting_loaded {
            return self.find_symbol_in_setting(@name);
        }

        # Support for compile-time CORE:: namespace
        if nqp::iseq_s(@name[0], 'CORE') {
            return self.find_symbol_in_setting(@name);
        }

        my @BLOCKS := self.context().blocks;

        # Work out where to start searching.
        my int $start_scope := $setting-only
            ?? ($*COMPILING_CORE_SETTING ?? 2 !! 1)
            !! nqp::elems(@BLOCKS);

        # If it's a multi-part name, see if the containing package
        # is a lexical somewhere or can be found in the current
        # package. Otherwise we fall back to looking in GLOBALish.
        my $result := $*GLOBALish;
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
            nqp::die("find_symbol2") unless nqp::objectid($*PACKAGE) == nqp::objectid($*LEAF.package);
            $cur-package := $*LEAF.package unless $cur-package;
            if nqp::existskey($cur-package.WHO, $first) {
                $result := nqp::atkey($cur-package.WHO, $first);
                @name := nqp::clone(@name);
                @name.shift();
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
        if +@name == 0 { $/.panic("Cannot compile empty name"); }
        my $orig_name := join('::', @name);

        # Handle fetching GLOBAL.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return QAST::Op.new( :op('getcurhllsym'),
                QAST::SVal.new( :value('GLOBAL') ) );
        }

        # Handle things starting with pseudo-package.
        if self.is_pseudo_package(@name[0]) && @name[0] ne 'GLOBAL' && @name[0] ne 'PROCESS' {
            my $lookup;
            $*W.cur_lexpad().no_inline(1);
            for @name {
                if $lookup {
                    $lookup := QAST::Op.new( :op('who'), $lookup );
                }
                else {
                    # Lookups start at the :: root.
                    $lookup := QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :value(self.find_single_symbol('PseudoStash', :setting-only)) )
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
                :value(self.find_symbol(['Bool', 'True'], :setting-only)),
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
            @suggestions.push: $with_sigil ?? '&codes'  !! 'codes';
        }
        elsif $name eq '&bytes' {
            @suggestions.push: '.encode($encoding).bytes';
        }
        elsif $name eq '&break' {
            @suggestions.push: 'last';
        }
        elsif $name eq '&skip' {
            @suggestions.push: 'next';
        }
        elsif $name eq '&continue' {
            @suggestions.push: 'NEXT';
            @suggestions.push: 'proceed';
            @suggestions.push: 'succeed';
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
        %opts<filename> := nqp::box_s(self.current_file,self.find_single_symbol('Str', :setting-only));
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
            my $c := $/;
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
                    $quotes := "<<>>" if $quotes eq '<>' && nqp::eqat($c.orig, '>', $qe + 1);
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
                        nqp::push(@a, nqp::hllizefor($_, 'Raku'));
                    }
                    %opts{$p.key} := nqp::hllizefor(@a, 'Raku');
                }
                else {
                    %opts{$p.key} := nqp::hllizefor($p.value, 'Raku');
                }
            }
            %opts<filename> := nqp::box_s(self.current_file,self.find_single_symbol('Str', :setting-only));
            try { return $ex.new(|%opts) };
        }

        my $Str;
        my $Int;
        my $List;
        my int $has_str;
        my int $has_int;
        my int $has_list;

        try { $Str := self.find_single_symbol("Str", :setting-only); $has_str := 1 }
        try { $Int := self.find_single_symbol("Int", :setting-only); $has_int := 1 }
        try { $List := self.find_single_symbol("List", :setting-only); $has_list := 1 }

        sub safely_stringify($target) {
            if $has_str && nqp::istype($target, $Str) {
                return nqp::isconcrete($target)
                  ?? ~nqp::unbox_s($target) !! '(Str)';
            } elsif $has_int && nqp::istype($target, $Int) {
                return nqp::isconcrete($target)
                  ?? ~nqp::unbox_i($target) !! '(Int)';
            } elsif $has_list && nqp::istype($target, $List) {
                return '(List)' unless nqp::isconcrete($target);
                my $storage := nqp::getattr($target, $List, '$!reified');
                my @result;
                for $storage {
                    nqp::push(@result, safely_stringify($_));
                }
                return "(" ~ join(", ", @result) ~ ")";
            }
            elsif nqp::ishash($target) {
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
        nqp::findmethod(HLL::Grammar, 'panic')($/, join('', @err));
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
            my $*LEAF := $/;
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
        try { $coercer := self.find_single_symbol('&COMP_EXCEPTION', :setting-only); ++$success; };
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
                nqp::box_s(self.current_file,self.find_single_symbol('Str', :setting-only)),
                nqp::box_i(self.current_line($/),self.find_single_symbol('Int', :setting-only)),
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
        try { $coercer := self.find_single_symbol('&COMP_EXCEPTION', :setting-only); ++$success; };
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
                nqp::box_s(self.current_file,self.find_single_symbol('Str', :setting-only)),
                nqp::box_i(self.current_line($/),self.find_single_symbol('Int', :setting-only)),
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
            my int $e := nqp::chars($v);
            my int $i := -1;
            while ++$i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
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

    method quote_lang_cache() {
        %!quote_lang_cache
    }

    method herestub_queue() {
        self.context.herestub_queue
    }
}

# vim: expandtab sw=4
