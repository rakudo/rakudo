use NQPP6QRegex;
use NQPP5QRegex;
use Raku::Actions;

my role startstops[$start, $stop1, $stop2] {
    token starter { $start }
    token stopper { $stop1 | $stop2 }
}

my role startstop[$start, $stop] {
    token starter { $start }
    token stopper { $stop }
}

my role stop[$stop] {
    token starter { <!> }
    token stopper { $stop }
}

role Raku::Common {
    ## Quote parsing

    token opener {
        <[
        \x0028 \x003C \x005B \x007B \x00AB \x0F3A \x0F3C \x169B \x2018 \x201A \x201B
        \x201C \x201E \x201F \x2039 \x2045 \x207D \x208D \x2208 \x2209 \x220A \x2215
        \x223C \x2243 \x2252 \x2254 \x2264 \x2266 \x2268 \x226A \x226E \x2270 \x2272
        \x2274 \x2276 \x2278 \x227A \x227C \x227E \x2280 \x2282 \x2284 \x2286 \x2288
        \x228A \x228F \x2291 \x2298 \x22A2 \x22A6 \x22A8 \x22A9 \x22AB \x22B0 \x22B2
        \x22B4 \x22B6 \x22C9 \x22CB \x22D0 \x22D6 \x22D8 \x22DA \x22DC \x22DE \x22E0
        \x22E2 \x22E4 \x22E6 \x22E8 \x22EA \x22EC \x22F0 \x22F2 \x22F3 \x22F4 \x22F6
        \x22F7 \x2308 \x230A \x2329 \x23B4 \x2768 \x276A \x276C \x276E \x2770 \x2772
        \x2774 \x27C3 \x27C5 \x27D5 \x27DD \x27E2 \x27E4 \x27E6 \x27E8 \x27EA \x2983
        \x2985 \x2987 \x2989 \x298B \x298D \x298F \x2991 \x2993 \x2995 \x2997 \x29C0
        \x29C4 \x29CF \x29D1 \x29D4 \x29D8 \x29DA \x29F8 \x29FC \x2A2B \x2A2D \x2A34
        \x2A3C \x2A64 \x2A79 \x2A7D \x2A7F \x2A81 \x2A83 \x2A8B \x2A91 \x2A93 \x2A95
        \x2A97 \x2A99 \x2A9B \x2AA1 \x2AA6 \x2AA8 \x2AAA \x2AAC \x2AAF \x2AB3 \x2ABB
        \x2ABD \x2ABF \x2AC1 \x2AC3 \x2AC5 \x2ACD \x2ACF \x2AD1 \x2AD3 \x2AD5 \x2AEC
        \x2AF7 \x2AF9 \x2E02 \x2E04 \x2E09 \x2E0C \x2E1C \x2E20 \x2E28 \x3008 \x300A
        \x300C \x300E \x3010 \x3014 \x3016 \x3018 \x301A \x301D \xFD3E \xFE17 \xFE35
        \xFE37 \xFE39 \xFE3B \xFE3D \xFE3F \xFE41 \xFE43 \xFE47 \xFE59 \xFE5B \xFE5D
        \xFF08 \xFF1C \xFF3B \xFF5B \xFF5F \xFF62
        ]>
    }

    method balanced($start, $stop) {
        if nqp::istype($stop, VMArray) {
            self.HOW.mixin(self, startstops.HOW.curry(startstops, $start, $stop[0], $stop[1]));
        }
        else {
            self.HOW.mixin(self, startstop.HOW.curry(startstop, $start, $stop));
        }
    }

    method unbalanced($stop) {
        self.HOW.mixin(self, stop.HOW.curry(stop, $stop));
    }

    token starter { <!> }
    token stopper { <!> }

    method quote_lang($l, $start, $stop, @base_tweaks?, @extra_tweaks?) {
        sub lang_key() {
            my $stopstr := nqp::istype($stop,VMArray) ?? nqp::join(' ',$stop) !! $stop;
            my @keybits := [
                self.HOW.name(self), $l.HOW.name($l), $start, $stopstr
            ];
            for @base_tweaks {
                @keybits.push($_);
            }
            for @extra_tweaks {
                if $_[0] eq 'to' {
                    return 'NOCACHE';
                }
                @keybits.push($_[0] ~ '=' ~ $_[1]);
            }
            nqp::join("\0", @keybits)
        }
        sub con_lang() {
            my $lang := $l.'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'()));
            $lang.clone_braid_from(self);
            for @base_tweaks {
                $lang := $lang."tweak_$_"(1);
            }

            for @extra_tweaks {
                my $t := $_[0];
                if nqp::can($lang, "tweak_$t") {
                    $lang := $lang."tweak_$t"($_[1]);
                }
                else {
                    self.sorry("Unrecognized adverb: :$t");
                }
            }
            for self.slangs {
                if nqp::istype($lang, $_.value) {
                    $lang.set_actions(self.slang_actions($_.key));
                    last;
                }
            }
            $lang.set_pragma("STOPPER",$stop);
            nqp::istype($stop,VMArray) ||
            $start ne $stop ?? $lang.balanced($start, $stop)
                            !! $lang.unbalanced($stop);
        }

        # Get language from cache or derive it.
        my $key := lang_key();
        my %quote_lang_cache := %*QUOTE_LANGS;
        my $quote_lang := nqp::existskey(%quote_lang_cache, $key) && $key ne 'NOCACHE'
            ?? %quote_lang_cache{$key}
            !! (%quote_lang_cache{$key} := con_lang());
        $quote_lang.set_package(self.package);
        $quote_lang;
    }

    # Note, $lang must carry its own actions by the time we call this.
    method nibble($lang) {
        $lang.'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'())).nibbler().set_braid_from(self)
    }

    method fail-terminator ($/, $start, $stop, $line?) {
        my $message;
        if $start ne nqp::chr(nqp::ord($start)) {
            $message := "Starter $start is immediately followed by a combining codepoint. Please use {nqp::chr(nqp::ord($start))} without a combining glyph";
            if $line {
                $message := "$message ($start was at line $line)";
            }
        }
        else {
            $message := "Couldn't find terminator $stop";
            if $line {
                $message := "$message (corresponding $start was at line $line)";
            }
        }
        $/.typed_panic('X::Comp::AdHoc',
            payload => $message,
            expected => [$stop]
        );
    }

    my class Herestub {
        has $!delim;
        has $!orignode;
        has $!grammar;
        method delim() { $!delim }
        method orignode() { $!orignode }
        method grammar() { $!grammar }
    }

    role herestop {
        token starter { <!> }
        token stopper { ^^ {} $<ws>=(\h*) $*DELIM \h* $$ [\r\n | \v]? }
        method parsing_heredoc() { 1 }
    }

    method heredoc () {
        my $actions := self.actions;
        if $*CU && my @herestub_queue := $*CU.herestub-queue {
            my $here := self.'!cursor_start_cur'();
            $here.'!cursor_pos'(self.pos);
            while @herestub_queue {
                my $herestub := nqp::shift(@herestub_queue);
                my $*DELIM := $herestub.delim;
                my $lang := $herestub.grammar.HOW.mixin($herestub.grammar, herestop);
                for self.slangs {
                    if nqp::istype($lang, $_.value) {
                        $lang.set_actions(self.slang_actions($_.key));
                        last;
                    }
                }
                my $doc := $here.nibble($lang);
                if $doc {
                    # Match stopper.
                    my $stop := $lang.'!cursor_init'(self.orig(), :p($doc.pos), :shared(self.'!shared'())).stopper();
                    $stop.clone_braid_from(self);
                    unless $stop {
                        self.panic("Ending delimiter $*DELIM not found");
                    }
                    $here.'!cursor_pos'($stop.pos);

                    # Get it trimmed and AST updated.
                    my $heredoc := $herestub.orignode.MATCH.ast;
                    $heredoc.replace-segments-from($doc.MATCH.ast);
                    $heredoc.set-stop($stop);
                    $heredoc.trim;
                }
                else {
                    self.panic("Ending delimiter $*DELIM not found");
                }
            }
            $here.'!cursor_pass'($here.pos);
            $here.set_actions($actions);
            $here
        }
        else {
            self
        }
    }

    token quibble($l, *@base_tweaks) {
        :my $lang;
        :my $start;
        :my $stop;
        <babble($l, @base_tweaks)>
        { my $B := $<babble><B>.ast; $lang := $B[0]; $start := $B[1]; $stop := $B[2]; }

        $start
        <nibble($lang)>
        [
        || $stop
        || {
            self.fail-terminator($/, $start, $stop,
                HLL::Compiler.lineof($<babble><B>.orig(), $<babble><B>.from(), :cache(1)))
           }
        ]

        {
            nqp::can($lang, 'herelang') && $*CU.queue-heredoc(
                Herestub.new(
                    :delim(
                        $<nibble>.ast.literal-value
                        // $/.panic("Stopper '" ~ $<nibble> ~ "' too complex for heredoc")
                    ),
                    :grammar($lang.herelang),
                    :orignode(self),
                )
            );
        }
    }

    token babble($l, @base_tweaks?) {
        :my @extra_tweaks;

        [ <quotepair> <.ws>
            {
                my $pair := $<quotepair>[-1].ast;
                my $k  := $pair.key;
                my $v := $pair.value;
                unless nqp::can($v, 'compile-time-value') {
                    self.panic("Invalid adverb value for " ~ $<quotepair>[-1].Str ~ ': ' ~ $v.HOW.name($v));
                }
                $v := $v.compile-time-value;
                nqp::push(@extra_tweaks, [$k, $v]);
            }
        ]*

        $<B>=[<?before .>]
        {
            # Work out the delimiters.
            my $c := $/;
            my @delims := $c.peek_delimiters($c.target, $c.pos);
            my $start := @delims[0];
            my $stop  := @delims[1];

            # Get the language.
            my $lang := self.quote_lang($l, $start, $stop, @base_tweaks, @extra_tweaks);
            $<B>.make([$lang, $start, $stop]);
        }
    }

    token RESTRICTED {
        [ <?{ $*RESTRICTED }> [ $ || <.security($*RESTRICTED)> ] ]?
        <!>
    }

    ## Error handling

    method security($payload) {
        self.typed_panic('X::SecurityPolicy::Eval', :$payload);
    }
    method malformed($what) {
        self.typed_panic('X::Syntax::Malformed', :$what);
    }
    method missing($what) {
        self.typed_panic('X::Syntax::Missing', :$what);
    }
    method missing_block($borg, $has_mystery) {
        my $marked := self.MARKED('ws');
        my $pos := $marked ?? $marked.from !! self.pos;

        if $borg<block> {
            self.'!clear_highwater'();
            self.'!cursor_pos'($borg<block>.pos);
            self.typed_sorry('X::Syntax::BlockGobbled', what => ($borg<name> // ''));
            self.'!cursor_pos'($pos);
            self.missing("block (apparently claimed by " ~ ($borg<name> ?? "'" ~ $borg<name> ~ "'" !! "expression") ~ ")");
        } elsif $pos > 0 && nqp::eqat(self.orig(), '}', $pos - 1) {
            self.missing("block (whitespace needed before curlies taken as a hash subscript?)");
        } elsif $has_mystery {
            self.missing("block (taken by some undeclared routine?)");
        } else {
            self.missing("block");
        }
    }
    method NYI($feature) {
        self.typed_panic('X::Comp::NYI', :$feature)
    }
    method EXPR_nonassoc($cur, $left, $right) {
        self.typed_panic('X::Syntax::NonAssociative', :left(~$left), :right(~$right));
    }
    method EXPR_nonlistassoc($cur, $left, $right) {
        self.typed_panic('X::Syntax::NonListAssociative', :left(~$left), :right(~$right));
    }
    method dupprefix($prefixes) {
        self.typed_panic('X::Syntax::DuplicatedPrefix', :$prefixes);
    }
    token obsbrace { <.obs('curlies around escape argument','square brackets')> }

    method panic(*@args) {
        self.typed_panic('X::Comp::AdHoc', payload => nqp::join('', @args))
    }
    method sorry(*@args) {
        self.typed_sorry('X::Comp::AdHoc', payload => nqp::join('', @args))
    }
    method worry(*@args) {
        self.typed_worry('X::Comp::AdHoc', payload => nqp::join('', @args))
    }

    method typed_panic($type_str, *%opts) {
        $*R.panic(self.build_exception($type_str, |%opts));
    }
    method typed_sorry($type_str, *%opts) {
        if $*SORRY_REMAINING-- {
            $*R.add-sorry(self.build_exception($type_str, |%opts));
            self
        }
        else {
            self.typed_panic($type_str, |%opts)
        }
    }
    method typed_worry($type_str, *%opts) {
        $*R.add-worry(self.build_exception($type_str, |%opts));
        self
    }

    method build_exception($type_str, *%opts) {
        my $c := self;
        if %opts<precursor> {
            $c := self.PRECURSOR;
        }

        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isnull($file) {
            $file := '<unknown file>';
        }
        elsif !nqp::eqat($file,'/',0) && !nqp::eqat($file,'-',0) && !nqp::eqat($file,':',1) {
            $file := nqp::cwd ~ '/' ~ $file;
        }

        my @locprepost := self.'!locprepost'($c);
        $*R.build-exception: $type_str,
            line => HLL::Compiler.lineof($c.orig, $c.pos, :cache(1)),
            pos => $c.pos,
            pre => @locprepost[0],
            post => @locprepost[1],
            file => $file,
            |%opts
    }

    method !locprepost($c) {
        my $orig := $c.orig;
        my $marked := $c.MARKED('ws');
        my $pos  := $marked && nqp::index(" }])>»", nqp::substr($orig, $c.pos, 1)) < 0
            ?? $marked.from
            !! $c.pos;

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

    method FAILGOAL($goal, $dba?) {
        my $stopper;
        unless $dba {
            $dba := nqp::getcodename(nqp::callercode());
            # Handle special case to conceal variable name leaked by core grammar
            if ~$goal eq '$stopper ' {
                my $ch := $dba ~~ /[post]?circumfix\:sym[\<|\«]\S+\s+(\S+)[\>|\»]/;
                $ch := ~$ch[0];
                if nqp::chars($ch) {
                    $stopper := "'" ~ $ch ~ "'";
                }
            }
        }
        # core grammar also has a penchant for sending us trailing .ws contents
        $stopper := $stopper // $goal;
        $stopper := $stopper ~~ /(.*\S)\s*/;
        $stopper := ~$stopper[0];
        self.typed_panic('X::Comp::FailGoal', :$dba, :goal($stopper),
                         :line-real(HLL::Compiler.lineof(self.orig(), self.from(),
                                                         :cache(1))));
    }

    # "when" arg assumes more things will become obsolete after Raku comes out...
    method obs($old, $new, $when = 'in Raku', :$ism = 'p5isms') {
        # TODO isms
#        unless $*LANG.pragma($ism) {
            self.typed_panic: 'X::Obsolete',
                old         => $old,
                replacement => $new,
                when        => $when;
#        }
        self;
    }
    method obsvar($name, $identifier-name?) {
        # TODO isms
#        unless $*LANG.pragma('p5isms') {
            self.typed_panic: 'X::Syntax::Perl5Var',
              :$name, :$identifier-name;
#        }
        self;
    }
    method sorryobs($old, $new, $when = 'in Raku') {
        # TODO isms
#        unless $*LANG.pragma('p5isms') {
            self.typed_sorry: 'X::Obsolete',
                old         => $old,
                replacement => $new,
                when        => $when;
#        }
        self;
    }
    method worryobs($old, $new, $when = 'in Raku') {
        # TODO isms
#        unless $*LANG.pragma('p5isms') {
            self.typed_worry: 'X::Obsolete',
                old         => $old,
                replacement => $new,
                when        => $when;
#        }
        self;
    }

    method check_variable($var) {
        my $ast := $var.ast;
        if $ast ~~ self.actions.r('Var', 'Lexical') {
            $ast.resolve-with($*R);
            unless $ast.is-resolved || $ast.sigil eq '&' {
                # TODO restore good error
                nqp::die("Undeclared variable " ~ $ast.name);
            }
        }
    }
}

grammar Raku::Grammar is HLL::Grammar does Raku::Common {
    ##
    ## Compilation unit, language version and other entry point bits
    ##

    method TOP() {
        # Set up the language braid.
        my $*LANG := self;
        my $*MAIN := 'MAIN';
        self.define_slang('MAIN',    self.WHAT,            self.actions);
        self.define_slang('Quote',   Raku::QGrammar,       Raku::QActions);
        self.define_slang('Regex',   Raku::RegexGrammar,   Raku::RegexActions);
        #self.define_slang('P5Regex', Raku::P5RegexGrammar, Raku::P5RegexActions);
        #self.define_slang('Pod',     Raku::PodGrammar,     Raku::PodActions);

        # Variables used during the parse.
        my $*IN_DECL;                             # what declaration we're in
        my $*OFTYPE;                              # type of the current declarator
        my $*LEFTSIGIL;                           # sigil of LHS for item vs list assignment
        my $*IN_META := '';                       # parsing a metaoperator like [..]
        my $*IN_REDUCE := 0;                      # attempting to parse an [op] construct
        my %*QUOTE_LANGS;                         # quote language cache
        my $*LASTQUOTE := [0,0];                  # for runaway quote detection
        my $*SORRY_REMAINING := 10;               # decremented on each sorry; panic when 0
        my $*BORG := {};                          # who gets blamed for a missing block

        # Variables used for Pod parsing.
        my $*VMARGIN := 0;
        my $*ALLOW_INLINE_CODE := 0;
        my $*POD_IN_CODE_BLOCK := 0;
        my $*POD_IN_FORMATTINGCODE := 0;
        my $*POD_ALLOW_FCODES := 0b11111111111111111111111111;
        my $*POD_ANGLE_COUNT := 0;

        # Parse a compilation unit.
        self.comp_unit
    }

    token comp_unit {
        <.bom>?

        # Set up compilation unit and symbol resolver according to the language
        # version that is declared, if any.
        :my $*CU;
        :my $*R;
        :my $*LITERALS;
        :my $*EXPORT;
        <.lang_setup>

        { $*R.enter-scope($*CU); $*R.create-scope-implicits(); }
        <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist')>
        [ $ || <.typed_panic: 'X::Syntax::Confused'> ]
        { $*R.leave-scope() }
    }

    token bom { \xFEFF }

    rule lang_setup {
        # TODO validate this and pay attention to it in actions
        [ <.ws>? 'use' <version> ';'? ]?
    }

    # This is like HLL::Grammar.LANG but it allows to call a token of a Raku level grammar.
    method FOREIGN_LANG($langname, $regex) {
        my $grammar := self.slang_grammar($langname);
        if nqp::istype($grammar, NQPMatch) {
            self.LANG($langname, $regex);
        }
        else {
            nqp::die('FOREIGN_LANG non-NQP branch NYI')
        }
    }

    ##
    ## Statements
    ##

    rule statementlist {
        :dba('statement list')
        <.ws>
        # Define this scope to be a new language.
        :my $*LANG;
        <!!{ $*LANG := $/.clone_braid_from(self); 1 }>
        [
        | $
        | <?before <.[\)\]\}]>>
        | [ <statement> <.eat_terminator> ]*
        ]
        <.set_braid_from(self)>   # any language tweaks must not escape
        <!!{ nqp::rebless($/, self.WHAT); 1 }>
    }

    rule semilist {
        :dba('list composer')
        ''
        [
        | <?before <.[)\]}]> >
        | [<statement><.eat_terminator> ]*
        ]
    }

    rule sequence {
        :dba('sequence of statements')
        ''
        [
        | <?before <.[)\]}]> >
        | [<statement><.eat_terminator> ]*
        ]
    }

    token statement {
        :my $*QSIGIL := '';
        :my $*SCOPE := '';

        :my $actions := self.slang_actions('MAIN');
        <!!{ $/.set_actions($actions); 1 }>
        <!before <.[\])}]> | $ >
        #<!stopper>
        <!!{ nqp::rebless($/, self.slang_grammar('MAIN')); 1 }>

        [
        | <label> <statement>
        | <statement_control>
        | <EXPR> :dba('statement end')
            [
            || <?MARKED('endstmt')>
            || :dba('statement modifier') <.ws> <statement_mod_cond> <statement_mod_loop>?
            || :dba('statement modifier loop') <.ws> <statement_mod_loop>
                {
                    my $sp := $<EXPR><statement_prefix>;
                    if $sp && $sp<sym> eq 'do' {
                        my $s := $<statement_mod_loop><sym>;
                        $/.obs("do..." ~ $s, "repeat...while or repeat...until")
                          unless $*LANG.pragma('p5isms');
                    }
                }
            ]?
        | <?[;]>
        #| <?stopper>
        | {} <.panic: "Bogus statement">
        ]
    }

    token label {
        <identifier> ':' <?[\s]> <.ws>
    }

    token eat_terminator {
        || ';'
        || <?MARKED('endstmt')> <.ws>
        || <?before ')' | ']' | '}' >
        || $
        || <?stopper>
        || <?before [if|while|for|loop|repeat|given|when] » > { $/.'!clear_highwater'(); self.typed_panic( 'X::Syntax::Confused', reason => "Missing semicolon" ) }
        || { $/.typed_panic( 'X::Syntax::Confused', reason => "Confused" ) }
    }

    token pblock {
        :dba('block or pointy block')
        :my $borg := $*BORG;
        :my $has_mystery := 0; # TODO
        { $*BORG := {} }
        :my $*BLOCK;
        [
        | <lambda>
          :my $*GOAL := '{';
          <.enter-block-scope('PointyBlock')>
          <signature>
          <blockoid>
          <.leave-block-scope>
        | <?[{]>
          <.enter-block-scope('Block')>
          <blockoid>
          <.leave-block-scope>
        || <.missing_block($borg, $has_mystery)>
        ]
    }

    token block {
        :dba('scoped block')
        :my $borg := $*BORG;
        :my $has_mystery := 0; # TODO
        { $*BORG := {} }
        :my $*BLOCK;
        [
        || <?[{]>
           <.enter-block-scope('Block')>
           <blockoid>
           <.leave-block-scope>
        || <.missing_block($borg, $has_mystery)>
        ]
    }

    token blockoid {
        :my $borg := $*BORG;
        :my $has_mystery := 0; # TODO
        :my $*MULTINESS := '';
        { $*BORG := {} }
        [
        | '{YOU_ARE_HERE}' <you_are_here>
        | :dba('block')
          '{'
          <statementlist>
          '}'
          <?ENDSTMT>
        || <.missing_block($borg, $has_mystery)>
        ]
    }

    token enter-block-scope($*SCOPE-KIND) { <?> }
    token leave-block-scope() { <?> }

    proto rule statement_control { <...> }

    rule statement_control:sym<if> {
        $<sym>=[if|with]<.kok> {}
        :my $*GOAL := '{';
        :my $*BORG := {};
        <condition=.EXPR>
        <then=.pblock>
        [
            [
            | 'else'\h*'if' <.typed_panic: 'X::Syntax::Malformed::Elsif'>
            | 'elif' { $/.typed_panic('X::Syntax::Malformed::Elsif', what => "elif") }
            | $<sym>='elsif' <condition=.EXPR> <then=.pblock>
            | $<sym>='orwith' <condition=.EXPR> <then=.pblock>
            ]
        ]*
        {}
        [
            'else'
            <else=.pblock>
        ]?
    }

    rule statement_control:sym<unless> {
        $<sym>='unless'<.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
        [ <!before [els[e|if]|orwith]» >
            || $<wrong-keyword>=[els[e|if]|orwith]» {}
                <.typed_panic: 'X::Syntax::UnlessElse',
                    keyword => ~$<wrong-keyword>,
                >
        ]
    }

    rule statement_control:sym<without> {
        $<sym>='without'<.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
        [ <!before [els[e|if]|orwith]» >
            || $<wrong-keyword>=[els[e|if]|orwith]» {}
                <.typed_panic: 'X::Syntax::WithoutElse',
                    keyword => ~$<wrong-keyword>,
                >
        ]
    }

    rule statement_control:sym<while> {
        $<sym>=[while|until]<.kok> {}
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
    }

    rule statement_control:sym<repeat> {
        <sym><.kok> {}
        [
        | $<wu>=[while|until]<.kok>
          :my $*GOAL := '{';
          :my $*BORG := {};
          <EXPR>
          <pblock>
        | <pblock>
          [$<wu>=['while'|'until']<.kok> || <.missing('"while" or "until"')>]
          <EXPR>
        ]
    }

    token statement_control:sym<loop> {
        <sym><.kok>
        :s''
        [
          :my $exprs := 0;
          '('
          [     <e1=.EXPR>? {$exprs := 1 if $<e1>}
          [ ';' <e2=.EXPR>? {$exprs := 2}
          [ ';' <e3=.EXPR>? {$exprs := 3}
          ]? ]? ]? # succeed anyway, this will leave us with a nice cursor
          [
          || <?{ $exprs == 3 }> ')'
          || <?before ')'>
             [
             || <?{ $exprs == 0 }>
                <.malformed("loop spec (expected 3 semicolon-separated expressions)")>
             || <.malformed("loop spec (expected 3 semicolon-separated expressions but got {$exprs})")>
             ]
          || <?before ‘;’>
             <.malformed('loop spec (expected 3 semicolon-separated expressions but got more)')>
          || <.malformed('loop spec')>
          ]
        ]?
        <block>
    }

    rule statement_control:sym<for> {
        <sym><.kok> {}
        [ <?before 'my'? '$'\w+\s+'(' >
            <.typed_panic: 'X::Syntax::P5'> ]?
        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
            <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
    }

    rule statement_control:sym<given> {
        <sym><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
    }

    rule statement_control:sym<when> {
        <sym><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pblock>
    }

    rule statement_control:sym<default> {
        <sym><.kok> <block>
    }

    rule statement_control:sym<CATCH> { <sym> <block> }
    rule statement_control:sym<CONTROL> { <sym> <block> }

    token statement_control:sym<use> {
        # TODO this is massively simplified
        <sym> <.ws>
        [
        | <version>
            { $/.typed_panic: 'X::Language::TooLate', version => ~$<version> }
        | <module_name=.longname> [ <.spacey> <arglist> ]?
        ]
        <.ws>
    }

    ##
    ## Statement modifiers
    ##

    method nomodexpr($k) {
        self.'!clear_highwater'();
        self.typed_panic( 'X::Syntax::Confused', reason => "Missing expression for '$k' statement modifier" );
        self
    }

    token modifier_expr($k) { <EXPR> || <.nomodexpr($k)> }

    proto rule statement_mod_cond { <...> }
    rule statement_mod_cond:sym<if>      { <sym><.kok> <modifier_expr('if')> }
    rule statement_mod_cond:sym<unless>  { <sym><.kok> <modifier_expr('unless')> }
    rule statement_mod_cond:sym<when>    { <sym><.kok> <modifier_expr('when')> }
    rule statement_mod_cond:sym<with>    { <sym><.kok> <modifier_expr('with')> }
    rule statement_mod_cond:sym<without> { <sym><.kok> <modifier_expr('without')> }

    proto rule statement_mod_loop { <...> }
    rule statement_mod_loop:sym<while> { <sym><.kok> <modifier_expr('while')> }
    rule statement_mod_loop:sym<until> { <sym><.kok> <modifier_expr('until')> }
    rule statement_mod_loop:sym<given> { <sym><.kok> <modifier_expr('given')> }
    rule statement_mod_loop:sym<for>   { <sym><.kok> <modifier_expr('for')> }

    ##
    ## Statement prefixes
    ##

    proto token statement_prefix { <...> }

    token statement_prefix:sym<BEGIN> { <sym><.kok> <blorst> }

    token statement_prefix:sym<END>   { <sym><.kok> <blorst> }

    token statement_prefix:sym<race>    { <sym><.kok> <blorst> }
    token statement_prefix:sym<hyper>   { <sym><.kok> <blorst> }
    token statement_prefix:sym<lazy>    { <sym><.kok> <blorst> }
    token statement_prefix:sym<eager>   { <sym><.kok> <blorst> }
    token statement_prefix:sym<try>     { <sym><.kok> <blorst> }
    token statement_prefix:sym<do>      { <sym><.kok> <blorst> }
    token statement_prefix:sym<quietly> { <sym><.kok> <blorst> }
    token statement_prefix:sym<gather>  { <sym><.kok> <blorst> }
    token statement_prefix:sym<start>   { <sym><.kok> <blorst> }

    token blorst {
        [
        | <?[{]> <block>
        | <![;]> <statement> # <.cheat_heredoc>?
        || <.missing: 'block or statement'>
        ]
    }

    ##
    ## Expression parsing and operators
    ##

    # Precedence levels and their defaults
    my %methodcall      := nqp::hash('prec', 'y=', 'assoc', 'unary', 'dba', 'methodcall', 'fiddly', 1);
    my %autoincrement   := nqp::hash('prec', 'x=', 'assoc', 'unary', 'dba', 'autoincrement');
    my %exponentiation  := nqp::hash('prec', 'w=', 'assoc', 'right', 'dba', 'exponentiation');
    my %symbolic_unary  := nqp::hash('prec', 'v=', 'assoc', 'unary', 'dba', 'symbolic unary');
    my %dottyinfix      := nqp::hash('prec', 'v=', 'assoc', 'left', 'dba', 'dotty infix', 'nextterm', 'dottyopish', 'sub', 'z=', 'fiddly', 1);
    my %multiplicative  := nqp::hash('prec', 'u=', 'assoc', 'left', 'dba', 'multiplicative');
    my %additive        := nqp::hash('prec', 't=', 'assoc', 'left', 'dba', 'additive');
    my %replication     := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication');
    my %replication_xx  := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication', 'thunky', 't.');
    my %concatenation   := nqp::hash('prec', 'r=', 'assoc', 'left', 'dba', 'concatenation');
    my %junctive_and    := nqp::hash('prec', 'q=', 'assoc', 'list', 'dba', 'junctive and');
    my %junctive_or     := nqp::hash('prec', 'p=', 'assoc', 'list', 'dba', 'junctive or');
    my %named_unary     := nqp::hash('prec', 'o=', 'assoc', 'unary', 'dba', 'named unary');
    my %structural      := nqp::hash('prec', 'n=', 'assoc', 'non', 'dba', 'structural infix', 'diffy', 1);
    my %chaining        := nqp::hash('prec', 'm=', 'assoc', 'chain', 'dba', 'chaining', 'iffy', 1, 'diffy', 1);
    my %tight_and       := nqp::hash('prec', 'l=', 'assoc', 'left', 'dba', 'tight and', 'thunky', '.t');
    my %tight_or        := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or', 'thunky', '.t');
    my %tight_or_minmax := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or');
    my %conditional     := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', '.tt');
    my %conditional_ff  := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', 'tt');
    my %item_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'item assignment');
    my %list_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'list assignment', 'sub', 'e=', 'fiddly', 1);
    my %loose_unary     := nqp::hash('prec', 'h=', 'assoc', 'unary', 'dba', 'loose unary');
    my %comma           := nqp::hash('prec', 'g=', 'assoc', 'list', 'dba', 'comma', 'nextterm', 'nulltermish', 'fiddly', 1);
    my %list_infix      := nqp::hash('prec', 'f=', 'assoc', 'list', 'dba', 'list infix');
    my %list_prefix     := nqp::hash('prec', 'e=', 'assoc', 'right', 'dba', 'list prefix');
    my %loose_and       := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.t');
    my %loose_andthen   := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.b');
    my %loose_or        := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.t');
    my %loose_orelse    := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.b');
    my %sequencer       := nqp::hash('prec', 'b=', 'assoc', 'list', 'dba', 'sequencer');

    method can_meta($op, $meta, $reason = "fiddly") {
        if $op<OPER> && $op<OPER><O>.made{$reason} == 1 {
            self.typed_panic: "X::Syntax::CannotMeta", :$meta, operator => ~$op<OPER>, dba => ~$op<OPER><O>.made<dba>, reason => "too $reason";
        }
        self;
    }

    method EXPR(str $preclim = '') {
        my $*LEFTSIGIL := '';
        nqp::findmethod(HLL::Grammar, 'EXPR')(self, $preclim, :noinfix($preclim eq 'y='));
    }

    token infixish($in_meta = nqp::getlexdyn('$*IN_META')) {
        :my $*IN_META := $in_meta;
        :my $*OPER;
        <!stdstopper>
        <!infixstopper>
        :dba('infix')
        [
        | <!{ $*IN_REDUCE }> <colonpair> <fake_infix> { $*OPER := $<fake_infix> }
        |   [
            | :dba('bracketed infix') '[' ~ ']' <infixish('[]')> { $*OPER := $<infixish><OPER> }
            | :dba('infixed function') <?before '[&' <twigil>? [<alpha>|'('] > '[' ~ ']' <variable>
                {
                    $<variable><O> := self.O(:prec<t=>, :assoc<left>, :dba<additive>).MATCH unless $<variable><O>;
                    $*OPER := $<variable>;
                    self.check_variable($<variable>);
                }
            | <infix_circumfix_meta_operator> { $*OPER := $<infix_circumfix_meta_operator> }
            | <infix_prefix_meta_operator> { $*OPER := $<infix_prefix_meta_operator> }
            | <infix> { $*OPER := $<infix> }
            | <?{ $*IN_META ~~ /^[ '[]' | 'hyper' | 'HYPER' | 'R' | 'S' ]$/ && !$*IN_REDUCE }> <.missing("infix inside " ~ $*IN_META)>
            ]
            [ <?before '='> <infix_postfix_meta_operator> { $*OPER := $<infix_postfix_meta_operator> } ]?
        ]
        <OPER=.AS_MATCH($*OPER)>
        { nqp::bindattr_i($<OPER>, NQPMatch, '$!pos', $*OPER.pos); }
    }

    token fake_infix {
        <O(|%item_assignment, :assoc<unary>, :fake<1>, :dba<adverb>)>
    }

    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before '!!'> <?{ $*GOAL eq '!!' }>
        | <?before '{' | <.lambda> > <?MARKED('ws')> <?{ $*GOAL eq '{' || $*GOAL eq 'endargs' }>
        ]
    }

    proto token infix_prefix_meta_operator { <...> }

    token infix_prefix_meta_operator:sym<!> {
        <sym> <![!]> {} [ <infixish('neg')> || <.panic: "Negation metaoperator not followed by valid infix"> ]
        <!{ $<infixish>.Str eq '=' }>
        [
        || <.can_meta($<infixish>, "negate")> <?{ $<infixish><OPER><O>.made<iffy> }>
           <O=.AS_MATCH($<infixish><OPER><O>)>
        || { self.typed_panic: "X::Syntax::CannotMeta", meta => "negate", operator => ~$<infixish>, dba => ~$<infixish><OPER><O>.made<dba>, reason => "not iffy enough" }
        ]
    }

    token infix_prefix_meta_operator:sym<R> {
        <sym> <infixish('R')> {}
        <.can_meta($<infixish>, "reverse the args of")>
        <O=.revO($<infixish>)>
    }

    token revO($from) {
        :my $*FROM := $from<OPER><O>.made;
        <?>
    }

    token infix_prefix_meta_operator:sym<X> {
        <sym> <infixish('X')> {}
        <.can_meta($<infixish>, "cross with")>
        <O(|%list_infix)>
    }

    token infix_prefix_meta_operator:sym<Z> {
        <sym> <infixish('Z')> {}
        <.can_meta($<infixish>, "zip with")>
        <O(|%list_infix)>
    }

    proto token infix_postfix_meta_operator { <...> }

    token infix_postfix_meta_operator:sym<=> {
        :my %prec;
        :my %fudge_oper;
        '='
        { %fudge_oper<OPER> := $*OPER }
        <.can_meta(%fudge_oper, "make assignment out of")>
        [ <!{ $*OPER<O>.made<diffy> }> || <.can_meta(%fudge_oper, "make assignment out of", "diffy")> ]
        {
            $<sym> := $*OPER<sym> ~ '=';
            if $*OPER<O>.made<prec> gt 'g=' {
                %prec := %item_assignment;
            }
            else {
                %prec := %list_assignment;
            }
        }
        <O(|%prec, :dba('assignment operator'), :iffy(0))> {}
    }

    proto token infix_circumfix_meta_operator { <...> }

    token infix_circumfix_meta_operator:sym<« »> {
        $<opening>=[ '«' | '»' ]
        {} <infixish('hyper')>
        $<closing>=[ '«' | '»' || <.missing("« or »")> ]
        <.can_meta($<infixish>, "hyper with")>
        {} <O=.AS_MATCH($<infixish><OPER><O>)>
    }

    token infix_circumfix_meta_operator:sym«<< >>» {
        $<opening>=[ '<<' | '>>' ]
        {} <infixish('HYPER')>
        $<closing>=[ '<<' | '>>' || <.missing("<< or >>")> ]
        {} <O=.AS_MATCH($<infixish><OPER><O>)>
    }

    token prefixish {
        :dba('prefix')
        <OPER=prefix>
        <prefix_postfix_meta_operator>?
        <.ws>
    }

    proto token prefix_postfix_meta_operator { <...> }

    token prefix_postfix_meta_operator:sym<«> {
        <sym> | '<<'
    }

    token postfixish {
        <!stdstopper>

        # last whitespace didn't end here
        <?{
            my $c := $/;
            my $marked := $c.MARKED('ws');
            !$marked || $marked.from == $c.pos;
        }>

        [ <!{ $*QSIGIL }> [ <.unsp> | '\\' ] ]?

        :dba('postfix')
        [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>?]?
        [
        | <OPER=postfix>
        | '.' <?before \W> <OPER=postfix>  ## dotted form of postfix operator (non-wordy only)
        | <OPER=postcircumfix>
        | '.' <?[ [ { < ]> <OPER=postcircumfix>
        | <OPER=dotty>
#        | <OPER=privop>
        | <?{ $<postfix_prefix_meta_operator> && !$*QSIGIL }>
            [
            || <?space> <.missing: "postfix">
            || <?alpha> <.missing: "dot on method call">
            || <.malformed: "postfix">
            ]
        ]
        { $*LEFTSIGIL := '@'; }
    }

    proto token postfix_prefix_meta_operator { <...> }

    token postfix_prefix_meta_operator:sym<»> {
        [ <sym> | $<sym> = '>>' ]
        [ <!{ $*QSIGIL }> || <![(]> ]
    }

    token postop {
        | <postfix>         $<O> = {$<postfix><O>} $<sym> = {$<postfix><sym>}
        | <postcircumfix>   $<O> = {$<postcircumfix><O>} $<sym> = {$<postcircumfix><sym>}
    }

    method AS_MATCH($v) {
        self.'!clone_match_at'($v,self.pos());
    }

    token postcircumfix:sym<[ ]> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '[' ~ ']' [ <.ws> <semilist> ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym<{ }> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '{' ~ '}' [ <.ws> <semilist> ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym<ang> {
        '<'
        [
        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w', 'v']))> '>'
        || '='* <?before \h* [ \d | <.sigil> | ':' ] >
           { $/.panic("Whitespace required before $/ operator") }
        || { $/.panic("Unable to parse quote-words subscript; couldn't find '>' (corresponding '<' was at line {HLL::Compiler.lineof($/.orig(), $/.from(), :cache(1))})") }
        ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym«<< >>» {
        :dba('shell-quote words')
        '<<'
        [
        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "<<", ">>", ['qq', 'ww', 'v']))> '>>'
        || { $/.panic("Unable to parse quote-words subscript; couldn't find '>>' (corresponding '<<' was at line {HLL::Compiler.lineof($/.orig(), $/.from(), :cache(1))})") }
        ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym<« »> {
        :dba('shell-quote words')
        '«'
        [
        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "«", "»", ['qq', 'ww', 'v']))> '»'
        || { $/.panic("Unable to parse quote-words subscript; couldn't find '»' (corresponding '«' was at line {HLL::Compiler.lineof($/.orig(), $/.from(), :cache(1))})") }
        ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym<( )> {
        :dba('argument list')
        '(' ~ ')' [ <.ws> <arglist> ]
        <O(|%methodcall)>
    }

    proto token dotty { <...> }
    token dotty:sym<.> {
        <sym> <dottyop>
        <O(|%methodcall)>
    }

    token dotty:sym<.^> {
        <sym> <dottyop('.^')>
        <O(|%methodcall)>
    }

    token dotty:sym<.?> {
        <sym> <dottyop('.?')>
        <O(|%methodcall)>
    }

    token dotty:sym<.&> {
        <sym> <dottyop('.&')>
        <O(|%methodcall)>
    }

    token dottyop($special?) {
        :dba('dotty method or postfix')
        <.unsp>?
        [
        | <methodop($special)>
#        | <colonpair>
        | <!alpha> <postop> $<O> = {$<postop><O>} $<sym> = {$<postop><sym>}
          <.dotty-non-ident($special)>
        ]
    }

    token methodop($*special) {
        [
        | <longname> {
                if $<longname> eq '::' { self.malformed("class-qualified postfix call") }
          }
#        | <?[$@&]> <variable> { self.check_variable($<variable>) }
        | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <.-["]>*? [\s|$] > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'."> ]
          <.dotty-non-ident($*special)>
        ] <.unsp>?
        :dba('method arguments')
        [
            [
            | <?[(]> <args>
            | ':' <?before \s | '{'> <!{ $*QSIGIL }> <args=.arglist>
            ]
            || <!{ $*QSIGIL }> <?>
            || <?{ $*QSIGIL }> <?[.]> <?>
        ] <.unsp>?
    }

    token dotty-non-ident($special) {
        | <!{ $special }>
        | <.panic("Cannot use $special on a non-identifier method call")>
    }

    token postfix:sym<i>  { <sym> >> <O(|%methodcall)> }

    token prefix:sym<++>  { <sym>  <O(|%autoincrement)> }
    token prefix:sym<-->  { <sym>  <O(|%autoincrement)> }
    token prefix:sym<++⚛> { <sym>  <O(|%autoincrement)> }
    token prefix:sym<--⚛> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<++> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<--> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<⚛++> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<⚛--> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<ⁿ> { <sign=[⁻⁺¯]>? <dig=[⁰¹²³⁴⁵⁶⁷⁸⁹]>+ <O(|%autoincrement)> }

    token infix:sym<**>   { <sym>  <O(|%exponentiation)> }

    token prefix:sym<+>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<~~>  { <sym> <.dupprefix('~~')> <O(|%symbolic_unary)> }
    token prefix:sym<~>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<->   { <sym> <O(|%symbolic_unary)> }
    token prefix:sym<−>   { <sym> <O(|%symbolic_unary)> }
    token prefix:sym<??>  { <sym> <.dupprefix('??')> <O(|%symbolic_unary)> }
    token prefix:sym<?>   { <sym> <!before '??'> <O(|%symbolic_unary)> }
    token prefix:sym<!>   { <sym> <!before '!!'> <O(|%symbolic_unary)> }
    token prefix:sym<|>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<+^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<~^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<?^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<^^>  { <sym> <.dupprefix('^^')> <O(|%symbolic_unary)> }
    token prefix:sym<^>   {
        <sym>  <O(|%symbolic_unary)>
        <?before \d+ <?before \. <.?alpha> > <.worry: "Precedence of ^ is looser than method call; please parenthesize"> >?
    }
    token prefix:sym<⚛>   { <sym>  <O(|%symbolic_unary)> }

    token infix:sym<.>    { <sym> <ws>
        <!{ $*IN_REDUCE }>
        [<!alpha>
            {
                my $pre := nqp::substr(self.orig, self.from - 1, 1);
                $<ws> ne ''
                ?? $¢.obs('. to concatenate strings', '~')
                !! $pre ~~ /^\s$/
                    ?? $¢.malformed('postfix call (only basic method calls that exclusively use a dot can be detached)')
                    !! $¢.malformed('postfix call')
            }
        ]?
        <O(|%dottyinfix)>
    }

    token infix:sym<.=> { <sym> <O(|%dottyinfix)> }

    token infix:sym<:=> {
        <sym> <O(|%list_assignment)>
    }

    token infix:sym<::=> {
        <sym> <O(|%item_assignment)> <.NYI('"::="')>
    }

    token dottyopish {
        <term=.dottyop>
    }

    token infix:sym<*>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<×>    { <sym>  <O(|%multiplicative)> }
    token infix:sym</>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<÷>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<div>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<gcd>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<lcm>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<%>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<mod>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<%%>   { <sym>  <O(|%multiplicative, :iffy(1))> }
    token infix:sym<+&>   { <sym>  <O(|%multiplicative)> }
    token infix:sym<~&>   { <sym>  <O(|%multiplicative)> }
    token infix:sym<?&>   { <sym>  <O(|%multiplicative, :iffy(1))> }
    token infix:sym«+<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
    token infix:sym«+>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }
    token infix:sym«~<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
    token infix:sym«~>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }

    token infix:sym«<<» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('<< to do left shift', '+< or ~<')> <O(|%multiplicative)> }

    token infix:sym«>>» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('>> to do right shift', '+> or ~>')> <O(|%multiplicative)> }

    token infix:sym<+>    { <sym>  <O(|%additive)> }
    token infix:sym<->    {
       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
        <sym> [<?before '>>'> || <![>]>]
        <O(|%additive)>
    }
    token infix:sym<−>    { <sym>  <O(|%additive)> }
    token infix:sym<+|>   { <sym>  <O(|%additive)> }
    token infix:sym<+^>   { <sym>  <O(|%additive)> }
    token infix:sym<~|>   { <sym>  <O(|%additive)> }
    token infix:sym<~^>   { <sym>  <O(|%additive)> }
    token infix:sym<?|>   { <sym>  <O(|%additive, :iffy(1))> }
    token infix:sym<?^>   { <sym>  <O(|%additive, :iffy(1))> }

    token infix:sym<x>    { <sym> >> <O(|%replication)> }
    token infix:sym<xx>    { <sym> >> <O(|%replication_xx)> }

    token infix:sym<~>    { <sym>  <O(|%concatenation)> }
    token infix:sym<∘>   { <sym>  <O(|%concatenation)> }
    token infix:sym<o>   { <sym>  <O(|%concatenation)> }

    token infix:sym<&>   { <sym> <O(|%junctive_and, :iffy(1))> }
    token infix:sym<(&)> { <sym> <O(|%junctive_and)> }
    token infix:sym«∩»   { <sym> <O(|%junctive_and)> }
    token infix:sym<(.)> { <sym> <O(|%junctive_and)> }
    token infix:sym«⊍»   { <sym> <O(|%junctive_and)> }

    token infix:sym<|>    { <sym> <O(|%junctive_or, :iffy(1))> }
    token infix:sym<^>    { <sym> <O(|%junctive_or, :iffy(1))> }
    token infix:sym<(|)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«∪»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(^)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«⊖»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(+)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«⊎»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(-)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«∖»    { <sym> <O(|%junctive_or)> }

    token prefix:sym<let>  { <sym><.kok> <O(|%named_unary)> }
    token prefix:sym<temp> { <sym><.kok> <O(|%named_unary)> }

    token infix:sym«=~=»  { <sym>  <O(|%chaining)> }
    token infix:sym«≅»    { <sym>  <O(|%chaining)> }
    token infix:sym«==»   { <sym>  <O(|%chaining)> }
    token infix:sym«!=»   { <sym> <?before \s|']'> <O(|%chaining)> }
    token infix:sym«≠»    { <sym>  <O(|%chaining)> }
    token infix:sym«<=»   { <sym>  <O(|%chaining)> }
    token infix:sym«≤»    { <sym>  <O(|%chaining)> }
    token infix:sym«>=»   { <sym>  <O(|%chaining)> }
    token infix:sym«≥»    { <sym>  <O(|%chaining)> }
    token infix:sym«<»    { <sym>  <O(|%chaining)> }
    token infix:sym«>»    { <sym>  <O(|%chaining)> }
    token infix:sym«eq»   { <sym> >> <O(|%chaining)> }
    token infix:sym«ne»   { <sym> >> <O(|%chaining)> }
    token infix:sym«le»   { <sym> >> <O(|%chaining)> }
    token infix:sym«ge»   { <sym> >> <O(|%chaining)> }
    token infix:sym«lt»   { <sym> >> <O(|%chaining)> }
    token infix:sym«gt»   { <sym> >> <O(|%chaining)> }
    token infix:sym«=:=»  { <sym>  <O(|%chaining)> }
    token infix:sym<===>  { <sym>  <O(|%chaining)> }
    token infix:sym<eqv>    { <sym> >> <O(|%chaining)> }
    token infix:sym<before> { <sym> >> <O(|%chaining)> }
    token infix:sym<after>  { <sym> >> <O(|%chaining)> }
    token infix:sym<~~>   { <sym> <O(|%chaining)> }
    token infix:sym<!~~>  { <sym> <O(|%chaining)> }
    token infix:sym<(elem)> { <sym> <O(|%chaining)> }
    token infix:sym«∈»      { <sym> <O(|%chaining)> }
    token infix:sym«∉»      { <sym> <O(|%chaining)> }
    token infix:sym<(cont)> { <sym> <O(|%chaining)> }
    token infix:sym«∋»      { <sym> <O(|%chaining)> }
    token infix:sym«∌»      { <sym> <O(|%chaining)> }
    token infix:sym«(<)»    { <sym> <O(|%chaining)> }
    token infix:sym«⊂»      { <sym> <O(|%chaining)> }
    token infix:sym«⊄»      { <sym> <O(|%chaining)> }
    token infix:sym«(>)»    { <sym> <O(|%chaining)> }
    token infix:sym«⊃»      { <sym> <O(|%chaining)> }
    token infix:sym«⊅»      { <sym> <O(|%chaining)> }
    token infix:sym«(<=)»   { <sym> <O(|%chaining)> }
    token infix:sym«⊆»      { <sym> <O(|%chaining)> }
    token infix:sym«⊈»      { <sym> <O(|%chaining)> }
    token infix:sym«(>=)»   { <sym> <O(|%chaining)> }
    token infix:sym«⊇»      { <sym> <O(|%chaining)> }
    token infix:sym«⊉»      { <sym> <O(|%chaining)> }
    token infix:sym«(<+)»   { <sym> <O(|%chaining)> }
    token infix:sym«≼»      { <sym> <O(|%chaining)> }
    token infix:sym«(>+)»   { <sym> <O(|%chaining)> }
    token infix:sym«≽»      { <sym> <O(|%chaining)> }

    token infix:sym<&&>   { <sym>  <O(|%tight_and, :iffy(1))> }

    token infix:sym<||>   { <sym>  <O(|%tight_or, :iffy(1), :assoc<left>)> }
    token infix:sym<^^>   { <sym>  <O(|%tight_or, :iffy(1), :thunky<..t>)> }
    token infix:sym<//>   { <sym>  <O(|%tight_or, :assoc<left>)> }
    token infix:sym<min>  { <sym> >> <O(|%tight_or_minmax)> }
    token infix:sym<max>  { <sym> >> <O(|%tight_or_minmax)> }

    token infix:sym<?? !!> {
        :my $*GOAL := '!!';
        $<sym>='??'
        <.ws>
        <EXPR('i=')>
        [ '!!'
        || <?before '::' <.-[=]>> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => "::" }
        || <?before ':' <.-[=\w]>> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => ":" }
        || <infixish> { self.typed_panic: "X::Syntax::ConditionalOperator::PrecedenceTooLoose", operator => ~$<infixish> }
        || <?{ ~$<EXPR> ~~ / '!!' / }> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartGobbled" }
        || <?before \N*? [\n\N*?]? '!!'> { self.typed_panic: "X::Syntax::Confused", reason => "Confused: Bogus code found before the !! of conditional operator" }
        || { self.typed_panic: "X::Syntax::Confused", reason => "Confused: Found ?? but no !!" }
        ]
        <O(|%conditional, :reducecheck<ternary>)>
    }

    token infix:sym«=>» { <sym> <O(|%item_assignment)> }

    token prefix:sym<so> { <sym><.end_prefix> <O(|%loose_unary)> }
    token prefix:sym<not>  { <sym><.end_prefix> <O(|%loose_unary)> }

    token infix:sym<minmax> { <sym> >> <O(|%list_infix)> }

    token infix:sym<,>    {
        <.unsp>? <sym> <O(|%comma, :fiddly(0))>
    }

    token infix:sym<Z>    { <!before <.sym> <.infixish> > <sym>  <O(|%list_infix)> }
    token infix:sym<X>    { <!before <.sym> <.infixish> > <sym>  <O(|%list_infix)> }

    token infix:sym<...>  { <sym> <O(|%list_infix)> }
    token infix:sym<…>    { <sym> <O(|%list_infix)> }
    token infix:sym<...^> { <sym>  <O(|%list_infix)> }
    token infix:sym<…^>   { <sym>  <O(|%list_infix)> }
    token infix:sym<^...> { <sym>  <O(|%list_infix)> }
    token infix:sym<^…>   { <sym>  <O(|%list_infix)> }
    token infix:sym<^...^> { <sym>  <O(|%list_infix)> }
    token infix:sym<^…^>   { <sym>  <O(|%list_infix)> }

    token infix:sym<?>    { <sym> {} <![?]> <?before <.-[;]>*?':'> <.obs('? and : for the ternary conditional operator', '?? and !!')> <O(|%conditional)> }

    token infix:sym<ff> { <sym> <O(|%conditional_ff)> }
    token infix:sym<^ff> { <sym> <O(|%conditional_ff)> }
    token infix:sym<ff^> { <sym> <O(|%conditional_ff)> }
    token infix:sym<^ff^> { <sym> <O(|%conditional_ff)> }

    token infix:sym<fff> { <sym> <O(|%conditional_ff)> }
    token infix:sym<^fff> { <sym> <O(|%conditional_ff)> }
    token infix:sym<fff^> { <sym> <O(|%conditional_ff)> }
    token infix:sym<^fff^> { <sym> <O(|%conditional_ff)> }

    token infix:sym<=> {
        <sym>
        [
        || <?{ $*LEFTSIGIL eq '$' || $*IN_META }> <O(|%item_assignment)>
        || <O(|%list_assignment)>
        ]
        { $*LEFTSIGIL := '' }
    }

    token infix:sym<⚛=> { <sym> <O(|%item_assignment)> }
    token infix:sym<⚛+=> { <sym> <O(|%item_assignment)> }
    token infix:sym<⚛-=> { <sym> <O(|%item_assignment)> }
    token infix:sym<⚛−=> { <sym> <O(|%item_assignment)> }

    token infix:sym<and>  { <sym> >> <O(|%loose_and, :iffy(1))> }

    token infix:sym<or>   { <sym> >> <O(|%loose_or, :iffy(1), :assoc<left>)> }
    token infix:sym<xor>  { <sym> >> <O(|%loose_or, :iffy(1))> }

    token infix:sym<..>   { <sym> [<!{ $*IN_META }> <?[)\]]> <.panic: "Please use ..* for indefinite range">]? <O(|%structural)> }
    token infix:sym<^..>  { <sym> <O(|%structural)> }
    token infix:sym<..^>  { <sym> <O(|%structural)> }
    token infix:sym<^..^> { <sym> <O(|%structural)> }

    token infix:sym<leg>    { <sym> >> <O(|%structural)> }
    token infix:sym<cmp>    { <sym> >> <O(|%structural)> }
    token infix:sym<unicmp> { <sym> >> <O(|%structural)> }
    token infix:sym<coll>   { <sym> >> <O(|%structural)> }
    token infix:sym«<=>»    { <sym> <O(|%structural)> }

    token circumfix:sym<( )> {
        :dba('parenthesized expression')
        '(' ~ ')' <semilist>
    }

    token circumfix:sym<[ ]> {
        :dba('array composer')
        '[' ~ ']' <semilist>
    }

    token circumfix:sym<{ }> {
        <?[{]> <pblock>
        { $*BORG<block> := $<pblock> }
    }

    token circumfix:sym<ang> {
        :dba('quote words')
        '<' ~ '>'
        [
            [ <?before 'STDIN>' > <.obs('<STDIN>', '$*IN.lines (or add whitespace to suppress warning)')> ]?
            [ <?[>]> <.obs('<>', 'lines() to read input, (\'\') to represent a null string or () to represent an empty list')> ]?
            <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w', 'v']))>
        ]
    }

    token circumfix:sym«<< >>» {
        :dba('shell-quote words')
        '<<' ~ '>>' <nibble(self.quote_lang(self.slang_grammar('Quote'), "<<", ">>", ['qq', 'ww', 'v']))>
    }

    token circumfix:sym<« »> {
        :dba('shell-quote words')
        '«' ~ '»' <nibble(self.quote_lang(self.slang_grammar('Quote'), "«", "»", ['qq', 'ww', 'v']))>
    }

    ##
    ## Terms
    ##

    token termish {
        :my $*SCOPE := "";
        :my $*MULTINESS := "";
        :my $*OFTYPE;
        :my $*VAR;
        :dba('term')
        # TODO try to use $/ for lookback to check for erroneous
        #      use of pod6 trailing declarator block, e.g.:
        #
        #        #=!
        #
        #      instead of
        #
        #        #=(
        #
        [
        ||  [
            | <prefixish>+
              [ || <term>
                || {} <.panic("Prefix " ~ $<prefixish>[-1].Str
                                        ~ " requires an argument, but no valid term found"
                                        ~ ".\nDid you mean "
                                        ~ $<prefixish>[-1].Str
                                        ~ " to be an opening bracket for a declarator block?"
                      )>
              ]
            | <term>
            ]
        || <!{ $*QSIGIL }> <?before <infixish> {
            $/.typed_panic('X::Syntax::InfixInTermPosition', infix => ~$<infixish>); } >
        || <!>
        ]
        :dba('postfix')
        [
        || <?{ $*QSIGIL }>
            [
            || <?{ $*QSIGIL eq '$' }> [ <postfixish>+! <?{ bracket_ending($<postfixish>) }> ]**0..1
            ||                          <postfixish>+! <?{ bracket_ending($<postfixish>) }>
            ]
        || <!{ $*QSIGIL }> <postfixish>*
        ]
        {
            if $*VAR {
                self.check_variable($*VAR);
                $*VAR := 0;
            }
        }
    }

    sub bracket_ending($matches) {
        my $check     := $matches[+$matches - 1];
        my str $str   := $check.Str;
        my $last  := nqp::substr($str, nqp::chars($check) - 1, 1);
        $last eq ')' || $last eq '}' || $last eq ']' || $last eq '>' || $last eq '»'
    }

    token term:sym<self> {
        <sym> <.end_keyword>
    }

    token term:sym<now> { <sym> <.tok> }

    token term:sym<time> { <sym> <.tok> }

    token term:sym<empty_set> { "∅" <!before <.[ \( \\ ' \- ]> || \h* '=>'> }

    token term:sym<rand> {
        <!{ $*LANG.pragma('p5isms') }>
        <sym> »
        [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand for Num or (^N).pick for Int result')> ]?
        [ <?before '()'> <.obs('rand()', 'rand')> ]?
        <.end_keyword>
    }

    token term:sym<fatarrow> {
        <key=.identifier> \h* '=>' <.ws> <val=.EXPR('i<=')>
    }

    token term:sym<colonpair>          { <colonpair> }
    token term:sym<variable>           { <variable> { $*VAR := $<variable> unless $*VAR; } }
    token term:sym<package_declarator> { <package_declarator> }
    token term:sym<scope_declarator>   { <scope_declarator> }
    token term:sym<routine_declarator> { <routine_declarator> }
    token term:sym<multi_declarator>   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
    token term:sym<regex_declarator>   { <regex_declarator> }
    token term:sym<statement_prefix>   { <statement_prefix> }
    token term:sym<*>                  { <sym> }
    token term:sym<**>                 { <sym> }
    token term:sym<lambda>             { <?lambda> <pblock> {$*BORG<block> := $<pblock> } }
    token term:sym<value>              { <value> }

    token term:sym<::?IDENT> {
        $<sym> = [ '::?' <identifier> ] »
    }

    token term:sym<identifier> {
        <identifier>
        <!{ $*R.is-identifier-type(~$<identifier>) }>
        [ <?before <.unsp>? '('> | \\ <?before '('> ]
        <args(1)>
        {
            if !$<args><invocant> {
                if $*BORG<block> {
                    unless $*BORG<name> {
                        $*BORG<name> := ~$<identifier>;
                    }
                }
            }
        }
    }

    token term:sym<name> {
        :my $*is-type;
        <longname>
        [
        || <?{ $*R.is-name-known($<longname>.ast) }>
           { $*is-type := $*R.is-name-type($<longname>.ast) }
        || [ \\ <?before '('> ]? <args(1)>
           {
                if !$<args><invocant> {
                    my $name := ~$<longname>;
                    if $*BORG<block> {
                        unless $*BORG<name> {
                            $*BORG<name> := $*BORG<name> // $name;
                        }
                    }
                }
            }
        ]
    }

    token term:sym<dotty> { <dotty> }

    token term:sym<capture> {
        '\\'
        [
        | '(' <args=.semiarglist> ')'
        | <?before '$' | '@' | '%' | '&'> <.typed_worry('X::Worry::P5::Reference')> <args=.termish>
        | <?before \d> <.typed_worry('X::Worry::P5::BackReference')> <args=.termish>
        | <?before \S> <args=.termish>
        | {} <.panic: "You can't backslash that">
        ]
    }

    token colonpair {
        :my $*key;
        ':'
        :dba('colon pair')
        [
        | $<neg>='!' [ <identifier> || <.panic: "Malformed False pair; expected identifier"> ]
            [ <[ \[ \( \< \{ ]> {
            $/.typed_panic('X::Syntax::NegatedPair', key => ~$<identifier>) } ]?
            { $*key := $<identifier>.Str }
        | $<num> = [\d+] <identifier> [ <?before <.[ \[ \( \< \{ ]>> {} <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)> <.circumfix> ]?
            <?{ self.no-synthetics(~$<num>) }>
            { $*key := $<identifier>.Str }
        | <identifier>
            { $*key := $<identifier>.Str }
            [ <.unsp>? :dba('pair value') <coloncircumfix($*key)> ]?
        | <coloncircumfix('')>
            { $*key := ""; }
        | <var=.colonpair_variable>
            { $*key := $<var><desigilname>.Str; self.check_variable($<var>); }
        ]
    }

    method no-synthetics($num) {
        # Here we go over each character in the numeral and check $ch.chr eq $ch.ord.chr
        # to fail any matches that have synthetics, such as 7\x[308]
        my $chars-num := nqp::chars($num);
        my $pos       := -1;
        nqp::while(
            nqp::islt_i( ($pos := nqp::add_i($pos, 1)), $chars-num )
            && nqp::eqat(
                $num,
                nqp::chr( nqp::ord($num, $pos) ),
                $pos,
            ),
            nqp::null,
        );
        nqp::iseq_i($chars-num, $pos);
    }

    token coloncircumfix($front) {
        # reset $*IN_DECL in case this colonpair is part of var we're
        # declaring, since colonpair might have other vars. Don't make those
        # think we're declaring them
        :my $*IN_DECL := '';
        [
        | '<>' <.worry("Pair with <> really means an empty list, not null string; use :$front" ~ "('') to represent the null string,\n  or :$front" ~ "() to represent the empty list more accurately")>
        | {} <circumfix>
        ]
    }

    token colonpair_variable {
        <sigil> {}
        [
        | <twigil>? <desigilname>
        | $<capvar>='<' <desigilname> '>'
        ]
    }

    token variable {
        :my $*IN_META := '';
        [
        | :dba('infix noun') '&[' ~ ']' <infixish('[]')>
        | <sigil> <twigil>? <desigilname>
        | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]
        | <sigil> $<index>=[\d+]
        | <sigil> <?[<]> <postcircumfix>
        | <?before <.sigil> <.?[ ( [ { ]>> <!RESTRICTED> <?{ !$*IN_DECL }> <contextualizer>
        | {} <sigil> <!{ $*QSIGIL }> <?MARKER('baresigil')>   # try last, to allow sublanguages to redefine sigils (like & in regex)
        ]
        { $*LEFTSIGIL := nqp::substr(self.orig(), self.from, 1) unless $*LEFTSIGIL }
    }

    token contextualizer {
        :dba('contextualizer')
        [ <?{ $*IN_DECL }> <.panic: "Cannot declare a contextualizer"> ]?
        [
        | <sigil> '(' ~ ')'    <coercee=sequence>
        | <sigil> <?[ \[ \{ ]> <coercee=circumfix>
        ]
    }

    regex term:sym<reduce> {
        :my $*IN_REDUCE := 1;
        :my $op;
        <?before '['\S+']'>
        <!before '['+ <.[ - + ? ~ ^ ]> <.[ \w $ @ ]> >  # disallow accidental prefix before termish thing

        '['
        [
        || <op=.infixish('red')> <?[\]]>
        || $<triangle>=[\\]<op=.infixish('tri')> <?[\]]>
        || <!>
        ]
        ']'
        { $op := $<op>; }

        <.can_meta($op, "reduce with")>

        [
        || <!{ $op<OPER><O>.made<diffy> }>
        || <?{ $op<OPER><O>.made<assoc> eq 'chain' }>
        || { self.typed_panic: "X::Syntax::CannotMeta", meta => "reduce with", operator => ~$op<OPER><sym>, dba => ~$op<OPER><O>.made<dba>, reason => 'diffy and not chaining' }
        ]

        { $*IN_REDUCE := 0 }
        <args>
    }

    ##
    ## Declarations
    ##

    proto token package_declarator { <...> }
    token package_declarator:sym<package> {
        <sym><.kok> <package_def('package')>
    }
    token package_declarator:sym<module> {
        <sym><.kok> <package_def('module')>
    }
    token package_declarator:sym<class> {
        <sym><.kok> <package_def('class')>
    }
    token package_declarator:sym<grammar> {
        <sym><.kok> <package_def('grammar')>
    }
    token package_declarator:sym<role> {
        <sym><.kok> <package_def('role')>
    }
    token package_declarator:sym<knowhow> {
        <sym><.kok> <package_def('knowhow')>
    }
    token package_declarator:sym<native> {
        <sym><.kok> <package_def('native')>
    }

    rule package_def($*PKGDECL) {
        :my $*BORG := {};
        :my $*BLOCK;
        :my $*PACKAGE;
        <!!{ $/.clone_braid_from(self) }>
        <longname>? {}
        [ :dba('generic role')
            <?{ ($*PKGDECL // '') eq 'role' }>
            '[' ~ ']' <signature>
            { $*IN_DECL := ''; }
        ]?
        <.stub-package($<longname>)>
       { $/.set_package($*PACKAGE) }
        <trait($*PACKAGE)>*
        <.enter-package-scope>
        [
        || <?[{]> <block>
        || ';'
            {
                unless $*SCOPE eq 'unit' {
                    $/.panic("Semicolon form of '$*PKGDECL' without 'unit' is illegal. You probably want to use 'unit $*PKGDECL'");
                }
            }
            { $*IN_DECL := ''; }
            <statementlist>
        || <.panic("Unable to parse $*PKGDECL definition")>
        ]
        <.leave-package-scope>
    }

    token stub-package($*PACKAGE-NAME) { <?> }
    token enter-package-scope() { <?> }
    token leave-package-scope { <?> }

    proto token scope_declarator { <...> }
    token scope_declarator:sym<my>    { <sym> <scoped('my')> }
    token scope_declarator:sym<our>   { <sym> <scoped('our')> }
    token scope_declarator:sym<has>   { <sym> <scoped('has')> }
    token scope_declarator:sym<HAS>   { <sym> <scoped('HAS')> }
    token scope_declarator:sym<anon>  { <sym> <scoped('anon')> }
    token scope_declarator:sym<state> { <sym> <scoped('state')> }
    token scope_declarator:sym<unit>  { <sym> <scoped('unit')> }

    token scoped($*SCOPE) {
        <.end_keyword>
        :dba('scoped declarator')
        [
        || <.ws>
           [
           | <DECL=declarator>
           | <DECL=regex_declarator>
           | <DECL=package_declarator>
           | [<typename><.ws>]+
             {
                if nqp::elems($<typename>) > 1 {
                    $/.NYI('Multiple prefix constraints');
                }
                $*OFTYPE := $<typename>[0];
             }
             <DECL=multi_declarator>
           | <DECL=multi_declarator>
           ]
        || <.malformed($*SCOPE)>
        ]
    }

    proto token multi_declarator { <...> }
    token multi_declarator:sym<multi> {
        <sym><.kok>
        :my $*MULTINESS := 'multi';
        [ <?before '('> <.typed_panic: "X::Anon::Multi", multiness => $*MULTINESS> ]?
        [ <declarator> || <routine_def('sub')> || <.malformed('multi')> ]
    }
    token multi_declarator:sym<proto> {
        <sym><.kok>
        :my $*MULTINESS := 'proto';
        [ <?before '('> <.typed_panic: "X::Anon::Multi", multiness => $*MULTINESS> ]?
        [ <declarator> || <routine_def('sub')> || <.malformed('proto')> ]
    }
    token multi_declarator:sym<null> {
        :my $*MULTINESS := '';
        <declarator>
    }

    token declarator {
        :my $*LEFTSIGIL := '';
        [
        | '\\' <defterm>
            [ <.ws> <term_init=initializer> || <.typed_panic: "X::Syntax::Term::MissingInitializer"> ]
        | <variable_declarator>
        | '(' ~ ')' <signature> [ <.ws> <trait>+ ]? [ <.ws> <initializer> ]?
        | <routine_declarator>
        ]
    }

    token variable_declarator {
        :my $*IN_DECL := 'variable';
        [
        | <sigil> <twigil>? <desigilname>?
        | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]
        # TODO error cases for when you declare something you're not allowed to
        ]
        {
            $*IN_DECL := '';
            $*LEFTSIGIL := nqp::substr(self.orig(), self.from, 1) unless $*LEFTSIGIL;
        }
        [ <.ws> <trait>+ ]?
        [<.ws> <initializer>]?
    }

    token desigilname {
        [
        | <?before <.sigil> <.sigil> > <variable>
        | <?sigil>
          [ <?{ $*IN_DECL }> <.typed_panic: 'X::Syntax::Variable::IndirectDeclaration'> ]?
          <variable> { $*VAR := $<variable> }
        | <longname>
        ]
    }

    proto token initializer { <...> }
    token initializer:sym<=> {
        <sym>
        [
            <.ws>
            [
            || <?{ $*LEFTSIGIL eq '$' }> <EXPR('i<=')>
            || <EXPR('e=')>
            ]
            || <.malformed: 'initializer'>
        ]
    }
    token initializer:sym<:=> {
        <sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
    }
    token initializer:sym<::=> {
        <sym> [ <.ws> <EXPR('e=')> <.NYI('"::="')> || <.malformed: 'binding'> ]
    }
#    token initializer:sym<.=> {
#        <sym> [ <.ws> <dottyopish> || <.malformed: 'mutator method call'> ]
#    }

    proto token routine_declarator { <...> }
    token routine_declarator:sym<sub> {
        <sym> <.end_keyword> <routine_def('sub')>
    }
    token routine_declarator:sym<method> {
        <sym> <.end_keyword> <method_def('method')>
    }
    token routine_declarator:sym<submethod> {
        <sym> <.end_keyword> <method_def('submethod')>
    }

    rule routine_def($declarator) {
        :my $*BORG := {};
        :my $*IN_DECL := $declarator;
        :my $*BLOCK;
        <.enter-block-scope(nqp::tclc($declarator))>
        <deflongname('my')>?
        [ '(' <signature> ')' ]?
        <trait($*BLOCK)>* :!s
        { $*IN_DECL := ''; }
        [
        || <onlystar>
        || <blockoid>
        ]
        <.leave-block-scope>
    }

    rule method_def($declarator) {
        :my $*BORG := {};
        :my $*IN_DECL := $declarator;
        :my $*BLOCK;
        <.enter-block-scope(nqp::tclc($declarator))>
        $<specials>=[<[ ! ^ ]>?]<deflongname('has')>?
        [ '(' <signature> ')' ]?
        <trait($*BLOCK)>* :!s
        { $*IN_DECL := ''; }
        [
        || <onlystar>
        || <blockoid>
        ]
        <.leave-block-scope>
    }

    token onlystar {
        <?{ $*MULTINESS eq 'proto' }>
        '{' <.ws> '*' <.ws> '}'
        <?ENDSTMT>
    }

    proto token regex_declarator { <...> }

    token regex_declarator:sym<rule> {
        <sym><.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN_DECL := 'rule';
        {
            %*RX<s> := 1;
            %*RX<r> := 1;
        }
        <regex_def>
    }

    token regex_declarator:sym<token> {
        <sym><.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN_DECL := 'token';
        {
            %*RX<r> := 1;
        }
        <regex_def>
    }

    token regex_declarator:sym<regex> {
        <sym><.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN_DECL := 'regex';
        <regex_def>
    }

    rule regex_def {
        :my $*BLOCK;
        <.enter-block-scope('RegexDeclaration')>
        [
          <deflongname('has')>?
          { if $<longname> { %*RX<name> := ~$<deflongname>.ast } }
          { $*IN_DECL := '' }
          [ '(' <signature> ')' ]?
          <trait($*BLOCK)>*
          '{'
          [
#          | ['*'|'<...>'|'<*>'] <?{ $*MULTINESS eq 'proto' }> $<onlystar>={1}
          | <nibble(self.quote_lang(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'), '{', '}'))>
          ]
          '}'<!RESTRICTED><?ENDSTMT>
          <.leave-block-scope>
        ] || <.malformed('regex')>
    }

    rule trait($*TARGET?) {
        :my $*IN_DECL := '';
        <trait_mod>
    }

    proto rule trait_mod { <...> }
    rule trait_mod:sym<is> {
        <sym> [ <longname><circumfix>? || <.panic: 'Invalid name'> ]
        {
            if $<circumfix> && nqp::eqat(self.orig, '{', $<longname>.to) {
                $*BORG<block> := $<circumfix>;
                $*BORG<name> := 'is ' ~ $<longname>;
            }
        }
    }
    rule trait_mod:sym<hides>   { <sym> [ <typename> || <.bad_trait_typename>] }
    rule trait_mod:sym<does>    { <sym> [ <typename> || <.bad_trait_typename>] }
    rule trait_mod:sym<of>      { <sym> [ <typename> || <.bad_trait_typename>] }
    rule trait_mod:sym<returns> { <sym> [ <typename> || <.bad_trait_typename>]
                                  || 'return' <.panic: 'Invalid trait modifier (did you mean \'returns\'?)'> }

    token bad_trait_typename {
        || <longname> {
                my $name := $*W.dissect_longname($<longname>);
                $*W.throw($/, ['X', 'InvalidType'],
                    :typename($name.name),
                    :suggestions($*W.suggest_typename($name.name)));
            }
        || <.malformed: 'trait'>
    }

    ##
    ## Values
    ##

    proto token value { <...> }
    token value:sym<quote>  { <quote> }
    token value:sym<number> { <number> }
    token value:sym<version> { <version> }

    proto token number { <...> }
    token number:sym<numish>   { <numish> }

    token numish {
        [
        | 'NaN' >>
        | <integer>
        | <dec_number>
        | <rad_number>
        | <rat_number>
#        | <complex_number>
        | 'Inf' >>
        | $<uinf>='∞'
        | <unum=:No+:Nl>
        ]
    }

    token integer {
        [
        | 0 [ b '_'? <VALUE=binint>
            | o '_'? <VALUE=octint>
            | x '_'? <VALUE=hexint>
            | d '_'? <VALUE=decint>
            | <VALUE=decint>
                <!!{ $/.typed_worry('X::Worry::P5::LeadingZero', value => ~$<VALUE>) }>
            ]
        | <VALUE=decint>
        ]
#        <!!before ['.' <?before \s | ',' | '=' | ':' <!before  <coloncircumfix <OPER=prefix> > > | <.terminator> | $ > <.typed_sorry: 'X::Syntax::Number::IllegalDecimal'>]? >
        [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
    }

    token signed-integer { <sign> <integer> }

    token dec_number {
        :dba('decimal number')
        [
        | $<coeff> = [               '.' <frac=.decint> ] <escale>?
        | $<coeff> = [ <int=.decint> '.' <frac=.decint> ] <escale>?
        | $<coeff> = [ <int=.decint>                    ] <escale>
        ]
    }

    token escale { <[Ee]> <sign> <decint> }

    token sign { '+' | '-' | '−' | '' }

    token rad_number {
        ':' $<radix> = [\d+] <.unsp>?
        :my $r := nqp::radix(10, $<radix>, 0, 0)[0];
        {}           # don't recurse in lexer
        :dba('number in radix notation')
        :my $rad_digit  := token rad_digit  { \d | <[ a..z A..Z ａ..ｚ Ａ..Ｚ ]> };
        :my $rad_digits := token rad_digits { <rad_digit>+ [ _ <rad_digit>+ ]* };
        [
        || '<'
                $<ohradix>  = [ '0x' <?{ $r < 34 }> | '0o' <?{ $r < 25 }> | '0d' <?{ $r < 14 }> | '0b' <?{ $r < 12 }> ]**0..1
                $<intpart>  = <rad_digits>
                $<fracpart> = [ '.' <rad_digits> ]**0..1
                [ '*' <base=.integer> '**' <exp=.integer> ]**0..1
           '>'
        || <?[[]> <bracket=circumfix>
        || <?[(]> <circumfix>
        || <.malformed: 'radix number'>
        ]
    }

    token rat_number { '<' <bare_rat_number> '>' }
    token bare_rat_number {
        <?before <.[-−+0..9<>:boxd]>+? '/'>
        <nu=.signed-integer> '/' <de=integer>
    }

    token version {
        <?before v\d+\w*> 'v' $<vstr>=[<vnum>+ % '.' '+'?]
        <!before '-'|\'> # cheat because of LTM fail
    }

    token vnum {
        \w+ | '*'
    }

    proto token quote { <...> }
    token quote:sym<apos>  { :dba('single quotes') "'" ~ "'" <nibble(self.quote_lang(self.slang_grammar('Quote'), "'", "'", ['q']))> }
    token quote:sym<sapos> { :dba('curly single quotes') "‘" ~ "’" <nibble(self.quote_lang(self.slang_grammar('Quote'), "‘", "’", ['q']))> }
    token quote:sym<lapos> { :dba('low curly single quotes') "‚" ~ <[’‘]> <nibble(self.quote_lang(self.slang_grammar('Quote'), "‚", ["’","‘"], ['q']))> }
    token quote:sym<hapos> { :dba('high curly single quotes') "’" ~ <[’‘]> <nibble(self.quote_lang(self.slang_grammar('Quote'), "’", ["’","‘"], ['q']))> }
    token quote:sym<dblq>  { :dba('double quotes') '"' ~ '"' <nibble(self.quote_lang(self.slang_grammar('Quote'), '"', '"', ['qq']))> }
    token quote:sym<sdblq> { :dba('curly double quotes') '“' ~ '”' <nibble(self.quote_lang(self.slang_grammar('Quote'), '“', '”', ['qq']))> }
    token quote:sym<ldblq> { :dba('low curly double quotes') '„' ~ <[”“]> <nibble(self.quote_lang(self.slang_grammar('Quote'), '„', ['”','“'], ['qq']))> }
    token quote:sym<hdblq> { :dba('high curly double quotes') '”' ~ <[”“]> <nibble(self.quote_lang(self.slang_grammar('Quote'), '”', ['”','“'], ['qq']))> }
    token quote:sym<crnr>  { :dba('corner quotes') '｢' ~ '｣' <nibble(self.quote_lang(self.slang_grammar('Quote'), '｢', '｣'))> }
    token quote:sym<q> {
        :my $qm;
        'q'
        [
        | <quote_mod> {} <.qok($/)> { $qm := $<quote_mod>.Str } <quibble(self.slang_grammar('Quote'), 'q', $qm)>
        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'), 'q')>
        ]
    }
    token quote:sym<qq> {
        :my $qm;
        'qq'
        [
        | <quote_mod> { $qm := $<quote_mod>.Str } <.qok($/)> <quibble(self.slang_grammar('Quote'), 'qq', $qm)>
        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'), 'qq')>
        ]
    }
    token quote:sym<Q> {
        :my $qm;
        'Q'
        [
        | <quote_mod> { $qm := $<quote_mod>.Str } <.qok($/)> <quibble(self.slang_grammar('Quote'), $qm)>
        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'))>
        ]
    }

    proto token quote_mod   {*}
    token quote_mod:sym<w>  { <sym> }
    token quote_mod:sym<ww> { <sym> }
    token quote_mod:sym<x>  { <sym> }
    token quote_mod:sym<to> { <sym> }
    token quote_mod:sym<s>  { <sym> }
    token quote_mod:sym<a>  { <sym> }
    token quote_mod:sym<h>  { <sym> }
    token quote_mod:sym<f>  { <sym> }
    token quote_mod:sym<c>  { <sym> }
    token quote_mod:sym<b>  { <sym> }

    token qok($x) {
        » <![(]>
#        [ <?[:]> || <!{ my str $n := ~$x; $*W.is_name([$n]) || $*W.is_name(['&' ~ $n]) }> ]
        [ \s* '#' <.panic: "# not allowed as delimiter"> ]?
        <.ws>
    }

    token quote:sym</null/> { '/' \s* '/' <.typed_panic: "X::Syntax::Regex::NullRegex"> }
    token quote:sym</ /> {
        :my %*RX;
        :my $*INTERPOLATE := 1;
        '/'
        <nibble(self.quote_lang(self.slang_grammar('Regex'), '/', '/'))>
        [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
        <.old_rx_mods>?
    }
    token quote:sym<rx>   {
        <sym>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        {} <.qok($/)>
        <rx_adverbs>
        <quibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'))>
        <!old_rx_mods>
    }
    token quote:sym<m> {
        <sym> (s)**0..1
        :my %*RX;
        :my $*INTERPOLATE := 1;
        { %*RX<s> := 1 if $/[0] }
        <.qok($/)>
        <rx_adverbs>
        <quibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'))>
        <!old_rx_mods>
    }

    token quote:sym<s> {
        <sym=[Ss]> (s)**0..1
        :my %*RX;
        :my $*INTERPOLATE := 1;
        {
            %*RX<s> := 1 if $/[0]
        }
        <.qok($/)>
        <rx_adverbs>
        <sibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'), self.slang_grammar('Quote'), ['qq'])>
        [ <?{ $<sibble><infixish> }> || <.old_rx_mods>? ]
    }

    token sibble($l, $lang2, @lang2tweaks?) {
        :my $lang;
        :my $start;
        :my $stop;

        <babble($l)>
        { my $B := $<babble><B>.ast; $lang := $B[0]; $start := $B[1]; $stop := $B[2]; }

        $start <left=.nibble($lang)> [ $stop || { self.fail-terminator($/, $start, $stop) } ]
        [ <?{ $start ne $stop }>
            <.ws>
            [ <?[ \[ \{ \( \< ]> <.obs('brackets around replacement', 'assignment syntax')> ]?
            [ <infixish> || <.missing: "assignment operator"> ]
            [ <?{ $<infixish>.Str eq '=' || $<infixish><infix_postfix_meta_operator> }> || <.malformed: "assignment operator"> ]
            <.ws>
            [ <right=.EXPR('i')> || <.panic: "Assignment operator missing its expression"> ]
        ||
            { $lang := self.quote_lang($lang2, $stop, $stop, @lang2tweaks); }
            <right=.nibble($lang)> $stop || <.panic("Malformed replacement part; couldn't find final $stop")>
        ]
    }

    token old_rx_mods {
        (<[ i g s m x c e ]>)
        {
            my $m := $/[0].Str;
            if    $m eq 'i' { $/.obs('/i',':i');                                   }
            elsif $m eq 'g' { $/.obs('/g',':g');                                   }
            elsif $m eq 'm' { $/.obs('/m','^^ and $$ anchors');                    }
            elsif $m eq 's' { $/.obs('/s','. or \N');                              }
            elsif $m eq 'x' { $/.obs('/x','normal default whitespace');            }
            elsif $m eq 'c' { $/.obs('/c',':c or :p');                             }
            elsif $m eq 'e' { $/.obs('/e','interpolated {...} or s{} = ... form'); }
            else            { $/.obs('suffix regex modifiers','prefix adverbs');   }
        }
    }

    token quote:sym<qr> {
        <sym> {} <.qok($/)> <.obs('qr for regex quoting', 'rx//')>
    }

    token rx_adverbs() {
        [ <quotepair> <.ws> ]*
    }

    token quotepair {
        :my $*key;
        ':'
        :dba('colon pair (restricted)')
        [
        | $<neg>='!' [ <identifier> || <.panic: "Malformed False pair; expected identifier"> ]
            [ <[ \[ \( \< \{ ]> {
            $/.typed_panic('X::Syntax::NegatedPair', key => ~$<identifier>) } ]?
            { $*key := $<identifier>.Str }
        | $<num> = [\d+] <identifier> [ <?before <.[ \[ \( \< \{ ]>> {} <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)> <.circumfix> ]?
            <?{ self.no-synthetics(~$<num>) }>
            { $*key := $<identifier>.Str }
        | <identifier>
            { $*key := ~$<identifier> }
            [ <?[(]> <circumfix> ]?
        ]
    }

    ##
    ## Types
    ##

    token typename {
        [
        | # parse ::?CLASS as special case
          '::?'<identifier> <colonpair>*
        | <longname>
          <?{
            # ::T introduces a type, so always is one
            nqp::eqat(~$<longname>, '::', 0) || $*R.is-name-known($<longname>.ast)
          }>
        ]
        # parametric/coercion type?
        <.unsp>? [
            <?[[]>
            '[' ~ ']' <arglist>
        ]?
        <.unsp>? [ <?before '{'> {
            $/.typed_panic('X::NYI', feature => 'Autovivifying object closures');
        } <whence=.postcircumfix> ]?
        <.unsp>? [ <?[(]> '(' ~ ')' [<.ws> [<accept=.typename> || $<accept_any>=<?>] <.ws>] ]?
        [<.ws> 'of' <.ws> <typename> ]?
    }

    ##
    ## Signatures
    ##

    token signature {
        :my $*zone := 'posreq';
        :my $*multi_invocant := 1;
        :my @*seps := nqp::list();
        <.ws>
        [
        | <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' >
        | <parameter>
        ]+ % <param_sep>
        <.ws>
        [ <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' > || <.malformed('parameter')> ]
        { $*IN_DECL := ''; }
        [ '-->' <.ws> [ || [<typename>|<value>||<typo_typename(1)>] <.ws>
                           [ || <?[ { ) ]>
                             || <?before <.param_sep>? <.parameter>>
                                <.malformed('return value (return constraints only allowed at the end of the signature)')>
                           ]
                        || <.malformed('return value')>
                      ] ]?
        { $*LEFTSIGIL := '@'; }
    }

    rule param_sep {
        '' $<sep>=[','|':'|';;'|';'] {
            if $<sep> eq ';;' {
                $/.panic("Can only specify ';;' once in a signature")
                  if $*multi_invocant == 0;
                $*multi_invocant := 0;
            }
            @*seps.push($<sep>);
        }
    }

    token parameter {
        [
        | <type_constraint>
          [
          | $<quant>=['**'|'*'|'+'] <param_var>
          | $<quant>=['\\'|'|'] <param_var> {
                $/.panic('Obsolete use of | or \\ with sigil on param ' ~ $<param_var>);
            }
          | $<quant>=['\\'|'|'|'+'] <param_term>
          | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
          | <?>
          ]
        | $<quant>=['**'|'*'|'+'] <param_var>
        | $<quant>=['\\'|'|'] <param_var> {
              $/.panic('Obsolete use of | or \\ with sigil on param ' ~ $<param_var>);
          }
        | $<quant>=['\\'|'|'|'+'] <param_term>
        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
        ]
        <.ws>
        <trait>*
        <post_constraint>*
        [
            <default_value>
            [ <modifier=.trait> {
                self.typed_panic: "X::Parameter::AfterDefault", type => "trait", modifier => $<modifier>, default => $<default_value>
            }]?
            [ <modifier=.post_constraint> {
                self.typed_panic: "X::Parameter::AfterDefault", type => "post constraint", modifier => $<modifier>, default => $<default_value>
            }]?
        ]?
    }

    rule post_constraint {
        :my $*IN_DECL := '';
        :dba('constraint')
        [
        | '[' ~ ']' <signature>
        | '(' ~ ')' <signature>
        | where <EXPR('i=')>
        ]
    }

    token param_var {
        :dba('formal parameter')
        [
        | '[' ~ ']' <signature>
        | '(' ~ ')' <signature>
        | $<declname>=[
            <sigil>
#            <twigil>?
            [
#            || <?{ $<sigil>.Str eq '&' }>
#               [<?identifier> {} <name=.sublongname> | <sigterm>]
            || <name=.identifier>
            || <name=.decint> { $*W.throw($/, 'X::Syntax::Variable::Numeric', what => 'parameter') }
            || $<name>=[<[/!]>]
            ]?
          ]

          :dba('shape declaration')
          :my $*IN_DECL := '';
          [
#          | <?before ':('>  ':'  # XXX allow fakesig parsed as subsig for the moment
          | <?before '('>         <.sorry: "Shape declaration with () is reserved;\n  please use whitespace if you meant a subsignature for unpacking,\n  or use the :() form if you meant to add signature info to the function's type">
#          | <?before '['> <arrayshape=.postcircumfix>
          | <?before <.[ { < « ]>> <.sorry: 'Shape declaration is not yet implemented; please use whitespace if you meant something else'>
               <postcircumfix>
          ]?
        ]
    }

    token param_term {
        <defterm>?
    }

    token named_param {
        :my $*GOAL := ')';
        :dba('named parameter')
        ':'
        [
        | <name=.identifier> '('
            <.ws> [ <named_param> | <param_var> ] <.ws>
            [ ')' || <.panic: 'Unable to parse named parameter; couldn\'t find right parenthesis'> ]
        | <param_var>
        ]
    }

    rule default_value {
        :my $*IN_DECL := '';
        '=' <EXPR('i=')>
    }

    token type_constraint {
        :my $*IN_DECL := '';
        [
#        | <value>
#        | [ <[-−]> :my $*NEGATE_VALUE := 1; | '+' ] $<value>=<numish>
        | <typename>
#        | where <.ws> <EXPR('i=')>
        ]
        <.ws>
    }

    ##
    ## Argument lists and captures
    ##

    token args($*INVOCANT_OK = 0) {
        :my $*INVOCANT;
        :my $*GOAL := '';
        :dba('argument list')
        [
        | '(' ~ ')' <semiarglist>
        | <.unsp> '(' ~ ')' <semiarglist>
        | [ \s <arglist> ]
        | <?>
        ]
    }

    token semiarglist {
        <arglist>+ % ';'
        <.ws>
    }

    token arglist {
        :my $*GOAL := 'endargs';
        :my $*QSIGIL := '';
        <.ws>
        :dba('argument list')
        [
        | <?stdstopper>
        | <EXPR('e=')>
        | <?>
        ]
    }

    ##
    ## Lexer stuff
    ##

    token apostrophe {
        <[ ' \- ]>
    }

    token identifier {
        <.ident> [ <.apostrophe> <.ident> ]*
    }

    token name {
        [
        | <identifier> <morename>*
        | <morename>+
        ]
    }

    token morename {
        :my $*QSIGIL := '';
        '::'
        [
        ||  <?before '(' | <.alpha> >
            [
            | <identifier>
            | :dba('indirect name') '(' ~ ')' [ <.ws> <EXPR> ]
            ]
        || <?before '::'> <.typed_panic: "X::Syntax::Name::Null">
        || $<bad>=[<.sigil><.identifier>] { my str $b := $<bad>; self.malformed("lookup of ::$b; please use ::('$b'), ::\{'$b'\}, or ::<$b>") }
        ]?
    }

    token longname {
        <name> {} [ <?before ':' <.+alpha+[\< \[ \« ]>> <!RESTRICTED> <colonpair> ]*
    }

    token deflongname($*DEFAULT-SCOPE) {
        :dba('new name to be defined')
        <name>
        <colonpair>*
    }

    token defterm {
        :dba('new term to be defined')
        <identifier>
        # TODO colonpairs
    }

    token sigil { <[$@%&]> }

    proto token twigil { <...> }
    token twigil:sym<.> { <sym> <?before \w> }
    token twigil:sym<!> { <sym> <?before \w> }
    token twigil:sym<^> { <sym> <?before \w> }
    token twigil:sym<:> { <sym> <?before \w> }
    token twigil:sym<*> { <sym> <?before \w> }
    token twigil:sym<?> { <sym> <?before \w> }
    token twigil:sym<=> { <sym> <?before \w> }
    token twigil:sym<~> { <sym> <?before \w> }

    token lambda { '->' | '<->' }

    token end_keyword {
        » <!before <.[ \( \\ ' \- ]> || \h* '=>'>
    }

    token end_prefix {
        <.end_keyword> \s*
    }

    token spacey { <?[\s#]> }

    token kok {
        <.end_keyword>
        [
        || <?before <.[ \s \# ]> > <.ws>
        || <?{
                my $n := nqp::substr(self.orig, self.from, self.pos - self.from);
                $*R.is-identifier-known($n) || $*R.is-identifier-known('&' ~ $n)
                    ?? False
                    !! self.panic("Whitespace required after keyword '$n'")
           }>
        ]
    }

    token tok {
        <.end_keyword>
        <!{
            my $n := nqp::substr(self.orig, self.from, self.pos - self.from);
            $*R.is-identifier-known($n) || $*R.is-identifier-known('&' ~ $n)
        }>
    }

    token ENDSTMT {
        [
        | \h* $$ <.ws> <?MARKER('endstmt')>
        | <.unv>? $$ <.ws> <?MARKER('endstmt')>
        ]?
    }

    proto token terminator { <...> }
    token terminator:sym<;> { <?[;]> }
    token terminator:sym<)> { <?[)]> }
    token terminator:sym<]> { <?[\]]> }
    token terminator:sym<}> { <?[}]> }
    token terminator:sym<ang> { <?[>]> <?{ $*IN_REGEX_ASSERTION }> }
    token terminator:sym<if>     { 'if'     <.kok> }
    token terminator:sym<unless> { 'unless' <.kok> }
    token terminator:sym<while>  { 'while'  <.kok> }
    token terminator:sym<until>  { 'until'  <.kok> }
    token terminator:sym<for>    { 'for'    <.kok> }
    token terminator:sym<given>  { 'given'  <.kok> }
    token terminator:sym<when>   { 'when'   <.kok> }
    token terminator:sym<with>   { 'with'   <.kok> }
    token terminator:sym<without> { 'without' <.kok> }
    token terminator:sym<arrow>  { '-->' }

    token stdstopper {
        [
        || <?MARKED('endstmt')> <?>
        || [
           | <?terminator>
           | $
           ]
       ]
    }

    # ws is highly performance sensitive. So, we check if we already marked it
    # at this point with a simple method, and only if that is not the case do
    # we bother doing any pattern matching.
    method ws() {
        self.MARKED('ws') ?? self !! self._ws()
    }
    token _ws {
        :my $old_highexpect := self.'!fresh_highexpect'();
        :dba('whitespace')
        <!ww>
        [
        | [\r\n || \v] <.heredoc>
        | <.unv>
        | <.unsp>
        ]*
        <?MARKER('ws')>
        :my $stub := self.'!fresh_highexpect'();
    }

    token unsp {
        \\ <?before \s | '#'>
        :dba('unspace')
        [
        | <.vws>
        | <.unv>
        | <.unsp>
        ]*
    }

    token vws {
        :dba('vertical whitespace')
        [
            [
            | \v
            | '<<<<<<<' {} <?before [.*? \v '=======']: .*? \v '>>>>>>>' > <.sorry: 'Found a version control conflict marker'> \V* \v
            | '=======' {} .*? \v '>>>>>>>' \V* \v   # ignore second half
            ]
        ]+
    }

    token unv {
        :dba('horizontal whitespace')
        [
        | \h+
        | \h* <.comment>
        | <?before \h* '=' [ \w | '\\'] > ^^ <.pod_content_toplevel>
        ]
    }

    proto token comment { <...> }

    token comment:sym<#> {
       '#' {} \N*
    }

    token comment:sym<#`(...)> {
        '#`' <?opener>
        <.quibble(self.slang_grammar('Quote'))>
    }
    token comment:sym<#`> {
        '#`' <!after \s> <!opener>
        <.typed_panic: 'X::Syntax::Comment::Embedded'>
    }

    ##
    ## Pod
    ##

    token pod_content_toplevel {
        <pod_block>
    }

    proto token pod_content { <...> }

    token pod_content:sym<block> {
        <pod_newline>*
        <pod_block>
        <pod_newline>*
    }

    # any number of paragraphs of text
    # the paragraphs are separated by one
    #   or more pod_newlines
    # each paragraph originally could have
    #   consisted of more than one line of
    #   text that were subsequently squeezed
    #   into one line
    token pod_content:sym<text> {
        <pod_newline>*

        # TODO get first line if IN-DEFN-BLOCK
        <pod_textcontent>+ % <pod_newline>+

        <pod_newline>*
    }

    # not a block, just a directive
    token pod_content:sym<config> {
        <pod_newline>*
        ^^ \h* '=config' \h+ $<type>=\S+ <pod_configuration>
        <pod_newline>+
    }

    proto token pod_textcontent { <...> }

    # for non-code (i.e., regular) text
    token pod_textcontent:sym<regular> {
        $<spaces>=[ \h* ]
         <?{ $*POD_IN_CODE_BLOCK
             || !$*ALLOW_INLINE_CODE
             || ($<spaces>.to - $<spaces>.from) <= $*VMARGIN }>

        $<text> = [
            \h* <!before '=' \w> <pod_string> [ <pod_newline> | $ ]
        ] +
    }

    token pod_textcontent:sym<code> {
        $<spaces>=[ \h* ]
        <?{ !$*POD_IN_CODE_BLOCK
            && $*ALLOW_INLINE_CODE
            && ($<spaces>.to - $<spaces>.from) > $*VMARGIN }>

        # TODO get first line if IN-DEFN-BLOCK
        $<text> = [
            [<!before '=' \w> \N+]+ % [<pod_newline>+ $<spaces>]
        ]
    }

    token pod_formatting_code {
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        :my $*POD_IN_FORMATTINGCODE := nqp::getlexdyn('$*POD_IN_FORMATTINGCODE');
        :my $*POD_ANGLE_COUNT := nqp::getlexdyn('$*POD_ANGLE_COUNT');
        <?{ $*POD_ALLOW_FCODES }>

        :my $endtag;
        <code=[A..Z]>
        $<begin-tag>=['<'+ <![<]> | '«'] { $*POD_IN_FORMATTINGCODE := 1 }
        <?{
            my $codenum := nqp::ord($<code>.Str) - nqp::ord("A");
            if !($*POD_ALLOW_FCODES +& (2 ** $codenum)) {
                0
            } elsif ~$<begin-tag> eq '«' {
              $endtag := "»";
              $*POD_ANGLE_COUNT := -1;
              1
            } else {
              my int $ct := nqp::chars($<begin-tag>);
              $endtag := nqp::x(">", $ct);
              my $rv := $*POD_ANGLE_COUNT <= 0 || $*POD_ANGLE_COUNT >= $ct;
              $*POD_ANGLE_COUNT := $ct;
              $rv;
            }
        }>
        {
            if $<code>.Str eq "V" || $<code>.Str eq "C" {
                $*POD_ALLOW_FCODES := 0;
            }
        }
        [ <!{$<code> eq 'E'}>
          $<contents>=[
              <!before $endtag>
              [ <?{$<code> ne 'L' && $<code> ne 'D' && $<code> ne 'X' }> || <!before \s* \| > ]
              <pod_string_character>
          ]*
        ]?
        [
        | <?{$<code> eq 'L'}> \s* \| \s* $<meta>=[<!before $endtag>.]+
        | <?{$<code> eq 'X'}> \s* \| \s* ( [$<meta>=[<!before $endtag | <[,;]> >.]+] +%% \, ) +%% \;
        | <?{$<code> eq 'D'}> \s* \| \s* [$<meta>=[<!before $endtag | \; >.]+] +%% \;
        | <?{$<code> eq 'E'}> ( <integer> | $<uni_name>=<[A..Z\s]>+ <![a..z]> || $<html_ref>=<[A..Za..z]>+ ) +%% \;
        ]?
        [ $endtag || <.worry: "Pod formatting code $<code> missing endtag '$endtag'."> ]
    }

    token pod_balanced_braces {
        <?{ $*POD_IN_FORMATTINGCODE }>
        :my $endtag;
        [
            $<braces>=[
                      || '<'+ <![<]>
                      || '>'+ <![>]>
                      ]
            <?{ nqp::chars($<braces>) < $*POD_ANGLE_COUNT || $*POD_ANGLE_COUNT < 0 }>
          ||
            <?{ $*POD_ANGLE_COUNT >= 1 }>
            $<start>=['<'+] <![<]>
            <?{ nqp::chars($<start>) == $*POD_ANGLE_COUNT || $*POD_ANGLE_COUNT < 0 }>
            {
                $endtag := nqp::x(">", nqp::chars($<start>));
            }
            $<contents>=[ <pod_string_character>*?]
            <!after '>'> $<endtag>=[$endtag]
        ]
    }

    token pod_string {
        <pod_string_character>+
    }

    token pod_string_character {
        <pod_balanced_braces> || <pod_formatting_code> || $<char>=[ \N || [
            <?{ $*POD_IN_FORMATTINGCODE }> \n [
                <?{ $*POD_DELIMITED_CODE_BLOCK }> <!before \h* '=end' \h+ <pod-delim-code-typ> > ||
                <!before \h* '=' \w>
                ]
            ]
        ]
    }

    proto token pod_block { <...> }

    token pod_configuration($spaces = '') {
        [ [\n $spaces '=']? \h+ <colonpair> ]*
    }

    token pod_block:sym<delimited_comment> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ 'comment' {}
        <pod_configuration($<spaces>)> <pod_newline>+
        [
         $<pod_content> = [ .*? ]
         ^^ $<spaces> '=end' \h+
         [
            'comment' [ <pod_newline> | $ ]
            || $<instead>=<identifier>? {
                   $/.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd',
                       type    => 'comment',
                       spaces  => ~$<spaces>,
                       instead => $<instead> ?? ~$<instead> !! ''
               }
         ]
        ]
    }

    regex pod_block:sym<delimited> {
        ^^
        $<spaces> = [ \h* ]
        '=begin'
        [ <?pod_newline>
          <.typed_panic('X::Syntax::Pod::BeginWithoutIdentifier')>
        ]?
        \h+ <!before 'finish'>
        {
            $*VMARGIN    := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        $<type> = [
            <pod_code_parent> {
                $*ALLOW_INLINE_CODE := 1;
            }
            || <identifier>
        ]
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>+
        [
         # TODO need first line to check for ws-separated '#'
         <pod_content> *
         ^^ $<spaces> '=end' \h+
         [
             $<type> [ <pod_newline> | $ ]
             || $<instead>=<identifier>? {
                    $/.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd',
                        type    => ~$<type>,
                        spaces  => ~$<spaces>,
                        instead => $<instead> ?? ~$<instead> !! ''
                    }
         ]
        ]
    }


    token pod_block:sym<delimited_table> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ 'table' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>+
        [
         [ $<table_row>=<.table_row_or_blank> ]*
         ^^ \h* '=end' \h+
         [
            'table' [ <pod_newline> | $ ]
             || $<instead>=<identifier>? {
                    $/.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd',
                        type    => 'table',
                        spaces  => ~$<spaces>,
                        instead => $<instead> ?? ~$<instead> !! ''
                    }
         ]
        ]
    }

    # There are several different identifiers for pod blocks
    # that are treated essentially the same: 'code', 'input',
    # and 'output'.
    token pod-delim-code-typ { code | input | output }
    token pod_block:sym<delimited_code> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ $<typ>=<pod-delim-code-typ> {}
        :my $*POD_ALLOW_FCODES  := 0;
        :my $*POD_IN_CODE_BLOCK := 1;
        :my $*POD_DELIMITED_CODE_BLOCK := 1;
        <pod_configuration($<spaces>)> <pod_newline>+
        [
        || <delimited_code_content($<spaces>)> $<spaces> '=end' \h+
            [ $<end>=<pod-delim-code-typ> [ <pod_newline> | $ ]
              { if ~$<end> ne ~$<typ> {
                         $/.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd',
                         type    => ~$<typ>,
                         spaces  => ~$<spaces>,
                         instead => $<end> ?? ~$<end> !! ''
              }}
              || $<instead>=<identifier>? {
                     $/.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd',
                     type    => $<typ>,
                     spaces  => ~$<spaces>,
                     instead => $<instead> ?? ~$<instead> !! ''
                 }
            ]
        ]
    }

    token delimited_code_content($spaces = '') {
        ^^
        (
        | $spaces
            <!before '=end' \h+ <pod-delim-code-typ> [ <pod_newline> | $ ]>
            <pod_string>**0..1 <pod_newline>
        | <pod_newline>
        )*
    }

    token table_row {
        \h* <!before '=' \w> \N+ [ \n | $ ]
    }

    token table_row_or_blank {
        <.table_row> | [\h* <!before '=' \w> \n ]
    }

    token pod_block:sym<finish> {
        ^^ \h*
        [
            | '=begin' \h+ 'finish' <pod_newline>
            | '=for'   \h+ 'finish' <pod_newline>
            | '=finish' <pod_newline>
        ]
        $<finish> = .*
    }

    token pod_block:sym<paragraph> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ <!before 'finish'>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        [ :!ratchet
            $<type> = [
                <pod_code_parent> { $*ALLOW_INLINE_CODE := 1 }
                || <identifier>
            ]
            :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
            <pod_configuration($<spaces>)>
            <pod_newline>
        ]
        # TODO [defn, term], [first text, line numbered-alias]
        #   if this is a defn block
        #     the first line of the first pod_textcontent
        #     becomes the term
        #     then combine the rest of the text
        <pod_content=.pod_textcontent>**0..1
    }

    token pod_block:sym<paragraph_comment> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ 'comment' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>
        $<pod_content> = [ \h* <!before '=' \w> \N+ [ \n | $ ] ]*
    }

    token pod_block:sym<paragraph_table> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ 'table' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>

        # TODO add numbered-alias token here
        [ <!before \h* \n> <table_row>]*
    }

    token pod_block:sym<paragraph_code> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ <pod-delim-code-typ> {}
        :my $*POD_ALLOW_FCODES := 0;
        :my $*POD_IN_CODE_BLOCK := 1;
        <pod_configuration($<spaces>)> <pod_newline>

        # TODO get first line if IN-DEFN-BLOCK
        [ <!before \h* '=' \w> <pod_line> ]*
    }

    # TODO make sure this token works in all desired
    #      places: may have to remove the before/afters
    #      when using with non-abbreviated blocks
    #      (particulary code blocks)
    token numbered-alias { <after [^|\s]> '#' <before \s> }
    token pod_block:sym<abbreviated> {
        # Note an abbreviated block does not have
        # %config data, but see the hash mark
        # handling below.
        ^^
        $<spaces> = [ \h* ]
        '=' <!before begin || end || for || finish || config>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        [ :!ratchet
            $<type> = [
                <pod_code_parent> {
                    $*ALLOW_INLINE_CODE := 1;
                }
                || <identifier>
            ]
            :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');

            # An optional hash char here is special.
            [\h+ <numbered-alias>]?

            [\h*\n|\h+]
        ]
        # TODO [defn, term], [first text, line numbered-alias]
        <pod_content=.pod_textcontent>**0..1
    }

    token pod_block:sym<abbreviated_comment> {
        ^^
        $<spaces> = [ \h* ]
        '=comment' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        [\h*\n|\h+]
        $<pod_content> = [ \h* <!before '=' \w> \N+ [ \n | $ ] ]*
    }

    token pod_block:sym<abbreviated_table> {
        ^^
        $<spaces> = [ \h* ]
        '=table' {}

        # An optional hash char here is special.
        [\h+ <numbered-alias>]?

        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_newline>
        [ <!before \h* \n> <table_row>]*
    }

    token pod_block:sym<abbreviated_code> {
        ^^
        $<spaces> = [ \h* ]
        '=' <pod-delim-code-typ> {}
        :my $*POD_ALLOW_FCODES  := 0;
        :my $*POD_IN_CODE_BLOCK := 1;

        # An optional hash char here is special.
        # For the code block, we want to eat any ws
        # between the '#' and the next char
        #$<numbered-alias>=[\h+ '#' \s]?
        $<numbered-alias>=[\h+ '#' \s]?

        [\h*\n|\h+]

        [ <!before \h* '=' \w> <pod_line> ]*
    }

    token pod_line { <pod_string>**1 [ <pod_newline> | $ ] }

    token pod_newline {
        \h* \n
    }

    # These parents can contain implicit code blocks when data
    # lines begin with whitespace indention from the virtual
    # margin.
    token pod_code_parent {
        [
        | [ 'pod' | 'item' \d* | 'nested' | 'defn' | 'finish' ]
        | <upper>+
        ]
        <![\w]>
    }
}

grammar Raku::QGrammar is HLL::Grammar does Raku::Common {
    proto token escape {*}
    proto token backslash {*}

    role b1 {
        token escape:sym<\\> { <sym> {} <item=.backslash> }
        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:delim { <text=.starter> | <text=.stopper> }
        token backslash:sym<a> { <sym> }
        token backslash:sym<b> { <sym> }
        token backslash:sym<c> { <sym> <charspec> }
        token backslash:sym<e> { <sym> }
        token backslash:sym<f> { <sym> }
        token backslash:sym<N> { <?before 'N{'<.[A..Z]>> <.obs('\N{CHARNAME}','\c[CHARNAME]')>  }
        token backslash:sym<n> { <sym> }
        token backslash:sym<o> { :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ] }
        token backslash:sym<r> { <sym> }
        token backslash:sym<rn> { 'r\n' }
        token backslash:sym<t> { <sym> }
        token backslash:sym<x> { :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ] }
        token backslash:sym<0> { <sym> }
        token backslash:sym<1> {
            <[1..9]>\d* {
              self.typed_panic: 'X::Backslash::UnrecognizedSequence',
                :sequence(~$/), :suggestion('$' ~ ($/ - 1))
            }
        }
        token backslash:sym<unrec> {
          {} (\w) {
            self.typed_panic: 'X::Backslash::UnrecognizedSequence',
              :sequence($/[0].Str)
          }
        }
        token backslash:sym<misc> { \W }
    }

    role b0 {
        token escape:sym<\\> { <!> }
    }

    role s1 {
        token escape:sym<$> {
            :my $*QSIGIL := '$';
            <?[$]>
            <!RESTRICTED>
            [ <EXPR=.LANG('MAIN', 'EXPR', 'y=')> || { $*W.throw($/, 'X::Backslash::NonVariableDollar') } ]
        }
    }

    role s0 {
        token escape:sym<$> { <!> }
    }

    role a1 {
        token escape:sym<@> {
            :my $*QSIGIL := '@';
            <?[@]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role a0 {
        token escape:sym<@> { <!> }
    }

    role h1 {
        token escape:sym<%> {
            :my $*QSIGIL := '%';
            <?[%]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role h0 {
        token escape:sym<%> { <!> }
    }

    role f1 {
        token escape:sym<&> {
            :my $*QSIGIL := '&';
            <?[&]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role f0 {
        token escape:sym<&> { <!> }
    }

    role c1 {
        token escape:sym<{ }> { :my $*ESCAPEBLOCK := 1; <?[{]> <!RESTRICTED> <block=.LANG('MAIN','block')> }
    }

    role c0 {
        token escape:sym<{ }> { <!> }
    }

    role ww {
        token escape:sym<'> {
            <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')>
        }
        token escape:sym<colonpair> {
            <?[:]> <!RESTRICTED> <colonpair=.LANG('MAIN','colonpair')>
        }
        token escape:sym<#> {
            <?[#]> <.LANG('MAIN', 'comment')>
        }
    }

    role to[$herelang] {
        method herelang() { $herelang }
        method postprocessors () { nqp::list_s('heredoc') } # heredoc strings are the only postproc when present
    }

    role q {
        token starter { \' }
        token stopper { \' }

        token escape:sym<\\> { <sym> <item=.backslash> }

        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:delim { <text=.starter> | <text=.stopper> }

        token backslash:sym<miscq> { {} . }

        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

    role qq does b1 does s1 does a1 does h1 does f1 does c1 {
        token starter { \" }
        token stopper { \" }
        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

    method truly($bool, $opt) {
        self.sorry("Cannot negate $opt adverb") unless $bool;
        self;
    }

    method apply_tweak($role) {
        my $target := nqp::can(self, 'herelang') ?? self.herelang !! self;
        $target.HOW.mixin($target, $role);
        self
    }

    method tweak_q($v)          { self.truly($v, ':q'); self.apply_tweak(Raku::QGrammar::q) }
    method tweak_single($v)     { self.tweak_q($v) }
    method tweak_qq($v)         { self.truly($v, ':qq'); self.apply_tweak(Raku::QGrammar::qq); }
    method tweak_double($v)     { self.tweak_qq($v) }
    method tweak_b($v)          { self.apply_tweak($v ?? b1 !! b0) }
    method tweak_backslash($v)  { self.tweak_b($v) }
    method tweak_s($v)          { self.apply_tweak($v ?? s1 !! s0) }
    method tweak_scalar($v)     { self.tweak_s($v) }
    method tweak_a($v)          { self.apply_tweak($v ?? a1 !! a0) }
    method tweak_array($v)      { self.tweak_a($v) }
    method tweak_h($v)          { self.apply_tweak($v ?? h1 !! h0) }
    method tweak_hash($v)       { self.tweak_h($v) }
    method tweak_f($v)          { self.apply_tweak($v ?? f1 !! f0) }
    method tweak_function($v)   { self.tweak_f($v) }
    method tweak_c($v)          { self.apply_tweak($v ?? c1 !! c0) }
    method tweak_closure($v)    { self.tweak_c($v) }

    my role postproc[@curlist] {
        method postprocessors() {
            @curlist;
        }
    }

    method add-postproc(str $newpp) {
        my $target := nqp::can(self, 'herelang') ?? self.herelang !! self;

        my @pplist := nqp::can($target, "postprocessors")
            ?? $target.postprocessors
            !! nqp::list_s();
        nqp::push_s(@pplist, $newpp);

        # yes, the currying is necessary. Otherwise weird things can happen,
        # e.g.  raku -e 'q:w:x//; q:ww:v//' turning the second into q:w:x:v//
        $target.HOW.mixin($target, postproc.HOW.curry(postproc, @pplist));
        self
    }

    method tweak_x($v)          { $v ?? self.add-postproc("exec") !! self }
    method tweak_exec($v)       { self.tweak_x($v) }
    method tweak_w($v)          { $v ?? self.add-postproc("words") !! self }
    method tweak_words($v)      { self.tweak_w($v) }
    method tweak_ww($v)         { $v ?? self.add-postproc("quotewords").apply_tweak(ww) !! self }
    method tweak_quotewords($v) { self.tweak_ww($v) }
    method tweak_v($v)          { $v ?? self.add-postproc("val") !! self }
    method tweak_val($v)        { self.tweak_v($v) }

    method tweak_to($v) {
        self.truly($v, ':to');
        # the cursor_init is to ensure it's been initialized the same way
        # 'self' was back in quote_lang
        my $q := self.slang_grammar('Quote');
        $q.HOW.mixin($q, to.HOW.curry(to, self)).'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'()))
    }
    method tweak_heredoc($v)    { self.tweak_to($v) }

    method tweak_regex($v) {
        self.truly($v, ':regex');
        return self.slang_grammar('Regex');
    }

    token nibbler {
        :my @*nibbles;
        <.do_nibbling>
    }

    token do_nibbling {
        :my $from := self.pos;
        :my $to   := $from;
        [
            <!stopper>
            [
            || <starter> <nibbler> <stopper>
                {
                    my $c := $/;
                    $to   := $<starter>[-1].from;
                    if $from != $to {
                        nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
                    }

                    nqp::push(@*nibbles, $<starter>[-1].Str);
                    nqp::push(@*nibbles, $<nibbler>[-1]);
                    nqp::push(@*nibbles, $<stopper>[-1].Str);

                    $from := $to := $c.pos;
                }
            || <escape>
                {
                    my $c := $/;
                    $to   := $<escape>[-1].from;
                    if $from != $to {
                        nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
                    }

                    nqp::push(@*nibbles, $<escape>[-1]);

                    $from := $to := $c.pos;
                }
            || .
            ]
        ]*
        {
            my $c := $/;
            $to   := $c.pos;
            $*LASTQUOTE := [self.pos, $to];
            if $from != $to || !@*nibbles {
                nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
            }
        }
    }
}

grammar Raku::RegexGrammar is QRegex::P6Regex::Grammar does Raku::Common {
    method throw_unrecognized_metachar ($metachar) {
        self.typed_sorry('X::Syntax::Regex::UnrecognizedMetachar', :$metachar);
    }
    method throw_null_pattern() {
        self.typed_sorry('X::Syntax::Regex::NullRegex');
    }
    method throw_unrecognized_regex_modifier($modifier) {
        self.typed_panic('X::Syntax::Regex::UnrecognizedModifier', :$modifier);
    }

    method throw_malformed_range() { self.typed_sorry('X::Syntax::Regex::MalformedRange') }
    method throw_confused() { self.typed_sorry('X::Syntax::Confused') }
    method throw_unspace($char) { self.typed_sorry('X::Syntax::Regex::Unspace', :$char) }
    method throw_regex_not_terminated() { self.typed_sorry('X::Syntax::Regex::Unterminated') }
    method throw_spaces_in_bare_range() { self.typed_sorry('X::Syntax::Regex::SpacesInBareRange') }
    method throw_non_quantifiable() { self.typed_sorry('X::Syntax::Regex::NonQuantifiable') }
    method throw_solitary_quantifier() { self.typed_panic('X::Syntax::Regex::SolitaryQuantifier') }
    method throw_solitary_backtrack_control() { self.typed_sorry('X::Syntax::Regex::SolitaryBacktrackControl') }

    token normspace { <?before \s | '#'> <.LANG('MAIN', 'ws')> }

    token rxstopper { <stopper> }

    token metachar:sym<:my> {
        ':' <?before ['my'|'constant'|'state'|'our'|'temp'|'let'] >> >
        <statement=.LANG('MAIN', 'statement')>
        <!RESTRICTED>
        <.LANG('MAIN', 'eat_terminator')>
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    # This doesn't handle the backref case; this is done in the (inherited)
    # metachar:sym<var>, which will win thanks to the LANG switch being an
    # LTM barrier.
    token metachar:sym<rakvar> {
        <?before <.sigil> $<twigil>=[<.alpha> | <+[\W]-[\s]><.alpha> | '(']>
        <!before <.sigil> <.rxstopper> >
        <var=.LANG('MAIN', 'variable')>
        { self.check_variable($<var>) }
        [
            <?before '.'? <.[ \[ \{ \< ]>>
            <.worry: "Apparent subscript will be treated as regex">
        ]?
        <.SIGOK>
    }


    token metachar:sym<qw> {
        <?before '<' \s >  # (note required whitespace)
        '<' <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w']))> '>'
        <.SIGOK>
    }

    token metachar:sym<'> { <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')> <.SIGOK> }

    token metachar:sym<{}> { \\<[xo]>'{' <.obsbrace> }

    token backslash:sym<1> {
        <.[\d] - [0]>\d*
        {}
        :my int $br := nqp::radix(10, $/, 0, 0)[0];
        <.typed_panic: 'X::Backslash::UnrecognizedSequence', :sequence(~$/), :suggestion('$' ~ ($/ - 1))>
    }

    token assertion:sym<name> {
        <longname=.LANG('MAIN','longname')>
            [
            | <?[>]>
            | '=' <assertion>
            | ':' <arglist>
            | '(' <arglist> ')'
            | <.normspace> <nibbler>
            ]?
    }

    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<?{ }> {
        '?' <?before '{'> <codeblock>
    }

    token assertion:sym<!{ }> {
        '!' <?before '{'> <codeblock>
    }

    token assertion:sym<var> {
        [
        | <?[&]> <!RESTRICTED> <call=.LANG('MAIN', 'term:sym<variable>')>
            [
            | ':' <arglist>
            | '(' <arglist> ')'
            ]?
        | <?sigil> <!RESTRICTED> <var=.LANG('MAIN', 'term:sym<variable>')>
        ]
    }

    token codeblock {
        :my $*ESCAPEBLOCK := 1;
        <!RESTRICTED>
        <block=.LANG('MAIN','block')>
    }

    token arglist {
        :my $*IN_REGEX_ASSERTION := 1;
        <!RESTRICTED>
        <arglist=.LANG('MAIN','arglist')>
    }
}
