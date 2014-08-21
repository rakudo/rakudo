use QRegex;
use NQPP6QRegex;
use NQPP5QRegex;
use Perl6::Actions;
use Perl6::World;
use Perl6::Pod;

role startstop[$start, $stop] {
    token starter { $start }
    token stopper { $stop }
}

role stop[$stop] {
    token starter { <!> }
    token stopper { $stop }
}

# This role captures things that STD factors out from any individual grammar,
# but that don't make sense to go in HLL::Grammar.
role STD {
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
        \x2AF7 \x2AF9 \x2E02 \x2E04 \x2E09 \x2E0C \x2E1C \x2E20 \x3008 \x300A \x300C
        \x300E \x3010 \x3014 \x3016 \x3018 \x301A \x301D \xFD3E \xFE17 \xFE35 \xFE37
        \xFE39 \xFE3B \xFE3D \xFE3F \xFE41 \xFE43 \xFE47 \xFE59 \xFE5B \xFE5D \xFF08
        \xFF1C \xFF3B \xFF5B \xFF5F \xFF62
        ]>
    }
    
    method balanced($start, $stop) {
        self.HOW.mixin(self, startstop.HOW.curry(startstop, $start, $stop));
    }
    method unbalanced($stop) {
        self.HOW.mixin(self, stop.HOW.curry(stop, $stop));
    }
    
    token starter { <!> }
    token stopper { <!> }
    
    my %quote_lang_cache;
    method quote_lang($l, $start, $stop, @base_tweaks?, @extra_tweaks?) {
        sub lang_key() {
            my @keybits := [$l.HOW.name($l), $start, $stop];
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
            my $lang := $l;
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
            $start ne $stop ?? $lang.balanced($start, $stop)
                            !! $lang.unbalanced($stop);
        }

        # Get language from cache or derive it.
        my $key := lang_key();
        nqp::ifnull(%quote_lang_cache, %quote_lang_cache := nqp::hash());
        nqp::existskey(%quote_lang_cache, $key) && $key ne 'NOCACHE'
            ?? %quote_lang_cache{$key}
            !! (%quote_lang_cache{$key} := con_lang());
    }
    
    token babble($l, @base_tweaks?) {
        :my @extra_tweaks;

        <.ws>
        [ <quotepair> <.ws>
            {
                my $kv := $<quotepair>[-1].ast;
                my $k  := $kv.named;
                if nqp::istype($kv, QAST::Stmts) || nqp::istype($kv, QAST::Stmt) && +@($kv) == 1 {
                    $kv := $kv[0];
                }
                my $v := nqp::istype($kv, QAST::IVal)
                    ?? $kv.value
                    !! $kv.has_compile_time_value
                        ?? $kv.compile_time_value
                        !! self.panic("Invalid adverb value for " ~ $<quotepair>[-1].Str);
                nqp::push(@extra_tweaks, [$k, $v]);
            }
        ]*

        $<B>=[<?>]
        {
            # Work out the delimeters.
            my $c := $/.CURSOR;
            my @delims := $c.peek_delimiters($c.target, $c.pos);
            my $start := @delims[0];
            my $stop  := @delims[1];
            
            # Get the language.
            my $lang := self.quote_lang($l, $start, $stop, @base_tweaks, @extra_tweaks);
            $<B>.'!make'([$lang, $start, $stop]);
        }
    }
    
    my @herestub_queue;

    my class Herestub {
        has $!delim;
        has $!orignode;
        has $!lang;
        method delim() { $!delim }
        method orignode() { $!orignode }
        method lang() { $!lang }
    }

    role herestop {
        token starter { <!> }
        token stopper { ^^ {} $<ws>=(\h*) $*DELIM \h* $$ \v? }
    }

    method heredoc () {
        if @herestub_queue {
            my $here := self.'!cursor_start_cur'();
            $here.'!cursor_pos'(self.pos);
            while @herestub_queue {
                my $herestub := nqp::shift(@herestub_queue);
                my $*DELIM := $herestub.delim;
                my $lang := $herestub.lang.HOW.mixin($herestub.lang, herestop);
                my $doc := $here.nibble($lang);
                if $doc {
                    # Match stopper.
                    my $stop := $lang.'!cursor_init'(self.orig(), :p($doc.pos), :shared(self.'!shared'())).stopper();
                    unless $stop {
                        self.panic("Ending delimiter $*DELIM not found");
                    }
                    $here.'!cursor_pos'($stop.pos);
                    
                    # Get it trimmed and AST updated.
                    $*ACTIONS.trim_heredoc($doc, $stop, $herestub.orignode.MATCH.ast);
                }
                else {
                    self.panic("Ending delimiter $*DELIM not found");
                }
            }
            $here.'!cursor_pass'($here.pos);
            $here
        }
        else {
            self
        }
    }

    method queue_heredoc($delim, $lang) {
        nqp::ifnull(@herestub_queue, @herestub_queue := []);
        nqp::push(@herestub_queue, Herestub.new(:$delim, :$lang, :orignode(self)));
        return self;
    }

    token quibble($l, *@base_tweaks) {
        :my $lang;
        :my $start;
        :my $stop;
        <babble($l, @base_tweaks)>
        { my $B := $<babble><B>.ast; $lang := $B[0]; $start := $B[1]; $stop := $B[2]; }

        $start <nibble($lang)> [ $stop || { $/.CURSOR.panic("Couldn't find terminator $stop") } ]

        {
            nqp::can($lang, 'herelang') && self.queue_heredoc(
                $*W.nibble_to_str($/, $<nibble>.ast[1], -> { "Stopper '" ~ $<nibble> ~ "' too complex for heredoc" }),
                $lang.herelang)
        }
    }

    method nibble($lang) {
        my $lang_cursor := $lang.'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'()));
        my $*ACTIONS;
        for %*LANG {
            if nqp::istype($lang, $_.value) {
                $*ACTIONS := %*LANG{$_.key ~ '-actions'};
                last;
            }
        }
        $lang_cursor.nibbler();
    }
    
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
        $*W.throw(self.MATCH(), nqp::split('::', $type_str), |%opts);
    }
    method typed_sorry($type_str, *%opts) {
        if +@*SORROWS + 1 == $*SORRY_LIMIT {
            $*W.throw(self.MATCH(), nqp::split('::', $type_str), |%opts);
        }
        else {
            @*SORROWS.push($*W.typed_exception(self.MATCH(), nqp::split('::', $type_str), |%opts));
        }
        self
    }
    method typed_worry($type_str, *%opts) {
        @*WORRIES.push($*W.typed_exception(self.MATCH(), nqp::split('::', $type_str), |%opts));
        self
    }
    
    method malformed($what) {
        self.typed_panic('X::Syntax::Malformed', :$what);
    }
    method missing($what) {
        self.typed_panic('X::Syntax::Missing', :$what);
    }
    method NYI($feature) {
        self.typed_panic('X::Comp::NYI', :$feature)
    }

    method EXPR_nonassoc($cur, $left, $right) {
        self.typed_panic('X::Syntax::NonAssociative', :left(~$left), :right(~$right));
    }

    # "when" arg assumes more things will become obsolete after Perl 6 comes out...
    method obs($old, $new, $when = 'in Perl 6') {
        $*W.throw(self.MATCH(), ['X', 'Obsolete'],
            old         => $old,
            replacement => $new,
            when        => $when,
        );
    }
    method sorryobs($old, $new, $when = ' in Perl 6') {
        $*W.throw(self.MATCH(), ['X', 'Obsolete'],
            old         => $old,
            replacement => $new,
            when        => $when,
        );
    }
    method worryobs($old, $new, $when = ' in Perl 6') {
        $*W.throw(self.MATCH(), ['X', 'Obsolete'],
            old         => $old,
            replacement => $new,
            when        => $when,
        );
    }
    
    method check_variable($var) {
        my $varast := $var.ast;
        if nqp::istype($varast, QAST::Op) && $varast.op eq 'ifnull' {
            $varast := $varast[0];
        }
        if !$*IN_DECL && nqp::istype($varast, QAST::Var) && $varast.scope eq 'lexical' {
            my $name := $varast.name;
            if $name ne '%_' && $name ne '@_' && !$*W.is_lexical($name) {
                if $var<sigil> ne '&' {
                    my @suggestions := $*W.suggest_lexicals($name);
                    $*W.throw($var, ['X', 'Undeclared'], symbol => $varast.name(), suggestions => @suggestions);
                }
                else {
                    $var.CURSOR.add_mystery($varast.name, $var.to, 'var');
                }
            }
            else {
                my $lex := $*W.cur_lexpad();
                my %sym := $lex.symbol($name);
                if %sym {
                    %sym<used> := 1;
                }
                else {
                    # Add mention-only record (used to poison outer
                    # usages and disambiguate hashes/blocks by use of
                    # $_ when $*IMPLICIT is in force).
                    my $au := $lex.ann('also_uses');
                    $lex.annotate('also_uses', $au := {}) unless $au;
                    $au{$name} := 1;
                }
            }
        }
        self
    }
}

grammar Perl6::Grammar is HLL::Grammar does STD {
    my $sc_id := 0;
    method TOP() {
        # Language braid.
        my %*LANG;
        %*LANG<Regex>           := Perl6::RegexGrammar;
        %*LANG<Regex-actions>   := Perl6::RegexActions;
        %*LANG<P5Regex>         := Perl6::P5RegexGrammar;
        %*LANG<P5Regex-actions> := Perl6::P5RegexActions;
        %*LANG<Q>               := Perl6::QGrammar;
        %*LANG<Q-actions>       := Perl6::QActions;
        %*LANG<MAIN>            := Perl6::Grammar;
        %*LANG<MAIN-actions>    := Perl6::Actions;
        
        # Package declarator to meta-package mapping. Starts pretty much empty;
        # we get the mappings either imported or supplied by the setting. One
        # issue is that we may have no setting to provide them, e.g. when we
        # compile the setting, but it still wants some kinda package. We just
        # fudge in knowhow for that.
        my %*HOW;
        %*HOW<knowhow> := nqp::knowhow();
        %*HOW<package> := nqp::knowhow();
        
        # Symbol table and serialization context builder - keeps track of
        # objects that cross the compile-time/run-time boundary that are
        # associated with this compilation unit.
        my $file := nqp::getlexdyn('$?FILES');
        my $source_id := nqp::sha1(
            nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>)
                ?? self.target() ~ $sc_id++
                !! self.target());
        my $*W := nqp::isnull($file) ??
            Perl6::World.new(:handle($source_id)) !!
            Perl6::World.new(:handle($source_id), :description($file));
        $*W.add_initializations();

        my $cursor := self.comp_unit;
        $*W.pop_lexpad(); # UNIT
        $*W.pop_lexpad(); # UNIT_OUTER
        $cursor;
    }

    ## Lexer stuff

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
        ||  <?before '(' | <alpha> >
            [
            | <identifier>
            | :dba('indirect name') '(' ~ ')' <EXPR>
            ]
        || <?before '::'> <.typed_panic: "X::Syntax::Name::Null">
        ]?
    }

    token longname {
        <name> {} [ <?before ':' <+alpha+[\< \[ \« ]>> <colonpair> ]*
    }

    token deflongname {
        :dba('new name to be defined')
        <name> <colonpair>*
    }

    token defterm {     # XXX this is probably too general
        :dba('new term to be defined')
        <identifier>
        [
        | <colonpair>+
            {
                if $<colonpair>[0]<coloncircumfix> -> $cf {
                    my $category := $<identifier>.Str;
                    my $opname := $cf<circumfix>
                        ?? $*W.colonpair_nibble_to_str($/, $cf<circumfix><nibble>)
                        !! '';
                    my $canname  := $category ~ ":sym<" ~ $opname ~ ">";
                    my $termname := $category ~ ":<" ~ $opname ~ ">";
                    $/.CURSOR.add_categorical($category, $opname, $canname, $termname);
                }
            }
        | <?>
        ]
    }

    token module_name {
        <longname>
        [ <?[[]> :dba('generic role') '[' ~ ']' <arglist> ]?
    }

    token end_keyword {
        <!before <[ \( \\ ' \- ]> || \h* '=>'> »
    }
    token spacey { <?[\s#]> }

    token ENDSTMT {
        [
        | \h* $$ <.ws> <?MARKER('endstmt')>
        | <.unv>? $$ <.ws> <?MARKER('endstmt')>
        ]?
    }

    # ws is highly performance sensitive. So, we check if we already marked it
    # at this point with a simple method, and only if that is not the case do
    # we bother doing any pattern matching.
    method ws() {
        if self.MARKED('ws') {
            self
        }
        else {
            self._ws()
        }
    }
    token _ws {
        :my $old_highexpect := self.'!fresh_highexpect'();
        :dba('whitespace')
        <!ww>
        [
        | <.vws> <.heredoc>
        | <.unv>
        | <.unsp>
        ]*
        <?MARKER('ws')>
        :my $stub := self.'!set_highexpect'($old_highexpect);
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
        '#`' <?opener> {}
        [ <.quibble(%*LANG<Q>)> || <.typed_panic: 'X::Syntax::Comment::Embedded'> ]
    }

    token comment:sym<#|(...)> {
        '#|' <?opener> <attachment=.quibble(%*LANG<Q>)>
    }

    token comment:sym<#|> {
        '#|' \h+ $<attachment>=[\N*]
        {
            unless %*SEEN_IT{ self.from() } {
                %*SEEN_IT{ self.from() } := 1;
                if $*DECLARATOR_DOCS eq '' {
                    $*DECLARATOR_DOCS := $<attachment>;
                } else {
                    $*DECLARATOR_DOCS := nqp::concat($*DECLARATOR_DOCS, nqp::concat("\n", $<attachment>));
                }
            }
        }
    }

    token comment:sym<#=> { # This seems to occur before the end of package_def (even if the comment follows the block) =)
        '#=' \h+ $<attachment>=[\N*]
        {
            unless %*SEEN_IT{ self.from() } {
                %*SEEN_IT{ self.from() } := 1;
                my $*DOC := $<attachment>;
                my $*DOCEE;
                #self.attach_docs;
                if ~$*DOC ne '' {
                    my $cont  := Perl6::Pod::serialize_aos(
                        [Perl6::Pod::formatted_text(~$*DOC)]
                    ).compile_time_value;
                    my $block := $*W.add_constant(
                        'Pod::Block::Declarator', 'type_new',
                        :nocache, :trailing([$cont]),
                    );
                    # XXX why compile_time_value?
                    $*DOCEE := $block.compile_time_value;
                    $*POD_BLOCKS.push($*DOCEE);
                }
                unless nqp::isnull($*PRECEDING_DECL) {
                    Perl6::Pod::document($/, $*PRECEDING_DECL, $*DOCEE, :trailing);
                }
            }
        }
    }

    method attach_docs() {
        if ~$*DOC ne '' {
            my $cont  := Perl6::Pod::serialize_aos(
                [Perl6::Pod::formatted_text(~$*DOC)]
            ).compile_time_value;
            my $block := $*W.add_constant(
                'Pod::Block::Declarator', 'type_new',
                :nocache, :leading([$cont]),
            );
            # XXX why compile_time_value?
            $*DOCEE := $block.compile_time_value;
            $*POD_BLOCKS.push($*DOCEE);
        }
        self
    }

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
    token pod_content:sym<text> {
        <pod_newline>*
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

    # text not being code
    token pod_textcontent:sym<regular> {
        $<spaces>=[ \h* ]
         <?{ $*POD_IN_CODE_BLOCK
             || !$*ALLOW_INLINE_CODE
             || ($<spaces>.to - $<spaces>.from) <= $*VMARGIN }>

        $<text> = [
            \h* <!before '=' \w> <pod_string> <pod_newline>
        ] +
    }

    token pod_textcontent:sym<code> {
        $<spaces>=[ \h* ]
        <?{ !$*POD_IN_CODE_BLOCK
            && $*ALLOW_INLINE_CODE
            && ($<spaces>.to - $<spaces>.from) > $*VMARGIN }>
        $<text> = [
            [<!before '=' \w> \N+]+ % [<pod_newline> $<spaces>]
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
              my $ct := nqp::chars($<begin-tag>);
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
                <?{ $*POD_DELIMITED_CODE_BLOCK }> <!before \h* '=end' \h+ code> ||
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
         ^^ $<spaces> '=end' \h+ 'comment' <pod_newline>
         || {$/.CURSOR.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd', type => 'comment', spaces => ~$<spaces>}
        ]
    }

    token pod_block:sym<delimited> {
        ^^
        $<spaces> = [ \h* ]
        '=begin'
        [ <?pod_newline>
          <.typed_panic('X::Syntax::Pod::BeginWithoutIdentifier')>
        ]?
        \h+ <!before 'END'>
        {
            $*VMARGIN    := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_INLINE_CODE := 1 }
            || <identifier>
        ]
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>+
        [
         <pod_content> *
         ^^ $<spaces> '=end' \h+ $<type> <pod_newline>
         || {$/.CURSOR.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd', type => ~$<type>, spaces => ~$<spaces>}
        ]
    }


    token pod_block:sym<delimited_table> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ 'table' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>+
        [
         <table_row>*
         ^^ \h* '=end' \h+ 'table' <pod_newline>
         || {$/.CURSOR.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd', type => 'table', spaces => ~$<spaces>}
        ]
    }

    token pod_block:sym<delimited_code> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ 'code' {}
        :my $*POD_ALLOW_FCODES  := 0;
        :my $*POD_IN_CODE_BLOCK := 1;
        :my $*POD_DELIMITED_CODE_BLOCK := 1;
        <pod_configuration($<spaces>)> <pod_newline>+
        [
        || <delimited_code_content($<spaces>)>
        || {$/.CURSOR.typed_panic: 'X::Syntax::Pod::BeginWithoutEnd', type => 'code', spaces => ~$<spaces>}
        ]
    }

    token delimited_code_content($spaces = '') {
        ^^
        [
        | $spaces
          [
          || '=end' \h+ 'code' <pod_newline>
          || <pod_string>**0..1 <pod_newline>
             <delimited_code_content($spaces)>
          ]
        | <pod_newline> <delimited_code_content($spaces)>
        ]
    }

    token table_row {
        \h* <!before '=' \w> \N* \n
    }

    token pod_block:sym<end> {
        ^^ \h*
        [
            | '=begin' \h+ 'END' <pod_newline>
            | '=for'   \h+ 'END' <pod_newline>
            | '=END' <pod_newline>
        ]
        .*
    }

    token pod_block:sym<paragraph> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ <!before 'END'>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_INLINE_CODE := 1 }
            || <identifier>
        ]
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>
        <pod_content=.pod_textcontent>**0..1
    }

    token pod_block:sym<paragraph_comment> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ 'comment' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>
        $<pod_content> = [ \h* <!before '=' \w> \N+ \n ]+
    }

    token pod_block:sym<paragraph_table> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ 'table' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_configuration($<spaces>)> <pod_newline>
        [ <!before \h* \n> <table_row>]*
    }

    token pod_block:sym<paragraph_code> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ 'code' {}
        :my $*POD_ALLOW_FCODES := 0;
        :my $*POD_IN_CODE_BLOCK := 1;
        <pod_configuration($<spaces>)> <pod_newline>
        [ <!before \h* '=' \w> <pod_line> ]*
    }

    token pod_block:sym<abbreviated> {
        ^^
        $<spaces> = [ \h* ]
        '=' <!before begin || end || for || END || config>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_INLINE_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_INLINE_CODE := 1 }
            || <identifier>
        ]
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        [\h*\n|\h+]
        <pod_content=.pod_textcontent>**0..1
    }

    token pod_block:sym<abbreviated_comment> {
        ^^
        $<spaces> = [ \h* ]
        '=comment' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        [\h*\n|\h+]
        $<pod_content> = [ \h* <!before '=' \w> \N+ \n ]*
    }

    token pod_block:sym<abbreviated_table> {
        ^^
        $<spaces> = [ \h* ]
        '=table' {}
        :my $*POD_ALLOW_FCODES := nqp::getlexdyn('$*POD_ALLOW_FCODES');
        <pod_newline>
        [ <!before \h* \n> <table_row>]*
    }

    token pod_block:sym<abbreviated_code> {
        ^^
        $<spaces> = [ \h* ]
        '=code' {}
        :my $*POD_ALLOW_FCODES  := 0;
        :my $*POD_IN_CODE_BLOCK := 1;
        [\h*\n|\h+]
        [ <!before \h* '=' \w> <pod_line> ]*
    }

    token pod_line { <pod_string>**1 <pod_newline> }

    token pod_newline {
        \h* [ \n | $ ]
    }

    token pod_code_parent {
        [
        | [ 'pod' | 'item' \d* | 'nested' | 'defn' | 'finish' ]
        | <upper>+
        ]
        <![\w]>
    }

    token install_doc_phaser { <?> }

    token vnum {
        \d+ | '*'
    }

    token version {
        'v' <?[\d]> {} $<vstr>=[<vnum>+ % '.' '+'?]
        <!before '-'|\'> # cheat because of LTM fail
    }

    ## Top-level rules

    token comp_unit {
        # From STD.pm.
        :my $*LEFTSIGIL;                           # sigil of LHS for item vs list assignment
        :my $*SCOPE := '';                         # which scope declarator we're under
        :my $*MULTINESS := '';                     # which multi declarator we're under
        :my $*QSIGIL := '';                        # sigil of current interpolation
        :my $*IN_META := '';                       # parsing a metaoperator like [..]
        :my $*IN_REDUCE := 0;                      # attempting to parse an [op] construct
        :my $*IN_DECL;                             # what declaration we're in
        :my $*HAS_SELF := '';                      # is 'self' available? (for $.foo style calls)
        :my $*MONKEY_TYPING := 0;                  # whether augment/supersede are allowed
        :my $*begin_compunit := 1;                 # whether we're at start of a compilation unit
        :my $*DECLARAND;                           # the current thingy we're declaring, and subject of traits
        :my $*METHODTYPE;                          # the current type of method we're in, if any
        :my $*PKGDECL;                             # what type of package we're in, if any
        :my %*MYSTERY;                             # names we assume may be post-declared functions
        :my $*BORG;                                # who gets blamed for a missing block
        :my $*CCSTATE := '';
        
        # Error related. There are three levels: worry (just a warning), sorry
        # (fatal but not immediately so) and panic (immediately deadly). There
        # is a limit on the number of sorrows also. Unlike STD, which emits the
        # textual messages as it goes, we keep track of the exception objects
        # and, if needed, make a compositite exception group.
        :my @*WORRIES;                             # exception objects resulting from worry
        :my @*SORROWS;                             # exception objects resulting from sorry
        :my $*SORRY_LIMIT := 10;                   # when sorrow turns to panic

        # Extras.
        :my %*METAOPGEN;                           # hash of generated metaops
        :my %*HANDLERS;                            # block exception handlers
        :my $*IMPLICIT;                            # whether we allow an implicit param
        :my $*FORBID_PIR := 0;                     # whether pir::op and Q:PIR { } are disallowed
        :my $*HAS_YOU_ARE_HERE := 0;               # whether {YOU_ARE_HERE} has shown up
        :my $*OFTYPE;
        :my $*VMARGIN    := 0;                     # pod stuff
        :my $*ALLOW_INLINE_CODE := 0;              # pod stuff
        :my $*POD_IN_CODE_BLOCK := 0;              # pod stuff
        :my $*POD_IN_FORMATTINGCODE := 0;          # pod stuff
        :my $*POD_ALLOW_FCODES := 0b11111111111111111111111111; # allow which fcodes?
        :my $*POD_ANGLE_COUNT := 0;                # pod stuff
        :my $*IN_REGEX_ASSERTION := 0;
        :my $*SOFT := 0;                           # is the soft pragma in effect
        :my $*IN_PROTO := 0;                       # are we inside a proto?
        
        # Various interesting scopes we'd like to keep to hand.
        :my $*GLOBALish;
        :my $*PACKAGE;
        :my $*SETTING;
        :my $*UNIT;
        :my $*UNIT_OUTER;
        :my $*EXPORT;
        # stack of packages, which the 'is export' needs
        :my @*PACKAGES := [];

        # A place for Pod
        :my $*POD_BLOCKS := [];
        :my $*POD_BLOCKS_SEEN := {};
        :my $*POD_PAST;
        :my $*DECLARATOR_DOCS;
        :my $*PRECEDING_DECL; # for #= comments
        :my $*PRECEDING_DECL_LINE := -1; # XXX update this when I see another comment like it?
        :my %*SEEN_IT; # XXX how does this relate to POD_BLOCKS_SEEN?
        
        # Quasis and unquotes
        :my $*IN_QUASI := 0;                       # whether we're currently in a quasi block
        :my $*MAIN := 'MAIN';

        # performance improvement stuff
        :my $*FAKE_INFIX_FOUND := 0;

        # Setting loading and symbol setup.
        {
            # Create unit outer (where we assemble any lexicals accumulated
            # from e.g. REPL) and the real UNIT.
            $*UNIT_OUTER := $*W.push_lexpad($/);
            $*UNIT := $*W.push_lexpad($/);
            
            # If we already have a specified outer context, then that's
            # our setting. Otherwise, load one.
            my $have_outer := nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>);
            if $have_outer {
                $*UNIT.annotate('IN_DECL', 'eval');
            }
            else {
                $*SETTING := $*W.load_setting($/, %*COMPILING<%?OPTIONS><setting> // 'CORE');
                $*UNIT.annotate('IN_DECL', 'mainline');
            }
            $/.CURSOR.unitstart();
            try {
                my $EXPORTHOW := $*W.find_symbol(['EXPORTHOW']);
                for $*W.stash_hash($EXPORTHOW) {
                    %*HOW{$_.key} := $_.value;
                }
            }
            
            # Create GLOBAL(ish), unless we were given one.
            if nqp::existskey(%*COMPILING<%?OPTIONS>, 'global') {
                $*GLOBALish := %*COMPILING<%?OPTIONS><global>;
            }
            elsif $have_outer && $*UNIT_OUTER.symbol('GLOBALish') {
                $*GLOBALish := $*W.force_value($*UNIT_OUTER.symbol('GLOBALish'), 'GLOBALish', 1);
            }
            else {
                $*GLOBALish := $*W.pkg_create_mo($/, %*HOW<package>, :name('GLOBAL'));
                $*W.pkg_compose($*GLOBALish);
            }
                
            # Create or pull in existing EXPORT.
            if $have_outer && $*UNIT_OUTER.symbol('EXPORT') {
                $*EXPORT := $*W.force_value($*UNIT_OUTER.symbol('EXPORT'), 'EXPORT', 1);
            }
            else {
                $*EXPORT := $*W.pkg_create_mo($/, %*HOW<package>, :name('EXPORT'));
                $*W.pkg_compose($*EXPORT);
            }
            
            # If there's a self in scope, set $*HAS_SELF.
            if $have_outer && $*UNIT_OUTER.symbol('self') {
                $*HAS_SELF := 'complete';
            }
                
            # Take current package from outer context if any, otherwise for a
            # fresh compilation unit we start in GLOBAL.
            if $have_outer && $*UNIT_OUTER.symbol('$?PACKAGE') {
                $*PACKAGE := $*W.force_value($*UNIT_OUTER.symbol('$?PACKAGE'), '$?PACKAGE', 1);
            }
            else {
                $*PACKAGE := $*GLOBALish;
            }
            
            # If we're eval'ing in the context of a %?LANG, set up our own
            # %*LANG based on it.
            if $have_outer && $*UNIT_OUTER.symbol('%?LANG') {
                for $*W.force_value($*UNIT_OUTER.symbol('%?LANG'), '%?LANG', 1).FLATTENABLE_HASH() {
                    %*LANG{$_.key} := $_.value;
                }
            }
            if $have_outer && $*UNIT_OUTER.symbol('$*MAIN') {
                $*MAIN := $*W.force_value($*UNIT_OUTER.symbol('$*MAIN'), '$*MAIN', 1);
            }
            
            # Install unless we've no setting, in which case we've likely no
            # static lexpad class yet either. Also, UNIT needs a code object.
            unless %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
                $*W.install_lexical_symbol($*UNIT, 'GLOBALish', $*GLOBALish);
                $*W.install_lexical_symbol($*UNIT, 'EXPORT', $*EXPORT);
                $*W.install_lexical_symbol($*UNIT, '$?PACKAGE', $*PACKAGE);
                $*W.install_lexical_symbol($*UNIT, '::?PACKAGE', $*PACKAGE);
                $*DECLARAND := $*W.stub_code_object('Block');
            }
            my $M := %*COMPILING<%?OPTIONS><M>;
            if nqp::defined($M) {
                for nqp::islist($M) ?? $M !! [$M] -> $longname {
                    my $module := $*W.load_module($/, $longname, {}, $*GLOBALish);
                    do_import($/, $module, $longname);
                    $/.CURSOR.import_EXPORTHOW($module);
                }
            }
        }
        
        <.finishpad>
        <.bom>?
        <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>

        <.install_doc_phaser>
        
        [ $ || <.typed_panic: 'X::Syntax::Confused'> ]
        
        {
            # Emit any errors/worries.
            self.explain_mystery();
            if @*SORROWS {
                if +@*SORROWS == 1 && !@*WORRIES {
                    @*SORROWS[0].throw()
                }
                else {
                    $*W.group_exception(@*SORROWS.pop).throw();
                }
            }
            if @*WORRIES {
                nqp::printfh(nqp::getstderr(), $*W.group_exception().gist());
            }
        
            # Install POD-related variables.
            $*POD_PAST := $*W.add_constant(
                'Array', 'type_new', |$*POD_BLOCKS
            );
            $*W.install_lexical_symbol(
                $*UNIT, '$=pod', $*POD_PAST.compile_time_value
            );
            
            # Tag UNIT with a magical lexical. Also if we're compiling CORE,
            # give it such a tag too.
            if %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
                my $marker := $*W.pkg_create_mo($/, %*HOW<package>, :name('!CORE_MARKER'));
                $marker.HOW.compose($marker);
                $*W.install_lexical_symbol($*UNIT, '!CORE_MARKER', $marker);
            }
            else {
                my $marker := $*W.pkg_create_mo($/, %*HOW<package>, :name('!UNIT_MARKER'));
                $marker.HOW.compose($marker);
                $*W.install_lexical_symbol($*UNIT, '!UNIT_MARKER', $marker);
            }
        }
        
        # CHECK time.
        { $*W.CHECK(); }
    }
    
    method import_EXPORTHOW($UNIT) {    
        # See if we've exported any HOWs.
        if nqp::existskey($UNIT, 'EXPORTHOW') {
            for $*W.stash_hash($UNIT<EXPORTHOW>) {
                %*HOW{$_.key} := nqp::decont($_.value);
            }
        }
    }

    rule statementlist($*statement_level = 0) {
        :my %*LANG := self.shallow_copy(nqp::getlexdyn('%*LANG'));
        :my %*HOW  := self.shallow_copy(nqp::getlexdyn('%*HOW'));
        :dba('statement list')
        ''
        [
        | $
        | <?before <[\)\]\}]>>
        | [ <statement> <.eat_terminator> ]*
        ]
    }

    method shallow_copy(%hash) {
        my %result;
        for %hash {
            %result{$_.key} := $_.value;
        }
        %result
    }

    rule semilist {
        :dba('lol composer')
        ''
        [
        | <?before <[)\]}]> >
        | [<statement><.eat_terminator> ]*
        ]
    }

    rule sequence {
        :dba('sequence of statements')
        ''
        [
        | <?before <[)\]}]> >
        | [<statement><.eat_terminator> ]*
        ]
    }

    token label {
        <identifier> ':' <?[\s]> <.ws>
        {
            $*LABEL       := ~$<identifier>;
            my $total     := nqp::chars(self.orig());
            my $from      := self.MATCH.from();
            my $to        := self.MATCH.to() + nqp::chars($*LABEL);
            my $line      := HLL::Compiler.lineof(self.orig(), self.from());
            my $prematch  := nqp::substr(self.orig(), $from > 20 ?? $from - 20 !! 0, 20);
            my $postmatch := nqp::substr(self.orig(), $to, 20);
            my $label     := $*W.find_symbol(['Label']).new( :name($*LABEL), :$line, :$prematch, :$postmatch );
            $*W.add_object($label);
            $*W.install_lexical_symbol($*W.cur_lexpad(), $*LABEL, $label);
        }
    }

    token statement($*LABEL = '') {
        :my $*QSIGIL := '';
        :my $*SCOPE := '';
        :my $*ACTIONS := %*LANG<MAIN-actions>;
        <!before <[\])}]> | $ >
        <!stopper>
        <!!{ nqp::rebless($/.CURSOR, %*LANG<MAIN>) }>
        [
        | <label> <statement($*LABEL)> { $*LABEL := '' if $*LABEL }
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
                        $/.CURSOR.obs("do..." ~ $s, "repeat...while or repeat...until");
                    }
                }
            ]?
        | <?[;]>
        | <?stopper>
        | {} <.panic: "Bogus statement">
        ]
    }

    token eat_terminator {
        || ';'
        || <?MARKED('endstmt')> <.ws>
        || <?before ')' | ']' | '}' >
        || $
        || <?stopper>
        || { $/.CURSOR.typed_panic( 'X::Syntax::Confused', reason => "Missing semicolon." ) }
    }

    token xblock($*IMPLICIT = 0) {
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR> <.ws> <pblock($*IMPLICIT)>
    }

    token pblock($*IMPLICIT = 0) {
        :my $*DECLARAND := $*W.stub_code_object('Block');
        :dba('parameterized block')
        [
        | <lambda>
            <.newpad>
            :my $*SCOPE := 'my';
            <signature>
            <blockoid>
        | <?[{]>
            <.newpad>
            <blockoid>
        || {
               if nqp::ishash($*BORG) && $*BORG<block> {
                   my $pos := $/.CURSOR.pos;
                   if $*BORG<name> {
                        $/.CURSOR.'!clear_highwater'();
                        $/.CURSOR.'!cursor_pos'($*BORG<block>.CURSOR.pos);
                        $/.CURSOR.typed_sorry('X::Syntax::BlockGobbled', what => ~$*BORG<name>);
                        $/.CURSOR.'!cursor_pos'($pos);
                        $/.CURSOR.missing("block (apparently taken by '" ~ $*BORG<name> ~ "')");
                   } else {
                        $/.CURSOR.'!clear_highwater'();
                        $/.CURSOR.'!cursor_pos'($*BORG<block>.CURSOR.pos);
                        $/.CURSOR.typed_sorry('X::Syntax::BlockGobbled');
                        $/.CURSOR.'!cursor_pos'($pos);
                        $/.CURSOR.missing("block (apparently taken by expression)");
                   }
               } elsif %*MYSTERY {
                   $/.CURSOR.missing("block (taken by some undeclared routine?)");
               } else {
                   $/.CURSOR.missing("block");
               }
           }
        ]
    }

    token lambda { '->' | '<->' }

    token block($*IMPLICIT = 0) {
        :my $*DECLARAND := $*W.stub_code_object('Block');
        :dba('scoped block')
        [ <?[{]> || <.missing: 'block'>]
        <.newpad>
        <blockoid>
    }

    token blockoid {
        :my $*CURPAD;
        :my %*HANDLERS;
        <.finishpad>
        [
        | '{YOU_ARE_HERE}' <you_are_here>
        | :dba('block') '{' ~ '}' <statementlist(1)> <?ENDSTMT>
        | <?terminator> { $*W.throw($/, 'X::Syntax::Missing', what =>'block') }
        | <?> { $*W.throw($/, 'X::Syntax::Missing', what => 'block') }
        ]
        { $*CURPAD := $*W.pop_lexpad() }
    }

    token unitstart { <?> }
    token you_are_here { <?> }
    token newpad { <?> { $*W.push_lexpad($/) } }
    token finishpad { <?> }

    token bom { \xFEFF }

    proto token terminator { <...> }

    token terminator:sym<;> { <?[;]> }
    token terminator:sym<)> { <?[)]> }
    token terminator:sym<]> { <?[\]]> }
    token terminator:sym<}> { <?[}]> }
    token terminator:sym<ang> { <?[>]> <?{ $*IN_REGEX_ASSERTION }> }
    token terminator:sym<if>     { 'if'     <.end_keyword> }
    token terminator:sym<unless> { 'unless' <.end_keyword> }
    token terminator:sym<while>  { 'while'  <.end_keyword> }
    token terminator:sym<until>  { 'until'  <.end_keyword> }
    token terminator:sym<for>    { 'for'    <.end_keyword> }
    token terminator:sym<given>  { 'given'  <.end_keyword> }
    token terminator:sym<when>   { 'when'   <.end_keyword> }
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

    ## Statement control

    proto rule statement_control { <...> }

    rule statement_control:sym<if> {
        <sym><.end_keyword> {}
        <xblock>
        [
            [
            | 'else'\h*'if' <.typed_panic: 'X::Syntax::Malformed::Elsif'>
            | 'elif' { $/.CURSOR.typed_panic('X::Syntax::Malformed::Elsif', what => "elif") }
            | 'elsif'\s <xblock>
            ]
        ]*
        [ 'else'\s <else=.pblock> ]?
    }

    rule statement_control:sym<unless> {
        <sym><.end_keyword> {}
        <xblock>
        [ <!before 'else'> || <.typed_panic: 'X::Syntax::UnlessElse'> ]
    }

    rule statement_control:sym<while> {
        $<sym>=[while|until]<.end_keyword> {}
        <xblock>
    }

    rule statement_control:sym<repeat> {
        <sym><.end_keyword> {}
        [
        | $<wu>=[while|until]\s <xblock>
        | <pblock>
          [$<wu>=['while'|'until']\s || <.missing('"while" or "until"')>]
          <EXPR>
        ]
    }

    rule statement_control:sym<for> {
        <sym><.end_keyword> {}
        [ <?before 'my'? '$'\w+ '(' >
            <.typed_panic: 'X::Syntax::P5'> ]?
        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
            <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
        <xblock(1)>
    }

    rule statement_control:sym<foreach> {
        <sym><.end_keyword> <.obs("'foreach'", "'for'")>
    }

    token statement_control:sym<loop> {
        <sym><.end_keyword>
        [ <?[({]> <.sorry: "Whitespace required after 'loop'"> ]?
        :s''
        [ '('
            <e1=.EXPR>? ';'
            <e2=.EXPR>? ';'
            <e3=.EXPR>?
        ')' ]?
        <block>
    }

    rule statement_control:sym<need> {
        <sym>
        [
        | <version>
        | <module_name>
        ] +% ','
        {
            for $<module_name> {
                my $lnd  := $*W.dissect_longname($_<longname>);
                my $name := $lnd.name;
                my %cp   := $lnd.colonpairs_hash('need');
                $*W.load_module($/, $name, %cp, $*GLOBALish);
            }
        }
    }

    token statement_control:sym<import> {
        <sym> <.ws>
        <module_name> [ <.spacey> <arglist> ]? <.ws>
        :my $*HAS_SELF := '';
        {
            my $longname := $*W.dissect_longname($<module_name><longname>);
            my $module;
            my $found := 0;
            try { $module := $*W.find_symbol($longname.components()); $found := 1; }
            if $found {
                # todo: fix arglist
                my $arglist;
                if $<arglist> {
                    $arglist := $*W.compile_time_evaluate($/, $<arglist><EXPR>.ast);
                    $arglist := nqp::getattr($arglist.list.eager,
                            $*W.find_symbol(['List']), '$!items');
                }
                do_import($/, $module.WHO, $longname.name, $arglist);
            }
            else {
                $/.CURSOR.panic("Could not find module " ~ ~$<module_name> ~
                    " to import symbols from");
            }
        }
    }

    token statement_control:sym<use> {
        :my $longname;
        :my $*IN_DECL := 'use';
        :my $*HAS_SELF := '';
        :my $*SCOPE   := 'use';
        :my $OLD_MAIN := ~$*MAIN;
        $<doc>=[ 'DOC' \h+ ]**0..1
        <sym> <.ws>
        [
        | <version> [ <?{ ~$<version><vnum>[0] eq '5' }> {
                        my $module := $*W.load_module($/, 'Perl5', {}, $*GLOBALish);
                        do_import($/, $module, 'Perl5');
                        $/.CURSOR.import_EXPORTHOW($module);
                    } ]?
                    [ <?{ ~$<version><vnum>[0] eq '6' }> {
                        $*MAIN := 'MAIN';
                    } ]?
        | <module_name>
            {
                $longname := $<module_name><longname>;
                
                # Some modules are handled in the actions are just turn on a
                # setting of some kind.
                if $longname.Str eq 'MONKEY_TYPING' {
                    $*MONKEY_TYPING := 1;
                    $longname := "";
                }
                elsif $longname.Str eq 'soft' {
                    # This is an approximation; need to pay attention to argument
                    # list really.
                    $*SOFT := 1;
                    $longname := "";
                }
                elsif $longname.Str eq 'FORBID_PIR' ||
                      $longname.Str eq 'Devel::Trace' ||
                      $longname.Str eq 'fatal' {
                    $longname := "";
                }
            }
            [
            || <.spacey> <arglist> <?{ $<arglist><EXPR> }>
                {
                    my $lnd     := $*W.dissect_longname($longname);
                    my $name    := $lnd.name;
                    my %cp      := $lnd.colonpairs_hash('use');
                    my $arglist := $*W.compile_time_evaluate($/,
                            $<arglist><EXPR>.ast);
                    $arglist    := nqp::getattr($arglist.list.eager,
                            $*W.find_symbol(['List']), '$!items');
                    my $module  := $*W.load_module($/, $name, %cp, $*GLOBALish);
                    do_import($/, $module, $name, $arglist);
                    $/.CURSOR.import_EXPORTHOW($module);
                }
            || { 
                    unless ~$<doc> && !%*COMPILING<%?OPTIONS><doc> {
                        if $longname {
                            my $lnd    := $*W.dissect_longname($longname);
                            my $name   := $lnd.name;
                            my %cp     := $lnd.colonpairs_hash('use');
                            my $module := $*W.load_module($/, $name, %cp, $*GLOBALish);
                            do_import($/, $module, $name);
                            $/.CURSOR.import_EXPORTHOW($module);
                        }
                    }
                }
            ]
        ]
        [ <?{ $*MAIN ne $OLD_MAIN }> <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)> || <?> ]
        <.ws>
    }

    # This is like HLL::Grammar.LANG but it allows to call a token of a Perl 6 level grammar.
    method FOREIGN_LANG($lang, $regex, *@args) {
        if nqp::istype(%*LANG{$lang}, NQPCursor) {
            return self.LANG($lang, $regex, @args)
        }
        else {
            my $Str := $*W.find_symbol(['Str']);
            my $lang_cursor := %*LANG{$lang}.'!cursor_init'($Str.new( :value(self.orig())), :p(self.pos()));
            if self.HOW.traced(self) {
                $lang_cursor.HOW.trace-on($lang_cursor, self.HOW.trace_depth(self));
            }
            my $*ACTIONS := %*LANG{$lang ~ '-actions'};
            my $ret := $lang_cursor."$regex"(|@args);

            # Build up something NQP-levelish we can return.
            my $new := NQPCursor.'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'()));
            my $p6cursor := $*W.find_symbol(['Cursor']);
            nqp::bindattr_i($new, NQPCursor, '$!from',  nqp::getattr_i($ret, $p6cursor, '$!from'));
            nqp::bindattr_i($new, NQPCursor, '$!pos',   nqp::getattr_i($ret, $p6cursor, '$!pos'));
            nqp::bindattr($new,   NQPCursor, '$!name',  nqp::getattr($ret,   $p6cursor, '$!name'));

            my $match := nqp::create(NQPMatch);
            nqp::bindattr($match, NQPMatch, '$!made', nqp::getattr($ret, $p6cursor, '$!made'));
            nqp::bindattr($new, NQPCursor, '$!match', $match);
            $new;
        }
    }

    sub do_import($/, $module, $package_source_name, $arglist?) {
        if nqp::existskey($module, 'EXPORT') {
            my $EXPORT := $*W.stash_hash($module<EXPORT>);
            my @to_import := ['MANDATORY'];
            my @positional_imports := [];
            if nqp::defined($arglist) {
                my $Pair := $*W.find_symbol(['Pair']);
                for $arglist -> $tag {
                    if nqp::istype($tag, $Pair) {
                        $tag := nqp::unbox_s($tag.key);
                        if nqp::existskey($EXPORT, $tag) {
                            $*W.import($/, $*W.stash_hash($EXPORT{$tag}), $package_source_name);
                        }
                        else {
                            nqp::die("Error while importing from '$package_source_name': no such tag '$tag'");
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
                    $*W.import($/, $*W.stash_hash($EXPORT{$tag}), $package_source_name);
                }
            }
            if nqp::existskey($module, '&EXPORT') {
                my $result := $module<&EXPORT>(|@positional_imports);
                my $EnumMap := $*W.find_symbol(['EnumMap']);
                if nqp::istype($result, $EnumMap) {
                    my $storage := $result.hash.FLATTENABLE_HASH();
                    $*W.import($/, $storage, $package_source_name);
                }
                else {
                    nqp::die("&EXPORT sub did not return an EnumMap");
                }
            }
            else {
                if +@positional_imports {
                    nqp::die("Error while importing from '$package_source_name': no EXPORT sub, but you provided positional argument in the 'use' statement");
                }
            }
        }
    }

    rule statement_control:sym<require> {
        <sym>
        [
        | <module_name>
        | <file=.variable>
        | <!sigil> <file=.term>
        ]
        <EXPR>?
    }

    rule statement_control:sym<given> {
        <sym><.end_keyword> <xblock(1)>
    }
    rule statement_control:sym<when> {
        <sym><.end_keyword> <xblock>
    }
    rule statement_control:sym<default> {
        <sym><.end_keyword> <block>
    }

    rule statement_control:sym<CATCH> {<sym> <block(1)> }
    rule statement_control:sym<CONTROL> {<sym> <block(1)> }

    proto token statement_prefix { <...> }
    token statement_prefix:sym<BEGIN>   { <sym> <blorst> }
    token statement_prefix:sym<COMPOSE> { <sym> <blorst> }
    token statement_prefix:sym<TEMP>    { <sym> <blorst> }
    token statement_prefix:sym<CHECK>   { <sym> <blorst> }
    token statement_prefix:sym<INIT>    { <sym> <blorst> }
    token statement_prefix:sym<ENTER>   { <sym> <blorst> }
    token statement_prefix:sym<FIRST>   { <sym> <blorst> }
    
    token statement_prefix:sym<END>   { <sym> <blorst> }
    token statement_prefix:sym<LEAVE> { <sym> <blorst> }
    token statement_prefix:sym<KEEP>  { <sym> <blorst> }
    token statement_prefix:sym<UNDO>  { <sym> <blorst> }
    token statement_prefix:sym<NEXT>  { <sym> <blorst> }
    token statement_prefix:sym<LAST>  { <sym> <blorst> }
    token statement_prefix:sym<PRE>   { <sym> <blorst> }
    token statement_prefix:sym<POST>  { <sym> <blorst> }
    
    token statement_prefix:sym<eager> { <sym> <blorst> }
    token statement_prefix:sym<lazy>  { <sym> <blorst> }
    token statement_prefix:sym<sink>  { <sym> <blorst> }
    token statement_prefix:sym<try>   { <sym> <blorst> }
    token statement_prefix:sym<gather>{ <sym> <blorst> }
    token statement_prefix:sym<once>  { <sym> <blorst> }
    token statement_prefix:sym<do>    { <sym> <blorst> }
    token statement_prefix:sym<DOC>   {
        <sym> \s <.ws> $<phase>=['BEGIN' || 'CHECK' || 'INIT']
        <blorst>
    }

    token blorst {
        \s <.ws> [ <?[{]> <block> | <![;]> <statement> || <.missing: 'block or statement'> ]
    }

    ## Statement modifiers

    proto rule statement_mod_cond { <...> }

    token modifier_expr { <EXPR> }

    rule statement_mod_cond:sym<if>     { <sym> <modifier_expr> }
    rule statement_mod_cond:sym<unless> { <sym> <modifier_expr> }
    rule statement_mod_cond:sym<when>   { <sym> <modifier_expr> }

    proto rule statement_mod_loop { <...> }

    rule statement_mod_loop:sym<while> { <sym> <smexpr=.EXPR> }
    rule statement_mod_loop:sym<until> { <sym> <smexpr=.EXPR> }
    rule statement_mod_loop:sym<for>   { <sym> <smexpr=.EXPR> }
    rule statement_mod_loop:sym<given> { <sym> <smexpr=.EXPR> }

    ## Terms

    token term:sym<fatarrow>           { <fatarrow> }
    token term:sym<colonpair>          { <colonpair> }
    token term:sym<variable>           { <variable> { $*VAR := $<variable> } }
    token term:sym<package_declarator> { <package_declarator> }
    token term:sym<scope_declarator>   { <scope_declarator> }
    token term:sym<routine_declarator> { <routine_declarator> }
    token term:sym<multi_declarator>   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
    token term:sym<regex_declarator>   { <regex_declarator> }
    token term:sym<circumfix>          { <circumfix> }
    token term:sym<statement_prefix>   { <statement_prefix> }
    token term:sym<**>                 { <sym> }
    token term:sym<*>                  { <sym> }
    token term:sym<lambda>             { <?lambda> <pblock> {$*BORG<block> := $<pblock> if nqp::ishash($*BORG)} }
    token term:sym<type_declarator>    { <type_declarator> }
    token term:sym<value>              { <value> }
    token term:sym<unquote>            { '{{{' <?{ $*IN_QUASI }> <statementlist> '}}}' }

    token term:sym<::?IDENT> {
        $<sym> = [ '::?' <identifier> ] »
    }
    
    token infix:sym<lambda> {
        <?before '{' | '->' > <!{ $*IN_META }> {
            my $needparens := 0;
            my $pos := $/.from;
            my $line := HLL::Compiler.lineof($/.orig, $/.from, :cache(1));
            my $lex := $*W.cur_lexpad();
            for 'if', 'unless', 'while', 'until', 'for', 'given', 'when', 'loop', 'sub', 'method' {
                $needparens++ if $_ eq 'loop';
                my $m := %*MYSTERY{$_ ~ '-' ~ $lex.cuid};
                next unless $m;
                my $m_pos  := $m<pos>[nqp::elems($m<pos>) - 1];
                my $m_line := HLL::Compiler.lineof($/.orig, $m_pos, :cache(1));
                if $line - $m_line < 5 {
                    if $m<ctx> eq '(' {
                        $/.CURSOR.'!clear_highwater'();
                        $/.CURSOR.'!cursor_pos'($m_pos);
                        $/.CURSOR.typed_sorry('X::Syntax::KeywordAsFunction',
                                word => $_,
                                :$needparens,
                        );
                        $/.CURSOR.'!cursor_pos'($pos);
                        $/.CURSOR.panic("Unexpected block in infix position (two terms in a row)");
                    }
                    else {
                        $/.CURSOR.'!clear_highwater'();
                        $/.CURSOR.'!cursor_pos'($m_pos);
                        $/.CURSOR.sorry("Word '$_' interpreted as a listop; please use 'do $_' to introduce the statement control word");
                        $/.CURSOR.'!cursor_pos'($pos);
                        $/.CURSOR.panic("Unexpected block in infix position (two terms in a row)");
                    }
                }
            }
        }
        [
        || <!{ $*IN_REDUCE }> {
            $/.CURSOR.panic("Unexpected block in infix position (two terms in a row, or previous statement missing semicolon?)");
        }
        || <!>
        ]
    }
    
    token term:sym<undef> {
        <sym> >> {}
        [ <?before \h*'$/' >
            <.obs('$/ variable as input record separator',
                 "the filehandle's .slurp method")>
        ]?
        [ <?before [ '(' || \h*<sigil><twigil>?\w ] >
            <.obs('undef as a verb', 'undefine function or assignment of Nil')>
        ]?
        <.obs('undef as a value', "something more specific:\n\tAny (the \"whatever\" type object),\n\tan undefined type object such as Int,\n\t:!defined as a matcher,\n\tAny:U as a type constraint,\n\tNil as the absence of a value\n\tor fail() as a failure return\n\t   ")>
    }

    token term:sym<new> {
        'new' \h+ <longname> \h* <![:]> <.obs("C++ constructor syntax", "method call syntax")>
    }

    token fatarrow {
        <key=.identifier> \h* '=>' <.ws> <val=.EXPR('i=')>
    }
    
    token coloncircumfix($front) {
        [
        | '<>' <.worry("Pair with <> really means an empty list, not null string; use :$front" ~ "('') to represent the null string,\n  or :$front" ~ "() to represent the empty list more accurately")>
        | <circumfix>
        ]
    }

    token colonpair {
        :my $*key;
        :my $*value;

        ':'
        :dba('colon pair')
        [
        | '!' [ <identifier> || <.panic: "Malformed False pair; expected identifier"> ]
            [ <[ \[ \( \< \{ ]> {
            $/.CURSOR.typed_panic('X::Syntax::NegatedPair', key => ~$<identifier>) } ]?
            { $*key := $<identifier>.Str; $*value := 0; }
        | $<num> = [\d+] <identifier> [ <?before <[ \[ \( \< \{ ]>> {} <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)> <.circumfix> ]?
            { $*key := $<identifier>.Str; $*value := +$<num>; }
        | <identifier>
            { $*key := $<identifier>.Str; }
            [
            || <.unsp>? :dba('pair value') <coloncircumfix($*key)> { $*value := $<coloncircumfix>; }
            || { $*value := 1; }
            ]
        | :dba('signature') '(' ~ ')' <fakesignature>
        | <coloncircumfix('')>
            { $*key := ""; $*value := $<coloncircumfix>; }
        | <var=.colonpair_variable>
            { $*key := $<var><desigilname>.Str; $*value := $<var>; self.check_variable($*value); }
        ]
    }
    
    token colonpair_variable {
        <sigil> {}
        [
        | <twigil>? <desigilname>
        | $<capvar>='<' <desigilname> '>'
        ]
    }

    proto token special_variable { <...> }

    token special_variable:sym<$!{ }> {
        '$!{' .*? '}'
        <.obs('${ ... } or %! variable', 'smart match against $!')>
    }

    token special_variable:sym<$~> {
        <sym> <?before \s | ',' | '=' <terminator> >
        <.obs('$~ variable', 'Form module')>
    }

    token special_variable:sym<$`> {
        <sym>  <?before \s | ',' | <terminator> >
        <.obs('$` variable', 'explicit pattern before <(')>
    }

    token special_variable:sym<$@> {
        <sym> <!before \w | '(' | <sigil> >
        <.obs('$@ variable as eval error', '$!')>
    }

    # TODO: use actual variable in error message
    token special_variable:sym<$#> {
        <sym>
        [
        || \w+ <.obs('$#variable', '@variable.end')>
        || <.obs('$# variable', '.fmt')>
        ]
    }

    token special_variable:sym<$$> {
        <sym> <!alpha> <?before \s | ',' | <terminator> >
        <.obs('$$ variable', '$*PID')>
    }
    token special_variable:sym<$%> {
        <sym> <!before \w | '(' | <sigil> >
        <.obs('$% variable', 'Form module')>
    }

    # TODO: $^X and other "caret" variables

    token special_variable:sym<$^> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$^ variable', 'Form module')>
    }

    token special_variable:sym<$&> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('$& variable', '$/ or $()')>
    }

    token special_variable:sym<$*> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$* variable', '^^ and $$')>
    }

    token special_variable:sym<$)> {
        <sym> <?{ $*GOAL ne ')' }> <?before \s | ',' | <terminator> >
        <.obs('$) variable', '$*EGID')>
    }

    token special_variable:sym<$-> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$- variable', 'Form module')>
    }

    token special_variable:sym<$=> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$= variable', 'Form module')>
    }

    token special_variable:sym<@+> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<%+> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('%+ variable', '.to method')>
    }

    token special_variable:sym<$+[ ]> {
        '$+['
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<@+[ ]> {
        '@+['
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<@+{ }> {
        '@+{'
        <.obs('%+ variable', '.to method')>
    }

    token special_variable:sym<@-> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<%-> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('%- variable', '.from method')>
    }

    token special_variable:sym<$-[ ]> {
        '$-['
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<@-[ ]> {
        '@-['
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<%-{ }> {
        '@-{'
        <.obs('%- variable', '.from method')>
    }

    token special_variable:sym<$+> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('$+ variable', 'Form module')>
    }

    token special_variable:sym<$[> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$[ variable', 'user-defined array indices')>
    }

    token special_variable:sym<$]> {
        <sym> <?before \s | ',' | <terminator> >
        <.obs('$] variable', '$*PERL_VERSION')>
    }

    token special_variable:sym<$\\> {
        '$\\' <?before \s | ',' | '=' | <terminator> >
        <.obs('$\\ variable', "the filehandle's :ors attribute")>
    }

    token special_variable:sym<$|> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$| variable', ':autoflush on open')>
    }

    token special_variable:sym<$:> {
        <sym> <?[\x20\t\n\],=)}]>
        <.obs('$: variable', 'Form module')>
    }

    token special_variable:sym<$;> {
        <sym> <?before \s | ',' | '=' | <terminator> >
        <.obs('$; variable', 'real multidimensional hashes')>
    }

    token special_variable:sym<$'> { #'
        <sym> <?before \s | ',' | <terminator> >
        <.obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
    }

    # TODO: $"

    token special_variable:sym<$,> {
        <sym> <?before \h* <[ = , ; ? : ! ) \] } ]> >
        <.obs('$, variable', ".join() method")>
    }

    token special_variable:sym['$<'] {
        <sym> <?before \h* <[ = , ; ? : ! ) \] } ]> <!before \S* '>'> >
        <.obs('$< variable', '$*UID')>
    }

    token special_variable:sym«\$>» {
        <sym> {} <?before \s | ',' | <terminator> >
        <.obs('$> variable', '$*EUID')>
    }

    token special_variable:sym<$.> {
        <sym> {} <?before \s | ',' | <terminator> >
        <.obs('$. variable', "the filehandle's .line method")>
    }

    token special_variable:sym<$?> {
        <sym> {} <?before \s | ',' | <terminator> >
        <.obs('$? variable as child error', '$!')>
    }
    
    regex special_variable:sym<${ }> {
        <sigil> '{' {} $<text>=[.*?] '}'
        <?{
            my $sigil := $<sigil>.Str;
            my $text := $<text>.Str;
            my $bad := $sigil ~ '{' ~ $text ~ '}';
            $text := $text - 1 if $text ~~ /^\d+$/ && $text > 0;
            if !($text ~~ /^(\w|\:)+$/) {
                if $*QSIGIL {
                    0
                }
                else {
                    $/.CURSOR.obs($bad, $sigil ~ '(' ~ $text ~ ')');
                }
            }
            elsif $*QSIGIL {
                $/.CURSOR.obs($bad, '{' ~ $sigil ~ $text ~ '}');
            }
            else {
                $/.CURSOR.obs($bad, $sigil ~ $text);
            }
        }>
    }

    token desigilname {
        [
        | <?before <.sigil> <.sigil> > <variable>
        | <?sigil>
            [ <?{ $*IN_DECL }> <.typed_panic: 'X::Syntax::Variable::IndirectDeclaration'> ]?
            <variable> {
                $*VAR := $<variable>;
                self.check_variable($*VAR);
            }
        | <longname>
        ]
    }

    token variable {
        :my $*IN_META := '';
        <?before <sigil> {
            unless $*LEFTSIGIL {
                $*LEFTSIGIL := $<sigil>.Str;
            }
        }> {}
        [
        || '&'
            [
            | :dba('infix noun') '[' ~ ']' <infixish('[]')>
            ]
        ||  [
            | <sigil> <twigil>? <desigilname>
            | <special_variable>
            | <sigil> $<index>=[\d+] [ <?{ $*IN_DECL}> <.typed_panic: "X::Syntax::Variable::Numeric">]?
            | <sigil> <?[<]> [ <?{ $*IN_DECL }> <.typed_panic('X::Syntax::Variable::Match')>]?  <postcircumfix>
            | :dba('contextualizer') <sigil> '(' ~ ')' <sequence> [<?{ $*IN_DECL }> <.panic: "Cannot declare a contextualizer">]?
            | $<sigil>=['$'] $<desigilname>=[<[/_!]>]
            | <sigil> <?{ $*IN_DECL }>
            | <!{ $*QSIGIL }> {
                $/.CURSOR.typed_panic( 'X::Syntax::Perl5Var',
                  name => nqp::substr(~$/.orig, $/.to - 1, 3 ) )
              }
            ]
        ]
        [ <?{ $<twigil> && $<twigil> eq '.' }>
            [ <.unsp> | '\\' | <?> ] <?[(]> <arglist=.postcircumfix>
        ]?
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

    proto token package_declarator { <...> }
    token package_declarator:sym<package> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'package';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<module> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'module';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<class> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'class';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<grammar> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'grammar';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<role> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'role';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<knowhow> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'knowhow';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<native> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'native';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<slang> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'slang';
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <package_def>
    }
    token package_declarator:sym<trusts> {
        <sym> <.ws> <typename>
    }
    rule package_declarator:sym<also> {
        <sym>
        [ <trait>+ || <.panic: "No valid trait found after also"> ]
    }

    rule package_def {
        :my $longname;
        :my $outer := $*W.cur_lexpad();
        :my $*IMPLICIT := 0;
        :my $*DECLARAND;
        :my $*IN_DECL := 'package';
        :my $*HAS_SELF := '';
        :my $*CURPAD;
        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        { $*DECLARATOR_DOCS := '' }
        <.attach_docs>
        
        # Type-object will live in here; also set default REPR (a trait
        # may override this, e.g. is repr('...')).
        :my $*PACKAGE;
        :my %*ATTR_USAGES;
        :my $*REPR;
        
        # Default to our scoped.
        { unless $*SCOPE { $*SCOPE := 'our'; } }
        
        [
            [ <longname> { $longname := $*W.dissect_longname($<longname>); } ]?
            <.newpad>
            
            [ :dba('generic role')
            <?{ ($*PKGDECL//'') eq 'role' }>
            { $*PACKAGE := $*OUTERPACKAGE } # in case signature tries to declare a package
            '[' ~ ']' <signature>
            { $*IN_DECL := ''; }
            ]?
            
            <trait>*
            
            {
                # Unless we're augmenting...
                if $*SCOPE ne 'augment' {
                    # Locate any existing symbol. Note that it's only a match
                    # with "my" if we already have a declaration in this scope.
                    my $exists := 0;
                    my @name := $longname ??
                        $longname.type_name_parts('package name', :decl(1)) !!
                        [];
                    if @name && $*SCOPE ne 'anon' {
                        if @name && $*W.already_declared($*SCOPE, $*OUTERPACKAGE, $outer, @name) {
                            $*PACKAGE := $*W.find_symbol(@name);
                            $exists := 1;
                        }
                    }

                    # If it exists already, then it's either uncomposed (in which
                    # case we just stubbed it), a role (in which case multiple
                    # variants are OK) or else an illegal redecl.
                    if $exists && ($*PKGDECL ne 'role' || !nqp::can($*PACKAGE.HOW, 'configure_punning')) {
                        if $*PKGDECL eq 'role' || $*PACKAGE.HOW.is_composed($*PACKAGE) {
                            $*W.throw($/, ['X', 'Redeclaration'],
                                symbol => $longname.name(),
                            );
                        }
                    }
                    
                    # If it's not a role, or it is a role but one with no name,
                    # then just needs meta-object construction and installation.
                    elsif $*PKGDECL ne 'role' || !@name {
                        # Construct meta-object for this package.
                        my %args;
                        if @name {
                            %args<name> := $longname.name();
                        }
                        if $*REPR ne '' {
                            %args<repr> := $*REPR;
                        }
                        $*PACKAGE := $*W.pkg_create_mo($/, %*HOW{$*PKGDECL}, |%args);
                        
                        # Install it in the symbol table if needed.
                        if @name {
                            $*W.install_package($/, @name, $*SCOPE, $*PKGDECL, $*OUTERPACKAGE, $outer, $*PACKAGE);
                        }
                    }
                    
                    # If it's a named role, a little trickier. We need to make
                    # a parametric role group for it (unless we got one), and
                    # then install it in that.
                    else {
                        # If the group doesn't exist, create it.
                        my $group;
                        if $exists {
                            $group := $*PACKAGE;
                        }
                        else {
                            $group := $*W.pkg_create_mo($/, %*HOW{'role-group'}, :name($longname.name()), :repr($*REPR));
                            $*W.install_package($/, @name, $*SCOPE, $*PKGDECL, $*OUTERPACKAGE, $outer, $group);
                        }

                        # Construct role meta-object with group.
                        sub needs_args($s) {
                            return 0 if !$s;
                            my @params := $s.ast<parameters>;
                            return 0 if nqp::elems(@params) == 0;
                            return nqp::elems(@params) > 1 || !@params[0]<optional>;
                        }
                        $*PACKAGE := $*W.pkg_create_mo($/, %*HOW{$*PKGDECL}, :name($longname.name()),
                            :repr($*REPR), :group($group), :signatured(needs_args($<signature>)));
                    }
                }
                else {
                    # Augment. Ensure we can.
                    if !$*MONKEY_TYPING && $longname.text ne 'Cool' {
                        $/.CURSOR.typed_panic('X::Syntax::Augment::WithoutMonkeyTyping');
                    }
                    elsif !$longname {
                        $*W.throw($/, 'X::Anon::Augment', package-kind => $*PKGDECL);
                    }

                    # Locate type.
                    my @name := 
                      $longname.type_name_parts('package name', :decl(1));
                    my $found;
                    try { $*PACKAGE := $*W.find_symbol(@name); $found := 1 }
                    unless $found {
                        $*W.throw($/, 'X::Augment::NoSuchType',
                            package-kind => $*PKGDECL,
                            package      => $longname.text(),
                        );
                    }
                    unless $*PACKAGE.HOW.archetypes.augmentable {
                        $/.CURSOR.typed_panic('X::Syntax::Augment::Illegal',
                            package      => $longname.text);
                    }
                }
                
                # Install $?PACKAGE, $?ROLE, $?CLASS, and :: variants as needed.
                my $curpad := $*W.cur_lexpad();
                unless $curpad.symbol('$?PACKAGE') {
                    $*W.install_lexical_symbol($curpad, '$?PACKAGE', $*PACKAGE);
                    $*W.install_lexical_symbol($curpad, '::?PACKAGE', $*PACKAGE);
                    if $*PKGDECL eq 'class' || $*PKGDECL eq 'grammar' {
                        $*W.install_lexical_symbol($curpad, '$?CLASS', $*PACKAGE);
                        $*W.install_lexical_symbol($curpad, '::?CLASS', $*PACKAGE);
                    }
                    elsif $*PKGDECL eq 'role' {
                        $*W.install_lexical_symbol($curpad, '$?ROLE', $*PACKAGE);
                        $*W.install_lexical_symbol($curpad, '::?ROLE', $*PACKAGE);
                        $*W.install_lexical_symbol($curpad, '$?CLASS',
                            $*W.pkg_create_mo($/, %*HOW<generic>, :name('$?CLASS')));
                        $*W.install_lexical_symbol($curpad, '::?CLASS',
                            $*W.pkg_create_mo($/, %*HOW<generic>, :name('::?CLASS')));
                    }
                }
                
                # Set declarand as the package.
                $*DECLARAND := $*PACKAGE;

                if $*PRECEDING_DECL_LINE < $*LINE_NO {
                    $*PRECEDING_DECL_LINE := $*LINE_NO;
                    $*PRECEDING_DECL := $*DECLARAND;
                }
                
                # Apply any traits.
                for $<trait> {
                    my $applier := $_.ast;
                    if $applier {
                        $applier($*DECLARAND);
                    }
                }
            }
            
            { nqp::push(@*PACKAGES, $*PACKAGE); }
            [
            || <?[{]> 
                [
                {
                    $*IN_DECL := '';
                    $*begin_compunit := 0;
                }
                <blockoid>
                ]
            
            || ';'
                [
                || <?{ $*begin_compunit }>
                    {
                        unless $longname {
                            $/.CURSOR.panic("Compilation unit cannot be anonymous");
                        }
                        unless $outer =:= $*UNIT {
                            $/.CURSOR.panic("Semicolon form of " ~ $*PKGDECL ~ " definition not allowed in subscope;\n  please use block form");
                        }
                        if $*PKGDECL eq 'package' {
                            $/.CURSOR.panic('This appears to be Perl 5 code. If you intended it to be Perl 6 code, please use a Perl 6 style package block like "package Foo { ... }", or "module Foo; ...".');
                        }
                        $*begin_compunit := 0;
                    }
                    { $*IN_DECL := ''; }
                    <.finishpad>
                    <statementlist(1)>     # whole rest of file, presumably
                    { $*CURPAD := $*W.pop_lexpad() }
                || <.panic("Too late for semicolon form of $*PKGDECL definition")>
                ]
            || <.panic("Unable to parse $*PKGDECL definition")>
            ]
            { nqp::pop(@*PACKAGES); }
        ] || { $/.CURSOR.malformed($*PKGDECL) }
    }

    token declarator {
        [
        # STD.pm6 uses <defterm> here, but we need different 
        # action methods
        | '\\' <identifier> <.ws>
            [ <term_init=initializer> || <.panic("Term definition requires an initializer")> ]
        | <variable_declarator>
          [
          || <?{ $*SCOPE eq 'has' }> <.newpad> <initializer>? { $*ATTR_INIT_BLOCK := $*W.pop_lexpad() }
          || <initializer>?
          ]
        | '(' ~ ')' <signature> <trait>* <.ws> <initializer>?
        | <routine_declarator>
        | <regex_declarator>
        | <type_declarator>
        ]
    }

    rule term:sym<winner> { <sym><.end_keyword> <xblock> }
    rule term:sym<combine>{ <sym><.end_keyword> <xblock> }
    rule statement_control:sym<more>   { <sym><.end_keyword> <xblock(1)> }
    rule statement_control:sym<done>   { <sym><.end_keyword> <xblock(1)> }
    rule statement_control:sym<quit>   { <sym><.end_keyword> <xblock(1)> }
    rule statement_control:sym<wait>   { <sym><.end_keyword> <xblock(1)> }

    proto token multi_declarator { <...> }
    token multi_declarator:sym<multi> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> :my $*MULTINESS := 'multi'; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.malformed('multi')> ]
    }
    token multi_declarator:sym<proto> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> :my $*MULTINESS := 'proto'; :my $*IN_PROTO := 1; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.malformed('proto')> ]
    }
    token multi_declarator:sym<only> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> :my $*MULTINESS := 'only'; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.malformed('only')>]
    }
    token multi_declarator:sym<null> {
        :my $*MULTINESS := '';
        <declarator>
    }

    proto token scope_declarator { <...> }
    token scope_declarator:sym<my>        { <sym> <scoped('my')> }
    token scope_declarator:sym<our>       { <sym> <scoped('our')> }
    token scope_declarator:sym<has>       {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym>
        :my $*HAS_SELF := 'partial';
        :my $*ATTR_INIT_BLOCK;
        <scoped('has')>
    }
    token scope_declarator:sym<augment>   { <sym> <scoped('augment')> }
    token scope_declarator:sym<anon>      { <sym> <scoped('anon')> }
    token scope_declarator:sym<state>     { <sym> <scoped('state')> }
    token scope_declarator:sym<supersede> {
        <sym> <scoped('supersede')> <.NYI('"supersede"')>
    }

    token scoped($*SCOPE) {
        <.end_keyword>
        :dba('scoped declarator')
        [
        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        {
            if $*SCOPE eq 'has' {
                $*DECLARATOR_DOCS := '';
                if $*PRECEDING_DECL_LINE < $*LINE_NO {
                    $*PRECEDING_DECL_LINE := $*LINE_NO;
                    $*PRECEDING_DECL := Mu; # actual declarand comes later, in Actions::declare_variable
                }
                self.attach_docs;
            }
        }
        <.ws>
        [
        | <DECL=declarator>
        | <DECL=regex_declarator>
        | <DECL=package_declarator>
        | [<typename><.ws>]+
          {
            if +$<typename> > 1 {
                $/.CURSOR.NYI('Multiple prefix constraints');
            }
            $*OFTYPE := $<typename>[0];
          }
          <DECL=multi_declarator>
        | <DECL=multi_declarator>
        ] <.ws>
        || <.ws>[<typename><.ws>]* <ident> <?before <.ws> [':'?':'?'=' | ';' | '}' ]> {}
            <.malformed("$*SCOPE (did you mean to declare a sigilless \\{~$<ident>} or \${~$<ident>}?)")>
        || <.ws><typo_typename> <!>
        || <.malformed($*SCOPE)>
        ]
    }

    token variable_declarator {
        :my $*IN_DECL := 'variable';
        :my $var;
        <variable>
        {
            $var := $<variable>.Str;
            $/.CURSOR.add_variable($var);
            $*IN_DECL := '';
        }
        [
            <.unsp>?
            $<shape>=[
            | '(' ~ ')' <signature>
                {
                    my $sigil := nqp::substr($var, 0, 1);
                    if $sigil eq '&' {
                        self.typed_sorry('X::Syntax::Reserved',
                            reserved => '() shape syntax in routine declarations',
                            instead => ' (maybe use :() to declare a longname?)'
                        );
                    }
                    elsif $sigil eq '@' {
                        self.typed_sorry('X::Syntax::Reserved',
                            reserved => '() shape syntax in array declarations');
                    }
                    elsif $sigil eq '%' {
                        self.typed_sorry('X::Syntax::Reserved',
                            reserved => '() shape syntax in hash declarations');
                    }
                    else {
                        self.typed_sorry('X::Syntax::Reserved',
                            reserved => '() shape syntax in variable declarations');
                    }
                }
            | :dba('shape definition') '[' ~ ']' <semilist> <.NYI: "Shaped variable declarations">
            | :dba('shape definition') '{' ~ '}' <semilist>
            | <?[<]> <postcircumfix> <.NYI: "Shaped variable declarations">
            ]+
        ]?
        <.ws>
        
        <trait>*
        <post_constraint>*
    }

    proto token routine_declarator { <...> }
    token routine_declarator:sym<sub> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <routine_def('sub')>
    }
    token routine_declarator:sym<method> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <method_def('method')>
    }
    token routine_declarator:sym<submethod> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <method_def('submethod')>
    }
    token routine_declarator:sym<macro> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym> <.end_keyword> <macro_def()>
    }

    rule routine_def($d) {
        :my $*IN_DECL := $d;
        :my $*METHODTYPE;
        :my $*IMPLICIT := 0;
        :my $*DOC := $*DECLARATOR_DOCS;
        { $*DECLARATOR_DOCS := '' }
        :my $*DOCEE;
        :my $*DECLARAND := $*W.stub_code_object('Sub');
        {
            if $*PRECEDING_DECL_LINE < $*LINE_NO {
                $*PRECEDING_DECL_LINE := $*LINE_NO;
                $*PRECEDING_DECL := $*DECLARAND;
            }
        }
        <.attach_docs>
        <deflongname>?
        {
            if $<deflongname> && $<deflongname><colonpair>[0]<coloncircumfix> -> $cf {
                # It's an (potentially new) operator, circumfix, etc. that we
                # need to tweak into the grammar.
                my $category := $<deflongname><name>.Str;
                my $opname := $cf<circumfix>
                    ?? $*W.colonpair_nibble_to_str($/, $cf<circumfix><nibble>)
                    !! '';
                my $canname := $category ~ ":sym<" ~ $opname ~ ">";
                $/.CURSOR.add_categorical($category, $opname, $canname, $<deflongname>.ast, $*DECLARAND);
            }
        }
        <.newpad>
        [ '(' <multisig> ')' ]?
        <trait>* :!s
        { $*IN_DECL := ''; }
        [
        || <onlystar>
        || <blockoid>
        ]
    }

    rule method_def($d) {
        :my $*IN_DECL := $d;
        :my $*METHODTYPE := $d;
        :my $*HAS_SELF := $d eq 'submethod' ?? 'partial' !! 'complete';
        :my $*DOC := $*DECLARATOR_DOCS;
        { $*DECLARATOR_DOCS := '' }
        :my $*DOCEE;
        :my $*DECLARAND := $*W.stub_code_object($d eq 'submethod' ?? 'Submethod' !! 'Method');
        {
            if $*PRECEDING_DECL_LINE < $*LINE_NO {
                $*PRECEDING_DECL_LINE := $*LINE_NO;
                $*PRECEDING_DECL := $*DECLARAND;
            }
        }
        <.attach_docs>
        [
            <.newpad>
            [
            | $<specials>=[<[ ! ^ ]>?]<longname> [ '(' <multisig> ')' ]? <trait>*
            | '(' <multisig> ')' <trait>*
            | <sigil>'.':!s
                :dba('subscript signature')
                [
                | '(' ~ ')' <multisig>
                | '[' ~ ']' <multisig>
                | '{' ~ '}' <multisig>
                ]:s
                <trait>*
            | <?>
            ]
            { $*IN_DECL := ''; }
            [
            || <onlystar>
            || <blockoid>
            ]
        ] || <.malformed('method')>
    }

    rule macro_def() {
        :my $*IN_DECL := 'macro';
        :my $*IMPLICIT := 0;
        :my $*DOC := $*DECLARATOR_DOCS;
        { $*DECLARATOR_DOCS := '' }
        :my $*DOCEE;
        :my $*DECLARAND := $*W.stub_code_object('Macro');
        <.attach_docs>
        <deflongname>?
        {
            if $<deflongname> && $<deflongname><colonpair>[0]<coloncircumfix> -> $cf {
                # It's an (potentially new) operator, circumfix, etc. that we
                # need to tweak into the grammar.
                my $category := $<deflongname><name>.Str;
                my $opname := $cf<circumfix>
                    ?? $*W.colonpair_nibble_to_str($/, $cf<circumfix><nibble>)
                    !! '';
                my $canname := $category ~ ":sym<" ~ $opname ~ ">";
                $/.CURSOR.add_categorical($category, $opname, $canname, $<deflongname>.ast, $*DECLARAND);
            }
        }
        <.newpad>
        [ '(' <multisig> ')' ]?
        <trait>*
        { $*IN_DECL := ''; }
        [
        || <onlystar>
        || <blockoid>
        ]
    }
    
    token onlystar {
        :my $*CURPAD;
        <?{ $*MULTINESS eq 'proto' }>
        '{' <.ws> '*' <.ws> '}'
        <?ENDSTMT>
        <.finishpad>
        { $*CURPAD := $*W.pop_lexpad() }
    }

    ###########################
    # Captures and Signatures #
    ###########################

    token capterm {
        '\\'
        [
        | '(' <capture>? ')'
        | <?before \S> <termish>
        | {} <.panic: "You can't backslash that">
        ]
    }

    rule capture {
        <EXPR>
    }

    rule param_sep {
        '' $<sep>=[','|':'|';;'|';'] { @*seps.push($<sep>) }
    }

    # XXX Not really implemented yet.
    token multisig {
        :my $*SCOPE := 'my';
        <signature>
    }

    token fakesignature {
        <.newpad>
        <signature>
    }

    token signature {
        :my $*IN_DECL := 'sig';
        :my $*zone := 'posreq';
        :my @*seps := nqp::list();
        <.ws>
        [
        | <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' >
        | [ <parameter> || <.malformed('parameter')> ]
        ]+ % <param_sep>
        <.ws>
        { $*IN_DECL := ''; }
        [ '-->' <.ws> <typename> || '-->' <.ws> <typo_typename> ]?
        { $*LEFTSIGIL := '@'; }
    }

    token parameter {
        # We'll collect parameter information into a hash, then use it to
        # build up the parameter object in the action method
        :my %*PARAM_INFO;
        [
        | <type_constraint>+
            [
            | $<quant>=['**'|'*'] <param_var>
            | $<quant>=['\\'|'|'] <param_var> { nqp::printfh(nqp::getstderr(), "Obsolete use of | or \\ with sigil on param { $<param_var> }\n") }
            | $<quant>=['\\'|'|'] <defterm>?

            | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
            | <?>
            ]
        | $<quant>=['**'|'*'] <param_var>
        | $<quant>=['\\'|'|'] <param_var> { nqp::printfh(nqp::getstderr, "Obsolete use of | or \\ with sigil on param { $<param_var> }\n") }
        | $<quant>=['\\'|'|'] <defterm>?
        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
        | <longname>
            {
                my $name := $*W.dissect_longname($<longname>);
                $*W.throw($/, ['X', 'Parameter', 'InvalidType'],
                    :typename($name.name),
                    :suggestions($*W.suggest_typename($name.name)));
            }
        ]
        <.ws>
        <trait>*
        <post_constraint>*
        <default_value>**0..1

        # enforce zone constraints
        {
            my $kind :=
                $<named_param>                      ?? '*' !!
                $<quant> eq '?' || $<default_value> ?? '?' !!
                $<quant> eq '!'                     ?? '!' !!
                $<quant> ne '' && $<quant> ne '\\'  ?? '*' !!
                                                       '!';
            my $name := %*PARAM_INFO<variable_name> // '';
            if $kind eq '!' {
                if $*zone eq 'posopt' {
                    $/.CURSOR.typed_panic('X::Parameter::WrongOrder', misplaced => 'required', after => 'optional', parameter => $name);
                }
                elsif $*zone eq 'var' {
                    $/.CURSOR.typed_panic('X::Parameter::WrongOrder', misplaced => 'required', after => 'variadic', parameter => $name);
                }
            }
            elsif $kind eq '?' {
                if $*zone  eq 'posreq' {
                        $*zone := 'posopt';
                }
                elsif $*zone eq  'var' {
                    $/.CURSOR.typed_panic('X::Parameter::WrongOrder', misplaced => 'optional positional', after => 'variadic', parameter => $name);
                }
            }
            elsif $kind eq '*' {
                $*zone := 'var';
            }

            %*PARAM_INFO<node> := $/;
        }
    }

    token param_var {
        :dba('formal parameter')
        :my $*DOC := $*DECLARATOR_DOCS; # these get cleared later
        :my $*DOCEE;
        <.attach_docs>
        {
            my $line_no := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
            if $*PRECEDING_DECL_LINE < $line_no {
                $*PRECEDING_DECL_LINE := $line_no;
                my $par_type := $*W.find_symbol(['Parameter']);
                $*PRECEDING_DECL := nqp::create($par_type); # actual declarand comes later, in World::create_parameter
            }
        }
        [
        | '[' ~ ']' <signature>
        | '(' ~ ')' <signature>
        | <sigil> <twigil>?
          [
          || <name=.identifier>
          || <name=.decint> { $*W.throw($/, 'X::Syntax::Variable::Numeric', what => 'parameter') }
          || $<name>=[<[/!]>]
          ]?
        ]
    }

    token named_param {
        :my $*GOAL := ')';
        :dba('named parameter')
        ':'
        [
        | <name=.identifier> '(' <.ws>
            [ <named_param> | <param_var> <.ws> ]
            [ ')' || <.panic: 'Unable to parse named parameter; couldnt find right parenthesis'> ]
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
        | <value>
        | <typename>
        | where <.ws> <EXPR('i=')>
        ]
        <.ws>
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

    proto token regex_declarator { <...> }
    token regex_declarator:sym<rule> {
        <sym>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*METHODTYPE := 'rule';
        :my $*IN_DECL    := 'rule';
        :my $*LINE_NO    := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        {
            %*RX<s> := 1;
            %*RX<r> := 1;
        }
        <regex_def>
    }
    token regex_declarator:sym<token> {
        <sym>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*METHODTYPE := 'token';
        :my $*IN_DECL    := 'token';
        :my $*LINE_NO    := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        {
            %*RX<r> := 1;
        }
        <regex_def>
    }
    token regex_declarator:sym<regex> {
        <sym>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*METHODTYPE := 'regex';
        :my $*IN_DECL    := 'regex';
        :my $*LINE_NO    := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <regex_def>
    }

    rule regex_def {
        <.end_keyword>
        :my $*CURPAD;
        :my $*HAS_SELF := 'complete';
        :my $*DOC := $*DECLARATOR_DOCS;
        { $*DECLARATOR_DOCS := '' }
        :my $*DOCEE;
        :my $*DECLARAND := $*W.stub_code_object('Regex');
        {
            if $*PRECEDING_DECL_LINE < $*LINE_NO {
                $*PRECEDING_DECL_LINE := $*LINE_NO;
                $*PRECEDING_DECL := $*DECLARAND;
            }
        }
        <.attach_docs>
        [
          <deflongname>?
          { if $<deflongname> { %*RX<name> := ~$<deflongname>.ast } }
          { $*IN_DECL := '' }
           <.newpad>
          [ [ ':'?'(' <signature> ')' ] | <trait> ]*
          '{'
          [
          | ['*'|'<...>'|'<*>'] <?{ $*MULTINESS eq 'proto' }> $<onlystar>={1}
          | <nibble(self.quote_lang(%*RX<P5> ?? %*LANG<P5Regex> !! %*LANG<Regex>, '{', '}'))>
          ]
          '}'<?ENDSTMT>
          { $*CURPAD := $*W.pop_lexpad() }
        ] || <.malformed('regex')>
    }

    proto token type_declarator { <...> }

    token type_declarator:sym<enum> {
        :my $*IN_DECL := 'enum';
        :my $*DECLARAND;
        <sym>  <.end_keyword> <.ws>
        [
        | <longname>
            {
                my $longname := $*W.dissect_longname($<longname>);
                my @name := $longname.type_name_parts('enum name', :decl(1));
                if $*W.already_declared($*SCOPE, $*PACKAGE, $*W.cur_lexpad(), @name) {
                    $*W.throw($/, ['X', 'Redeclaration'],
                        symbol => $longname.name(),
                    );
                }
            }
        | <variable>
        | <?>
        ]
        { $*IN_DECL := ''; }
        <.ws>
        <trait>*
        <?[<(«]> <term> <.ws>
    }

    rule type_declarator:sym<subset> {
        <sym><.end_keyword> :my $*IN_DECL := 'subset';
        [
            [
                [
                    <longname>
                    {
                        my $longname := $*W.dissect_longname($<longname>);
                        my @name := $longname.type_name_parts('subset name', :decl(1));
                        if $*W.already_declared($*SCOPE, $*PACKAGE, $*W.cur_lexpad(), @name) {
                            $*W.throw($/, ['X', 'Redeclaration'],
                                symbol => $longname.name(),
                            );
                        }
                    }
                ]?
                { $*IN_DECL := '' }
                <trait>*
                [ where <EXPR('e=')> ]?
            ]
            || <.malformed('subset')>
        ]
    }

    token type_declarator:sym<constant> {
        :my $*IN_DECL := 'constant';
        <sym> <.end_keyword> <.ws>

        [
        | '\\'? <defterm>
        | <variable>
        | <?>
        ]
        { $*IN_DECL := ''; }
        <.ws>

        <trait>*

        { $*W.push_lexpad($/) }
        [
        || <initializer>
        || <.missing: "initializer on constant declaration">
        ]
    }

    proto token initializer { <...> }
    token initializer:sym<=> {
        <sym>
        [
            <.ws>
            [
            || <?{ $*LEFTSIGIL eq '$' }> <EXPR('i=')>
            || <EXPR('e=')>
            ]
            || <.malformed: 'initializer'>
        ]
    }
    token initializer:sym<:=> {
        <sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
    }
    token initializer:sym<::=> {
        <sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
    }
    token initializer:sym<.=> {
        <sym> [ <.ws> <dottyopish> || <.malformed: 'mutator method call'> ]
    }

    rule trait {
        :my $*IN_DECL := '';
        [
        | <trait_mod>
        | <colonpair>
        ]
    }

    proto rule trait_mod { <...> }
    rule trait_mod:sym<is>      { <sym> <longname><circumfix>**0..1 }
    rule trait_mod:sym<hides>   { <sym> <typename> }
    rule trait_mod:sym<does>    { <sym> <typename> }
    rule trait_mod:sym<will>    { <sym> <identifier> <pblock> }
    rule trait_mod:sym<of>      { <sym> <typename> }
    rule trait_mod:sym<as>      { <sym> <typename> }
    rule trait_mod:sym<returns> { <sym> <typename> }
    rule trait_mod:sym<handles> { <sym> <term> }


    ## Terms

    proto token term { <...> }

    token term:sym<self> {
        <sym> <.end_keyword>
        {
            $*HAS_SELF || self.typed_sorry('X::Syntax::Self::WithoutObject')
        }
    }

    token term:sym<now> { <sym> <.end_keyword> }

    token term:sym<time> { <sym> <.end_keyword> }

    token term:sym<empty_set> { "∅" <!before <[ \( \\ ' \- ]> || \h* '=>'> }

    token term:sym<rand> {
        <sym> »
        [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand or (1..N).pick')> ]?
        [ <?before '()'> <.obs('rand()', 'rand')> ]?
        <.end_keyword>
    }

    token term:sym<...> { <sym> <args> }
    token term:sym<???> { <sym> <args> }
    token term:sym<!!!> { <sym> <args> }

    token term:sym<identifier> {
        :my $pos;
        <identifier> <!{ $*W.is_type([~$<identifier>]) }> <?before <.unsp>|'('> <![:]>
        { $pos := $/.CURSOR.pos }
        <args>
        { self.add_mystery($<identifier>, $<args>.from, nqp::substr(~$<args>, 0, 1)) }
        {
            if nqp::ishash($*BORG) && $*BORG<block> {
                unless $*BORG<name> {
                    $*BORG<name> := ~$<identifier>;
                }
            }
        }
    }
    
    token term:sym<pir::op> {
        'pir::' $<op>=[\w+] <args>?
    }

    token term:sym<pir::const> {
        'pir::const::' $<const>=[\w+]
    }

    token term:sym<nqp::op> {
        'nqp::' $<op>=[\w+] <args>?
    }

    token term:sym<nqp::const> {
        'nqp::const::' $<const>=[\w+]
    }

    token term:sym<name> {
        <longname>
        :my $*longname;
        :my $pos;
        { $*longname := $*W.dissect_longname($<longname>); $pos := $/.CURSOR.pos }
        [
        ||  <?{ nqp::substr($<longname>.Str, 0, 2) eq '::' || $*W.is_name($*longname.components()) }>
            <.unsp>?
            [
                <?{ $*W.is_type($*longname.components()) }>
                <?[[]> :dba('type parameter') '[' ~ ']' <arglist>
            ]?
        || <args> { self.add_mystery($<longname>, $<args>.from, 'termish')
                        unless nqp::index($<longname>.Str, '::') >= 0 }
           {
               if nqp::ishash($*BORG) && $*BORG<block> {
                   unless $*BORG<name> {
                       $*BORG<name> := $*BORG<name> // ~$<longname>;
                   }
               }
           }
        ]
    }

    token term:sym<dotty> { <dotty> }

    token term:sym<capterm> { <capterm> }
    
    token term:sym<onlystar> {
        '{*}' <?ENDSTMT>
        [ <?{ $*IN_PROTO }> || <.panic: '{*} may only appear in proto'> ]
    }

    token args {
        :my $*GOAL := '';
        :my $*FAKE_INFIX_FOUND := 0;
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
        | 'Inf' >>
        | '+Inf' >>
        | '-Inf' >>
        ]
    }

    token dec_number {
        :dba('decimal number')
        [
        | $<coeff> = [               '.' <frac=.decint> ] <escale>?
        | $<coeff> = [ <int=.decint> '.' <frac=.decint> ] <escale>?
        | $<coeff> = [ <int=.decint>                    ] <escale>
        ]
    }
    
    token integer {
        [
        | 0 [ b '_'? <VALUE=binint>
            | o '_'? <VALUE=octint>
            | x '_'? <VALUE=hexint>
            | d '_'? <VALUE=decint>
            | <VALUE=decint>
                <!!{ $/.CURSOR.worry("Leading 0 does not indicate octal in Perl 6; please use 0o" ~ $<VALUE>.Str ~ " if you mean that") }>
            ]
        | <VALUE=decint>
        ]
        <!!before ['.' <?before \s | ',' | '=' | <terminator> > <.sorry: "Decimal point must be followed by digit">]? >
        [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
    }

    token rad_number {
        ':' $<radix> = [\d+] <.unsp>?
        {}           # don't recurse in lexer
        :dba('number in radix notation')
        [
        || '<'
                $<intpart> = [ <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]
                $<fracpart> = [ '.' <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]**0..1
                [ '*' <base=.radint> '**' <exp=.radint> ]**0..1
           '>'
        || <?[[]> <bracket=circumfix>
        || <?[(]> <circumfix>
        || <.malformed: 'radix number'>
        ]
    }

    token radint {
        [
        | <integer>
        # | <?before ':'\d> <rad_number> <?{
        #                         defined $<rad_number><intpart>
        #                         and
        #                         not defined $<rad_number><fracpart>
        #                     }>
        ]
    }

    token escale { <[Ee]> $<sign>=[<[+\-]>?] <decint> }

    token typename {
        [
        | '::?'<identifier> <colonpair>*    # parse ::?CLASS as special case
        | <longname>
          <?{
            my $longname := $*W.dissect_longname($<longname>);
            nqp::substr(~$<longname>, 0, 2) eq '::' ??
                1 !! # ::T introduces a type, so always is one
                $*W.is_name($longname.type_name_parts('type name'))
          }>
        ]
        # parametric type?
        <.unsp>? [ <?[[]> '[' ~ ']' <arglist> ]**0..1
        <.unsp>? [ <?[(]> '(' ~ ')' <arglist> <.NYI("coercive type declarations")>]**0..1
        [<.ws> 'of' <.ws> <typename> ]?
    }

    token typo_typename {
        <longname>
        {
          my $longname := $*W.dissect_longname($<longname>);
          my @suggestions := $*W.suggest_typename($longname.name);
          $/.CURSOR.typed_sorry('X::Undeclared',
                    what => "Type",
                    symbol => $longname.name(),
                    suggestions => @suggestions);
        }
    }


    token quotepair($*purpose = 'quoteadverb') {
        :my $*key;
        :my $*value;
        ':'
        :dba('colon pair (restricted)')
        [
        | '!' <identifier> [ <?[(]> <.sorry('Argument not allowed on negated pair')> ]?
            { $*key := ~$<identifier>; $*value := 0; }
        | <identifier> 
            { $*key := ~$<identifier> }
            [
            || <?[(]> <circumfix> { $*value := $<circumfix>.ast; }
            || { $*value := 1; }
            ]
        | (\d+) <identifier>
            [ <?[(]> <.circumfix> <.sorry('2nd argument not allowed on pair')> ]?
            { $*key := ~$<identifier>; $*value := +~$/[0] }
        ]
    }

    token rx_adverbs {
        [
            <quotepair('rxadverb')> <.ws>
            :my $*ADVERB;
            { $*ADVERB := $<quotepair>[-1] }
            <.setup_quotepair>
        ]*
    }
    
    proto token quote_mod   {*}
    token quote_mod:sym<w>  { <sym> }
    token quote_mod:sym<ww> { <sym> }
    # XXX uncomment this when it's implemented
    #token quote_mod:sym<p>  { <sym> }
    token quote_mod:sym<x>  { <sym> }
    token quote_mod:sym<to> { <sym> }
    token quote_mod:sym<s>  { <sym> }
    token quote_mod:sym<a>  { <sym> }
    token quote_mod:sym<h>  { <sym> }
    token quote_mod:sym<f>  { <sym> }
    token quote_mod:sym<c>  { <sym> }
    token quote_mod:sym<b>  { <sym> }

    proto token quote { <...> }
    token quote:sym<apos>  { :dba('single quotes') "'" ~ "'" <nibble(self.quote_lang(%*LANG<Q>, "'", "'", ['q']))> }
    token quote:sym<dblq>  { :dba('double quotes') '"' ~ '"' <nibble(self.quote_lang(%*LANG<Q>, '"', '"', ['qq']))> }
    token quote:sym<crnr>  { :dba('corner quotes') '｢' ~ '｣' <nibble(self.quote_lang(%*LANG<Q>, '｢', '｣'))> }
    token quote:sym<q> {
        :my $qm;
        'q'
        [
        | <quote_mod> » <![(]> { $qm := $<quote_mod>.Str } <quibble(%*LANG<Q>, 'q', $qm)>
        | » <![(]> <.ws> <quibble(%*LANG<Q>, 'q')>
        ]
    }
    token quote:sym<qq> {
        :my $qm;
        'qq'
        [
        | <quote_mod> » <![(]> { $qm := $<quote_mod>.Str } <.ws> <quibble(%*LANG<Q>, 'qq', $qm)>
        | » <![(]> <.ws> <quibble(%*LANG<Q>, 'qq')>
        ]
    }
    token quote:sym<Q> {
        :my $qm;
        'Q'
        [
        | <quote_mod> » <![(]> { $qm := $<quote_mod>.Str } <quibble(%*LANG<Q>, $qm)>
        | » <![(]> <.ws> <quibble(%*LANG<Q>)>
        ]
    }
    token quote:sym<Q:PIR> { 'Q:PIR' <.ws> <quibble(%*LANG<Q>)> }
    
    token quote:sym</null/> { '/' \s* '/' <.typed_panic: "X::Syntax::Regex::NullRegex"> }
    token quote:sym</ />  {
        :my %*RX;
        :my $*INTERPOLATE := 1;
        '/' <nibble(self.quote_lang(%*LANG<Regex>, '/', '/'))> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
        <.old_rx_mods>?
    }
    token quote:sym<rx>   {
        <sym> >> 
        :my %*RX;
        :my $*INTERPOLATE := 1;
        <rx_adverbs>
        <quibble(%*RX<P5> ?? %*LANG<P5Regex> !! %*LANG<Regex>)>
        <!old_rx_mods>
    }

    token quote:sym<m> {
        <sym> (s)**0..1>>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        { %*RX<s> := 1 if $/[0] }
        <rx_adverbs>
        <quibble(%*RX<P5> ?? %*LANG<P5Regex> !! %*LANG<Regex>)>
        <!old_rx_mods>
    }

    token quote:sym<qr> {
        <sym> <.end_keyword> <.obs('qr for regex quoting', 'rx//')>
    }

    token setup_quotepair { '' }

    token sibble($l, $lang2, @lang2tweaks?) {
        :my $lang;
        :my $start;
        :my $stop;
        <babble($l)>
        { my $B := $<babble><B>.ast; $lang := $B[0]; $start := $B[1]; $stop := $B[2]; }

        $start <left=.nibble($lang)> [ $stop || <.panic("Couldn't find terminator $stop")> ]
        [ <?{ $start ne $stop }>
            <.ws>
            [ <?[ \[ \{ \( \< ]> <.obs('brackets around replacement', 'assignment syntax')> ]?
            [ <infixish> || <.missing: "assignment operator"> ]
            [ <?{ $<infixish>.Str eq '=' }> || <.malformed: "assignment operator"> ]
            # XXX When we support it, above check should add: || $<infixish><infix_postfix_meta_operator>[0]
            <.ws>
            [ <right=.EXPR('i')> || <.panic: "Assignment operator missing its expression"> ]
        ||
            { $lang := self.quote_lang($lang2, $stop, $stop, @lang2tweaks); }
            <right=.nibble($lang)> $stop || <.panic("Malformed replacement part; couldn't find final $stop")>
        ]
    }

    token quote:sym<s> {
        <sym> (s)**0..1 >>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        {
            %*RX<s> := 1 if $/[0]
        }
        <rx_adverbs>
        <sibble(%*RX<P5> ?? %*LANG<P5Regex> !! %*LANG<Regex>, %*LANG<Q>, ['qq'])>
        <.old_rx_mods>?
    }

    token tribble ($l, $lang2 = $l, @lang2tweaks?) {
        :my $lang;
        :my $start;
        :my $stop;
        :my $*CCSTATE := '';
        <babble($l, @lang2tweaks)>
        { my $B := $<babble><B>.ast; $lang := $B[0]; $start := $B[1]; $stop := $B[2]; }

        $start <left=.nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]
        { $*CCSTATE := ''; }
        [ <?{ $start ne $stop }>
            $start <right=.nibble($lang)> [ $stop || { $/.CURSOR.panic("Couldn't find terminator $stop") } ]
        ||
            { $lang := self.quote_lang($lang2, $stop, $stop, @lang2tweaks); }
            <right=.nibble($lang)> $stop || <.panic("Malformed replacement part; couldn't find final $stop")>
        ]
    }

    token quote:sym<tr> {
        <sym>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        <rx_adverbs>
        <tribble(%*LANG<Q>, %*LANG<Q>, ['cc'])>
        <.old_rx_mods>?
    }

    token quote:sym<y> {
        <sym> »
        # could be defined as a function or constant
        <!{ $*W.is_lexical('&y') || $*W.is_lexical('y') }>
        <!before '('> <?before \h*\W>
        <.obs('y///','tr///')>
    }

    token old_rx_mods {
        (<[ i g s m x c e ]>)
        {
            my $m := $/[0].Str;
            if    $m eq 'i' { $/.CURSOR.obs('/i',':i');                                   }
            elsif $m eq 'g' { $/.CURSOR.obs('/g',':g');                                   }
            elsif $m eq 'm' { $/.CURSOR.obs('/m','^^ and $$ anchors');                    }
            elsif $m eq 's' { $/.CURSOR.obs('/s','. or \N');                              }
            elsif $m eq 'x' { $/.CURSOR.obs('/x','normal default whitespace');            }
            elsif $m eq 'c' { $/.CURSOR.obs('/c',':c or :p');                             }
            elsif $m eq 'e' { $/.CURSOR.obs('/e','interpolated {...} or s{} = ... form'); }
            else            { $/.CURSOR.obs('suffix regex modifiers','prefix adverbs');   }
        }
    }

    token quote:sym<quasi> {
        <sym> <.ws> <![(]>
        :my $*IN_QUASI := 1;
        :my @*UNQUOTE_ASTS := [];
        <block>
    }

    token circumfix:sym<SEQ( )> { :dba('statement list') 'SEQ(' ~ ')' <sequence> }

    token circumfix:sym<( )> { :dba('parenthesized expression') '(' ~ ')' <semilist> }
    token circumfix:sym<[ ]> { :dba('array composer') '[' ~ ']' <semilist> }
    token circumfix:sym<ang> {
        :dba('quote words')
        '<' ~ '>'
        [
            [ <?before 'STDIN>' > <.obs('<STDIN>', '$*IN.lines (or add whitespace to suppress warning)')> ]?
            [ <?[>]> <.obs('<>', 'lines() to read input, (\'\') to represent a null string or () to represent an empty list')> ]?
            <nibble(self.quote_lang(%*LANG<Q>, "<", ">", ['q', 'w']))>
        ]
    }
    token circumfix:sym«<< >>» { :dba('shell-quote words') '<<' ~ '>>' <nibble(self.quote_lang(%*LANG<Q>, "<<", ">>", ['qq', 'ww']))> }
    token circumfix:sym<« »> { :dba('shell-quote words') '«' ~ '»' <nibble(self.quote_lang(%*LANG<Q>, "«", "»", ['qq', 'ww']))> }
    token circumfix:sym<{ }> { <?[{]> <pblock(1)> {$*BORG<block> := $<pblock> if nqp::ishash($*BORG)} }

    ## Operators

    INIT {
        Perl6::Grammar.O(':prec<y=>, :assoc<unary>, :dba<methodcall>, :fiddly<1>', '%methodcall');
        Perl6::Grammar.O(':prec<x=>, :assoc<unary>, :dba<autoincrement>', '%autoincrement');
        Perl6::Grammar.O(':prec<w=>, :assoc<right>, :dba<exponentiation>', '%exponentiation');
        Perl6::Grammar.O(':prec<v=>, :assoc<unary>, :dba<symbolic unary>', '%symbolic_unary');
        Perl6::Grammar.O(':prec<u=>, :assoc<left>, :dba<multiplicative>',  '%multiplicative');
        Perl6::Grammar.O(':prec<t=>, :assoc<left>, :dba<additive>',  '%additive');
        Perl6::Grammar.O(':prec<s=>, :assoc<left>, :dba<replication>',  '%replication');
        Perl6::Grammar.O(':prec<r=>, :assoc<left>, :dba<concatenation>',  '%concatenation');
        Perl6::Grammar.O(':prec<q=>, :assoc<list>, :dba<junctive and>', '%junctive_and');
        Perl6::Grammar.O(':prec<p=>, :assoc<list>, :dba<junctive or>', '%junctive_or');
        Perl6::Grammar.O(':prec<o=>, :assoc<unary>, :dba<named unary>', '%named_unary');
        Perl6::Grammar.O(':prec<n=>, :assoc<non>, :dba<structural infix>',  '%structural');
        Perl6::Grammar.O(':prec<m=>, :assoc<left>, :dba<chaining>, :iffy<1>, :pasttype<chain>',  '%chaining');
        Perl6::Grammar.O(':prec<l=>, :assoc<left>, :dba<tight and>',  '%tight_and');
        Perl6::Grammar.O(':prec<k=>, :assoc<list>, :dba<tight or>',  '%tight_or');
        Perl6::Grammar.O(':prec<j=>, :assoc<right>, :dba<conditional>, :fiddly<1>', '%conditional');
        Perl6::Grammar.O(':prec<i=>, :assoc<right>, :dba<item assignment>', '%item_assignment');
        Perl6::Grammar.O(':prec<i=>, :assoc<right>, :dba<list assignment>, :sub<e=>, :fiddly<1>', '%list_assignment');
        Perl6::Grammar.O(':prec<h=>, :assoc<unary>, :dba<loose unary>', '%loose_unary');
        Perl6::Grammar.O(':prec<g=>, :assoc<list>, :dba<comma>, :nextterm<nulltermish>, :fiddly<1>',  '%comma');
        Perl6::Grammar.O(':prec<f=>, :assoc<list>, :dba<list infix>',  '%list_infix');
        Perl6::Grammar.O(':prec<e=>, :assoc<right>, :dba<list prefix>', '%list_prefix');
        Perl6::Grammar.O(':prec<d=>, :assoc<left>, :dba<loose and>',  '%loose_and');
        Perl6::Grammar.O(':prec<c=>, :assoc<list>, :dba<loose or>',  '%loose_or');
        Perl6::Grammar.O(':prec<b=>, :assoc<list>, :dba<sequencer>',  '%sequencer');
    }

    token termish {
        :my $*SCOPE := "";
        :my $*MULTINESS := "";
        :my $*OFTYPE;
        :my $*VAR;
        :dba('prefix or term')
        [
        || <prefixish>* <term>
            :dba('postfix')
            [
            || <?{ $*QSIGIL }>
                [
                || <?{ $*QSIGIL eq '$' }> [ <postfixish>+! <?{ bracket_ending($<postfixish>) }> ]**0..1
                ||                          <postfixish>+! <?{ bracket_ending($<postfixish>) }>
                || { $*VAR := 0 } <!>
                ]
            || <!{ $*QSIGIL }> <postfixish>*
            ]
        || <!{ $*QSIGIL }> <?before <infixish> {
            $/.CURSOR.typed_panic('X::Syntax::InfixInTermPosition', infix => ~$<infixish>); } >
        || <!>
        ]
        { self.check_variable($*VAR) if $*VAR; }
    }

    sub bracket_ending($matches) {
        my $check := $matches[+$matches - 1];
        my $str   := $check.Str;
        my $last  := nqp::substr($str, nqp::chars($check) - 1, 1);
        $last eq ')' || $last eq '}' || $last eq ']' || $last eq '>'
    }

    method EXPR(str $preclim = '') {
        # Override this so we can set $*LEFTSIGIL.
        my $*LEFTSIGIL := '';
        nqp::findmethod(HLL::Grammar, 'EXPR')(self, $preclim, :noinfix($preclim eq 'y='));
    }

    token prefixish { 
        :dba('prefix or meta-prefix')
        [
        | <OPER=prefix>
        | <OPER=prefix_circumfix_meta_operator>
        ]
        <prefix_postfix_meta_operator>**0..1
        <.ws>
    }

    token infixish($in_meta = nqp::getlexdyn('$*IN_META')) {
        :my $*IN_META := $in_meta;
        :my $oper;
        <!stdstopper>
        <!infixstopper>
        :dba('infix or meta-infix')
        [
        | <colonpair> <fake_infix> { $oper := $<fake_infix> }
        |   [
            | :dba('bracketed infix') '[' ~ ']' <infixish('[]')> { $oper := $<infixish><OPER> }
                # XXX Gets false positives.
                #[ <!before '='> { self.worry("Useless use of [] around infix op") unless $*IN_META; } ]?
            | :dba('infixed function') <?before '[&' <twigil>? [<alpha>|'('] > '[' ~ ']' <variable>
                {
                    $<variable><O> := nqp::hash('prec', 't=', 'assoc', 'left', 'dba', 'additive') unless $<variable><O>;
                    $oper := $<variable>;
                    self.check_variable($<variable>)
                }
            | <infix_circumfix_meta_operator> { $oper := $<infix_circumfix_meta_operator> }
            | <infix_prefix_meta_operator> { $oper := $<infix_prefix_meta_operator> }
            | <infix> { $oper := $<infix> }
            ]
            [ <?before '='> <infix_postfix_meta_operator> { $oper := $<infix_postfix_meta_operator> } ]?
        ]
        <OPER=.asOPER($oper)>
    }

    method asOPER($OPER) {
        my $cur  := self.'!cursor_start_cur'();
        $cur.'!cursor_pass'(self.pos());
        nqp::bindattr($cur, NQPCursor, '$!match', $OPER);
        $cur
    }
    
    token fake_infix {
        <O('%item_assignment, :assoc<unary>, :fake<1>, :dba<adverb>')>
        { $*FAKE_INFIX_FOUND := 1 }
    }
    
    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before '!!'> <?{ $*GOAL eq '!!' }>
        | <?before '{' | <lambda> > <?MARKED('ws')> <?{ $*GOAL eq '{' || $*GOAL eq 'endargs' }>
        ]
    }

    token postfixish {
        <!stdstopper>
        
        # last whitespace didn't end here
        <!MARKED('ws')>

        [ <!{ $*QSIGIL }> [ <.unsp> | '\\' ] ]?

        :dba('postfix')
        [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>?]**0..1
        [
        | <OPER=postfix>
        | <OPER=postcircumfix>
        | <OPER=dotty>
        | <OPER=privop>
        ]
        { $*LEFTSIGIL := '@'; }
    }

    token postop {
        | <postfix>         $<O> = {$<postfix><O>} $<sym> = {$<postfix><sym>}
        | <postcircumfix>   $<O> = {$<postcircumfix><O>} $<sym> = {$<postcircumfix><sym>}
    }

    proto token prefix_circumfix_meta_operator { <...> }

    proto token infix_postfix_meta_operator { <...> }

    proto token infix_prefix_meta_operator { <...> }

    proto token infix_circumfix_meta_operator { <...> }

    proto token postfix_prefix_meta_operator { <...> }

    proto token prefix_postfix_meta_operator { <...> }
    
    method can_meta($op, $meta) {
        !$op<OPER><O><fiddly> ||
            self.sorry("Cannot " ~ $meta ~ " " ~ $op<OPER><sym> ~ " because " ~ $op<OPER><O><dba> ~ " operators are too fiddly");
        self;
    }
    
    regex term:sym<reduce> {
        :my $*IN_REDUCE := 1;
        :my $op;
        <?before '['\S+']'>
        
        '['
        [
        || <op=.infixish('red')> <?[\]]>
        || $<triangle>=[\\]<op=.infixish('tri')> <?[\]]>
        || <!>
        ]
        ']'
        { $op := $<op>; }
        
        <.can_meta($op, "reduce with")>

        <args>
    }

    token postfix_prefix_meta_operator:sym<»> {
        [ <sym> | '>>' ] 
        [ <!{ $*QSIGIL }> || <![(]> ]
    }

    token prefix_postfix_meta_operator:sym<«> {
        <sym> | '<<'
    }

    token infix_circumfix_meta_operator:sym<« »> {
        $<opening>=[ '«' | '»' ]
        {} <infixish('hyper')>
        $<closing>=[ '«' | '»' || <.missing("« or »")> ]
        {} <O=.copyO($<infixish>)>
    }

    token infix_circumfix_meta_operator:sym«<< >>» {
        $<opening>=[ '<<' | '>>' ]
        {} <infixish('HYPER')>
        $<closing>=[ '<<' | '>>' || <.missing("<< or >>")> ]
        {} <O=.copyO($<infixish>)>
    }

    method copyO($from) {
        my $O   := $from<OPER><O>;
        my $cur := self.'!cursor_start_cur'();
        $cur.'!cursor_pass'(self.pos());
        nqp::bindattr($cur, NQPCursor, '$!match', $O);
        $cur
    }

    proto token dotty { <...> }
    token dotty:sym<.> {
        <sym> <dottyop>
        <O('%methodcall')>
    }

    token dotty:sym<.*> {
        $<sym>=['.' [ <[+*?=]> | '^' '!'? ]] <dottyop>
        <O('%methodcall')>
    }

    token dottyop {
        :dba('dotty method or postfix')
        <.unsp>?
        [
        | <methodop>
        | <colonpair>
        | <!alpha> <postop> $<O> = {$<postop><O>} $<sym> = {$<postop><sym>}
        ]
    }

    token privop {
        '!' <methodop>
        <O('%methodcall')>
    }

    token methodop {
        [
        | <longname>
        | <?[$@&]> <variable> { self.check_variable($<variable>) }
        | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <-["]>*? [\s|$] > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'."> ]
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

    token dottyopish {
        <term=.dottyop>
    }

    token postcircumfix:sym<[ ]> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '[' ~ ']' [ <.ws> <semilist> ]
        <O('%methodcall')>
    }

    token postcircumfix:sym<{ }> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '{' ~ '}' [ <.ws> <semilist> ]
        <O('%methodcall')>
    }

    token postcircumfix:sym<ang> {
        '<'
        [
        || <nibble(self.quote_lang(%*LANG<Q>, "<", ">", ['q', 'w']))> '>'
        || <?before \h* [ \d | <sigil> | ':' ] >
           { $/.CURSOR.panic("Whitespace required before < operator") }
        || { $/.CURSOR.panic("Unable to parse quote-words subscript; couldn't find right angle quote") }
        ]
        <O('%methodcall')>
    }
    
    token postcircumfix:sym«<< >>» {
        :dba('shell-quote words')
        '<<'
        [
        || <nibble(self.quote_lang(%*LANG<Q>, "<<", ">>", ['qq', 'ww']))> '>>'
        || { $/.CURSOR.panic("Unable to parse quote-words subscript; couldn't find right double-angle quote") }
        ]
        <O('%methodcall')>
    }
    
    token postcircumfix:sym<« »> {
        :dba('shell-quote words')
        '«'
        [
        || <nibble(self.quote_lang(%*LANG<Q>, "«", "»", ['qq', 'ww']))> '»'
        || { $/.CURSOR.panic("Unable to parse quote-words subscript; couldn't find right double-angle quote") }
        ]
        <O('%methodcall')>
    }

    token postcircumfix:sym<( )> {
        :dba('argument list')
        '(' ~ ')' [ <.ws> <arglist> ]
        <O('%methodcall')>
    }

    token postfix:sym<i>  { <sym> >> <O('%methodcall')> }

    token prefix:sym<++>  { <sym>  <O('%autoincrement')> }
    token prefix:sym<-->  { <sym>  <O('%autoincrement')> }
    token postfix:sym<++> { <sym>  <O('%autoincrement')> }
    token postfix:sym<--> { <sym>  <O('%autoincrement')> }

    # TODO: report the correct bracket in error message
    token postfix:sym«->» {
        <sym>
        [
        |  ['[' | '{' | '(' ] <.obs('->(), ->{} or ->[] as postfix dereferencer', '.(), .[] or .{} to deref, or whitespace to delimit a pointy block')>
        | <.obs('-> as postfix', 'either . to call a method, or whitespace to delimit a pointy block')>
        ]
    }

    token infix:sym<**>   { <sym>  <O('%exponentiation')> }

    token prefix:sym<+>   { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<~>   { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<->   { <sym> <![>]> <O('%symbolic_unary')> }
    token prefix:sym<?>   { <sym> <!before '??'> <O('%symbolic_unary')> }
    token prefix:sym<!>   { <sym> <!before '!!'> <O('%symbolic_unary')> }
    token prefix:sym<+^>  { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<~^>  { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<?^>  { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<^>   { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<|>   { <sym>  <O('%symbolic_unary')> }

    token infix:sym<*>    { <sym>  <O('%multiplicative')> }
    token infix:sym</>    { <sym>  <O('%multiplicative')> }
    token infix:sym<div>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<gcd>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<lcm>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<%>    { <sym>  <O('%multiplicative')> }
    token infix:sym<mod>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<%%>   { <sym>  <O('%multiplicative, :iffy<1>')> }
    token infix:sym<+&>   { <sym>  <O('%multiplicative')> }
    token infix:sym<~&>   { <sym>  <O('%multiplicative')> }
    token infix:sym<?&>   { <sym>  <O('%multiplicative')> }
    token infix:sym«+<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O('%multiplicative')> }
    token infix:sym«+>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O('%multiplicative')> }
    token infix:sym«~<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O('%multiplicative')> }
    token infix:sym«~>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O('%multiplicative')> }

    token infix:sym«<<» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('<< to do left shift', '+< or ~<')> <O('%multiplicative')> }

    token infix:sym«>>» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('>> to do right shift', '+> or ~>')> <O('%multiplicative')> }

    token infix:sym<+>    { <sym>  <O('%additive')> }
    token infix:sym<->    {
       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
        <sym> [<?before '>>'> || <![>]>]
        <O('%additive')>
    }
    token infix:sym<+|>   { <sym>  <O('%additive')> }
    token infix:sym<+^>   { <sym>  <O('%additive')> }
    token infix:sym<~|>   { <sym>  <O('%additive')> }
    token infix:sym<~^>   { <sym>  <O('%additive')> }
    token infix:sym<?|>   { <sym>  <O('%additive')> }
    token infix:sym<?^>   { <sym>  <O('%additive')> }

    token infix:sym<x>    { <sym> >> <O('%replication')> }
    token infix:sym<xx>    { <sym> >> <O('%replication')> }

    token infix:sym<~>    { <sym>  <O('%concatenation')> }
    token infix:sym<.>    { <sym> <[\]\)\},:\s\$"']>  <.obs('. to concatenate strings', '~')> }

    token infix:sym<&>   { <sym> <O('%junctive_and')> }
    token infix:sym<(&)> { <sym> <O('%junctive_and')> }
    token infix:sym«∩»   { <sym> <O('%junctive_and')> }
    token infix:sym<(.)> { <sym> <O('%junctive_and')> }
    token infix:sym«⊍»   { <sym> <O('%junctive_and')> }

    token infix:sym<|>    { <sym> <O('%junctive_or')> }
    token infix:sym<^>    { <sym> <O('%junctive_or')> }
    token infix:sym<(|)>  { <sym> <O('%junctive_or')> }
    token infix:sym«∪»    { <sym> <O('%junctive_or')> }
    token infix:sym<(^)>  { <sym> <O('%junctive_or')> }
    token infix:sym«⊖»    { <sym> <O('%junctive_or')> }
    token infix:sym<(+)>  { <sym> <O('%junctive_or')> }
    token infix:sym«⊎»    { <sym> <O('%junctive_or')> }
    token infix:sym<(-)>  { <sym> <O('%junctive_or')> }
    token infix:sym«∖»    { <sym> <O('%junctive_or')> }

    token prefix:sym<let>  { <sym> \s+ <!before '=>'> <O('%named_unary')> { $*W.give_cur_block_let($/) } }
    token prefix:sym<temp> { <sym> \s+ <!before '=>'> <O('%named_unary')> { $*W.give_cur_block_temp($/) } }

    token infix:sym«==»   { <sym>  <O('%chaining')> }
    token infix:sym«!=»   { <sym> <?before \s|']'> <O('%chaining')> }
    token infix:sym«<=»   { <sym>  <O('%chaining')> }
    token infix:sym«>=»   { <sym>  <O('%chaining')> }
    token infix:sym«<»    { <sym>  <O('%chaining')> }
    token infix:sym«>»    { <sym>  <O('%chaining')> }
    token infix:sym«eq»   { <sym> >> <O('%chaining')> }
    token infix:sym«ne»   { <sym> >> <O('%chaining')> }
    token infix:sym«le»   { <sym> >> <O('%chaining')> }
    token infix:sym«ge»   { <sym> >> <O('%chaining')> }
    token infix:sym«lt»   { <sym> >> <O('%chaining')> }
    token infix:sym«gt»   { <sym> >> <O('%chaining')> }
    token infix:sym«=:=»  { <sym>  <O('%chaining')> }
    token infix:sym<===>  { <sym>  <O('%chaining')> }
    token infix:sym<eqv>    { <sym> >> <O('%chaining')> }
    token infix:sym<before> { <sym> >> <O('%chaining')> }
    token infix:sym<after>  { <sym> >> <O('%chaining')> }
    token infix:sym<~~>   { <sym> <O('%chaining')> <!dumbsmart> }
    token infix:sym<!~~>  { <sym> <O('%chaining')> <!dumbsmart> }
    token infix:sym<(elem)> { <sym> <O('%chaining')> }
    token infix:sym«∈»      { <sym> <O('%chaining')> }
    token infix:sym«∉»      { <sym> <O('%chaining')> }
    token infix:sym<(cont)> { <sym> <O('%chaining')> }
    token infix:sym«∋»      { <sym> <O('%chaining')> }
    token infix:sym«∌»      { <sym> <O('%chaining')> }
    token infix:sym«(<)»    { <sym> <O('%chaining')> }
    token infix:sym«⊂»      { <sym> <O('%chaining')> }
    token infix:sym«⊄»      { <sym> <O('%chaining')> }
    token infix:sym«(>)»    { <sym> <O('%chaining')> }
    token infix:sym«⊃»      { <sym> <O('%chaining')> }
    token infix:sym«⊅»      { <sym> <O('%chaining')> }
    token infix:sym«(<=)»   { <sym> <O('%chaining')> }
    token infix:sym«⊆»      { <sym> <O('%chaining')> }
    token infix:sym«⊈»      { <sym> <O('%chaining')> }
    token infix:sym«(>=)»   { <sym> <O('%chaining')> }
    token infix:sym«⊇»      { <sym> <O('%chaining')> }
    token infix:sym«⊉»      { <sym> <O('%chaining')> }
    token infix:sym«(<+)»   { <sym> <O('%chaining')> }
    token infix:sym«≼»      { <sym> <O('%chaining')> }
    token infix:sym«(>+)»   { <sym> <O('%chaining')> }
    token infix:sym«≽»      { <sym> <O('%chaining')> }

    token dumbsmart {
        # should be
        # 'Bool::'? True && <.longname>
        # once && in regexes is implemented
        | <?before \h* [ 'Bool::'? 'True' && <.longname> ] >  <.worry("Smartmatch against True always matches; if you mean to test the topic for truthiness, use :so or *.so or ?* instead")>
        | <?before \h* [ 'Bool::'? 'False' && <.longname> ] > <.worry("Smartmatch against False always fails; if you mean to test the topic for truthiness, use :!so or *.not or !* instead")>
    }

    token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

    token infix:sym<||>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<unless>')> }
    token infix:sym<^^>   { <sym>  <O('%tight_or, :pasttype<xor>')> }
    token infix:sym<//>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<defor>')> }
    token infix:sym<min>  { <sym> >> <O('%tight_or')> }
    token infix:sym<max>  { <sym> >> <O('%tight_or')> }

    token infix:sym<?? !!> {
        :my $*GOAL := '!!';
        '??'
        <.ws>
        <EXPR('i=')>
        [ '!!'
        || <?before '::'<-[=]>> <.panic: "Please use !! rather than ::">
        || <?before ':' <-[=]>> <.panic: "Please use !! rather than :">
        || <?before \N*? [\n\N*?]?> '!!' <.sorry("Bogus code found before the !!")> <.panic("Confused")>
        || <.sorry("Found ?? but no !!")> <.panic("Confused")>
        ]
        <O('%conditional, :reducecheck<ternary>, :pasttype<if>')>
    }

    token infix_prefix_meta_operator:sym<!> {
        <sym> <![!]> {} [ <infixish('neg')> || <.panic: "Negation metaoperator not followed by valid infix"> ]
        [
        || <?{ $<infixish>.Str eq '=' }> <O('%chaining')>
        || <?{ $<infixish><OPER><O><iffy> }> <O=.copyO($<infixish>)>
        || <.panic("Cannot negate " ~ $<infixish>.Str ~ " because it is not iffy enough")>
        ]
    }
    token infix_prefix_meta_operator:sym<R> { <sym> <infixish('R')> {} <O=.copyO($<infixish>)> }
    token infix_prefix_meta_operator:sym<S> { <sym> <infixish('S')> {} <O=.copyO($<infixish>)> }
    token infix_prefix_meta_operator:sym<X> { <sym> <infixish('X')> <O('%list_infix')> }
    token infix_prefix_meta_operator:sym<Z> { <sym> <infixish('Z')> <O('%list_infix')> }
    token infix:sym<minmax> { <sym> >> <O('%list_infix')> }

    token infix:sym<:=> {
        <sym>  <O('%list_assignment')>
    }

    token infix:sym<::=> {
        <sym>  <O('%item_assignment')>
    }

    token infix:sym<.=> { <sym> <O('%item_assignment, :nextterm<dottyopish>')> }

    # Should probably have <!after '='> to agree w/spec, but after NYI.
    # Modified infix != below instead to prevent misparse
    token infix_postfix_meta_operator:sym<=> { '=' <O('%item_assignment')> }

    token infix:sym«=>» { <sym> <O('%item_assignment')> }

    token prefix:sym<so> { <sym> >> <O('%loose_unary')> }
    token prefix:sym<not>  { <sym> >> <O('%loose_unary')> }

    token infix:sym<,>    {
        <.unsp>? <sym> <O('%comma, :fiddly<0>')>
        # TODO: should be <.worry>, not <.panic>
        [ <?before \h*'...'> <.panic: "Comma found before apparent series operator; please remove comma (or put parens\n    around the ... listop, or use 'fail' instead of ...)"> ]?
    }

    token infix:sym<Z>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }
    token infix:sym<X>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }

    token infix:sym<...>  { <sym>  <O('%list_infix')> }
    token infix:sym<...^> { <sym>  <O('%list_infix')> }
    # token term:sym<...>   { <sym> <args>**0..1 <O(|%list_prefix)> }

    token infix:sym<?>    { <sym> {} <![?]> <?before <-[;]>*?':'> <.obs('?: for the conditional operator', '??!!')> <O('%conditional')> }

    token infix:sym<ff> { <sym> <O('%conditional')> }
    token infix:sym<^ff> { <sym> <O('%conditional')> }
    token infix:sym<ff^> { <sym> <O('%conditional')> }
    token infix:sym<^ff^> { <sym> <O('%conditional')> }
    
    token infix:sym<fff> { <sym> <O('%conditional')> }
    token infix:sym<^fff> { <sym> <O('%conditional')> }
    token infix:sym<fff^> { <sym> <O('%conditional')> }
    token infix:sym<^fff^> { <sym> <O('%conditional')> }

    token infix:sym<=> {
        <sym>
        [
        || <?{ $*LEFTSIGIL eq '$' }> <O('%item_assignment')>
        || <O('%list_assignment')>
        ]
    }

    token infix:sym<and>  { <sym> >> <O('%loose_and, :pasttype<if>')> }
    token infix:sym<andthen> { <sym> >> <O('%loose_and, :assoc<list>')> }

    token infix:sym<or>   { <sym> >> <O('%loose_or, :assoc<left>, :pasttype<unless>')> }
    token infix:sym<xor>  { <sym> >> <O('%loose_or, :pasttype<xor>')> }
    token infix:sym<orelse> { <sym> >> <O('%loose_or, :assoc<left>, :pasttype<defor>')> }

    token infix:sym«<==»  { <sym> <O('%sequencer')> }
    token infix:sym«==>»  { <sym> <O('%sequencer')> }
    token infix:sym«<<==» { <sym> <O('%sequencer')> }
    token infix:sym«==>>» { <sym> <O('%sequencer')> }

    token infix:sym<..>   { <sym> [<!{ $*IN_META }> <?[)\]]> <.panic: "Please use ..* for indefinite range">]? <O('%structural')> }
    token infix:sym<^..>  { <sym> <O('%structural')> }
    token infix:sym<..^>  { <sym> <O('%structural')> }
    token infix:sym<^..^> { <sym> <O('%structural')> }

    token infix:sym<leg>  { <sym> >> <O('%structural')> }
    token infix:sym<cmp>  { <sym> >> <O('%structural')> }
    token infix:sym«<=>»  { <sym> <O('%structural')> }

    token infix:sym<but>  { <sym> >> <O('%structural')> }
    token infix:sym<does> { <sym> >> <O('%structural')> }

    token infix:sym<!~> { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> <O('%chaining')> }
    token infix:sym<=~> { <sym> <.obs('=~ to do pattern matching', '~~')> <O('%chaining')> }

    method add_mystery($token, $pos, $ctx) {
        my $name := ~$token;
        unless $name eq '' || $*W.is_lexical('&' ~ $name) || $*W.is_lexical($name) {
            my $lex := $*W.cur_lexpad();
            my $key := $name ~ '-' ~ $lex.cuid;
            if nqp::existskey(%*MYSTERY, $key) {
                nqp::push(%*MYSTERY{$key}<pos>, $pos);
            }
            else {
                %*MYSTERY{$key} := nqp::hash(
                    'lex', $lex,
                    'name', $name,
                    'ctx', $ctx,
                    'pos', [$pos]);
            }
        }
        self;
    }
    
    method explain_mystery() {
        my %post_types;
        my %unk_types;
        my %unk_routines;
        
        sub push_lines(@target, @pos) {
            for @pos {
                nqp::push(@target, HLL::Compiler.lineof(self.orig, $_, :cache(1)));
            }
        }
        
        my %routine_suggestion := hash();
        my %type_suggestion := hash();
        
        for %*MYSTERY {
            my %sym  := $_.value;
            my $name := %sym<name>;
            my $decl := $*W.is_lexically_visible($name, %sym<lex>);
            if $decl == 2 {
                # types may not be post-declared
                %post_types{$name} := [] unless %post_types{$name};
                push_lines(%post_types{$name}, %sym<pos>);
                next;
            }

            next if $decl == 1;
            next if $*W.is_lexically_visible('&' ~ $name, %sym<lex>);

            # no sigil or &
            if $name ge 'a' || nqp::substr($name,0,1) eq '&' {
                %unk_routines{$name} := [] unless %unk_routines{$name};
                my @suggs := $*W.suggest_routines($name);
                %routine_suggestion{$name} := @suggs;
                push_lines(%unk_routines{$name}, %sym<pos>);
            }

            # hopefully improve error reporting
            else {
                %unk_types{$name} := [] unless %unk_types{$name};
                my @suggs := $*W.suggest_typename($name);
                %type_suggestion{$name} := @suggs;
                push_lines(%unk_types{$name}, %sym<pos>);
            }
        }
        
        if %post_types || %unk_types || %unk_routines {
            self.typed_sorry('X::Undeclared::Symbols',
                :%post_types, :%unk_types, :%unk_routines,
                :%routine_suggestion, :%type_suggestion);
        }
        
        self;        
    }
    
    method add_variable($name) {
        my $categorical := $name ~~ /^'&'((\w+)':<'\s*(\S+?)\s*'>')$/;
        if $categorical {
            self.add_categorical(~$categorical[0][0], ~$categorical[0][1], ~$categorical[0], $name);
        }
    }

    # Called when we add a new choice to an existing syntactic category, for
    # example new infix operators add to the infix category. Augments the
    # grammar as needed.
    method add_categorical($category, $opname, $canname, $subname, $declarand?) {
        my $self := self;
        
        # Ensure it's not a null name.
        if $opname eq '' {
            self.typed_panic('X::Syntax::Extension::Null');
        }
        
        # If we already have the required operator in the grammar, just return.
        if nqp::can(self, $canname) {
            return 1;
        }

        # Work out what default precedence we want, or if it's more special than
        # just an operator.
        my $prec;
        my $is_oper;
        my $is_term := 0;
        if $category eq 'infix' {
            $prec := '%additive';
            $is_oper := 1;
        }
        elsif $category eq 'prefix' {
            $prec := '%symbolic_unary';
            $is_oper := 1;
        }
        elsif $category eq 'postfix' {
            $prec := '%autoincrement';
            $is_oper := 1;
        }
        elsif $category eq 'postcircumfix'
           || $category eq 'circumfix' {
            $is_oper := 0;
        }
        elsif $category eq 'trait_mod' {
            return 0;
        }
        elsif $category eq 'term' {
            $is_term := 1;
        }
        elsif $category eq 'METAOP_TEST_ASSIGN' {
            return 0;
        }
        else {
            self.typed_panic('X::Syntax::Extension::Category', :$category);
        }

        if $is_term {
            my role Term[$meth_name, $op] {
                token ::($meth_name) { $<sym>=[$op] }
            }
            self.HOW.mixin(self, Term.HOW.curry(Term, $canname, $opname));
        }
        # Mix an appropraite role into the grammar for parsing the new op.
        elsif $is_oper {
            my role Oper[$meth_name, $op, $precedence, $declarand] {
                token ::($meth_name) { $<sym>=[$op] <O=.genO($precedence, $declarand)> }
            }
            self.HOW.mixin(self, Oper.HOW.curry(Oper, $canname, $opname, $prec, $declarand));
        }
        elsif $category eq 'postcircumfix' {
            # Find opener and closer and parse an EXPR between them.
            # XXX One day semilist would be nice, but right now that
            # runs us into fun with terminators.
            my @parts := nqp::split(' ', $opname);
            if +@parts != 2 {
                self.panic("Unable to identify both starter and stopper from '$opname'\nPerhaps you forgot to separate them with whitespace?");
            }
            my role Postcircumfix[$meth_name, $starter, $stopper] {
                token ::($meth_name) {
                    :my $*GOAL := $stopper;
                    :my $stub := %*LANG<MAIN> := nqp::getlex('$¢').unbalanced($stopper);
                    $starter ~ $stopper [ <.ws> <statement> ]
                    <O('%methodcall')>
                }
            }
            self.HOW.mixin(self, Postcircumfix.HOW.curry(Postcircumfix, $canname, @parts[0], @parts[1]));
        }
        else {
            # Find opener and closer and parse an EXPR between them.
            my @parts := nqp::split(' ', $opname);
            if +@parts != 2 {
                self.panic("Unable to identify both starter and stopper from '$opname'\nPerhaps you forgot to separate them with whitespace?");
            }
            my role Circumfix[$meth_name, $starter, $stopper] {
                token ::($meth_name) {
                    :my $*GOAL := $stopper;
                    :my $stub := %*LANG<MAIN> := nqp::getlex('$¢').unbalanced($stopper);
                    $starter ~ $stopper <semilist>
                }
            }
            self.HOW.mixin(self, Circumfix.HOW.curry(Circumfix, $canname, @parts[0], @parts[1]));
        }

        # This also becomes the current MAIN. Also place it in %?LANG.
        %*LANG<MAIN> := self.WHAT;
        $*W.install_lexical_symbol($*W.cur_lexpad(), '%?LANG', $*W.p6ize_recursive(%*LANG));
        
        # Declarand should get precedence traits.
        if $is_oper && nqp::isconcrete($declarand) {
            my $base_prec := self.O($prec).MATCH<prec>;
            $*W.apply_trait(self.MATCH, '&trait_mod:<is>', $declarand,
                :prec(nqp::hash('prec', $base_prec)));
        }

        # May also need to add to the actions.
        if $category eq 'postcircumfix' {
            my role PostcircumfixAction[$meth, $subname] {
                method ::($meth)($/) {
                    make QAST::Op.new(
                        :op('call'), :name('&' ~ $subname),
                        $<statement>.ast
                    );
                }
            };
            %*LANG<MAIN-actions> := $*ACTIONS.HOW.mixin($*ACTIONS,
                PostcircumfixAction.HOW.curry(PostcircumfixAction, $canname, $subname));
        }
        elsif $category eq 'circumfix' {
            my role CircumfixAction[$meth, $subname] {
                method ::($meth)($/) {
                    make QAST::Op.new(
                        :op('call'), :name('&' ~ $subname),
                        $<semilist>.ast
                    );
                }
            };
            %*LANG<MAIN-actions> := $*ACTIONS.HOW.mixin($*ACTIONS,
                CircumfixAction.HOW.curry(CircumfixAction, $canname, $subname));
        }
        elsif $is_term {
            my role TermAction[$meth, $subname] {
                method ::($meth)($/) {
                    make QAST::Op.new(
                        :op('call'), :name('&' ~ $subname),
                    );
                }
            };
            my role TermActionConstant[$meth, $name] {
                method ::($meth)($/) {
                    make QAST::Var.new( :$name, :scope('lexical') );
                }
            };
            %*LANG<MAIN-actions> := $*ACTIONS.HOW.mixin($*ACTIONS,
                $*IN_DECL eq 'constant'
                    ?? TermAction.HOW.curry(TermActionConstant, $canname, $subname)
                    !! TermAction.HOW.curry(TermAction, $canname, $subname));
        }

        return 1;
    }
    
    method genO($default, $declarand) {
        my $desc := $default;
        if nqp::can($declarand, 'prec') {
            my %extras := $declarand.prec.FLATTENABLE_HASH;
            for %extras {
                $desc := "$desc, :" ~ $_.key ~ "<" ~ $_.value ~ ">";
            }
        }
        self.O($desc)
    }
}

grammar Perl6::QGrammar is HLL::Grammar does STD {

    method throw_unrecog_backslash_seq ($sequence) {
        self.typed_sorry('X::Backslash::UnrecognizedSequence', :$sequence);
    }

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
        token backslash:sym<n> { <sym> }
        token backslash:sym<o> { :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token backslash:sym<r> { <sym> }
        token backslash:sym<t> { <sym> }
        token backslash:sym<x> { :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token backslash:sym<0> { <sym> }
    }

    role b0 {
        token escape:sym<\\> { <!> }
    }

    role c1 {
        token escape:sym<{ }> { <?[{]> <block=.LANG('MAIN','block')> }
    }

    role c0 {
        token escape:sym<{ }> { <!> }
    }

    role s1 {
        token escape:sym<$> {
            :my $*QSIGIL := '$';
            <?[$]>
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
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role f0 {
        token escape:sym<&> { <!> }
    }

    role p1 {
        method postprocessor () { 'path' }
    }

    role p0 {
        method postprocessor () { 'null' }
    }

    role v1 {
        method postprocessor () { 'val' }
    }

    role v0 {
        method postprocessor () { 'null' }
    }

    role w1 {
        method postprocessor () { 'words' }
    }

    role w0 {
        method postprocessor () { 'null' }
    }

    role ww1 {
        method postprocessor () { 'quotewords' }
        token escape:sym<' '> {
            <?[']> <quote=.LANG('MAIN','quote')>
        }
        token escape:sym<" "> {
            <?["]> <quote=.LANG('MAIN','quote')>
        }
        token escape:sym<colonpair> {
            <?[:]> <colonpair=.LANG('MAIN','colonpair')>
        }
    }

    role ww0 {
        method postprocessor () { 'null' }
    }

    role x1 {
        method postprocessor () { 'run' }
    }

    role x0 {
        method postprocessor () { 'null' }
    }
    
    role to[$herelang] {
        method herelang() { $herelang }
        method postprocessor () { 'heredoc' }
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

    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
        token starter { \" }
        token stopper { \" }
        token backslash:sym<unrec> { {} (\w) { self.throw_unrecog_backslash_seq: $/[0].Str } }
        token backslash:sym<misc> { \W }

        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
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
                    my $c := $/.CURSOR;
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
                    my $c := $/.CURSOR;
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
            my $c := $/.CURSOR;
            $to   := $c.pos;
            if $from != $to || !@*nibbles {
                nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
            }
        }
    }
    
    role cc {
        token starter { \' }
        token stopper { \' }

        method ccstate ($s) {
            if $*CCSTATE eq '..' {
                $*CCSTATE := '';
            }
            else {
                $*CCSTATE := $s;
            }
            self;
        }

        # (must not allow anything to match . in nibbler or we'll lose track of state)
        token escape:ws { \s+ [ <?[#]> <.ws> ]? }
        token escape:sym<#> { '#' <.panic: "Please backslash # for literal char or put whitespace in front for comment"> }
        token escape:sym<\\> { <sym> <item=.backslash> <.ccstate('\\' ~ $<item>)> }
        token escape:sym<..> { <sym>
            [
            || <?{ ($*CCSTATE eq '') || ($*CCSTATE eq '..') }> <.sorry("Range missing start character on the left")>
            || <?before \s* <!stopper> <!before '..'> \S >
            || <.sorry("Range missing stop character on the right")>
            ]
            { $*CCSTATE := '..'; }
        }

        token escape:sym<-> {
            '-' <?{ $*CCSTATE ne '' }> \s* <!stopper> \S 
            <.obs('- as character range','.. (or \\- if you mean a literal hyphen)')>
        }
        token escape:ch { $<ch> = [\S] { self.ccstate($<ch>) } }

        token backslash:delim { <text=.starter> | <text=.stopper> }
        token backslash:<\\> { <text=.sym> }
        token backslash:sym<a> { :i <sym> }
        token backslash:sym<b> { :i <sym> }
        token backslash:sym<c> { :i <sym> <charspec> }
        token backslash:sym<d> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<e> { :i <sym> }
        token backslash:sym<f> { :i <sym> }
        token backslash:sym<h> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<n> { :i <sym> }
        token backslash:sym<o> { :i :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token backslash:sym<r> { :i <sym> }
        token backslash:sym<s> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<t> { :i <sym> }
        token backslash:sym<v> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<w> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<x> { :i :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token backslash:sym<0> { <sym> }

        # keep random backslashes like qq does
        token backslash:misc { {}
            [
            | $<text>=(\W)
            | $<x>=(\w) <.sorry("Unrecognized backslash sequence: '\\" ~ $<x> ~ "'")>
            ] 
        }
        multi method tweak_q($v) { self.panic("Too late for :q") }
        multi method tweak_qq($v) { self.panic("Too late for :qq") }
        multi method tweak_cc($v) { self.panic("Too late for :cc") }
    }

    method truly($bool, $opt) {
        self.sorry("Cannot negate $opt adverb") unless $bool;
        self;
    }
    
    method tweak_q($v)          { self.truly($v, ':q'); self.HOW.mixin(self, Perl6::QGrammar::q) }
    method tweak_single($v)     { self.tweak_q($v) }
    method tweak_qq($v)         { self.truly($v, ':qq'); self.HOW.mixin(self, Perl6::QGrammar::qq); }
    method tweak_double($v)     { self.tweak_qq($v) }

    method tweak_b($v)          { self.HOW.mixin(self, $v ?? b1 !! b0) }
    method tweak_backslash($v)  { self.tweak_b($v) }
    method tweak_s($v)          { self.HOW.mixin(self, $v ?? s1 !! s0) }
    method tweak_scalar($v)     { self.tweak_s($v) }
    method tweak_a($v)          { self.HOW.mixin(self, $v ?? a1 !! a0) }
    method tweak_array($v)      { self.tweak_a($v) }
    method tweak_h($v)          { self.HOW.mixin(self, $v ?? h1 !! h0) }
    method tweak_hash($v)       { self.tweak_h($v) }
    method tweak_f($v)          { self.HOW.mixin(self, $v ?? f1 !! f0) }
    method tweak_function($v)   { self.tweak_f($v) }
    method tweak_c($v)          { self.HOW.mixin(self, $v ?? c1 !! c0) }
    method tweak_closure($v)    { self.tweak_c($v) }

    method tweak_x($v)          { self.HOW.mixin(self, $v ?? x1 !! x0) }
    method tweak_exec($v)       { self.tweak_x($v) }
    method tweak_w($v)          { self.HOW.mixin(self, $v ?? w1 !! w0) }
    method tweak_words($v)      { self.tweak_w($v) }
    method tweak_ww($v)         { self.HOW.mixin(self, $v ?? ww1 !! ww0) }
    method tweak_quotewords($v) { self.tweak_ww($v) }

    method tweak_cc($v)         { self.truly($v, ':cc'); self.HOW.mixin(self, cc); }

    method tweak_to($v) {
        self.truly($v, ':to');
        %*LANG<Q>.HOW.mixin(%*LANG<Q>, to.HOW.curry(to, self))
    }
    method tweak_heredoc($v)    { self.tweak_to($v) }

    method tweak_regex($v) {
        self.truly($v, ':regex');
        return %*LANG<Regex>;
    }
}

grammar Perl6::RegexGrammar is QRegex::P6Regex::Grammar does STD {
    method throw_unrecognized_metachar ($metachar) {
        self.typed_sorry('X::Syntax::Regex::UnrecognizedMetachar', :$metachar);
    }
    method throw_null_pattern() {
        self.typed_sorry('X::Syntax::Regex::NullRegex');
    }

    method throw_malformed_range() { self.typed_sorry('X::Syntax::Regex::MalformedRange') }
    method throw_confused() { self.typed_sorry('X::Syntax::Confused') }
    method throw_unspace($char) { self.typed_sorry('X::Syntax::Regex::Unspace', :$char) }
    method throw_regex_not_terminated() { self.typed_sorry('X::Syntax::Regex::Unterminated') }
    method throw_spaces_in_bare_range() { self.typed_sorry('X::Syntax::Regex::SpacesInBareRange') }
    
    token normspace { <?before \s | '#'> <.LANG('MAIN', 'ws')> }

    token rxstopper { <stopper> }

    token metachar:sym<:my> {
        ':' <?before 'my'|'constant'|'state'|'our'> <statement=.LANG('MAIN', 'statement')>
        <.LANG('MAIN', 'eat_terminator')>
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    token metachar:sym<rakvar> {
        <?before <sigil> $<twigil>=[<alpha> | \W<alpha> | '(']>
        <!before <sigil> <rxstopper> >
        <var=.LANG('MAIN', 'variable')>
        [
        || $<binding> = ( \s* '=' \s* <quantified_atom> )
           { self.check_variable($<var>) unless $<twigil> eq '<' }
        || { self.check_variable($<var>) }
           [ <?before '.'? <[ \[ \{ \< ]>> <.worry: "Apparent subscript will be treated as regex"> ]?
        ]
        <.SIGOK>
    }

    token metachar:sym<qw> {
        <?before '<' \s >  # (note required whitespace)
        '<' <nibble(self.quote_lang(%*LANG<Q>, "<", ">", ['q', 'w']))> '>'
        <.SIGOK>
    }
    
    token metachar:sym<'> { <?[']> <quote=.LANG('MAIN','quote')> <.SIGOK> }

    token metachar:sym<"> { <?["]> <quote=.LANG('MAIN','quote')> <.SIGOK> }
    
    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<?{ }> {
        $<zw>=[ <[?!]> <?[{]> ] <codeblock>
    }

    token assertion:sym<var> {
        <?sigil> <var=.LANG('MAIN', 'term:sym<variable>')>
    }
    
    token assertion:sym<~~> {
        <sym>
        [ <?[>]> | $<num>=[\d+] | <desigilname=.LANG('MAIN','desigilname')> ]
    }

    token codeblock {
        <block=.LANG('MAIN','block')>
    }

    token arglist {
        :my $*IN_REGEX_ASSERTION := 1;
        <arglist=.LANG('MAIN','arglist')>
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
}

grammar Perl6::P5RegexGrammar is QRegex::P5Regex::Grammar does STD {
    token rxstopper { <stopper> }
    
    token p5metachar:sym<(?{ })> {
        '(?' <?[{]> <codeblock> ')'
    }
    
    token p5metachar:sym<(??{ })> {
        '(??' <?[{]> <codeblock> ')'
    }

    token p5metachar:sym<var> {
        <?[$]> <var=.LANG('MAIN', 'variable')>
    }

    token codeblock {
        <block=.LANG('MAIN','block')>
    }
}

# vim: ft=perl6 et
