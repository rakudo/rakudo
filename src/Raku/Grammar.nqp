use NQPP5QRegex;
use Raku::Actions;

sub p6ize_recursive($x) {
    if nqp::islist($x) {
        my @copy := [];
        for $x {
            nqp::push(@copy, p6ize_recursive($_));
        }
        nqp::hllizefor(@copy, 'Raku')
    }
    elsif nqp::ishash($x) {
        my %copy := nqp::hash();
        for $x {
            %copy{$_.key} := p6ize_recursive($_.value);
        }
        nqp::hllizefor(%copy, 'Raku').item
    }
    else {
        nqp::hllizefor($x, 'Raku')
    }
}

#-------------------------------------------------------------------------------
# Roles used by multiple slangs

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

#-------------------------------------------------------------------------------
# Functionality common to all Raku grammars

role Raku::Common {

    token O(*%spec) {
        <.panic: "Internal error: O() should not be used anymore">
    }

    # The alternatives for fat arrows
    token fatty { \h* [ '=>' | '⇒' ] }

    # The current language revision (1 = 6.c, 2 = 6.d, 3 = 6.e)
    method language-revision() { nqp::getcomp('Raku').language_revision }

    # Helper method for chars of match
    method leading-char()   { nqp::substr(self.orig, self.from,     1) }
    method preceding-char() { nqp::substr(self.orig, self.from - 1, 1) }

    # Helper method for determining type smileys
    method type-smiley(str $key) {
        $key eq 'D' || $key eq 'U' || $key eq '_'
    }

    # Helper method to see whether the string of the given node does not
    # have any (hidden) synthetics.  Go over each character in the string
    # and check $ch.chr eq $ch.ord.chr to fail any matches that have
    # synthetics, such as 7\x[308]
    method no-synthetics($node) {
        my str $string := ~$node;
        my int $chars  := nqp::chars($string);
        if $chars == 1 {
            $string eq nqp::chr(nqp::ord($string))
        }
        else {
            my int $i;
            while $i < $chars
              && nqp::eqat($string,nqp::chr(nqp::ord($string,$i)),$i) {
                ++$i;
            }
            $i == $chars
        }
    }

#-------------------------------------------------------------------------------
# Cursor methods
#
# These mostly exist for debugging, commenting and for possible future
# replacement by actual Raku grammar versions.  They interface to private
# cursor methods in NQP's regex engine for now.

    # Produce fresh new cursor
    method new-cursor() {
        self.'!cursor_start_cur'()
    }

    # Produce fresh new cursor at given position
    method new-cursor-at(int $pos) {
        my $cursor := self.'!cursor_start_cur'();
        $cursor.set-pos($pos);
        $cursor
    }

    # Set position of cursor
    method set-pos(int $pos) { nqp::bindattr_i(self,NQPMatch,'$!pos',$pos) }

    # Pass at current position of cursor
    method pass-at-current() {
        self.'!cursor_pass_quick'(self.pos)
    }

    # Produce a new cursor for given lang at current position
    method lang-cursor($lang) {
        $lang.'!cursor_init'(self.orig, :p(self.pos), :shared(self.'!shared'()))
    }

    # Produce a new cursor for given lang at given position
    method lang-cursor-at($lang, int $p) {
        $lang.'!cursor_init'(self.orig, :$p, :shared(self.'!shared'()))
    }

    # Produce match with item at given position
    method match-with-at($match, int $pos) {
        self.'!clone_match_at'($match, $pos)
    }

    # Produce match with item at current position
    method match-with($match) {
        self.'!clone_match_at'($match, self.pos)
    }

    # Produce match with item at current position of match
    method match-with-match($match) {
        self.'!clone_match_at'($match, $match.pos)
    }

    # Reset expectations
    method reset-expectations() {
        nqp::setelems(self.'!highexpect'(),0);
        self
    }

#-------------------------------------------------------------------------------
# Quote parsing

    method Regex($P5?) { self.slang_grammar($P5 ?? 'P5Regex' !! 'Regex') }

    method Quote() { self.slang_grammar('Quote') }

    method quote-Q($opener = "｢", $closer = "｣") {
        self.quote-lang(self.Quote, $opener, $closer)
    }

    method quote-q($opener = "'", $closer = "'") {
        self.quote-lang(self.Quote, $opener, $closer, 'q')
    }

    method quote-qq($opener = '"', $closer = '"') {
        self.quote-lang(self.Quote, $opener, $closer, 'qq')
    }

    method quote-qw() {
        self.quote-lang(self.Quote, "<", ">", 'q', [['w',1], ['v',1]])
    }

    method quote-qqw($opener = "<<", $closer = ">>") {
        self.quote-lang(
          self.Quote, $opener, $closer, 'qq', [['ww',1], ['v',1]]
        )
    }

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
        my $HOW := self.HOW;
        nqp::istype($stop,VMArray)
          ?? $HOW.mixin(self,
               startstops.HOW.curry(startstops, $start, $stop[0], $stop[1])
             )
          !! $HOW.mixin(self, startstop.HOW.curry(startstop, $start, $stop));
    }

    method unbalanced($stop) {
        self.HOW.mixin(self, stop.HOW.curry(stop, $stop));
    }

    token starter { <!> }
    token stopper { <!> }

    # Updates to the quote lang cache need to be thread-safe
    my $quote-lang-lock := NQPLock.new;

    # Define a quote language, a combination of a base grammar with a
    # set of base tweaks, and a set of additional tweaks.  For a quote
    # string such as qq:!s/foo bar/.
    method quote-lang(
      $l,       # grammar class to be used
      $start,   # the starter string
      $stop,    # the string marking the end of the quote language
      $base?,   # base quote-language, e.g. 'q' for q/foobar/
      @tweaks?  # :adverbs, 's' in q:s/foobar/, as [key,Bool] lists
    ) {

        # Check validity of extra tweaks
        for @tweaks {
            my $t := $_[0];
            if $t eq 'o' || $t eq 'format' {
                unless self.language-revision >= 3 {
                    self.panic("Unrecognized adverb: :$t");
                }
            }
        }

        # Return a key to identify this quote language in a cache.  The
        # .WHICH of the quote language, if you will.
        sub key-for-quote-lang() {

            # Assemble the parts of the key
            my @keybits := [self.HOW.name(self), $l.HOW.name($l), $start];
            @keybits.push(nqp::istype($stop,VMArray)
              ?? nqp::join(' ',$stop)
              !! $stop
            );

            @keybits.push($base) if $base;
            for @tweaks {
                my str $t := $_[0];
                @keybits.push($t eq 'to'
                  ?? 'HEREDOC'         # all heredocs share the same lang
                  !! $t ~ '=' ~ $_[1]  # cannot use nqp::join as [1] is Bool
                );
            }

            nqp::join("\0", @keybits)
        }

        # Create a new type for the given quote language arguments
        sub create-quote-lang-type() {
            my $lang := self.lang-cursor($l);
            $lang.clone_braid_from(self);

            # mixin any base tweak other than Q
            $lang := $lang."tweak_$base"(1) if $base;

            # mixin any extra tweaks
            for @tweaks {
                my str $t := $_[0];
                nqp::can($lang,"tweak_$t")
                  ?? ($lang := $lang."tweak_$t"($_[1]))
                  !! self.panic("Unrecognized adverb: :$t");
            }

            # make sure any actions are available and the stopper is known
            for self.slangs {
                if nqp::istype($lang, $_.value) {
                    $lang.set_actions(self.slang_actions($_.key));
                    last;
                }
            }
            $lang.set_pragma("STOPPER",$stop);

            # balanced if stopper different from starter, or multiple stoppers
            nqp::istype($stop,VMArray) || $start ne $stop
              ?? $lang.balanced($start, $stop)
              !! $lang.unbalanced($stop)
        }

        # get language from cache or derive it.
        my $key   := key-for-quote-lang();
        my %cache := %*QUOTE-LANGS;

        # Read from / Update to cache in a thread-safe manner
        nqp::lock($quote-lang-lock);
        my $quote-lang := nqp::ifnull(
          nqp::atkey(%cache,$key),
          nqp::bindkey(%cache,$key,create-quote-lang-type())
        );
        nqp::unlock($quote-lang-lock);

        $quote-lang.set_package(self.package);
        $quote-lang
    }

    # Note, $lang must carry its own actions by the time we call this.
    method nibble($lang) {
        self.lang-cursor($lang).nibbler.set_braid_from(self)
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
        $/.panic($message, expected => [$stop]);
    }

#-------------------------------------------------------------------------------
# Heredoc handling

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
        method parsing-heredoc() { 1 }
    }

    method heredoc () {
        my $CU      := $*CU;
        my $actions := self.actions;

        if $CU && my @herestub_queue := $CU.herestub-queue {
            my $here := self.new-cursor-at(self.pos);
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
                    my $stop := self.lang-cursor-at($lang, $doc.pos).stopper;
                    $stop.clone_braid_from(self);
                    unless $stop {
                        self.panic("Ending delimiter $*DELIM not found");
                    }
                    $here.set-pos($stop.pos);

                    # Get it trimmed and AST updated.
                    my $heredoc := $herestub.orignode.MATCH.ast;
                    $heredoc.replace-segments-from($doc.MATCH.ast);
                    $heredoc.steal-processors-from($doc.MATCH.ast);
                    $heredoc.set-stop(~$stop);
                    my str $ws := $stop.MATCH<ws>.Str;
                    my int $actualchars := nqp::chars($ws);
                    my int $indent := $actualchars;
                    my int $tabstop := $*R.resolve-lexical('$?TABSTOP').compile-time-value;
                    my int $checkidx;
                    while $checkidx < $actualchars {
                        if nqp::eqat($ws, "\t", $checkidx) {
                            $indent := $indent + ($tabstop - 1);
                        }
                        ++$checkidx;
                    }
                    $heredoc.set-indent($indent);
                    $heredoc.trim();
                }
                else {
                    self.panic("Ending delimiter $*DELIM not found");
                }
            }
            $here.pass-at-current;
            $here.set_actions($actions);
            $here
        }
        else {
            self
        }
    }

    token cheat-heredoc {
        <?{ nqp::elems($*CU.herestub-queue) }>
        \h*
        <[ ; } ]>
        \h*
        <?before \n | '#'>

        :my $R;
        :my $scope;
        {
            $R     := $*R;
            $scope := $R.current-scope;
            $R.leave-scope;
        }
        <.ws>
        { $R.enter-scope($scope) }
        <?MARKER('end-statement')>
    }

    token quibble($l, $base?, *@tweaks) {
        :my $lang;
        :my $start;
        :my $stop;
        <babble($l, $base, @tweaks)>
        {
            my $B  := $<babble><B>.ast;
            $lang  := $B[0];
            $start := $B[1];
            $stop  := $B[2];
        }
        $start
        <nibble($lang)>
        [
          || $stop

          || {
                 my $B := $<babble><B>;
                 self.fail-terminator($/, $start, $stop,
                   HLL::Compiler.lineof($B.orig(), $B.from(), :cache(1))
                 );
             }
        ]

        {
            if nqp::can($lang,'herelang') {
                my $delim := $<nibble>.ast.literal-value // $/.panic(
                  "Stopper '" ~ $<nibble> ~ "' too complex for heredoc"
                );
                $*CU.queue-heredoc(Herestub.new(
                  :$delim, :grammar($lang.herelang), :orignode(self)
                ));
            }
        }
    }

    token babble($l, $base?, @tweaks?) {
        [ <quotepair>
          <.ws>
          {
              my $pair := $<quotepair>[-1].ast;
              my $k    := self.adverb-q2str($pair.key);
              my $v    := $pair.value;
              nqp::can($v,'compile-time-value')
                ?? nqp::push(@tweaks, [$k, $v.compile-time-value])
                !! self.panic("Invalid adverb value for "
                     ~ $<quotepair>[-1].Str
                     ~ ': '
                     ~ $v.HOW.name($v)
                   );
          }
        ]*

        $<B>=[<?before .>]
        {
            # Work out the delimiters.
            my $c      := $/;
            my @delims := $c.peek_delimiters($c.target, $c.pos);
            my $start  := @delims[0];
            my $stop   := @delims[1];

            # Get the language.
            my $lang := self.quote-lang($l, $start, $stop, $base, @tweaks);
            $<B>.make([$lang, $start, $stop]);
        }
    }

    # Handle restricted code tests
    token RESTRICTED {
        [ <?{ $*RESTRICTED }>
          [                    # checking for restricted code
               $               # end of source reached, ok
            || {self.typed-panic:  # OR we've run into restricted code
                 'X::SecurityPolicy::Eval', :payload($*RESTRICTED)}
          ]
        ]?
        <!>
    }

#-------------------------------------------------------------------------------
# Error handling

    # Specific error handling
    method NYI($feature) {
        self.typed-panic: 'X::Comp::NYI', :$feature;
    }
    method malformed($what, $additional?) {
        my $name := 'X::Syntax::Malformed';
        if $additional {
            $name := $name ~ '::' ~ $additional;
        }
        self.typed-panic: $name, :$what;
    }
    method missing($what) {
        self.typed-panic: 'X::Syntax::Missing', :$what;
    }
    method missing-block($borg, $has-mystery) {
        my $marked := self.MARKED('ws');
        my $pos    := $marked ?? $marked.from !! self.pos;
        my $block  := $borg<block>;

        # If the highwater is beyond the current position, force the cursor to
        # that location.  (Maybe.)
        my $high := self.'!highwater'();
        if $high >= self.pos() {
            self.'!cursor_pos'($high);
        }

        if $block {
            my $name := $borg<name> // '';
            self.typed-sorry-at: $block.pos, 'X::Syntax::BlockGobbled',
              :what($name);
            self.missing:
              "block (apparently claimed by "
                ~ ($name ?? "'$name'" !! "expression")
                ~ ")";
        }
        else {
            self.missing: $pos > 0 && nqp::eqat(self.orig(),'}',$pos - 1)
              ?? "block (whitespace needed before curlies taken as a hash subscript?)"
              !! $has-mystery
                ?? "block (taken by some undeclared routine?)"
                !! "block";
        }
    }

    # Shadow error handling from HLL::Grammar
    method dupprefix($prefixes) {
        self.typed-panic: 'X::Syntax::DuplicatedPrefix', :$prefixes;
    }

    # All sorts of ad-hoc exception handling
    method panic(*@args, *%nameds) {
        self.typed-panic:
          'X::Comp::AdHoc', payload => nqp::join('', @args), |%nameds
    }
    method sorry(*@args) {
        self.typed-sorry: 'X::Comp::AdHoc', payload => nqp::join('', @args)
    }
    method worry(*@args) {
        self.typed-worry: 'X::Comp::AdHoc', payload => nqp::join('', @args)
    }

    # All sorts of typed exception handling
    method typed-panic($name, *%opts) {
        $*R.panic: self.build-exception($name, |%opts);
    }
    method typed-sorry($name, *%opts) {

        # Still allowing sorries
        if $*SORRY_REMAINING-- {
            $*R.add-sorry: self.build-exception($name, |%opts);
            self
        }

        # Too many sorries, call it a day
        else {
            self.typed-panic($name, |%opts)
        }
    }
    method typed-sorry-at(int $pos, $name, *%opts) {
        self.'!clear_highwater'();
        my $original-pos := self.pos;
        self.set-pos($pos);
        self.typed-sorry($name, |%opts);
        self.set-pos($original-pos);
    }
    method typed-worry($name, *%opts) {
        $*R.add-worry: self.build-exception($name, |%opts);
        self
    }

    # Build an exception by name through the current resolver
    method build-exception($name, *%opts) {

        # Set up absolute path if possible
        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isnull($file) {
            $file := '<unknown file>';
        }
        elsif !nqp::eqat($file,'/', 0)    # does not start with /
          &&  !nqp::eqat($file,'-e',0)    # and it's not -e
          &&  !nqp::eqat($file,':', 1) {  # and no drive letter at start
            $file := nqp::cwd ~ '/' ~ $file;
        }

        my $cursor := %opts<precursor> ?? self.PRECURSOR !! self;
        my @prepost := self.prepost($cursor);
        my $pre := @prepost[0];
        my $post := @prepost[1];

        my @expected;
        my $high := $cursor.'!highwater'();
        if %opts<precursor> {
            $cursor := self.PRECURSOR;
        }
        elsif %opts<expected> {
            @expected := %opts<expected>;
        }
        elsif $high >= $cursor.pos() {
            my @raw_expected := $cursor.'!highexpect'();
            $cursor.'!cursor_pos'($high);
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
        if $name eq 'X::Syntax::Confused' {
            if $post ~~ / ^ \s* <[ } ) \] > » ]> / {
                %opts<reason> := "Unexpected closing bracket";
                @expected := [];
            }
            elsif $pre ~~ / \} \s* $ / {
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
                    elsif $*IN-META {
                        %opts<reason> := "Bogus infix";
                    }
                    elsif $cursor.MARKED('baresigil') {
                        %opts<reason> := "Name must begin with alphabetic character";
                    }
                    elsif $post ~~ / ^ \s* <[ $ @ \w ' " ]> / ||
                          $post ~~ / ^ \s+ <[ ( [ { « . ]> / {
                        %opts<reason> := "Two terms in a row";
                    }
                    elsif $post ~~ / ^ '<EOL>' / {
                        %opts<reason> := "Two terms in a row across lines (missing semicolon or comma?)";
                    }
                    elsif $post ~~ / ^ \S / {
                        %opts<reason> := "Bogus postfix";
                    }
                    # "Confused" is already the default, so no "else" clause needed here.
                }
                # or here...
            }
            my $qs := $*LASTQUOTE[0];
            my $qe := $*LASTQUOTE[1];
            if HLL::Compiler.lineof($cursor.orig, $qe, :cache(1)) >= HLL::Compiler.lineof($cursor.orig, $cursor.pos, :cache(1)) - 1
                && nqp::index(nqp::substr($cursor.orig, $qs, $qe - $qs), "\n") >= 0 {
                my $quotes :=
                    nqp::substr($cursor.orig, $qs - 1 , 1) ~
                    nqp::substr($cursor.orig, $qe, 1);
                $quotes := "<<>>" if $quotes eq '<>' && nqp::eqat($cursor.orig, '>', $qe + 1);
                %opts<reason> := %opts<reason> ~ " (runaway multi-line " ~ $quotes ~
                    " quote starting at line " ~ HLL::Compiler.lineof($cursor.orig, $qs, :cache(1)) ~ " maybe?)";
            }
        }

        $*R.build-exception: $name,
          line => HLL::Compiler.lineof($cursor.orig, $cursor.pos, :cache(1)),
          pos  => $cursor.pos,
          pre  => $pre,
          post => $post,
          filename => $file,
          |%opts
    }

    # Separate text before/after given cursor for error messages
    method prepost($cursor) {
        my $orig    := $cursor.orig;
        my $marked  := $cursor.MARKED('ws');
        my int $pos := $marked
          && nqp::index(" }])>»", nqp::substr($orig, $cursor.pos, 1)) < 0
          ?? $marked.from
          !! $cursor.pos;

        my int $distance := 40;
        my int $prestart := $pos - $distance;
        $prestart := 0 if $prestart < 0;

        # FIXME workaround for when $pos is -3. Need to figure out how to
        # get the real pos
        $pos := 0 if $pos < 0;

        my $pre := nqp::substr($orig,$prestart,$pos - $prestart);
        $pre    := subst($pre, /.*\n/, "", :global);
        $pre    := '<BOL>' if $pre eq '';

        my int $postchars := $pos + $distance > nqp::chars($orig)
          ?? nqp::chars($orig) - $pos
          !! $distance;
        my $post := nqp::substr($orig, $pos, $postchars);
        $post    := subst($post, /\n.*/, "", :global);
        $post    := '<EOL>' if $post eq '';

        [$pre, $post]
    }

    method FAILGOAL($goal, $dba?) {
        my $stopper;
        unless $dba {
            $dba := nqp::getcodename(nqp::callercode());
            # Handle special case to hide variable name leaked by core grammar
            if ~$goal eq '$stopper ' {
                my $ch := $dba ~~ /
                  [post]?
                  circumfix\:sym[ \< | \« ] \S+ \s+ (\S+) [ \> | \» ]
                /;
                $ch      := ~$ch[0];
                $stopper := "'$ch'" if nqp::chars($ch);
            }
        }
        # core grammar also has a penchant for sending us trailing .ws contents
        $stopper := $stopper // $goal;
        $stopper := $stopper ~~ / (.*\S) \s* /;
        $stopper := ~$stopper[0];
        self.typed-panic: 'X::Comp::FailGoal',
          :$dba,
          :goal($stopper),
          :line-real(HLL::Compiler.lineof(self.orig(), self.from(), :cache(1)))
        ;
    }

    # "when" arg assumes more things will become obsolete after Raku comes out
    method obs($old, $replacement, $when = 'in Raku', :$ism = 'p5isms') {
        $*LANG.pragma($ism)
          ?? self
          !! self.typed-panic: 'X::Obsolete', :$old, :$replacement, :$when
    }
    method obsvar($name, $identifier-name?) {
        $*LANG.pragma('p5isms')
          ?? self
          !! self.typed-panic: 'X::Syntax::Perl5Var', :$name, :$identifier-name
    }
    method sorryobs($old, $replacement, $when = 'in Raku') {
        self.typed-sorry('X::Obsolete', :$old, :$replacement, :$when)
          unless $*LANG.pragma('p5isms');
        self
    }
    method worryobs($old, $new, $when = 'in Raku') {
        self.typed-worry('X::Obsolete',
            old         => $old,
            replacement => $new,
            when        => $when,
        ) unless $*LANG.pragma('p5isms');
        self
    }
    method obsbrace() {
        self.obs: 'curlies around escape argument', 'square brackets';
    }

    # Return the name of the meta op if any
    method meta-op-name($desigilname) {
        my $op := $desigilname.colonpairs[0].literal-value;
        if $op ne '!=' && $op ne '≠' {
            my $lang := self.'!cursor_init'($op, :p(0));
            $lang.clone_braid_from(self);

            my $category := $desigilname.canonicalize(:colonpairs(0));
            my $method   := $category eq 'infix'
              ?? 'infixish'
              !! $category eq 'prefix'
                ?? nqp::eqat($op,"[",0) && nqp::eqat($op,"]",nqp::chars($op)-1)
                  ?? 'term:sym<reduce>'
                  !! 'prefixish'
                !! $category eq 'postfix'
                  ?? 'postfixish'
                  !! $category;

            my $cursor := $lang."$method"();
            if $cursor.pos == nqp::chars($op) {
                my $match := $cursor.MATCH;
                if   $match<infix-prefix-meta-operator>
                  || $match<infix-circumfix-meta-operator>
                  || $match<infix-postfix-meta-operator>
                  || $match<prefix-postfix-meta-operator>
                  || $match<postfix-prefix-meta-operator>
                  || $match<op>
                {
                    return $match;
                }
            }
        }

        NQPMu
    }

    # Check the validity of a variable, handle meta-ops for Callables
    method check-variable($var) {
        my $ast := $var.ast;

        # Not capable of checking
        return Nil
          unless nqp::eqaddr($ast.WHAT,self.actions.r('Var', 'Lexical').WHAT);

        return Nil if nqp::isconcrete($*DECLARE-TARGETS) && $*DECLARE-TARGETS == 0;

        # Nothing to do?
        $ast.ensure-parse-performed($*R, $*CU.context);
        return Nil if $ast.is-resolved;

        my $name := $ast.name;
        if $ast.sigil eq '&' {
            # Nothing to do?
            return Nil unless $ast.IMPL-IS-META-OP;
            my $desigilname := $ast.desigilname;
            my $meta-op-name := self.meta-op-name($desigilname);
            return Nil unless nqp::isconcrete($meta-op-name);

            my $META := $meta-op-name.ast;
            $META.to-begin-time($*R, $*CU.context);

            my $meta-op := $META.IMPL-HOP-INFIX;
            $ast.set-resolution(
              self.actions.r('Declaration','External','Constant').new(
                lexical-name       => $name,
                compile-time-value => $meta-op
              )
            );
        }

        elsif ($*VARIABLE-NAME && $*VARIABLE-NAME eq $name) {
            self.typed-panic: 'X::Syntax::Variable::Initializer', :$name;
        }

        # Not resolved and not a Callable
        else {
            self.typed-panic: 'X::Undeclared',
              symbol          => $name,
              is-compile-time => 1,
              suggestions     => $*R.suggest-lexicals($name);
        }
    }

    # Provide parent's rule/token @*ORIGIN-NESTINGS to ease and unify
    # creating a stack of key AST nodes.
    method PARENT-NESTINGS() {
        # Expect to be called immediately from the nesting token.
        my $parent-ctx := nqp::ctxcallerskipthunks(nqp::ctxcaller(nqp::ctx()));
        nqp::getlexreldyn($parent-ctx, '@*ORIGIN-NESTINGS');
    }

    method key-origin($subrule, *@pos, *%named) {
        my @*PARENT-NESTINGS := self.PARENT-NESTINGS();
        my @*ORIGIN-NESTINGS := [];
        my $rc := self."$subrule"(|@pos, |%named);
        self.actions.key-origin($rc) if $rc;
        $rc
    }
}

#-------------------------------------------------------------------------------
# Compilation unit, language version and other entry point bits

grammar Raku::Grammar is HLL::Grammar does Raku::Common {

#-------------------------------------------------------------------------------
# Translatable tokens

    # These tokens replace bare strings so that they can be localized by
    # mixing in tokens with translated localizations.
    token block-default  { default}
    token block-else     { else}
    token block-elsif    { elsif}
    token block-for      { for}
    token block-given    { given}
    token block-if       { if}
    token block-loop     { loop}
    token block-orwith   { orwith}
    token block-repeat   { repeat}
    token block-unless   { unless}
    token block-until    { until}
    token block-when     { when}
    token block-whenever { whenever}
    token block-while    { while}
    token block-with     { with}
    token block-without  { without}

    token constraint-where { where}

    token infix-after      { after}
    token infix-and        { and}
    token infix-andthen    { andthen}
    token infix-before     { before}
    token infix-but        { but}
    token infix-cmp        { cmp}
    token infix-coll       { coll}
    token infix-pcontp     { '(cont)'}
    token infix-div        { div}
    token infix-does       { does}
    token infix-pelemp     { '(elem)'}
    token infix-eq         { eq}
    token infix-eqv        { eqv}
    token infix-ff         { ff}
    token infix-cff        { '^ff'}
    token infix-ffc        { 'ff^'}
    token infix-cffc       { '^ff^'}
    token infix-fff        { fff}
    token infix-cfff       { '^fff'}
    token infix-fffc       { 'fff^'}
    token infix-cfffc      { '^fff^'}
    token infix-gcd        { gcd}
    token infix-ge         { ge}
    token infix-gt         { gt}
    token infix-lcm        { lcm}
    token infix-le         { le}
    token infix-leg        { leg}
    token infix-lt         { lt}
    token infix-max        { max}
    token infix-min        { min}
    token infix-minmax     { minmax}
    token infix-mod        { mod}
    token infix-ne         { ne}
    token infix-notandthen { notandthen}
    token infix-or         { or}
    token infix-orelse     { orelse}
    token infix-unicmp     { unicmp}
    token infix-x          { x}
    token infix-xor        { xor}
    token infix-xx         { xx}

    token meta-R { R}
    token meta-S { S}
    token meta-X { X}
    token meta-Z { Z}

    token modifier-for     { for}
    token modifier-given   { given}
    token modifier-if      { if}
    token modifier-unless  { unless}
    token modifier-until   { until}
    token modifier-when    { when}
    token modifier-while   { while}
    token modifier-with    { with}
    token modifier-without { without}

    token multi-multi { multi}
    token multi-only  { only}
    token multi-proto { proto}

    token package-class   { class}
    token package-grammar { grammar}
    token package-knowhow { knowhow}
    token package-module  { module}
    token package-native  { native}
    token package-package { package}
    token package-role    { role}

    token phaser-BEGIN   { BEGIN}
    token phaser-CATCH   { CATCH}
    token phaser-CHECK   { CHECK}
    token phaser-CLOSE   { CLOSE}
    token phaser-CONTROL { CONTROL}
    token phaser-DOC     { DOC}
    token phaser-END     { END}
    token phaser-ENTER   { ENTER}
    token phaser-FIRST   { FIRST}
    token phaser-INIT    { INIT}
    token phaser-KEEP    { KEEP}
    token phaser-LAST    { LAST}
    token phaser-LEAVE   { LEAVE}
    token phaser-NEXT    { NEXT}
    token phaser-POST    { POST}
    token phaser-PRE     { PRE}
    token phaser-QUIT    { QUIT}
    token phaser-TEMP    { TEMP}
    token phaser-UNDO    { UNDO}

    token prefix-let  { let}
    token prefix-not  { not}
    token prefix-so   { so}
    token prefix-temp { temp}

    token quote-lang-m  { m}
    token quote-lang-ms { ms}
    token quote-lang-q  { q}
    token quote-lang-Q  { Q}
    token quote-lang-qq { qq}
    token quote-lang-rx { rx}
    token quote-lang-s  { s}
    token quote-lang-S  { S}
    token quote-lang-ss { ss}
    token quote-lang-Ss { Ss}

    token routine-method    { method}
    token routine-sub       { sub}
    token routine-regex     { regex}
    token routine-rule      { rule}
    token routine-submethod { submethod}
    token routine-token     { token}

    token scope-anon      { anon}
    token scope-augment   { augment}
    token scope-constant  { constant}
    token scope-has       { has}
    token scope-HAS       { HAS}
    token scope-my        { my}
    token scope-our       { our}
    token scope-state     { state}
    token scope-supersede { supersede}
    token scope-unit      { unit}

    token stmt-prefix-also    { also}
    token stmt-prefix-do      { do}
    token stmt-prefix-eager   { eager}
    token stmt-prefix-gather  { gather}
    token stmt-prefix-hyper   { hyper}
    token stmt-prefix-lazy    { lazy}
    token stmt-prefix-once    { once}
    token stmt-prefix-quietly { quietly}
    token stmt-prefix-race    { race}
    token stmt-prefix-react   { react}
    token stmt-prefix-sink    { sink}
    token stmt-prefix-start   { start}
    token stmt-prefix-supply  { supply}
    token stmt-prefix-try     { try}

    token term-self { self}
    token term-nano { nano}
    token term-now  { now}
    token term-rand { rand}
    token term-time { time}

    token traitmod-does    { does}
    token traitmod-will    { will}
    token traitmod-handles { handles}
    token traitmod-hides   { hides}
    token traitmod-is      { is}
    token traitmod-of      { of}
    token traitmod-returns { returns}

    token typer-enum   { enum}
    token typer-subset { subset}

    token use-import  { import}
    token use-need    { need}
    token use-no      { no}
    token use-require { require}
    token use-use     { use}

    # Convert the invocant, a match that is expected to have a RakuAST::Name
    # object as its ".ast" (or to have no ".ast" at all), to a RakuAST::Name
    # object with the name of the core functionality if there is an original
    # name known.  Otherwise it should just return the ".ast" of the invocant.
    method core2ast() {
        self.ast // self.actions.r('Name').from-identifier(~self)
    }

    # Convert the invocant, a match that is expected to have a RakuAST::Name
    # object as its ".ast" (or to have no ".ast" at all), to a RakuAST::Name
    # object with the name of the trait_mod:<is> name if there is an
    # original name known.  Otherwise it should just return the ".ast" ofi
    # the invocant.
    method trait-is2ast() {
        self.ast // self.actions.r('Name').from-identifier(~self)
    }

    # Convert the given postcircumfix adverb if there is an original name
    # for it.  Otherwise it should just return the adverb unchanged.
    method adverb-pc2str(str $key) { $key }

    # Convert the given quoting adverb if there is an original name
    # for it.  Otherwise it should just return the adverb unchanged.
    method adverb-q2str(str $key) { $key }

    # Convert the given regex adverb if there is an original name
    # for it.  Otherwise it should just return the adverb unchanged.
    method adverb-rx2str(str $key) { $key }

    # Convert the given named argument if there is an original name
    # for it.  Otherwise it should just return the named argument unchanged.
    method named2str(str $key) { $key }

    # Convert the given pragma if there is an original name for it.
    # Otherwise it should just return the given pragma unchanged.
    method pragma2str(str $key) { $key }

    # Convert the given system method if there is an original name for it.
    # Otherwise it should just return the system method unchanged.
    method system2str(str $key) { $key }

#-------------------------------------------------------------------------------
# Grammar entry point

    method TOP() {
        # Set up the language braid.
        my $*LANG := self;
        my $*MAIN := 'MAIN';
        self.define_slang('MAIN',    self.WHAT,            self.actions);
        self.define_slang('Quote',   Raku::QGrammar,       Raku::QActions);
        self.define_slang('Regex',   Raku::RegexGrammar,   Raku::RegexActions);
        self.define_slang('P5Regex', Raku::P5RegexGrammar, Raku::P5RegexActions);

        # we default to strict!
        self.set_pragma('strict',1);

        # Variables used during the parse.
        my $*IN-DECL;                # what declaration we're in
        my $*OFTYPE;                 # type of the current declarator
        my $*LEFTSIGIL;              # sigil of LHS for item vs list assignment
        my $*IN-META := '';          # parsing a metaoperator like [..]
        my $*IN_REDUCE := 0;         # attempting to parse an [op] construct
        my $*IN_TABLE := 0;          # > 0 if inside a =table
        my %*QUOTE-LANGS;            # quote language cache
        my $*LASTQUOTE := [0,0];     # for runaway quote detection
        my $*SORRY_REMAINING := 10;  # decremented on each sorry; panic when 0
        my $*BORG := {};             # who gets blamed for a missing block

        # -1 indicates we're outside of any "supply" or "react" block
        my $*WHENEVER-COUNT := -1;

        # This contains the given options, from the command line if invoked
        # from there, otherwise from EVAL invocations.
        my %*OPTIONS := %*COMPILING<%?OPTIONS>;

        # This contains the current index to determine the order in which
        # legacy $=pod is being filled by ::Doc::Block and ::Doc::Declarator
        # blocks.  Whenever a new object of these is made, the value of
        # this dynamic variable will be stored in the object, and then
        # incremented.  At CHECK time, the generated Pod:: objects will
        # be bound to the index given at creation time.
        my $*LEGACY-POD-INDEX := 0;

        # $/.from locations of declarator doc and rakudo blocks that have
        # been seen and handled before.  Needed because the grammar can
        # actually visit the same piece of the code more than once.
        my $*FROM-SEEN  := {};

        # Set to True if parsing inside a DOC-BLOCK, to prevent attaching
        # of doc blocks to statements that will not actually be CHECKed
        my $*PARSING-DOC-BLOCK;

        # RakuDoc blocks collected so far, to be included with next statement
        # into its statement list.
        my $*DOC-BLOCKS-COLLECTED := [];

        # RakuDoc aliases (=alias -> A<>) collected so far
        my $*DOC-ALIASES := {};

        # RakuDoc config info (=config foo :allow<B C>)
        my $*DOC-CONFIG := {};

        # Any resolver that exists outside this grammar: usually this is the
        # resolver that is active whenever code is being EVALled inside BEGIN
        # block, which would create a new resolver and put it in $*R.
        my $*OUTER-RESOLVER := $*R;

        # Parse a compilation unit.
        self.comp-unit($*CU)
    }

    # The ByteOrderMarker
    token bom { \xFEFF }

    # Set up the language to be used, possibly specified by "use vxxx"
    rule lang-setup($*OUTER-CU) {
        # TODO validate this and pay attention to it in actions
        [ <.ws>? use <version> ';'? ]?
    }

    # This is like HLL::Grammar.LANG but it allows to call a token of a
    # Raku level grammar.  Takes the language (usually 'MAIN') and the
    # name of the regex to be executed.
    method FOREIGN-LANG($langname, $regex) {
        my $grammar := self.slang_grammar($langname);
        if nqp::istype($grammar, NQPMatch) {
            self.LANG($langname, $regex);
        }
        else {
            nqp::die('FOREIGN-LANG non-NQP branch NYI')
        }
    }

    # Set up compilation unit and symbol resolver according to the language
    # version that is declared, if any. Then parse the outer statement list.
    token comp-unit($outer-cu) {
        <.bom>?  # ignore any ByteOrderMark

        :my $*CU;              # current RakuAST::CompUnit object
        :my $*ORIGIN-SOURCE;   # current RakuAST::Origin::Source object
        :my @*ORIGIN-NESTINGS := [];  # handling nested origins
        :my $*R;               # current RakuAST::Resolver::xxx object
        :my $*LITERALS;        # current RakuAST::LiteralBuilder object
        :my &*DD;              # debug helper to dd()
        {
            self.actions.comp-unit-prologue($/);  # set the above variables
        }

        :my $*IN-TYPENAME;      # fallback for inside typename flag
        :my $*ADVERB-AS-INFIX;  # fallback for fake infix handling

        :my @*LEADING-DOC := [];         # temp storage leading declarator doc
        :my $*DECLARAND;                 # target for trailing declarator doc
        :my $*LAST-TRAILING-LINE := -1;  # number of last line with trailing doc
        :my $*IGNORE-NEXT-DECLARAND;     # True if next declarand to be ignored
        :my $*DECLARAND-WORRIES := {};   # $/ of worries when clearing DECLARAND

        :my $*EXPORT;
        :my $*COMPILING_CORE_SETTING := 0;
        :my $*NEXT-STATEMENT-ID := 0;  # to give each statement an ID
        :my $*START-OF-COMPUNIT := 1;  # flag: start of a compilation unit?
        <.lang-setup($outer-cu)>  # set the above variables

        # Further needed initializations
        {
             $*R.enter-scope($*CU);
             $*R.create-scope-implicits();
             self.actions.load-M-modules($/);
        }

        # Perform the actual parsing of the code, using origin tracking
        <statementlist=.key-origin('FOREIGN-LANG',$*MAIN,'statementlist')>

        # All parsed so far
        [
          $                                         # all ok, reach the end
          || <?before <.[ } ) \] > » ]>>
             {self.typed-panic: 'X::Syntax::Confused', reason => 'Unexpected closing bracket'}
          || {self.typed-panic: 'X::Syntax::Confused'}  # huh??
        ]
        { $*R.leave-scope }
    }

#-------------------------------------------------------------------------------
# Statement level parsing

    # Parsing zero or more statements, e.g. inside a (pointy) block
    rule statementlist {
        :dba('statement list')
        <.ws>
        :my $*LANG;              # Define this scope to be a new language
        <!!{ $*LANG := $/.clone_braid_from(self); 1 }>
        [
          | $                                       # the end of code
          | <?before <.[\)\]\}]>>                   # or bumping into ) ] }
          | [
              <statement=.key-origin('statement')>  # or statement with tracking
              <.eat-terminator>                     # until any terminator
            ]*
        ]
        <.set_braid_from(self)>  # Language tweaks must not escape
        <!!{ nqp::rebless($/, self.WHAT); 1 }>
    }

    # Parsing zero or more statements in an expression, e.g @a[10;20]
    rule semilist {
        :dba('list composer')
        ''
        [
          | <?before <.[)\]}]> >  # bumping into  ) ] }
          | [
              <statement>         # or the statement (without origin tracking)
              <.eat-terminator>   # until any terminator
            ]*
        ]
    }

    # Parsing zero or more statements in a contextualizer, e.g $(10;20)
    rule sequence {
        :dba('sequence of statements')
        ''
        [
          | <?before <.[)\]}]> >  # bumping into  ) ] }
          | [
              <statement>         # or the statement (without origin tracking)
              <.eat-terminator>   # until any terminator
            ]*
        ]
    }

    # Parsing an actual Raku statement
    token statement {
        :my $*QSIGIL       := '';                          # init quote lang
        :my $*SCOPE        := '';                          # init scope type
        :my $*STATEMENT-ID := ++$*NEXT-STATEMENT-ID;       # set statement ID
        :my $actions       := self.slang_actions('MAIN');  # shortcut to actions

        <!!{ $/.set_actions($actions); 1 }>
        <!before <.[\])}]> | $ >
        #<!stopper>
        <!!{ nqp::rebless($/, self.slang_grammar('MAIN')); 1 }>

        [
          | <label> <statement>
          | <statement-control>
          | <EXPR>
            :dba('statement end')
            [
              || <?MARKED('end-statement')>
              || :dba('statement modifier')
                 <.ws>
                 <statement-mod-cond>
                 <statement-mod-loop>?
              || :dba('statement modifier loop')
                 <.ws>
                 <statement-mod-loop>
                 {
                     unless $*LANG.pragma('p5isms') {
                         my $sp := $<EXPR><statement-prefix>;
                         $/.obs(
                           "do..." ~ $<statement-mod-loop><sym>,
                           "repeat...while or repeat...until"
                         ) if $sp && $sp<sym> eq 'do';
                     }
                 }
            ]?
          | <?[;]>
          #| <?stopper>
          | {} <.panic: "Bogus statement">
        ]
    }

    # Parsing a statement label
    token label {
        <identifier> ':' <?[\s]> <.ws>
    }

    # Helper token to parse until the end of a statement
    token eat-terminator {
        || ';'                         # a real end of statement
        || <?MARKED('end-statement')> <.ws>  # OR XXX
        || <?before <.[)\]}]> >        # OR bumping into  ) ] }
        || $                           # OR end of text
        || <?stopper>                  # OR XXX
        || <?before [                  # OR looks like leaking into next
               <.block-for>
             | <.block-if>
             | <.block-given>
             | <.block-loop>
             | <.block-repeat>
             | <.block-when>
             | <.block-while>
           ] » >
           { $/.'!clear_highwater'() }
           {self.typed-panic: 'X::Syntax::Confused', reason => "Missing semicolon"}
        || {self.typed-panic: 'X::Syntax::Confused'} # OR give up
    }

    # Helper token to match the start of a pointy block
    token pointy-block-starter { '->' | '→' | '<->' | '↔' }

    # Parsing a (pointy) block
    token pointy-block {
        :dba('block or pointy block')
        :my $borg := $*BORG;                        # keep current context
        :my $has-mystery := 0; # TODO
        { $*BORG := {} }                            # initialize new context
        :my $*BLOCK;                                # localize block to here
        [
          | <pointy-block-starter>                 # block with signature
            :my $*GOAL := '{';
            <.enter-block-scope('PointyBlock')>
            {
                if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                    $*BLOCK.ensure-parse-performed($*R, $*CU.context);
                }
            }
            {}
            :my $*DEFAULT-RW := $<pointy-block-starter> eq '<->' ?? 2 !! 0;
            <signature>
            { if $<signature> { $*BLOCK.replace-signature($<signature>.ast); } }
            <blockoid>
            <.leave-block-scope>
          | <?[{]>                                  # block without signature
            <.enter-block-scope('Block')>
            {
                if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                    $*BLOCK.ensure-parse-performed($*R, $*CU.context);
                }
            }
            <blockoid>
            <.leave-block-scope>
          || <.missing-block($borg, $has-mystery)>  # OR give up
        ]
    }

    # Parsing a block *without* a signature (e.g. phasers)
    token block($kind = 'Block', :$parameterization) {
        :dba('scoped block')
        :my $borg := $*BORG;                        # keep current context
        :my $has-mystery := 0; # TODO
        { $*BORG := {} }                            # initialize new context
        :my $*BLOCK;                                # localize block to here
        [
          || <?[{]>                                 # block without signature
             <.enter-block-scope($kind, $parameterization)>
             {
                 if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                     $*BLOCK.ensure-parse-performed($*R, $*CU.context);
                 }
             }
             <blockoid>
             <.leave-block-scope>
          || <.missing-block($borg, $has-mystery)>  # OR give up
        ]
    }

    # Parsing the statements between { }
    token blockoid {
        :my $borg := $*BORG;                        # keep current context
        :my $has-mystery := 0; # TODO
        :my $*MULTINESS := '';
        :my @*PARENT-NESTINGS := self.PARENT-NESTINGS();
        :my @*ORIGIN-NESTINGS := [];
        { $*BORG := {} }                            # initialize new context
        [
          | '{YOU_ARE_HERE}' <you_are_here>        # TODO core setting
          | :dba('block')
            '{'                                     # actual block start
            <statementlist=.key-origin('statementlist')>
            [<.cheat-heredoc> || '}']               # actual block end
            <?end-statement>                        # XXX
          || <.missing-block($borg, $has-mystery)>  # OR give up
        ]
    }

    token you_are_here {
        <?{ nqp::getlexdyn('$?FILES') ~~ /\.setting$/ }> ||
            {self.typed-panic: 'X::Syntax::Reserved',
                reserved => 'use of {YOU_ARE_HERE} outside of a setting',
                instead => ' (use whitespace if not a setting, or rename file with .setting extension?)'
            }
    }

    # Parsing any unit scoped block (either package or sub)
    token unit-block($decl, $kind = 'Block', :$parameterization) {
        :my $*BLOCK;
        {                                           # entry check
            unless $*SCOPE eq 'unit' {
                $/.panic("Semicolon form of '$decl' without 'unit' is illegal. You probably want to use 'unit $decl'");
            }
        }
        { $*IN-DECL := ''; }                        # not inside declaration
        <.enter-block-scope($kind, $parameterization)>
        {
            if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                $*BLOCK.ensure-parse-performed($*R, $*CU.context);
            }
        }
        <statementlist=.key-origin('statementlist')>
        <.leave-block-scope>
    }

    # Helper token to set the kind of scope a block is in, *and* have
    # any appropriate actions executed on them
    token enter-block-scope($*SCOPE-KIND, $*PARAMETERIZATION = Mu) { <?> }

    # Helper token to make the actions handle the end of a scope
    token leave-block-scope() { <?> }

#-------------------------------------------------------------------------------
# Statement control

    proto rule statement-control {*}

    # Simple control statements that take a block
    rule statement-control:sym<default> { <.block-default><.kok>  <block> }
    rule statement-control:sym<CATCH>   { <.phaser-CATCH><.kok>   <block> }
    rule statement-control:sym<CONTROL> { <.phaser-CONTROL><.kok> <block> }

    # Simple control statements that take a pointy block
    rule statement-control:sym<given> {
        <.block-given><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
    }
    rule statement-control:sym<when> {
        <.block-when><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
    }

    # Handle "while" / "until"
    rule statement-control:sym<while> {
        :my $*WHILE := 1;
        [<.block-while>|<.block-until>{$*WHILE := 0}]<.kok>
        {}
        :my $*GOAL := '{';
        :my $*BORG := {};
        :my $*IN-LOOP := 1;
        <EXPR>
        <pointy-block>
    }

    # Control statements that take a pointy block without else/elsif/orwith
    rule statement-control:sym<unless> {
        <.block-unless><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
        [ <!before [<.block-else>|<.block-elsif>|<.block-orwith>]» >
            || $<keyword>=[<.block-else>|<.block-elsif>|<.block-orwith>]» {}
               {self.typed-panic: 'X::Syntax::UnlessElse',
                   keyword => ~$<keyword>,
               }
        ]
    }
    rule statement-control:sym<without> {
        <.block-without><.kok>
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
        [ <!before [<.block-else>|<.block-elsif>|<.block-orwith>]» >
            || $<keyword>=[<.block-else>|<.block-elsif>|<block-orwith>]» {}
               {self.typed-panic: 'X::Syntax::WithoutElse',
                   keyword => ~$<keyword>,
               }
        ]
    }

    # Handle "if" / "with"
    rule statement-control:sym<if> {
        :my @*IF-PARTS;
        [<.block-if>{@*IF-PARTS.push('If')}|<.block-with>{@*IF-PARTS.push('With')}]<.kok>
        {}
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>            # initial condition
        <pointy-block>    # initial body
        [                 # any elsifs/orwiths
          [
            | [<.block-elsif>{@*IF-PARTS.push('Elsif')}|<.block-orwith>{@*IF-PARTS.push('Orwith')}]
              <EXPR>
              <pointy-block>

            | $<what>=[<.block-else>\h*<.block-if>|elif]
              <.malformed: ~$<what>, 'Elsif'>
          ]
        ]*
        {}
        [
            <.block-else>
            <else=.pointy-block>
        ]?
    }

    # Helper rule for possible exclusion in the future, and to allow
    # slangs adapting the "for" syntax to be made easier
    rule vetPerlForSyntax {
        [ <?before 'my'? '$'\w+\s+'(' >
          <.typed-panic: 'X::Syntax::P5'>
        ]?
        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
          <.obs: 'C-style "for (;;)" loop', '"loop (;;)"'>
        ]?
    }

    # Handle "for"
    rule statement-control:sym<for> {
        <.block-for><.kok>
        {}
        <.vetPerlForSyntax>
        :my $*GOAL := '{';
        :my $*BORG := {};
        :my $*IN-LOOP := 1;
        <EXPR>
        <pointy-block>
    }

    # Handle repeat ... while | until
    rule statement-control:sym<repeat> {
        :my $*WHILE := 1;
        <.block-repeat><.kok>
        {}
        [
          | [<.block-while>|<.block-until>{$*WHILE := 0}]<.kok>
            :my $*GOAL := '{';
            :my $*BORG := {};
            :my $*IN-LOOP := 1;
            <EXPR>
            <pointy-block>
          | <pointy-block>
            [
                 [<.block-while>|<.block-until>{$*WHILE := 0}]<.kok>
              || <.missing: '"while" or "until"'>
            ]
            <EXPR>
        ]
    }

    # Handle whenever inside supply / react
    rule statement-control:sym<whenever> {
        <.block-whenever><.kok>
        [
          || <?{ self.language-revision == 1 || $*WHENEVER-COUNT >= 0 }>
          || <.typed-panic: 'X::Comp::WheneverOutOfScope'>
        ]
        { $*WHENEVER-COUNT++ }
        :my $*GOAL := '{';
        :my $*BORG := {};
        <EXPR>
        <pointy-block>
    }

    # Handle basic loop / C-style loop
    token statement-control:sym<loop> {
        <.block-loop><.kok>
        :s''
        [
          :my int $exprs;
          '('
          [
            <e1=.EXPR>? { $exprs := 1 if $<e1> }
            [
              ';' <e2=.EXPR>? { $exprs := 2 }
              [
                ';' <e3=.EXPR>? { $exprs := 3 }
              ]?
            ]?
          ]? # succeed anyway, this will leave us with a nice cursor
          [
            || <?{ $exprs == 3 }> ')'
            || <?before ')'>
               [
                 || <?{ $exprs == 0 }>
                    <.malformed:
                      "loop spec (expected 3 semicolon-separated expressions)"
                    >
                 || <.malformed:
                      "loop spec (expected 3 semicolon-separated expressions but got {$exprs})"
                    >
               ]
            || <?before ‘;’>
               <.malformed:
                 "loop spec (expected 3 semicolon-separated expressions but got more)"
               >
            || <.malformed: "loop spec">
          ]
        ]?
        :my $*IN-LOOP := 1;
        <block>
    }

    # handle people coming from Perl
    rule statement-control:sym<foreach> {
        <.sym><.end-keyword>
        <.obs: "'foreach'", "'for'">
    }

    # Not really control statements, more grammar tweaks
    rule statement-control:sym<also> {
        <.stmt-prefix-also><.kok>
        [
          <trait>+
          || <.panic: "No valid trait found after 'also'">
        ]
    }

#-------------------------------------------------------------------------------
# Pragma and module loading related statements

    token statement-control:sym<no> {
        <.use-no>
        <.ws>
        <module-name=.longname> [ <.spacey> <arglist> ]?
        <.ws>
    }

    token statement-control:sym<use> {
        # TODO this is massively simplified
        <.use-use>
        <.ws>
        [
          | <version>
            { $/.typed-panic: 'X::Language::TooLate', version => ~$<version> }

          | <module-name=.longname>
            [ <.spacey> <arglist> <.cheat-heredoc>? ]?
        ]
        <.ws>
    }

    rule statement-control:sym<need> {
        <.use-need>
        [
          | <version>
            <.sorry('In case of using pragma, use "use" instead (e.g., "use v6;", "use v6.c;").')>

          | <module-name=.longname>
        ]+ % ','
    }

    token statement-control:sym<import> {
        :my $*IN-DECL := 'import';
        <.use-import> <.ws>
        <module-name=.longname>
        [ <.spacey> <arglist> ]?
        <.ws>
    }

    rule statement-control:sym<require> {
        <.use-require>
        [
          | <module-name=.longname>
          | <file=.variable>
          | <!sigil> <file=.term>
        ]
        <EXPR>?
    }

#-------------------------------------------------------------------------------
# Statement modifiers

    # Helper method for error handling
    method nomodexpr($k) {
        self.'!clear_highwater'();
        self.typed-panic: 'X::Syntax::Confused',
          reason => "Missing expression for '$k' statement modifier";
        self
    }

    # Helper token for error handling
    token modifier-expr($k) { <EXPR> || <.nomodexpr($k)> }

    # Simple statement modifiers
    proto rule statement-mod-cond {*}
    rule statement-mod-cond:sym<if> {
        <.modifier-if><.kok> <modifier-expr('if')>
    }
    rule statement-mod-cond:sym<unless> {
        <.modifier-unless><.kok> <modifier-expr('unless')>
    }
    rule statement-mod-cond:sym<when> {
        <.modifier-when><.kok> <modifier-expr('when')>
    }
    rule statement-mod-cond:sym<with> {
        <.modifier-with><.kok> <modifier-expr('with')>
    }
    rule statement-mod-cond:sym<without> {
        <.modifier-without><.kok> <modifier-expr('without')>
    }

    proto rule statement-mod-loop {*}
    rule statement-mod-loop:sym<for> {
        <.modifier-for><.kok> <modifier-expr('for')>
    }
    rule statement-mod-loop:sym<given> {
        <.modifier-given><.kok> <modifier-expr('given')>
    }
    rule statement-mod-loop:sym<until> {
        <.modifier-until><.kok> <modifier-expr('until')>
    }
    rule statement-mod-loop:sym<while> {
        <.modifier-while><.kok> <modifier-expr('while')>
    }

#-------------------------------------------------------------------------------
# Phasers

    # Simple phasers that just take a blorst
    proto token statement-prefix {*}
    token statement-prefix:sym<BEGIN> { <.phaser-BEGIN> <.kok> <blorst> }
    token statement-prefix:sym<CHECK> { <.phaser-CHECK> <.kok> <blorst> }
    token statement-prefix:sym<CLOSE> { <.phaser-CLOSE> <.kok> <blorst> }
    token statement-prefix:sym<END>   { <.phaser-END>   <.kok> <blorst> }
    token statement-prefix:sym<ENTER> { <.phaser-ENTER> <.kok> <blorst> }
    token statement-prefix:sym<FIRST> { <.phaser-FIRST> <.kok> <blorst> }
    token statement-prefix:sym<INIT>  { <.phaser-INIT>  <.kok> <blorst> }
    token statement-prefix:sym<KEEP>  { <.phaser-KEEP>  <.kok> <blorst> }
    token statement-prefix:sym<LAST>  { <.phaser-LAST>  <.kok> <blorst> }
    token statement-prefix:sym<LEAVE> { <.phaser-LEAVE> <.kok> <blorst> }
    token statement-prefix:sym<NEXT>  { <.phaser-NEXT>  <.kok> <blorst> }
    token statement-prefix:sym<POST>  { <.phaser-POST>  <.kok> <blorst> }
    token statement-prefix:sym<PRE>   { <.phaser-PRE>   <.kok> <blorst> }
    token statement-prefix:sym<QUIT>  { <.phaser-QUIT>  <.kok> <blorst> }
    token statement-prefix:sym<TEMP>  { <.phaser-TEMP>  <.kok> <blorst> }
    token statement-prefix:sym<UNDO>  { <.phaser-UNDO>  <.kok> <blorst> }

    # DOC phaser also takes a "sub" phaser identifier
    token statement-prefix:sym<DOC> {
        :my $*DOC-PHASER;
        <.phaser-DOC>
        <.kok>
        [
             <.phaser-BEGIN> { $*DOC-PHASER := 'Begin' }
          || <.phaser-CHECK> { $*DOC-PHASER := 'Check' }
          || <.phaser-INIT>  { $*DOC-PHASER := 'Init'  }
        ]
        <.end-keyword>
        <.ws>
        <blorst>
    }

#-------------------------------------------------------------------------------
# Statement prefixes

    # Generic "BLock OR STatement" token
    token blorst {
        [
          | <?[{]>
            <block>

          | <![;]>
            <block=.statement>
            <.cheat-heredoc>?
              || <.missing: 'block or statement'>
        ]
    }

    # Simple prefixes that just take a blorst
    token statement-prefix:sym<do> {
        <.sym> <.kok> <blorst>
#        <.stmt-prefix-do> <.kok> <blorst>  # XXX needs fixing
    }
    token statement-prefix:sym<eager> {
        <.stmt-prefix-eager> <.kok> <blorst>
    }
    token statement-prefix:sym<sink> {
        <.stmt-prefix-sink> <.kok> <blorst>
    }
    token statement-prefix:sym<gather> {
        <.stmt-prefix-gather> <.kok> <blorst>
    }
    token statement-prefix:sym<once> {
        <.stmt-prefix-once> <.kok> <blorst>
    }
    token statement-prefix:sym<quietly> {
        <.stmt-prefix-quietly> <.kok> <blorst>
    }
    token statement-prefix:sym<start> {
        <.stmt-prefix-start> <.kok> <blorst>
    }
    token statement-prefix:sym<try> {
        <.stmt-prefix-try> <.kok> <blorst>
        <.set_braid_from(self)>
    }

    # Prefixes that work differently on for loops
    token statement-prefix:sym<hyper> {
        <.stmt-prefix-hyper> <.kok> <blorst>
    }
    token statement-prefix:sym<lazy> {
        <.stmt-prefix-lazy> <.kok> <blorst>
    }
    token statement-prefix:sym<race> {
        <.stmt-prefix-race> <.kok> <blorst>
    }

    # Prefixes that allow "whenever" inside them
    token statement-prefix:sym<react> {
        :my $*WHENEVER-COUNT := 0;
        <.stmt-prefix-react> <.kok> <blorst>
    }
    token statement-prefix:sym<supply> {
        :my $*WHENEVER-COUNT := 0;
        <.stmt-prefix-supply> <.kok> <blorst>
    }

#-------------------------------------------------------------------------------
# Expression parsing

    # Helper methods for throwing exceptions
    method EXPR-nonassoc($cur, $left, $right) {
        self.typed-panic: 'X::Syntax::NonAssociative',
          :left(~$left), :right(~$right);
    }
    method EXPR-nonlistassoc($cur, $left, $right) {
        self.typed-panic: 'X::Syntax::NonListAssociative',
          :left(~$left), :right(~$right);
    }

    # Helper method to obtain OperatorProprties for the given node
    # with error checking
    method properties-for-node($node) {
        (my $ast := $node.ast)
          ?? $ast.properties
          !! $node<colonpair>
            ?? self.actions.OperatorProperties.postfix(':')
            !! nqp::die("Internal error: no properties for: " ~ $node.dump)
    }

    # The EXPR method implements an operator precedence parsing algorithm.
    # One needs a stack for that and there's not a neat way to express it
    # within the rule language.
    method EXPR(str $preclim = '') {
        my $*LEFTSIGIL := '';
        my int $noinfix := $preclim eq 'y=';

        my $here    := self.new-cursor;
        my int $pos := $here.from;
        my str $rx  := 'termish';

        my @opstack;
        my @termstack;
        my $infix;
        my $inprop;
        my str $inprec;
        my str $opprec;
        my int $more_infix;
        my int $term_done;

        while 1 {
            $here.set-pos($pos);
            my $termcur := $here."$rx"();
            $pos        := $termcur.pos;
            $here.set-pos($pos);
            if $pos < 0 {
                $here.panic('Missing required term after infix')
                  if nqp::elems(@opstack);
                return $here;
            }

            # Interleave any prefix/postfix we might have found.
            my $termish  := $termcur.MATCH;
            my %termOPER := $termish;
            %termOPER := nqp::atkey(%termOPER,'OPER')
                while nqp::existskey(%termOPER,'OPER');
            my @prefixish  := nqp::atkey(%termOPER,'prefixish');
            my @postfixish := nqp::atkey(%termOPER,'postfixish');

            # Both prefixes as well as postfixes found
            unless nqp::isnull(@prefixish) || nqp::isnull(@postfixish) {
                while nqp::elems(@prefixish) && nqp::elems(@postfixish) {
                    my $preprec := self.properties-for-node(
                      @prefixish[0]
                    ).sub-or-precedence;
                    my $postprec := self.properties-for-node(
                      @postfixish[nqp::elems(@postfixish) - 1]
                    ).sub-or-precedence;

                    $postprec gt $preprec
                      ?? nqp::push(@opstack, nqp::shift(@prefixish))
                      !! $postprec lt $preprec
                        ?? nqp::push(@opstack, nqp::pop(@postfixish))
                        !! self.EXPR-nonassoc(
                             $here, ~@prefixish[0], ~@postfixish[0]
                           );
                }
                nqp::push(@opstack, nqp::shift(@prefixish))
                  while nqp::elems(@prefixish);
                nqp::push(@opstack, nqp::pop(@postfixish))
                  while nqp::elems(@postfixish);
            }
            nqp::deletekey($termish, 'prefixish');
            nqp::deletekey($termish, 'postfixish');
            nqp::push(@termstack, nqp::atkey($termish, 'term'));
            my $*TERMISH := $termish;

            last if $noinfix;

            $more_infix := 1;
            $term_done  := 0;
            while $more_infix {

                # Now see if we can fetch an infix operator
                # First, we need ws to match.
                $here.set-pos($pos);
                $pos := $here.ws.pos;
                if $pos < 0 {
                    $term_done := 1;
                    last;
                }

                # Next, try the infix itself.
                $here.set-pos($pos);
                $infix := $here.infixish;
                $pos := $infix.pos;
                if $pos < 0 {
                    $term_done := 1;
                    last;
                }

                # We got an infix.
                $inprop := self.properties-for-node($infix);
                $rx     := $inprop.next-term;
                $inprec := $inprop.precedence;
                if $inprec le $preclim {  # lower or no precedence
                    if $inprec {
                        $term_done := 1;
                        last;
                    }
                    else {
                        $infix.panic('Missing infixish operator precedence');
                    }
                }

                while nqp::elems(@opstack) {
                    my $ast := @opstack[nqp::elems(@opstack)-1].ast;
                    $opprec := $ast
                      ?? $ast.properties.sub-or-precedence
                      !! "";
                    last unless $opprec gt $inprec;
                    self.EXPR-reduce(@termstack, @opstack);
                }

                if $inprop.adverb {
                    nqp::push(@opstack, $infix);
                    self.EXPR-reduce(@termstack, @opstack);
                }
                else {
                    $more_infix := 0;
                }
            }
            last if $term_done;

            # if equal precedence, use associativity to decide
            if $opprec eq $inprec {
                my str $inassoc := $inprop.associative;
                if $inassoc eq 'non' {
                    self.EXPR-nonassoc(
                      $infix,
                      @opstack[nqp::elems(@opstack)-1]<OPER>.Str,
                      $infix.Str
                    );
                }
                elsif $inassoc eq 'left' || $inassoc eq 'chain' {
                    # left associative, reduce immediately
                    self.EXPR-reduce(@termstack, @opstack);
                }
                elsif $inassoc eq 'list' {
                    my $op1 := @opstack[nqp::elems(@opstack)-1]<OPER>.Str;
                    my $op2 := $infix.Str;
                    self.EXPR-nonlistassoc($infix, $op1, $op2)
                      if $op1 ne $op2 && $op1 ne ':';
                }
            }

            nqp::push(@opstack, $infix); # The Shift
            $here.set-pos($pos);
            $pos := $here.ws.pos;
            $here.set-pos($pos);
            return $here if $pos < 0;
        }

        self.EXPR-reduce(@termstack, @opstack) while nqp::elems(@opstack);

        self.actions.EXPR(self.match-with-at(nqp::pop(@termstack), $here.pos))
    }

    method EXPR-reduce(@termstack, @opstack) {
        # if there is no @opstack, we have nowhere for the contents of @termstack to go...
        #   (@termstack comes in with size 1 for this edge-case, so we can just pass the first argument)
        return self.typed-sorry("X::Syntax::AmbiguousAdverb", adverb => ~nqp::atpos(@termstack, 0))
            unless nqp::elems(@opstack);

        my $op := nqp::pop(@opstack);

        # Give it a fresh capture list, since we'll have assumed it has
        # no positional captures and not taken them.
        nqp::bindattr($op,NQPCapture,'@!list',nqp::list);

        # Some shortcuts
        my $properties := self.properties-for-node($op);
        my $actions    := self.actions;

        my str $opassoc := $properties.associative;
        if $opassoc eq 'unary' {
            my $arg := nqp::pop(@termstack);
            $op[0]  := $arg;
            if $arg.from < $op.from {
                if $op<colonpair> -> $cp {
                    my $ast := $cp.ast;
                    $ast.set-key($op.adverb-pc2str($ast.key));
                }
                $actions.POSTFIX-EXPR($op)
            }
            else {
                $actions.PREFIX-EXPR($op);
            }
        }

        elsif $opassoc eq 'list' {
            nqp::unshift($op, nqp::pop(@termstack));
            my str $sym := nqp::ifnull(
              nqp::atkey(
                nqp::atkey($op,'OPER'),
                'sym'
              ),
              ''
            );
            while @opstack {
                last if $sym ne nqp::ifnull(
                  nqp::atkey(
                    nqp::atkey(
                      nqp::atpos(@opstack,nqp::elems(@opstack) - 1),
                      'OPER'
                    ),
                  'sym'),
                  ''
                );
                nqp::unshift($op, nqp::pop(@termstack));
                nqp::pop(@opstack);
            }
            nqp::unshift($op, nqp::pop(@termstack));
            $actions.LIST-EXPR($op);
        }

        else { # infix op assoc: left|right|ternary|...
            $op[1] := nqp::pop(@termstack); # right
            $op[0] := nqp::pop(@termstack); # left

            $properties.ternary
              ?? $actions.TERNARY-EXPR($op)
              !! $actions.INFIX-EXPR($op);
        }
        nqp::push(@termstack, $op);
    }

#-------------------------------------------------------------------------------
# Operators

    # Look for infix operator or adverb looking like one
    token infixish($IN-META = nqp::getlexdyn('$*IN-META')) {
        :my $*IN-META := $IN-META;
        :my $*OPER;
        <!stdstopper>
        <!infixstopper>
        :dba('infix')
        [
          | <!{ $*IN_REDUCE }>
            <colonpair>
            <adverb-as-infix>
            { $*OPER := $<adverb-as-infix> }

          | [
              | :dba('bracketed infix')
                '[' ~ ']' <infixish('[]')>
                { $*OPER := $<infixish><OPER> }

              | :dba('infixed function')
                <?before '[&' <twigil>? [ <alpha> | '(' ] >
                '[' ~ ']' <variable>
                {
                    $*OPER := $<variable>;
                    self.check-variable($<variable>);
                }

              | <infix-circumfix-meta-operator>
                { $*OPER := $<infix-circumfix-meta-operator> }

              | <infix-prefix-meta-operator>
                { $*OPER := $<infix-prefix-meta-operator> }

              | <infix>
                { $*OPER := $<infix> }

              | <?{ $*IN-META ~~ /^[ '[]' | 'hyper' | 'HYPER' | 'R' | 'S' ]$/
                      && !$*IN_REDUCE
                }>
                <.missing: "infix inside " ~ $*IN-META>
            ]
            [ <?before '='>
              <infix-postfix-meta-operator>
              { $*OPER := $<infix-postfix-meta-operator> }
            ]?
        ]
        <OPER=.match-with($*OPER)>
        { $<OPER>.set-pos($*OPER.pos) }
    }

    token adverb-as-infix {
        { $*ADVERB-AS-INFIX := 1 }
    }

    regex infixstopper {
        :dba('infix stopper')
        [
          | <?before '!!'>
            <?{ $*GOAL eq '!!' }>
          | <?before '{' | <.pointy-block-starter> >
            <?MARKED('ws')>
            <?{ $*GOAL eq '{' || $*GOAL eq 'endargs' }>
        ]
    }
#-------------------------------------------------------------------------------
    method can-meta($op, $meta, $reason = "fiddly") {
        my $properties := self.properties-for-node($op);
        self.typed-panic: "X::Syntax::CannotMeta", :$meta, operator => ~$op<OPER>, dba => $properties.dba, reason => "too $reason"
            if $properties.ternary;
        self;
    }

    proto token infix-prefix-meta-operator {*}

    # !foo
    token infix-prefix-meta-operator:sym<!> {
        <.sym>
        <![!]>
        {}
        [    <infixish('neg')>
          || <.panic: "Negation metaoperator not followed by valid infix">
        ]
    }

    # Rfoo
    token infix-prefix-meta-operator:sym<R> {
        <sym=.meta-R> <infixish('R')> {}
        <.can-meta($<infixish>, "reverse the args of")>
    }

    # Sfoo
    token infix-prefix-meta-operator:sym<S> {
        <sym=.meta-S> <infixish('S')> {}
        <.can-meta($<infixish>, "sequence the args of")>
    }

    # Xfoo
    token infix-prefix-meta-operator:sym<X> {
        <sym=.meta-X> <infixish('X')> {}
        <.can-meta($<infixish>, "cross with")>
    }

    # Zfoo
    token infix-prefix-meta-operator:sym<Z> {
        <sym=.meta-Z> <infixish('Z')> {}
        <.can-meta($<infixish>, "zip with")>
    }

#-------------------------------------------------------------------------------

    proto token infix-postfix-meta-operator {*}

    # foo=
    token infix-postfix-meta-operator:sym<=> {
        '='
    }

#-------------------------------------------------------------------------------

    proto token infix-circumfix-meta-operator {*}

    # «foo»
    token infix-circumfix-meta-operator:sym<« »> {
        $<opening>=[ '«' | '»' ]
        {}
        <infixish('hyper')>
        $<closing>=[ '«' | '»' || <.missing: "« or »"> ]
        {}
    }

    # <<foo>>
    token infix-circumfix-meta-operator:sym«<< >>» {
        $<opening>=[ '<<' | '>>' ]
        {}
        <infixish('HYPER')>
        $<closing>=[ '<<' | '>>' || <.missing: "<< or >>"> ]
        {}
    }

#-------------------------------------------------------------------------------

    token prefixish {
        :dba('prefix')
        <OPER=prefix>
        <prefix-postfix-meta-operator>?
        <.ws>
    }

    proto token prefix-postfix-meta-operator {*}

    token prefix-postfix-meta-operator:sym<«> {
        <.sym> | '<<'
    }

#-------------------------------------------------------------------------------

    token postfixish {
        <!stdstopper>

        # last whitespace didn't end here
        <?{
            my $c      := $/;
            my $marked := $c.MARKED('ws');
            !$marked || $marked.from == $c.pos;
        }>

        [
          <!{ $*QSIGIL }> [ <.unspace> | '\\' ]
        ]?

        :dba('postfix')
        [
          ['.' <.unspace>?]? <postfix-prefix-meta-operator> <.unspace>?]?
        [
          | <OPER=postfix>

          # dotted form of postfix operator (non-wordy only)
          | '.' <?before \W> <OPER=postfix>

          | <OPER=postcircumfix>

          | '.' <?[ [ { < ]> <OPER=postcircumfix>

          | <OPER=dotty>

          | <OPER=privop>

          | <?{ $<postfix-prefix-meta-operator> && !$*QSIGIL }>
            [
              || <?space> <.missing: "postfix">
              || <?alpha> <.missing: "dot on method call">
              || <.malformed: "postfix">
            ]
        ]
        { $*LEFTSIGIL := '@'; }
    }

    proto token postfix-prefix-meta-operator {*}

    token postfix-prefix-meta-operator:sym<»> {
        [ <sym> | $<sym> = '>>' ]
        [ <!{ $*QSIGIL }> || <![(]> ]
    }

    token postop {
        | <postfix>
          $<sym> = {$<postfix><sym>}

        | <postcircumfix>
          $<sym> = {$<postcircumfix><sym>}
    }

#-------------------------------------------------------------------------------

    proto token postcircumfix {*}

    token postcircumfix:sym<( )> {
        :dba('argument list')
        '(' ~ ')' [ <.ws> <arglist> ]
    }

    token postcircumfix:sym<[ ]> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '[' ~ ']' [ <.ws> <semilist> ]
    }

    token postcircumfix:sym<{ }> {
        :my $*QSIGIL := '';
        :dba('subscript')
        '{' ~ '}' [ <.ws> <semilist> ]
    }

    token postcircumfix:sym<ang> {
        '<'
        [
          || <nibble(self.quote-qw)>
             '>'

          || '='* <?before \h* [ \d | <.sigil> | ':' ] >
             { $/.panic: "Whitespace required before $/ operator" }

          || { $/.typed-panic: 'X::QuoteWords::Missing::Closer',
                 opener => '<',
                 closer => '>',
                 line   => HLL::Compiler.lineof($/.orig, $/.from, :cache(1))
             }
        ]
    }

    token postcircumfix:sym«<< >>» {
        :dba('shell-quote words')
        '<<'
        [
          || <nibble(self.quote-qqw)>
             '>>'

          || { $/.typed-panic: 'X::QuoteWords::Missing::Closer',
                 opener => '<<',
                 closer => '>>',
                 line   => HLL::Compiler.lineof($/.orig, $/.from, :cache(1))
             }
        ]
    }

    token postcircumfix:sym<« »> {
        :dba('shell-quote words')
        '«'
        [
          || <nibble(self.quote-qqw("«", "»"))>
             '»'

          || { $/.typed-panic: 'X::QuoteWords::Missing::Closer',
                 opener => '«',
                 closer => '»',
                 line   => HLL::Compiler.lineof($/.orig, $/.from, :cache(1))
             }
        ]
    }

#-------------------------------------------------------------------------------

    proto token dotty {*}
    token dotty:sym<.> {
        <.sym>
        <dottyop>
    }

    token dotty:sym<.^> {
        <.sym>
        <dottyop('.^')>
    }

    token dotty:sym<.?> {
        <.sym>
        <dottyop('.?')>
    }

    token dotty:sym<.&> {
        <.sym>
        <dottyop('.&')>
    }

    token dotty:sym<.+> {
        <.sym>
        <dottyop('.+')>
    }

    token dotty:sym<.*> {
        <.sym>
        <dottyop('.*')>
    }

    token dotty:sym<.=> {
        <.sym>
        <dottyop('.=')>
    }

    token dottyop($special?) {
        :dba('dotty method or postfix')
        <.unspace>?
        [
          | <methodop($special)>

          | <colonpair>

          | <!alpha>
            <postop>
            $<sym> = {$<postop><sym>}
            <.dotty-non-ident($special)>
        ]
    }

    token privop {
        '!'
        <methodop('!')>
    }

    token methodop($*DOTTY) {
        [
        | <longname> {
              self.malformed("class-qualified postfix call")
                if $<longname> eq '::';
          }
        | <?[$@&]> <variable> { self.check-variable($<variable>) }
        | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <.-["]>*? [\s|$] > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'."> ]
        ]
        <.unspace>?
        :dba('method arguments')
        [
          [
            | <?[(]> <args>
            | ':' <?before \s | '{'> <!{ $*QSIGIL }> <args=.arglist>
          ]
          || <!{ $*QSIGIL }> <?>
          || <?{ $*QSIGIL }> <?[.]> <?>
        ]
        <.unspace>?
    }

    token dotty-non-ident($dotty) {
        | <!{ $dotty }>
        | <.panic: "Cannot use $dotty on a non-identifier method call">
    }

#-------------------------------------------------------------------------------

    token super-sign    { <[⁻⁺¯]> }
    token sub-sign      { <[₊₋]>  }
    token super-integer { <[⁰¹²³⁴⁵⁶⁷⁸⁹]>+ }
    token sub-integer   { <[₀₁₂₃₄₅₆₇₈₉]>+ }

    token power {
        <super-sign>? <super-integer>
    }
    token vulgar {
          <[ ½ ↉ ⅓ ⅔ ¼ ¾ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅐ ⅛ ⅜ ⅝ ⅞ ⅑ ⅒ ]>
        | [ <super-integer> [ '/' | '⁄' ] <sub-integer> ]
    }

    token dottyopish { <term=.dottyop> }

    proto token postfix {*}
    token postfix:sym<i> { <sym> »  }
    token postfix:sym<ⁿ> { <power>  }
    token postfix:sym<+> { <vulgar> }

    token postfix:sym<++>  { <sym> }
    token postfix:sym<-->  { <sym> }
    token postfix:sym<⚛++> { <sym> }
    token postfix:sym<⚛--> { <sym> }

    # TODO: report the correct bracket in error message
    token postfix:sym«->» {
        $<sym>=[ '->' | '→' ]
        [
          | ['[' | '{' | '(' ]
            <.obs: '->(), ->{} or ->[] as postfix dereferencer',
              '.(), .[] or .{} to deref, or whitespace to delimit a pointy block'>

          | <.obs: '-> as postfix',
              'either . to call a method, or whitespace to delimit a pointy block'>
        ]
    }

#-------------------------------------------------------------------------------# Prefixes

    proto token prefix {*}
    token prefix:sym<++>  { <sym> }
    token prefix:sym<-->  { <sym> }
    token prefix:sym<++⚛> { <sym> }
    token prefix:sym<--⚛> { <sym> }

    token prefix:sym<~~> { <sym> <.dupprefix: '~~'> }
    token prefix:sym<??> { <sym> <.dupprefix: '??'> }
    token prefix:sym<^^> { <sym> <.dupprefix: '^^'> }

    token prefix:sym<?> { <sym> <!before '??'> }
    token prefix:sym<!> { <sym> <!before '!!'> }

    token prefix:sym<+>  { <sym> }
    token prefix:sym<~>  { <sym> }
    token prefix:sym<->  { <sym> }
    token prefix:sym<−>  { <sym> }
    token prefix:sym<|>  { <sym> }
    token prefix:sym<+^> { <sym> }
    token prefix:sym<~^> { <sym> }
    token prefix:sym<?^> { <sym> }
    token prefix:sym<⚛>  { <sym> }
    token prefix:sym<^> {
        <sym>
        <?before \d+ <?before \. <.?alpha> >
        <.worry: "Precedence of ^ is looser than method call; please parenthesize"> >?
    }

#-------------------------------------------------------------------------------
# Alpha prefixes that can be easily sub-classed for non-English versions of
# the Raku Programming Language

    # Prefixes requiring scope interaction
    token prefix:sym<let>  {
        <sym=.prefix-let>
        <.kok>
        { ($*BLOCK // $*CU.mainline).set-has-let }
    }
    token prefix:sym<temp> {
        <sym=.prefix-temp>
        <.kok>
        { ($*BLOCK // $*CU.mainline).set-has-temp }
    }

    token prefix:sym<so>  { <sym=.prefix-so>  <.end-prefix> }
    token prefix:sym<not> { <sym=.prefix-not> <.end-prefix> }

#-------------------------------------------------------------------------------
# Alpha infixes that may need a right word boundary, and which can be easily
# sub-classed for non-English versions of the Raku Programming Language

    proto token infix {*}
    token infix:sym<after>      { <sym=.infix-after> »      }
    token infix:sym<and>        { <sym=.infix-and> »        }
    token infix:sym<andthen>    { <sym=.infix-andthen> »    }
    token infix:sym<before>     { <sym=.infix-before> »     }
    token infix:sym<but>        { <sym=.infix-but> »        }
    token infix:sym<cmp>        { <sym=.infix-cmp> »        }
    token infix:sym<(cont)>     { <sym=.infix-pcontp>       }
    token infix:sym<coll>       { <sym=.infix-coll> »       }
    token infix:sym<div>        { <sym=.infix-div> »        }
    token infix:sym<does>       { <sym=.infix-does> »       }
    token infix:sym<(elem)>     { <sym=.infix-pelemp>       }
    token infix:sym<eq>         { <sym=.infix-eq> »         }
    token infix:sym<eqv>        { <sym=.infix-eqv> »        }
    token infix:sym<ff>         { <sym=.infix-ff> »         }
    token infix:sym<^ff>        { <sym=.infix-cff> »        }
    token infix:sym<ff^>        { <sym=.infix-ffc>          }
    token infix:sym<^ff^>       { <sym=.infix-cffc>         }
    token infix:sym<fff>        { <sym=.infix-fff> »        }
    token infix:sym<^fff>       { <sym=.infix-cfff> »       }
    token infix:sym<fff^>       { <sym=.infix-fffc>         }
    token infix:sym<^fff^>      { <sym=.infix-cfffc>        }
    token infix:sym<gcd>        { <sym=.infix-gcd> »        }
    token infix:sym<ge>         { <sym=.infix-ge> »         }
    token infix:sym<gt>         { <sym=.infix-gt> »         }
    token infix:sym<lcm>        { <sym=.infix-lcm> »        }
    token infix:sym<le>         { <sym=.infix-le> »         }
    token infix:sym<leg>        { <sym=.infix-leg> »        }
    token infix:sym<lt>         { <sym=.infix-lt> »         }
    token infix:sym<max>        { <sym=.infix-max> »        }
    token infix:sym<min>        { <sym=.infix-min> »        }
    token infix:sym<minmax>     { <sym=.infix-minmax> »     }
    token infix:sym<mod>        { <sym=.infix-mod> »        }
    token infix:sym<ne>         { <sym=.infix-ne> »         }
    token infix:sym<notandthen> { <sym=.infix-notandthen> » }
    token infix:sym<or>         { <sym=.infix-or> »         }
    token infix:sym<orelse>     { <sym=.infix-orelse> »     }
    token infix:sym<unicmp>     { <sym=.infix-unicmp> »     }
    token infix:sym<x>          { <sym=.infix-x> »          }
    token infix:sym<xor>        { <sym=.infix-xor> »        }
    token infix:sym<xx>         { <sym=.infix-xx> »         }

#-------------------------------------------------------------------------------
# Other infixes

    token infix:sym<**> { <sym> }

    # Dotty infixes
    token infix:sym<.> {
        <.sym>
        <ws>
        <!{ $*IN_REDUCE }>
        [
          <!alpha>
          {
              my $pre := self.preceding-char;
              $<ws> ne ''
                ?? $¢.obs('. to concatenate strings', '~')
                !! $pre ~~ /^\s$/
                  ?? $¢.malformed('postfix call (only basic method calls that exclusively use a dot can be detached)')
                  !! $¢.malformed('postfix call')
          }
        ]?
    }
    token infix:sym<.=> { <.sym> }

    # Assignment infixes
    token infix:sym<:=>  { <sym> }
    token infix:sym<::=> { <sym> <.NYI: '"::="'> }

    # Iffy multiplicative infixes
    token infix:sym<%%> { <sym> }
    token infix:sym<?&> { <sym> }

    # Multiplicatve infixes with meta interaction
    token infix:sym«+<» {
        <sym>
        [
             <!{ $*IN-META }>
          || <?before '<<'>
          || <![<]>
        ]
    }
    token infix:sym«+>» {
        <sym>
        [
             <!{ $*IN-META }>
          || <?before '>>'>
          || <![>]>
        ]
    }
    token infix:sym«~<» {
        <sym>
        [
             <!{ $*IN-META }>
          || <?before '<<'>
          || <![<]>
        ]
    }
    token infix:sym«~>» {
        <sym>
        [
             <!{ $*IN-META }>
          || <?before '>>'>
          || <![>]>
        ]
    }

    token infix:sym«<<» {
        <.sym>
        <!{ $*IN-META }>
        <?[\s]>
        <.sorryobs('<< to do left shift', '+< or ~<')>
    }
    token infix:sym«>>» {
        <.sym>
        <!{ $*IN-META }>
        <?[\s]>
        <.sorryobs('>> to do right shift', '+> or ~>')>
    }

    # Other multiplicative infixes
    token infix:sym<*>  { <sym> }
    token infix:sym<×>  { <sym> }
    token infix:sym</>  { <sym> }
    token infix:sym<÷>  { <sym> }
    token infix:sym<%>  { <sym> }
    token infix:sym<+&> { <sym> }
    token infix:sym<~&> { <sym> }

    token infix:sym<-> {  # 2D HYPHEN-MINUS -
       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
        <sym> [<?before '>>'> || <![>]>]
    }

    # Iffy additive infixes
    token infix:sym<?|> { <sym> }
    token infix:sym<?^> { <sym> }

    # Other additive infixes
    token infix:sym<−>  { <sym> }  # 2212 MINUS SIGN −
    token infix:sym<+>  { <sym> }
    token infix:sym<+|> { <sym> }
    token infix:sym<+^> { <sym> }
    token infix:sym<~|> { <sym> }
    token infix:sym<~^> { <sym> }

    # Concatenating infixes
    token infix:sym<~> { <sym> }
    token infix:sym<∘> { <sym> }
    token infix:sym<o> { <sym> }

    # Iffy junctive and infixes
    token infix:sym<&> { <sym> }

    # Other junctive and infixes
    token infix:sym<(&)> { <sym> }
    token infix:sym«∩»   { <sym> }
    token infix:sym<(.)> { <sym> }
    token infix:sym«⊍»   { <sym> }

    # Iffy junctive or infixes
    token infix:sym<|> { <sym> }
    token infix:sym<^> { <sym> }

    # Other junctive or infixes
    token infix:sym<(|)> { <sym> }
    token infix:sym«∪»   { <sym> }
    token infix:sym<(^)> { <sym> }
    token infix:sym«⊖»   { <sym> }
    token infix:sym<(+)> { <sym> }
    token infix:sym«⊎»   { <sym> }
    token infix:sym<(-)> { <sym> }
    token infix:sym«∖»   { <sym> }

    token infix:sym«!=» { <sym> <?before \s|']'>}

    # Other chaining infixes
    token infix:sym«=~=»    { <sym> }
    token infix:sym«≅»      { <sym> }
    token infix:sym«==»     { <sym> }
    token infix:sym«≠»      { <sym> }
    token infix:sym«<=»     { <sym> }
    token infix:sym«≤»      { <sym> }
    token infix:sym«>=»     { <sym> }
    token infix:sym«≥»      { <sym> }
    token infix:sym«<»      { <sym> }
    token infix:sym«>»      { <sym> }
    token infix:sym«=:=»    { <sym> }
    token infix:sym<===>    { <sym> }
    token infix:sym<⩶>      { <sym> }
    token infix:sym<~~>     { <sym> }
    token infix:sym<!~~>    { <sym> }
    token infix:sym«∈»      { <sym> }
    token infix:sym«∊»      { <sym> }
    token infix:sym«∉»      { <sym> }
    token infix:sym«∋»      { <sym> }
    token infix:sym«∌»      { <sym> }
    token infix:sym«(<)»    { <sym> }
    token infix:sym«⊂»      { <sym> }
    token infix:sym«⊄»      { <sym> }
    token infix:sym«(>)»    { <sym> }
    token infix:sym«⊃»      { <sym> }
    token infix:sym«⊅»      { <sym> }
    token infix:sym«(==)»   { <sym> }
    token infix:sym«≡»      { <sym> }
    token infix:sym«≢»      { <sym> }
    token infix:sym«(<=)»   { <sym> }
    token infix:sym«⊆»      { <sym> }
    token infix:sym«⊈»      { <sym> }
    token infix:sym«(>=)»   { <sym> }
    token infix:sym«⊇»      { <sym> }
    token infix:sym«⊉»      { <sym> }
    token infix:sym«(<+)»   { <sym> }
    token infix:sym«≼»      { <sym> }
    token infix:sym«(>+)»   { <sym> }
    token infix:sym«≽»      { <sym> }

    token infix:sym<&&> { <sym> }

    token infix:sym<||> { <sym> }
    token infix:sym<//> { <sym> }
    token infix:sym<^^> { <sym> }

    # Parsing of the ?? !! ternary is really treated as an infix, where the
    # left side is the condition, the right side is the "else" expression,
    # and the '?? expression !!' is initially parsed as the operator.
    token infix:sym<?? !!> {
        $<sym>=['??' | '⁇']
        <.ws>
        :my $*GOAL := '!!';
        <EXPR('i=')>
        [
             [ '!!' | '‼' ]
          || <?before '::' <.-[=]>>
             {self.typed-panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => "::"}
          || <?before ':' <.-[=\w]>>
             {self.typed-panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => ":"}
          || <infixish>
             {self.typed-panic("X::Syntax::ConditionalOperator::PrecedenceTooLoose", operator => ~$<infixish>)}
          || <?{ ~$<EXPR> ~~ / '!!' / }>
             <.typed-panic: "X::Syntax::ConditionalOperator::SecondPartGobbled">
          || <?before \N*? [\n\N*?]? '!!'>
             {self.typed-panic: "X::Syntax::Confused", reason => "Confused: Bogus code found before the !! of conditional operator"}
          || {self.typed-panic: "X::Syntax::Confused", reason => "Confused: Found ?? but no !!"}
        ]
    }

    token infix:sym<,> { <.unspace>? <sym> }
    token infix:sym<:> {
        <?{ $*INVOCANT_OK && $*GOAL ne '!!' }>
        <.unspace>? <sym> <?before \s | <.terminator> | $ >
        [ <?{ $*INVOCANT_OK }> || <.panic: "Invocant colon not allowed here"> ]
        { $*INVOCANT_OK := 0; }
    }

    token infix:sym<X> { <!before <.sym> <.infixish>> <sym> }
    token infix:sym<Z> { <!before <.sym> <.infixish>> <sym> }


    token infix:sym<...>   { <sym> }
    token infix:sym<…>     { <sym> }
    token infix:sym<...^>  { <sym> }
    token infix:sym<…^>    { <sym> }
    token infix:sym<^...>  { <sym> }
    token infix:sym<^…>    { <sym> }
    token infix:sym<^...^> { <sym> }
    token infix:sym<^…^>   { <sym> }

    token infix:sym<?> {
        <.sym>
        {}
        <![?]>
        <?before <.-[;]>*?':'>
        <.obs: '? and : for the ternary conditional operator', '?? and !!'>
    }

    token infix:sym<=> {
        <sym>
        :my $*ITEM := $*LEFTSIGIL eq '$' || $*IN-META;
        { $*LEFTSIGIL := '' }
    }

    token infix:sym«=>»  { <sym> }
    token infix:sym«⇒»   { <sym> }
    token infix:sym<⚛=>  { <sym> }
    token infix:sym<⚛+=> { <sym> }
    token infix:sym<⚛-=> { <sym> }
    token infix:sym<⚛−=> { <sym> }

    token infix:sym<..> {
        <sym>
        [
          <!{ $*IN-META }>
          <?[)\]]>
          <.panic: "Please use ..* for indefinite range">
        ]?
    }

    token infix:sym<^..>  { <sym> }
    token infix:sym<..^>  { <sym> }
    token infix:sym<^..^> { <sym> }
    token infix:sym«<=>»  { <sym> }

    token infix:sym«<==»  { <sym> }
    token infix:sym«==>»  { <sym> }
    token infix:sym«<<==» { <sym> }
    token infix:sym«==>>» { <sym> }

    token infix:sym<!~> {
        <.sym>
        \s
        <.obs: '!~ to do negated pattern matching', '!~~'>
    }
    token infix:sym<=~> {
        <.sym>
        <.obs: '=~ to do pattern matching', '~~'>
    }

#-------------------------------------------------------------------------------# Circumfixes

    proto token circumfix {*}
    token circumfix:sym<( )> {
        :my $*ADVERB-AS-INFIX := 0;
        :dba('parenthesized expression')
        '(' ~ ')' <semilist>
    }

    token circumfix:sym<[ ]> {
        :dba('array composer')
        '[' ~ ']' <semilist>
    }

    token circumfix:sym<{ }> {
        :my $*ADVERB-AS-INFIX := 0;
        <?[{]> <pointy-block>
        { $*BORG<block> := $<pointy-block> }
    }

    token circumfix:sym<ang> {
        :dba('quote words')
        '<' ~ '>'
        [
          [ <?before 'STDIN>'>
            <.obs: '<STDIN>',
              '$*IN.lines (or add whitespace to suppress warning)'>
          ]?

          [ <?[>]>
            <.obs: '<>',
              'lines() to read input, (\'\') to represent a null string or () to represent an empty list'>
          ]?

          <nibble(self.quote-qw)>
        ]
    }

    token circumfix:sym«<< >>» {
        :dba('shell-quote words')
        '<<' ~ '>>'
        <nibble(self.quote-qqw)>
    }

    token circumfix:sym<« »> {
        :dba('shell-quote words')
        '«' ~ '»'
        <nibble(self.quote-qqw("«", "»"))>
    }

#-------------------------------------------------------------------------------# Terms

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
          || [
               | <prefixish>+
                 [
                   || <term>

                   || {}
                      <.panic("Prefix "
                        ~ $<prefixish>[-1].Str
                        ~ " requires an argument, but no valid term found"
                        ~ ".\nDid you mean "
                        ~ $<prefixish>[-1].Str
                        ~ " to be an opening bracket for a declarator block?"
                      )>
                 ]

               | <term>
             ]

          || <!{ $*QSIGIL }>
             <?before <infixish> {
                 $/.typed-panic('X::Syntax::InfixInTermPosition',
                   infix => ~$<infixish>
                 );
             }>

          || <!>
        ]

        :dba('postfix')
        [
          || <?{ $*QSIGIL }>
             [
               || <?{ $*QSIGIL eq '$' }>
                  [ <postfixish>+!
                    <?{ bracket-ending($<postfixish>) }>
                  ]**0..1

               || <postfixish>+!
                  <?{ bracket-ending($<postfixish>) }>
             ]

          || <!{ $*QSIGIL }> <postfixish>*
        ]
        {
            if $*VAR {
                self.check-variable($*VAR);
                $*VAR := 0;
            }
        }
    }

    sub bracket-ending($matches) {
        my $check    := $matches[+$matches - 1];
        my str $str  := $check.Str;
        my str $last := nqp::substr($str, nqp::chars($str) - 1, 1);
        $last eq ')'
          || $last eq '}'
          || $last eq ']'
          || $last eq '>'
          || $last eq '»'
    }

    proto token term {*}

    token term:sym<circumfix> { <circumfix> }

    token term:sym<self> { <.term-self> <.end-keyword> }
    token term:sym<now>  { <.term-now>  <.end-keyword> }
    token term:sym<time> { <.term-time> <.end-keyword> }

    token term:sym<nano> {
        <?{ self.language-revision >= 3
              || $*R.is-identifier-known('&term:<nano>')
        }>
        <.term-nano> <.end-keyword>
    }

    token term:sym<empty_set> {
        "∅"
        <!before <.[ \( \\ ' \- ]> || <.fatty>>
    }

    token term:sym<rand> {
        <!{ $*LANG.pragma('p5isms') }>
        <.term-rand>
        »
        [ <?before '('?  \h* [\d|'$']>
          <.obs: 'rand(N)', 'N.rand for Num or (^N).pick for Int result'>
        ]?
        [ <?before '()'>
          <.obs: 'rand()', 'rand'>
        ]?
        <.end-keyword>
    }

    token term:sym<...> {
        [ <.sym> | '…' ]
        [ <?after ',' \h* <.[ . … ]>+>
          <.worry("Comma found before apparent sequence operator; please remove comma (or put parens around the ... call, or use 'fail' instead of ...)")>
        ]?
        [ <?{ $*GOAL eq 'endargs' && !$*COMPILING_CORE_SETTING }>
          <?after <.:L + [\]]> \h* <[ . … ]>+>
          <.worry("Apparent sequence operator parsed as stubbed function argument; please supply any missing argument to the function or the sequence (or parenthesize the ... call, or use 'fail' instead of ...)")>
        ]?
        <args>
    }

    token term:sym<???> { <.sym> <args> }
    token term:sym<!!!> { <.sym> <args> }

    token term:sym<undef> {
        <!{ $*LANG.pragma('p5isms') }>
        <.sym>
        »
        {}
        [ <?before \h*'$/' >
          <.obs: '$/ variable as input record separator', "the filehandle's .slurp method">
        ]?
        [ <?before [ '(' || \h*<.sigil><.twigil>?\w ] >
            <.obs: 'undef as a verb', 'undefine() or assignment of Nil'>
        ]?
        <.obs: 'undef as a value',
          "something more specific:\n\tan undefined type object such as Any or Int,\n\t:!defined as a matcher,\n\tAny:U as a type constraint,\n\tNil as the absence of an expected value\n\tor fail() as a failure return\n\t   ">
    }

    token term:sym<new> {
        <!{ $*LANG.pragma('c++isms') }>
        'new'
        \h+
        <.longname>
        \h*
        <![:]>
        <.obs: "C++ constructor syntax", "method call syntax", :ism<c++isms>>
    }

    token term:sym<fatarrow> {
        <key=.identifier> <.fatty> <.ws> <val=.EXPR('i<=')>
    }

    token term:sym<colonpair> { <colonpair> }

    token term:sym<variable> {
        <variable>
        { $*VAR := $<variable> unless $*VAR }
    }

    token term:sym<package-declarator> { <package-declarator> }
    token term:sym<scope-declarator>   { <scope-declarator> }
    token term:sym<routine-declarator> { <routine-declarator> }
    token term:sym<regex-declarator>   { <regex-declarator> }
    token term:sym<statement-prefix>   { <statement-prefix> }
    token term:sym<type-declarator>    { <type-declarator> }

    token term:sym<multi-declarator>   {
        <?before <.multi-multi> | <.multi-proto> | <.multi-only>>
        <multi-declarator>
    }

    token term:sym<*>  { <.sym> }
    token term:sym<**> { <.sym> }

    token term:sym<lambda> {
        <?pointy-block-starter>
        <pointy-block>
        {$*BORG<block> := $<pointy-block> }
    }

    token term:sym<value> { <value> }

    token term:sym<::?IDENT> {
        $<sym> = [ '::?' <identifier> ] »
    }

    token term:sym<p5end> {
        « __END__ »
        <.obs: '__END__ as end of code',
          'the =finish pod marker and $=finish to read'>
    }
    token term:sym<p5data> {
        « __DATA__ »
        <.obs: '__DATA__ as start of data',
          'the =finish pod marker and $=finish to read'>
    }

    token infix:sym<lambda> {
        <?before '{' | <.pointy-block-starter> > <!{ $*IN-META }> {
            my $needparens := 0;
            my $pos := $/.from;
            my $line := HLL::Compiler.lineof($/.orig, $/.from, :cache(1));
            if $*TERMISH {
                my $term := $*TERMISH.ast;
                if nqp::istype($term, self.actions.r('Call', 'Name')) {
                    $term.to-begin-time($*R, $*CU.context);
                    $term.PERFORM-CHECK($*R, $*CU.context);
                    if nqp::istype($term, self.actions.r('Lookup')) && !$term.is-resolved && $term.needs-resolution {
                        my $word := $term.name.canonicalize;
                        for 'if', 'unless', 'while', 'until', 'for', 'given', 'when', 'loop', 'sub', 'method', 'with', 'without', 'supply', 'whenever', 'react' {
                            if $_ eq $word {
                                $needparens++ if $_ eq 'loop';
                                self.typed-sorry-at($*TERMISH<term><identifier>.to, 'X::Syntax::KeywordAsFunction', :$word, :$needparens);
                                self.panic("Unexpected block in infix position (two terms in a row)");
                            }
                        }
                    }
                }
            }
        }
        [
        || <!{ $*IN_REDUCE }> {
            $/.panic("Unexpected block in infix position (missing statement control word before the expression?)");
        }
        || <!>
        ]
    }

    token term:sym<identifier> {
        <identifier>
        <!{ $*R.is-identifier-type(~$<identifier>) }>
        [ <?before <.unspace>? '('> | \\ <?before '('> ]
        <args(1)>
        {
            unless $<args><invocant> {
                if $*BORG<block> {
                    unless $*BORG<name> {
                        $*BORG<name> := ~$<identifier>;
                    }
                }
            }
        }
    }

    token term:sym<nqp::op> {
        'nqp::' $<op>=[\w+] <args>?
    }

    token term:sym<nqp::const> {
        'nqp::const::' $<const>=[\w+]
    }

    my %deftrap := nqp::hash(
        'say', 1, 'print', 1, 'abs', 1, 'chomp', 1, 'chop', 1, 'chr', 1, 'cos', 1,
        'defined', 1, 'exp', 1, 'lc', 1, 'log', 1, 'mkdir', 1, 'ord', 1, 'reverse', 1,
        'rmdir', 1, 'sin', 1, 'split', 1, 'sqrt', 1, 'uc', 1, 'unlink', 1, 'fc', 1,

        'WHAT', 2, 'WHICH', 2, 'WHERE', 2, 'HOW', 2, 'WHENCE', 2, 'WHO', 2, 'VAR', 2, 'any', 2,
        'all', 2, 'none', 2, 'one', 2, 'set', 2, 'bag', 2, 'tclc', 2, 'wordcase', 2, 'put', 2,
    );

    token term:sym<name> {
        :my $*IS-TYPE;
        :my $*META-OP;
        <longname>
        :my $base-name;
        [
          || <?{
                 $base-name := $<longname>.ast.without-colonpairs;
                 nqp::eqat($<longname>.Str, '::', 0)
                   || $*R.is-name-known($base-name)
             }>
             { $*IS-TYPE := $*R.is-name-type($base-name) }
             [
               <?[[]>
               <?{ $*IS-TYPE }>
               :dba('type parameter')
               '[' ~ ']' <arglist>
             ]?
             <.unspace>?
             [
               <?[{]>
               <?{ $*IS-TYPE }>
               <whence=.postcircumfix>
               <.NYI: 'Autovivifying object closures'>
             ]?
             <.unspace>?
             [
               <?[(]>
               <?{ $*IS-TYPE }>
               '('
               <.ws>
               [
                 || <accept=.maybe-typename>
                    <?{
                        my $it := $<accept>.ast;
                        nqp::istype($it,self.actions.r('Type','Coercion'))
                          || $*R.is-name-type($it.name)
                    }>
                 || $<accept_any>=<?>
               ]
               <.ws>
               ')'
             ]?

          || [ \\ <?before '('> ]?
             <args(1)>
             {
                my $desigilname := $<longname>.ast;
                if nqp::elems($desigilname.colonpairs) == 1
                    && nqp::istype($desigilname.colonpairs[0], self.actions.r('QuotedString'))
                {
                    my $meta-op-name := self.meta-op-name($desigilname);
                    if nqp::isconcrete($meta-op-name) {
                        $*META-OP := $meta-op-name;
                    }
                }
                my $name := ~$<longname>;
                unless $<args>.ast.invocant {
                    if $*BORG<block> {
                        unless $*BORG<name> {
                            $*BORG<name> := $*BORG<name> // $name;
                        }
                    }
                    my $nextch := nqp::substr($/.orig, $/.pos, 1) || ' ';
                    if %deftrap{$name} {
                        my $al := $<args><arglist>;
                        my int $ok := 0;
                        $ok := 1 unless $al<EXPR> eq '';
                        $ok := 1 if $<args><semiarglist>;
                        unless $ok {
                            my $trap := %deftrap{$name};
                            if nqp::index('<[{', $nextch) >= 0 {
                                $/.typed-panic('X::Syntax::Confused', reason => "Use of non-subscript brackets after \"$name\" where postfix is expected; please use whitespace before any arguments")
                            }
                            elsif nqp::index('$@%&+-/*', $nextch) >= 0 {
                                $/.typed-panic('X::Syntax::Confused', reason => "A list operator such as \"$name\" must have whitespace before its arguments (or use parens)")
                            }
                            else {
                                my $missing := $/.terminator() || $/.infixish();
                                $/.'!clear_highwater'();  # don't have suppose
                                my $orry := $missing ?? "sorry" !! "worry";
                                if $trap == 1 {        # probably misused P5ism
                                    $<longname>."{$orry}obs"("bare \"$name\"", ".$name if you meant to call it as a method on \$_, or use an explicit invocant or argument, or use &$name to refer to the function as a noun");
                                }
                                elsif $trap == 2 {        # probably misused P6ism
                                    $<longname>."$orry"("Function \"$name\" may not be called without arguments (please use () or whitespace to denote arguments, or &$name to refer to the function as a noun, or use .$name if you meant to call it as a method on \$_)");
                                }
                                $<longname>.sorry("Argument to \"$name\" seems to be malformed")
                                  if $orry eq 'worry'
                                  && !$*LANG.pragma('p5isms');
                            }
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
          | '('
            <args=.semiarglist>
            ')'

          | <?before <.sigil>>
            <.typed-worry: 'X::Worry::P5::Reference'>
            <args=.termish>

          | <?before \d>
            <.typed-worry: 'X::Worry::P5::BackReference'>
            <args=.termish>

          | <?before \S>
            <args=.termish>

          | {}
            <.panic: "You can't backslash that">
        ]
    }

    token term:sym<onlystar> {
        '{*}'
        <?end-statement>
        # [ <?{ $*IN_PROTO }> || <.panic: '{*} may only appear in proto'> ]
    }

    regex term:sym<reduce> {
        :my $*IN_REDUCE := 1;
        <?before '['\S+']'>

        # disallow accidental prefix before termish thing
        <!before '['+ <.[ - + ? ~ ^ ]> <.[ \w $ @ ]> >

        '['
        [
          || <op=.infixish('red')> <?[\]]>

          || $<triangle>=[\\]<op=.infixish('tri')> <?[\]]>

          || <!>
        ]
        ']'

        { $*IN_REDUCE := 0 }
        <args>
    }

#-------------------------------------------------------------------------------
# Colonpairs

    token colonpair {
        :my $*KEY;
        ':'
        :dba('colon pair')
        [
          | $<neg>='!'
            [ <identifier> || <.malformed: "False pair; expected identifier"> ]
            [
              <[ \[ \( \< \{ ]>
              {
                  $/.typed-panic: 'X::Syntax::NegatedPair',
                    key => ~$<identifier>
              }
            ]?
            { $*KEY := $<identifier> }

          | $<num>=[\d+]
            <identifier>
            [
              <?before <.[ \[ \( \< \{ ]>>
              {}
              <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)>
              <.circumfix>
            ]?
            <?{ self.no-synthetics($<num>) }>
            { $*KEY := $<identifier> }

          | <identifier>
            { $*KEY := $<identifier> }
            [
              <.unspace>?
              <?{ !$*IN-TYPENAME && !self.type-smiley(~$*KEY) }>
              :dba('pair value')
              <coloncircumfix($*KEY)>
            ]?

          | :dba('signature')
            '(' ~ ')' <fakesignature>

          | <coloncircumfix('')>
            { $*KEY := "" }

          | <var=.colonpair-variable>
            {
                $*KEY := $<var><desigilname>;
                self.check-variable($<var>);
            }
        ]
    }

    token coloncircumfix($front) {
        # Reset $*IN-DECL in case this colonpair is part of var we're
        # declaring, since colonpair might have other vars. Don't make those
        # think we're declaring them
        :my $*IN-DECL := '';
        [
          | '<>'
            <.worry: "Pair with <> really means an empty list, not null string; use :$front" ~ "('') to represent the null string,\n  or :$front" ~ "() to represent the empty list more accurately">

          | {}
            :my $*OBJECT-HASH := $front ?? 0 !! 1;
            <circumfix>
        ]
    }

    token colonpair-variable {
        <sigil>
        {}
        [
          | <twigil>? <desigilname>
          | $<capvar>='<' <desigilname> '>'
        ]
    }

#-------------------------------------------------------------------------------
# Special variables, mostly for catching Perlisms

    proto token special-variable {*}

    token special-variable:sym<$!{ }> {
        [ '$!{' .*? '}' | '%!' ]
        <.obsvar: '%!'>
    }

    token special-variable:sym<$`> {
        <.sym>  <?before \s | ',' | <.terminator> >
        <.obsvar: '$`'>
    }

    token special-variable:sym<$@> {
        <.sym> <[ \s ; , ) ]> .
        <.obsvar: '$@'>
    }

    token special-variable:sym<$#> {
        <.sym> <identifier>
        {}
        <.obsvar: '$#', ~$<identifier>>
    }

    token special-variable:sym<$$> {
        <.sym> \W
        <.obsvar: '$$'>
    }

    token special-variable:sym<$&> {
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '$&'>
    }

    token special-variable:sym<@+> {
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '@+'>
    }

    token special-variable:sym<%+> {
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '%+'>
    }

    token special-variable:sym<$+[ ]> {
        '$+['
        <.obsvar: '@+'>
    }

    token special-variable:sym<@+[ ]> {
        '@+['
        <.obsvar: '@+'>
    }

    token special-variable:sym<@+{ }> {
        '@+{'
        <.obsvar: '%+'>
    }

    token special-variable:sym<@-> {
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '@-'>
    }

    token special-variable:sym<%-> {
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '%-'>
    }

    token special-variable:sym<$-[ ]> {
        '$-['
        <.obsvar: '@-'>
    }

    token special-variable:sym<@-[ ]> {
        '@-['
        <.obsvar: '@-'>
    }

    token special-variable:sym<%-{ }> {
        '@-{'
        <.obsvar: '%-'>
    }

    token special-variable:sym<$/> {
        <.sym>
        <?before \h* '=' \h* <.[ ' " ]> >
        <.obsvar: '$/'>
    }

    token special-variable:sym<$\\> {
        '$\\'
        <?before \s | ',' | '=' | <.terminator> >
        <.obsvar: '$\\'>
    }

    token special-variable:sym<$|> {
        <.sym> <?before \h* '='>
        <.obsvar: '$|'>
    }

    token special-variable:sym<$;> {
        <.sym> <?before \h* '='>
        <.obsvar: '$;'>
    }

    token special-variable:sym<$'> { #'
        <.sym> <?before \s | ',' | <.terminator> >
        <.obsvar: '$' ~ "'">
    }

    token special-variable:sym<$"> {
        <.sym> <?before \h* '='>
        <.obsvar: '$"'>
    }

    token special-variable:sym<$,> {
        <.sym> <?before \h* '='>
        <.obsvar: '$,'>
    }

    token special-variable:sym<$.> {
        <.sym> {} <!before \w | '(' | ':' | '^' >
        <.obsvar: '$.'>
    }

    token special-variable:sym<$?> {
        <.sym> {} <!before \w | '('>
        <.obsvar: '$?'>
    }

    token special-variable:sym<$]> {
        <.sym> {} <!before \w | '('>
        <.obsvar: '$]'>
    }

    regex special-variable:sym<${ }> {
        <sigil>
        '{'
        {}
        $<text>=[.*?]
        '}'
        <!{ $*IN-DECL }>
        <!{ $<text> ~~ / [ '=>' | '⇒' ] || ':'<:alpha> || '|%' / }>
        <!{ $<text> ~~ / ^ \s* $ / }>
        <?{
            my $sigil := $<sigil>.Str;
            my $text  := $<text>.Str;
            my $bad   := $sigil ~ '{' ~ $text ~ '}';
            if $text ~~ /^\d+$/ {
                $text := nqp::radix(10, $text, 0, 0)[0];
                $text := $text - 1 if $text > 0;
            }
            if $sigil ne '$' && $sigil ne '@' {
                False;  # not likely a P5ism
            }
            elsif !($text ~~ /^(\w|\:)+$/) {
                $/.obs: $bad,
                  "$sigil\($text) for hard ref or $sigil\::($text) for symbolic ref";
            }
            elsif $*QSIGIL {
                $/.obs: $bad, '{' ~ $sigil ~ $text ~ '}';
            }
            else {
                $/.obs: $bad, $sigil ~ $text;
            }
        }>
    }

    token variable {
        :my $*IN-META := '';
        [
          # &[foo]
          | :dba('infix noun') '&[' ~ ']' <infixish('[]')>

          # $foo and $!foo
          | <sigil> <twigil>? <desigilname>

          # $/ $_ $! $¢
          | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]

          # $0
          | <sigil> $<index>=[\d+]

          # $<foo>
          | <sigil> <?[<]> <postcircumfix>

          # 👍
          | $<desigilname>=<.sigilless-variable>

          # obsolete Perl vars
          | <.special-variable>

          # $() @() %() &() $[] …
          | <?before <.sigil> <.?[ ( [ { ]>>
            <!RESTRICTED>
            <?{ !$*IN-DECL }>
            <contextualizer>

          # try last, to allow sublanguages to redefine sigils (like & in regex)
          | {}
            <sigil>
            <!{ $*QSIGIL }>
            <?MARKER('baresigil')>
        ]

        { $*LEFTSIGIL := self.leading-char unless $*LEFTSIGIL }
    }

    token contextualizer {
        :dba('contextualizer')
        [ <?{ $*IN-DECL }> <.panic: "Cannot declare a contextualizer"> ]?
        [
          | <sigil> '(' ~ ')'    <coercee=sequence>

          | <sigil> <?[ \[ \{ ]> <coercee=circumfix>
        ]
    }

#-------------------------------------------------------------------------------
# Declarations

    proto token package-declarator {*}
    token package-declarator:sym<package> {
        <.package-package> <.kok> <package-def('package')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<module> {
        <.package-module> <.kok> <package-def('module')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<class> {
        <.package-class> <.kok> <package-def('class')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<grammar> {
        <.package-grammar> <.kok> <package-def('grammar')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<role> {
        <.package-role> <.kok> <package-def('role')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<knowhow> {
        <.package-knowhow> <.kok> <package-def('knowhow')>
        <.set_braid_from(self)>
    }
    token package-declarator:sym<native> {
        <.package-native> <.kok> <package-def('native')>
        <.set_braid_from(self)>
    }

    rule package-def($*PKGDECL) {
        :my $*BORG := {};
        :my $*BLOCK;
        :my $*PACKAGE;
        <!!{ $/.clone_braid_from(self) }>
        <longname>? {}
        [ :dba('generic role')
          <?{ ($*PKGDECL // '') eq 'role' }>
          '[' ~ ']' <signature(:DECLARE-TARGETS(0))>
          { $*IN-DECL := ''; }
        ]?
        <.stub-package($<longname>)>
        { $/.set_package($*PACKAGE) }
        :my $*ALSO-TARGET := $*PACKAGE;
        <trait($*PACKAGE)>*
        <.enter-package-scope($<signature>)>
        [
          || <?[{]> { $*START-OF-COMPUNIT := 0; } <block($*PKGDECL eq 'role' ?? 'RoleBody' !! 'Block', :parameterization($<signature> ?? $<signature>.ast !! Mu))>
          || ';'
             [
               || <?{ $*START-OF-COMPUNIT }>
                  { $*START-OF-COMPUNIT := 0; }
                  <unit-block($*PKGDECL, $*PKGDECL eq 'role' ?? 'RoleBody' !! 'Block', :parameterization($<signature> ?? $<signature>.ast !! Mu))>

               || { $/.typed-panic: "X::UnitScope::TooLate", what => $*PKGDECL }
             ]

          || <.panic("Unable to parse $*PKGDECL definition")>
        ]
        <.leave-block-scope>
        <.leave-package-scope>
    }

    token stub-package($*PACKAGE-NAME) { <?> }
    token enter-package-scope($*SIGNATURE) { <?> }
    token leave-package-scope { <?> }

    proto token scope-declarator {*}
    token scope-declarator:sym<my>      { <.scope-my>      <scoped('my')>      }
    token scope-declarator:sym<our>     { <.scope-our>     <scoped('our')>     }
    token scope-declarator:sym<has>     { <.scope-has>     <scoped('has')>     }
    token scope-declarator:sym<HAS>     { <.scope-HAS>     <scoped('HAS')>     }
    token scope-declarator:sym<anon>    { <.scope-anon>    <scoped('anon')>    }
    token scope-declarator:sym<state>   { <.scope-state>   <scoped('state')>   }
    token scope-declarator:sym<unit>    { <.scope-unit>    <scoped('unit')>    }
    token scope-declarator:sym<augment> { <.scope-augment> <scoped('augment')> }
    token scope-declarator:sym<supersede> {
        <.scope-supersede> <scoped('supersede')> <.NYI: '"supersede"'>
    }

    token scoped($*SCOPE) {
        <.end-keyword>
        :dba('scoped declarator')
        [
          || <.ws>
             [
               | <DECL=declarator>

               | <DECL=regex-declarator>

               | <DECL=package-declarator>

               | [<typename><.ws>]+
                 {
                    nqp::elems($<typename>) > 1
                      ?? $/.NYI('Multiple prefix constraints')
                      !! ($*OFTYPE := $<typename>[0]);
                 }
                 <DECL=multi-declarator>

               | <DECL=multi-declarator>
             ]
          || <.ws>[<typename><.ws>]* <ident>
             <?before <.ws> [
               | ':'?':'?'='
               | <.terminator>
               | <trait>
               | <.constraint-where> <.ws> <EXPR>
               | $
             ]>
             {}
             <.malformed: "$*SCOPE (did you mean to declare a sigilless \\{~$<ident>} or \${~$<ident>}?)">

          || <.ws><!typename> <typo-typename> <!>

          || <.malformed: $*SCOPE>
        ]
    }

    proto token multi-declarator {*}
    token multi-declarator:sym<multi> {
        <.multi-multi>
        <.kok>
        :my $*MULTINESS := 'multi';
        [ <?before '('>
          {self.typed-panic: "X::Anon::Multi", multiness => $*MULTINESS}
        ]?
        [    <declarator>
          || <routine-def('sub')>
          || <.malformed: $*MULTINESS>
        ]
    }
    token multi-declarator:sym<proto> {
        <.multi-proto>
        <.kok>
        :my $*MULTINESS := 'proto';
        [ <?before '('>
          {self.typed-panic: "X::Anon::Multi", multiness => $*MULTINESS}
        ]?
        [    <declarator>
          || <routine-def('sub')>
          || <.malformed: $*MULTINESS>
        ]
    }
    token multi-declarator:sym<only> {
        <.multi-only>
        <.kok>
        :my $*MULTINESS := 'only';
        [ <?before '('>
          {self.typed-panic: "X::Anon::Multi", multiness => $*MULTINESS}
        ]?
        [    <declarator>
          || <routine-def('sub')>
          || <.malformed: $*MULTINESS>
        ]
    }
    token multi-declarator:sym<null> {
        :my $*MULTINESS := '';
        <declarator>
    }

    token declarator {
        :my $*LEFTSIGIL := '';
        [
          | '\\'
            <defterm>
            [    <.ws> <term-init=initializer>
              || <.typed-panic: "X::Syntax::Term::MissingInitializer">
            ]

          | <variable-declarator>

          | [
              :my $*DEFAULT-RW := 1;
              '(' ~ ')' <signature(:ON-VARDECLARATION)> [ <.ws> <trait>+ ]? [ <.ws> <initializer> ]?
          ]

          | <routine-declarator>

          | <regex-declarator>

          | <type-declarator>
        ]
    }

    token sigilless-variable { <!> }

    method reserved-sorry(str $sigil) {
        my %args;
        my str $reserved;
        if $sigil eq '&' {
            $reserved := 'routine';
            %args<instead> := ' (maybe use :() to declare a longname?)';
        }
        else  {
            $reserved := $sigil eq '@'
              ?? 'array'
              !! $sigil eq '%'
                ?? 'hash'
                !! 'variable';
        }
        %args<reserved> := "() shape syntax in $reserved declarations";

        self.typed-sorry('X::Syntax::Reserved', |%args);
    }

    token variable-declarator {
        :my $*IN-DECL := 'variable';
        :my $*VARIABLE;
        :my $*VARIABLE-NAME;
        :my $sigil;
        [
          | <sigil> <twigil>? <desigilname>?

          | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]

          | $<desigilname>=<.sigilless-variable>

          # TODO cases for when you declare something you're not allowed to
        ]
        {
            $*IN-DECL := '';
            $*LEFTSIGIL := self.leading-char unless $*LEFTSIGIL;
            $sigil := $<sigil> ?? $<sigil>.Str !! "";
            $*VARIABLE-NAME := $<sigil> ~ $<twigil> ~ $<desigilname>;
        }
        [
          <.unspace>?
          $<shape>=[
                     | '(' ~ ')' <signature>
                       <.reserved-sorry: $sigil>

                     | :dba('shape definition')
                       '[' ~ ']' <semilist>
                       {
                           self.typed-sorry('X::Syntax::Reserved',
                             reserved => "[] shape syntax with the $sigil sigil"
                           ) if $sigil ne '@';
                       }

                     | :dba('shape definition')
                       '{' ~ '}' <semilist>
                       {
                           self.typed-sorry('X::Syntax::Reserved',
                             reserved => "{} shape syntax with the $sigil sigil"
                           ) if $sigil ne '%';
                       }

                     | <?[<]>
                       <postcircumfix>
                       <.NYI: "Shaped variable declarations">
                   ]+
        ]?
        [ <.ws> <trait>+ ]?
        :my $where;
        [
            <.ws> <.constraint-where> <.ws> <EXPR('j')>
            { $where := $<EXPR>.ast }
        ]?
        <.stub-variable($/, $where)>
        [
            [
                <?{ $*VARIABLE.is-attribute }>
                { $*R.enter-scope($*VARIABLE.initializer-method); $*R.create-scope-implicits(); }
                [<.ws> <initializer>]?
                { $*R.leave-scope }
            ]
            | [<.ws> <initializer>]?
        ]
    }

    token stub-variable($*VARIABLE-MATCH, $*WHERE) { <?> }

    token desigilname {
        [
          | <?before <.sigil> <.sigil> > <variable>

          | <?sigil>
            [ <?{ $*IN-DECL }>
              <.typed-panic: 'X::Syntax::Variable::IndirectDeclaration'>
            ]?
            <variable>
            { $*VAR := $<variable> }

          | <longname>
        ]
    }

    proto token initializer {*}
    token initializer:sym<=> {
        <.sym>
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
        <.sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
    }
    token initializer:sym<::=> {
        <.sym> [ <.ws> <EXPR('e=')> <.NYI: '"::="'> || <.malformed: 'binding'> ]
    }
    token initializer:sym<.=> {
        <.sym> [ <.ws> <dottyop> || <.malformed: 'mutator method call'> ]
    }

    proto token routine-declarator {*}
    token routine-declarator:sym<sub> {
        <.routine-sub>
        <.end-keyword>
        <routine-def=.key-origin('routine-def', 'sub')>
    }
    token routine-declarator:sym<method> {
        <.routine-method>
        <.end-keyword>
        <method-def=.key-origin('method-def', 'method')>
    }
    token routine-declarator:sym<submethod> {
        <.routine-submethod>
        <.end-keyword>
        <method-def=.key-origin('method-def', 'submethod')>
    }

    rule routine-def($declarator) {
        :my $*BORG := {};
        :my $*IN-DECL := $declarator;
        :my $*BLOCK;
        <.enter-block-scope(nqp::tclc($declarator))>
        <deflongname('my')>?
        {
            if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                $*BLOCK.ensure-parse-performed($*R, $*CU.context);
            }
        }
        {
            my $deflongname := $<deflongname>;
            if $deflongname && $deflongname<colonpair>[0]<coloncircumfix> -> $cf {
                # It's an (potentially new) operator, circumfix, etc. that we
                # need to tweak into the grammar.
                my $name     := $deflongname<name>;
                my $category := $name.Str;

                my $opname := '';
                if $cf<circumfix> -> $ccf {
                    $opname := (my $nibble := $ccf<nibble>)
                      ?? $nibble.ast.literal-value // ~$nibble
                      !! $ccf<semilist>;
                }
                my $canname := $category
                  ~ ':sym'
                  ~ self.actions.r('ColonPairish').IMPL-QUOTE-VALUE(~$opname);

                $/.add-categorical(
                  $category, $opname, $canname, $name.ast.canonicalize, $*BLOCK
                );
            }
        }
        [ '(' <signature(:ON-ROUTINE(1))> ')' ]?
        :my $*ALSO-TARGET := $*BLOCK;
        <trait($*BLOCK)>* :!s
        { if $<signature> { $*BLOCK.replace-signature($<signature>.ast); } }
        { $*IN-DECL := ''; }
        [
          || ';'
             {
                 if $<deflongname> ne 'MAIN' {
                     $/.typed-panic: "X::UnitScope::Invalid",
                        what       => "sub",
                        where      => "except on a MAIN sub",
                        suggestion => "Please use the block form. If you did not mean to declare a unit-scoped sub,\nperhaps you accidentally placed a semicolon after routine's definition?";
                 }
                 unless $*START-OF-COMPUNIT {
                     $/.typed-panic: "X::UnitScope::TooLate", what => "sub";
                 }
                 unless $*MULTINESS eq '' || $*MULTINESS eq 'only' {
                     $/.typed-panic: "X::UnitScope::Invalid",
                       what  => "sub",
                       where => "on a $*MULTINESS sub";
                 }
                 unless $*R.outer-scope =:= $*UNIT {
                     $/.typed-panic: "X::UnitScope::Invalid",
                       what  => "sub",
                       where => "in a subscope";
                 }
                 $*START-OF-COMPUNIT := 0;
             }

          || <onlystar>

          || <blockoid>
        ]
        <.leave-block-scope>
    }

    rule method-def($declarator) {
        :my $*BORG := {};
        :my $*IN-DECL := $declarator;
        :my $*BLOCK;
        <.enter-block-scope(nqp::tclc($declarator))>
        $<specials>=[<[ ! ^ ]>?]<deflongname('has')>?
        {
            if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                $*BLOCK.to-parse-time($*R, $*CU.context);
            }
        }
        [ '(' <signature(1, :ON-ROUTINE(1))> ')' ]?
        <trait($*BLOCK)>* :!s
        { if $<signature> { $*BLOCK.replace-signature($<signature>.ast); } }
        { $*R.create-scope-implicits(); }
        { $*IN-DECL := ''; }
        [
          || <onlystar>
          || <blockoid>
        ]
        <.leave-block-scope>
    }

    token onlystar {
        <?{ $*MULTINESS eq 'proto' }>
        '{' <.ws> '*' <.ws> '}'
        <?end-statement>
    }

    proto token regex-declarator {*}

    token regex-declarator:sym<rule> {
        <.routine-rule>
        <.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN-DECL := 'rule';
        :my $*WHITESPACE-OK := 1;
        <regex-def>
    }

    token regex-declarator:sym<token> {
        <.routine-token>
        <.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN-DECL := 'token';
        <regex-def>
    }

    token regex-declarator:sym<regex> {
        <.routine-regex>
        <.kok>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        :my $*IN-DECL := 'regex';
        <regex-def>
    }

    rule regex-def {
        :my $*BLOCK;
        :my $type := $*IN-DECL;
        <.enter-block-scope(nqp::tclc($*IN-DECL) ~ 'Declaration')>
        [
          <deflongname('has')>?
          {
              if nqp::istype($*BLOCK, self.actions.r('ParseTime')) {
                  $*BLOCK.ensure-parse-performed($*R, $*CU.context);
              }
          }
          { if $<deflongname> { %*RX<name> := $*BLOCK.name } }
          { $*IN-DECL := '' }
          [ '(' <signature> ')' ]?
          <trait($*BLOCK)>*
          '{'
          [
            | ['*'|'<...>'|'<*>'] <?{ $*MULTINESS eq 'proto' }> $<onlystar>={1}
            | <nibble(self.quote-lang(self.Regex(%*RX<P5>), '{', '}'))>
          ]
          '}'<!RESTRICTED><?end-statement>
          <.leave-block-scope>
        ] || <.malformed: $type>
    }

    proto token type-declarator {*}

    method add-variable($name) {
        my $categorical := $name ~~ /^'&'((\w+) [ ':<'\s*(\S+?)\s*'>' | ':«'\s*(\S+?)\s*'»' ])$/;
        my $cat := ~$categorical[0][0];
        if $categorical && nqp::can(self, $cat) {
            my $canop := self.actions.r('ColonPairish').IMPL-QUOTE-VALUE($categorical[0][1]);
            my $canname := $cat ~ ':sym' ~ $canop;
            self.add-categorical($cat, ~$categorical[0][1], $canname, ~$categorical[0]);
        }
    }

    token type-declarator:sym<constant> {
        :my $*IN-DECL := 'constant';
        <.sym>
        <.kok>
        [
          | '\\'?
            <defterm>
          | <variable> { $¢.add-variable(~$<variable>) } # for new &infix:<foo> synonyms
          | <?>
        ]
        { $*IN-DECL := ''; }
        <.ws>

        <trait>*

        [ <.ws>
          <term_init=initializer>
            || <.typed-panic: "X::Syntax::Missing">
        ]

        <.cheat-heredoc>?
    }

    token type-declarator:sym<enum> {
        <.typer-enum>
        <.kok>
        :my $*IN-DECL := 'enum';
        [
          | <longname>
          | <variable>
          | <?>
        ]
        { $*IN-DECL := '' }
        <.ws>

        <trait>*

        [ <?[<(«]>
          <term>
          <.ws>
            || <.panic: 'An enum must supply an expression using <>, «», or ()'>
        ]
    }

    rule type-declarator:sym<subset> {
        <.typer-subset><.kok>
        :my $*IN-DECL := 'subset';
        [
          [
            [ <longname> ]
            { $*IN-DECL := '' }
            <trait>*
            [ <.constraint-where> <EXPR('e=')> ]?
          ] || <.malformed: 'subset'>
        ]
    }

    rule trait($*TARGET?) {
        :my $*IN-DECL := '';
        <trait_mod>
    }

    proto rule trait_mod {*}
    rule trait_mod:sym<is> {
        <.traitmod-is> [ <typename(:allow-capture(0))> || <longname><circumfix>? || <.panic: 'Invalid name'> ]
        {
            if $<circumfix> && nqp::eqat(self.orig, '{', $<longname>.to) {
                $*BORG<block> := $<circumfix>;
                $*BORG<name> := 'is ' ~ $<longname>;
            }
        }
    }
    rule trait_mod:sym<hides> {
        <.traitmod-hides> [ <typename> || <.bad-traitmod-typename>]
    }
    rule trait_mod:sym<does> {
        <.traitmod-does> [ <typename> || <.bad-traitmod-typename>]
    }
    rule trait_mod:sym<will> {
        <.traitmod-will> [ <identifier> || <.panic: 'Invalid name'>] <pointy-block>
    }
    rule trait_mod:sym<of> {
        <.traitmod-of> [ <typename> || <.bad-traitmod-typename>]
    }
    rule trait_mod:sym<returns> {
        <.traitmod-returns>
        [ <typename> || <.bad-traitmod-typename>]
          || 'return'
             <.panic: 'Invalid trait modifier (did you mean \'returns\'?)'>
    }
    rule trait_mod:sym<handles> {
        <.traitmod-handles> [ <term> || <.panic: 'Invalid term'>]
    }

    token bad-traitmod-typename {
        || <longname>
           {
                my $name := $<longname>.ast;
                $/.typed-panic('X::InvalidType',
                  :typename($name.canonicalize(:colonpairs(0))),
                  :suggestions([])); #TODO suggestions
           }
        || <.malformed: 'trait'>
    }

#-------------------------------------------------------------------------------
# Values

    proto token value {*}
    token value:sym<quote>  { <quote> }
    token value:sym<number> { <number> }
    token value:sym<version> { <version> }

    proto token number {*}
    token number:sym<numish>   { <numish> }

    token numish {
        [
          | 'NaN' »
          | <integer>
          | <decimal-number>
          | <radix-number>
          | <rational-number>
          | <complex-number>
          | 'Inf' »
          | $<uinf>='∞'
          | <unum=:No+:Nl>
        ]
    }

    token binint { [\d+]+ % '_' }  # action method panics if invalid
    token octint { [\d+]+ % '_' }  # action method panics if invalid
    token decint { [\d+]+ % '_' }
    token hexint {
        [
          [ \d | <[ a..f A..F ａ..ｆ Ａ..Ｆ ]> ]+
        ]+ % '_'
    }

    token integer {
        [
          | 0
            [   b '_'? <VALUE=.binint>
              | o '_'? <VALUE=.octint>
              | x '_'? <VALUE=.hexint>
              | d '_'? <VALUE=.decint>
              | <VALUE=.decint>
                { $/.typed-worry: 'X::Worry::P5::LeadingZero', :value(~$<VALUE>) }
            ]
          | <VALUE=.decint>
        ]
        <!!before [
          '.'
          <?before
              \s
            | ','
            | '='
            | ':' <!before <coloncircumfix <OPER=prefix> > >
            | <.terminator>
            | $
          >
          <.typed-sorry: 'X::Syntax::Number::IllegalDecimal'>
        ]?>
        [
          <?before '_' '_'+\d>
          <.sorry: "Only isolated underscores are allowed inside numbers">
        ]?
    }

    token signed-integer { <sign> <integer> }

    token signed-number { <sign> <number> }

    token decimal-number {
        :dba('decimal number')
        [
          | $<coeff> = [               '.' <frac=.decint> ] <escale>?
          | $<coeff> = [ <int=.decint> '.' <frac=.decint> ] <escale>?
          | $<coeff> = [ <int=.decint>                    ] <escale>
        ]
    }

    token escale { <[Ee]> <sign> <decint> }

    token sign { '+' | '-' | '−' | '' }

    token radix-number {
        ':' $<radix> = [\d+] <.unspace>?
        :my $r := nqp::radix(10, $<radix>, 0, 0)[0];
        {}           # don't recurse in lexer
        :dba('number in radix notation')
        :my $rad_digit  := token rad_digit  { \d | <[ a..z A..Z ａ..ｚ Ａ..Ｚ ]> };
        :my $rad_digits := token rad_digits { <rad_digit>+ [ _ <rad_digit>+ ]* };
        [
          || '<'
             $<ohradix>=[
                 '0x' <?{ $r < 34 }>
               | '0o' <?{ $r < 25 }>
               | '0d' <?{ $r < 14 }>
               | '0b' <?{ $r < 12 }>
             ]**0..1
             $<intpart>  = <rad_digits>
             $<fracpart> = [ '.' <rad_digits> ]**0..1
             [ '*' <base=.integer> '**' <exp=.integer> ]**0..1
             '>'

          || <?[[]> <bracket=circumfix>

          || <?[(]> <circumfix>

          || <.malformed: 'radix number'>
        ]
    }

    token rational-number {
        [
            '<' <bare-rational-number> '>'
          | <super-sign>? <super-integer> '/' <sub-integer>
        ]
    }
    token bare-rational-number {
        <?before <.[-−+0..9<>:boxd]>+? '/'>
        <nu=.signed-integer> '/' <de=integer>
    }

    token complex-number { '<' <bare-complex-number> '>' }
    token bare-complex-number {
        <?before <.[-−+0..9<>:.eEboxdInfNa\\]>+? 'i'>
        <re=.signed-number> <?[-−+]> <im=.signed-number> \\? 'i'
    }

    token version {
        <?before v\d+\w*
            [ '.' \d
              || <!{ $*R.resolve-lexical(~$/) }> ]>
        'v' $<vstr>=[<vnum>+ % '.' ['+' | '-']? ]
        <!before '-'|\'> # cheat because of LTM fail
    }

    token vnum {
        \w+ | '*'
    }

#-------------------------------------------------------------------------------
# Quoting

    proto token quote {*}
    token quote:sym<apos> {
        :dba('single quotes')
        "'" ~ "'" <nibble(self.quote-q)>
    }

    token quote:sym<sapos> {
        :dba('curly single quotes')
        "‘" ~ "’" <nibble(self.quote-q("‘", "’"))>
    }

    token quote:sym<lapos> {
        :dba('low curly single quotes')
        "‚" ~ <[’‘]> <nibble(self.quote-q("‚", ["’","‘"]))>
    }

    token quote:sym<hapos> {
        :dba('high curly single quotes')
        "’" ~ <[’‘]> <nibble(self.quote-q("’", ["’","‘"]))>
    }

    token quote:sym<dblq> {
        :dba('double quotes')
        '"' ~ '"' <nibble(self.quote-qq)>
    }

    token quote:sym<sdblq> {
        :dba('curly double quotes')
        '“' ~ '”' <nibble(self.quote-qq('“', '”'))>
    }

    token quote:sym<ldblq> {
        :dba('low curly double quotes')
        '„' ~ <[”“]> <nibble(self.quote-qq('„', ['”','“']))>
    }

    token quote:sym<hdblq> {
        :dba('high curly double quotes')
        '”' ~ <[”“]>
        <nibble(self.quote-qq('”', ['”','“']))>
    }

    token quote:sym<crnr> {
        :dba('corner quotes')
        '｢' ~ '｣' <nibble(self.quote-Q)>
    }

    token quote:sym<q> {
        <.quote-lang-q>
        :my $qm;
        [
          | <quote-modifier>
            {}  # make sure $/ is set
            <.qok($/)>
            { $qm := self.adverb-q2str(~$<quote-modifier>) }
            <quibble(self.Quote, 'q', [$qm, 1])>

          | {}  # make sure $/ is set
            <.qok($/)>
            <quibble(self.Quote, 'q')>
        ]
    }

    token quote:sym<qq> {
        <.quote-lang-qq>
        :my $qm;
        [
          | <quote-modifier>
            { $qm := self.adverb-q2str(~$<quote-modifier>) }
            <.qok($/)>
            <quibble(self.Quote, 'qq', [$qm, 1])>

          | {}
            <.qok($/)>
            <quibble(self.Quote, 'qq')>
        ]
    }

    token quote:sym<Q> {
        <.quote-lang-Q>
        :my $qm;
        [
          | <quote-modifier>
            { $qm := self.adverb-q2str(~$<quote-modifier>) }
            <.qok($/)>
            <quibble(self.Quote, '', [$qm, 1])>

          | {}
            <.qok($/)>
            <quibble(self.Quote)>
        ]
    }

    proto token quote-modifier   {*}
    token quote-modifier:sym<w>  { <sym> }
    token quote-modifier:sym<ww> { <sym> }
    token quote-modifier:sym<x>  { <sym> }
    token quote-modifier:sym<to> { <sym> }
    token quote-modifier:sym<s>  { <sym> }
    token quote-modifier:sym<a>  { <sym> }
    token quote-modifier:sym<h>  { <sym> }
    token quote-modifier:sym<f>  { <sym> }
    token quote-modifier:sym<c>  { <sym> }
    token quote-modifier:sym<b>  { <sym> }
    token quote-modifier:sym<o>  { <sym> }

    token qok($x) {
        »
        <![(]>
        [
             <?[:]>
          || <!{ $*R.is-identifier-known(~$x) }>
        ]
        [ \s* '#' <.panic: "# not allowed as delimiter"> ]?
        <.ws>
    }

    token quote:sym</null/> {
        '/' \s* '/'
        <.typed-panic: "X::Syntax::Regex::NullRegex">
    }

    token quote:sym</ /> {
        :my %*RX;
        :my $*INTERPOLATE := 1;
        '/'
        <nibble(self.quote-lang(self.Regex, '/', '/'))>
        [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
        <.old-rx-modifiers>?
    }
    token quote:sym<rx>   {
        <.quote-lang-rx>
        :my %*RX;
        :my $*INTERPOLATE := 1;
        {}  # make sure $/ gets set
        <.qok($/)>
        <rx-adverbs>
        <quibble(self.Regex(%*RX<P5>))>
        <!old-rx-modifiers>
    }

    token quote:sym<m> {
        <.quote-lang-m>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 0;
        {}  # make sure $/ gets set
        <.qok($/)>
        <rx-adverbs>
        <quibble(self.Regex(%*RX<P5>))>
        <!old-rx-modifiers>
    }

    token quote:sym<ms> {
        <.quote-lang-ms>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 1;
        { %*RX<s> := 1 }
        <.qok($/)>
        <rx-adverbs>
        <quibble(self.Regex(%*RX<P5>))>
        <!old-rx-modifiers>
    }

    token quote:sym<s> {
        <.quote-lang-s>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 0;
        {}  # make sure $/ gets set
        <.qok($/)>
        <rx-adverbs>
        <sibble(self.Regex(%*RX<P5>), self.Quote, 'qq')>
        [ <?{ $<sibble><infixish> }> || <.old-rx-modifiers>? ]
    }

    token quote:sym<ss> {
        <.quote-lang-ss>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 1;
        { %*RX<s> := 1 }
        <.qok($/)>
        <rx-adverbs>
        <sibble(self.Regex(%*RX<P5>), self.Quote, 'qq')>
        [ <?{ $<sibble><infixish> }> || <.old-rx-modifiers>? ]
    }

    token quote:sym<S> {
        <.quote-lang-S>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 0;
        {}  # make sure $/ gets set
        <.qok($/)>
        <rx-adverbs>
        <sibble(self.Regex(%*RX<P5>), self.Quote, 'qq')>
        [ <?{ $<sibble><infixish> }> || <.old-rx-modifiers>? ]
    }

    token quote:sym<Ss> {
        <.quote-lang-Ss>
        :my %*RX;
        :my $*INTERPOLATE   := 1;
        :my $*WHITESPACE-OK := 1;
        { %*RX<s> := 1 }
        <.qok($/)>
        <rx-adverbs>
        <sibble(self.Regex(%*RX<P5>), self.Quote, 'qq')>
        [ <?{ $<sibble><infixish> }> || <.old-rx-modifiers>? ]
    }

    token sibble($l, $lang2, $base?) {
        <babble($l)>

        :my $lang;
        :my $start;
        :my $stop;
        {
            my $B  := $<babble><B>.ast;
            $lang  := $B[0];
            $start := $B[1];
            $stop  := $B[2];
        }

        $start
        <left=.nibble($lang)>

        [    $stop
          || { self.fail-terminator: $/, $start, $stop }
        ]

        [ <?{ $start ne $stop }>
          <.ws>
          [ <?[ \[ \{ \( \< ]>
            <.obs: 'brackets around replacement', 'assignment syntax'>
          ]?
          [ <infixish> || <.missing: "assignment operator"> ]
          [ <?{
                $<infixish>.Str eq '='
                  || $<infixish><infix-postfix-meta-operator>
            }> || <.malformed: "assignment operator">
          ]
          <.ws>
          [ <right=.EXPR('i')>
              || <.panic: "Assignment operator missing its expression">
          ]
            || {
                $lang := self.quote-lang($lang2, $stop, $stop, $base)
            }
            <right=.nibble($lang)>
            $stop || <.malformed: "Replacement part; couldn't find final $stop">
        ]
    }

    token old-rx-modifiers {
        (<[ i g s m x c e ]>)
        {
            my $m := $/[0].Str;
            $/.obs: "/$m", $m eq 'i'
              ?? ':i'
              !! $m eq 'g'
                ?? ':g'
                !! $m eq 'm'
                  ?? '^^ and $$ anchors'
                  !! $m eq 's'
                    ?? '. or \N'
                    !! $m eq 'x'
                      ?? 'normal default whitespace'
                      !! $m eq 'c'
                        ?? ':c or :p'
                        !! 'interpolated {...} or s{} = ... form'  # $m eq 'e'
            ;
        }
    }

    token quote:sym<qr> {
        <.sym>
        {}
        <.qok($/)>
        <.obs: 'qr for regex quoting', 'rx//'>
    }

    token rx-adverbs() {
        [ <quotepair> <.ws> ]*
        {
            for $<quotepair> {
                my $ast := $_.ast;
                $ast.set-key(self.adverb-rx2str($ast.key));
            }
        }
    }

    token quotepair {
        :my $*KEY;
        ':'
        :dba('colon pair (restricted)')
        [
          | $<neg>='!'
            [ <identifier> || <.malformed: "False pair; expected identifier"> ]
            [
              <[ \[ \( \< \{ ]>
              { $/.typed-panic('X::Syntax::NegatedPair',key => ~$<identifier>) }
            ]?
            { $*KEY := $<identifier> }

          | $<num>=[\d+]
            <identifier>
            [ <?before <.[ \[ \( \< \{ ]>>
              {}
              <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)>
              <.circumfix>
            ]?
            <?{ self.no-synthetics($<num>) }>
            { $*KEY := $<identifier> }

          | <identifier>
            { $*KEY := $<identifier> }
            [ <?[(]> <circumfix> ]?
        ]
    }

#-------------------------------------------------------------------------------
# Types

    token typename(:$allow-capture = 1) {
        [
          # parse ::?CLASS as special case
          | '::?'<identifier> <colonpair>*

          | :my $*IN-TYPENAME := 1;
            <longname>
            <?{
                 # ::T introduces a type, so always is one
                 nqp::eqat(~$<longname>, '::', 0)
                   ?? $allow-capture
                   !! $*R.is-name-known($<longname>.ast.without-colonpairs)
            }>
        ]
        # parametric/coercion type?
        <.unspace>?
        [ <?[[]> '[' ~ ']' <arglist> ]?
        <.unspace>?
        [ <?before '{'>
          <.NYI: 'Autovivifying object closures'>
          <whence=.postcircumfix>
        ]?
        <.unspace>?
        [ <?[(]>
          '(' ~ ')' [<.ws> [<accept=.typename> || $<accept_any>=<?>] <.ws>]
        ]?
        [<.ws> <.traitmod-of> <.ws> <typename> ]?
    }

    token typo-typename($panic = 0) {
        <longname>
        {
            #TODO bring back suggestions for which types may have been meant
            my $method := $panic ?? 'typed-panic' !! 'typed-sorry';
            $/."$method"('X::Undeclared',
              what   => "Type",
              symbol => $<longname>.ast.canonicalize
            );
        }
    }

    method maybe-typename() {
        CATCH { return self.new-cursor }
        self.typename;
    }

#-------------------------------------------------------------------------------
# Signatures

    token fakesignature {
        :my $*BLOCK;
        <.enter-block-scope('PointyBlock')>
        <signature(1, :DECLARE-TARGETS(0))>
        <.leave-block-scope>
    }

    token signature($*ALLOW_INVOCANT = 0, :$*DECLARE-TARGETS = 1, :$*ON-VARDECLARATION, :$*ON-ROUTINE) {
        :my $*MULTI-INVOCANT := 1;
        :my @*SEPS := nqp::list();
        <.ws>
        [
          | <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' >
          | <parameter>
        ]+ % <param-sep>
        <.ws>
        [ <?before '-->' | ')' | ']' | '{' | ':'\s | ';;'>
            || <.malformed: 'parameter'>
        ]
        { $*IN-DECL := ''; }
        [ '-->'
          <.ws>
          [
            || [ <typename> | <value> || <typo-typename(1)> ]
               <.ws>
               [
                 || <?[ { ) ]>

                 || <?before <.param-sep>? <.parameter>>
                    <.malformed: 'return value (return constraints only allowed at the end of the signature)'>
               ] || <.malformed: 'return value'>
          ]
        ]?
        { $*LEFTSIGIL := '@'; }
    }

    rule param-sep {
        ''
        $<sep>=[','|':'|';;'|';']
        {
            if $<sep> eq ';;' {
                $/.panic("Can only specify ';;' once in a signature")
                  if $*MULTI-INVOCANT == 0;
                $*MULTI-INVOCANT := 0;
            }
            @*SEPS.push($<sep>);
        }
    }

    method obs-pipe($/) {
        $/.panic: 'Obsolete use of | or \\ with sigil on param ' ~ $<param-var>;
    }
    method panic-after-default($/, str $type) {
        $/.typed-panic: "X::Parameter::AfterDefault",
          type     => $type,
          modifier => ~$<modifier>,
          default  => ~$<default-value>
    }

    token parameter {
        [
          | <type-constraint>+
          [
            | $<quant>=[ '**' | '*' | '+' ]
              <param-var>

            | $<quant>=[ '\\' | '|' ]
              <param-var>
              { self.obs-pipe($/) }

            | $<quant>=[ '\\' | '|' |'+' ]
              <param-term>

            | [ <param-var> | <named-param> ]
              $<quant>=[ '?' | '!' | <?> ]

            | <?>
          ]

          | $<quant>=[ '**' | '*' | '+' ]
            <param-var>

          | $<quant>=[ '\\' | '|' ]
            <param-var>
            { self.obs-pipe($/) }

          | $<quant>=[ '\\' | '|' | '+' ]
            <param-term>

          | [ <param-var> | <named-param> ]
            $<quant>=[ '?' | '!' | <?> ]

          | <longname>
            # TODO: Re-add suggestions
            {
                self.typed-panic: 'X::Parameter::InvalidType',
                  :typename($<longname>)
            }
        ]
        <.ws>
        <trait>*
        <post-constraint>*
        [
          <default-value>
          [ <modifier=.trait>
            { self.panic-after-default($/, "trait") }
          ]?
          [ <modifier=.post-constraint>
            { self.panic-after-default($/, "post constraint") }
          ]?
        ]?
    }

    rule post-constraint {
        :my $*IN-DECL := '';
        :dba('constraint')
        [
          | '[' ~ ']' <signature(:DECLARE-TARGETS($*DECLARE-TARGETS))>

          | '(' ~ ')' <signature(:DECLARE-TARGETS($*DECLARE-TARGETS))>

          | <.constraint-where> <EXPR('i=')>
        ]
    }

    token param-var {
        :dba('formal parameter')
        [
          | '[' ~ ']' <signature(:DECLARE-TARGETS($*DECLARE-TARGETS))>

          | '(' ~ ')' <signature(:DECLARE-TARGETS($*DECLARE-TARGETS))>

          | $<declname>=[
              <sigil>
              <twigil>?
              [
#               || <?{ $<sigil>.Str eq '&' }>
#                  [<?identifier> {} <name=.sublongname> | <sigterm>]

                || <name=.identifier>

                || <name=.decint>
                   {self.typed-panic: 'X::Syntax::Variable::Numeric', what => 'parameter'}

                || $<name>=[<[/!]>]
              ]?
            ]

            :dba('shape declaration')
            :my $*IN-DECL := '';
            [
               # XXX allow fakesig parsed as subsig for the moment
#              | <?before ':('>  ':'

               | <?before '('>
                 <.sorry: "Shape declaration with () is reserved;\n  please use whitespace if you meant a subsignature for unpacking,\n  or use the :() form if you meant to add signature info to the function's type">

#               | <?before '['> <arrayshape=.postcircumfix>

               | <?before <.[ { < « ]>>
                 <.sorry: 'Shape declaration is not yet implemented; please use whitespace if you meant something else'>
                 <postcircumfix>
            ]?
        ]
    }

    token param-term {
        <defterm>?
    }

    token named-param {
        :my $*GOAL := ')';
        :dba('named parameter')
        ':'
        [
          | <name=.identifier>
            '('
            <.ws>
            [ <named-param> | <param-var> ]
            <.ws>
            [ ')'
              || <.panic: 'Unable to parse named parameter; couldn\'t find right parenthesis'>
            ]

          | <param-var>
        ]
    }

    rule default-value {
        :my $*IN-DECL := '';
        '='
        <EXPR('i=')>
    }

    token type-constraint {
        :my $*IN-DECL := '';
        [
          | <value>

          | [ <[-−]> :my $*NEGATE_VALUE := 1; | '+' ]
            $<value>=<numish>

          | <typename>

#          | <.constraint-where> <.ws> <EXPR('i=')>
        ]
        <.ws>
    }

#-------------------------------------------------------------------------------
# Argument lists and captures

    token args($*INVOCANT_OK = 0) {
        :my $*INVOCANT;
        :my $*GOAL := '';
        :my $*ADVERB-AS-INFIX := 0;
        :dba('argument list')
        [
          | '(' ~ ')' <semiarglist>             # keep these two lines
          | <.unspace> '(' ~ ')' <semiarglist>  # separate for performance
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
            {
                my $actions := self.actions;

                sub handle-any-named($ast) {
                    $ast.set-key(self.named2str($ast.key))
                      if nqp::istype($ast,$actions.r('ColonPair'))
                      || nqp::istype($ast,$actions.r('FatArrow'));
                }

                my $ast := $<EXPR>.ast;
                if nqp::istype($ast,$actions.r('ApplyListInfix')) {
                    for $ast.operands.FLATTENABLE_LIST {
                        handle-any-named($_);
                    }
                }
                else {
                    handle-any-named($ast);
                }
            }

          | <?>
        ]
    }

#-------------------------------------------------------------------------------
# Identifiers

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
          || <?before '(' | <.alpha>>
            [
              | <identifier>
              | :dba('indirect name')
                '(' ~ ')' [ <.ws> <EXPR> ]
            ]

          || <?before '::'>
             <.typed-panic: "X::Syntax::Name::Null">

          || $<bad>=[<.sigil><.identifier>]
             {
                 my str $b := $<bad>;
                 self.malformed("lookup of ::$b; please use ::('$b'), ::\{'$b'\}, or ::<$b>")
            }
        ]?
    }

    token longname {
        <name>
        {}
        [
          <?before ':' <.+alpha+[\< \[ \« ]>>
          <!RESTRICTED>
          <colonpair>
        ]*
    }

    token deflongname($*DEFAULT-SCOPE) {
        :dba('new name to be defined')
        <name>
        <colonpair>*
    }

    token defterm {
        :dba('new term to be defined')

        <identifier>
        [
        | <colonpair>+
            {
                if $<colonpair>[0]<coloncircumfix> -> $cf {
                    my $category := $<identifier>.Str;
                    my $opname := '';
                    if $cf<circumfix> -> $ccf {
                        $opname := (my $nibble := $ccf<nibble>)
                          ?? $nibble.ast.literal-value // ~$nibble
                          !! $ccf<semilist>;
                    }
                    my $canop := self.actions.r('ColonPairish').IMPL-QUOTE-VALUE(~$opname);
                    my $canname := $category ~ ':sym' ~ $canop;
                    my $termname := $category ~ ':' ~ $canop;
                    $/.add-categorical($category, $opname, $canname, $termname, :defterm);
                }
            }
        | <?>
        ]
    }

    token sigil { <[$@%&]> }

    proto token twigil {*}
    token twigil:sym<.> { <sym> <?before <alpha>> }
    token twigil:sym<!> { <sym> <?before <alpha>> }
    token twigil:sym<^> { <sym> <?before <alpha>> }
    token twigil:sym<:> { <sym> <?before <alpha>> }
    token twigil:sym<*> { <sym> <?before <alpha>> }
    token twigil:sym<?> { <sym> <?before <alpha>> }
    token twigil:sym<=> { <sym> <?before <alpha>> }
    token twigil:sym<~> { <sym> <?before <alpha>> }

    token end-keyword {
        »
        <!before <.[ \( \\ ' \- ]> || <.fatty>>
    }

    token end-prefix {
        <.end-keyword> \s*
    }

    token spacey { <?[\s#]> }

    token kok {
        <.end-keyword>
        [
          || <?before <.[ \s \# ]> > <.ws>
          || <!>
#
#          || <?{
#                 $*R.is-identifier-known(~self)
#                   ?? False
#                   !! self.panic:
#                        "Whitespace required after keyword '" ~ self ~ "'";
#             }>
        ]
    }

    token end-statement {
        [    # keep these alternations separate for performance
          | \h*                       $$ <.ws> <?MARKER('end-statement')>
          | <.horizontal-whitespace>? $$ <.ws> <?MARKER('end-statement')>
        ]?
    }

    proto token terminator {*}
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
          || <?MARKED('end-statement')>
             <?>

          || [
                | <?terminator>
                | $
             ]
        ]
    }

#-------------------------------------------------------------------------------
# Categoricals

    # Called when we add a new choice to an existing syntactic category, for
    # example new infix operators add to the infix category. Augments the
    # grammar as needed.
    method add-categorical(
      $category, $opname, $canname, $subname, $declarand?, :$defterm
    ) {
        my $actions            := self.actions;
        my $OperatorProperties := $actions.OperatorProperties;

        # Ensure it's not a null name or a compiler-handled op.
        if $opname eq '' {
            self.typed-panic: 'X::Syntax::Extension::Null';
        }

        # Make sure it's not reserved
        elsif $OperatorProperties.is-reserved-operator($canname)
          && !$*COMPILING_CORE_SETTING {
            self.typed-panic: 'X::Syntax::Extension::SpecialForm',
              :$category, :$opname,
              :hint($OperatorProperties.reserved-operator-hint($canname));
        }

        # Nothing to do if already have the required operator in the grammar
        elsif nqp::can(self, $canname) {
            return 1;
        }

        # Trait_mods don't require grammar changes
        elsif $category eq 'trait_mod' {
            return 0;
        }

        # Should be treated as a normal sub
        elsif $category eq 'METAOP_TEST_ASSIGN' || $category eq 'METAOP_TEST_ASSIGN_VALUE' {
            return 0;
        }

        # Work out what default precedence we want, or if it's more special
        # than just an operator.
        my %prec;
        my int $is-operator;
        my @parts := nqp::split(' ', $opname);

        # Sanity checks
        if $category eq 'infix'
          || $category eq 'prefix'
          || $category eq 'postfix' {
            self.typed-panic('X::Syntax::AddCategorical::TooManyParts',
              :$category, :needs(1)
            ) unless @parts == 1;

            %prec := $OperatorProperties."$category"().prec;  # XXX for now
            $is-operator := 1;
        }

        elsif $category eq 'postcircumfix' || $category eq 'circumfix' {
            unless @parts == 2 {
                my str $number := @parts > 2 ?? 'Many' !! 'Few';
                self.typed-panic:
                  'X::Syntax::AddCategorical::Too' ~ $number ~ 'Parts',
                  :$category, :needs(2);
            }
        }

        elsif $category eq 'term' {
            self.typed-panic('X::Syntax::AddCategorical::TooManyParts',
              :$category, :needs(1)
            ) unless @parts == 1;
        }

        # If the sub is in the form of something:<blah>, then we assume
        # the user is trying to define a custom op for an unknown category
        # We also reserve something:sym<blah> form for future use
        # (see https://irclogs.raku.org/perl6/2017-01-25.html#17:27-0002)
        # If it's neither of those cases, then it's just a sub with an
        # extended name like sub foo:bar<baz> {}; let the user use it.
        else {
            self.typed-panic(
                'X::Syntax::Extension::Category', :$category
            ) if nqp::iseq_s($subname, "$category:<$opname>")
              || nqp::iseq_s($subname, "$category:sym<$opname>")
                   && self.language-revision < 2;

            self.typed-panic(
                'X::Syntax::Reserved', :reserved(':sym<> colonpair')
            ) if nqp::isne_i(nqp::index($subname, ':sym<'), -1);
            return 0;
        }

        # This is a canary that will let itself be known should someone make
        # changes to the setting that would cause a grammar change, and thus
        # a slowdown.
        self.panic(
"Don't change grammar in the setting, please!  The settings should
only have 1 MAIN grammar, otherwise it will slow down loading of
Rakudo significantly on *every* run."
        ) if $*COMPILING_CORE_SETTING;

        $declarand := $declarand.compile-time-value if $declarand;

        # when importing, reuse known precedence overrides
        if %prec && nqp::can($declarand,'prec') {
            for $declarand.prec.FLATTENABLE_HASH {
                %prec{$_.key} := $_.value;
            }
        }

        # Mix an appropriate role into the grammar for parsing the new term
        my $grammar-mixin;
        if $category eq 'term' {
            my role Term[$meth-name, $op] {
                token ::($meth-name) { $<sym>=[$op] }
            }
            $grammar-mixin := Term.HOW.curry(Term, $canname, $opname);
        }

        # Mix an appropriate role into the grammar for parsing the new op
        elsif $is-operator {
            my role Oper[$meth-name, $op, $precedence, $declarand] {
                token ::($meth-name) {
                    $<sym>=[$op]
                }
            }
            $grammar-mixin := Oper.HOW.curry(
              Oper, $canname, $opname, %prec, $declarand
            );
        }

        # Find opener and closer and parse an EXPR between them.
        # XXX One day semilist would be nice, but right now that
        # runs us into fun with terminators.
        elsif $category eq 'postcircumfix' {
            my role Postcircumfix[$meth_name, $starter, $stopper] {
                token ::($meth_name) {
                    :my $*GOAL := $stopper;
                    :my $cursor := nqp::getlex('$¢');
                    :my $stub := $cursor.define_slang('MAIN', %*LANG<MAIN> := $cursor.unbalanced($stopper).WHAT, $cursor.actions);
                    $starter ~ $stopper [ <.ws> <statement> ]
                }
            }
            $grammar-mixin := Postcircumfix.HOW.curry(
              Postcircumfix, $canname, @parts[0], @parts[1]
            );
        }

        # Find opener and closer and parse an EXPR between them.
        else {  # $category eq 'circumfix'
            my role Circumfix[$meth_name, $starter, $stopper] {
                token ::($meth_name) {
                    :my $*GOAL := $stopper;
                    :my $cursor := nqp::getlex('$¢');
                    :my $stub := $cursor.define_slang(
                      'MAIN',
                      %*LANG<MAIN> := $cursor.unbalanced($stopper).WHAT,
                      $cursor.actions
                    );
                    $starter ~ $stopper <semilist>
                }
            }
            $grammar-mixin := Circumfix.HOW.curry(
              Circumfix, $canname, @parts[0], @parts[1]
            );
        }

        # This also becomes the current MAIN. Also place it in %?LANG.
        self.HOW.mixin(self, $grammar-mixin);
        %*LANG<MAIN> := self.WHAT;

        # Declarand should get precedence traits.
        if $is-operator && nqp::isconcrete($declarand) {
            my $base_prec := %prec<prec>;
            #$declarand.add-trait(self.actions.r('Trait', 'Is').new(
            #    :name(self.actions.r('Name').from-lexical('prec')),
            #    :argument(self.action.r('Circumfix', 'Parentheses').new(
            #        self.actions.r('SemiList').new(
            #            self.action.r('')
            #        )
            #    )),
            #));
            #$*W.apply_trait(self.MATCH, '&trait_mod:<is>', $declarand,
            #    :prec(nqp::hash('prec', $base_prec)));
        }

        # May also need to add to the actions.
        my $actions-mixin := nqp::null;
        if $category eq 'postcircumfix' {
            my role PostcircumfixAction[$meth, $subname] {
                method ::($meth)($/) {
                    make QAST::Op.new(
                        :op('call'), :name('&' ~ $subname), :node($/),
                        $<statement>.ast
                    );
                }
            };
            $actions-mixin := PostcircumfixAction.HOW.curry(
              PostcircumfixAction, $canname, $subname
            );
        }
        elsif $category eq 'circumfix' {
            my role CircumfixAction[$meth, $subname] {
                method ::($meth)($/) {
                    make QAST::Op.new(
                        :op('call'), :name('&' ~ $subname), :node($/),
                        $<semilist>.ast
                    );
                }
            };
            $actions-mixin := CircumfixAction.HOW.curry(
              CircumfixAction, $canname, $subname
            );
        }
        elsif $category eq 'term' {
            my role TermAction[$meth, $subname] {
                method ::($meth)($/) {
                    self.attach: $/, $actions.r('Term', 'Named').new($subname)
                }
            };
            my role TermActionConstant[$meth, $name] {
                method ::($meth)($/) {
                    self.attach: $/, $actions.r('Term', 'Name').new($actions.r('Name').from-identifier($name))
                }
            };
            $actions-mixin := $defterm
              ?? TermAction.HOW.curry(TermActionConstant, $canname, $subname)
              !! TermAction.HOW.curry(TermAction, $canname, $subname);
        }

        # Set up next statement to have new actions.
        $actions := $actions.HOW.mixin($actions, $actions-mixin)
          unless nqp::isnull($actions-mixin);
        %*LANG<MAIN-actions> := $actions;
        self.define_slang('MAIN', self.WHAT, $actions);

        # Set up the rest of this statement to have new actions too.
        self.set_actions($actions);

        my $scalar := p6ize_recursive(%*LANG);
        my $descriptor_type := $*R.type-from-setting('ContainerDescriptor');
        my $descriptor := $descriptor_type.new( :dynamic(1), :name("LANG") );
        nqp::bindattr($scalar, $*R.type-from-setting('Scalar'), '$!descriptor', $descriptor);

        $*R.outer-scope.merge-generated-lexical-declaration(
            :resolver($*R),
            :force,
            self.actions.r('VarDeclaration', 'Implicit', 'Constant').new(
                :name('%?LANG'),
                :value($scalar),
            )
        );

        $*LANG := self;
        #$*LEAF := self;
        return 1;
    }

#-------------------------------------------------------------------------------
# Whitespace handling

    # ws is highly performance sensitive. So, we check if we already marked it
    # at this point with a simple method, and only if that is not the case do
    # we bother doing any pattern matching.
    method ws() { self.MARKED('ws') ?? self !! self.mark-whitespace }

    # Helper method for "ws" to mark whitespace as parsed, so it doesn't
    # need to be parsed again after having backtracked
    token mark-whitespace {
        <.reset-expectations>
        :dba('whitespace')
        <!ww>
        [
          | [ \r\n || \v ] <.heredoc>
          | <.horizontal-whitespace>
          | <.unspace>
        ]*
        <?MARKER('ws')>
        <.reset-expectations>
    }

    # Handle unspace (aka, the capability to continue a line by putting
    # a lone backspace at the end of a line)
    token unspace {
        \\ <?before \s | '#'>
        :dba('unspace')
        [
          | <.vertical-whitespace>
          | <.horizontal-whitespace>
          | <.unspace>
        ]*
    }

    # Handle vertical whitespace including version control markers
    token vertical-whitespace {
        :dba('vertical whitespace')
        [
          [
            | \v
            | '<<<<<<<'
              {}                       # disable LTM
              <?before [.*? \v '=======']: .*? \v '>>>>>>>' >
              <.sorry: 'Found a version control conflict marker'> \V* \v
            | '======='
              {}                       # disable LTM
              .*? \v '>>>>>>>' \V* \v  # ignore second part of a conflict marker
          ]
        ]+
    }

    # Handle horizontal whitespace including any RakuDoc
    token horizontal-whitespace {
        :dba('horizontal whitespace')
        [
          | \h+
          | \h* <.comment>
          | <?before \h* '=' [ \w | '\\'] > ^^ <.doc-TOP>
        ]
    }

#-------------------------------------------------------------------------------
# Comment handling

    proto token comment {*}

    token comment:sym<#> {
       '#' {} \N*
    }

    token comment:sym<#`(...)> {
        '#`' <?opener>
        <.quibble(self.Quote)>
    }
    token comment:sym<#`> {
        '#`' <!after \s> <!opener>
        <.typed-panic: 'X::Syntax::Comment::Embedded'>
    }

    # single / multi-line leading declarator block
    token comment:sym<#|> { '#|' \h $<attachment>=[\N* \n?] }
    token comment:sym<#|(...)> {
       '#|' <?opener> <attachment=.quibble(self.Quote)>
    }

    # single / multi-line trailing declarator block
    token comment:sym<#=> { '#=' \h+ $<attachment>=[\N* \n?] }
    token comment:sym<#=(...)> {
        '#=' <?opener> <attachment=.quibble(self.Quote)> \n?
    }

#-------------------------------------------------------------------------------
# RakuDoc

    token doc-TOP {
        { $*PARSING-DOC-BLOCK := 1 }
        :my $*SEEN := {};
        <.doc-newline>*
        <doc-block>
        { $*PARSING-DOC-BLOCK := 0 }
    }

    # directives that may not be used as block names
    token rakudoc-directives {
        [
        alias | begin | column | config | end | finish | for | place | row
        ] >>
    }

    proto token doc-block {*}

    # handle =finish
    token doc-block:sym<finish> {
        ^^ \h* '=finish' <doc-newline> $<finish> = .*
    }

    # handle =alias
    token doc-block:sym<alias> {
        :my $width;

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # fetch lemma as first line
        '=alias' \h+ $<lemma>=<.doc-identifier> \h+ $<first>=\N+

        { $width := $<first>.from - $<margin>.to - 1 }
        [\n $<margin> '='   " " ** {$width}   $<line>=\N+]*

        \n?
    }

    # handle =column
    token doc-block:sym<column> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # fetch column and any configuration
        '=column' [ [\n $<margin> '=']? \h+ <colonpair> ]*
        { $/.panic("=column outside of table") unless $*IN_TABLE }

        # should now be at end of line
        <.doc-newline>
    }

    # handle =row
    token doc-block:sym<row> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # fetch row and any configuration
        '=row' [ [\n $<margin> '=']? \h+ <colonpair> ]*
        { $/.panic("=row outside of table") unless $*IN_TABLE }

        # should now be at end of line
        <.doc-newline>
    }

    # handle =place
    token doc-block:sym<place> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # needs a schema
        '=place' \s+ $<uri>=[ \w+ ':' \S+ ]

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # should now be at end of line
        <.doc-newline>
    }

    # handle =formula
    token doc-block:sym<formula> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # needs an actual formula
        '=formula' \s+ $<formula>=<-[ \v : ]>+

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # should now be at end of line
        <.doc-newline>
    }

    # handle =config
    token doc-block:sym<config> {

        ^^ $<margin>=[ \h* ] '=config'

        [\h+ $<doc-identifier>=[ <.doc-identifier> | '*' ] ]?

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # should now be at end of line
        <.doc-newline>
    }

    # handle =begin on verbatim blocks
    token doc-block:sym<verbatim> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # start of 'begin comment' block
        '=begin' \h+ $<type>=[ comment | code | data | input | output ]

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # any number of newlines
        <.doc-newline>+

        # fetch all non-empty lines, *NOT* looking at =pod markers
        $<lines>=[^^ \N* \n?]*?

        # until the matching end block
        ^^ $<margin> '=end' \h+ $<type> [<.doc-newline> | $]
    }

    # handle all the other =begin
    token doc-block:sym<begin> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # start of 'begin' doc block should have type on same line
        '=begin'
        [ <?doc-newline>
          <.typed-panic('X::Syntax::Pod::BeginWithoutIdentifier')>
        ]?

        # must have some whitespace between header and type
        \h+

        # may not be a directive name
        [ $<directive>=<.rakudoc-directives> {
              self.typed-panic(
                'X::Syntax::Pod::BeginWithDirective',
                :directive(~$<directive>)
              )
          }
        ]?

        # identifier indicates type of block
        $<type>=<.doc-identifier>

        {
            my str $type := ~$<type>;
            if $type eq 'table' {
                $*IN_TABLE++;
            }
            elsif $type eq 'cell' {
                $/.panic("=begin cell outside of table") unless $*IN_TABLE;
            }
        }

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # should now be at end of line
        <.doc-newline>+

        # get any doc blocks
        <doc-content>*

        # until the end marker
        ^^ $<margin> '=end' \h+

        [
          # we're all good, same identifier as at beginning
          $<type> [ <.doc-newline> | $ ]

          # alas, a mismatch
          || $<instead>=<.doc-identifier>? {self.typed-panic(
               'X::Syntax::Pod::BeginWithoutEnd.new',
               type    => ~$<type>,
               spaces  => ~$<margin>,
               instead => $<instead> ?? ~$<instead> !! ''
              )}
        ]

        { $*IN_TABLE-- if ~$<type> eq 'table' }
    }

    token doc-block:sym<for> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ]

        # start of a 'for' doc block should have type on same line
        '=for'
        [ <?doc-newline>
          <.typed-panic('X::Syntax::Pod::BeginWithoutIdentifier')>
        ]?

        # must have some whitespace between header and type
        \h+

        # may not be a directive name
        [ $<directive>=<.rakudoc-directives> {
              self.typed-panic(
                'X::Syntax::Pod::BeginWithDirective',
                :directive(~$<directive>),
                :for<for>
              )
          }
        ]?

        # identifier indicates type of block
        $<type>=<.doc-identifier>

        # cells not allowed outside table
        {
            $/.panic("=for cell outside of table")
              if ~$<type> eq 'cell' && !$*IN_TABLE;
        }

        # fetch any configuration
        [ [\n $<margin> '=']? \h+ <colonpair> ]*

        # should now be at end of line
        <.doc-newline>

        # and any following lines as well
        $<lines>=[[^^ $<margin> \h* [ <-[=\n]> | '=' ** 2..* ] \N* \n? ]* \n*]
    }

    token doc-block:sym<abbreviated> {

        # save any leading whitespace from start of line
        ^^ $<margin>=[ \h* ] '='

        # handled elsewhere
        <!.before <.rakudoc-directives>>

        # start of an abbreviated doc block with an optional hash
        # char to indicate :numbered in config, or just a string
        $<type>=<.doc-identifier> [\h+ <doc-numbered>]?

        # cells not allowed outside table
        {
            $/.panic("=cell outside of table")
              if ~$<type> eq 'cell' && !$*IN_TABLE;
        }

        # the rest of the line is considered content
        [ [ \h+ $<header>=[\N+ \n?]? ] | <.doc-newline> ]

        # and any following lines as well
        $<lines>=[[^^ $<margin> \h* [ <-[=\n]> | '=' ** 2..* ] \N* \n? ]* \n*]
    }

    token doc-block:sym<lines> {
        [^^ \h* [ <![=]> \N+ | '=' \W \N* ] \n?]+
    }

    token doc-identifier {
        <.alpha> [ <[-']> | [\d* <.alpha>+] ]* $<level>=\d*
    }

    token doc-content {
        <doc-block>
        <.doc-newline>*
    }

    token doc-numbered { <.after [^|\s]> '#' <.before \s> }

    token doc-newline { \h* \n }
}

#-------------------------------------------------------------------------------
# The base grammar for supported quote languages in Raku.  It provides a
# large number of roles to tweak the base grammer into a grammar that can be
# used for a specific set of quote language adverbs.  Generally, each adverb
# has an "on" version (with postfix "1") and an "off" version (with postfix
# "0").
grammar Raku::QGrammar is HLL::Grammar does Raku::Common {

#-------------------------------------------------------------------------------
# Escape / backslash sequences

    proto token escape    {*}
    proto token backslash {*}

    # Allow for general backslash support, aka "qq" and friends
    role b1 {

        # simple backslash sequences
        token backslash:sym<a>  { <.sym> }
        token backslash:sym<b>  { <.sym> }
        token backslash:sym<e>  { <.sym> }
        token backslash:sym<f>  { <.sym> }
        token backslash:sym<n>  { <.sym> }
        token backslash:sym<r>  { <.sym> }
        token backslash:sym<t>  { <.sym> }
        token backslash:sym<0>  { <.sym> }
        token backslash:sym<\\> { <.sym> }
        token backslash:sym<rn> { 'r\n' }
        token backslash:sym<misc> { \W }

        token backslash:sym<qq> {
            <?[q]> <quote=.LANG('MAIN','quote')>
        }
        token backslash:delim {
            <text=.starter> | <text=.stopper>
        }
        token backslash:sym<c> {
            <.sym> <charspec>
        }
        token backslash:sym<N> {
            <?before 'N{'<.[A..Z]>> <.obs: '\N{CHARNAME}','\c[CHARNAME]'>
        }
        token backslash:sym<o> {
            :dba('octal character')
            <.sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ]
        }

        token backslash:sym<x> {
            :dba('hex character')
            <.sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ]
        }

        token backslash:sym<1> {
            <[1..9]>\d* {
              self.typed-panic: 'X::Backslash::UnrecognizedSequence',
                :sequence(~$/), :suggestion('$' ~ ($/ - 1))
            }
        }
        token backslash:sym<unrec> {
          {}
          (\w)
          {
              self.typed-panic: 'X::Backslash::UnrecognizedSequence',
                :sequence($/[0].Str)
          }
        }

        token escape:sym<\\> {
            <.sym> {} <item=.backslash>
        }
    }

    # do NOT allow any backslashes, aka "q"
    role b0 {
        token escape:sym<\\> { <!> }
    }

#-------------------------------------------------------------------------------
# Interpolation

    # interpolate scalars ($)
    role s1 {
        token escape:sym<$> {
            :my $*QSIGIL := '$';
            <?[$]>
            <!RESTRICTED>
            [ <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
               || { self.typed-panic: 'X::Backslash::NonVariableDollar' } ]
        }
    }

    # do NOT interpolate scalars
    role s0 { token escape:sym<$> { <!> } }

    # interpolate arrays (@)
    role a1 {
        token escape:sym<@> {
            :my $*QSIGIL := '@';
            <?[@]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    # do NOT interpolate arrays
    role a0 { token escape:sym<@> { <!> } }

    # interpolate hashes (%)
    role h1 {
        token escape:sym<%> {
            :my $*QSIGIL := '%';
            <?[%]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    # do NOT interpolate hashes
    role h0 { token escape:sym<%> { <!> } }

    # interpolate subroutine calls (&)
    role f1 {
        token escape:sym<&> {
            :my $*QSIGIL := '&';
            <?[&]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    # do NOT interpolate function calls
    role f0 { token escape:sym<&> { <!> } }

    # allow interpolated closures ({...})
    role c1 {
        token escape:sym<{ }> {
            :my $*ESCAPEBLOCK := 1;
            <?[{]> <!RESTRICTED> <block=.LANG('MAIN','block')>
        }
    }

    # do NOT allow interpolated closures
    role c0 { token escape:sym<{ }> { <!> } }

#-------------------------------------------------------------------------------
# Affecting global behaviour of quote languages

    # allow for quoted words
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

    # allow for format specification
    role o {
        method postprocessors () { nqp::list_s('format') }
    }

    # base role for heredocs
    role to[$herelang] {
        method herelang() { $herelang }
        # heredoc strings are the only postproc when present
        method postprocessors () { nqp::list_s('heredoc') }
    }

    # base role for q//, aka '' parsing
    role q {
        token starter { \' }
        token stopper { \' }

        token escape:sym<\\> { <.sym> <item=.backslash> }

        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
        token backslash:sym<\\> { <.sym> }
        token backslash:delim { <text=.starter> | <text=.stopper> }

        token backslash:sym<miscq> { {} . }

        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

    # base role for qq//, aka "" parsing
    role qq does b1 does s1 does a1 does h1 does f1 does c1 {
        token starter { \" }
        token stopper { \" }
        method tweak_q($v)  { self.panic("Too late for :q")  }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

#-------------------------------------------------------------------------------
# Helper methods that map a given adverb to the appropriate tweak.  The
# naming of the "tweak_" indicates the adverb they are being associated with.
# See the "create-quote-lang-type" sub on where this is being done.

    # helper method for quote lang adverbs that cannot be negated
    method truly($bool, $opt) {
        $bool
          ?? self
          !! self.sorry("Cannot negate $opt adverb")
    }

    # helper method to mixin a give role
    method apply_tweak($role) {
        my $target := nqp::can(self, 'herelang') ?? self.herelang !! self;
        $target.HOW.mixin($target, $role);
        self
    }

    # helper role to set the post-processors
    my role postproc[@curlist] {
        method postprocessors() { @curlist }
    }

    # helper method to add a post-processor
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

    # tweaks that can NOT be negated
    method tweak_q($v) {
        self.truly($v, ':q');
        self.apply_tweak(Raku::QGrammar::q)
    }
    method tweak_qq($v) {
        self.truly($v, ':qq');
        self.apply_tweak(Raku::QGrammar::qq)
    }
    method tweak_regex($v) {
        self.truly($v, ':regex');
        self.Regex
    }
    method tweak_to($v) {
        self.truly($v, ':to');
        # the cursor_init is to ensure it's been initialized the same way
        # 'self' was back in quote-lang
        my $q := self.Quote;
        self.lang-cursor($q.HOW.mixin($q, to.HOW.curry(to, self)))
    }

    # tweaks that disable when negated
    method tweak_a($v) { self.apply_tweak($v ?? a1 !! a0) }
    method tweak_b($v) { self.apply_tweak($v ?? b1 !! b0) }
    method tweak_c($v) { self.apply_tweak($v ?? c1 !! c0) }
    method tweak_f($v) { self.apply_tweak($v ?? f1 !! f0) }
    method tweak_h($v) { self.apply_tweak($v ?? h1 !! h0) }
    method tweak_s($v) { self.apply_tweak($v ?? s1 !! s0) }

    method tweak_o($v) { $v ?? self.apply_tweak(o) !! self }
    method tweak_x($v) { $v ?? self.add-postproc("exec")  !! self }
    method tweak_w($v) { $v ?? self.add-postproc("words") !! self }
    method tweak_v($v) { $v ?? self.add-postproc("val")   !! self }
    method tweak_ww($v) {
        $v ?? self.add-postproc("quotewords").apply_tweak(ww) !! self
    }

    # tweaks accessed by their long adverb name, mapping to short
    method tweak_array($v)      { self.tweak_a($v)  }
    method tweak_backslash($v)  { self.tweak_b($v)  }
    method tweak_closure($v)    { self.tweak_c($v)  }
    method tweak_double($v)     { self.tweak_qq($v) }
    method tweak_exec($v)       { self.tweak_x($v)  }
    method tweak_format($v)     { self.tweak_o($v)  }
    method tweak_function($v)   { self.tweak_f($v)  }
    method tweak_hash($v)       { self.tweak_h($v)  }
    method tweak_heredoc($v)    { self.tweak_to($v) }
    method tweak_quotewords($v) { self.tweak_ww($v) }
    method tweak_scalar($v)     { self.tweak_s($v)  }
    method tweak_single($v)     { self.tweak_q($v)  }
    method tweak_val($v)        { self.tweak_v($v)  }
    method tweak_words($v)      { self.tweak_w($v)  }

#-------------------------------------------------------------------------------
# Actual slang nibbling logic

    token nibbler {
        :my @*NIBBLES;
        <.do-nibbling>
    }

    token do-nibbling {
        :my int $from := self.pos;
        :my int $to   := $from;
        :my @NIBBLES;
        [
          <!stopper>
          [
            || <starter>
               <nibbler>
               <stopper>
               {
                   my $c := $/;
                   $to   := $<starter>[-1].from;
                   nqp::push(@NIBBLES,nqp::substr($c.orig,$from,$to - $from))
                     if $from != $to;

                   nqp::push(@NIBBLES,$<starter>[-1].Str);
                   nqp::push(@NIBBLES,$<nibbler>[-1]);
                   nqp::push(@NIBBLES,$<stopper>[-1].Str);

                   $from := $to := $c.pos;
               }
            || <escape>
               {
                   my $c := $/;
                   $to   := $<escape>[-1].from;
                   nqp::push(@NIBBLES,nqp::substr($c.orig,$from,$to - $from))
                     if $from != $to;

                   nqp::push(@NIBBLES,$<escape>[-1]);

                   $from := $to := $c.pos;
               }
            || .
          ]
        ]*
        {
            my $c := $/;
            $to   := $c.pos;
            $*LASTQUOTE := [self.pos, $to];
            nqp::push(@NIBBLES,nqp::substr($c.orig,$from,$to - $from))
              if $from != $to || !@NIBBLES;
            @*NIBBLES := @NIBBLES;
        }
    }
}

#-------------------------------------------------------------------------------
# Grammar to parse Raku regexes, mostly consisting of overrides allowing
# HLL actions on what is an NQP grammar

grammar Raku::RegexGrammar is QRegex::P6Regex::Grammar does Raku::Common {
    method throw_unrecognized_metachar ($metachar) {
        self.typed-sorry: 'X::Syntax::Regex::UnrecognizedMetachar', :$metachar;
    }
    method throw_null_pattern() {
        self.typed-sorry: 'X::Syntax::Regex::NullRegex';
    }
    method throw_unrecognized_regex_modifier($modifier) {
        self.typed-panic: 'X::Syntax::Regex::UnrecognizedModifier', :$modifier;
    }

    method throw_malformed_range() {
        self.typed-sorry: 'X::Syntax::Regex::MalformedRange';
    }
    method throw_confused() {
        self.typed-sorry: 'X::Syntax::Confused';
    }
    method throw_unspace($char) {
        self.typed-sorry: 'X::Syntax::Regex::Unspace', :$char;
    }
    method throw_regex_not_terminated() {
        self.typed-sorry: 'X::Syntax::Regex::Unterminated';
    }
    method throw_spaces_in_bare_range() {
        self.typed-sorry: 'X::Syntax::Regex::SpacesInBareRange';
    }
    method throw_non_quantifiable() {
        self.typed-sorry: 'X::Syntax::Regex::NonQuantifiable';
    }
    method throw_solitary_quantifier() {
        self.typed-panic: 'X::Syntax::Regex::SolitaryQuantifier';
    }
    method throw_solitary_backtrack_control() {
        self.typed-sorry: 'X::Syntax::Regex::SolitaryBacktrackControl';
    }

    token normspace { <?before \s | '#'> <.LANG('MAIN', 'ws')> }

    token rxstopper { <stopper> }

    token metachar:sym<:my> {
        ':' <?before ['my'|'constant'|'state'|'our'|'temp'|'let'] » >
        <statement=.LANG('MAIN', 'statement')>
        <!RESTRICTED>
        <.LANG('MAIN', 'eat-terminator')>
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
        { self.check-variable($<var>) }
        [
        || $<binding> = ( \s* '=' \s* <quantified_atom> )
        || <?before '.'? <.[ \[ \{ \< ]>>
            <.worry: "Apparent subscript will be treated as regex">
        ]?
        <.SIGOK>
    }

    token metachar:sym<qw> {
        <?before '<' \s >  # (note required whitespace)
        '<'
        <nibble(self.quote-lang(self.Quote, "<", ">", 'q', [['w', 1],]))>
        '>'
        <.SIGOK>
    }

    token metachar:sym<'> {
        <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]>
        <quote=.LANG('MAIN','quote')>
        <.SIGOK>
    }

    token metachar:sym<{}> {
        \\<[xo]>'{'
        <.obsbrace>
    }

    token metachar:sym<mod> {
        ':'
        $<negated>  = '!'?
        $<modifier> = \w+

        :my $*NEGATED;
        :my $*MODIFIER;
        {
            $*NEGATED  := ?~$<negated>;
            $*MODIFIER := self.slangs<MAIN>.adverb-rx2str(~$<modifier>);
        }
    }

    token backslash:sym<1> {
        <.[\d] - [0]>\d*
        {}
        :my int $br := nqp::radix(10, $/, 0, 0)[0];
        {self.typed-panic: 'X::Backslash::UnrecognizedSequence',
          :sequence(~$/),
          :suggestion('$' ~ ($/ - 1))
        }
    }

    token assertion:sym<name> {
        <longname=.LANG('MAIN','longname')>
        [
          | <?[>]>

          | '=' <assertion>

          | ':' <arglist>

          | '(' <arglist> ')'

          | <.normspace>
              <nibbler>
        ]?
    }

    token assertion:sym<{ }>  {     <?before '{'> <codeblock> }
    token assertion:sym<?{ }> { '?' <?before '{'> <codeblock> }
    token assertion:sym<!{ }> { '!' <?before '{'> <codeblock> }

    token assertion:sym<var> {
        [
          | <?[&]>
            <!RESTRICTED>
            <call=.LANG('MAIN', 'term:sym<variable>')>
            [
              | ':' <arglist>

              | '(' <arglist> ')'
            ]?

          | <?sigil>
            <!RESTRICTED>
            <var=.LANG('MAIN', 'term:sym<variable>')>
        ]
    }

    token atom {
        # :dba('regex atom')
        [
          | \w
          [ <?before ' ' \w <!before <.quantifier> > >
            <!{ $*WHITESPACE-OK }>
            <.typed-worry: 'X::Syntax::Regex::InsignificantWhitespace'>
          ]?
          <.SIGOK>

          | <metachar>
        ]
    }

    token assertion:sym<~~> {
        <.sym>
        <!RESTRICTED>
        [    <?[>]>

          | $<num>=[\d+]

          | <desigilname=.LANG('MAIN','desigilname')>
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

#-------------------------------------------------------------------------------
# Grammar to parse PCRE like regexes

grammar Raku::P5RegexGrammar is QRegex::P5Regex::Grammar does Raku::Common {
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
        :my $*ESCAPEBLOCK := 1;
        <block=.LANG('MAIN','block')>
    }
}
