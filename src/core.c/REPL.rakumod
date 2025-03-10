my class X::REPL::InvalidEnvironment is Exception {
    has $.reason is required;
    method message() { "Invalid REPL environment: $!reason" }
}

class REPL { ... }

do {
    my sub sorted-set-insert(@values, $value) {
        my $low        = 0;
        my $high       = @values.end;
        my $insert_pos = 0;

        while $low <= $high {
            my $middle = floor($low + ($high - $low) / 2);

            my $middle_elem = @values[$middle];

            if $middle == @values.end {
                if $value eq $middle_elem {
                    return;
                } elsif $value lt $middle_elem {
                    $high = $middle - 1;
                } else {
                    $insert_pos = +@values;
                    last;
                }
            } else {
                my $middle_plus_one_elem = @values[$middle + 1];

                if $value eq $middle_elem || $value eq $middle_plus_one_elem {
                    return;
                } elsif $value lt $middle_elem {
                    $high = $middle - 1;
                } elsif $value gt $middle_plus_one_elem {
                    $low = $middle + 1;
                } else {
                    $insert_pos = $middle + 1;
                    last;
                }
            }
        }

        splice(@values, $insert_pos, 0, $value);
    }

    my role ReadlineBehavior[$WHO] {
        my $Readline = try { require Readline }
        my $read = $Readline.new;
        if !Rakudo::Internals.IS-WIN {
            $read.read-init-file("/etc/inputrc");
            $read.read-init-file(%*ENV<INPUTRC> // "~/.inputrc");
        }
        method init-line-editor {
            $read.read-history($.history-file);
        }
        method repl-read(Mu \prompt) {
            my $line = $read.readline(prompt);

            if $line.defined && $line.match(/\S/) {
                $read.add-history($line);
                $read.append-history(1, $.history-file);
            }

            $line
        }
    }

    my role LinenoiseBehavior[$WHO] {
        my &linenoise                      = $WHO<&linenoise>;
        my &linenoiseHistoryAdd            = $WHO<&linenoiseHistoryAdd>;
        my &linenoiseSetCompletionCallback = $WHO<&linenoiseSetCompletionCallback>;
        my &linenoiseAddCompletion         = $WHO<&linenoiseAddCompletion>;
        my &linenoiseHistoryLoad           = $WHO<&linenoiseHistoryLoad>;
        my &linenoiseHistorySave           = $WHO<&linenoiseHistorySave>;

        method completions-for-line(Str $line, int $cursor-index) { ... }

        method history-file(--> Str:D) { ... }

        method init-line-editor {
            linenoiseSetCompletionCallback(sub ($line, $c) {
                eager self.completions-for-line($line, $line.chars).map(&linenoiseAddCompletion.assuming($c));
            });
            linenoiseHistoryLoad($.history-file);
        }

        method teardown-line-editor {
            my $err = linenoiseHistorySave($.history-file);
            return if !$err;
            note "Couldn't save your history to $.history-file";
        }

        method repl-read(Mu \prompt) {
            self.update-completions;
            my $line = linenoise(prompt);

            if $line.defined && $line.match(/\S/) {
                linenoiseHistoryAdd($line);
            }

            $line
        }
    }

    my role TerminalLineEditorBehavior[$WHO] {
        my $cli-input = $WHO<CLIInput>;
        my $cli;

        method completions-for-line(Str $line, int $cursor-index) { ... }

        method history-file(--> Str:D) { ... }

        method init-line-editor {
            my sub get-completions($contents, $pos) {
                eager self.completions-for-line($contents, $pos)
            }
            $cli = $cli-input.new(:&get-completions);
            $cli.load-history($.history-file);
        }

        method teardown-line-editor {
            $cli.save-history($.history-file);
        }

        method repl-read(Mu \prompt) {
            self.update-completions;
            my $line = $cli.prompt(prompt.chop);

            if $line.defined && $line.match(/\S/) {
                $cli.add-history($line);
            }

            $line
        }
    }

    my role FallbackBehavior {
        method repl-read(Mu \prompt) {
            print prompt;
            $*IN.get
        }
    }

    my role Completions {
        has @!completions = CORE::.keys.flatmap({
                    /^ "&"? $<word>=[\w* <.lower> \w*] $/ ?? ~$<word> !! []
                }).sort;

        method update-completions(--> Nil) {
            my $context := self.compiler.context;

            return unless $context;

            my $pad := nqp::ctxlexpad($context);
            my $it := nqp::iterator($pad);

            while $it {
                my $k := nqp::iterkey_s(nqp::shift($it));
                my $m = $k ~~ /^ "&"? $<word>=[\w* <.lower> \w*] $/;
                next if !$m;
                my $word = ~$m<word>;
                sorted-set-insert(@!completions, $word);
            }

            my $PACKAGE = self.compiler.eval('$?PACKAGE', :outer_ctx($context));

            for $PACKAGE.WHO.keys -> $k {
                sorted-set-insert(@!completions, $k);
            }
        }

        method extract-last-word(Str $line) {
            my $m = $line ~~ /^ $<prefix>=[.*?] <|w>$<last_word>=[\w*]$/;

            return ( $line, '') unless $m;

            ( ~$m<prefix>, ~$m<last_word> )
        }

        method completions-for-line(Str $line, int $cursor-index) {
            return @!completions unless $line;

            # ignore $cursor-index until we have a backend that provides it
            my ( $prefix, $word-at-cursor ) = self.extract-last-word($line);

            # XXX this could be more efficient if we had a smarter starting index
            gather for @!completions -> $word {
                if $word ~~ /^ "$word-at-cursor" / {
                    take $prefix ~ $word;
                }
            }
        }
    }

    class REPL {
        also does Completions;

        has Mu $.compiler;
        has Bool $!multi-line-enabled;
        has IO::Path $!history-file;

        has $!save_ctx;

        # Unique internal values for out-of-band eval results
        has $!need-more-input = {};
        has $!control-not-allowed = {};

        # The grammar / actions to use
        has $!grammar;
        has $!actions;

        sub do-mixin($self, Str $module-name, $behavior, :@extra-modules,
                     Str :$fallback, Bool :$classlike) {
            my Bool $problem = False;
            try {
                CATCH {
                    when {
                        $_ ~~ X::CompUnit::UnsatisfiedDependency
                        and .specification.Str.contains: $module-name
                    } {
                        # ignore it
                    }
                    default {
                        say "I ran into a problem while trying to set up $module-name: $_";
                        if $fallback {
                            say "Falling back to $fallback (if present)";
                        }
                        $problem = True;
                    }
                }

                (require ::($_)) for @extra-modules;
                my $module = do require ::($module-name);
                my $who = $classlike ?? $module.WHO !! $module.WHO<EXPORT>.WHO<ALL>.WHO;
                my $new-self = $self but $behavior.^parameterize($who);
                $new-self.?init-line-editor();
                return ( $new-self, False );
            }

            ( Any, $problem )
        }

        sub mixin-readline($self, |c) {
            do-mixin($self, 'Readline', ReadlineBehavior, |c)
        }

        sub mixin-linenoise($self, |c) {
            do-mixin($self, 'Linenoise', LinenoiseBehavior, |c)
        }

        sub mixin-terminal-lineeditor($self, |c) {
            do-mixin($self, 'Terminal::LineEditor', TerminalLineEditorBehavior,
                     :extra-modules('Terminal::LineEditor::RawTerminalInput',),
                     :classlike, |c)
        }

        sub mixin-line-editor($self) {
            return $self but FallbackBehavior if  %*ENV<INSIDE_EMACS>;

            my %editor-to-mixin = (
                :Linenoise(&mixin-linenoise),
                :Readline(&mixin-readline),
                :LineEditor(&mixin-terminal-lineeditor),
                :none(-> $self { ( $self but FallbackBehavior, False ) }),
            );

            if %*ENV<RAKUDO_LINE_EDITOR> -> $line-editor {
                if !%editor-to-mixin{$line-editor} {
                    say "Unrecognized line editor '$line-editor'";
                    return $self but FallbackBehavior;
                }

                my $mixin = %editor-to-mixin{$line-editor};
                my ( $new-self, $problem ) = $mixin($self);
                return $new-self if $new-self;

                say "Could not find $line-editor module" unless $problem;
                return $self but FallbackBehavior;
            }

            my ( $new-self, $problem ) = mixin-readline($self, :fallback<Linenoise>);
            return $new-self if $new-self;

            ( $new-self, $problem ) = mixin-linenoise($self, :fallback<LineEditor>);
            return $new-self if $new-self;

            ( $new-self, $problem ) = mixin-terminal-lineeditor($self);
            return $new-self if $new-self;

            if $problem {
                say "Continuing without tab completions or line editor.\n";
                say 'You may want to consider installing `rlwrap` for simple line editor functionality.'
                  .naive-word-wrapper;
                say '';
            }
            elsif !Rakudo::Internals.IS-WIN
              && !( %*ENV<_>:exists and %*ENV<_>.ends-with: 'rlwrap' ) {
                say "No line editor found.\n";
                say 'You may want to exit first and `zef install Readline`, `zef install Linenoise`, or `zef install Terminal::LineEditor` or install `rlwrap` for a line editor before entering the REPL again.'
                  .naive-word-wrapper;
                say '';
            }

            $self but FallbackBehavior
        }

        method new(Mu \compiler, Mu \adverbs, $skip = %*ENV<RAKUDO_NO_VERSION>) {
            unless $skip {
                say compiler.version_string(
                  :shorten-versions,
                  :no-unicode(Rakudo::Internals.IS-WIN)
                );
                say '';
            }

            my $multi-line-enabled = !%*ENV<RAKUDO_DISABLE_MULTILINE>;
            my $self = self.bless();
            $self.init(compiler, $multi-line-enabled);
            $self = mixin-line-editor($self);
            $self
        }

        method init(Mu \compiler, $multi-line-enabled --> Nil) {
            if compiler.repl-mode eq 'tty' && not $*IN.t {
                X::REPL::InvalidEnvironment.new(reason => "Unable to initialize REPL outside of a TTY").throw;
            }

            $!compiler := compiler;
            $!grammar  := nqp::null;
            $!actions  := nqp::null;
            $!multi-line-enabled = $multi-line-enabled;
            PROCESS::<$SCHEDULER>.uncaught_handler =  -> $exception {
                note "Uncaught exception on thread $*THREAD.id():\n" ~
                    $exception.gist.indent(4);
            }
        }

        method teardown {
            self.?teardown-line-editor;
        }

        # Calling this method is apparently being done by some ecosystem
        # modules, while this should really be considered an implementation
        # detail.  Keep the original state of the method for now, but
        # this should probably be DEPRECATED and/or given a properly
        # documented and tested API.
        method repl-eval($code, \exception, *%adverbs) {
            CATCH {
                when X::Syntax::Missing {
                    return $!need-more-input
                      if $!multi-line-enabled && .pos == $code.chars;
                    .throw;
                }

                when X::Comp::FailGoal {
                    return $!need-more-input
                      if $!multi-line-enabled && .pos == $code.chars;
                    .throw;
                }

                when X::ControlFlow::Return {
                    return $!control-not-allowed;
                }

                default {
                    exception = $_;
                    return;
                }
            }

            CONTROL {
                when CX::Emit | CX::Take { .rethrow; }
                when CX::Warn { .gist.say; .resume;  }
                return $!control-not-allowed;
            }

            self.compiler.eval($code, |%adverbs)
        }

        method new-repl-eval($code, \exception, @*_, *%adverbs) {
            CATCH {
                when X::Syntax::Missing {
                    return $!need-more-input
                      if $!multi-line-enabled && .pos == $code.chars;
                    .throw;
                }

                when X::Comp::FailGoal {
                    return $!need-more-input
                      if $!multi-line-enabled && .pos == $code.chars;
                    .throw;
                }

                when X::AdHoc {
                    my str $message = .message;
                    if $message eq 'Premature heredoc consumption'
                      || $message.starts-with('Ending delimiter ')
                      || $message.starts-with("Couldn't find terminator")
                      || $message.contains("is immediately followed by a combining codepoint") {
                        return $!need-more-input
                          if $!multi-line-enabled;
                    }
                    .throw
                }

                when X::ControlFlow::Return {
                    return $!control-not-allowed;
                }

                default {
                    exception = $_;
                    return;
                }
            }

            CONTROL {
                when CX::Emit | CX::Take { .rethrow; }
                when CX::Warn { .gist.say; .resume;  }
                return $!control-not-allowed;
            }

            my $*GRAMMAR;
            %adverbs<grammar> := $!grammar unless nqp::isnull($!grammar);
            %adverbs<actions> := $!actions unless nqp::isnull($!actions);
            my $result := self.compiler.eval(
              $code.subst(/ '$*' \d+ /, { '@*_[' ~ $/.substr(2) ~ ']' }, :g),
              |%adverbs
            );

            # Grammar was changed, make sure we use changed one from now on
            if $*GRAMMAR -> $grammar {
                $!grammar := $grammar;
                $!actions := $grammar.actions;
            }

            $result
        }

        method interactive_prompt($index) { "[$index] > " }

        method repl-loop(:$no-exit, *%adverbs) {
            my int $stopped;     # did we press CTRL-c just now?
            my $previous-evals := IterationBuffer.new;  # previous values
#?if !jvm
            signal(SIGINT).tap: {
                exit if $stopped++;
                say "Pressed CTRL-c, press CTRL-c again to exit";
                print self.interactive_prompt($previous-evals.elems);
            }
#?endif

            say $no-exit
              ?? "Type 'exit' to leave"
              !! Rakudo::Internals.IS-WIN
                ?? "To exit type 'exit' or '^Z'"
                !! "To exit type 'exit' or '^D'";

            my $prompt;
            my $code;
            sub reset(--> Nil) {
                $code = '';
                $prompt = self.interactive_prompt($previous-evals.elems);
            }
            reset;

            REPL: loop {
                # Why doesn't the catch-default in repl-eval catch all?
                CATCH {
                    default { say $_; reset }
                }
                my $newcode = self.repl-read(~$prompt);
                last if $no-exit and $newcode and $newcode eq 'exit';

                $stopped = 0;
                my $initial_out_position = $*OUT.tell;

                # An undef $newcode implies ^D or similar
                if !$newcode.defined {
                    last;
                }

                $code = $code ~ $newcode ~ "\n";

                if $code ~~ /^ <.ws> $/ {
                    next;
                }

                my $*CTXSAVE := self;
                my $*MAIN_CTX;

                my $output is default(Nil) = self.new-repl-eval(
                    $code,
                    my $exception,
                    $previous-evals.List,
                    :outer_ctx($!save_ctx),
                    |%adverbs);

                if self.input-incomplete($output) {
                    $prompt = '* ';
                    next;
                }

                if self.input-toplevel-control($output) {
                    say "Control flow commands not allowed in toplevel";
                    reset;
                    next;
                }

                if $*MAIN_CTX {
                    $!save_ctx := $*MAIN_CTX;
                }

                # Print the result if:
                # - there wasn't some other output
                # - the result is an *unhandled* Failure
                # - print an exception if one had occured
                if $exception.DEFINITE {
                    self.repl-print($exception);
                }
                elsif $initial_out_position == $*OUT.tell
                    or nqp::istype($output, Failure) and not $output.handled {
                    self.repl-print($output);
                    $previous-evals.push: $output<>;
                }
                reset;
            }

            self.teardown;
        }

        # This appears to be a magic method that is called somewhere inside
        # the compiler.  The semantics of $*MAIN_CTX and $*CTXSAVE appear
        # to be needed to get a persistency with regards to scope between
        # lines entered in the REPL.
        method ctxsave(--> Nil) {
            $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
            $*CTXSAVE := 0;
        }

        method input-incomplete(Mu $value --> Bool:D) {
            nqp::hllbool(nqp::can($value, 'WHERE'))
              and $value.WHERE == $!need-more-input.WHERE
        }

        method input-toplevel-control(Mu $value --> Bool:D) {
            nqp::hllbool(nqp::can($value, 'WHERE'))
              and $value.WHERE == $!control-not-allowed.WHERE
        }

        method repl-print(Mu $value --> Nil) {
            my $method := %*ENV<RAKU_REPL_OUTPUT_METHOD> // "gist";
            CATCH {
                default { say ."$method"() }
            }
            nqp::can($value,$method)
              and say $value."$method"()
              or say "(low-level object `$value.^name()`)";
        }

        method history-file(--> Str:D) {
            without $!history-file {
                if %*ENV<RAKUDO_HIST> -> $history-file {
                    $!history-file = $history-file.IO;
                }
                else {
                    my $dir := $*HOME || $*TMPDIR;
                    my $old := $dir.add('.perl6/rakudo-history');
                    my $new := $dir.add('.raku/rakudo-history');
                    if $old.e && !$new.e {  # migrate old hist to new location
                        $new.spurt($old.slurp);
                        $old.unlink;
                    }
                    $!history-file = $new;
                }

                without mkdir $!history-file.parent {
                    note "I ran into a problem trying to set up history: {.exception.message}";
                    note 'Sorry, but history will not be saved at the end of your session';
                }
            }

            # make sure there is a history file
            $!history-file.open(:a).close unless $!history-file.e;

            $!history-file.absolute
        }
    }
}

sub repl(*%_) {
    my $repl := REPL.new(nqp::getcomp("Raku"), %_, True);
    nqp::bindattr($repl,REPL,'$!save_ctx',nqp::ctxcaller(nqp::ctx));

    $repl.repl-loop(:no-exit);
}

# vim: expandtab shiftwidth=4
