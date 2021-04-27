# Proc is a wrapper around Proc::Async, providing a synchronous API atop of
# the asynchronous API.
my class Proc {
    has IO::Pipe $.in;
    has IO::Pipe $.out;
    has IO::Pipe $.err;
    has $.exitcode is default(Nil);
    has $.signal;
    has $.pid is default(Nil);
    has @.command;

    has Proc::Async $!proc;
    has Bool $!w;
    has @!pre-spawn;
    has @!post-spawn;
    has $!active-handles = 0;
    has &!start-stdout;
    has &!start-stderr;
    has $!finished;

    submethod BUILD(:$in = '-', :$out = '-', :$err = '-', :$exitcode,
                    Bool :$bin, Bool :$chomp = True, Bool :$merge, :$command,
                    Str :$enc, Str:D :$nl = "\n", :$signal --> Nil) {
        @!command := $command.List if $command;
        if nqp::istype($in, IO::Handle) && $in.DEFINITE {
            @!pre-spawn.push({ $!proc.bind-stdin($in) });
        }
        elsif $in === True {
            my $cur-promise = Promise.new;
            $cur-promise.keep(True);
            $!in = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-out => $nl,
                :on-write(-> $blob {
                    $cur-promise .= then({ await $!proc.write($blob) });
                }),
                :on-close({
                    $cur-promise .= then({ $!proc.close-stdin; });
                    self!await-if-last-handle
                }));
            ++$!active-handles;
            $!w := True;
        }
        elsif nqp::istype($in, Str) && $in eq '-' {
            # Inherit; nothing to do
        }
        else {
            $!w := True;
            @!post-spawn.push({ $!proc.close-stdin });
        }

        if $merge {
            if nqp::istype($out, IO::Handle) && $out.DEFINITE {
                @!pre-spawn.push({
                    $!proc.stdout(:bin).merge($!proc.stderr(:bin)).act: { $out.write($_) }
                });
            } else {
                my $chan = Channel.new;
                $!out = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                    :on-read({ (try $chan.receive) // buf8 }),
                    :on-close({ self!await-if-last-handle }));
                ++$!active-handles;
                @!pre-spawn.push({
                    $!proc.stdout(:bin).merge($!proc.stderr(:bin)).act: { $chan.send($_) },
                        done => { $chan.close },
                        quit => { $chan.fail($_) }
                });
            }
        }
        else {
            if $out === True {
                my $chan;
                my $stdout-supply;
                &!start-stdout = {
                    $chan = $stdout-supply.Channel;
                    &!start-stdout = Nil;
                }
                $!out = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                    :on-read({
                        &!start-stdout() if &!start-stdout;
                        (try $chan.receive) // buf8
                    }),
                    :on-close({
                        $chan //= $stdout-supply.Channel; # If we never read
                        self!await-if-last-handle
                    }),
                    :on-native-descriptor({
                        $!active-handles--;
                        &!start-stdout = Nil;
                        await $stdout-supply.native-descriptor
                    }));
                ++$!active-handles;
                @!pre-spawn.push({
                    $stdout-supply = $!proc.stdout(:bin)
                });
            }
            elsif nqp::istype($out, IO::Handle) && $out.DEFINITE {
                @!pre-spawn.push({ $!proc.bind-stdout($out) });
            }
            elsif nqp::istype($out, Str) && $out eq '-' {
                # Inherit; nothing to do
            }
            else {
                @!pre-spawn.push({
                    $!proc.stdout(:bin).tap: -> $ { }, quit => -> $ { }
                });
            }

            if $err === True {
                my $chan;
                my $stderr-supply;
                &!start-stderr = {
                    $chan = $stderr-supply.Channel;
                    &!start-stderr = Nil;
                }
                $!err = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                    :on-read({
                        &!start-stderr() if &!start-stderr;
                        (try $chan.receive) // buf8
                    }),
                    :on-close({
                        $chan //= $stderr-supply.Channel; # If we never read
                        self!await-if-last-handle
                    }),
                    :on-native-descriptor({
                        &!start-stderr = Nil;
                        $!active-handles--;
                        await $stderr-supply.native-descriptor
                    }));
                ++$!active-handles;
                @!pre-spawn.push({
                    $stderr-supply = $!proc.stderr(:bin);
                });
            }
            elsif nqp::istype($err, IO::Handle) && $err.DEFINITE {
                @!pre-spawn.push({ $!proc.bind-stderr($err) });
            }
            elsif nqp::istype($err, Str) && $err eq '-' {
                # Inherit; nothing to do
            }
            else {
                @!pre-spawn.push({
                    $!proc.stderr(:bin).tap: -> $ { }, quit => -> $ { }
                });
            }
        }

        if nqp::istype($exitcode, Int) && $exitcode.DEFINITE {
            $!exitcode = $exitcode;
        }
        if nqp::istype($signal, Int) && $signal.DEFINITE {
            $!signal = $signal;
        }
    }

    method !await-if-last-handle() {
        self!wait-for-finish unless --$!active-handles;
        self
    }

    method !wait-for-finish {
        CATCH { default { self!set-status(0x100) } }
        &!start-stdout() if &!start-stdout;
        &!start-stderr() if &!start-stderr;
        self!set-status(await($!finished).status)
          if nqp::istype($!exitcode,Nil);
    }

    method spawn(*@args where .so, :$cwd = $*CWD, :$env, :$arg0, :$win-verbatim-args = False --> Bool:D) {
        @!command := @args.List;
        self!spawn-internal(@args, $cwd, $env, :$arg0, :$win-verbatim-args)
    }

    method shell($cmd, :$cwd = $*CWD, :$env --> Bool:D) {
        @!command := $cmd.List;
        my @args := Rakudo::Internals.IS-WIN
            ?? (%*ENV<ComSpec>, '/c', $cmd)
            !! ('/bin/sh', '-c', $cmd);
        self!spawn-internal(@args, $cwd, $env, :win-verbatim-args)
    }

    method !spawn-internal(@args, $cwd, $env, :$arg0, :$win-verbatim-args --> Bool:D) {
        my %ENV := $env ?? $env.hash !! %*ENV;
        $!proc := Proc::Async.new(|@args, :$!w, :$arg0, :$win-verbatim-args);
        .() for @!pre-spawn;
        $!finished = $!proc.start(:$cwd, :%ENV, scheduler => $PROCESS::SCHEDULER);
        my $is-spawned := do {
            CATCH { default { self!set-status(0x100) } }
            $!pid = await $!proc.ready;
            True
        } // False;
        .() for @!post-spawn;
        self!wait-for-finish unless $!out || $!err || $!in;
        $is-spawned
    }

    method !set-status($new_status) {
        $!exitcode = $new_status +> 8;
        $!signal   = $new_status +& 0xFF;
    }
    method !status() {
        self!wait-for-finish;
        ($!exitcode +< 8) +| $!signal
    }

    # see https://github.com/rakudo/rakudo/issues/1366
    # should be deprecated and removed
    proto method status(|) {*}
    multi method status($new_status) {
        $!exitcode = $new_status +> 8;
        $!signal   = $new_status +& 0xFF;
    }
    multi method status(Proc:D:)  {
        self!wait-for-finish;
        ($!exitcode +< 8) +| $!signal
    }

    multi method Numeric(Proc:D:) {
        self!wait-for-finish;
        $!exitcode
    }
    multi method Bool(Proc:D:) {
        self!wait-for-finish;
        $!exitcode == 0 && $!signal == 0
    }
    method exitcode {
        self!wait-for-finish;
        $!exitcode
    }
    method signal {
        self!wait-for-finish;
        $!signal
    }

    method sink(--> Nil) {
        self!wait-for-finish;
        X::Proc::Unsuccessful.new(:proc(self)).throw if $!exitcode > 0 || $!signal > 0;
    }
}

proto sub run(|) {*}
multi sub run(*@args where .so, :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str  :$enc, Str:D :$nl = "\n", :$cwd = $*CWD, :$env, :$arg0, :$win-verbatim-args = False) {
    my $proc := Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.spawn(@args, :$cwd, :$env, :$arg0, :$win-verbatim-args);
    $proc
}

proto sub shell($, *%) {*}
multi sub shell($cmd, :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str  :$enc, Str:D :$nl = "\n", :$cwd = $*CWD, :$env) {
    my $proc := Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.shell($cmd, :$cwd, :$env);
    $proc
}

sub QX($cmd, :$cwd = $*CWD, :$env) is implementation-detail {
    my $proc := Proc.new(:out);
    $proc.shell($cmd, :$cwd, :$env);
    $proc.out.slurp(:close) // Failure.new("Unable to read from '$cmd'")
}

# vim: expandtab shiftwidth=4
