# Proc is a wrapper around Proc::Async, providing a synchronous API atop of
# the asynchronous API.
my class Proc::Async { ... }
my class Proc {
    has IO::Pipe $.in;
    has IO::Pipe $.out;
    has IO::Pipe $.err;
    has $.exitcode = -1;  # distinguish uninitialized from 0 status
    has $.signal;
    has @.command;

    has Proc::Async $!proc;
    has Bool $!w;
    has @!pre-spawn;
    has @!post-spawn;
    has $!active-handles = 0;
    has $!finished;

    submethod BUILD(:$in = '-', :$out = '-', :$err = '-', :$exitcode,
                    Bool :$bin, Bool :$chomp = True, Bool :$merge, :$command,
                    Str :$enc, Str:D :$nl = "\n", :$signal --> Nil) {
        @!command = |$command if $command;
        if nqp::istype($in, IO::Handle) && $in.DEFINITE {
            @!pre-spawn.push({ $!proc.bind-stdin($in) });
        }
        elsif $in === True {
            $!in = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-out => $nl,
                :on-write({ await $!proc.write($_) }),
                :on-close({ $!proc.close-stdin; self!await-if-last-handle }));
            $!active-handles++;
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
            my $chan = Channel.new;
            $!out = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                :on-read({ (try $chan.receive) // buf8.new }),
                :on-close({ self!await-if-last-handle }),
                :bin-supply({ $chan.Supply }));
            $!active-handles++;
            @!pre-spawn.push({ $!proc.Supply(:bin).tap: { $chan.send($_) } });
        }
        else {
            if $out === True {
                my $chan = Channel.new;
                $!out = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                    :on-read({ (try $chan.receive) // buf8.new }),
                    :on-close({ self!await-if-last-handle }),
                    :bin-supply({ $chan.Supply }));
                $!active-handles++;
                @!pre-spawn.push({
                    $!proc.stdout(:bin).tap: { $chan.send($_) },
                        done => { $chan.close },
                        quit => { $chan.fail($_) }
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
                my $chan = Channel.new;
                $!err = IO::Pipe.new(:proc(self), :$chomp, :$enc, :$bin, nl-in => $nl,
                    :on-read({ (try $chan.receive) // buf8.new }),
                    :on-close({ self!await-if-last-handle }),
                    :bin-supply({ $chan.Supply }));
                $!active-handles++;
                @!pre-spawn.push({
                    $!proc.stderr(:bin).tap: { $chan.send($_) },
                        done => { $chan.close },
                        quit => { $chan.fail($_) }
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

    method !await-if-last-handle(--> Nil) {
        self!wait-for-finish unless --$!active-handles;
    }

    method !wait-for-finish {
        CATCH { default { self.status(0x100) } }
        self.status(await($!finished).status) if $!exitcode == -1;
    }

    method spawn(*@args where .so, :$cwd = $*CWD, :$env --> Bool:D) {
        @!command = @args;
        self!spawn-internal(@args, $cwd, $env)
    }

    method shell($cmd, :$cwd = $*CWD, :$env --> Bool:D) {
        @!command = $cmd;
        my @args := Rakudo::Internals.IS-WIN
            ?? (%*ENV<ComSpec>, '/c', $cmd)
            !! ('/bin/sh', '-c', $cmd);
        self!spawn-internal(@args, $cwd, $env)
    }

    method !spawn-internal(@args, $cwd, $env --> Bool:D) {
        my %ENV := $env ?? $env.hash !! %*ENV;
        $!proc := Proc::Async.new(|@args, :$!w);
        .() for @!pre-spawn;
        $!finished = $!proc.start(:$cwd, :%ENV, scheduler => $PROCESS::SCHEDULER);
        my $is-spawned := do {
            CATCH { default { self.status(0x100) } }
            await $!proc.ready;
            True
        } // False;
        .() for @!post-spawn;
        self!wait-for-finish unless $!out || $!err || $!in;
        $is-spawned
    }

    proto method status(|) { * }
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
        $!exitcode == 0
    }
    method exitcode {
        self!wait-for-finish;
        $!exitcode
    }

    method sink(--> Nil) {
        self!wait-for-finish;
        X::Proc::Unsuccessful.new(:proc(self)).throw if $!exitcode > 0;
    }
}

sub run(*@args where .so, :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str  :$enc, Str:D :$nl = "\n", :$cwd = $*CWD, :$env) {
    my $proc = Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.spawn(@args, :$cwd, :$env);
    $proc
}

sub shell($cmd, :$in = '-', :$out = '-', :$err = '-',
        Bool :$bin, Bool :$chomp = True, Bool :$merge,
        Str  :$enc, Str:D :$nl = "\n", :$cwd = $*CWD, :$env) {
    my $proc = Proc.new(:$in, :$out, :$err, :$bin, :$chomp, :$merge, :$enc, :$nl);
    $proc.shell($cmd, :$cwd, :$env);
    $proc
}

sub QX($cmd, :$cwd = $*CWD, :$env) {
    my $proc = Proc.new(:out);
    my $status := $proc.shell($cmd, :$cwd, :$env);
    my $result;
    try {
        $result := $proc.out.slurp;
        $status := $proc.out.close;
    }
    $result.DEFINITE
      ?? $result
      !! Failure.new("Unable to read from '$cmd'")
}

# vim: ft=perl6 expandtab sw=4
