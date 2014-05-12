class Kernel {
    has $!name;
    has $!ver;
    has $!release;
    has $!hardware;
    has $!arch;

    method gist { $.name ~ (" ($!ver)" if $.ver ne "unknown") }
    method Str  { $.name }

    method name {
        $!name //= do {
            given $*DISTRO.name.lc {
                when any <linux darwin> { # needs adapting
                    qx/uname -s/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }

    method ver {
        $!ver //= do {
            given $*DISTRO.name.lc {
                when any <linux darwin> { # needs adapting
                    qx/uname -v/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }

    method release {
        $!release //= do {
            given $*DISTRO.name.lc {
                when any <linux darwin> { # needs adapting
                    qx/uname -r/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }

    method hardware {
        $!hardware //= do {
            given $*DISTRO.name.lc {
                when any <linux darwin> { # needs adapting
                    qx/uname -m/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }

    method arch {
        $!arch //= do {
            given $*DISTRO.name.lc {
                when any <linux darwin> { # needs adapting
                    qx/uname -p/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }
}

PROCESS::<$KERNEL> = Kernel.new;
