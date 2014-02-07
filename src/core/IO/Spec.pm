my class IO::Spec {
    my %module = (
        'MSWin32' => 'Win32',
        'os2' =>     'Win32',
        'dos'     => 'Win32',
        'symbian' => 'Win32',
        'NetWare' => 'Win32',
        'Win32'   => 'Win32',
        'cygwin'  => 'Cygwin',
        'Cygwin'  => 'Cygwin',
        'qnx'     => 'QNX',
        'QNX'     => 'QNX',
        'nto'     => 'QNX',
        # <MacOS Mac>  »=>» 'Mac',
        # 'VMS'     => 'VMS'
);

    #  this is really just a way of getting $*OS when it's not in scope yet
    my $submodule;
#?if parrot
    $submodule = %module{ nqp::atkey(nqp::atpos(pir::getinterp__P, pir::const::IGLOBALS_CONFIG_HASH), 'osname') };
#?endif
#?if jvm
    $submodule = %module{ nqp::p6box_s(nqp::atkey(nqp::jvmgetproperties(), 'os.name')) };
#?endif
#?if moar
    $submodule = %module{ nqp::p6box_s(nqp::atkey(nqp::backendconfig(), 'osname')) };
#?endif
    my $SPEC := IO::Spec.WHO{ $submodule // 'Unix' };

    method FSTYPE ($OS = $*OS)   { %module{$OS} // 'Unix' }

    #| Dispatches methods to the appropriate class for the current $*OS
    #| Well, it should, if handles worked here.  Still useful, though.
    method MODULE 
       # handles
       # <canonpath curdir updir rootdir devnull tmpdir
       #  is-absolute no-parent-or-current-test
       #  path split join splitpath catpath catfile
       #  splitdir catdir abs2rel rel2abs>
            { $SPEC }

    #| Returns a copy of the module for the given OS string
    #| e.g. IO::Spec.os('Win32') returns IO::Spec::Win32
    method os (Str $OS = $*OS) {
        IO::Spec.WHO{%module{$OS} // 'Unix'};
    }

    method canonpath( |c )             { $SPEC.canonpath( |c )             }
    method curdir                      { $SPEC.curdir()                    }
    method updir                       { $SPEC.updir()                     }
    method rootdir                     { $SPEC.rootdir()                   }
    method devnull                     { $SPEC.devnull()                   }
    method tmpdir                      { $SPEC.tmpdir()                    }
    method is-absolute( |c )           { $SPEC.is-absolute( |c )           }
    method no-parent-or-current-test   { $SPEC.no-parent-or-current-test   }
    method path                        { $SPEC.path()                      }
    method split ( |c )                { $SPEC.split( |c )                 }
    method join ( |c )                 { $SPEC.join( |c )                  }
    method splitpath( |c )             { $SPEC.splitpath( |c )             }
    method catpath( |c )               { $SPEC.catpath( |c )               }
    method catfile( |c )               { $SPEC.catfile( |c )               }
    method splitdir( |c )              { $SPEC.splitdir( |c )              }
    method catdir( |c )                { $SPEC.catdir( |c )                }
    method abs2rel( |c )               { $SPEC.abs2rel( |c )               }
    method rel2abs( |c )               { $SPEC.rel2abs( |c )               }
}

nqp::gethllsym('perl6', 'ModuleLoader').register_absolute_path_func(
    sub ($path) { return IO::Spec.rel2abs($path); }
);
