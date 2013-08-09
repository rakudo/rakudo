class Perl6::JavaModuleLoader {
    my $interop;
    my $interop_loader;
    
    method set_interop_loader($loader) {
        $interop_loader := $loader;
    }
    
    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file?) {
        # Load interop support if needed.
        $interop := $interop_loader() unless nqp::isconcrete($interop);
        
        # Try to get hold of the type.
        my @parts := nqp::split('::', $module_name);
        my $jname := nqp::join('.', @parts);
        my $type  := nqp::existskey(%opts, 'jar')
                        ?? $interop.typeForNameFromJAR($jname, nqp::decont(%opts<jar>))
                        !! $interop.typeForName($jname);
        if $type =:= NQPMu {
            nqp::die("Could not locate Java module $jname");
        }
        
        # Return unit-like thing with an EXPORT::DEFAULT.
        nqp::hash('EXPORT', make_package('EXPORT',
            nqp::hash('DEFAULT', make_package('DEFAULT',
                nqp::hash(@parts[nqp::elems(@parts) - 1], $type)))))
    }
    
    sub make_package($name, %who) {
        my $pkg := nqp::knowhow().new_type(:$name);
        $pkg.HOW.compose($pkg);
        nqp::setwho($pkg, %who);
        $pkg
    }
}

Perl6::ModuleLoader.register_language_module_loader('java', Perl6::JavaModuleLoader);
nqp::bindhllsym('perl6', 'JavaModuleLoader', Perl6::JavaModuleLoader);
