use NQPP6Regex;
use QRegex;

class Perl6::Compiler is HLL::Compiler {
    method command_eval(*@args, *%options) {
        my $hll_ns := pir::get_root_global__Ps('perl6');
        $hll_ns<@!ARGS> := @args;
        my $super := pir::find_method__PPs(HLL::Compiler, 'command_eval');
        $super(self, |@args, |%options);
    }
}


