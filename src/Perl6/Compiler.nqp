use NQPP6Regex;
use QRegex;
use Perl6::Optimizer;

class Perl6::Compiler is HLL::Compiler {
    method command_eval(*@args, *%options) {
        if pir::exists(%options, 'doc') && !%options<doc> {
            %options<doc> := 'text';
        }

        my $hll_ns := pir::get_root_global__Ps('perl6');
        my $argiter := nqp::iterator(@args);
        nqp::shift($argiter) if $argiter && !pir::defined(%options<e>);
        $hll_ns<$!ARGITER> := $argiter;
        my $super := pir::find_method__PPs(HLL::Compiler, 'command_eval');
        my %*COMPILING;
        %*COMPILING<%?OPTIONS> := %options;
        $super(self, |@args, |%options);
    }
    
    method optimize($past, *%adverbs) {
        %adverbs<optimize> ??
            Perl6::Optimizer.new.optimize($past, |%adverbs) !!
            $past
    }
}
