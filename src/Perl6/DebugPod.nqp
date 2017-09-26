# various helper methods for debugging Pod parsing and
# processing

# to use these subroutines, in the calling file:
#
#     use Perl6::DebugPod;
#
# to call the subroutines, in the using file:
#
#     Perl6::DebugPod::foo(...);

class Perl6::DebugPod {

    #============================================
    # subs for debugging
    #============================================
    our sub is_type($obj) {
        return 'list' if nqp::islist($obj);
        return 'hash' if nqp::ishash($obj);
        return 'int'  if nqp::isint($obj);
        return 'num'  if nqp::isnum($obj);
        return 'str'  if nqp::isstr($obj);
        my $s := $obj.ast;
	die("FATAL: Unknown type '$s'");
    }

    our sub debug_rows($desc, @rows) {
        say("=======================================================================");
        say($desc);
        say("=======================================================================");
        my $i := 0;
        while $i < +@rows {
	    # we expect table rows to be a list or a string--make sure they are
            if !nqp::islist(@rows[$i]) {
		my $typ := is_type(@rows[$i]);
		nqp::die("FATAL: not a string, it's a '$typ'") if !nqp::isstr(@rows[$i]);
      		say("row $i: '{@rows[$i]}' ");
            }
            else {
		print("row $i cells: ");
		for @rows[$i] -> $cell {
	            print(" '$cell',");
		}
		print("\n");
            }
            $i := $i + 1;
	}
        say("=======================================================================");
        say("End 'debug_rows'");
        say("=======================================================================");
    }

    our sub debug_array($desc, @arr) {
	say("=======================================================================");
	say($desc);
	say("=======================================================================");
	
	# expecting a list
	nqp::die("FATAL: incoming array is not a list") if !nqp::islist(@arr);
	
	my $i := 0;
	my $nc := +@arr;
	print("$nc cells: [");
	while $i < $nc {
            print(",") if $i;
	    my $j := @arr[$i];
	    if $j eq '0' {
		$j := '0'; # okay
	    }
	    elsif !$j {
		$j := ' ';
	    }
	    print(" '$j'");
            $i := $i + 1;
	}
	print(" ]\n");
	say("=======================================================================");
        say("End 'debug_array'");
        say("=======================================================================");
    }

    our sub debug_hdr_content($desc, $headers, $content) {
	say("=======================================================================");
	say($desc);
	say("=======================================================================");
	say("== headers");
	my $nh := nqp::elems($headers);
	print("hdr cells: ");
	my $i := 0;
	while $i < $nh {
            my $cell := $headers[$i];
            print(" '$cell',");
            $i := $i + 1;
	}
	print("\n");
	
	say("== content");
	my $nr := nqp::elems($content);
	$i := 0;
	while $i < $nr {
            print("row $i cells: ");
            my @c := $content[$i];
            for @c -> $cell {
		print(" '$cell',");
            }
            print("\n");
            $i := $i + 1;
	}
	say("=======================================================================");
        say("End 'debug_hdr_content'");
        say("=======================================================================");
    }
}
