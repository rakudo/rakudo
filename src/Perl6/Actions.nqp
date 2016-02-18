use NQPP6QRegex;
use NQPP5QRegex;
use Perl6::Pod;
use Perl6::Ops;
use QRegex;
use QAST;

my $wantwant := Mu;

sub block_closure($code) {
    my $closure := QAST::Op.new(
        :op('callmethod'), :name('clone'),
        $code
    );
    $closure := QAST::Op.new( :op('p6capturelex'), $closure);
    $closure.annotate('past_block', $code.ann('past_block'));
    $closure.annotate('code_object', $code.ann('code_object'));
    $closure
}

sub wantall($ast, $by) {
    my int $i := 0;
    my int $e := $ast ?? +@($ast) !! 0;
    while $i < $e { $ast[$i] := wanted($ast[$i], $by ~ ' wa'); $i := $i + 1 }
    Nil;
}

sub WANTALL($ast, $by) {
    my int $i := 0;
    my int $e := $ast ?? +@($ast) !! 0;
    while $i < $e { $ast[$i] := WANTED($ast[$i], $by ~ ' WA'); $i := $i + 1 }
    Nil;
}

sub unwantall($ast, $by) {
    my int $i := 0;
    my int $e := $ast ?? +@($ast) !! 0;
    while $i < $e { $ast[$i] := unwanted($ast[$i], $by ~ ' ua'); $i := $i + 1 }
    Nil;
}

sub UNWANTALL($ast, $by) {
    my int $i := 0;
    my int $e := $ast ?? +@($ast) !! 0;
    while $i < $e { $ast[$i] := UNWANTED($ast[$i], $by ~ ' ua'); $i := $i + 1 }
    Nil;
}

# Note that these wanted/unwanted routines can return a different ast
# from the one passed, so always store the result back from where
# got it.  (Like how wantall does it above.)

sub wanted($ast,$by) {
#    $wantwant := nqp::getenvhash<RAKUDO_WANT> unless nqp::isconcrete($wantwant);
    my $byby := $wantwant ?? $by ~ ' u' !! $by;
    return $ast unless nqp::can($ast,'ann');
    my $addr := nqp::where($ast);
    return $ast if $ast.ann('WANTED');  # already marked from here down
    return $ast if $ast.ann('context'); # already marked from here down
    note('wanted ' ~ $addr ~ ' by ' ~ $by ~ "\n" ~ $ast.dump) if $wantwant;
#    if $ast.ann('context') {
#        note("Oops, already sunk node is now wanted!?! \n" ~ $ast.dump);
#        $ast.annotate('context','');
#    }
    my $e := +@($ast) - 1;
    $ast.annotate('BY',$byby);

    if nqp::istype($ast,QAST::Stmt) || nqp::istype($ast,QAST::Stmts) {
        my $resultchild := $ast.resultchild // $e;
        my int $i := 0;
        while $i <= $e {
            $ast[$i] := $i == $resultchild ?? wanted($ast[$i], $byby) !! unwanted($ast[$i], $byby);
            ++$i;
        }
        $ast.annotate('WANTED',1);
    }
    elsif nqp::istype($ast,QAST::Block) {
        my int $i := 1;
        my $*WANTEDOUTERBLOCK := $ast;
        while $i <= $e {
            $ast[$i] := WANTED($ast[$i], $byby);
            ++$i;
        }
        $ast.annotate('WANTED',1);
    }
    elsif nqp::istype($ast,QAST::Op) {
        if $ast.op eq 'call' && (!$ast.name || $ast.name eq '&infix:<,>') {
            WANTALL($ast,$byby);
        }
        elsif $ast.op eq 'p6capturelex' {
            $ast.annotate('past_block', wanted($ast.ann('past_block'), $byby));
            $ast.annotate('WANTED',1);
        }
        elsif $ast.op eq 'call' ||
              $ast.op eq 'callstatic' ||
              $ast.op eq 'callmethod' ||
              $ast.op eq 'handle' ||
              $ast.op eq 'locallifetime' ||
              $ast.op eq 'p6typecheckrv' ||
              $ast.op eq 'lexotic' {
            $ast[0] := WANTED($ast[0], $byby) if +@($ast);
            $ast.annotate('WANTED',1);
        }
        elsif $ast.op eq 'p6decontrv' {
            $ast[1] := WANTED($ast[1], $byby) if +@($ast);
            $ast.annotate('WANTED',1);
        }
        elsif $ast.op eq 'while' ||
              $ast.op eq 'until' ||
              $ast.op eq 'repeat_while' ||
              $ast.op eq 'repeat_until' {

            my $repeat := nqp::eqat($ast.op,'repeat',0);
            my $while := nqp::index($ast.op,'while',0) >= 0;

            # we always have a body
            my $cond := WANTED($ast[0],$byby);
            my $body := WANTED($ast[1],$byby);
            my $block := Perl6::Actions::make_thunk_ref($body, $body.node);
            my $past := QAST::Op.new(:op<callmethod>, :name<from-loop>, :node($body.node),
                QAST::WVal.new( :value($*W.find_symbol(['Seq']))),
                block_closure($block) );

            # conditional (if not always true (or if repeat))
            if $repeat || !$cond.has_compile_time_value || !$cond.compile_time_value == $while {
                $cond := QAST::Op.new( :op<callmethod>, :name<not>, $cond ) unless $while;
                $block := Perl6::Actions::make_thunk_ref($cond, nqp::can($cond,'node') ?? $cond.node !! $body.node);
                $past.push( block_closure($block) );
            }

            # 3rd part of loop, if any
            if +@($ast) > 2 {
                $block := Perl6::Actions::make_thunk_ref($ast[2], $ast[2].node);
                $block.annotate('outer',$*WANTEDOUTERBLOCK) if $*WANTEDOUTERBLOCK;
                $past.push( UNWANTED(block_closure($block),$byby) )
            }

            if $repeat {
                my $wval := QAST::WVal.new( :value($*W.find_symbol(['True'])) );
                $wval.named('repeat');
                $past.push($wval);
            }

            $ast := $past;
            $ast.annotate('WANTED',1);
        }
        elsif $ast.op eq 'if' ||
              $ast.op eq 'unless' ||
              $ast.op eq 'with' ||
              $ast.op eq 'without' {
            $ast[1] := WANTED($ast[1], $byby);
            $ast[2] := WANTED($ast[2], $byby) if +@($ast) > 2 && nqp::istype($ast[2],QAST::Node);
            $ast.annotate('WANTED',1);
        }
    }
    elsif nqp::istype($ast,QAST::Want) {
        $ast.annotate('WANTED',1);
        my $node := $ast[0];
        if nqp::istype($node,QAST::Op) {
            if $node.op eq 'call' && !$node.name {
                $node := $node[0];
                if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                    $node.annotate('past_block', WANTED($node.ann('past_block'), $byby));
                }
            }
            elsif $node.op eq 'callstatic' {
                $node[0] := WANTED($node[0], $byby);
            }
            elsif $node.op eq 'p6for' {
                $node := $node[1];
                if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                    $node.annotate('past_block', WANTED($node.ann('past_block'), $byby));
                }
            }
            elsif $node.op eq 'while' ||
                  $node.op eq 'until' ||
                  $node.op eq 'repeat_while' ||
                  $node.op eq 'repeat_until' {
                return WANTED($node,$byby) if !$*COMPILING_CORE_SETTING;
                $node[1] := WANTED($node[1], $byby);
                $node.annotate('WANTED',1);
            }
            elsif $node.op eq 'if' ||
                  $node.op eq 'unless' ||
                  $node.op eq 'with' ||
                  $node.op eq 'without' {
                $node[1] := WANTED($node[1], $byby);
                $node[2] := WANTED($node[2], $byby) if +@($node) > 2 && nqp::istype($node[2],QAST::Node);
                $node.annotate('WANTED',1);
            }
        }
    }
    $ast;
}

sub WANTED($ast, $by) {
    if nqp::istype($ast, QAST::Node) {
        $ast := wanted($ast, $by ~ ' W');
        $ast.annotate('WANTED',1);  # force in case it's just a thunk
    }
    else {
        note("Non ast passed to WANTED: " ~ $ast.HOW.name($ast));
    }
    $ast;
}

sub unwanted($ast, $by) {
    my $byby := $by ~ ' u';
    return $ast unless nqp::can($ast,'ann');
    my $addr := nqp::where($ast);
    return $ast if $ast.ann('context');
    return $ast if $ast.ann('WANTED');  # probably a loose thunk just stashed somewhere random
    $ast.annotate('BY',$byby);
    my $e := +@($ast) - 1;
    note('unwanted ' ~ $addr ~ ' by ' ~ $by ~ "\n" ~ $ast.dump) if $wantwant;
    if nqp::istype($ast,QAST::Stmt) || nqp::istype($ast,QAST::Stmts) {
        # Unwant all kids, not just last one, so we recurse into blocks and such,
        # don't just rely on the optimizer to default to void.
        my int $i := 0;
        while $i <= $e {
            $ast[$i] := unwanted($ast[$i], $byby);
            ++$i;
        }
        $ast.annotate('context','sink');
        $ast.push(QAST::WVal.new( :value($*W.find_symbol(['True'])) ))
            if $e >= 0 && nqp::istype($ast[$e],QAST::Op) && $ast[$e].op eq 'bind';
    }
    elsif nqp::istype($ast,QAST::Block) {
        my int $i := 1;
        my $*WANTEDOUTERBLOCK := $ast;
        while $i <= $e {
            $ast[$i] := UNWANTED($ast[$i], $byby);
            ++$i;
        }
        $ast.annotate('context','sink');
    }
    elsif nqp::istype($ast,QAST::Op) {
        if $ast.op eq 'call' && $ast.name eq '&infix:<,>' {
            UNWANTALL($ast,$byby);
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'p6capturelex' {
            $ast.annotate('past_block', unwanted($ast.ann('past_block'), $byby));
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'call' ||
              $ast.op eq 'callstatic' ||
              $ast.op eq 'callmethod' ||
              $ast.op eq 'handle' ||
              $ast.op eq 'locallifetime' ||
              $ast.op eq 'p6typecheckrv' ||
              $ast.op eq 'lexotic' ||
              $ast.op eq 'ifnull' {
            $ast[0] := UNWANTED($ast[0], $byby) if +@($ast);
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'p6decontrv' {
            $ast[1] := UNWANTED($ast[1], $byby) if +@($ast);
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'while' ||
              $ast.op eq 'until' ||
              $ast.op eq 'repeat_while' ||
              $ast.op eq 'repeat_until' {
            # Do we need to force loop to produce return values for internal reasons?
            if !$*COMPILING_CORE_SETTING && $ast[1].ann('WANTMEPLEASE') {
                $ast := QAST::Op.new(:op<callmethod>, :name<sink>, WANTED($ast, $byby));
                $ast.annotate('context','sink');
                return $ast;
            }
            $ast[1] := UNWANTED($ast[1], $byby);
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'if' ||
              $ast.op eq 'unless' ||
              $ast.op eq 'with' ||
              $ast.op eq 'without' {
            $ast[1] := UNWANTED($ast[1], $byby);
            $ast[2] := UNWANTED($ast[2], $byby) if +@($ast) > 2 && nqp::istype($ast[2],QAST::Node);
            $ast.annotate('context','sink');
        }
        elsif $ast.op eq 'bind' {
            $ast.annotate('context','sink');
        }
    }
    elsif nqp::istype($ast,QAST::Want) {
        $ast.annotate('context','sink');
        my $node := $ast[0];
        if nqp::istype($node,QAST::Op) && $node.op eq 'call' && !$node.name {
            $node := $node[0];
            if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                $node.annotate('past_block', UNWANTED($node.ann('past_block'), $byby));
            }
        }
        elsif nqp::istype($node,QAST::Op) && $node.op eq 'call' {
            $node.annotate('context','sink');
            unwantall($node, $byby) if $node.name eq '&infix:<,>';
        }
        elsif nqp::istype($node,QAST::Op) && $node.op eq 'p6for' {
            $node := $node[1];
            if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                $node.annotate('past_block', UNWANTED($node.ann('past_block'), $byby));
            }
        }
        elsif nqp::istype($node,QAST::Op) && ($node.op eq 'while' || $node.op eq 'until') {
            if !$*COMPILING_CORE_SETTING && $node[1].ann('WANTMEPLEASE') {
                $ast := QAST::Op.new(:op<callmethod>, :name<sink>, WANTED($node, $byby));
                $ast.annotate('context','sink');
                return $ast;
            }
            $node[1] := UNWANTED($node[1], $byby);
            $node.annotate('context','sink');
        }
        elsif nqp::istype($node,QAST::Op) && ($node.op eq 'if' || $node.op eq 'unless' || $node.op eq 'with' || $node.op eq 'without') {
            for 1,2 {
                if +@($node) > $_ && nqp::istype($node[$_],QAST::Node) {
                    if nqp::istype($node[$_],QAST::Op) && $node[$_].op eq 'bind' {
                        $node[$_] := QAST::Stmts.new(
                                        $node[$_],
                                        QAST::WVal.new( :value($*W.find_symbol(['True']))));
                    }
                    $node[$_] := UNWANTED($node[$_], $byby);
                }
            }
            $node.annotate('context','sink');
        }
        elsif nqp::istype($node,QAST::Op) && $node.op eq 'callmethod' && $node.name eq 'new' {
            $node.annotate('context','sink');
        }
        elsif nqp::istype($node,QAST::WVal) {
            $node.annotate('context','sink');
            $ast[2].annotate('context','sink');
        }

    }
    $ast;
}

sub UNWANTED($ast, $by) {
    if nqp::istype($ast, QAST::Node) {
        $ast := unwanted($ast, $by ~ ' U');
        $ast.annotate('context','sink');  # force in case it's just a thunk
    }
    else {
        note("Non ast passed to UNWANTED: " ~ $ast.HOW.name($ast));
    }
    $ast;
}

register_op_desugar('p6callmethodhow', -> $qast {
    $qast   := $qast.shallow_clone();
    my $inv := $qast.shift;
    my $tmp := QAST::Node.unique('how_invocant');
    $qast.op('callmethod');
    $qast.unshift(QAST::Var.new( :name($tmp), :scope('local') ));
    $qast.unshift(QAST::Op.new(
        :op('how'),
        QAST::Var.new( :name($tmp), :scope('local') )
    ));
    QAST::Stmts.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
            $inv
        ),
        QAST::Op.new( :op('hllize'), $qast )
    )
});
register_op_desugar('p6fatalize', -> $qast {
    my $tmp := QAST::Node.unique('fatalizee');
    QAST::Stmts.new(
        :resultchild(0),
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
            $qast[0]
        ),
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('istype'),
                QAST::Var.new( :name($tmp), :scope('local') ),
                $qast[1],
            ),
            QAST::Op.new(
                :op('callmethod'), :name('sink'),
                QAST::Var.new( :name($tmp), :scope('local') )
            )
        ))
});
register_op_desugar('p6for', -> $qast {
    my $cond := $qast[0];
    my $block := $qast[1];
    my $label := $qast[2];
    my $for-list-name := QAST::Node.unique('for-list');
    my $iscont := QAST::Op.new(:op('iscont'), QAST::Var.new( :name($for-list-name), :scope('local') ));
    $iscont.named('item');
    my $call := QAST::Op.new(
        :op<callmethod>, :name<map>, :node($qast),
        QAST::Var.new( :name($for-list-name), :scope('local') ),
        $block,
        $iscont,
    );
    if $label {
        $call.push($label);
    }
    my $bind := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($for-list-name), :scope('local'), :decl('var') ),
        $cond,
    );
    QAST::Stmts.new(
        $bind,
        QAST::Op.new( :op<callmethod>, :name($qast.ann('context')), $call )
    );
});

sub monkey_see_no_eval() {
    nqp::existskey(%*PRAGMAS,'MONKEY-SEE-NO-EVAL')
        ?? %*PRAGMAS<MONKEY-SEE-NO-EVAL>   # prevails if defined, can be either 1 or 0
        !! $*COMPILING_CORE_SETTING ||
           try { $*W.find_symbol(['&MONKEY-SEE-NO-EVAL'])() } ||
           nqp::getenvhash<RAKUDO_MONKEY_BUSINESS>;
}

role STDActions {
    method quibble($/) {
        make $<nibble>.ast;
    }

    method trim_heredoc($doc, $stop, $origast) {
        $origast.pop();
        $origast.pop();

        my str $ws := $stop.MATCH<ws>.Str;
        my int $actualchars := nqp::chars($ws);
        my int $indent := -$actualchars;

        my int $tabstop := $*W.find_symbol(['$?TABSTOP']);
        my int $checkidx := 0;
        while $checkidx < $actualchars {
            if nqp::eqat($ws, "\t", $checkidx) {
                $indent := $indent - ($tabstop - 1);
            }
            $checkidx := $checkidx + 1;
        }

        my $docast := $doc.MATCH.ast;
        if $docast.has_compile_time_value {
            my $dedented := nqp::unbox_s($docast.compile_time_value.indent($indent));
            $origast.push($*W.add_string_constant($dedented));
        }
        else {
            # we need to remove spaces from the beginnings of only textual lines,
            # so we have to track after each concatenation if the spaces at the
            # beginning of our chunk belong to a fresh line or come after an
            # interpolation or something
            my $in-fresh-line := 1;

            sub descend($node) {
                if nqp::istype($node, QAST::Want) {
                    if +@($node) == 3 && $node[1] eq "Ss" {
                        my $strval := $node[0].compile_time_value;
                        if !$in-fresh-line {
                            if $strval ~~ /\n/ {
                                my $strbox := nqp::box_s(nqp::x(" ", -$indent) ~ nqp::unbox_s($strval), $*W.find_symbol(["Str"]));
                                $strval := nqp::unbox_s($strbox.indent($indent));
                                $in-fresh-line := 1;
                                return $*W.add_string_constant($strval);
                            }
                        } else {
                            $strval := nqp::unbox_s($strval.indent($indent));
                            return $*W.add_string_constant($strval);
                        }
                    }
                } elsif nqp::istype($node, QAST::Op) && $node.op eq 'call' && $node.name eq '&infix:<~>' {
                    my @results;
                    # since we have the $in-fresh-line state, we need to traverse
                    # and replace the child nodes in order
                    for @($node) {
                        nqp::push(@results, descend($node.shift))
                    }
                    for @results {
                        nqp::push($node, $_)
                    }
                    return $node;
                }
                $in-fresh-line := 0;
                return $node
            }

            $origast.push(descend($docast))
        }
        $origast;
    }
}

class Perl6::Actions is HLL::Actions does STDActions {
    our @MAX_PERL_VERSION;

    # Could add to this based on signatures.
    our %commatrap := nqp::hash(
        '&categorize', 1,
        '&classify', 1,
        '&first', 2,
        '&grep', 2,
        '&map', 1,
        '&reduce', 1,
        '&sort', 1,
    );

    INIT {
        # If, e.g., we support Perl up to v6.1.2, set
        # @MAX_PERL_VERSION to [6, 1, 2].
        @MAX_PERL_VERSION[0] := 6;
    }

    sub sink($past) {
        QAST::Want.new(
            $past,
            'v', QAST::Op.new( :op('p6sink'), $past )
        )
    }
    my %sinkable := nqp::hash(
            'call',         1,
            'callmethod',   1,
            'if',           1,
            'while',        1,
            'unless',       1,
            'until',        1,
            'repeat_until', 1,
            'repeat_while', 1,
            'handle',       1,
            'hllize',       1,
    );
    sub autosink($past) {
        nqp::istype($past, QAST::Op) && %sinkable{$past.op} && $*statement_level && !$past.ann('nosink')
            ?? sink($past)
            !! $past;
    }

    method ints_to_string($ints) {
        if nqp::islist($ints) {
            my $result := '';
            for $ints {
                $result := $result ~ nqp::chr(nqp::unbox_i($_.ast));
            }
            $result;
        } else {
            nqp::chr(nqp::unbox_i($ints.ast));
        }
    }

    sub string_to_bigint($src, $base) {
        my $res := nqp::radix_I($base, ~$src, 0, 2, $*W.find_symbol(['Int']));
        $src.CURSOR.panic("'$src' is not a valid number")
            unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), nqp::chars($src));
        nqp::atpos($res, 0);
    }

    sub xblock_immediate_with($xblock) {
        $xblock[1] := pblock_immediate_with($xblock[1]);
        $xblock;
    }

    sub xblock_immediate($xblock) {
        $xblock[1] := pblock_immediate($xblock[1]);
        $xblock;
    }

    sub pblock_immediate_with($pblock) {
        my $pb := block_immediate($pblock.ann('uninstall_if_immediately_used').shift);
        $pb.arity(1);  # gotta force this, or Block node gets optimized away
        $pb;
    }

    sub pblock_immediate($pblock) {
        block_immediate($pblock.ann('uninstall_if_immediately_used').shift);
    }

    our sub block_immediate($block) {
        $block.blocktype('immediate');
        $block;
    }

    method deflongname($/) {
        if $<colonpair> {
            my $name := ~$<name>;
            for $<colonpair> {
                my $key := $_<identifier> || '';
                if $_<coloncircumfix> -> $cf {
                    if $cf<circumfix> -> $op_name {
                        $name := $name ~ $*W.canonicalize_pair($key, $*W.colonpair_nibble_to_str(
                            $/, $op_name<nibble> // $op_name<semilist> // $op_name<pblock>));
                    }
                    else {
                        $name := $name ~ ':' ~ $key;
                    }
                }
                else {
                    $name := $name ~ ':' ~ $key;
                }
            }
            make $name;
        }
        else {
            make $*W.dissect_deflongname($/).name(
                :dba("$*IN_DECL declaration"),
                :decl<routine>,
            );
        }
    }

    method deftermnow($/) {
        # 'my \foo' style declaration
        if $*SCOPE ne 'my' {
            $*W.throw($/, 'X::Comp::NYI',
                feature => "$*SCOPE scoped term definitions (only 'my' is supported at the moment)");
        }
        my $name       :=  $<defterm>.ast;
        my $cur_lexpad := $*W.cur_lexpad;
        if $cur_lexpad.symbol($name) {
            $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
        }
        if $*OFTYPE {
            my $type := $*OFTYPE.ast;
            $cur_lexpad[0].push(QAST::Var.new( :$name, :scope('lexical'),
                :decl('var'), :returns($type) ));
            $cur_lexpad.symbol($name, :$type, :scope<lexical>);
        }
        else {
            $cur_lexpad[0].push(QAST::Var.new(:$name, :scope('lexical'), :decl('var')));
            $cur_lexpad.symbol($name, :scope('lexical'));
        }
        make $<defterm>.ast;
    }

    method defterm($/) {
        my $name := ~$<identifier>;
        if $<colonpair> {
            for $<colonpair> {
                my $key := $_<identifier> || '';
                if $_<coloncircumfix> -> $cf {
                    if $cf<circumfix> -> $op_name {
                        $name := $name ~ $*W.canonicalize_pair($key, $*W.colonpair_nibble_to_str($/, $op_name<nibble>));
                    }
                    else {
                        $name := $name ~ ':' ~ $key;
                    }
                }
                else {
                    $name := $name ~ ':' ~ $key;
                }
            }
        }
        make $name;
    }

    # Turn $code into "for lines() { $code }"
    sub wrap_option_n_code($/, $code) {
        $code := make_topic_block_ref($/, $code, copy => 1);
        my $past := QAST::Op.new(:op<callmethod>, :name<map>,
            QAST::Op.new(:op<call>, :name<&lines>),
            QAST::Op.new(:op<p6capturelex>, $code)
        );
        $past := QAST::Want.new(
            QAST::Op.new( :op<callmethod>, :name<sink>, $past ),
            'v', QAST::Op.new( :op<callmethod>, :name<sink>, $past )
        );
    }

    # Turn $code into "for lines() { $code; say $_ }"
    # &wrap_option_n_code already does the C<for> loop, so we just add the
    # C<say> call here
    sub wrap_option_p_code($/, $code) {
        wrap_option_n_code($/,
            QAST::Stmts.new(
                $code,
                QAST::Op.new(:name<&say>, :op<call>,
                    QAST::Var.new(:name<$_>, :scope<lexical>)
                )
            )
        )
    }

    method comp_unit($/) {
        # Finish up code object for the mainline.
        if $*DECLARAND {
            $*W.attach_signature($*DECLARAND, $*W.create_signature(
                nqp::hash('parameter_objects', [])));
            $*W.finish_code_object($*DECLARAND, $*UNIT);
            $*W.add_phasers_handling_code($*DECLARAND, $*UNIT);
        }

        # Checks.
        $*W.assert_stubs_defined($/);
        $*W.sort_protos();

        # Get the block for the unit mainline code.
        my $unit := $*UNIT;
        my $mainline := QAST::Stmts.new(
            $*POD_PAST,
            statementlist_with_handlers($/)
        );
        unless $*NEED_RESULT {
            # Evaluate last statement in sink context, by pushing another
            # statement after it, unless we need the result.
            unwantall($mainline, 'comp_unit');
            $mainline.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        }
        fatalize($mainline) if %*PRAGMAS<fatal>;

        if %*COMPILING<%?OPTIONS><p> { # also covers the -np case, like Perl 5
            $mainline[1] := QAST::Stmt.new(wrap_option_p_code($/, $mainline[1]));
        }
        elsif %*COMPILING<%?OPTIONS><n> {
            $mainline[1] := QAST::Stmt.new(wrap_option_n_code($/, $mainline[1]));
        }

        # We'll install our view of GLOBAL as the main one; any other
        # compilation unit that is using this one will then replace it
        # with its view later (or be in a position to restore it).
        my $global_install := QAST::Op.new(
            :op('bindcurhllsym'),
            QAST::SVal.new( :value('GLOBAL') ),
            QAST::WVal.new( :value($*GLOBALish) )
        );
        $*W.add_fixup_task(:deserialize_ast($global_install), :fixup_ast($global_install));

        # Get the block for the entire compilation unit.
        my $outer := $*UNIT_OUTER;
        $outer.node($/);
        $*UNIT_OUTER.unshift(QAST::Var.new( :name('__args__'), :scope('local'), :decl('param'), :slurpy(1) ));
        $unit.name('<unit>');
        $outer.name('<unit-outer>');

        # Load the needed libraries.
        $*W.add_libs($unit);

        # If the unit defines &MAIN, and this is in the mainline,
        # add a &MAIN_HELPER.
        if !$*W.is_precompilation_mode && +(@*MODULES // []) == 0 && $unit.symbol('&MAIN') {
            $mainline := QAST::Op.new(
                :op('call'),
                :name('&MAIN_HELPER'),
                $mainline,
            );
        }

        # If our caller wants to know the mainline ctx, provide it here.
        # (CTXSAVE is inherited from HLL::Actions.) Don't do this when
        # there was an explicit {YOU_ARE_HERE}.
        unless $*HAS_YOU_ARE_HERE {
            $unit.push( self.CTXSAVE() );
        }

        # Add the mainline code to the unit.
        $unit.push($mainline);

        # Executing the compilation unit causes the mainline to be executed.
        $outer.push(QAST::Op.new( :op<call>, $unit ));

        # Do not want closure semantics on this outermost scope.
        $unit.blocktype('declaration_static');

        # Wrap everything in a QAST::CompUnit.
        my $compunit := QAST::CompUnit.new(
            :hll('perl6'),

            # Serialization related bits.
            :sc($*W.sc()),
            :code_ref_blocks($*W.code_ref_blocks()),
            :compilation_mode($*W.is_precompilation_mode()),
            :pre_deserialize($*W.load_dependency_tasks()),
            :post_deserialize($*W.fixup_tasks()),
            :repo_conflict_resolver(QAST::Op.new(
                :op('callmethod'), :name('resolve_repossession_conflicts'),
                QAST::WVal.new( :value($*W.find_symbol(['CompUnit', 'RepositoryRegistry'])) )
            )),

            # If this unit is loaded as a module, we want it to automatically
            # execute the mainline code above after all other initializations
            # have occurred.
            :load(QAST::Op.new(
                :op('call'),
                QAST::BVal.new( :value($outer) ),
            )),

            # Finally, the outer block, which in turn contains all of the
            # other program elements.
            $outer
        );

        # Pass some extra bits along to the optimizer.
        $compunit.annotate('UNIT', $unit);
        $compunit.annotate('GLOBALish', $*GLOBALish);
        $compunit.annotate('W', $*W);
        make $compunit;
    }

    method install_doc_phaser($/) {
        # Add a default DOC INIT phaser
        my $doc := %*COMPILING<%?OPTIONS><doc>;
        if $doc {
            my $block := $*W.push_lexpad($/);

            my $renderer := "Pod::To::$doc";

            my $module := $*W.load_module($/, $renderer, {}, $*GLOBALish);

            my $pod2text := QAST::Op.new(
                :op<callmethod>, :name<render>, :node($/),
                self.make_indirect_lookup([$renderer]),
                QAST::Var.new(:name<$=pod>, :scope('lexical'), :node($/))
            );

            $block.push(
                QAST::Op.new(
                    :op('if'),
                    $pod2text,
                    QAST::Op.new(
                        :op<call>, :node($/),
                        :name('&say'), $pod2text,
                    ),
                )
            );

            # TODO: We should print out $?USAGE too,
            # once it's known at compile time

            $block.push(
                QAST::Op.new(
                    :op<call>, :node($/),
                    :name('&exit'),
                )
            );

            $*W.pop_lexpad();
            $*W.add_phaser(
                $/, 'INIT', $*W.create_simple_code_object($block, 'Block'), $block
            );
        }
    }

    method pod_content_toplevel($/) {
        my $child := $<pod_block>.ast;
        # make sure we don't push the same thing twice
        if $child {
            my $id := $/.from ~ "," ~ ~$/.to;
            if !$*POD_BLOCKS_SEEN{$id} {
                $*POD_BLOCKS.push($child);
                $*POD_BLOCKS_SEEN{$id} := 1;
            }
        }
        make $child;
    }

    method pod_content:sym<block>($/) {
        make $<pod_block>.ast;
    }

    method pod_configuration($/) {
        make Perl6::Pod::make_config($/);
    }

    method pod_block:sym<delimited>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<delimited_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<delimited_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<delimited_code>($/) {
        my $config  := $<pod_configuration>.ast;
        my @contents := $<delimited_code_content>.ast;
        my $twine   := Perl6::Pod::serialize_array(@contents).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :contents($twine),
            :config($config),
        ).compile_time_value
    }

    method delimited_code_content($/) {
        my @t := [];
        for $/[0] {
            if $_<pod_string> {
                nqp::splice(@t, Perl6::Pod::merge_twines($_<pod_string>), +@t, 0);
                nqp::push(@t, $*W.add_constant(
                    'Str', 'str', ~$_<pod_newline>
                ).compile_time_value);
            } else {
                @t.push($*W.add_constant('Str', 'str', "\n").compile_time_value);
            }
        }
        make @t;
    }

    method pod_block:sym<paragraph>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<paragraph_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<paragraph_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<paragraph_code>($/) {
        my $config := $<pod_configuration>.ast;
        my @t := [];
        for $<pod_line> {
            nqp::splice(@t, $_.ast, +@t, 0);
        }
        my $twine  := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :contents($twine),
            :config($config),
        ).compile_time_value
    }

    method pod_block:sym<abbreviated>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<abbreviated_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<abbreviated_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<abbreviated_code>($/) {
        my @t := [];
        for $<pod_line> {
            nqp::splice(@t, $_.ast, +@t, 0);
        }
        my $twine := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :contents($twine)
        ).compile_time_value
    }

    method pod_line ($/) {
        my @t := Perl6::Pod::merge_twines($<pod_string>);
        @t.push($*W.add_constant(
            'Str', 'str', ~$<pod_newline>
        ).compile_time_value);
        make @t;
    }

    method pod_block:sym<finish>($/) {
        $*W.install_lexical_symbol(
          $*UNIT,'$=finish', nqp::hllizefor(~$<finish>, 'perl6'));
    }

    method pod_content:sym<config>($/) {
        make Perl6::Pod::config($/);
    }

    method pod_content:sym<text>($/) {
        my @ret := [];
        for $<pod_textcontent> {
            @ret.push($_.ast);
        }
        my $past := Perl6::Pod::serialize_array(@ret);
        make $past.compile_time_value;
    }

    method pod_textcontent:sym<regular>($/) {
        my @t     := Perl6::Pod::merge_twines($<pod_string>);
        my $twine := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Para', :contents($twine)
        ).compile_time_value
    }

    method pod_textcontent:sym<code>($/) {
        my $s := $<spaces>.Str;
        my $t := subst($<text>.Str, /\n$s/, "\n", :global);
        $t    := subst($t, /\n$/, ''); # chomp!
        my $past := Perl6::Pod::serialize_object(
            'Pod::Block::Code',
            :contents(Perl6::Pod::serialize_aos([$t]).compile_time_value),
        );
        make $past.compile_time_value;
    }

    method pod_formatting_code($/) {
        if $<code> eq 'V' {
            make ~$<contents>;
        } elsif $<code> eq 'E' {
            my @contents := [];
            my @meta    := [];
            for $/[0] {
                if $_<html_ref> {
                    @contents.push(~$_);
                    @meta.push($*W.add_string_constant(~$_).compile_time_value);
                    #my $s := Perl6::Pod::str_from_entity(~$_);
                    #$s ?? @contents.push($s) && @meta.push(~$_)
                    #   !! $/.CURSOR.worry("\"$_\" is not a valid HTML5 entity.");
                } else {
                    my $n := $_<integer>
                          ?? $_<integer>.made
                          !! nqp::codepointfromname(~$_);
                    if $n >= 0 {
                        @contents.push(nqp::chr($n));
                        @meta.push($n);
                    } else {
                        $/.CURSOR.worry("\"$_\" is not a valid Unicode character name or code point.");
                    }
                }
            }
            @contents := Perl6::Pod::serialize_aos(@contents).compile_time_value;
            @meta    := Perl6::Pod::serialize_array(@meta).compile_time_value;
            make Perl6::Pod::serialize_object(
                'Pod::FormattingCode',
                :type($*W.add_string_constant(~$<code>).compile_time_value),
                :@contents,
                :@meta,
            ).compile_time_value;
        } else {
            my @contents := [];
            for $<pod_string_character> {
                @contents.push($_.ast)
            }
            my @meta := [];
            if $<code> eq 'X' {
                for $/[0] {
                    my @tmp := [];
                    for $_<meta> {
                        @tmp.push(~$_);
                    }
                    @meta.push(@tmp);
                }
                @meta := Perl6::Pod::serialize_aoaos(@meta).compile_time_value;
            } else {
                for $<meta> {
                    @meta.push(~$_)
                }
                @meta := Perl6::Pod::serialize_aos(@meta).compile_time_value;
            }
            my @t    := Perl6::Pod::build_pod_string(@contents);
            my $past := Perl6::Pod::serialize_object(
                'Pod::FormattingCode',
                :type(
                    $*W.add_string_constant(~$<code>).compile_time_value
                ),
                :contents(
                    Perl6::Pod::serialize_array(@t).compile_time_value
                ),
                :meta(@meta),
            );
            make $past.compile_time_value;
        }
    }

    method pod_string($/) {
        my @contents := [];
        for $<pod_string_character> {
            @contents.push($_.ast)
        }
        make Perl6::Pod::build_pod_string(@contents);
    }

    method pod_balanced_braces($/) {
        if $<endtag> {
            my @contents := [];
            my @stringparts := [];
            @stringparts.push(~$<start>);
            if $<pod_string_character> {
                for $<pod_string_character> {
                    if nqp::isstr($_.ast) {
                        @stringparts.push($_.ast);
                    } else {
                        @contents.push(nqp::join("", @stringparts));
                        @stringparts := nqp::list();
                        @contents.push($_.ast);
                    }
                }
            }
            @stringparts.push(~$<endtag>);
            @contents.push(nqp::join("", @stringparts));
            if +@contents == 1 {
                make @contents[0];
            } else {
                make Perl6::Pod::build_pod_string(@contents);
            }
        } else {
            make ~$<braces>
        }
    }

    method pod_string_character($/) {
        if $<pod_formatting_code> {
            make $<pod_formatting_code>.ast
        } elsif $<pod_balanced_braces> {
            make $<pod_balanced_braces>.ast
        } else {
            make ~$<char>;
        }
    }

    method table_row($/) {
        make ~$/
    }

    method table_row_or_blank($/) {
        make ~$/
    }

    method unitstart($/) {
        # Use SET_BLOCK_OUTER_CTX (inherited from HLL::Actions)
        # to set dynamic outer lexical context and namespace details
        # for the compilation unit.
        self.SET_BLOCK_OUTER_CTX($*UNIT_OUTER);
    }

    method statementlist($/) {
        my $past := QAST::Stmts.new( :node($/) );
        if $<statement> {
            my int $i := 0;
            my int $e := nqp::elems($<statement>) - 1;
            while $i <= $e {
                my $ast := $<statement>[$i].ast;
                if $ast {
                    if $ast.ann('statement_level') && $*statement_level {
                        $ast.ann('statement_level')();
                    }
                    if $ast.ann('sink_ast') {
                        $ast := QAST::Want.new($ast, 'v', $ast.ann('sink_ast'));
                        $ast := UNWANTED($ast, 'statementlist/sink_ast') if $i < $e;
                    }
                    elsif $ast.ann('bare_block') {
                        if $i < $e {
                            $ast := UNWANTED(autosink($ast.ann('bare_block')), "statementlist/bare_block");
                        }
                        elsif $*ESCAPEBLOCK {
                            $ast := WANTED($ast.ann('bare_block'),'statementlist/escape');
                        }
                        else {
                            $ast := autosink($ast.ann('bare_block'));
                        }
                    }
                    else {
                        if nqp::istype($ast,QAST::Op) && ($ast.op eq 'while' || $ast.op eq 'until') {
                            $ast := UNWANTED($ast,'statementlist/loop');   # statement level loops never want return value
                        }
                        elsif $i == $e && $*ESCAPEBLOCK {
                            $ast := QAST::Stmt.new(autosink(WANTED($ast,'statementlist/else')), :returns($ast.returns));
                        }
                        else {
                            $ast := QAST::Stmt.new(autosink($ast), :returns($ast.returns));
                        }
                    }
                    $ast.node($<statement>[$i]);
                    $past.push( $ast );
                }
                ++$i;
            }
        }
        if +$past.list < 1 {
            $past.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        }
        else {
            my $pl := $past[+@($past) - 1];
            if $pl.ann('context') eq 'sink' {
                $past.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
            }
            else {
                $pl.annotate('final', 1);
                $past.returns($pl.returns);
            }
        }
        make $past;
    }

    # Produces a LoL from a semicolon list
    method semilist($/) {
        if $<statement> {
            my $past := QAST::Stmts.new( :node($/) );
            if $<statement> > 1 {
                my $l := QAST::Op.new( :name('&infix:<,>'), :op('call') );
                for $<statement> {
                    $l.push(wanted($_.ast,'semilist'));
                }
                $past.push($l);
                $past.annotate('multislice', 1);
            }
            else {
                $past.push($<statement>[0].ast);
            }
            make $past;
        }
        else {
            make QAST::Op.new( :op('call'), :name('&infix:<,>') );
        }
    }

    method sequence($/) {
        my $past := QAST::Stmts.new( :node($/) );
        if $<statement> {
            for $<statement> { $past.push($_.ast) if $_.ast; }
        }
        unless +@($past) {
            $past.push( QAST::Op.new( :op('call'), :name('&infix:<,>') ) );
        }
        make $past;
    }

    method statement($/) {
        my $past;
        if $<EXPR> {
            my $mc := $<statement_mod_cond>;
            my $ml := $<statement_mod_loop>;
            $past := $<EXPR>.ast;
            if $mc {
                if ~$mc<sym> eq 'with' {
                    make thunkity_thunk($/,'.b',QAST::Op.new( :op('call'), :name('&infix:<andthen>')),[$mc,$<EXPR>]);
                    return;
                }
                elsif ~$mc<sym> eq 'without' {
                    make thunkity_thunk($/,'.b',QAST::Op.new( :op('call'), :name('&infix:<notandthen>')),[$mc,$<EXPR>]);
                    return;
                }
                my $mc_ast := $mc.ast;
                if $past.ann('bare_block') {
                    my $cond_block := $past.ann('past_block');
                    remove_block($*W.cur_lexpad(), $cond_block);
                    $cond_block.blocktype('immediate');
                    $past := $cond_block;
                }
                $mc_ast.push($past);
                $mc_ast.push(QAST::WVal.new( :value($*W.find_symbol(['Empty'])) ));
                $past := $mc_ast;
            }
            if $ml {
                $past.annotate("okifnil",1);
                $past[0].annotate("okifnil",1) if +@($past);
                my $cond := $ml<smexpr>.ast;
                if ~$ml<sym> eq 'given' {
                    unless $past.ann('bare_block') {
                        $past := make_topic_block_ref($/, $past, migrate_stmt_id => $*STATEMENT_ID);
                    }
                    $past := QAST::Op.new( :op('call'), block_closure($past), $cond );
                }
                elsif ~$ml<sym> eq 'for' {
                    unless $past.ann('past_block') {
                        $past := make_topic_block_ref($/, $past, migrate_stmt_id => $*STATEMENT_ID);
                    }
                    $past := QAST::Want.new(
                        QAST::Op.new(
                            :op<p6for>, :node($/),
                            $cond,
                            block_closure($past),
                        ),
                        'v', QAST::Op.new(
                            :op<p6for>, :node($/),
                            $cond,
                            block_closure($past),
                        ),
                    );
                    $past[0].annotate('context', 'eager');
                    $past[2].annotate('context', 'sink');
                    my $sinkee := $past[0];
                    $past.annotate('statement_level', -> { $sinkee.annotate('context', 'sink') });
                }
                else {
                    $past := QAST::Op.new($cond, $past, :op(~$ml<sym>), :node($/) );
                }
            }
        }
        elsif $<statement> { $past := $<statement>.ast; }
        elsif $<statement_control> { $past := $<statement_control>.ast; }
        else { $past := 0; }

        if $past {
            my $id := $*STATEMENT_ID;
            $past.annotate('statement_id', $id);

            # only trace when running in source
            if %*PRAGMAS<trace> && !$*W.is_precompilation_mode {
                my $code := ~$/;

                # don't bother putting ops for activating it
                if $code eq 'use trace' {
                    $past := 0;
                }

                # need to generate code
                else {
                    my $line := $*W.current_line($/);
                    my $file := $*W.current_file;
                    $code    := subst($code, /\s+$/, ''); # chomp!
                    $past := QAST::Stmts.new(:node($/),
                        QAST::Op.new(
                            :op<sayfh>,
                            QAST::Op.new(:op<getstderr>),
                            QAST::SVal.new(:value("$id ($file line $line)\n$code"))
                        ),
                        $past
                    );
                }
            }
        }

        make $past;
    }

    method xblock($/) {
        make QAST::Op.new( WANTED($<EXPR>.ast, 'xblock'), $<pblock>.ast, :op('if'), :node($/) );
    }

    method pblock($/) {
        if $<blockoid><you_are_here> {
            make $<blockoid>.ast;
        }
        else {
            # Locate or build a set of parameters.
            my %sig_info;
            my @params;
            my $block := $<blockoid>.ast;
            if $block.ann('placeholder_sig') && $<signature> {
                $*W.throw($/, ['X', 'Signature', 'Placeholder'],
                    precursor => '1',
                    placeholder => $block.ann('placeholder_sig')[0]<placeholder>,
                );
            }
            elsif $block.ann('placeholder_sig') {
                @params := $block.ann('placeholder_sig');
                %sig_info<parameters> := @params;
                if $*IMPLICIT {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'pblock/place'),
                        WANTED(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ),'pblock/place')
                    ));
                }
            }
            elsif $<signature> {
                %sig_info := %*SIG_INFO;
                @params := %sig_info<parameters>;
                if $*IMPLICIT {
                    my int $declares_topic := 0;
                    for @params {
                        if $_<variable_name> eq '$_' {
                            $declares_topic := 1;
                        }
                    }
                    unless $declares_topic {
                        $block[0].push(QAST::Op.new(
                            :op('bind'),
                            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'pblock/sig'),
                            WANTED(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ),'pblock/sig')
                        ));
                    }
                }
                $block[1] := wrap_return_type_check($block[1], $*DECLARAND);
            }
            else {
                if $*IMPLICIT {
                    @params.push(hash(
                        :variable_name('$_'), :optional(1),
                        :nominal_type($*W.find_symbol(['Mu'])),
                        :default_from_outer(1), :is_raw(1),
                    ));
                }
                elsif !$block.symbol('$_') {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ),'pblock/sawone'),
                        WANTED(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ),'pblock/sawone')
                    ));
                    $block.symbol('$_', :scope('lexical'), :type($*W.find_symbol(['Mu'])));
                }
                %sig_info<parameters> := @params;
            }

            # Create signature object if we didn't already, and set up binding.
            my $signature := $*SIG_OBJ // $*W.create_signature_and_params(
                $<signature>, %sig_info, $block, 'Mu');
            add_signature_binding_code($block, $signature, @params);

            # We'll install PAST in current block so it gets capture_lex'd.
            # Then evaluate to a reference to the block (non-closure - higher
            # up stuff does that if it wants to).
            ($*W.cur_lexpad())[0].push(my $uninst := QAST::Stmts.new($block));
            Perl6::Pod::document($/, $*DECLARAND, $*POD_BLOCK, :leading);
            $*W.attach_signature($*DECLARAND, $signature);
            $*W.finish_code_object($*DECLARAND, $block);
            $*W.add_phasers_handling_code($*DECLARAND, $block);
            my $ref := reference_to_code_object($*DECLARAND, $block);
            $ref.annotate('uninstall_if_immediately_used', $uninst);
            make $ref;
        }
    }

    method block($/) {
        my $block := $<blockoid>.ast;
        if $block.ann('placeholder_sig') {
            my $name := $block.ann('placeholder_sig')[0]<variable_name>;
            unless $name eq '%_' || $name eq '@_' {
                $name := nqp::concat(nqp::substr($name, 0, 1),
                        nqp::concat('^', nqp::substr($name, 1)));
            }

            $*W.throw( $/, ['X', 'Placeholder', 'Block'],
                placeholder => $name,
            );
        }
        ($*W.cur_lexpad())[0].push(my $uninst := QAST::Stmts.new($block));
        $*W.attach_signature($*DECLARAND,
            $*W.create_signature(nqp::hash('parameter_objects', [])));
        $*W.finish_code_object($*DECLARAND, $block);
        $*W.add_phasers_handling_code($*DECLARAND, $block);
        my $ref := reference_to_code_object($*DECLARAND, $block);
        $ref.annotate('uninstall_if_immediately_used', $uninst);
        make $ref;
    }

    method blockoid($/) {
        if $<statementlist> {
            my $past := statementlist_with_handlers($/);
            my $BLOCK := $*CURPAD;
            $BLOCK.blocktype('declaration_static');
            $BLOCK.push($past);
            $BLOCK.node($/);
            $BLOCK.annotate('handlers', %*HANDLERS) if %*HANDLERS;
            fatalize($past) if %*PRAGMAS<fatal>;
            make $BLOCK;
        }
        else {
            if $*HAS_YOU_ARE_HERE {
                $/.CURSOR.panic('{YOU_ARE_HERE} may only appear once in a setting');
            }
            $*HAS_YOU_ARE_HERE := 1;
            make $<you_are_here>.ast;
        }
    }

    sub statementlist_with_handlers($/) {
        my $past := $<statementlist>.ast;
        my $ret := %*SIG_INFO<returns>;
        $past.push(QAST::WVal.new(:value($ret))) if nqp::isconcrete($ret) || $ret.HOW.name($ret) eq 'Nil';
        if %*HANDLERS {
            $past := QAST::Op.new( :op('handle'), $past );
            for %*HANDLERS {
                $past.push($_.key);
                $past.push($_.value);
            }
        }
        $past
    }

    # Under "use fatal", re-write all calls to fatalize their return value
    # unless we can see they are in a boolean context.
    my %boolify_first_child_ops := nqp::hash(
        'if', 1, 'unless', 1, 'defor', 1, 'p6bool', 1,
        'while', 1, 'until', 1, 'repeat_while', 1, 'repeat_until', 1,
    );
    my %boolify_first_child_calls := nqp::hash(
        '&prefix:<?>', 1, '&prefix:<so>', 1,
        '&prefix:<!>', 1, '&prefix:<not>', 1,
        '&defined', 1
    );
    sub fatalize($ast, $bool-context = 0) {
        if nqp::istype($ast, QAST::Op) {
            my str $op := $ast.op;
            if $op eq 'p6fatalize' {
                # We've been here before (tree with shared bits, presumably).
            }
            elsif nqp::existskey(%boolify_first_child_ops, $op) ||
                    $op eq 'call' && nqp::existskey(%boolify_first_child_calls, $ast.name) {
                my int $first := 1;
                for @($ast) {
                    if $first {
                        fatalize($_, 1);
                        $first := 0;
                    }
                    else {
                        fatalize($_);
                    }
                }
            }
            elsif $op eq 'hllize' {
                fatalize($_, $bool-context) for @($ast);
            }
            else {
                 fatalize($_) for @($ast);
                 if !$bool-context && ($op eq 'call' || $op eq 'callmethod') {
                    if $ast.name eq '&fail' {
                        $ast.name('&die');
                    }
                    else {
                        my $new-node := QAST::Op.new( :node($ast.node), :$op, :name($ast.name), :returns($ast.returns) );
                        $new-node.push($ast.shift) while @($ast);
                        $ast.op('p6fatalize');
                        $ast.push($new-node);
                        $ast.push(QAST::WVal.new( :value($*W.find_symbol(['Failure'])) ));
                    }
                 }
            }
        }
        elsif nqp::istype($ast, QAST::Stmt) || nqp::istype($ast, QAST::Stmts) || nqp::istype($ast, QAST::Want) {
            fatalize($_) for @($ast);
        }
    }

    method you_are_here($/) {
        make self.CTXSAVE();
    }

    method newpad($/) {
        my $new_block := $*W.cur_lexpad();
        $new_block.annotate('IN_DECL', $*IN_DECL);
    }

    method finishpad($/) {
        # Generate the $_, $/, and $! lexicals for routines if they aren't
        # already declared. For blocks, $_ will come from the outer if it
        # isn't already declared.
        my $BLOCK := $*W.cur_lexpad();
        my $type := $BLOCK.ann('IN_DECL');
        if $type eq 'mainline' && %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
            # Don't do anything in the case where we are in the mainline of
            # the setting; we don't have any symbols (Scalar, etc.) yet.
            return 1;
        }
        my $is_routine := $type eq 'sub' || $type eq 'method' ||
                          $type eq 'submethod' || $type eq 'mainline';
        if $is_routine {
            # Generate the lexical variable except if...
            #   (1) the block already has one, or
            #   (2) the variable is '$_' and $*IMPLICIT is set
            #       (this case gets handled by getsig)
            unless $BLOCK.symbol('$_') || $*IMPLICIT {
                $*W.install_lexical_magical($BLOCK, '$_');
            }
            for <$/ $! $¢> {
                unless $BLOCK.symbol($_) {
                    $*W.install_lexical_magical($BLOCK, $_);
                }
            }
        }
        else {
            unless $BLOCK.symbol('$_') {
                $BLOCK[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
                unless $*IMPLICIT {
                    $BLOCK[0].push(QAST::Op.new(
                        :op('bind'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'finishpad'),
                        WANTED(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ),'finishpad')
                    ));
                }
                $BLOCK.symbol('$_', :scope('lexical'), :type($*W.find_symbol(['Mu'])));
            }
        }
    }


    ## Statement control

    method statement_control:sym<if>($/) {
        my $count := +$<xblock> - 1;
        my $past;
        if ~$<sym>[$count] ~~ /with/ {
            $past := xblock_immediate_with( $<xblock>[$count].ast );
            $past.op('with');
            $past.push( $<else>
                        ?? pblock_immediate_with( $<else>.ast )
                        !! QAST::WVal.new( :value($*W.find_symbol(['Empty'])) )
            );
        }
        else {
            $past := xblock_immediate( $<xblock>[$count].ast );
            $past.op('if');
            $past.push( $<else>
                        ?? pblock_immediate( $<else>.ast )
                        !! QAST::WVal.new( :value($*W.find_symbol(['Empty'])) )
            );
        }
        # build if/then/elsif structure
        while $count > 0 {
            $count--;
            my $else := $past;
            if ~$<sym>[$count] ~~ /with/ {
                $past := xblock_immediate_with( $<xblock>[$count].ast );
                $past.op('with');
            }
            else {
                $past := xblock_immediate( $<xblock>[$count].ast );
                $past.op('if');
            }
            $past.push($else);
        }
        make $past;
    }

    method statement_control:sym<unless>($/) {
        my $past := $<sym> eq 'without'
            ?? xblock_immediate_with( $<xblock>.ast )
            !! xblock_immediate( $<xblock>.ast );
        $past.push(QAST::WVal.new( :value($*W.find_symbol(['Empty'])) ));
        $past.op(~$<sym>);
        make $past;
    }

    method statement_control:sym<while>($/) {
        my $past := $<xblock>.ast;
        $past.op(~$<sym>);
        make tweak_loop($past);
    }

    method statement_control:sym<repeat>($/) {
        my $op := 'repeat_' ~ ~$<wu>;
        my $past;
        if $<xblock> {
            $past := $<xblock>.ast;
            $past.op($op);
        }
        else {
            $past := QAST::Op.new( $<EXPR>.ast, $<pblock>.ast, :op($op), :node($/) );
        }
        make tweak_loop($past);
    }

    method statement_control:sym<for>($/) {
        my $xblock := $<xblock>.ast;
        my $past := QAST::Want.new(
            QAST::Op.new(
                :op<p6for>, :node($/),
                $xblock[0],
                block_closure($xblock[1]),
            ),
            'v', QAST::Op.new(
                :op<p6for>, :node($/),
                $xblock[0],
                block_closure($xblock[1]),
            ),
        );
        if $*LABEL {
            my $label := QAST::WVal.new( :value($*W.find_symbol([$*LABEL])), :named('label') );
            $past[0].push($label);
            $past[2].push($label);
        }
        $past[0].annotate('context', 'eager');
        $past[2].annotate('context', 'sink');
        my $sinkee := $past[0];
        $past.annotate('statement_level', -> { $sinkee.annotate('context', 'sink') });
        make $past;
    }

    method statement_control:sym<whenever>($/) {
        my $xblock := $<xblock>.ast;
        make QAST::Op.new(
            :op<call>, :name<&WHENEVER>, :node($/),
            $xblock[0], block_closure($xblock[1])
        );
    }

    method statement_control:sym<loop>($/) {
        my $cond := $<e2> ?? WANTED($<e2>.ast, 'statement_control/e2') !! QAST::IVal.new( :value(1) );
        my $loop := QAST::Op.new( $cond, :op('while'), :node($/) );
        $loop.push($<block>.ast);
        if $<e3> {
            $loop.push(UNWANTED($<e3>.ast, 'statement_control/e3'));
        }
        $loop := tweak_loop($loop);
        if $<e1> {
            $loop := QAST::Stmts.new( UNWANTED($<e1>.ast, 'statement_control/e1'), $loop, :node($/) );
        }
        make $loop;
    }

    sub tweak_loop($loop) {
        if $*LABEL {
            $loop.push(QAST::WVal.new( :value($*W.find_symbol([$*LABEL])), :named('label') ));
        }
        # Handle phasers.
        my $code := $loop[1].ann('code_object');
        my $block_type := $*W.find_symbol(['Block'], :setting-only);
        my $phasers := nqp::getattr($code, $block_type, '$!phasers');
        if nqp::isnull($phasers) {
            $loop[1] := pblock_immediate($loop[1]);
        }
        else {
            if nqp::existskey($phasers, 'NEXT') {
                my $phascode := $*W.run_phasers_code($code, $loop[1], $block_type, 'NEXT');
                if +@($loop) == 2 {
                    $loop.push($phascode);
                }
                else {
                    $loop[2] := QAST::Stmts.new($phascode, $loop[2]);
                }
            }
            if nqp::existskey($phasers, 'FIRST') {
                my $tmp := QAST::Node.unique('LOOP_BLOCK');
                $loop := QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                        QAST::Op.new( :op('p6setfirstflag'), $loop[1] )
                    ),
                    $loop);
                $loop[1][1] := QAST::Op.new( :op('call'), QAST::Var.new( :name($tmp), :scope('local') ) );
            }
            else {
                $loop[1] := pblock_immediate($loop[1]);
            }
            if nqp::existskey($phasers, 'LAST') {
                $loop := QAST::Stmts.new(
                    :resultchild(0),
                    $loop,
                    $*W.run_phasers_code($code, $loop[1], $block_type, 'LAST'));
            }
        }
        $loop
    }

    method statement_control:sym<need>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        for $<version> {
            # XXX TODO: Version checks.
        }
        make $past;
    }

    method statement_control:sym<import>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        make $past;
    }

    method statement_control:sym<use>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        if $<statementlist> {
            $past := $<statementlist>.ast;
        }
        elsif $<arglist> {
            WANTED($<arglist><EXPR>.ast, 'use');
        }
        elsif $<version> {
            # TODO: replace this by code that doesn't always die with
            # a useless error message
#            my $i := -1;
#            for $<version><vnum> {
#                ++$i;
#                if $_ ne '*' && $_ < @MAX_PERL_VERSION[$i] {
#                    last;
#                } elsif $_ > @MAX_PERL_VERSION[$i] {
#                    my $mpv := nqp::join('.', @MAX_PERL_VERSION);
#                    $/.CURSOR.panic("Perl $<version> required--this is only v$mpv")
#                }
#            }
        }
        make $past;
    }

    method statement_control:sym<require>($/) {
        my $past := QAST::Stmts.new(:node($/));
        my $compunit_past;
        my $target_package;
        my $has_file;
        my $longname;
        if $<module_name> {
            for $<module_name><longname><colonpair> -> $colonpair {
                if ~$colonpair<identifier> eq 'file' {
                    $has_file := $colonpair.ast[2];
                    last;
                }
            }
            $longname := $*W.dissect_longname($<module_name><longname>);
            $target_package := $*W.dissect_longname($<module_name><longname>).name_past;
        }
        if $<module_name> && nqp::defined($has_file) == 0 {
            my $short_name := nqp::clone($target_package);
            $short_name.named('short-name');
            my $spec := QAST::Op.new(
                :op('callmethod'), :name('new'),
                $*W.symbol_lookup(['CompUnit', 'DependencySpecification'], $/),
                $short_name,
            );
            $compunit_past := QAST::Op.new(
                :op('callmethod'), :name('need'),
                QAST::Op.new(
                    :op('callmethod'), :name('head'),
                    $*W.symbol_lookup(['CompUnit', 'RepositoryRegistry'], $/),
                ),
                $spec,
            );
        }
        else {
            my $file_past := WANTED(($has_file ?? $has_file !! $<file>.ast), 'require/name');
            $compunit_past := QAST::Op.new(
                :op('callmethod'), :name('load'),
                QAST::Op.new(
                    :op('callmethod'), :name('head'),
                    $*W.symbol_lookup(['CompUnit', 'RepositoryRegistry'], $/),
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('IO'),
                    $file_past,
                ),
            );
        }
        my $require_past := QAST::Op.new(:node($/), :op<call>,
                                        :name<&REQUIRE_IMPORT>,
                                        $compunit_past,
                                        );
        if $<EXPR> {
            my $p6_argiter   := $*W.compile_time_evaluate($/, $<EXPR>.ast).eager.iterator;
            my $IterationEnd := $*W.find_symbol(['IterationEnd']);
            my $lexpad      := $*W.cur_lexpad();
            my $*SCOPE      := 'my';

            while !((my $arg := $p6_argiter.pull-one) =:= $IterationEnd) {
                my $symbol := nqp::unbox_s($arg.Str());
                $*W.throw($/, ['X', 'Redeclaration'], :$symbol)
                    if $lexpad.symbol($symbol);
                declare_variable($/, $past,
                        nqp::substr($symbol, 0, 1), '', nqp::substr($symbol, 1),
                        []);
                $require_past.push($*W.add_string_constant($symbol));
            }
        }
        $past.push($require_past);
        $past.push($<module_name>
                   ?? self.make_indirect_lookup($longname.components())
                   !! $<file>.ast);
        make $past;
    }

    method statement_control:sym<given>($/) {
        my $past := $<xblock>.ast;
        $past.push($past.shift); # swap [0] and [1] elements
        $past[0] := block_closure($past[0]);
        $past.op('call');
        make $past;
    }

    method statement_control:sym<when>($/) {
        # Get hold of the smartmatch expression and the block.
        my $xblock := $<xblock>.ast;
        my $sm_exp := $xblock.shift;
        my $pblock := $xblock.shift;
        check_smartmatch($<xblock>,$sm_exp);

        # Handle the smart-match.
        my $match_past := QAST::Op.new( :op('callmethod'), :name('ACCEPTS'),
            $sm_exp,
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'when')
        );

        # Use the smartmatch result as the condition for running the block,
        # and ensure continue/succeed handlers are in place and that a
        # succeed happens after the block.
        $pblock := pblock_immediate($pblock);
        make QAST::Op.new( :op('if'), :node( $/ ),
            $match_past, when_handler_helper($pblock)
        );
    }

    method statement_control:sym<default>($/) {
        # We always execute this, so just need the block, however we also
        # want to make sure we succeed after running it.
        make when_handler_helper($<block>.ast);
    }

    method statement_control:sym<CATCH>($/) {
        if nqp::existskey(%*HANDLERS, 'CATCH') {
            $*W.throw($/, ['X', 'Phaser', 'Multiple'], block => 'CATCH');
        }
        my $block := $<block>.ast;
        set_block_handler($/, $block, 'CATCH');
        make QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method statement_control:sym<CONTROL>($/) {
        if nqp::existskey(%*HANDLERS, 'CONTROL') {
            $*W.throw($/, ['X', 'Phaser', 'Multiple'], block => 'CONTROL');
        }
        my $block := $<block>.ast;
        set_block_handler($/, $block, 'CONTROL');
        make QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method statement_control:sym<QUIT>($/) {
        my $block := $<block>.ast;

        # Take exception as parameter and bind into $_.
        my $past := $block.ann('past_block');
        $past[0].push(QAST::Op.new(
            :op('bind'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'QUIT'),
            WANTED(QAST::Var.new( :name('__param'), :scope('local'), :decl('param') ),'QUIT')
        ));

        # If the handler has a succeed handler, then make sure we sink
        # the exception it will produce.
        if $past.ann('handlers') && nqp::existskey($past.ann('handlers'), 'SUCCEED') {
            my $suc := $past.ann('handlers')<SUCCEED>;
            $suc[0] := QAST::Stmts.new(
                sink(QAST::Op.new(
                    :op('getpayload'),
                    QAST::Op.new( :op('exception') )
                )),
                QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
            );
        }

        # If we don't handle the exception by succeeding, we'll return it.
        if $past.ann('handlers') {
            $past[1][0].push(WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'QUIT/handlers'));
        }
        else {
            $past[1].push(WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'QUIT/handlers'));
        }

        # Add as a QUIT phaser, which evaluates to Nil.
        make $*W.add_phaser($/, 'QUIT', $block.ann('code_object'));
    }

    method statement_prefix:sym<BEGIN>($/) {
        begin_time_lexical_fixup($<blorst>.ast.ann('past_block'));
        make $*W.add_phaser($/, 'BEGIN', wanted($<blorst>.ast,'BEGIN').ann('code_object'));
    }
    method statement_prefix:sym<CHECK>($/) {
        begin_time_lexical_fixup($<blorst>.ast.ann('past_block'));
        make $*W.add_phaser($/, 'CHECK', wanted($<blorst>.ast,'CHECK').ann('code_object'));
    }
    method statement_prefix:sym<COMPOSE>($/) { make $*W.add_phaser($/, 'COMPOSE', unwanted($<blorst>.ast,'COMPOSE').ann('code_object')); }
    method statement_prefix:sym<INIT>($/)    { make $*W.add_phaser($/, 'INIT', $<blorst>.ast.ann('code_object'), ($<blorst>.ast).ann('past_block')); }
    method statement_prefix:sym<ENTER>($/)   { make $*W.add_phaser($/, 'ENTER', $<blorst>.ast.ann('code_object')); }
    method statement_prefix:sym<FIRST>($/)   { make $*W.add_phaser($/, 'FIRST', $<blorst>.ast.ann('code_object')); }

    method statement_prefix:sym<END>($/)   { make $*W.add_phaser($/, 'END', unwanted($<blorst>.ast,'END').ann('code_object')); }
    method statement_prefix:sym<LEAVE>($/) { make $*W.add_phaser($/, 'LEAVE', unwanted($<blorst>.ast,'LEAVE').ann('code_object')); }
    method statement_prefix:sym<KEEP>($/)  { make $*W.add_phaser($/, 'KEEP', unwanted($<blorst>.ast,'KEEP').ann('code_object')); }
    method statement_prefix:sym<UNDO>($/)  { make $*W.add_phaser($/, 'UNDO', unwanted($<blorst>.ast,'UNDO').ann('code_object')); }
    method statement_prefix:sym<NEXT>($/)  { make $*W.add_phaser($/, 'NEXT', unwanted($<blorst>.ast,'NEXT').ann('code_object')); }
    method statement_prefix:sym<LAST>($/)  { make $*W.add_phaser($/, 'LAST', unwanted($<blorst>.ast,'LAST').ann('code_object')); }
    method statement_prefix:sym<PRE>($/)   { make $*W.add_phaser($/, 'PRE', wanted($<blorst>.ast,'PRE').ann('code_object'), ($<blorst>.ast).ann('past_block')); }
    method statement_prefix:sym<POST>($/)  { make $*W.add_phaser($/, 'POST', wanted($<blorst>.ast,'POST').ann('code_object'), ($<blorst>.ast).ann('past_block')); }
    method statement_prefix:sym<CLOSE>($/) { make $*W.add_phaser($/, 'CLOSE', unwanted($<blorst>.ast,'CLOSE').ann('code_object')); }

    method statement_prefix:sym<DOC>($/)   {
        if %*COMPILING<%?OPTIONS><doc> {
            make $*W.add_phaser($/, ~$<phase>, ($<blorst>.ast).ann('code_object'), wanted($<blorst>.ast,'DOC').ann('past_block'));
        }
        else {
            make QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        }
    }

    method statement_prefix:sym<do>($/) {
        make QAST::Op.new( :op('call'), $<blorst>.ast );
    }

    method statement_prefix:sym<gather>($/) {
        my $past := block_closure(unwanted($<blorst>.ast,'gather'));
        $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        make QAST::Op.new( :op('call'), :name('&GATHER'), $past );
    }

    method statement_prefix:sym<supply>($/) {
        my $past := block_closure($<blorst>.ast);
        $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        make QAST::Op.new( :op('call'), :name('&SUPPLY'), $past );
    }

    method statement_prefix:sym<react>($/) {
        my $past := block_closure($<blorst>.ast);
        $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        make QAST::Op.new( :op('call'), :name('&REACT'), $past );
    }

    method statement_prefix:sym<once>($/) {
        # create state variable to remember whether we ran the block
        my $pad := $*W.cur_lexpad();
        my $sym := $pad.unique('once_');
        my $mu := $*W.find_symbol(['Mu']);
        my $descriptor := $*W.create_container_descriptor($mu, 1, $sym);
        my %info;
        %info<container_type> := %info<container_base> := $*W.find_symbol(['Scalar']);
        %info<scalar_value> := %info<default_value> := %info<bind_constraint> := %info<value_type> := $mu;
        $*W.install_lexical_container($pad, $sym, %info, $descriptor, :scope('state'));
        for @($pad[0]) {
            if nqp::istype($_, QAST::Var) && $_.name eq $sym {
                # Mark, so we can migrate the once into a correct scope.
                $_.annotate('statement_id', $*STATEMENT_ID);
                $_.annotate('in_stmt_mod', $*IN_STMT_MOD);
                last;
            }
        }

        # generate code that runs the block only once
        make QAST::Op.new(
            :op('if'),
            QAST::Op.new( :op('p6stateinit') ),
            QAST::Op.new(
                :op('p6store'),
                WANTED(QAST::Var.new( :name($sym), :scope('lexical') ),'once'),
                QAST::Op.new( :op('call'), wanted($<blorst>.ast,'once') )
            ),
            WANTED(QAST::Var.new( :name($sym), :scope('lexical') ),'once')
        );
    }

    method statement_prefix:sym<start>($/) {
        make QAST::Op.new(
            :op('callmethod'),
            :name('start'),
            :returns($*W.find_symbol(['Promise'])),
            QAST::WVal.new( :value($*W.find_symbol(['Promise'])) ),
            $<blorst>.ast
        );
    }

    method statement_prefix:sym<lazy>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('lazy'),
            QAST::Op.new( :op('call'), $<blorst>.ast )
        );
    }

    method statement_prefix:sym<eager>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('eager'),
            QAST::Op.new( :op('call'), $<blorst>.ast )
        );
    }

    method statement_prefix:sym<hyper>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('hyper'),
            QAST::Op.new( :op('call'), $<blorst>.ast )
        );
    }

    method statement_prefix:sym<race>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('race'),
            QAST::Op.new( :op('call'), $<blorst>.ast )
        );
    }

    method statement_prefix:sym<sink>($/) {
        make QAST::Stmts.new(
            QAST::Op.new(
                :op('callmethod'), :name('eager'),
                QAST::Op.new( :op('call'), $<blorst>.ast )
            ),
            QAST::Var.new( :name('Nil'), :scope('lexical')),
            :node($/)
        );
    }

    method statement_prefix:sym<try>($/) {
        my $block := $<blorst>.ast;
        my $past;
        if $block.ann('past_block').ann('handlers') && $block.ann('past_block').ann('handlers')<CATCH> {
            # we already have a CATCH block, nothing to do here
            $past := QAST::Op.new( :op('call'), $block );
        } else {
            $block := QAST::Op.new(:op<call>, $block);
            $past := QAST::Op.new(
                :op('handle'),

                # Success path puts Any into $! and evaluates to the block.
                QAST::Stmt.new(
                    :resultchild(0),
                    $block,
                    QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new( :name<$!>, :scope<lexical> ),
                        QAST::Var.new( :name<Any>, :scope<lexical> )
                    )
                ),

                # On failure, capture the exception object into $!.
                'CATCH', QAST::Stmts.new(
                    QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new(:name<$!>, :scope<lexical>),
                        QAST::Op.new(
                            :name<&EXCEPTION>, :op<call>,
                            QAST::Op.new( :op('exception') )
                        ),
                    ),
                    QAST::WVal.new(
                        :value( $*W.find_symbol(['Nil']) ),
                    ),
                )
            );
        }
        make $past;
    }

    method statement_prefix:sym<quietly>($/) {
        make QAST::Op.new(
            :op('handle'),
            QAST::Op.new( :op('call'), $<blorst>.ast ),
            'WARN',
            QAST::Op.new( :op('resume'), QAST::Op.new( :op('exception') ) )
        );
    }

    method blorst($/) {
        my $block;
        if $<block> {
            $block := $<block>.ast;
        }
        else {
            my $stmt := $<statement>.ast;
            $block := make_thunk_ref($stmt, $/);
            migrate_blocks($*W.cur_lexpad, $block.ann('past_block'),
                -> $b { ($b.ann('statement_id') // -1) == $stmt.ann('statement_id') });
        }
        make block_closure($block);
    }

    # Statement modifiers

    method modifier_expr($/) { make WANTED($<EXPR>.ast, 'modifier_expr'); }

    method statement_mod_cond:sym<if>($/)     {
        make QAST::Op.new( :op<if>, $<modifier_expr>.ast, :node($/) );
    }

    method statement_mod_cond:sym<unless>($/) {
        make QAST::Op.new( :op<unless>, $<modifier_expr>.ast, :node($/) );
    }

    method statement_mod_cond:sym<when>($/) {
        my $pat := $<modifier_expr>.ast;
        check_smartmatch($<modifier_expr>,$pat);
        make QAST::Op.new( :op<if>,
            QAST::Op.new( :name('ACCEPTS'), :op('callmethod'),
                          $pat,
                          WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'when') ),
            :node($/)
        );
    }

    method statement_mod_cond:sym<with>($/)    { make $<modifier_expr>.ast; }
    method statement_mod_cond:sym<without>($/) { make $<modifier_expr>.ast; }

    method smexpr($/) { make WANTED($<EXPR>.ast, 'smexpr'); }

    method statement_mod_loop:sym<while>($/)  { make $<smexpr>.ast; }
    method statement_mod_loop:sym<until>($/)  { make $<smexpr>.ast; }
    method statement_mod_loop:sym<for>($/)    { make $<smexpr>.ast; }
    method statement_mod_loop:sym<given>($/)  { make $<smexpr>.ast; }

    ## Terms

    method term:sym<fatarrow>($/)           { make $<fatarrow>.ast; }
    method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
    method term:sym<variable>($/)           { make $<variable>.ast; }
    method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
    method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
    method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
    method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
    method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
    method term:sym<type_declarator>($/)    { make $<type_declarator>.ast; }
    method term:sym<circumfix>($/)          { make $<circumfix>.ast; }
    method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
    method term:sym<sigterm>($/)            { make $<sigterm>.ast; }
    method term:sym<∞>($/)                  { make QAST::WVal.new( :value($*W.find_symbol(['Inf'])) ); }
    method term:sym<lambda>($/) {
        my $ast   := $<pblock>.ast;
        my $block := $ast.ann('past_block');
        $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
        $block[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));
        make block_closure($ast);
    }
    method term:sym<unquote>($/) {
        make QAST::Unquote.new(:position(nqp::elems(@*UNQUOTE_ASTS)));
        @*UNQUOTE_ASTS.push($<statementlist>.ast);
    }

    method name($/) { }

    method fatarrow($/) {
        make make_pair($/,$<key>.Str, wanted($<val>.ast, 'fatarrow'));
    }

    method coloncircumfix($/) {
        make $<circumfix>
            ?? $<circumfix>.ast
            !! QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method colonpair($/) {
        if $*key {
            if $<var> {
                make make_pair($/,$*key, $<var>.ast);
            }
            elsif $<num> {
                make make_pair($/,$*key, $*W.add_numeric_constant($/, 'Int', $*value));
            }
            elsif $*value ~~ NQPMatch {
                my $val_ast := $*value.ast;
                if $val_ast.isa(QAST::Stmts) && +@($val_ast) == 1 {
                    $val_ast := $val_ast[0];
                }
                make make_pair($/,$*key, $val_ast);
            }
            else {
                make make_pair($/,$*key, QAST::Op.new(
                    :op('p6bool'),
                    QAST::IVal.new( :value($*value) )
                ));
            }
        }
        elsif $<fakesignature> {
            make $<fakesignature>.ast;
        }
        else {
            make $*value.ast;
        }
    }

    method colonpair_variable($/) {
        if $<capvar> {
            make QAST::Op.new(
                :op('call'),
                :name('&postcircumfix:<{ }>'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $*W.add_string_constant(~$<desigilname>)
            );
        }
        else {
            make make_variable($/, [~$/]);
        }
    }

    sub make_pair($/,$key_str, $value) {
        my $key := $*W.add_string_constant($key_str);
        QAST::Op.new(
            :op('callmethod'), :name('new'), :returns($*W.find_symbol(['Pair'])), :node($/),
            WANTED(QAST::Var.new( :name('Pair'), :scope('lexical'), :node($/) ),'make_pair'),
            $key, WANTED($value, 'make_pair')
        )
    }

    method desigilname($/) {
        if $<variable> {
            make QAST::Op.new( :op('callmethod'), wanted($<variable>.ast, 'desigilname') );
        }
    }

    method variable($/) {
        my $past;
        if $<index> {
            $past := QAST::Op.new(
                :op('call'),
                :name('&postcircumfix:<[ ]>'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $*W.add_constant('Int', 'int', +$<index>),
            );
            if $<sigil> eq '@' || $<sigil> eq '%' {
                my $name := $<sigil> eq '@' ?? 'list' !! 'hash';
                $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
            }
        }
        elsif $<postcircumfix> {
            $past := $<postcircumfix>.ast;
            $past.unshift( QAST::Var.new( :name('$/'), :scope('lexical') ) );
            if $<sigil> eq '@' || $<sigil> eq '%' {
                my $name := $<sigil> eq '@' ?? 'list' !! 'hash';
                $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
            }
        }
        elsif $<contextualizer> {
            $past := $<contextualizer>.ast;
        }
        elsif $<infixish> {
            my $name := '&infix' ~ $*W.canonicalize_pair('', $<infixish>.Str);
            $past := QAST::Op.new(
                :op('ifnull'),
                QAST::Var.new( :name($name), :scope('lexical') ),
                QAST::Op.new(
                    :op('die_s'),
                    QAST::SVal.new( :value("Could not find sub $name") )
                ));
        }
        elsif $<desigilname><variable> {
            $past := $<desigilname>.ast;
            $past.name(~$<sigil> eq '@' ?? 'cache' !!
                       ~$<sigil> eq '%' ?? 'hash' !!
                                           'item');
        }
        else {
            my $indirect;
            if $<desigilname> && $<desigilname><longname> {
                my $longname := $*W.dissect_longname($<desigilname><longname>);
                if $longname.contains_indirect_lookup() {
                    if $*IN_DECL {
                        $*W.throw($/, ['X', 'Syntax', 'Variable', 'IndirectDeclaration']);
                    }
                    $past := self.make_indirect_lookup($longname.components(), ~$<sigil>);
                    $indirect := 1;
                }
                else {
                    $past := make_variable($/, $longname.attach_adverbs.variable_components(
                        ~$<sigil>, $<twigil> ?? ~$<twigil> !! ''));
                }
            }
            else {
                my $name := ~$/;
                if !$*IN_DECL && nqp::chars($name) == 1 && $name eq ~$<sigil> {
                    my $*IN_DECL := 'variable';
                    my $*SCOPE := 'state';
                    my $*OFTYPE;  # should default to Mu/Mu/Any
                    $past := QAST::Var.new( :node($/) );
                    $past := declare_variable($/, $past, $name, '', '', []);
                    $past.annotate('nosink', 1);
                }
                else {
                    $past := make_variable($/, [$name]);
                }
            }
        }
        if $*IN_DECL eq 'variable' {
            $past.annotate('sink_ok', 1);
        }

        make $past;
    }

    method contextualizer($/) {
        my $past := $<coercee>.ast;
        if $<sigil> eq '$' && ~$<coercee> eq '' { # for '$()'
            my $result_var := $past.unique('sm_result');
            $past := QAST::Stmt.new(
                # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
                # return value to a result variable.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($result_var), :scope('local'), :decl('var') ),
                    QAST::Op.new(
                        :op('if'),
                        # condition
                        QAST::Op.new(
                            :op('callmethod'), :name('ast'),
                            QAST::Var.new( :name('$/'), :scope('lexical') )
                        ),
                        # when true
                        QAST::Op.new(
                            :op('callmethod'), :name('ast'),
                            QAST::Var.new( :name('$/'), :scope('lexical') )
                        ),
                        # when false
                        QAST::Op.new(
                            :op('callmethod'), :name('Str'),
                            QAST::Var.new( :name('$/'), :scope('lexical') )
                        )
                    )
                ),
                # And finally evaluate to the smart-match result.
                WANTED(QAST::Var.new( :name($result_var), :scope('local') ),'make_smartmatch')
            );
            $past := QAST::Op.new( :op('locallifetime'), $past, $result_var );
        }
        else {
            my $name := ~$<sigil> eq '@' ?? 'cache' !!
                        ~$<sigil> eq '%' ?? 'hash' !!
                                            'item';
            # @() and %()
            $past := QAST::Var.new( :name('$/'), :scope('lexical') ) if ~$<coercee> eq '';

            $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
        }
        make WANTED($past, 'contextualizer');
    }

    sub make_variable($/, @name) {
        make_variable_from_parts($/, @name, ~$<sigil>, ~$<twigil>, ~$<desigilname>);
    }

    sub make_variable_from_parts($/, @name, $sigil, $twigil, $desigilname) {
        my $past := QAST::Var.new( :name(@name[+@name - 1]), :node($/));
        my $name := $past.name();

        if $twigil eq '*' {
            if +@name > 1 {
                $*W.throw($/, 'X::Dynamic::Package', symbol => ~$/);
            }
            $past := QAST::Op.new(
                :op('call'), :name('&DYNAMIC'),
                $*W.add_string_constant($name));
        }
        elsif $twigil eq '?' && $*IN_DECL eq 'variable' && !$*COMPILING_CORE_SETTING {
            $*W.throw($/, 'X::Syntax::Variable::Twigil',
              twigil     => $twigil,
              scope      => $*SCOPE,
              additional => ' because it is reserved'
            );
        }
        elsif $twigil eq '!' {
            # In a declaration, don't produce anything here.
            if $*IN_DECL ne 'variable' {
                setup_attr_var($/, $past);
            }
        }
        elsif $twigil eq '.' && $*IN_DECL ne 'variable' {
            if !$*HAS_SELF {
                $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => $name);
            } elsif $*HAS_SELF eq 'partial' {
                $*W.throw($/, ['X', 'Syntax', 'VirtualCall'], call => $name);
            }
            # Need to transform this to a method call.
            $past := $<arglist> ?? $<arglist>.ast !! QAST::Op.new();
            $past.op('callmethod');
            $past.name($desigilname);
            $past.unshift(QAST::Var.new( :name('self'), :scope('lexical') ));
            # Contextualize based on sigil.
            $past := QAST::Op.new(
                :op('callmethod'),
                :name($sigil eq '@' ?? 'list' !!
                      $sigil eq '%' ?? 'hash' !!
                      'item'),
                $past);
        }
        elsif $twigil eq '^' || $twigil eq ':' {
            $past := add_placeholder_parameter($/, $sigil, $desigilname,
                                :named($twigil eq ':'), :full_name($name));
        }
        elsif $twigil eq '~' {
            my $actionsname := $desigilname ~ '-actions';
            $past := QAST::Op.new(
                :op<callmethod>, :name<new>, :returns($*W.find_symbol(['Slang'])),
                QAST::Var.new( :name<Slang>, :scope<lexical> ));
            if nqp::existskey(%*LANG, $desigilname) {
                my $wval := QAST::WVal.new( :value(%*LANG{$desigilname}) );
                $wval.named('grammar');
                $past.push($wval);
            }
            if nqp::existskey(%*LANG, $actionsname) {
                my $wval := QAST::WVal.new( :value(%*LANG{$actionsname}) );
                $wval.named('actions');
                $past.push($wval);
            }
        }
        elsif $twigil eq '=' && $desigilname ne 'pod' && $desigilname ne 'finish' {
            $*W.throw($/,
              'X::Comp::NYI', feature => 'Pod variable ' ~ $name);
        }
        elsif $name eq '@_' {
            if $*W.nearest_signatured_block_declares('@_') {
                $past.scope('lexical');
            }
            else {
                $past := add_placeholder_parameter($/, '@', '_',
                                :pos_slurpy(1), :full_name($name));
            }
        }
        elsif $name eq '%_' {
            if $*W.nearest_signatured_block_declares('%_') || $*METHODTYPE {
                $past.scope('lexical');
            }
            else {
                $past := add_placeholder_parameter($/, '%', '_', :named_slurpy(1),
                                :full_name($name));
            }
        }
        elsif $name eq '$?LINE' || $name eq '$?FILE' {
            if $*IN_DECL eq 'variable' {
                $*W.throw($/, 'X::Syntax::Variable::Twigil',
                  twigil => '?',
                  scope  => $*SCOPE,
                );
            }
            if $name eq '$?LINE' {
                $past := $*W.add_constant('Int', 'int', $*W.current_line($/));
            }
            else {
                $past := $*W.add_string_constant($*W.current_file);
            }
        }
        elsif $name eq '%?RESOURCES' {
            my $resources := nqp::getlexdyn('$*RESOURCES');
            unless $resources {
                my $Resources := $*W.find_symbol(['Distribution', 'Resources']);
                $resources := $Resources.from-precomp();
            }
            if $resources {
                $past := QAST::WVal.new( :value($resources) );
                if nqp::isnull(nqp::getobjsc($resources)) {
                    $*W.add_object($resources);
                }
            }
            else {
                $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
            }
        }
        elsif $name eq '&?BLOCK' || $name eq '&?ROUTINE' {
            if $*IN_DECL eq 'variable' {
                $*W.throw($/, 'X::Syntax::Variable::Twigil',
                  twigil => '?',
                  scope  => $*SCOPE,
                );
            }
            my $Routine := $*W.find_symbol(['Routine'], :setting-only);
            if $name eq '&?BLOCK' || nqp::istype($*CODE_OBJECT, $Routine) {
                # Just need current code object.
                $past := QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) );
            }
            else {
                my int $scopes := 0;
                my int $done := 0;
                $past := QAST::Op.new( :op('ctx') );
                until $done {
                    my $co := $*W.get_code_object(:$scopes);
                    if nqp::istype($co, $Routine) {
                        $past := QAST::Op.new(
                            :op('getcodeobj'),
                            QAST::Op.new( :op('ctxcode'), $past )
                        );
                        $done := 1;
                    }
                    elsif !nqp::isconcrete($co) {
                        # Spit out a lexical that we'll fail to look up. Can't just
                        # go and throw because if we're in an interpolated string
                        # we should not complain about this.
                        $past := QAST::Var.new( :name('&?ROUTINE'), :scope('lexical') );
                        $done := 1;
                    }
                    else {
                        $past := QAST::Op.new( :op('ctxouterskipthunks'), $past );
                    }
                    $scopes++;
                }
            }
        }
        elsif +@name > 1 {
            $past := $*W.symbol_lookup(@name, $/, :lvalue(1));
        }
        elsif $*IN_DECL ne 'variable' && (my $attr_alias := $*W.is_attr_alias($name)) {
            $past.name($attr_alias);
            setup_attr_var($/, $past);
        }
        elsif $*IN_DECL ne 'variable' {
            # Expect variable to have been declared somewhere.
            # Locate descriptor and thus type.
            $past.scope('lexical');
            try {
                my $type := $*W.find_lexical_container_type($name);
                $past.returns($type);
                if nqp::objprimspec($type) && !$*W.is_lexical_marked_ro($past.name) {
                    $past.scope('lexicalref');
                }
            }

            # If it's a late-bound sub lookup, we may not find it, so be sure
            # to handle the case where the lookup comes back null.
            if $sigil eq '&' {
                $past := QAST::Op.new(
                    :op('ifnull'), $past,
                    QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
            }
        }
        $past
    }

    sub setup_attr_var($/, $past) {
        unless $*HAS_SELF {
            $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => $past.name());
        }
        my $attr := $*W.get_attribute_meta_object($/, $past.name(), $past);
        my $type := $attr ?? $attr.type !! NQPMu;
        $past.returns($type) if $attr;
        $past.scope(nqp::objprimspec($type) ?? 'attributeref' !! 'attribute');
        $past.unshift(instantiated_type(['$?CLASS'], $/));
        $past.unshift(QAST::Var.new( :name('self'), :scope('lexical') ));
    }

    method package_declarator:sym<package>($/) { make $<package_def>.ast; }
    method package_declarator:sym<module>($/)  { make $<package_def>.ast; }
    method package_declarator:sym<class>($/)   { make $<package_def>.ast; }
    method package_declarator:sym<grammar>($/) { make $<package_def>.ast; }
    method package_declarator:sym<role>($/)    { make $<package_def>.ast; }
    method package_declarator:sym<knowhow>($/) { make $<package_def>.ast; }
    method package_declarator:sym<native>($/)  { make $<package_def>.ast; }

    method package_declarator:sym<trusts>($/) {
        $*W.apply_trait($/, '&trait_mod:<trusts>', $*PACKAGE, $<typename>.ast);
    }

    method package_declarator:sym<also>($/) {
        $*W.apply_traits($<trait>, $*DECLARAND);
    }

    method package_def($/) {
        # Get the body block AST.
        my $block;
        if $<blockoid> {
            $block := $<blockoid>.ast;
        }
        else {
            $block := $*CURPAD;
            $block.push($<statementlist>.ast);
            $block.node($/);
        }
        $block.blocktype('immediate');

        if $*PKGDECL ne 'role' && $block.ann('placeholder_sig') {
            my $name := $block.ann('placeholder_sig')[0]<variable_name>;
            unless $name eq '%_' || $name eq '@_' {
                $name := nqp::concat(nqp::substr($name, 0, 1),
                        nqp::concat('^', nqp::substr($name, 1)));
            }
            $*W.throw( $/, ['X', 'Placeholder', 'Block'],
                placeholder => $name,
            );
        }

        # If it's a stub, add it to the "must compose at some point" list,
        # then just evaluate to the type object. Don't need to do any more
        # just yet.
        if nqp::eqat($<blockoid><statementlist><statement>[0], '...', 0) {
            unless $*PKGDECL eq 'role' {
                $*W.add_stub_to_check($*PACKAGE);
            }
            $block.blocktype('declaration');
            make QAST::Stmts.new( $block, QAST::WVal.new( :value($*PACKAGE) ) );
            return 1;
        }

        # Handle parametricism for roles.
        if $*PKGDECL eq 'role' {
            # Set up signature. Needs to have $?CLASS as an implicit
            # parameter, since any mention of it is generic.
            my %sig_info := $<signature> ?? $<signature>.ast !! hash(parameters => []);
            my @params := %sig_info<parameters>;
            @params.unshift(hash(
                is_multi_invocant => 1,
                type_captures     => ['$?CLASS', '::?CLASS']
            ));
            my $sig := $*W.create_signature_and_params($<signature>, %sig_info, $block, 'Mu');
            add_signature_binding_code($block, $sig, @params);
            $block.blocktype('declaration_static');

            # Role bodies run at BEGIN time, so need fixup.
            begin_time_lexical_fixup($block);

            # As its last act, it should grab the current lexpad so that
            # we have the type environment, and also return the parametric
            # role we're in (because if we land it through a multi-dispatch,
            # we won't know).
            $block[1].push(QAST::Op.new(
                :op('list'),
                QAST::WVal.new( :value($*PACKAGE) ),
                QAST::Op.new( :op('curlexpad') )));

            # Finish code object and add it as the role's body block.
            my $code := $*CODE_OBJECT;
            $*W.attach_signature($code, $sig);
            $*W.finish_code_object($code, $block, 0);
            $*W.add_phasers_handling_code($code, $block);
            $*W.pkg_set_role_body_block($/, $*PACKAGE, $code, $block);

            # Compose before we add the role to the group, so the group sees
            # it composed.
            $*W.pkg_compose($/, $*PACKAGE);

            # Add this role to the group if needed.
            my $group := $*PACKAGE.HOW.group($*PACKAGE);
            unless $group =:= $*PACKAGE {
                $*W.pkg_add_role_group_possibility($/, $group, $*PACKAGE);
            }
        }
        else {
            # Compose.
            $*W.pkg_compose($/, $*PACKAGE);

            # Finish code object for the block.
            my $code := $*CODE_OBJECT;
            $*W.attach_signature($code, $*W.create_signature(nqp::hash('parameter_objects', [])));
            $*W.finish_code_object($code, $block, 0);
            $*W.add_phasers_handling_code($code, $block);
        }

        # check up any private attribute usage
        for %*ATTR_USAGES {
            my $name   := $_.key;
            my @usages := $_.value;
            for @usages {
                my $past := $_;
                my $attr := $*W.get_attribute_meta_object($past.node, $name);
                $past.returns($attr.type);
            }
        }

        # Document
        Perl6::Pod::document($/, $*PACKAGE, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($*PACKAGE);
        }

        make QAST::Stmts.new(
            $block, QAST::WVal.new( :value($*PACKAGE) )
        );
    }

    # When code runs at BEGIN time, such as role bodies and BEGIN
    # blocks, we need to ensure we get lexical outers fixed up
    # properly when deserializing after pre-comp. To do this we
    # make a list of closures, which each point to the outer
    # context. These surive serialization and thus point at what
    # has to be fixed up.
    sub begin_time_lexical_fixup($block) {
        my $has_nested_blocks := 0;
        for @($block[0]) {
            if nqp::istype($_, QAST::Block) {
                $has_nested_blocks := 1;
                last;
            }
        }
        return 0 unless $has_nested_blocks;

        my $throwaway_block_past := QAST::Block.new(
            :blocktype('declaration'), :name('!LEXICAL_FIXUP'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ),'btlf')
        );
        $throwaway_block_past.annotate('outer', $block);
        $block[0].push($throwaway_block_past);
        my $throwaway_block := $*W.create_code_object($throwaway_block_past,
            'Block', $*W.create_signature(nqp::hash('parameter_objects', [])));
        my $fixup := $*W.create_lexical_capture_fixup();
        $fixup.push(QAST::Op.new(
                :op('p6capturelex'),
                QAST::Op.new(
                    :op('callmethod'), :name('clone'),
                    QAST::WVal.new( :value($throwaway_block) )
                )));
        $block[0].push($fixup);
    }

    method scope_declarator:sym<my>($/)      { make $<scoped>.ast; }
    method scope_declarator:sym<our>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<has>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)    { make $<scoped>.ast; }
    method scope_declarator:sym<augment>($/) { make $<scoped>.ast; }
    method scope_declarator:sym<state>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<unit>($/)    { make $<scoped>.ast; }

    method declarator($/) {
        if    $<routine_declarator>  { make $<routine_declarator>.ast  }
        elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
        elsif $<type_declarator>     { make $<type_declarator>.ast     }
        elsif $<variable_declarator> {
            my $past := $<variable_declarator>.ast;
            if $<initializer> {
                my $orig_past := $past;
                my $initast := $<initializer>.ast;
                if $*SCOPE eq 'has' {
                    if $<initializer><sym> eq '=' {
                        self.install_attr_init($<initializer>,
                            $past.ann('metaattr'),
                            $initast, $*ATTR_INIT_BLOCK);
                    }
                    elsif $<initializer><sym> eq '.=' {
                        my $type := $*W.find_symbol([ $*OFTYPE // 'Any']);
                        my $dot_equals := $initast;
                        $dot_equals.unshift(QAST::WVal.new(:value($type)));
                        $dot_equals.returns($type);
                        self.install_attr_init($<initializer>,
                            $past.ann('metaattr'),
                            $dot_equals, $*ATTR_INIT_BLOCK);
                    }
                    else {
                        $/.CURSOR.panic("Cannot use " ~ $<initializer><sym> ~
                            " to initialize an attribute");
                    }
                }
                elsif $<initializer><sym> eq '=' {
                    $past := assign_op($/, $past, $initast);
                }
                elsif $<initializer><sym> eq '.=' {
                    $past := make_dot_equals($past, $initast);
                }
                else {
                    if nqp::istype($past, QAST::Var) {
                        find_var_decl($*W.cur_lexpad(), $past.name).decl('var');
                    }
                    $past := bind_op($/, $past, $initast,
                        $<initializer><sym> eq '::=');
                }
                if $*SCOPE eq 'state' {
                    $past := QAST::Op.new( :op('if'),
                        QAST::Op.new( :op('p6stateinit') ),
                        $past,
                        $orig_past);
                    $past.annotate('nosink', 1);
                }
            }
            # No initializer, check that the (specified) type accepts the default value.
            elsif $<variable_declarator><variable><sigil> eq '$' {
                if nqp::istype($past, QAST::Var) {
                    if $*W.cur_lexpad.symbol($past.name) -> %sym {
                        check_default_value_type($/, %sym<descriptor>, %sym<type>, 'variables');
                    }
                }
                elsif $past.ann('metaattr') -> $attr {
                    if !$attr.required && !$attr.type.HOW.archetypes.generic {
                        check_default_value_type($/, $attr.container_descriptor, $attr.type, 'attributes');
                    }
                }
            }
            make $past;
        }
        elsif $<signature> {
            # Go over the params and declare the variable defined in them.
            my $list      := QAST::Op.new( :op('call'), :name('&infix:<,>') );
            my @params    := $<signature>.ast<parameters>;
            my $common_of := $*OFTYPE;
            my @nosigil;
            for @params {
                my $*OFTYPE := $common_of;
                if nqp::existskey($_, 'of_type') && nqp::existskey($_, 'of_type_match') {
                    if $common_of {
                        ($_<node> // $<signature>).CURSOR.typed_sorry(
                            'X::Syntax::Variable::ConflictingTypes',
                            outer => $common_of.ast, inner => $_<of_type>);
                    }
                    $*OFTYPE := $_<of_type_match>;
                    $*OFTYPE.make($_<of_type>);
                }
                if $_<variable_name> {
                    my $past := QAST::Var.new( :name($_<variable_name>) );
                    $past := declare_variable($/, $past, $_<sigil>, $_<twigil>,
                        $_<desigilname>, $<trait>);
                    unless $past.isa(QAST::Op) && $past.op eq 'null' {
                        $list.push($past);
                        if $_<sigil> eq '' {
                            nqp::push(@nosigil, ~$_<desigilname>);
                        }
                    }
                }
                else {
                    $*W.handle_OFTYPE_for_pragma($/,'parameters');
                    my %cont_info := $*W.container_type_info($/, $_<sigil> || '$',
                        $*OFTYPE ?? [$*OFTYPE.ast] !! [], []);
                    $list.push($*W.build_container_past(
                      %cont_info,
                      $*W.create_container_descriptor(
                        %cont_info<value_type>, 1, 'anon', %cont_info<default_value>)));
                }
            }

            if $<initializer> {
                my $orig_list := $list;
                my $initast := $<initializer>.ast;
                if $<initializer><sym> eq '=' {
                    $/.CURSOR.panic("Cannot assign to a list of 'has' scoped declarations")
                        if $*SCOPE eq 'has';
                    $list := assign_op($/, $list, $initast);
                    if @nosigil {
                        $list := QAST::Stmts.new( :resultchild(0), $list );
                        for @nosigil {
                            $list.push(QAST::Op.new(
                                :op('bind'),
                                QAST::Var.new( :name($_), :scope('lexical') ),
                                QAST::Op.new(
                                    :op('p6recont_ro'),
                                    QAST::Var.new( :name($_), :scope('lexical') )
                                )));
                        }
                    }
                }
                elsif $<initializer><sym> eq '.=' {
                    $/.CURSOR.panic("Cannot use .= initializer with a list of declarations");
                }
                else {
                    my %sig_info := $<signature>.ast;
                    my $signature := $*W.create_signature_and_params($/, %sig_info, $*W.cur_lexpad(), 'Mu');
                    $list := QAST::Op.new(
                        :op('p6bindcaptosig'),
                        QAST::WVal.new( :value($signature) ),
                        QAST::Op.new(
                            :op('callmethod'), :name('Capture'),
                            $initast
                        )
                    );
                }
                if $*SCOPE eq 'state' {
                    $list := QAST::Op.new( :op('if'),
                        QAST::Op.new( :op('p6stateinit') ),
                        $list, $orig_list);
                }
            }
            elsif @nosigil {
                $/.CURSOR.typed_panic('X::Syntax::Term::MissingInitializer');
            }

            make $list;
        }
        elsif $<deftermnow> {
            # 'my \foo' style declaration
            my $name       :=  $<deftermnow>.ast;
            if $*OFTYPE {
                my $type := $*OFTYPE.ast;
                make QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                    $type =:= $*W.find_symbol(['Mu'])
                        ?? WANTED($<term_init>.ast, 'declarator/deftermnow1')
                        !! QAST::Op.new(
                            :op('p6bindassert'),
                            WANTED($<term_init>.ast, 'declarator/deftermnow2'),
                            QAST::WVal.new( :value($type) ),
                        )
                );
            }
            else {
                make QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                        WANTED($<term_init>.ast, 'declarator/bind')
                );
            }
        }
        else {
            $/.CURSOR.panic('Unknown declarator type');
        }
    }

    sub check_default_value_type($/, $descriptor, $bind_constraint, $what) {
        unless nqp::istype($descriptor.default, $bind_constraint) {
            $*W.throw($/, 'X::Syntax::Variable::MissingInitializer',
                type => nqp::how($bind_constraint).name($bind_constraint),
                implicit => !nqp::istype($*OFTYPE, NQPMatch) || !$*OFTYPE<colonpairs><D> && !$*OFTYPE<colonpairs><U>
                         ?? ':' ~ %*PRAGMAS{$what} ~ ' by pragma'
                         !! 0
            );
        }
    }

    method multi_declarator:sym<multi>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<proto>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<only>($/)  { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<null>($/)  { make $<declarator>.ast }

    method scoped($/) {
        make $<DECL>.ast;
    }

    method variable_declarator($/) {
        my $past   := $<variable>.ast;
        my $sigil  := $<variable><sigil>;
        my $twigil := $<variable><twigil>;
        my $desigilname := ~$<variable><desigilname>;
        my $name := $sigil ~ $twigil ~ $desigilname;

        # Don't know why this doesn't work all the time.
        if $desigilname ~~ /\w ':' <![:]>/ {
            $name   := $<variable>.ast.name;  # is already canonicalized in <variable>
            $desigilname := nqp::substr($name, nqp::chars($sigil ~ $twigil));
        }

        my @post;
        for $<post_constraint> {
            @post.push($_.ast);
        }
        if $<variable><desigilname> {
            my $lex := $*W.cur_lexpad();
            if $lex.symbol($name) {
                $/.CURSOR.typed_worry('X::Redeclaration', symbol => $name);
            }
            elsif $lex.ann('also_uses') && $lex.ann('also_uses'){$name} {
                if ~$twigil eq '*' {
                    $/.CURSOR.typed_sorry('X::Dynamic::Postdeclaration', symbol => $name);
                }
                else {
                    $/.CURSOR.typed_sorry('X::Redeclaration::Outer', symbol => $name);
                }
            }
        }
        if nqp::elems($<semilist>) > 1 {
            $/.CURSOR.panic('Multiple shapes not yet understood');
        }
        make declare_variable($/, $past, ~$sigil, ~$twigil, $desigilname, $<trait>, $<semilist>, :@post);
    }

    sub declare_variable($/, $past, $sigil, $twigil, $desigilname, $trait_list, $shape?, :@post) {
        my $name  := $sigil ~ $twigil ~ $desigilname;
        my $BLOCK := $*W.cur_lexpad();

        my int $have_of_type;
        my $of_type;
        my int $have_is_type;
        my $is_type;

        $*W.handle_OFTYPE_for_pragma($/, $*SCOPE eq 'has' ?? 'attributes' !! 'variables');
        if $*OFTYPE {
            $have_of_type := 1;
            $of_type := $*OFTYPE.ast;
            my $archetypes := $of_type.HOW.archetypes;
            unless $archetypes.nominal || $archetypes.nominalizable || $archetypes.generic || $archetypes.definite {
                $*OFTYPE.CURSOR.typed_sorry('X::Syntax::Variable::BadType', type => $of_type);
            }
        }

        # Process traits for `is Type` and `of Type`, which get special
        # handling by the compiler.
        my @late_traits;
        for $trait_list {
            my $trait := $_.ast;
            if $trait {
                my str $mod := $trait.mod;
                if $mod eq '&trait_mod:<of>' {
                    my $type := $trait.args[0];
                    if $have_of_type {
                        $_.CURSOR.typed_sorry(
                            'X::Syntax::Variable::ConflictingTypes',
                            outer => $of_type, inner => $type)
                    }
                    $have_of_type := 1;
                    $of_type := $type;
                    next;
                }
                if $mod eq '&trait_mod:<is>' {
                    my @args := $trait.args;
                    if nqp::elems(@args) == 1 && !nqp::isconcrete(@args[0]) {
                        $have_is_type := 1;
                        $is_type := @args[0];
                        next;
                    }
                }
                nqp::push(@late_traits, $_);
            }
        }

        if $*SCOPE eq 'has' {
            # Ensure current package can take attributes.
            unless nqp::can($*PACKAGE.HOW, 'add_attribute') {
                if $*PKGDECL {
                    $*W.throw($/, ['X', 'Attribute', 'Package'],
                        package-kind => $*PKGDECL,
                        :$name,
                    );
                } else {
                    $*W.throw($/, ['X', 'Attribute', 'NoPackage'], :$name);
                }
            }

            # Create container descriptor and decide on any default value.
            if $desigilname eq '' {
                $/.CURSOR.panic("Cannot declare an anonymous attribute");
            }
            my $attrname   := ~$sigil ~ '!' ~ $desigilname;
            my %cont_info  := $*W.container_type_info($/, $sigil,
                $have_of_type ?? [$of_type] !! [],
                $have_is_type ?? [$is_type] !! [],
                $shape, :@post);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, 1, $attrname, %cont_info<default_value>);

            # Create meta-attribute and add it.
            my $metaattr := $*W.resolve_mo($/, $*PKGDECL ~ '-attr');
            my %config := hash(
                name => $attrname,
                has_accessor => $twigil eq '.',
                container_descriptor => $descriptor,
                type => %cont_info<bind_constraint>,
                package => $*W.find_symbol(['$?CLASS']));
            if %cont_info<build_ast> {
                %config<container_initializer> := $*W.create_thunk($/,
                    %cont_info<build_ast>);
            }
            my $attr := $*W.pkg_add_attribute($/, $*PACKAGE, $metaattr,
                %config, %cont_info, $descriptor);

            # Document it
            Perl6::Pod::document($/, $attr, $*POD_BLOCK, :leading);
            if ~$*POD_BLOCK ne '' {
                $*POD_BLOCK.set_docee($attr);
            }

            # Set it up for trailing declarations
            $*PRECEDING_DECL := $attr;

            # If no twigil, note $foo is an alias to $!foo.
            if $twigil eq '' {
                $BLOCK.symbol($name, :attr_alias($attrname));
            }

            # Apply any traits.
            $*W.apply_traits(@late_traits, $attr);

            # Nothing to emit here; hand back a Nil.
            $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
            $past.annotate('metaattr', $attr);
        }
        elsif $*SCOPE eq 'my' || $*SCOPE eq 'our' || $*SCOPE eq 'state' {
            # Some things can't be done to our vars.
            my $varname;
            if $*SCOPE eq 'our' {
                if $have_of_type || @post {
                    $/.CURSOR.panic("Cannot put a type constraint on an 'our'-scoped variable");
                }
                elsif $shape {
                    $/.CURSOR.panic("Cannot put a shape on an 'our'-scoped variable");
                }
                elsif $desigilname eq '' {
                    $/.CURSOR.panic("Cannot have an anonymous 'our'-scoped variable");
                }
                if nqp::can($*PACKAGE.HOW, 'archetypes') && $*PACKAGE.HOW.archetypes.parametric {
                    $*W.throw($/, 'X::Declaration::OurScopeInRole',
                        declaration => 'variable'
                    );
                }
            }
            elsif $desigilname eq '' {
                if $twigil {
                    $/.CURSOR.panic("Cannot have an anonymous variable with a twigil");
                }
                $name    := QAST::Node.unique($sigil ~ 'ANON_VAR_');
                $varname := $sigil;
            }

            # Create a container descriptor. Default to rw and set a
            # type if we have one; a trait may twiddle with that later.
            my %cont_info  := $*W.container_type_info($/, $sigil,
                $have_of_type ?? [$of_type] !! [],
                $have_is_type ?? [$is_type] !! [],
                $shape, :@post);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, 1, $varname || $name, %cont_info<default_value>);

            # Install the container.
            my $cont := $*W.install_lexical_container($BLOCK, $name, %cont_info, $descriptor,
                :scope($*SCOPE), :package($*PACKAGE));

            # Set scope and type on container, and if needed emit code to
            # reify a generic type or create a fresh container.
            if $past.isa(QAST::Var) {
                my $bind_type := %cont_info<bind_constraint>;
                $past.name($name);
                $past.returns($bind_type);
                $past.scope(nqp::objprimspec($bind_type) ?? 'lexicalref' !! 'lexical');
                if %cont_info<bind_constraint>.HOW.archetypes.generic {
                    $past := QAST::Op.new(
                        :op('callmethod'), :name('instantiate_generic'),
                        QAST::Op.new( :op('p6var'), $past ),
                        QAST::Op.new( :op('curlexpad') ));
                }
                elsif %cont_info<build_ast> {
                    if $*SCOPE eq 'state' {
                        $past := QAST::Op.new( :op('if'),
                            QAST::Op.new( :op('p6stateinit') ),
                            QAST::Op.new( :op('bind'), $past, %cont_info<build_ast> ),
                            $past);
                    }
                    else {
                        $past := QAST::Op.new( :op('bind'), $past, %cont_info<build_ast> );
                    }
                }

                if $*SCOPE eq 'our' {
                    $BLOCK[0].push(QAST::Op.new(
                        :op('bind'),
                        $past,
                        $*W.symbol_lookup([$name], $/, :package_only(1), :lvalue(1))
                    ));
                }
            }

            # Twigil handling.
            if $twigil eq '.' {
                add_lexical_accessor($/, $past, $desigilname, $*W.cur_lexpad());
                $name := $sigil ~ $desigilname;
            }
            elsif $twigil eq '!' {
                $*W.throw($/, ['X', 'Syntax', 'Variable', 'Twigil'],
                    twigil => $twigil,
                    scope  => $*SCOPE,
                );
            }

            # Apply any traits.
            if @late_traits {
                my $Variable := $*W.find_symbol(['Variable']);
                my $varvar   := nqp::create($Variable);
                nqp::bindattr_s($varvar, $Variable, '$!name', $name);
                nqp::bindattr_s($varvar, $Variable, '$!scope', $*SCOPE);
                nqp::bindattr($varvar, $Variable, '$!var', $cont);
                nqp::bindattr($varvar, $Variable, '$!block', $*CODE_OBJECT);
                nqp::bindattr($varvar, $Variable, '$!slash', $/);
                $*W.apply_traits(@late_traits, $varvar);
            }
        }
        elsif $*SCOPE eq '' {
            $*W.throw($/, 'X::Declaration::Scope',
                    scope       => '(unknown scope)',
                    declaration => 'variable',
            );
        }
        else {
            $*W.throw($/, 'X::Comp::NYI',
                feature => "$*SCOPE scoped variables");
        }

        $past
    }

    sub add_lexical_accessor($/, $var_past, $meth_name, $install_in) {
        # Generate and install code block for accessor.
        my $a_past := $*W.push_lexpad($/);
        $a_past.name($meth_name);
        $a_past.blocktype('declaration_static');
        $a_past.push($var_past);
        $*W.pop_lexpad();
        $install_in[0].push($a_past);

        # Produce a code object and install it.
        my $invocant_type := $*W.find_symbol([$*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my %sig_info := hash(parameters => []);
        my $signature := $*W.create_signature_and_params($/, %sig_info, $a_past, 'Any',
            :method, :$invocant_type);
        my $code := methodize_block($/, $*W.stub_code_object('Method'),
            $a_past, $signature, %sig_info);
        install_method($/, $meth_name, 'has', $code, $install_in, :gen-accessor);
    }

    method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
    method routine_declarator:sym<method>($/) { make $<method_def>.ast; }
    method routine_declarator:sym<submethod>($/) { make $<method_def>.ast; }

    method routine_def($/) {
        my $block;

        if $<onlystar> {
            $block := $<onlystar>.ast;
        }
        else {
            if $<blockoid> {
                $block := WANTED($<blockoid>.ast,'&defoid');
            } else {
                $block := $*CURPAD;
                $block.blocktype('declaration_static');
                $block.push(WANTED($<statementlist>.ast,'&def'));
                $block.node($/);
            }
            if is_clearly_returnless($block) {
                $block[1] := QAST::Op.new(
                    :op('p6decontrv'),
                    QAST::WVal.new( :value($*DECLARAND) ),
                    $block[1]);
                $block[1] := wrap_return_type_check($block[1], $*DECLARAND);
            }
            else {
                $block[1] := wrap_return_handler($block[1]);
            }
        }
        $block.blocktype('declaration_static');

        # Attach signature, building placeholder if needed.
        my @params;
        my $signature;
        if $*SIG_OBJ {
            if $block.ann('placeholder_sig') {
                $*W.throw($/, ['X', 'Signature', 'Placeholder'],
                    precursor => '1',
                    placeholder => $block.ann('placeholder_sig')[0]<placeholder>,
                );
            }
            @params    := %*SIG_INFO<parameters>;
            $signature := $*SIG_OBJ;
        }
        else {
            @params := $block.ann('placeholder_sig') || [];
            $signature := $*W.create_signature_and_params($/,
                nqp::hash('parameters', @params), $block, 'Any');
        }
        add_signature_binding_code($block, $signature, @params);

        # Needs a slot that can hold a (potentially unvivified) dispatcher;
        # if this is a multi then we'll need it to vivify to a MultiDispatcher.
        if $*MULTINESS eq 'multi' {
            $*W.install_lexical_symbol($block, '$*DISPATCHER', $*W.find_symbol(['MultiDispatcher']));
        }
        else {
            $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
        }
        $block[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));

        # If it's a proto but not an onlystar, need some variables for the
        # {*} implementation to use.
        if $*MULTINESS eq 'proto' && !$<onlystar> {
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('savecapture') )
            ));
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
            ));
        }

        # Set name.
        if $<deflongname> {
            $block.name(~$<deflongname>.ast);
        }

        # Finish code object, associating it with the routine body.
        my $code := $*DECLARAND;
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $block, $*MULTINESS eq 'proto', :yada(is_yada($/)));

        # attach return type
        if $*OFTYPE {
            my $sig := $code.signature;
            if $sig.has_returns {
                my $prev_returns := $sig.returns;
                $*W.throw($*OFTYPE, 'X::Redeclaration',
                    what    => 'return type for',
                    symbol  => $code.name,
                    postfix => " (previous return type was "
                                ~ $prev_returns.HOW.name($prev_returns)
                                ~ ')',
                );
            }
            $sig.set_returns($*OFTYPE.ast);
        }
        # and mixin the parameterize callable for type checks
        if $signature.has_returns {
            my $callable := $*W.find_symbol(['Callable']);
            $code.HOW.mixin($code, $callable.HOW.parameterize($callable, $signature.returns));
        }

        # Document it
        Perl6::Pod::document($/, $code, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($code);
        }

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*W.cur_lexpad();
        my $clone := !($outer =:= $*UNIT);
        $outer[0].push(QAST::Stmt.new($block));

        if $<deflongname> {
            # If it's a multi, need to associate it with the surrounding
            # proto.
            # XXX Also need to auto-multi things with a proto in scope.
            my $name := '&' ~ ~$<deflongname>.ast;
            if $*MULTINESS eq 'multi' {
                # Do we have a proto in the current scope?
                my $proto;
                if $outer.symbol($name) {
                    $proto := $*W.force_value($outer.symbol($name), $name, 0);
                }
                else {
                    unless $*SCOPE eq '' || $*SCOPE eq 'my' {
                        $*W.throw($/, 'X::Declaration::Scope::Multi',
                            scope       => $*SCOPE,
                            declaration => 'multi',
                        );
                    }
                    # None; search outer scopes.
                    my $new_proto;
                    try {
                        $proto := $*W.find_symbol([$name]);
                    }
                    if $proto && $proto.is_dispatcher {
                        # Found in outer scope. Need to derive.
                        $new_proto := $*W.derive_dispatcher($proto);
                    }
                    else {
                        $new_proto := self.autogenerate_proto($/, $block.name, $outer[0]);
                    }

                    # Install in current scope.
                    $*W.install_lexical_symbol($outer, $name, $new_proto, :$clone);
                    $proto := $new_proto;
                }

                # Ensure it's actually a dispatcher.
                unless nqp::can($proto, 'is_dispatcher') && $proto.is_dispatcher {
                    $*W.throw($/, ['X', 'Redeclaration'],
                        what    => 'routine',
                        symbol  => ~$<deflongname>.ast,
                    );
                }

                # Install the candidate.
                $*W.add_dispatchee_to_proto($proto, $code);
            }
            else {
                # Install.
                my $predeclared := $outer.symbol($name);
                if $predeclared {
                    unless nqp::getattr_i($predeclared<value>, $*W.find_symbol(['Routine'], :setting-only), '$!yada') {
                        $*W.throw($/, ['X', 'Redeclaration'],
                                symbol => ~$<deflongname>.ast,
                                what   => 'routine',
                        );
                    }
                }
                if $*SCOPE eq '' || $*SCOPE eq 'my' {
                    $*W.install_lexical_symbol($outer, $name, $code, :$clone);
                }
                elsif $*SCOPE eq 'our' || $*SCOPE eq 'unit' {
                    # Install in lexpad and in package, and set up code to
                    # re-bind it per invocation of its outer.
                    $*W.install_lexical_symbol($outer, $name, $code, :$clone);
                    my $package := $*PACKAGE;
                    if nqp::existskey($package.WHO, $name) {
                        $*W.throw($/, ['X', 'Redeclaration'],
                            symbol  => ~$<deflongname>.ast,
                            what    => 'routine',
                            postfix => ' (already defined in package ' ~ $package.HOW.name($package) ~ ')'
                        );
                    }
                    $*W.install_package_symbol($/, $package, $name, $code, 'sub');
                    $outer[0].push(QAST::Op.new(
                        :op('bindkey'),
                        QAST::Op.new( :op('who'), QAST::WVal.new( :value($package) ) ),
                        QAST::SVal.new( :value($name) ),
                        QAST::Var.new( :name($name), :scope('lexical') )
                    ));
                }
                elsif $*SCOPE eq 'anon' {
                    # don't do anything
                }
                else {
                    $*W.throw($/, 'X::Declaration::Scope',
                            scope       => $*SCOPE,
                            declaration => 'sub',
                    );
                }
            }
        }
        elsif $*MULTINESS {
            $*W.throw($/, 'X::Anon::Multi', multiness => $*MULTINESS);
        }

        # Apply traits.
        $*W.apply_traits($<trait>, $code);
        if $<onlystar> {
            # Protect with try; won't work when declaring the initial
            # trait_mod proto in CORE.setting!
            try $*W.apply_trait($/, '&trait_mod:<is>', $*DECLARAND, :onlystar(1));
        }

        # Handle any phasers.
        $*W.add_phasers_handling_code($code, $block);

        # Add inlining information if it's inlinable; also mark soft if the
        # appropriate pragma is in effect.
        if $<deflongname> {
            if %*PRAGMAS<soft> {
                $*W.find_symbol(['&infix:<does>'])($code, $*W.find_symbol(['SoftRoutine'], :setting-only));
            }
            elsif !nqp::can($code, 'CALL-ME') {
                my $phasers :=
                  nqp::getattr($code,$*W.find_symbol(['Block'], :setting-only),'$!phasers');
                if nqp::isnull($phasers) || !nqp::p6bool($phasers) {
                    self.add_inlining_info_if_possible($/, $code, $signature, $block, @params);
                }
            }
        }

        # If it's a proto, add it to the sort-at-CHECK-time queue.
        if $*MULTINESS eq 'proto' {
            $*W.add_proto_to_sort($code);
        }

        my $closure := block_closure(reference_to_code_object($code, $block));
        $closure.annotate('sink_ast', QAST::Op.new( :op('null') ));
        make $closure;
    }

    method autogenerate_proto($/, $name, $install_in) {
        my $p_past := $*W.push_lexpad($/);
        $p_past.name(~$name);
        $p_past.is_thunk(1);
        $p_past.push(QAST::Op.new(
            :op('invokewithcapture'),
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new(
                    :op('multicachefind'),
                    QAST::Var.new(
                        :name('$!dispatch_cache'), :scope('attribute'),
                        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                        QAST::WVal.new( :value($*W.find_symbol(['Routine'], :setting-only)) ),
                    ),
                    QAST::Op.new( :op('usecapture') )
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('find_best_dispatchee'),
                    QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                    QAST::Op.new( :op('savecapture') )
                ),
            ),
            QAST::Op.new( :op('usecapture') )
        ));
        $*W.pop_lexpad();
        $install_in.push(QAST::Stmt.new($p_past));
        my @p_params := [hash(is_capture => 1, nominal_type => $*W.find_symbol(['Mu']) )];
        my $p_sig := $*W.create_signature(nqp::hash('parameter_objects',
            [$*W.create_parameter($/, @p_params[0])]));
        add_signature_binding_code($p_past, $p_sig, @p_params);
        my $code := $*W.create_code_object($p_past, 'Sub', $p_sig, 1);
        $*W.apply_trait($/, '&trait_mod:<is>', $code, :onlystar(1));
        $*W.add_proto_to_sort($code);
        $code
    }

    my $SIG_ELEM_IS_COPY := 512;
    method add_inlining_info_if_possible($/, $code, $sig, $past, @params) {
        # Make sure the block has the common structure we expect
        # (decls then statements).
        return 0 unless +@($past) == 2;

        # Ensure all parameters are simple and build placeholders for
        # them.
        my $Param  := $*W.find_symbol(['Parameter'], :setting-only);
        my @p_objs := nqp::getattr($sig, $*W.find_symbol(['Signature'], :setting-only), '$!params');
        my int $i  := 0;
        my int $n  := nqp::elems(@params);
        my %arg_placeholders;
        while $i < $n {
            my %info      := @params[$i];
            my $param_obj := @p_objs[$i];
            return 0 if %info<optional> || %info<is_capture> || %info<pos_slurpy> ||
                %info<named_slurpy> || %info<pos_lol> || %info<pos_onearg> || %info<bind_attr> ||
                %info<bind_accessor> || %info<nominal_generic> || %info<named_names> ||
                %info<type_captures> || %info<post_constraints>;
            my int $flags := nqp::getattr_i($param_obj, $Param, '$!flags');
            return 0 if $flags +& $SIG_ELEM_IS_COPY;
            %arg_placeholders{%info<variable_name>} :=
                QAST::InlinePlaceholder.new( :position($i) );
            $i++;
        }

        # Ensure nothing extra is declared and there are no inner blocks.
        for @($past[0]) {
            if nqp::istype($_, QAST::Var) && $_.scope eq 'lexical' {
                my $name := $_.name;
                return 0 if $name ne '$*DISPATCHER' && $name ne '$_' &&
                    $name ne '$/' && $name ne '$¢' && $name ne '$!' &&
                    !nqp::existskey(%arg_placeholders, $name);
            }
            elsif nqp::istype($_, QAST::Block) {
                return 0;
            }
            elsif nqp::istype($_, QAST::Stmt) && nqp::istype($_[0], QAST::Block) {
                return 0;
            }
        }

        # If all is well, we try to build the QAST for inlining. This dies
        # if we fail.
        my $PseudoStash;
        try $PseudoStash := $*W.find_symbol(['PseudoStash']);
        sub clear_node($qast) {
            $qast.node(nqp::null());
            $qast.clear_annotations();
            $qast
        }
        sub node_walker($node) {
            # Simple values are always fine; just return them as they are, modulo
            # removing any :node(...).
            if nqp::istype($node, QAST::IVal) || nqp::istype($node, QAST::SVal)
            || nqp::istype($node, QAST::NVal) {
                return $node.node ?? clear_node($node.shallow_clone()) !! $node;
            }

            # WVal is OK, though special case for PseudoStash usage (which means
            # we are doing funny lookup stuff).
            elsif nqp::istype($node, QAST::WVal) {
                if $node.value =:= $PseudoStash {
                    nqp::die("Routines using pseudo-stashes are not inlinable");
                }
                else {
                    return $node.node ?? clear_node($node.shallow_clone()) !! $node;
                }
            }

            # Operations need checking for their inlinability. If they are OK in
            # themselves, it comes down to the children.
            elsif nqp::istype($node, QAST::Op) {
                if nqp::getcomp('QAST').operations.is_inlinable('perl6', $node.op) {
                    my $replacement := $node.shallow_clone();
                    my int $i := 0;
                    my int $n := +@($node);
                    while $i < $n {
                        $replacement[$i] := node_walker($node[$i]);
                        $i := $i + 1;
                    }
                    return clear_node($replacement);
                }
                else {
                    nqp::die("Non-inlinable op '" ~ $node.op ~ "' encountered");
                }
            }

            # Variables are fine *if* they are arguments.
            elsif nqp::istype($node, QAST::Var) && ($node.scope eq 'lexical' || $node.scope eq '') {
                if nqp::existskey(%arg_placeholders, $node.name) {
                    my $replacement := %arg_placeholders{$node.name};
                    if $node.named || $node.flat {
                        $replacement := $replacement.shallow_clone();
                        if $node.named { $replacement.named($node.named) }
                        if $node.flat { $replacement.flat($node.flat) }
                    }
                    return $replacement;
                }
                else {
                    nqp::die("Cannot inline with non-argument variables");
                }
            }

            # Statements need to be cloned and then each of the nodes below them
            # visited.
            elsif nqp::istype($node, QAST::Stmt) || nqp::istype($node, QAST::Stmts) {
                my $replacement := $node.shallow_clone();
                my int $i := 0;
                my int $n := +@($node);
                while $i < $n {
                    $replacement[$i] := node_walker($node[$i]);
                    $i := $i + 1;
                }
                return clear_node($replacement);
            }

            # Want nodes need copying and every other child visiting.
            elsif nqp::istype($node, QAST::Want) {
                my $replacement := $node.shallow_clone();
                my int $i := 0;
                my int $n := +@($node);
                while $i < $n {
                    $replacement[$i] := node_walker($node[$i]);
                    $i := $i + 2;
                }
                return clear_node($replacement);
            }

            # Otherwise, we don't know what to do with it.
            else {
                nqp::die("Unhandled node type; won't inline");
            }
        };
        my $inline_info;
        try $inline_info := node_walker($past[1]);
        return 0 unless nqp::istype($inline_info, QAST::Node);

        # Attach inlining information.
        $*W.apply_trait($/, '&trait_mod:<is>', $code, inlinable => $inline_info)
    }

    method method_def($/) {
        my $past;
        if $<onlystar> {
            $past := $<onlystar>.ast;
        }
        else {
            $past := $<blockoid>.ast;
            if $past.ann('placeholder_sig') {
                $/.PRECURSOR.panic('Placeholder variables cannot be used in a method');
            }
            $past[1] := wrap_return_handler($past[1]);
        }
        $past.blocktype('declaration_static');

        my $name;
        if $<longname> -> $ln {
            if $ln<colonpair> {
                $name := ~$ln<name>;
                for $ln<colonpair> {
                    my $key := $_<identifier> || '';
                    if $_<coloncircumfix> -> $cf {
                        if $cf<circumfix> -> $op_name {
                            $name := $name ~ $*W.canonicalize_pair($key, $*W.colonpair_nibble_to_str(
                                $ln, $op_name<nibble> // $op_name<semilist> // $op_name<pblock>));
                        }
                        else {
                            $name := $name ~ ':' ~ $key;
                        }
                    }
                    else {
                        $name := $name ~ ':' ~ $key;
                    }
                }
            }
            else {
                my $longname := $*W.dissect_longname($<longname>);
                $name := $longname.name(:dba('method name'), :decl<routine>);
            }
        }
        elsif $<sigil> {
            if $<sigil> eq '@'    { $name := 'postcircumfix:<[ ]>' }
            elsif $<sigil> eq '%' { $name := 'postcircumfix:<{ }>' }
            elsif $<sigil> eq '&' { $name := 'postcircumfix:<( )>' }
            else {
                $/.PRECURSOR.panic("Cannot use " ~ $<sigil> ~ " sigil as a method name");
            }
        }
        $past.name($name ?? $name !! '<anon>');

        my $code := methodize_block($/, $*DECLARAND, $past, $*SIG_OBJ,
            %*SIG_INFO, :yada(is_yada($/)));

        # If it's a proto but not an onlystar, need some variables for the
        # {*} implementation to use.
        if $*MULTINESS eq 'proto' && !$<onlystar> {
            # Also stash the current lexical dispatcher and capture, for the {*}
            # to resolve.
            $past[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('savecapture') )
            ));
            $past[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
            ));
        }

        # attach return type
        if $*OFTYPE {
            my $sig := $code.signature;
            if $sig.has_returns {
                my $prev_returns := $sig.returns;
                $*W.throw($*OFTYPE, 'X::Redeclaration',
                    what    => 'return type for',
                    symbol  => $code.name,
                    postfix => " (previous return type was "
                                ~ $prev_returns.HOW.name($prev_returns)
                                ~ ')',
                );
            }
            $sig.set_returns($*OFTYPE.ast);
        }


        # Document it
        Perl6::Pod::document($/, $code, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($code);
        }

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*W.cur_lexpad();
        $outer[0].push($past);

        # Apply traits.
        $*W.apply_traits($<trait>, $code);
        if $<onlystar> {
            $*W.apply_trait($/, '&trait_mod:<is>', $*DECLARAND, :onlystar(1));
        }
        $*W.add_phasers_handling_code($code, $past);

        # Install method.
        if $name {
            my $meta := $<specials> && ~$<specials> eq '^';
            install_method($/, $name, $*SCOPE, $code, $outer, :$meta,
                :private($<specials> && ~$<specials> eq '!'));
        }
        elsif $*MULTINESS {
            $*W.throw($/, 'X::Anon::Multi',
                multiness       => $*MULTINESS,
                routine-type    => 'method',
            );
        }

        # If it's a proto, add it to the sort-at-CHECK-time queue.
        if $*MULTINESS eq 'proto' {
            $*W.add_proto_to_sort($code);
        }

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure.annotate('sink_ast', QAST::Op.new( :op('null') ));
        make $closure;
    }

    method macro_def($/) {
        my $block;

        $block := $<blockoid>.ast;
        $block.blocktype('declaration_static');
        $block[1] := wrap_return_handler($block[1]);

        # Obtain parameters, create signature object and generate code to
        # call binder.
        if $block.ann('placeholder_sig') && $<multisig> {
            $*W.throw($/, 'X::Signature::Placeholder',
                precursor => '1',
                placeholder => $block.ann('placeholder_sig')[0]<placeholder>,
            );
        }
        my %sig_info;
        if $<multisig> {
            %sig_info := $<multisig>.ast;
        }
        else {
            %sig_info<parameters> := $block.ann('placeholder_sig') ?? $block.ann('placeholder_sig') !!
                                                                [];
        }
        my @params := %sig_info<parameters>;
        my $signature := $*W.create_signature_and_params($<multisig> ?? $<multisig> !! $/,
            %sig_info, $block, 'Any');
        add_signature_binding_code($block, $signature, @params);

        # Finish code object, associating it with the routine body.
        if $<deflongname> {
            $block.name(~$<deflongname>.ast);
        }
        my $code := $*DECLARAND;
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $block, $*MULTINESS eq 'proto');

        # Document it
        Perl6::Pod::document($/, $code, $*POD_BLOCK, :leading);

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*W.cur_lexpad();
        $outer[0].push(QAST::Stmt.new($block));

        if $<deflongname> {
            my $name := '&' ~ ~$<deflongname>.ast;
            # Install.
            if $outer.symbol($name) {
                $/.PRECURSOR.panic("Illegal redeclaration of macro '" ~
                    ~$<deflongname>.ast ~ "'");
            }
            if $*SCOPE eq '' || $*SCOPE eq 'my' {
                $*W.install_lexical_symbol($outer, $name, $code);
            }
            elsif $*SCOPE eq 'our' {
                # Install in lexpad and in package, and set up code to
                # re-bind it per invocation of its outer.
                $*W.install_lexical_symbol($outer, $name, $code);
                $*W.install_package_symbol($/, $*PACKAGE, $name, $code, 'macro');
                $outer[0].push(QAST::Op.new(
                    :op('bind'),
                    $*W.symbol_lookup([$name], $/, :package_only(1)),
                    QAST::Var.new( :name($name), :scope('lexical') )
                ));
            }
            else {
                $/.PRECURSOR.panic("Cannot use '$*SCOPE' scope with a macro");
            }
        }
        elsif $*MULTINESS {
            $/.PRECURSOR.panic('Cannot put ' ~ $*MULTINESS ~ ' on anonymous macro');
        }

        # Apply traits.
        $*W.apply_traits($<trait>, $code);
        $*W.add_phasers_handling_code($code, $block);

        my $closure := block_closure(reference_to_code_object($code, $block));
        $closure.annotate('sink_ast', QAST::Op.new( :op('null') ));
        make $closure;
    }

    sub methodize_block($/, $code, $past, $signature, %sig_info, :$yada) {
        # Add signature binding code.
        add_signature_binding_code($past, $signature, %sig_info<parameters>);

        # Place to store invocant.
        $past[0].unshift(QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') ));
        $past.symbol('self', :scope('lexical'));

        # Needs a slot to hold a multi or method dispatcher.
        $*W.install_lexical_symbol($past, '$*DISPATCHER',
            $*W.find_symbol([$*MULTINESS eq 'multi' ?? 'MultiDispatcher' !! 'MethodDispatcher']));
        $past[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));

        # Finish up code object.
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $past, $*MULTINESS eq 'proto', :yada($yada));
        $*W.add_phasers_handling_code($code, $past);
        $code
    }

    # Installs a method into the various places it needs to go.
    sub install_method($/, $name, $scope, $code, $outer, :$private, :$meta, :$gen-accessor) {
        my $meta_meth;
        if $private {
            if $*MULTINESS { $/.PRECURSOR.panic("Private multi-methods are not supported"); }
            $meta_meth := 'add_private_method';
        }
        elsif $meta {
            if $*MULTINESS { $/.PRECURSOR.panic("Meta multi-methods are not supported"); }
            $meta_meth := 'add_meta_method';
        }
        else {
            $meta_meth := $*MULTINESS eq 'multi' ?? 'add_multi_method' !! 'add_method';
        }
        if $scope eq '' || $scope eq 'has' {
            # Ensure that current package supports methods, and if so
            # add the method.
            if nqp::can($*PACKAGE.HOW, $meta_meth) {
                $*W.pkg_add_method($/, $*PACKAGE, $meta_meth, $name, $code);
            }
            elsif $gen-accessor {
                $/.PRECURSOR.worry("Useless generation of accessor method in " ~
                    ($*PKGDECL || "mainline"));
            }
            else {
                my $nocando := $*MULTINESS eq 'multi' ?? 'multi-method' !! 'method';
                $/.PRECURSOR.worry(
                    "Useless declaration of a has-scoped $nocando in " ~
                    ($*PKGDECL || "mainline") ~ " (did you mean 'my $*METHODTYPE $name'?)");
            }
        }
        elsif $scope eq 'my' {
            my $mang-name := '&' ~ $name;
            if $outer.symbol($mang-name) {
                $*W.throw($/, ['X', 'Redeclaration'], symbol => $name, what => 'method');
            }
            $*W.install_lexical_symbol($outer, $mang-name, $code, :clone(1));
        }
        elsif $scope eq 'our' {
            my $mang-name := '&' ~ $name;
            if $outer.symbol($mang-name) {
                $*W.throw($/, ['X', 'Redeclaration'], symbol => $name, what => 'method');
            }
            $*W.install_lexical_symbol($outer, $mang-name, $code, :clone(1));
            my $package := $*PACKAGE;
            if nqp::existskey($package.WHO, $name) {
                $*W.throw($/, ['X', 'Redeclaration'],
                    symbol  => $name,
                    what    => 'method',
                    postfix => ' (already defined in package ' ~ $package.HOW.name($package) ~ ')'
                );
            }
            $*W.install_package_symbol($/, $package, '&' ~ $name, $code, 'method');
        }
    }

    sub is_clearly_returnless($block) {
        sub returnless_past($past) {
            return 0 unless
                # It's a simple operation.
                nqp::istype($past, QAST::Op)
                    && nqp::getcomp('QAST').operations.is_inlinable('perl6', $past.op) ||
                # Just a variable lookup.
                nqp::istype($past, QAST::Var) ||
                # Just a QAST::Want
                nqp::istype($past, QAST::Want) ||
                # Just a primitive or world value.
                nqp::istype($past, QAST::WVal) ||
                nqp::istype($past, QAST::IVal) ||
                nqp::istype($past, QAST::NVal) ||
                nqp::istype($past, QAST::SVal);
            for @($past) {
                if nqp::istype($_, QAST::Node) {
                    if !returnless_past($_) {
                        return 0;
                    }
                }
            }
            1;
        }

        # Only analyse things with a single simple statement.
        if +$block[1].list == 1 && nqp::istype($block[1][0], QAST::Stmt) && +$block[1][0].list == 1 {
            # Ensure there's no nested blocks.
            for @($block[0]) {
                if nqp::istype($_, QAST::Block) { return 0; }
                if nqp::istype($_, QAST::Stmts) {
                    for @($_) {
                        if nqp::istype($_, QAST::Block) { return 0; }
                    }
                }
            }

            # Ensure that the PAST is whitelisted things.
            returnless_past($block[1][0][0])
        }
        elsif +$block[1].list == 1 && nqp::istype($block[1][0], QAST::WVal) {
            1
        }
        else {
            0
        }
    }

    sub is_yada($/) {
        if $<blockoid><statementlist> && +$<blockoid><statementlist><statement> == 1 {
            my $btxt := ~$<blockoid><statementlist><statement>[0];
            if $btxt ~~ /^ \s* ['...'|'???'|'!!!'|'…'] \s* $/ {
                return 1;
            }
        }
        0
    }

    method onlystar($/) {
        my $BLOCK := $*CURPAD;

        # Remove special variables; no need for them in onlystar.
        my int $i := 0;
        my int $n := +@($BLOCK[0]);
        while $i < $n {
            my $consider := $BLOCK[0][$i];
            if nqp::istype($consider, QAST::Var) {
                my $name := $consider.name;
                if $name eq '$_' || $name eq '$/' || $name eq '$!' || $name eq '$¢' {
                    $BLOCK[0][$i] := QAST::Op.new( :op('null') );
                }
            }
            $i++;
        }

        # Add dispatching code.
        $BLOCK.push(QAST::Op.new(
            :op('invokewithcapture'),
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new(
                    :op('multicachefind'),
                    QAST::Var.new(
                        :name('$!dispatch_cache'), :scope('attribute'),
                        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                        QAST::WVal.new( :value($*W.find_symbol(['Routine'], :setting-only)) ),
                    ),
                    QAST::Op.new( :op('usecapture') )
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('find_best_dispatchee'),
                    QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                    QAST::Op.new( :op('savecapture') )
                )
            ),
            QAST::Op.new( :op('usecapture') )
        ));
        $BLOCK.node($/);
        $BLOCK.is_thunk(1);
        make $BLOCK;
    }

    method regex_declarator:sym<regex>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<token>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<rule>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_def($/) {
        my $coderef;
        my $name := ~%*RX<name>;
        $*CURPAD.blocktype('declaration_static');
        my %sig_info := $<signature> ?? $<signature>[0].ast !! hash(parameters => []);

        if $*MULTINESS eq 'proto' {
            unless $<onlystar> {
                $/.PRECURSOR.panic("Proto regex body must be \{*\} (or <*> or <...>, which are deprecated)");
            }
            my $proto_body := QAST::Op.new(
                :op('callmethod'), :name('!protoregex'),
                QAST::Var.new( :name('self'), :scope('local') ),
                QAST::SVal.new( :value($name) ));
            $coderef := regex_coderef($/, $*DECLARAND, $proto_body, $*SCOPE, $name, %sig_info, $*CURPAD, $<trait>, :proto(1));
        } elsif $<nibble>.ast {
            $coderef := regex_coderef($/, $*DECLARAND, $<nibble>.ast, $*SCOPE, $name, %sig_info, $*CURPAD, $<trait>);
        }
        else {
            $/.CURSOR.typed_panic("X::Syntax::Regex::NullRegex");
        }

        # Document it
        Perl6::Pod::document($/, $*DECLARAND, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($*DECLARAND);
        }

        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure.annotate('sink_ast', QAST::Op.new( :op('null') ));
        make $closure;
    }

    sub regex_coderef($/, $code, $qast, $scope, $name, %sig_info, $block, $traits?, :$proto, :$use_outer_match) {
        # Regexes can't have place-holder signatures.
        if $qast.ann('placeholder_sig') {
            $/.PRECURSOR.panic('Placeholder variables cannot be used in a regex');
        }

        # Create a code reference from a regex qast tree
        my $past;
        if $proto {
            $block[1] := $qast;
            $past := $block;
        }
        else {
            $*W.install_lexical_magical($block, '$¢');
            unless $use_outer_match {
                $*W.install_lexical_magical($block, '$/');
            }
            $past := %*RX<P5>
                ?? %*LANG<P5Regex-actions>.qbuildsub($qast, $block, code_obj => $code)
                !! %*LANG<Regex-actions>.qbuildsub($qast, $block, code_obj => $code);
        }
        $past.name($name);
        $past.blocktype("declaration_static");

        # Install a $?REGEX (mostly for the benefit of <~~>).
        $block[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new(:name<$?REGEX>, :scope<lexical>, :decl('var')),
            QAST::Op.new(
                :op('getcodeobj'),
                QAST::Op.new( :op('curcode') )
            )));
        $block.symbol('$?REGEX', :scope<lexical>);

        # Do the various tasks to turn the block into a method code object.
        my $invocant_type := $*W.find_symbol([ # XXX Maybe Cursor below, not Mu...
            $name && $*SCOPE ne 'my' && $*SCOPE ne 'our' && $*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my $signature := $*W.create_signature_and_params($/, %sig_info, $past, 'Any',
            :method, :$invocant_type);
        methodize_block($/, $code, $past, $signature, %sig_info);

        # Need to put self into a register for the regex engine.
        $past[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name('self'), :scope('local'), :decl('var') ),
            QAST::Var.new( :name('self'), :scope('lexical') )));

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*W.cur_lexpad();
        $outer[0].push($past);

        # Apply traits.
        $*W.apply_traits($traits, $code) if $traits;

        # Install in needed scopes.
        install_method($/, $name, $scope, $code, $outer) if $name ne '';

        # Bind original source to $!source
        my $Regex  := $*W.find_symbol(['Regex'], :setting-only);
        my $source := ($*METHODTYPE ?? $*METHODTYPE ~ ' ' !! '') ~ $/;
        my $match  := $source ~~ /\s+$/;

        if $match {
            $source := nqp::substr($source, 0, $match.from());
        }
        nqp::bindattr($code, $Regex, '$!source', $source);

        # Return a reference to the code object
        reference_to_code_object($code, $past);
    }

    method type_declarator:sym<enum>($/) {
        # Get, or find, enumeration base type and create type object with
        # correct base type.
        my $longname   := $<longname> ?? $*W.dissect_longname($<longname>) !! 0;
        my $name       := $<longname> ?? $longname.name() !! $<variable><desigilname> || '';
        my @name_parts := $<longname> ?? $longname.type_name_parts('enum name', :decl(1)) !! [];

        my $type_obj;
        my sub make_type_obj($base_type) {
            $type_obj := $*W.pkg_create_mo($/, $*W.resolve_mo($/, 'enum'), :$name, :$base_type);
            # Add roles (which will provide the enum-related methods).
            $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['Enumeration']));
            if istype($type_obj, $*W.find_symbol(['Numeric'])) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['NumericEnumeration']));
            }
            if istype($type_obj, $*W.find_symbol(['Stringy'])) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['StringyEnumeration']));
            }
            # Apply traits, compose and install package.
            $*W.apply_traits($<trait>, $type_obj);
            $*W.pkg_compose($/, $type_obj);
        }
        my $base_type;
        my int $has_base_type := 0;
        if $*OFTYPE {
            $base_type     := $*OFTYPE.ast;
            $has_base_type := 1;
            make_type_obj($base_type);
        }

        if $<variable> {
            $*W.throw($/, 'X::Comp::NYI',
                feature => "Variable case of enums",
            );
        }

        # Get list of either values or pairs; fail if we can't.
        my $Pair := $*W.find_symbol(['Pair']);
        my @values;
        my $term_ast := $<term>.ast;

        # remove val call on a single item
        if $term_ast.isa(QAST::Op) && $term_ast.name eq '&val' {
            $term_ast := $term_ast[0];
        }

        if $term_ast.isa(QAST::Stmts) && +@($term_ast) == 1 {
            $term_ast := $term_ast[0];
        }
        if $term_ast.isa(QAST::Op) && $term_ast.name eq '&infix:<,>' {
            wantall($term_ast, 'enum');
            for @($term_ast) {
                my $item_ast := $_;
                if $item_ast.isa(QAST::Op) && $item_ast.name eq '&val' {
                    $item_ast := $item_ast[0];
                }

                if istype($item_ast.returns(), $Pair) && $item_ast[1].has_compile_time_value {
                    @values.push($item_ast);
                }
                elsif $item_ast.has_compile_time_value {
                    @values.push($item_ast);
                }
                else {
                    @values.push($*W.compile_time_evaluate($<term>, $item_ast));
                }
            }
        }
        elsif $term_ast.has_compile_time_value {
            @values.push($term_ast);
        }
        elsif istype($term_ast.returns, $Pair) && $term_ast[1].has_compile_time_value {
            @values.push($term_ast);
        }
        else {
            @values := $*W.compile_time_evaluate($<term>, $<term>.ast).List.FLATTENABLE_LIST;
        }

        # Now we have them, we can go about computing the value
        # for each of the keys, unless they have them supplied.
        # XXX Should not assume integers, and should use lexically
        # scoped &postfix:<++> or so.
        my $cur_value := nqp::box_i(-1, $*W.find_symbol(['Int']));
        my @redecl;
        my $block := $*W.cur_lexpad();
        for @values {
            # If it's a pair, take that as the value; also find
            # key.
            my $cur_key;
            if nqp::istype($_, QAST::Node) && istype($_.returns(), $Pair) {
                $cur_key := $_[1].compile_time_value;
                $cur_value := $*W.compile_time_evaluate($<term>, $_[2]);
                if $has_base_type {
                    unless istype($cur_value, $base_type) {
                        $/.CURSOR.panic("Type error in enum. Got '"
                                ~ $cur_value.HOW.name($cur_value)
                                ~ "' Expected: '"
                                ~ $base_type.HOW.name($base_type)
                                ~ "'"
                        );
                    }
                }
                else {
                    $base_type     :=  $cur_value.WHAT;
                    $has_base_type := 1;
                    make_type_obj($base_type);
                }
            }
            else {
                unless $has_base_type {
                    $base_type := $*W.find_symbol(['Int']);
                    make_type_obj($base_type);
                    $has_base_type := 1;
                }

                if nqp::istype($_, QAST::Node) {
                    $cur_key := $*W.compile_time_evaluate($<term>, $_);
                }
                else {
                    $cur_key := $_;
                }

                $cur_value := $cur_value.succ();
            }
            unless nqp::istype($cur_key, $*W.find_symbol(['Str'])) {
                $cur_key := $cur_key.Str;
            }
            unless nqp::defined($cur_value) {
                $*W.throw($/, 'X::Comp::NYI',
                    feature => "Using a type object as a value for an enum",
                );
            }

            # Create and install value.
            my $val_obj := $*W.create_enum_value($type_obj, $cur_key, $cur_value);
            $cur_key    := nqp::unbox_s($cur_key);
            $*W.install_package_symbol_unchecked($type_obj, $cur_key, $val_obj);
            if $block.symbol($cur_key) {
                nqp::push(@redecl, $cur_key);
                $*W.install_lexical_symbol($block, $cur_key,
                    $*W.find_symbol(['Failure']).new(
                        $*W.find_symbol(['X', 'PoisonedAlias'], :setting-only).new(
                            :alias($cur_key), :package-type<enum>, :package-name($name)
                        )
                    )
                );
            }
            else {
                $*W.install_lexical_symbol($block, $cur_key, $val_obj);
            }
            if $*SCOPE eq '' || $*SCOPE eq 'our' {
                $*W.install_package_symbol_unchecked($*PACKAGE, $cur_key, $val_obj);
            }
        }

        if +@redecl -> $amount {
            if $amount > 2 {
                @redecl[$amount - 2] := @redecl[$amount - 2] ~ ' and ' ~ nqp::pop(@redecl);
                $/.CURSOR.typed_worry('X::Redeclaration', symbol => nqp::join(', ', @redecl));
            }
            elsif $amount > 1 {
                $/.CURSOR.typed_worry('X::Redeclaration', symbol => nqp::join(' and ', @redecl));
            }
            else {
                $/.CURSOR.typed_worry('X::Redeclaration', symbol => @redecl[0]);
            }
        }


        # create a type object even for empty enums
        make_type_obj($*W.find_symbol(['Int'])) unless $has_base_type;

        $*W.install_package($/, @name_parts,
            ($*SCOPE || 'our'), 'enum', $*PACKAGE, $block, $type_obj);

        # Compose the added enum values.
        $type_obj.HOW.compose_values($type_obj);

        # Document it
        Perl6::Pod::document($/, $type_obj, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($type_obj);
        }
        # Set it up for trailing declarations
        $*PRECEDING_DECL := $type_obj;

        # We evaluate to the enums values, if we need to.
        my $ast := QAST::Op.new( :op('call'), :name('&ENUM_VALUES'), $term_ast );
        $ast.annotate('sink_ast', QAST::Op.new( :op('null') ));
        make $ast;
    }

    method type_declarator:sym<subset>($/) {
        # We refine Any by default; "of" may override.
        my $refinee := $*W.find_symbol([ $*OFTYPE // 'Any']);

        # If we have a refinement, make sure it's thunked if needed. If none,
        # just always true.
        my $refinement := make_where_block($<EXPR>, $<EXPR> ?? $<EXPR>.ast !!
            QAST::Op.new( :op('p6bool'), QAST::IVal.new( :value(1) ) ));

        # Create the meta-object.
        my $subset;
        my $longname := $<longname> && $*W.dissect_longname($<longname>);
        my @name := $longname ?? $longname.type_name_parts('subset name', :decl(1)) !! [];
        if @name {
            my $target_package := $longname.is_declared_in_global()
                ?? $*GLOBALish
                !! $*PACKAGE;
            my $fullname := $longname.fully_qualified_with($target_package);
            $subset := $*W.create_subset($*W.resolve_mo($/, 'subset'), $refinee, $refinement,
                :name($fullname));
            $*W.install_package($/, @name, ($*SCOPE || 'our'), 'subset',
                $target_package, $*W.cur_lexpad(), $subset);
        }
        else {
            $subset := $*W.create_subset($*W.resolve_mo($/, 'subset'), $refinee, $refinement);
        }

        # Apply traits.
        $*W.apply_traits($<trait>, $subset);

        # Document it
        Perl6::Pod::document($/, $subset, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($subset);
        }
        # Set it up for trailing declarations
        $*PRECEDING_DECL := $subset;

        # We evaluate to the refinement type object.
        make QAST::WVal.new( :value($subset) );
    }

    method type_declarator:sym<constant>($/) {
        my $value_ast := $<initializer>.ast;
        my $sigil := '';

        # Provided it's named, install it.
        my $name;
        if $<defterm> {
            $name := $<defterm>.ast;
        }
        elsif $<variable> {
            if $<variable><sigil> {
                $sigil := ~$<variable><sigil>;
                if $sigil eq '@' {
                    $value_ast := QAST::Op.new( :op<callmethod>, :name<cache>, $value_ast);
                }
            }
            if $<variable><twigil> {
                my $twigil := ~$<variable><twigil>;
                if $twigil eq '?' {
                    unless $*COMPILING_CORE_SETTING {
                        $*W.throw($/, 'X::Comp::NYI',
                          feature => "Constants with a '$twigil' twigil"
                        );
                    }
                }

                elsif $twigil eq '*' {
                    $*W.throw($/, 'X::Syntax::Variable::Twigil',
                      what       => 'constant',
                      twigil     => $twigil,
                      scope      => $*SCOPE,
                      additional => ' because dynamic constants are an oxymoron'
                    );
                }

                # Don't handle other twigil'd case yet.
                else {
                    $*W.throw($/, 'X::Comp::NYI',
                      feature => "Constants with a '$twigil' twigil");
                }
            }
            $name := ~$<variable>;
        }

        # Get constant value.
        my $type := $*W.find_symbol([ $*OFTYPE // 'Any']);
        if $<initializer><sym> eq '.=' {
            $value_ast.unshift(QAST::WVal.new(:value($type)));
        }
        $value_ast.returns($type);

        my $con_block := $*W.pop_lexpad();
        my $value;
        if $value_ast.has_compile_time_value {
            $value := $value_ast.compile_time_value;
        }
        else {
            $con_block.push($value_ast);
            my $value_thunk := $*W.create_simple_code_object($con_block, 'Block');
            $value := $*W.handle-begin-time-exceptions($/, 'evaluating a constant', $value_thunk);
            $*W.add_constant_folded_result($value);
        }
        if $sigil eq '%' {
            my $Associative := $*W.find_symbol(['Associative']);
            if !nqp::istype($value, $Associative) {
                $*W.throw($/, 'X::TypeCheck',
                    operation => "constant declaration of " ~ ~$<variable>,
                    expected => $Associative, got => $*W.find_symbol([$value.HOW.name($value)]) );
            }
        }

        if $name {
            my $cur_pad := $*W.cur_lexpad();
            if $cur_pad.symbol($name) {
                $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
            }

            $*W.install_package($/, [$name], ($*SCOPE || 'our'),
                'constant', $*PACKAGE, $cur_pad, $value);
        }
        for $<trait> {
            $_.ast.apply($value, :SYMBOL($name)) if $_.ast;
        }

        # Evaluate to the constant.
        make QAST::WVal.new( :value($value), :returns($type) );
    }

    method initializer:sym<=>($/) {
        make WANTED($<EXPR>.ast, 'init=');
    }
    method initializer:sym<:=>($/) {
        make WANTED($<EXPR>.ast, 'init:=');
    }
    method initializer:sym<::=>($/) {
        make WANTED($<EXPR>.ast, 'init::=');
    }
    method initializer:sym<.=>($/) {
        make WANTED($<dottyopish><term>.ast, 'init.=');
    }

    method initializer:sym<.>($/) {
        make WANTED($<dottyopish><term>.ast, 'init.');
    }

    method capterm($/) {
        my $past := $<termish>
            ?? QAST::Op.new( $<termish>.ast )
            !! $<semiarglist>.ast;
        wantall($past, 'capterm');
        $past.unshift(QAST::WVal.new( :value($*W.find_symbol(['Capture']) ) ));
        $past.op('callmethod');
        $past.name('from-args');
        make $past;
    }

    method multisig($/) {
        make $<signature>.ast;
    }

    method fakesignature($/) {
        my $fake_pad := $*W.pop_lexpad();
        my $sig := $*W.create_signature_and_params($/, $<signature>.ast,
            $fake_pad, 'Mu', :no_attr_check(1));

        %*PARAM_INFO<subsig_returns> := $sig.returns;
        $*W.cur_lexpad()[0].push($fake_pad);
        $*W.create_code_object($fake_pad, 'Block', $sig);

        make QAST::WVal.new( :value($sig) );
    }

    method signature($/) {
        # Fix up parameters with flags according to the separators.
        my %signature;
        my @parameter_infos;
        my int $param_idx := 0;
        for $<parameter> {
            my %info := $_.ast;
            my $sep := @*seps[$param_idx];
            if ~$sep eq ':' {
                if $param_idx != 0 {
                    $*W.throw($/, 'X::Syntax::Signature::InvocantMarker')
                }
                unless $*ALLOW_INVOCANT {
                    $*W.throw($/, 'X::Syntax::Signature::InvocantNotAllowed')
                }
                %info<is_invocant> := 1;
            }
            @parameter_infos.push(%info);
            $param_idx := $param_idx + 1;
        }
        %signature<parameters> := @parameter_infos;
        if $<typename> {
            %signature<returns> := $<typename>.ast;
        }
        elsif $<value> {
            %signature<returns> := $<value>.ast.compile_time_value;
        }

        # Mark current block as having a signature.
        $*W.mark_cur_lexpad_signatured();

        # Result is set of parameter descriptors.
        make %signature;
    }

    method parameter($/) {
        # If it's a defterm, need to do parameter setup here.
        if $<defterm> {
            my $name := $<defterm>.ast;
            %*PARAM_INFO<variable_name> := $name;
            %*PARAM_INFO<desigilname>   := $name;
            %*PARAM_INFO<sigil>         := '';
            self.declare_param($/, $name);
        }

        # Sanity checks.
        my $quant := $<quant>;
        if $<default_value> {
            my $name := %*PARAM_INFO<variable_name> // '';
            if $quant eq '*' {
                $/.CURSOR.typed_sorry('X::Parameter::Default', how => 'slurpy',
                            parameter => $name);
            }
            if $quant eq '!' {
                $/.CURSOR.typed_sorry('X::Parameter::Default', how => 'required',
                            parameter => $name);
            }
            my $val := WANTED($<default_value>[0].ast, 'parameter/def');
            if $val.has_compile_time_value {
                my $value := $val.compile_time_value;
                check_param_default_type($/, $value);
                %*PARAM_INFO<default_value> := $value;
                %*PARAM_INFO<default_is_literal> := 1;
            }
            else {
                my $maybe_code_obj := $val.ann('code_object');
                if nqp::isconcrete($maybe_code_obj) {
                    $val.annotate('past_block', WANTED($val.ann('past_block'), 'parameters'));
                    check_param_default_type($/, $maybe_code_obj);
                }
                %*PARAM_INFO<default_value> :=
                    $*W.create_thunk($<default_value>[0], $val);
            }
        }

        # Set up various flags.
        %*PARAM_INFO<pos_slurpy>   := $quant eq '*' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<pos_lol>      := $quant eq '**' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<named_slurpy> := $quant eq '*' && %*PARAM_INFO<sigil> eq '%';
        %*PARAM_INFO<optional>     := $quant eq '?' || $<default_value> || ($<named_param> && $quant ne '!');
        %*PARAM_INFO<is_raw>       := $quant eq '\\' || ($quant eq '+' && !%*PARAM_INFO<sigil>);
        %*PARAM_INFO<is_capture>   := $quant eq '|';
        %*PARAM_INFO<pos_onearg>   := $quant eq '+';

        # Stash any traits.
        %*PARAM_INFO<traits> := $<trait>;

        if $<type_constraint> {
            if %*PARAM_INFO<pos_slurpy> || %*PARAM_INFO<pos_lol> || %*PARAM_INFO<pos_onearg> {
                $/.CURSOR.typed_sorry('X::Parameter::TypedSlurpy', kind => 'positional');
            }
            elsif %*PARAM_INFO<named_slurpy> {
                $/.CURSOR.typed_sorry('X::Parameter::TypedSlurpy', kind => 'named');
            }
            elsif %*PARAM_INFO<sigil> eq '&' && nqp::existskey(%*PARAM_INFO, 'subsig_returns')
                    && !(%*PARAM_INFO<subsig_returns> =:= $*W.find_symbol(["Mu"])) {
                $/.CURSOR.'!fresh_highexpect'();
                $*W.throw($/, 'X::Redeclaration',
                    what    => 'return type for',
                    symbol  => $<param_var>.Str,
                    postfix => " (previous return type was "
                                ~ $<type_constraint>[0].Str
                                ~ ')',
                );
            }
        }

        # Result is the parameter info hash.
        make %*PARAM_INFO;
    }

    sub check_param_default_type($/, $value) {
        if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
            my $expected := %*PARAM_INFO<nominal_type>;
            if nqp::objprimspec($expected) == 0 {
                unless nqp::istype($value, $expected) {
                    # Ensure both types are composed before complaining,
                    # or we give spurious errors on stubbed things or
                    # things we're in the middle of compiling.
                    my $got_comp := try $value.HOW.is_composed($value);
                    my $exp_comp := try $expected.HOW.is_composed($expected);
                    if $got_comp && $exp_comp {
                        $<default_value>[0].CURSOR.typed_sorry(
                            'X::Parameter::Default::TypeCheck',
                            got => $value,
                            expected => %*PARAM_INFO<nominal_type>);
                    }
                }
            }
        }
    }

    method param_var($/) {
        if $<signature> {
            if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
            }
            %*PARAM_INFO<sub_signature_params> := $<signature>.ast;
            if nqp::eqat(~$/, '[', 0) {
                %*PARAM_INFO<sigil> := '@';
                %*PARAM_INFO<nominal_type> := $*W.find_symbol(['Positional']);
            }
        }
        else {
            # Set name, if there is one.
            if $<name> {
                %*PARAM_INFO<variable_name> := ~$/;
                %*PARAM_INFO<desigilname> := ~$<name>;
            }
            %*PARAM_INFO<sigil> := my $sigil := ~$<sigil>;

            # Depending on sigil, use appropriate role.
            my int $need_role;
            my $role_type;
            if $sigil eq '@' {
                $role_type := $*W.find_symbol(['Positional']);
                $need_role := 1;
            }
            elsif $sigil eq '%' {
                $role_type := $*W.find_symbol(['Associative']);
                $need_role := 1;
            }
            elsif $sigil eq '&' {
                $role_type := $*W.find_symbol(['Callable']);
                $need_role := 1;
            }
            if $need_role {
                if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                    %*PARAM_INFO<nominal_type> := $*W.parameterize_type_with_args($/,
                        $role_type, [%*PARAM_INFO<nominal_type>], nqp::hash());
                }
                else {
                    %*PARAM_INFO<nominal_type> := $role_type;
                }
            }

            # Handle twigil.
            my $twigil := $<twigil> ?? ~$<twigil> !! '';
            %*PARAM_INFO<twigil> := $twigil;
            if $twigil eq '' || $twigil eq '*' {
                # Need to add the name.
                if $<name> {
                    my $name := ~$/;
                    if $<name><sigterm> {
                        $name := nqp::substr($name, 0, nqp::chars($name) - nqp::chars(~$<name><sigterm>));
                        %*PARAM_INFO<variable_name> := $name;
                    }
                    self.declare_param($/, $name);
                }
            }
            elsif $twigil eq '!' {
                if !$*HAS_SELF && $*SURROUNDING_DECL ne 'variable' {
                    $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => ~$/);
                }
                %*PARAM_INFO<bind_attr> := 1;
                my int $succ := 1;
                try {
                    %*PARAM_INFO<attr_package> := $*W.find_symbol(['$?CLASS']);
                    CATCH {
                        $succ := 0;
                    }
                }
                unless $succ {
                    $/.CURSOR.panic('cannot use a $! parameter in a signature where no $?CLASS is available');
                }
            }
            elsif $twigil eq '.' {
                if $*SURROUNDING_DECL ne 'variable' {
                    if !$*HAS_SELF {
                        $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => ~$/);
                    }
                    elsif $*HAS_SELF eq 'partial' {
                        $*W.throw($/, ['X', 'Syntax', 'VirtualCall'], call => ~$/);
                    }
                }
                %*PARAM_INFO<bind_accessor> := 1;
                if $<name> {
                    %*PARAM_INFO<variable_name> := ~$<name>;
                }
                else {
                    $/.CURSOR.panic("Cannot declare $. parameter in signature without an accessor name");
                }
            }
            else {
                if $twigil eq ':' {
                    $/.CURSOR.typed_sorry('X::Parameter::Placeholder',
                        parameter => ~$/,
                        right     => ':' ~ $<sigil> ~ ~$<name>,
                    );
                }
                else {
                    $/.CURSOR.typed_sorry('X::Parameter::Twigil',
                        parameter => ~$/,
                        twigil    => $twigil,
                    );
                }
            }
        }
        # Handle leading declarative docs
        if $*DECLARATOR_DOCS ne '' {
            %*PARAM_INFO<docs> := $*POD_BLOCK;
            $*DECLARATOR_DOCS  := '';
        }

        # Attach the dummy param we set up in Grammar::param_var to PARAM_INFO,
        # so we can access it later on.  The dummy param may have goodies like
        # trailing docs!
        my $par_type := $*W.find_symbol(['Parameter'], :setting-only);
        if nqp::istype($*PRECEDING_DECL, $par_type) {
            %*PARAM_INFO<dummy> := $*PRECEDING_DECL;
        }

        if $<name><sigterm> {
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            my $get_signature_past := QAST::Op.new(
                :op('callmethod'),
                :name('signature'),
                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'param_var')
            );
            my $fakesig := $<name><sigterm><fakesignature>;
            my $closure_signature := $fakesig.ast;

            my $where := make_where_block($fakesig, $closure_signature, $get_signature_past);
            %*PARAM_INFO<post_constraints>.push($where);
        }

        if $<arrayshape> {
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            my $where := make_where_block($<arrayshape>,
                QAST::Op.new(
                    :op('callmethod'), :name('list'),
                    $<arrayshape>.ast),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('can'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'param_var'),
                        QAST::SVal.new( :value('shape') )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('shape'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'param_var')
                    )));
            %*PARAM_INFO<post_constraints>.push($where);
        }
    }

    method declare_param($/, $name) {
        my $cur_pad := $*W.cur_lexpad();
        if $cur_pad.symbol($name) {
            $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
        }
        if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
            $cur_pad[0].push(QAST::Var.new( :$name, :scope('lexical'),
                :decl('var'), :returns(%*PARAM_INFO<nominal_type>) ));
            %*PARAM_INFO<container_descriptor> := $*W.create_container_descriptor(
                %*PARAM_INFO<nominal_type>, 0, %*PARAM_INFO<variable_name>);
            $cur_pad.symbol(%*PARAM_INFO<variable_name>, :descriptor(%*PARAM_INFO<container_descriptor>),
                :type(%*PARAM_INFO<nominal_type>));
        } else {
            $cur_pad[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        }
        $cur_pad.symbol($name, :scope('lexical'));
    }

    method named_param($/) {
        %*PARAM_INFO<named_names> := %*PARAM_INFO<named_names> || [];
        if $<name>               { %*PARAM_INFO<named_names>.push(~$<name>); }
        elsif $<param_var><name> { %*PARAM_INFO<named_names>.push(~$<param_var><name>); }
        else                     { %*PARAM_INFO<named_names>.push(''); }
    }

    method default_value($/) {
        make WANTED($<EXPR>.ast, 'default_value');
    }

    method type_constraint($/) {
        if $<typename> {
            my str $typename := ~$<typename>;
            if nqp::eqat($typename, '::', 0) && !nqp::eqat($typename, '?', 2) {
                # Set up signature so it will find the typename.
                my $desigilname := nqp::substr($typename, 2);
                unless %*PARAM_INFO<type_captures> {
                    %*PARAM_INFO<type_captures> := []
                }
                %*PARAM_INFO<type_captures>.push($desigilname);

                # Install type variable in the static lexpad. Of course,
                # we'll find the real thing at runtime, but in the static
                # view it's a type variable to be reified.
                $*W.install_lexical_symbol($*W.cur_lexpad(), $desigilname,
                    $<typename>.ast);
            }
            else {
                if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                    $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                    );
                }
                my $type := $<typename>.ast;
                if nqp::isconcrete($type) {
                    # Actual a value that parses type-ish.
                    %*PARAM_INFO<nominal_type> := $type.WHAT;
                    unless %*PARAM_INFO<post_constraints> {
                        %*PARAM_INFO<post_constraints> := [];
                    }
                    %*PARAM_INFO<post_constraints>.push($type);
                }
                elsif $type.HOW.archetypes.nominal {
                    %*PARAM_INFO<nominal_type> := $type;
                }
                elsif $type.HOW.archetypes.coercive {
                    %*PARAM_INFO<nominal_type> := $type.HOW.constraint_type($type);
                    %*PARAM_INFO<coerce_type>  := $type.HOW.target_type($type);
                }
                elsif $type.HOW.archetypes.definite {
                    %*PARAM_INFO<nominal_type> := $type.HOW.base_type($type);
                }
                elsif $type.HOW.archetypes.generic {
                    %*PARAM_INFO<nominal_type> := $type;
                    %*PARAM_INFO<nominal_generic> := 1;
                }
                elsif $type.HOW.archetypes.nominalizable {
                    my $nom := $type.HOW.nominalize($type);
                    %*PARAM_INFO<nominal_type> := $nom;
                    unless %*PARAM_INFO<post_constraints> {
                        %*PARAM_INFO<post_constraints> := [];
                    }
                    %*PARAM_INFO<post_constraints>.push($type);
                }
                else {
                    $<typename>.CURSOR.typed_sorry('X::Parameter::BadType', :$type);
                }
                %*PARAM_INFO<of_type> := %*PARAM_INFO<nominal_type>;
                %*PARAM_INFO<of_type_match> := $<typename>;
                %*PARAM_INFO<defined_only>   := 1 if $<typename><colonpairs><D>;
                %*PARAM_INFO<undefined_only> := 1 if $<typename><colonpairs><U>;
            }
        }
        elsif $<value> {
            if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                );
            }
            my $ast := wanted($<value>.ast, 'type_constraint');
            my $val;
            if nqp::can($ast,'has_compile_time_value') && $ast.has_compile_time_value {
                $val := $ast.compile_time_value;
            }
            else {  # for negatives
                my $i  := $*W.add_numeric_constant(NQPMu, 'Int', +$<value>.Str);
                $val := $i.compile_time_value;
            }
            %*PARAM_INFO<nominal_type> := $val.WHAT;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push($val);
        }
        else {
            $/.CURSOR.panic('Cannot do non-typename cases of type_constraint yet');
        }
    }

    method post_constraint($/) {
        if $*CONSTRAINT_USAGE eq 'param' {
            if $<signature> {
                if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                    $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
                }
                %*PARAM_INFO<sub_signature_params> := $<signature>.ast;
                if nqp::eqat(~$/, '[', 0) {
                    %*PARAM_INFO<sigil> := '@' unless %*PARAM_INFO<sigil>;
                }
            }
            else {
                unless %*PARAM_INFO<post_constraints> {
                    %*PARAM_INFO<post_constraints> := [];
                }
                %*PARAM_INFO<post_constraints>.push(make_where_block($<EXPR>, $<EXPR>.ast));
            }
        }
        else {
            if $<signature> {
                $/.CURSOR.NYI('Signatures as constraints on variables');
            }
            else {
                make make_where_block($<EXPR>, $<EXPR>.ast);
            }
        }
    }

    method trait($/) {
        make $<trait_mod> ?? $<trait_mod>.ast !! $<colonpair>.ast;
    }

    my class Trait {
        has $!match;
        has str $!trait_mod;
        has @!pos_args;
        has %!named_args;

        method new($match, str $trait_mod, *@pos_args, *%named_args) {
            my $self := nqp::create(self);
            nqp::bindattr($self, Trait, '$!match', $match);
            nqp::bindattr_s($self, Trait, '$!trait_mod', $trait_mod);
            nqp::bindattr($self, Trait, '@!pos_args', @pos_args);
            nqp::bindattr($self, Trait, '%!named_args', %named_args);
            $self
        }

        method apply($declarand, *%additional) {
            $*W.apply_trait($!match, $!trait_mod, $declarand, |@!pos_args,
                |%!named_args, |%additional);
        }

        method mod() { $!trait_mod }
        method args() { @!pos_args }
    }

    method trait_mod:sym<is>($/) {
        # Handle is repr specially.
        if ~$<longname> eq 'repr' {
            if $<circumfix> {
                if nqp::istype($<circumfix>[0].ast, QAST::WVal) {
                    $*REPR := compile_time_value_str($<circumfix>[0].ast, "is repr(...) trait", $/);
                } else {
                    $*REPR := compile_time_value_str($<circumfix>[0].ast[0], "is repr(...) trait", $/);
                }
            }
            else {
                $/.CURSOR.panic("is repr(...) trait needs a parameter");
            }
        }
        else
        {
            # If we have an argument, get its compile time value or
            # evaluate it to get that.
            my @trait_arg;
            if $<circumfix> {
                my $arg := WANTED($<circumfix>[0].ast, 'is');
                if nqp::istype($arg, QAST::Want) {
                    $arg := $arg[0];
                }

                @trait_arg[0] := $arg.has_compile_time_value ??
                    $arg.compile_time_value !!
                    $*W.create_thunk($/, $<circumfix>[0].ast)();
            }

            # If we have a type name then we need to dispatch with that type; otherwise
            # we need to dispatch with it as a named argument.
            my @name := $*W.dissect_longname($<longname>).components();
            if $*W.is_name(@name) {
                my $trait := $*W.find_symbol(@name);
                make Trait.new($/, '&trait_mod:<is>', $trait, |@trait_arg);
            }
            else {
                my %arg;
                %arg{~$<longname>} := @trait_arg ?? @trait_arg[0] !!
                    $*W.find_symbol(['Bool', 'True']);
                make Trait.new($/, '&trait_mod:<is>', |%arg);
            }
        }
    }

    method trait_mod:sym<hides>($/) {
        make Trait.new($/, '&trait_mod:<hides>', $<typename>.ast);
    }

    method trait_mod:sym<does>($/) {
        make Trait.new($/, '&trait_mod:<does>', $<typename>.ast);
    }

    method trait_mod:sym<will>($/) {
        my %arg;
        %arg{~$<identifier>} := ($*W.add_constant('Int', 'int', 1)).compile_time_value;
        make Trait.new($/, '&trait_mod:<will>', ($<pblock>.ast).ann('code_object'), |%arg);
    }

    method trait_mod:sym<of>($/) {
        make Trait.new($/, '&trait_mod:<of>', $<typename>.ast);
    }

    method trait_mod:sym<returns>($/) {
        make Trait.new($/, '&trait_mod:<returns>', $<typename>.ast);
    }

    method trait_mod:sym<handles>($/) {
        # The term may be fairly complex. Thus we make it into a thunk
        # which the trait handler can use to get the term and work with
        # it.
        my $thunk := $*W.create_thunk($/, WANTED($<term>.ast, 'handles'));
        make Trait.new($/, '&trait_mod:<handles>', $thunk);
    }

    method postop($/) {
        if $<postfix> {
            make $<postfix>.ast
                 || QAST::Op.new( :name('&postfix' ~ $*W.canonicalize_pair('', $<postfix>.Str)), :op<call> )
        } else {
            make $<postcircumfix>.ast
                 || QAST::Op.new( :name('&postcircumfix' ~ $*W.canonicalize_pair('', $<postcircumfix>.Str)), :op<call> );
        }
    }

    method dotty:sym<.>($/) { make $<dottyop>.ast; }

    method dotty:sym<.*>($/) {
        my $past := $<dottyop>.ast;
        unless $past.isa(QAST::Op) && $past.op() eq 'callmethod' {
            $/.CURSOR.panic("Cannot use " ~ $<sym>.Str ~ " on a non-identifier method call");
        }
        if $<sym> eq '.^' {
            $past.op('p6callmethodhow');
        }
        else {
            $past.unshift($*W.add_string_constant($past.name))
                if $past.name ne '';
            $past.name('dispatch' ~ $*W.canonicalize_pair('', ~$<sym>));
        }
        make $past;
    }

    method dottyop($/) {
        if $<methodop> {
            make $<methodop>.ast;
        }
        elsif $<postop> {
            make $<postop>.ast;
        }
        else {
            if $<colonpair><identifier> eq "" && $<colonpair><coloncircumfix> -> $cf {
                if $cf<circumfix> -> $op_name {
                    make QAST::Op.new( :op<call>, :node($/),
                    :name('&prefix' ~
                    $*W.canonicalize_pair('', $*W.colonpair_nibble_to_str(
                        $/, $op_name<nibble> // $op_name<semilist> // $op_name<pblock>
                    ))));
                }
            } else {
                make $<colonpair>.ast;
            }
        }
    }

    method privop($/) {
        # Compiling private method calls is somewhat interesting. If it's
        # in any way qualified, we need to ensure that the current package
        # is trusted by the target class. Otherwise we assume that the call
        # is to a private method in the current (non-virtual) package.
        # XXX Attribute accesses? Again, maybe for the optimizer, since it
        # runs after CHECK time.
        my $past := $<methodop>.ast;
        if $<methodop><longname> {
            my @parts   := $*W.dissect_longname($<methodop><longname>).components();
            my $name    := @parts.pop;
            if @parts {
                my $methpkg := $*W.find_symbol(@parts);
                unless nqp::can($methpkg.HOW, 'is_trusted') && $methpkg.HOW.is_trusted($methpkg, $*PACKAGE) {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Permission'],
                        :method(         $name),
                        :source-package( $methpkg.HOW.name($methpkg)),
                        :calling-package( $*PACKAGE.HOW.name($*PACKAGE)),
                    );
                }
                $past[1].returns($methpkg);
            }
            else {
                my $pkg := $*PACKAGE;
                unless nqp::can($pkg.HOW, 'find_private_method') {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Unqualified'],
                        :method($name),
                    );
                }
                if $pkg.HOW.archetypes.parametric {
                    $past.unshift(QAST::Var.new( :name('::?CLASS'), :scope('typevar') ));
                }
                else {
                    $past.unshift(QAST::WVal.new( :value($pkg) ));
                    $past[0].returns($pkg);
                }
                $past.unshift($*W.add_string_constant($name));
            }
            $past.name('dispatch:<!>');
        }
        elsif $<methodop><quote> {
            my $name := $past.shift;
            my $pkg  := $*PACKAGE;
            if $pkg.HOW.archetypes.parametric {
                $past.unshift(QAST::Var.new( :name('::?CLASS'), :scope('typevar') ));
            }
            else {
                $past.unshift(QAST::WVal.new( :value($*PACKAGE) ));
            }
            $past.unshift($name);
            $past.name('dispatch:<!>');
        }
        else {
            $/.CURSOR.panic("Cannot use this form of method call with a private method");
        }
        make $past;
    }

    method methodop($/) {
        my $past := $<args> ?? $<args>.ast !! QAST::Op.new( :node($/) );
        $past.op('callmethod');
        my $name;
        if $<longname> {
            # May just be .foo, but could also be .Foo::bar. Also handle the
            # macro-ish cases.
            my @parts := $*W.dissect_longname($<longname>).components();
            $name := @parts.pop;
            wantall($past, 'methodop/longname');
            if +@parts {
                $past.unshift($*W.symbol_lookup(@parts, $/));
                $past.unshift($*W.add_string_constant($name));
                $past.name('dispatch:<::>');
                make $past;
                return;
            }
        }
        elsif $<identifier> {
            $name := ~$<identifier>;
        }
        else {
            $name := '';
        }

        if $name ne '' {
            if $name eq 'WHAT' {
                whine_if_args($/, $past, $name);
                $past.op('what');
            }
            elsif $name eq 'HOW' {
                whine_if_args($/, $past, $name);
                $past.op('how');
            }
            elsif $name eq 'WHO' {
                whine_if_args($/, $past, $name);
                $past.op('who');
            }
            elsif $name eq 'VAR' {
                whine_if_args($/, $past, $name);
                $past.op('p6var');
            }
            elsif $name eq 'REPR' {
                whine_if_args($/, $past, $name);
                $past.op('p6reprname');
            }
            elsif $name eq 'DEFINITE' {
                whine_if_args($/, $past, $name);
                $past.op('p6definite');
            }
            else {
                $past.name( $name );
            }
        }
        elsif $<quote> {
            $past.unshift(
                QAST::Op.new(
                    :op<unbox_s>,
                    $<quote>.ast
                )
            );
        }
        elsif $<variable> {
            $past.unshift(WANTED($<variable>.ast, 'methodop/var'));
            $past.name('dispatch:<var>');
        }
        unless $name eq 'sink' {
            wantall($past, 'methodop');
        }

        make $past;
    }

    sub whine_if_args($/, $past, $name) {
        if +@($past) > 0 {
           $*W.throw($/, ['X', 'Syntax', 'Argument', 'MOPMacro'], macro => $name);
        }
    }

    method term:sym<::?IDENT>($/) {
        make instantiated_type([~$/], $/);
    }

    method term:sym<self>($/) {
        make QAST::Var.new( :name('self'), :scope('lexical'), :returns($*PACKAGE), :node($/) );
    }

    method term:sym<now>($/) {
        make QAST::Op.new( :op('call'), :name('&term:<now>'), :node($/) );
    }

    method term:sym<time>($/) {
        make QAST::Op.new( :op('call'), :name('&term:<time>'), :node($/) );
    }

    method term:sym<empty_set>($/) {
        make QAST::Op.new( :op('call'), :name('&set'), :node($/) );
    }

    method term:sym<rand>($/) {
        make QAST::Op.new( :op('call'), :name('&rand'), :node($/) );
    }

    sub make_yada($name, $/) {
        my $past := $<args>.ast;
        $past.name($name);
        $past.node($/);
        unless +$past.list() {
            $past.push($*W.add_string_constant('Stub code executed'));
        }

        # Since we stub out a number of things in the setting,
        # we don't always have X::StubCode available.  If that
        # is the case, fall back to using the string variant
        try {
            my $X_StubCode := $*W.find_symbol(['X', 'StubCode'], :setting-only);
            $past[0].named('message');
            $past[0] := QAST::Op.new(
                :op('callmethod'), :name('new'),
                QAST::WVal.new( :value($X_StubCode) ),
                $past[0],
            );
        }
        $past
    }

    method term:sym<...>($/) {
        make make_yada('&fail', $/);
    }

    method term:sym<???>($/) {
        make make_yada('&warn', $/);
    }

    method term:sym<!!!>($/) {
        make make_yada('&die', $/);
    }

    method term:sym<dotty>($/) {
        my $past := $<dotty>.ast;
        $past.unshift(WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'dotty') );
        make QAST::Op.new( :op('hllize'), $past);
    }

    sub find_macro_routine(@symbol) {
        my $routine;
        try {
            $routine := $*W.find_symbol(@symbol);
            if istype($routine, $*W.find_symbol(['Macro'])) {
                return $routine;
            }
        }
        0
    }

    sub expand_macro($macro, $name, $/, &collect_argument_asts) {
        my @argument_asts := &collect_argument_asts();
        my $macro_ast := $*W.ex-handle($/, { $macro(|@argument_asts) });
        my $nil_class := $*W.find_symbol(['Nil']);
        if istype($macro_ast, $nil_class) {
            return QAST::WVal.new( :value($nil_class) );
        }
        my $ast_class := $*W.find_symbol(['AST']);
        unless istype($macro_ast, $ast_class) {
            $*W.throw('X::TypeCheck::Splice',
                got         => $macro_ast,
                expected    => $ast_class,
                symbol      => $name,
                action      => 'macro application',
            );
        }
        my $macro_ast_qast := nqp::getattr(
            nqp::decont($macro_ast),
            $ast_class,
            '$!past'
        );
        unless nqp::defined($macro_ast_qast) {
            return QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        }
        my $block := QAST::Block.new(:blocktype<raw>, $macro_ast_qast);
        $*W.add_quasi_fixups($macro_ast, $block);
        my $past := QAST::Stmts.new(
            $block,
            QAST::Op.new( :op('call'), QAST::BVal.new( :value($block) ) )
        );
        $past
    }

    method term:sym<identifier>($/) {
        my $macro := find_macro_routine(['&' ~ ~$<identifier>]);
        if $macro {
            make expand_macro($macro, ~$<identifier>, $/, sub () {
                my @argument_asts := [];
                if $<args><semiarglist> {
                    for $<args><semiarglist><arglist> {
                        if $_<EXPR> {
                            add_macro_arguments(WANTED($_<EXPR>.ast, 'identifier'), @argument_asts, ~$<args>);
                        }
                    }
                }
                return @argument_asts;
            });
        }
        elsif $<args><invocant> {
            my $past := self.methodop($/);       # invocant was already removed from args
            $past.unshift(WANTED($<args><invocant>,'identifier')); # (and was stored here)
            # say($past.dump);
            $past.node($/);
            make $past;
        }
        else {
            my $past := capture_or_raw($/,$<args>.ast, ~$<identifier>);
            $past.name('&' ~ $<identifier>);
            $past.node($/);
            make $past;
        }
    }

    sub add_macro_arguments($expr, @argument_asts, $code_string) {
        my $ast_class := $*W.find_symbol(['AST']);

        sub wrap_and_add_expr($expr) {
            my $quasi_ast := $ast_class.new();
            my $wrapped := QAST::Op.new( :op('call'), make_thunk_ref($expr, $expr.node) );
            nqp::bindattr($quasi_ast, $ast_class, '$!past', $wrapped);
            nqp::bindattr($quasi_ast, $ast_class, '$!Str', $code_string);
            @argument_asts.push($quasi_ast);
        }

        if nqp::istype($expr, QAST::Op) && $expr.name eq '&infix:<,>' {
            for $expr.list {
                wrap_and_add_expr($_);
            }
        }
        else {
            wrap_and_add_expr($expr);
        }
    }

    method make_indirect_lookup(@components, $sigil?) {
        my $past := QAST::Op.new(
            :op<call>,
            :name<&INDIRECT_NAME_LOOKUP>,
            QAST::Op.new(
                :op<callmethod>, :name<new>,
                QAST::WVal.new( :value($*W.find_symbol(['PseudoStash'])) )
            )
        );
        $past.push($*W.add_string_constant($sigil)) if $sigil;
        for @components {
            if nqp::can($_, 'isa') && $_.isa(QAST::Node) {
                $past.push($_);
            } else {
                $past.push($*W.add_string_constant(~$_));
            }
        }
        $past;
    }

    method term:sym<name>($/) {
        my $past;
        if $*longname.contains_indirect_lookup() {
            if $<args> {
                $/.CURSOR.panic("Combination of indirect name lookup and call not supported");
            }
            elsif $<arglist> {
                $/.CURSOR.panic("Combination of indirect name lookup and type arguments not supported");
            }
            elsif $<accept> || $<accept_any> {
                $/.CURSOR.panic("Combination of indirect name lookup and coercion type construction not supported");
            }
            $past := self.make_indirect_lookup($*longname.components())
        }
        elsif $<args><invocant> {
            $past := self.methodop($/);       # invocant was already removed from args
            $past.unshift(WANTED($<args><invocant>,'name')); # (and was stored here)
            # say($past.dump);
        }
        elsif $<args> {
            # If we have args, it's a call. Look it up dynamically
            # and make the call.
            # Add & to name.
            my @name := nqp::clone($*longname.attach_adverbs.components);
            my $final := @name[+@name - 1];
            unless nqp::eqat($final, '&', 0) {
                @name[+@name - 1] := '&' ~ $final;
            }
            my $macro := find_macro_routine(@name);
            if $macro {
                $past := expand_macro($macro, $*longname.text, $/, sub () {
                    my @argument_asts := [];
                    if $<args><semiarglist> {
                        for $<args><semiarglist><arglist> {
                            if $_<EXPR> {
                                add_macro_arguments(WANTED($_<EXPR>.ast, 'name/macro1'), @argument_asts, ~$<args>);
                            }
                        }
                    }
                    elsif $<args><arglist> {
                        if $<args><arglist><EXPR> {
                            add_macro_arguments(wanted($<args><arglist><EXPR>.ast, 'name/macro2'), @argument_asts, ~$<args>);
                        }
                    }
                    return @argument_asts;
                });
            }
            else {
                $past := capture_or_raw($/,$<args>.ast, ~$<longname>);
                if +@name == 1 {
                    $past.name(@name[0]);
                    $/.CURSOR.add_mystery(@name[0], $<args>.from, 'termish');
                    if +$past.list == 1 && %commatrap{@name[0]} {
                        my $prelen := $<longname>.from;
                        $prelen := 100 if $prelen > 100;
                        my $pre := nqp::substr($/.orig, $<longname>.from - $prelen, $prelen);
                        my $post := nqp::substr($/.orig, $<args>.to, 100);
                        if nqp::index($pre, "==>") < 0 && nqp::index($post, "<==") < 0 && $<args>.Str ~~ /^\s*['{'|'->'|'<->']/ {
                            $/.CURSOR.missing("comma after block argument to " ~ nqp::substr(@name[0],1));
                        }
                    }
                    check_smartmatch($<args>,$past[0]) if %commatrap{@name[0]} == 2;
                }
                else {
                    $past.unshift($*W.symbol_lookup(@name, $/));
                }
            }
        }
        else {
            # Otherwise, it's a type name; build a reference to that
            # type, since we can statically resolve them.
            my @name := $*longname.type_name_parts('type name');
            if $<arglist> {
                # Look up parametric type.
                my $ptype := $*W.find_symbol(@name);

                # Do we know all the arguments at compile time?
                my int $all_compile_time := 1;
                my $ast := $<arglist>.ast;
                wantall($past, 'name');
                for @($ast) {
                    unless $_.has_compile_time_value {
                        $all_compile_time := 0;
                    }
                }
                if $all_compile_time {
                    my $curried := $*W.parameterize_type($ptype, $<arglist>.ast, $/);
                    $past := QAST::WVal.new( :value($curried) );
                }
                else {
                    my $ptref := QAST::WVal.new( :value($ptype) );
                    $past := $<arglist>.ast;
                    $past.op('callmethod');
                    $past.name('parameterize');
                    $past.unshift($ptref);
                    $past.unshift(QAST::Op.new( :op('how'), $ptref ));
                }
            }
            elsif +@name == 0 {
                $past := QAST::Op.new(
                    :op<callmethod>, :name<new>,
                    QAST::WVal.new( :value($*W.find_symbol(['PseudoStash'])) )
                );
            }
            elsif $*W.is_pseudo_package(@name[0]) {
                $past := $*W.symbol_lookup(@name, $/);
            }
            elsif +@name == 1 && $*W.is_name(@name)
                    && !$*W.symbol_has_compile_time_value(@name) {
                # it's a sigilless param or variable
                $past := make_variable_from_parts($/, @name, '', '', @name[0]);
            }
            elsif +@name && @name[0] eq 'EXPORT' {
                my int $i := 1;
                my int $m := +@name;
                $past     := QAST::Var.new( :name<EXPORT>, :scope<lexical> );
                while $i < $m {
                    $past := QAST::Op.new( :op<callmethod>, :name<package_at_key>,
                        QAST::Op.new( :op<who>, $past ),
                        QAST::SVal.new(:value(@name[$i]))
                    );
                    $i := $i + 1
                }
            }
            else {
                $past := instantiated_type(@name, $/);
            }

            # Names ending in :: really want .WHO.
            if $*longname.get_who {
                $past := QAST::Op.new( :op('who'), $past );
            }

            if $<colonpairs><D> {
                unless nqp::istype($past, QAST::WVal) {
                    $/.CURSOR.panic("Type too complex to form a definite type");
                }
                my $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $past.value, 1); # XXX add constants
                $past    := QAST::WVal.new( :value($type) );
            }
            elsif $<colonpairs><U> {
                unless nqp::istype($past, QAST::WVal) {
                    $/.CURSOR.panic("Type too complex to form a definite type");
                }
                my $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $past.value, 0);
                $past    := QAST::WVal.new( :value($type) );
            }

            # If needed, try to form a coercion type.
            if $<accept> || $<accept_any> {
                my $value;
                if nqp::istype($past, QAST::WVal) {
                    $value := $past.value;
                }
                elsif $past.has_compile_time_value {
                    $value := $past.compile_time_value;
                }
                else {
                    $/.CURSOR.panic("Target type too complex to form a coercion type");
                }

                my $type := $*W.create_coercion_type($/, $value,
                    $<accept> ?? $<accept>.ast !! $*W.find_symbol(['Any']));
                $past := QAST::WVal.new( :value($type) );
            }
        }

        $past.node($/);
        make $past;
    }

    method term:sym<nqp::op>($/) {
        my @args   := $<args> ?? $<args>.ast.list !! [];
        my str $op := ~$<op>;

        # using nqp::op outside of setting
        unless %*PRAGMAS<MONKEY-GUTS> || %*PRAGMAS<nqp> || $*COMPILING_CORE_SETTING {
            $/.CURSOR.typed_panic('X::NQP::NotFound', op => $op);
        }

        my $past := QAST::Op.new( :$op, |@args );
        if $op eq 'want' || $op eq 'handle' {
            my int $i := 1;
            my int $n := nqp::elems($past.list);
            while $i < $n {
                $past[$i] := compile_time_value_str($past[$i], 'want specification', $/);
                $i := $i + 2;
            }
        }
        $past.node($/);
        nqp::getcomp('QAST').operations.attach_result_type('perl6', $past);
        make $past;
    }

    method term:sym<nqp::const>($/) {
        make QAST::Op.new( :op('const'), :name(~$<const>) );
    }

    method term:sym<*>($/) {
        make $*W.whatever();
    }

    method term:sym<**>($/) {
        make $*W.hyper_whatever();
    }

    method term:sym<capterm>($/) {
        make $<capterm>.ast;
    }

    method term:sym<onlystar>($/) {
        my $dc_name := QAST::Node.unique('dispatch_cap');
        my $stmts := QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($dc_name), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical') )
            ),
            QAST::Op.new(
                :op('invokewithcapture'),
                QAST::Op.new(
                    :op('ifnull'),
                    QAST::Op.new(
                        :op('multicachefind'),
                        QAST::Var.new(
                            :name('$!dispatch_cache'), :scope('attribute'),
                            QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical') ),
                            QAST::WVal.new( :value($*W.find_symbol(['Routine'], :setting-only)) ),
                        ),
                        QAST::Var.new( :name($dc_name), :scope('local') )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('find_best_dispatchee'),
                        QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical') ),
                        QAST::Var.new( :name($dc_name), :scope('local') )
                    )
                ),
                QAST::Var.new( :name($dc_name), :scope('local') )
            ));
        make QAST::Op.new( :op('locallifetime'), $stmts, $dc_name );
    }

    method args($/) {
        my $past;
        if    $<semiarglist> { $past := $<semiarglist>.ast; }
        elsif $<arglist>     { $past := $<arglist>.ast; }
        else {
            $past := QAST::Op.new( :op('call'), :node($/) );
        }
        $/<invocant> := $*INVOCANT if $*INVOCANT;
        make $past;
    }

    method semiarglist($/) {
        if +$<arglist> == 1 {
            make $<arglist>[0].ast;
        }
        else {
            my $past := QAST::Op.new( :op('call'), :node($/) );
            for $<arglist> {
                my $ast := $_.ast;
                $ast.name('&infix:<,>');
                $past.push(wanted($ast, 'semiarglist'));
            }
            make $past;
        }
    }

    sub hunt_loose_adverbs_in_arglist($/, @past) {
        # if we have a non-comma-separated list of adverbial pairs in an
        # arglist or semiarglist, only the first will be passed as a named
        # argument.

        # Thus, we need to find chained adverbs. They show up on the parse
        # tree as colonpair rules followed by a fake_infix.

        if nqp::getenvhash<COLONPAIR> eq 'trace' { say($/.dump) }
        if +$/.list == 1 && nqp::istype($/[0].ast, QAST::Op) && $/[0].ast.op eq 'call' && $/[0].ast.name ne 'infix:<,>' {
            nqp::die("these adverbs belong to a deeper-nested thing");
        }
        if $<fake_infix>
               || $<colonpair> && +($/.list) == 0 && +($/.hash) == 1 {
            if +($/.list) == 1 {
                hunt_loose_adverbs_in_arglist($/[0], @past);
            }
            my $Pair := $*W.find_symbol(['Pair']);
            if $<colonpair> && istype($<colonpair>.ast.returns, $Pair) {
                if $*WAS_SKIPPED {
                    nqp::push(@past, $<colonpair>.ast);
                } else {
                    $*WAS_SKIPPED := 1;
                }
            }
        } elsif $<OPER>.Str eq ',' {
           my $*WAS_SKIPPED := 0;
            for $/.list {
                hunt_loose_adverbs_in_arglist($_, @past);
            }
        }
    }

    method arglist($/) {
        my $Pair := $*W.find_symbol(['Pair']);
        my $past := QAST::Op.new( :op('call'), :node($/) );
        my @names;
        if $<EXPR> {
            # Make first pass over arguments, finding any duplicate named
            # arguments.
            my $expr := $<EXPR>.ast;
            my @args;
            if nqp::istype($expr, QAST::Op) {
                if $expr.name eq '&infix:<,>' {
                    @args := $expr.list;
                }
                elsif $expr.name eq '&infix:<:>' {
                    $*INVOCANT := $expr.list[0];
                    my $expr2 := $expr.list[1];
                    if $expr2 {
                        if nqp::istype($expr2, QAST::Op) && $expr2.name eq '&infix:<,>' {
                            @args := $expr2.list;
                        }
                        else {
                            @args := [$expr2];
                        }
                    }
                }
                else {
                    @args := [$expr];
                }
            }
            else {
                @args := [$expr];
            }

            # but first, look for any chained adverb pairs
            my $*WAS_SKIPPED := 0;
            try {
                if $*FAKE_INFIX_FOUND {
                    hunt_loose_adverbs_in_arglist($<EXPR>, @args);
                }
            }
            my %named_counts;
            for @args {
                if nqp::istype($_, QAST::Op) && istype($_.returns, $Pair)
                    && nqp::can($_[1], 'has_compile_time_value') {
                    my $name := compile_time_value_str($_[1], 'LHS of pair', $/);
                    %named_counts{$name} := +%named_counts{$name} + 1;
                    unless $*IN_DECL eq 'use' || $*IN_DECL eq 'no' || $*IN_DECL eq 'import' || $*IN_RETURN {
                        $_[2].named($name);
                    }
                }
            }

            # Make result.
            for @args {
                if nqp::istype($_, QAST::Op) && istype($_.returns, $Pair)
                    && nqp::can($_[1], 'has_compile_time_value') {
                    my $name := compile_time_value_str($_[1], 'LHS of pair', $/);
                    if %named_counts{$name} == 1 {
                        $past.push($_[2]);
                        $_[2].annotate('before_promotion', $_);
                    }
                    else {
                        %named_counts{$name} := %named_counts{$name} - 1;
                    }
                }
                elsif nqp::istype($_, QAST::Op) && $_.name eq '&prefix:<|>' {
                    my $reg := $past.unique('flattening_');
                    $past.push(QAST::Op.new(
                        :op('callmethod'), :name('FLATTENABLE_LIST'),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($reg), :scope('local'), :decl('var') ),
                            $_[0]),
                        :flat(1) ));
                    $past.push(QAST::Op.new(
                        :op('callmethod'), :name('FLATTENABLE_HASH'),
                        QAST::Var.new( :name($reg), :scope('local') ),
                        :flat(1), :named(1) ));
                    @names.push($reg);
                }
                else {
                    $past.push($_);
                }
            }
        }

        make $past; # TODO figure out how to work a locallifetime in here
    }

    method term:sym<value>($/) { make $<value>.ast; }

    method circumfix:sym<( )>($/) {
        my $past := $<semilist>.ast;
        my @args;
        # look for any chained adverb pairs
        if $<semilist><statement>[0]<EXPR> -> $EXPR {
            my $*WAS_SKIPPED := 0;
            try {
                if $*FAKE_INFIX_FOUND {
                    hunt_loose_adverbs_in_arglist($EXPR, @args);
                }
                for @args {
                    $_[2] := QAST::Want.new(|$_[2].list);
                }
            }
        }
        my $size := +$past.list;
        if $size == 0 {
            $past := QAST::Stmts.new( :node($/) );
            $past.push(QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/)));
        }
        elsif +@args {
            if $size == 1
            && nqp::istype($past[0],    QAST::Op)  && $past[0].op eq 'callmethod' && $past[0].name eq 'new'
            && nqp::istype($past[0][0], QAST::Var) && $past[0][0].name eq 'Pair' {
                $past := wanted(QAST::Stmts.new( :node($/),
                    QAST::Op.new( :op('call'), :name('&infix:<,>'),
                        QAST::Op.new(
                            :op('callmethod'), :name('new'), :returns($*W.find_symbol(['Pair'])), :node($past[0].node // $/),
                            QAST::Var.new( :name('Pair'), :scope('lexical'), :node($past[0].node // $/) ),
                            $past[0][1], $past[0][2]
                        ),
                        |@args
                    )
                ), 'circumfix()/pair');
            }
            else {
                for @args {
                    $past.push(wanted($_, 'circumfix()/args'));
                }
            }
        }
        make $past;
    }

    method circumfix:sym<STATEMENT_LIST( )>($/) {
        my $past := $<sequence>.ast;
        my $size := +$past.list;
        if $size == 0 {
            $past := QAST::Op.new( :op('call'), :name('&infix:<,>') );
        }
        else {
            my $last := $past[ $size - 1 ];
            $past.returns($last.returns);
            if nqp::istype($last, QAST::Block) {
                $past.arity($last.arity);
            }
        }
        make $past;
    }

    method circumfix:sym<ang>($/) { make $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { make $<nibble>.ast; }

    method circumfix:sym<« »>($/) { make $<nibble>.ast; }

    method circumfix:sym<{ }>($/) {
        # If it was {YOU_ARE_HERE}, nothing to do here.
        my $past := $<pblock>.ast;
        if ~$/ eq '{YOU_ARE_HERE}' {
            make $past;
            return 1;
        }

        # If it is completely empty or consists of a single list, the first
        # element of which is either a hash or a pair, it's a hash constructor.
        # Note that if it declares any symbols it is also not one.
        my $Pair := $*W.find_symbol(['Pair']);
        my int $is_hash   := 0;
        my int $has_stuff := 1;
        my $stmts := +$<pblock><blockoid><statementlist><statement>;
        my $bast  := $<pblock><blockoid>.ast;
        if $bast.symbol('$_')<used> || $bast.ann('also_uses') && $bast.ann('also_uses')<$_> {
            # Uses $_, so not a hash.
        }
        elsif $stmts == 0 {
            # empty block, so a hash
            $is_hash   := 1;
            $has_stuff := 0;
        }
        elsif $stmts == 1 {
            my $elem := try $past.ann('past_block')[1][0][0];
            $elem := $elem[0] if $elem ~~ QAST::Want;
            $elem := $elem[0] if nqp::istype($elem, QAST::Op) && $elem.op eq 'p6fatalize';
            if $elem ~~ QAST::Op && $elem.name eq '&infix:<,>' {
                # block contains a list, so test the first element
                $elem := $elem[0];
            }
            $elem := $elem[0] if nqp::istype($elem, QAST::Op) && $elem.op eq 'p6fatalize';
            if $elem ~~ QAST::Op && $elem.op eq 'p6capturelex' {
                my $subelem := $elem[0];
                if $subelem ~~ QAST::Op && $subelem.op eq 'callmethod' && $subelem.name eq 'clone' {
                    $subelem := $subelem[0];
                    if $subelem ~~ QAST::WVal && nqp::istype($subelem.value, $*W.find_symbol(['WhateverCode'], :setting-only)) {
                        $/.CURSOR.malformed("double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *");
                    }
                }
            }
            if $elem ~~ QAST::Op
                    && (istype($elem.returns, $Pair) || $elem.name eq '&infix:«=>»') {
                # first item is a pair
                $is_hash := 1;
            }
            elsif $elem ~~ QAST::Op && $elem.op eq 'call' &&
                    $elem[0] ~~ QAST::Op && $elem[0].name eq '&METAOP_REVERSE' &&
                    $elem[0][0] ~~ QAST::Var && $elem[0][0].name eq '&infix:«=>»' {
                # first item is a pair constructed with R=>
                $is_hash := 1;
            }
            elsif $elem ~~ QAST::Var && nqp::eqat($elem.name, '%', 0) {
                # first item is a hash (%foo or %!foo)
                $is_hash := 1;
            }
            elsif $elem ~~ QAST::Op && $elem.name eq '&DYNAMIC' &&
                    $elem[0] ~~ QAST::Want && $elem[0][1] eq 'Ss' &&
                    $elem[0][2] ~~ QAST::SVal && nqp::substr($elem[0][2].value, 0, 1) eq '%' {
                # first item is a hash (%*foo)
                $is_hash := 1;
            }
        }
        if $is_hash {
            for $past.ann('past_block').symtable() {
                my $sym := $_.key;
                if $sym ne '$_' && $sym ne '$*DISPATCHER' {
                    $is_hash := 0;
                }
            }
        }
        if $is_hash && $past.ann('past_block').arity == 0 {
            migrate_blocks($past.ann('past_block'), $*W.cur_lexpad());
            my @children := @($past.ann('past_block')[1]);
            $past := QAST::Op.new(
                :op('call'),
                :name(
                    $/.from && nqp::substr($/.orig, $/.from - 1, 1) eq ':' ?? '&circumfix:<:{ }>' !! '&circumfix:<{ }>'
                ),
                :node($/)
            );
            if $has_stuff {
                for @children {
                    if nqp::istype($_, QAST::Stmt) {
                        # Mustn't use QAST::Stmt here, as it will cause register
                        # re-use within a statemnet, which is a problem.
                        $_ := QAST::Stmts.new( |$_.list );
                    }
                    $past.push($_);
                }
                # look for any chained adverb pairs
                if $<pblock><blockoid><statementlist><statement>[0]<EXPR> -> $EXPR {
                    my $*WAS_SKIPPED := 0;
                    try {
                        my @args;
                        if $*FAKE_INFIX_FOUND {
                            hunt_loose_adverbs_in_arglist($EXPR, @args);
                        }
                        for @args {
                            $_[2] := QAST::Want.new(|$_[2].list);
                            $past.push(
                                QAST::Op.new(
                                    :op('callmethod'), :name('new'), :returns($*W.find_symbol(['Pair'])), :node($_.node // $/),
                                    QAST::Var.new( :name('Pair'), :scope('lexical'), :node($_.node // $/) ),
                                    $_[1], $_[2]
                                )
                            );
                        }
                    }
                }
            }
        }
        else {
            my $block := $past.ann('past_block');
            $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
            $block[0].push(QAST::Op.new(
                :op('takedispatcher'),
                QAST::SVal.new( :value('$*DISPATCHER') )
            ));
            $past := block_closure($past);
            $past.annotate('bare_block', QAST::Op.new( :op('call'), $past ));
        }
        $past.node($/);
        make $past;
    }

    # Some constructs are parsed and compiled with blocks inside of them, but
    # then the outer block goes away (for example, when a {...} becomes a
    # hash). Others get thunked and so need to have certain blocks in an
    # expression moved into the thunk. This performs the migration. Takes an
    # optional predicate to decide whether to move a block.
    sub migrate_blocks($from, $to, $predicate?) {
        my @decls := @($from[0]);
        my int $n := nqp::elems(@decls);
        my int $i := 0;
        while $i < $n {
            my $decl := @decls[$i];
            if nqp::istype($decl, QAST::Block) {
                if !$predicate || $predicate($decl) {
                    $to[0].push($decl);
                    @decls[$i] := QAST::Op.new( :op('null') );
                }
            }
            elsif (nqp::istype($decl, QAST::Stmt) || nqp::istype($decl, QAST::Stmts)) &&
                  nqp::istype($decl[0], QAST::Block) {
                if !$predicate || $predicate($decl[0]) {
                    $to[0].push($decl[0]);
                    $decl[0] := QAST::Op.new( :op('null') );
                }
            }
            elsif nqp::istype($decl, QAST::Var) && $predicate && $predicate($decl) {
                $to[0].push($decl);
                @decls[$i] := QAST::Op.new( :op('null') );
            }
            $i++;
        }
    }

    method circumfix:sym<[ ]>($/) {
        make QAST::Op.new( :op('call'), :name('&circumfix:<[ ]>'), $<semilist>.ast, :node($/) );
    }

    ## Expressions
    my %specials := nqp::hash(
        '==>',  -> $/, $sym { make_feed($/) },
        '==>>', -> $/, $sym { make_feed($/) },
        '<==',  -> $/, $sym { make_feed($/) },
        '<<==', -> $/, $sym { make_feed($/) },
        '~~',   -> $/, $sym { make_smartmatch($/, 0) },
        '!~~',  -> $/, $sym { make_smartmatch($/, 1) },
        '=',    -> $/, $sym { assign_op($/, $/[0].ast, $/[1].ast) },
        ':=',   -> $/, $sym { bind_op($/, $/[0].ast, $/[1].ast, 0) },
        '::=',  -> $/, $sym { bind_op($/, $/[0].ast, $/[1].ast, 1) },
        'ff',   -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 0, 0) },
        '^ff',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 0, 0) },
        'ff^',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 1, 0) },
        '^ff^', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 1, 0) },
        'fff',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 0, 1) },
        '^fff', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 0, 1) },
        'fff^', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 1, 1) },
        '^fff^',-> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 1, 1) }
    );
    method EXPR($/, $KEY?) {
        unless $KEY { return 0; }
        my $past := $/.ast // $<OPER>.ast;
        my $key := nqp::lc($KEY // 'infix');
        $key := 'infix' if $key eq 'list';
        my $sym := ~$/{$key}<sym>;
        my $O := $/{$key}<O>;
        my $thunky := $O<thunky>;
        my int $return_map := 0;
        if !$past && $sym eq '.=' {
            make make_dot_equals($/[0].ast, $/[1].ast);
            return 1;
        }
        if !$past && $sym eq '.' {
            make make_dot($/[0].ast, $/[1].ast);
            return 1;
        }
        elsif $past && nqp::eqat($past.name, '&METAOP_TEST_ASSIGN', 0) {
            $past.push(WANTED($/[0].ast, 'EXPR/META'));
            $past.push(block_closure(make_thunk_ref(WANTED($/[1].ast, 'EXPR/META'), $/)));
            make $past;
            return 1;
        }
        elsif nqp::existskey(%specials, $sym) {
            my $p := %specials{$sym}($/, $sym);
            if $p {
                make $p;
                return 1;
            }
        }
        elsif !$past && ($sym eq 'does' || $sym eq 'but') {
            make mixin_op($/, $sym);
            return 1;
        }
        elsif !$past && $thunky && ($sym eq 'xx' || $sym eq 'andthen' || $sym eq 'orelse') {
            $past := thunkity_thunk($/, $thunky,
                QAST::Op.new( :op('call'), :name("&infix" ~ $*W.canonicalize_pair('', $sym))),
                $/.list);
            make $past;
            return 1;
        }
        unless $past {
            if $<OPER><O><pasttype> {
                $past := QAST::Op.new( :node($/), :op( ~$<OPER><O><pasttype> ) );
            }
            else {
                $past := QAST::Op.new( :node($/), :op('call') );
            }
            if $<OPER><sym> {
                my $name;
                if $past.isa(QAST::Op) && !$past.name {
                    $name := $key ~ $*W.canonicalize_pair('', $<OPER><sym>);
                    $past.name('&' ~ $name);
                }
                my $macro := find_macro_routine(['&' ~ $name]);
                if $macro {
                    make expand_macro($macro, $name, $/, sub () {
                        my @argument_asts := [];
                        for @($/) {
                            add_macro_arguments($_.ast, @argument_asts, ~$_);
                        }
                        return @argument_asts;
                    });
                    return 'an irrelevant value';
                }
            }
        }
        my $arity := 0;
        if $key eq 'postfix' {
            # If may be an adverb.
            if $<colonpair> {
                my $target := $past := $/[0].ast;
                if nqp::istype($target, QAST::Op) && $target.op eq 'hllize' {
                    $target := $target[0];
                }
                unless nqp::istype($target, QAST::Op) && ($target.op eq 'call' || $target.op eq 'callmethod') {
                    if nqp::can($target, 'name') {
                        $/.CURSOR.typed_panic('X::Syntax::Adverb', what => $target.name);
                    }
                    else {
                        $/.CURSOR.typed_panic('X::Syntax::Adverb', what => ~$/[0]);
                    }
                }
                my $cpast := $<colonpair>.ast;
                $cpast[2].named(compile_time_value_str($cpast[1], 'LHS of pair', $/));
                $target.push(WANTED($cpast[2],'EXPR/POSTFIX'));

                if nqp::istype($past, QAST::Op) && $past.op eq 'hllize' {
                    $past[0] := WANTED($target,'EXPR/POSTFIX');
                }
                else {
                    $past := WANTED($target,'EXPR/POSTFIX');
                }

                make $past;
                return 1;
            }

            # Method calls may be to a foreign language, and thus return
            # values may need type mapping into Perl 6 land.
            $past.unshift(WANTED($/[0].ast,'EXPR/POSTFIX'));
            if $past.isa(QAST::Op) && $past.op eq 'callmethod' {
                $return_map := 1;
            }
        }
        elsif $past.ann('thunky') {
            $arity := $arity + +$/.list;
            $past := thunkity_thunk($/, $past.ann('thunky'), $past, $/.list);
        }
        elsif $past.name eq '&infix:<,>' || $past.name eq '&infix:<:>' {
            for $/.list { if $_.ast { $past.push($_.ast); ++$arity; } }
            make $past;
            return 1;
        }
        else {
            for $/.list { if $_.ast { $past.push(WANTED($_.ast,'EXPR/list')); ++$arity; } }
        }
        if $past.op eq 'xor' {
            $past.push(QAST::WVal.new( :named<false>, :value($*W.find_symbol(['Nil'])) ));
        }
#        if nqp::atkey(nqp::getenvhash,'RAKUDO_EXPR') {
#            note("$key $sym");
#            note($past.dump) if $past;
#        }
        if $key eq 'prefix' || $key eq 'postfix' || $key eq 'infix' {
            $past := self.whatever_curry($/, (my $orig := $past), $KEY eq 'LIST' ?? $arity !! $key eq 'infix' ?? 2 !! 1);
            if $return_map && $orig =:= $past {
                $past := QAST::Op.new($past,
                    :op('hllize'), :returns($past.returns()));
            }
        }
        make $past;
    }

    sub make_feed($/) {
        # Assemble into list of AST of each step in the pipeline.
        my @stages;
        if $/<infix><sym> eq '==>' {
            for @($/) { @stages.push(WANTED($_.ast,'==>')); }
        }
        elsif $/<infix><sym> eq '<==' {
            for @($/) { @stages.unshift(WANTED($_.ast,'<==')); }
        }
        else {
            $*W.throw($/, 'X::Comp::NYI',
                feature => $/<infix> ~ " feed operator"
            );
        }

        # Check what's in each stage and make a chain of blocks
        # that call each other. They'll return lazy things, which
        # will be passed in as var-arg parts to other things. The
        # first thing is just considered the result.
        my $result := @stages.shift;
        for @stages {
            # Wrap current result in a block, so it's thunked and can be
            # called at the right point.
            $result := QAST::Block.new( $result );

            # Check what we have. XXX Real first step should be looking
            # for @(*) since if we find that it overrides all other things.
            # But that's todo...soon. :-)
            if $_.isa(QAST::Op) && $_.op eq 'call' {
                # It's a call. Stick a call to the current supplier in
                # as its last argument.
                $_.push(QAST::Op.new( :op('call'), $result ));
            }
            elsif $_ ~~ QAST::Var {
                # It's a variable. We need code that gets the results, pushes
                # them onto the variable and then returns them (since this
                # could well be a tap.
                my $tmp := QAST::Node.unique('feed_tmp');
                $_ := QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :scope('local'), :name($tmp), :decl('var') ),
                        QAST::Op.new(
                            :op('callmethod'), :name('list'),
                            QAST::Op.new( :op('call'), $result )
                        ),
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('append'),
                        $_,
                        QAST::Var.new( :scope('local'), :name($tmp) )
                    ),
                    QAST::Var.new( :scope('local'), :name($tmp) )
                );
                $_ := QAST::Op.new( :op('locallifetime'), $_, $tmp );
            }
            else {
                $/.CURSOR.panic('Sorry, do not know how to handle this case of a feed operator yet.');
            }
            $result := $_;
        }

        WANTED($result,'make_feed');
        $result
    }

    sub check_smartmatch($/,$pat) {
        if nqp::can($pat,'ann') && $pat.ann('is_S') {
            $/.PRECURSOR.worry('Smartmatch with S/// can never succeed because the string it returns will fail to match. You can use given instead of ~~.');
        }

        if $pat ~~ QAST::WVal && istype($pat.returns, $*W.find_symbol(['Bool'])) && nqp::isconcrete($pat.value) {
            my $p := ~$pat.compile_time_value;
            if $p eq 'True' {
                $/.PRECURSOR.worry("Smartmatch against True always matches; if you mean to test the topic for truthiness, use :so or *.so or ?* instead")
            }
            elsif $p eq 'False' {
                $/.PRECURSOR.worry("Smartmatch against False always fails; if you mean to test the topic for truthiness, use :!so or *.not or !* instead")
            }
        }
    }

    sub make_smartmatch($/, $negated) {
        my $lhs := wanted($/[0].ast,'smarmatch/lhs');
        my $rhs := wanted($/[1].ast,'smarmatch/rhs');
        check_smartmatch($/[1],$rhs);
        # autoprime only on Whatever with explicit *
        return 0 if $lhs ~~ QAST::WVal && istype($lhs.returns, $*W.find_symbol(['Whatever'])) && nqp::isconcrete($lhs.value);
        return 0 if $rhs ~~ QAST::WVal && istype($rhs.returns, $*W.find_symbol(['Whatever'])) && nqp::isconcrete($rhs.value);

        # don't need topicalization, so allow chaining?
        return 0 if !$*COMPILING_CORE_SETTING && (
            $rhs.has_compile_time_value ||
            nqp::istype($rhs,QAST::Var) ||
            nqp::istype($lhs,QAST::Op) && $lhs.op eq 'chain'
        );

        my $old_topic_var := $lhs.unique('old_topic');
        my $result_var := $lhs.unique('sm_result');
        my $sm_call;

        # Call $rhs.ACCEPTS( $_ ), where $_ is $lhs.
        $sm_call := QAST::Op.new(
            :op('callmethod'), :name('ACCEPTS'),
            $rhs,
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'sm')
        );

        if $negated {
            $sm_call := QAST::Op.new( :op('call'), :name('&prefix:<!>'), $sm_call );
        }

        QAST::Op.new(
            :op('locallifetime'),
            QAST::Stmt.new(
                # Stash original $_.
                QAST::Op.new( :op('bind'),
                    WANTED(QAST::Var.new( :name($old_topic_var), :scope('local'), :decl('var') ),'sm/ot'),
                    WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'sm/ot')
                ),

                # Evaluate LHS and bind it to $_.
                QAST::Op.new( :op('bind'),
                    WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'sm/eval'),
                    $lhs
                ),

                # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
                # return value to a result variable.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($result_var), :scope('local'), :decl('var') ),
                    $sm_call
                ),

                # Re-instate original $_.
                QAST::Op.new( :op('bind'),
                    WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'sm/reinstate'),
                    QAST::Var.new( :name($old_topic_var), :scope('local') )
                ),

                # And finally evaluate to the smart-match result.
                WANTED(QAST::Var.new( :name($result_var), :scope('local') ),'make_sm')
            ),
            $old_topic_var,
            $result_var,
        );
    }

    sub bind_op($/, $target, $source, $sigish) {
        # Check we know how to bind to the thing on the LHS.
        $target := WANTED($target,'bind_op');
        if $target.isa(QAST::Var) {
            # Check it's not a native type; we can't bind to those.
            if nqp::objprimspec($target.returns) {
                $*W.throw($/, ['X', 'Bind', 'NativeType'],
                        name => ($target.name // ''),
                );
            }

            # We may need to decontainerize the right, depending on sigil.
            my $sigil := nqp::substr($target.name(), 0, 1);
            if $sigil eq '@' || $sigil eq '%' {
                $source := QAST::Op.new( :op('decont'), $source );
            }

            # Now go by scope.
            if $target.scope eq 'attribute' {
                # Source needs type check.
                my $type;
                try {
                    $type := $*PACKAGE.HOW.get_attribute_for_usage(
                        $*PACKAGE, $target.name
                    ).type;
                }
                unless $type =:= $*W.find_symbol(['Mu']) {
                    $source := QAST::Op.new(
                        :op('p6bindassert'),
                        $source, QAST::WVal.new( :value($type) ))
                }
            }
            else {
                # Probably a lexical.
                my int $was_lexical := 0;
                try {
                    my $type := $*W.find_lexical_container_type($target.name);
                    unless $type =:= $*W.find_symbol(['Mu']) {
                        $source := QAST::Op.new(
                            :op('p6bindassert'),
                            $source, QAST::WVal.new( :value($type) ));
                    }
                    $was_lexical := 1;
                }
                unless $was_lexical {
                    $*W.throw($/, ['X', 'Bind']);
                }
            }

            # Finally, just need to make a bind.
            make QAST::Op.new( :op('bind'), $target, $source );
        }
        elsif $target.isa(QAST::Op) && $target.op eq 'hllize' &&
                $target[0].isa(QAST::Op) && $target[0].op eq 'call' &&
                ($target[0].name eq '&postcircumfix:<[ ]>' ||
                 $target[0].name eq '&postcircumfix:<{ }>' ||
                 $target[0].name eq '&postcircumfix:<[; ]>') {
            $source.named('BIND');
            $target[0].push($source);
            $target.annotate('nosink', 1);
            make $target;
        }
        elsif $target.isa(QAST::Op) && $target.op eq 'call' &&
              ($target.name eq '&postcircumfix:<[ ]>' ||
               $target.name eq '&postcircumfix:<{ }>' ||
               $target.name eq '&postcircumfix:<[; ]>') {
            $source.named('BIND');
            $target.push($source);
            $target.annotate('nosink', 1);
            make $target;
        }
        elsif $target.isa(QAST::WVal) && nqp::istype($target.value, $*W.find_symbol(['Signature'], :setting-only)) {
            make QAST::Op.new(
                :op('p6bindcaptosig'),
                $target,
                QAST::Op.new(
                    :op('callmethod'), :name('Capture'),
                    $source
                ));
        }
        elsif $target.isa(QAST::Op) && $target.op eq 'call' && $target.name eq '&DYNAMIC' &&
            $target[0][1] eq 'Ss' {
            my $complain := QAST::Op.new(
                :op('die_s'),
                QAST::SVal.new( :value('Contextual ' ~ ~$/ ~ ' not found') )
            );
            my $contextual := QAST::VarWithFallback.new(
                :name($target[0][2].value), :scope('contextual'), :fallback($complain) );
            my $dynbind := QAST::Op.new( :op('bind'), $contextual, $source);
            $dynbind.annotate('nosink', 1);
            make $dynbind;
        }
        # XXX Several more cases to do...
        else {
            $*W.throw($/, ['X', 'Bind']);
        }
    }

    my @native_assign_ops := ['', 'assign_i', 'assign_n', 'assign_s'];
    sub assign_op($/, $lhs_ast, $rhs_ast) {
        my $past;
        my $var_sigil;
        $lhs_ast := WANTED($lhs_ast,'assign_op/lhs');
        $rhs_ast := wanted($rhs_ast,'assign_op/rhs');
        if nqp::istype($lhs_ast, QAST::Var) {
            $var_sigil := nqp::substr($lhs_ast.name, 0, 1);
            if $var_sigil eq '%' {
                if nqp::can($rhs_ast,'name') {
                    my $name := $rhs_ast.name;
                    if $name ~~ /^ '&circumfix:<' ':'? '{ }>' $/ {
                        $/.CURSOR.worry("Useless use of hash composer on right side of hash assignment; did you mean := instead?");
                    }
                }
            }
        }
        if nqp::istype($lhs_ast, QAST::Var)
                && nqp::objprimspec($lhs_ast.returns) -> $spec {
            # Native assignment is only possible to a reference; complain now
            # rather than at runtime since we'll innevitably fail.
            my $scope := $lhs_ast.scope;
            if $scope ne 'lexicalref' && $scope ne 'attributeref' {
                $lhs_ast.node.CURSOR.typed_sorry('X::Assignment::RO::Comp',
                    variable => $lhs_ast.name);
            }
            $past := QAST::Op.new(
                :op(@native_assign_ops[$spec]), :returns($lhs_ast.returns),
                $lhs_ast, $rhs_ast);
        }
        elsif $var_sigil eq '@' || $var_sigil eq '%' {
            # While the scalar container store op would end up calling .STORE,
            # it may do it in a nested runloop, which gets pricey. This is a
            # simple heuristic check to try and avoid that by calling .STORE.
            $past := QAST::Op.new(
                :op('callmethod'), :name('STORE'),
                $lhs_ast, $rhs_ast);
            $past.annotate('nosink', 1);
        }
        elsif $var_sigil eq '$' {
            # If it's a $ scalar, we can assume it's some kind of scalar
            # container with a container spec, so can go directly for the
            # low level assign op.
            $past := QAST::Op.new( :op('assign'), $lhs_ast, $rhs_ast );
        }
        elsif $lhs_ast.isa(QAST::Op) && $lhs_ast.op eq 'call' &&
              ($lhs_ast.name eq '&postcircumfix:<[ ]>' ||
               $lhs_ast.name eq '&postcircumfix:<{ }>' ||
               $lhs_ast.name eq '&postcircumfix:<[; ]>') &&
                +@($lhs_ast) == 2 { # no adverbs
            $lhs_ast.push($rhs_ast);
            $past := $lhs_ast;
            $past.annotate('nosink', 1);
        }
        elsif $lhs_ast.isa(QAST::Op) && $lhs_ast.op eq 'hllize' &&
                $lhs_ast[0].isa(QAST::Op) && $lhs_ast[0].op eq 'call' &&
                ($lhs_ast[0].name eq '&postcircumfix:<[ ]>' || $lhs_ast[0].name eq '&postcircumfix:<{ }>') &&
                +@($lhs_ast[0]) == 2 { # no adverbs
            $lhs_ast[0].push($rhs_ast);
            $past := $lhs_ast;
            $past.annotate('nosink', 1);
        }
        else {
            $past := QAST::Op.new( :node($/), :op('p6store'),
                $lhs_ast, $rhs_ast);
        }
        $past
    }

    sub mixin_op($/, $sym) {
        my $rhs  := $/[1].ast;
        my $past := QAST::Op.new(
            :op('call'), :name('&infix' ~ $*W.canonicalize_pair('', $sym)),
            $/[0].ast);
        if $rhs.isa(QAST::Op) && $rhs.op eq 'call' {
            if $rhs.name && +@($rhs) == 1 {
                try {
                    $past.push(QAST::WVal.new( :value($*W.find_symbol([nqp::substr($rhs.name, 1)])) ));
                    $rhs[0].named('value');
                    $past.push($rhs[0]);
                    CATCH { $past.push($rhs); }
                }
            }
            else {
                if +@($rhs) == 2 && $rhs[0].has_compile_time_value {
                    $past.push($rhs[0]); $rhs[1].named('value');
                    $past.push($rhs[1]);
                }
                else {
                    $past.push($rhs);
                }
            }
        }
        elsif $rhs.isa(QAST::Stmts) && +@($rhs) == 1 &&
                $rhs[0].isa(QAST::Op) && $rhs[0].name eq '&infix:<,>' {
            for @($rhs[0]) {
                $past.push($_);
            }
        }
        else {
            $past.push($rhs);
        }
        $past
    }

    sub thunkity_thunk($/,$thunky,$past,@clause) {
        my int $i := 0;
        my int $e := nqp::elems(@clause);
        my int $te := nqp::chars($thunky);
        my $type := nqp::substr($thunky,0,1);
        while $i < $e {
            my $ast := @clause[$i];
            $ast := WANTED($ast.ast, 'thunkity') if nqp::can($ast,'ast');  # reduce already passes ast...

            if $type eq 'T' || $type eq 'B' || $type eq 'A' {
                my $argast := $ast;
                $argast := $argast[0] if nqp::istype($argast,QAST::Stmts);
                if nqp::istype($argast,QAST::Op) && $argast.op eq 'call' && $argast.name eq '&infix:<,>' {
#                    note("thunky $type bingo:\n" ~ $argast.dump);
                    my int $ae := nqp::elems($argast);
                    my int $a := 0;
                    while $a < $ae {
                        my $elem := WANTED($argast[$a],'thunkity/comma');
                        if $type eq 'T' {  # thunk maybe (for xx)
                            unless $elem.has_compile_time_value {
                                $elem := block_closure(make_thunk_ref($elem, $/));
                            }
                        }
                        elsif $type eq 'B' {  # thunk and topicalize to a block
                            unless $elem.ann('bare_block') || $elem.ann('past_block') {
                                $elem := block_closure(make_topic_block_ref(@clause[$i], $elem, migrate_stmt_id => $*STATEMENT_ID));
                            }
                        }
                        elsif $type eq 'A' {  # thunk always
                            $elem := block_closure(make_thunk_ref($elem, $/));
                        }
                        $argast[$a] := $elem;
                        ++$a;
                    }
                }
            }
            if $type eq '.' || $type eq 'T' || $type eq 'B' || $type eq 'A' || nqp::istype($ast,QAST::SpecialArg) {
                $past.push($ast);
            }
            elsif $type eq 't' {  # thunk anything that might need (re)calculation
                if $ast.has_compile_time_value {
                    $past.push($ast);
                }
                else {
                    $past.push(block_closure(make_thunk_ref($ast, $/)));
                }
            }
            elsif $type eq 'b' {  # thunk and topicalize to a block
                unless $ast.ann('bare_block') || $ast.ann('past_block') {
                    $ast := block_closure(make_topic_block_ref(@clause[$i], $ast, migrate_stmt_id => $*STATEMENT_ID));
                }
                $past.push($ast);
            }
            elsif $type eq 'a' {  # always thunk  (XXX not ever needed?)
                $past.push(block_closure(make_thunk_ref($ast, $/)));
            }
            else {
                $/.CURSOR.panic("Unknown thunk spec '$type'");
            }
            $type := nqp::substr($thunky,$i,1) if ++$i < $te;  # repeat last thunk spec as necessary
        }
        $past;
    }

    sub flipflop($lhs, $rhs, $min_excl, $max_excl, $one_only) {
        # Need various constants.
        my $zero  := $*W.add_numeric_constant(NQPMu, 'Int', 0);
        my $one   := $*W.add_numeric_constant(NQPMu, 'Int', 1);
        my $nil   := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        my $false := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'False'])) );
        my $true  := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'True'])) );
        my $topic := WANTED(QAST::Var.new( :name('$_'), :scope<lexical> ),'ff');

        # Need a state variable to track the state.
        my %cont;
        my $id    := $lhs.unique('FLIPFLOP_STATE_');
        my $state := '!' ~ $id;
        %cont{'bind_constraint'} := $*W.find_symbol(['Mu']);
        %cont{'container_type'}  := $*W.find_symbol(['Scalar']);
        %cont{'container_base'}  := %cont{'container_type'};
        %cont{'default_value'}   := $zero.compile_time_value;
        %cont{'scalar_value'}    := $zero.compile_time_value;
        $*W.install_lexical_container($*W.cur_lexpad(), $state, %cont,
            $*W.create_container_descriptor(%cont{'bind_constraint'}, 1, $state),
            :scope('state'));

        # Twiddle to make special-case RHS * work.
        if istype($rhs.returns, $*W.find_symbol(['Whatever'])) {
            $rhs := $false;
        }

        # Evaluate LHS and RHS. Note that in one-only mode, we use
        # the state bit to decide which side to evaluate.
        my $ff_code := QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($id ~ '_lhs'), :scope('local'), :decl('var') ),
                ($one_only ??
                    QAST::Op.new(
                        :op('if'),
                        QAST::Var.new( :name($state), :scope('lexical') ),
                        $false,
                        QAST::Op.new( :op('call'), :name('&infix:<~~>'), $topic, wanted($lhs,'flipflop1') )
                    ) !!
                    QAST::Op.new( :op('call'), :name('&infix:<~~>'), $topic, wanted($lhs,'flipflop2') ))
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($id ~ '_rhs'), :scope('local'), :decl('var') ),
                ($one_only ??
                    QAST::Op.new(
                        :op('if'),
                        QAST::Var.new( :name($state), :scope('lexical') ),
                        QAST::Op.new( :op('call'), :name('&infix:<~~>'), $topic, wanted($rhs,'flipflop3') ),
                        $false
                    ) !!
                    QAST::Op.new( :op('call'), :name('&infix:<~~>'), $topic, wanted($rhs,'flipflop4') ))
            )
        );

        # Now decide what to do based on current state and current
        # results.
        $ff_code.push(QAST::Op.new(
            :op('if'),
            QAST::Var.new( :name($state), :scope('lexical') ),

            # State is currently true. Check RHS. If it's false, then we
            # increment the sequence count. If it's true, then we reset,
            # the state to zero and and what we return depends on $max_excl.
            QAST::Op.new(
                :op('if'),
                QAST::Var.new( :name($id ~ '_rhs'), :scope('local') ),
                ($max_excl ??
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $zero
                        ),
                        $nil
                    ) !!
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($id ~ '_orig'), :scope('local'), :decl('var') ),
                            QAST::Op.new(
                                :op('call'), :name('&prefix:<++>'),
                                QAST::Var.new( :name($state), :scope('lexical') )
                            )
                        ),
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $zero
                        ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($id ~ '_orig'), :scope('local') )
                        )
                    )),
                QAST::Stmts.new(
                    QAST::Op.new(
                        :op('call'), :name('&prefix:<++>'),
                        QAST::Var.new( :name($state), :scope('lexical') )
                    )
                )
            ),

            # State is currently false. Check LHS. If it's false, then we
            # stay in a false state. If it's true, then we flip the bit,
            # but only if the RHS is not also true. We return a result
            # based on $min_excl.
            QAST::Op.new(
                :op('if'),
                QAST::Var.new( :name($id ~ '_lhs'), :scope('local') ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Var.new( :name($id ~ '_rhs'), :scope('local') ),
                    $min_excl || $max_excl ?? $nil !! $one,
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $one
                        ),
                        $min_excl ?? $nil !! $one
                    )
                ),
                $nil
            )
        ));

        QAST::Op.new( :op('locallifetime'), $ff_code, $id ~ '_lhs', $id ~ '_rhs' );
    }

    method prefixish($/) {
        if $<prefix_postfix_meta_operator> {
            make QAST::Op.new( :node($/), :op<call>,
                QAST::Op.new( :node($/),
                     :name<&METAOP_HYPER_PREFIX>,
                     :op<call>,
                     QAST::Var.new( :name('&prefix' ~ $*W.canonicalize_pair('', $<OPER>.Str)),
                                    :scope<lexical> ))
            );
        }
    }

    sub baseop_reduce($/) {
        my str $reduce := 'LEFT';
        if $<prec> eq 'f='         { $reduce := 'LISTINFIX'; }
        elsif $<assoc> eq 'right'
           || $<assoc> eq 'list'   { $reduce := nqp::uc($<assoc>); }
        elsif $<prec> eq 'm='      { $reduce := 'CHAIN'; }
        elsif $<pasttype> eq 'xor' { $reduce := 'XOR'; }
        '&METAOP_REDUCE_' ~ $reduce;
    }

    method infixish($/) {
        return 0 if $<fake_infix>;

        my $ast;
        if $<infix> {
            $ast := $<infix>.ast;
        }
        elsif $<variable> {
            $ast := QAST::Op.new( :node($/), :op<call>, $<variable>.ast);
        }
        elsif $<infixish> {
            $ast := $<infixish>.ast;
        }
        elsif $<infix_circumfix_meta_operator> {
            $ast := $<infix_circumfix_meta_operator>.ast;
        }
        elsif $<infix_prefix_meta_operator> {
            my $metasym  := ~$<infix_prefix_meta_operator><sym>;
            my $base     := $<infix_prefix_meta_operator><infixish>;
            my $basesym  := ~$base<OPER>;
            my $basepast := $base.ast
                              ?? $base.ast[0]
                              !! QAST::Var.new(:name("&infix" ~ $*W.canonicalize_pair('', $basesym)),
                                               :scope<lexical>);
            my $t        := $basepast.ann('thunky') || $base<OPER><O><thunky>;
            my $helper   := '';
            if    $metasym eq '!' { $helper := '&METAOP_NEGATE'; }
            if    $metasym eq 'R' { $helper := '&METAOP_REVERSE'; $t := nqp::flip($t) if $t; }
            elsif $metasym eq 'X' { $helper := '&METAOP_CROSS'; $t := nqp::uc($t); }  # disable transitive thunking for now
            elsif $metasym eq 'Z' { $helper := '&METAOP_ZIP'; $t := nqp::uc($t); }

            my $metapast := QAST::Op.new( :op<call>, :name($helper), WANTED($basepast,'infixish') );
            $metapast.push(QAST::Var.new(:name(baseop_reduce($base<OPER><O>)),
                                         :scope<lexical>))
                if $metasym eq 'X' || $metasym eq 'Z';
            $metapast.annotate('thunky', $t) if $t;
            $ast := QAST::Op.new( :node($/), :op<call>, $metapast );
            $ast.annotate('thunky', $t) if $t;
        }

        if $<infix_postfix_meta_operator> {
            my $basesym;
            if $<infix> {
                $basesym := ~$<infix><sym>;
            }
            elsif $<infixish> {
                my $cur := $/;
                while $cur<infixish> { $cur := $cur<infixish> }
                if $cur<infix> { $basesym := ~$cur<infix><sym> }
            }
            else {
                $basesym := '';
            }
            if $basesym eq '||' || $basesym eq '&&' || $basesym eq '//' || $basesym eq 'orelse' || $basesym eq 'andthen' {
                $ast := WANTED(QAST::Op.new( :op<call>,
                        :name('&METAOP_TEST_ASSIGN' ~ $*W.canonicalize_pair('', $basesym)) ),'infixish');
            }
            else {
                $ast := QAST::Op.new( :node($/), :op<call>,
                    QAST::Op.new( :op<call>, :name<&METAOP_ASSIGN>,
                        WANTED($ast[0] // QAST::Var.new(
                            :name("&infix" ~ $*W.canonicalize_pair('', $basesym)), :scope('lexical')) ,'infixish')));
            }
        }

        make $ast;
    }

    method term:sym<reduce>($/) {
        my $base     := $<op>;
        my $basepast := $base.ast
                          ?? $base.ast[0]
                          !! QAST::Var.new(:name("&infix" ~ $*W.canonicalize_pair('', $base<OPER><sym>)),
                                           :scope<lexical>);
        my $metaop   := baseop_reduce($base<OPER><O>);
        my $metapast := QAST::Op.new( :op<call>, :name($metaop), WANTED($basepast,'reduce'));
        my $t        := $basepast.ann('thunky') || $base<OPER><O><thunky>;
        if $<triangle> {
            $metapast.push($*W.add_constant('Int', 'int', 1));
        }
        my $args := $<args>.ast;
        # one-arg rule?
        if +$args.list == 1 && !$args[0].flat && !$args[0].named {
            make QAST::Op.new(:node($/), :op<call>, $metapast, $args[0]);
        }
        else {
            if $t {
                # note("$metaop $t bingo\n" ~ $args.dump);
                if $metaop eq '&METAOP_REDUCE_LEFT' || $metaop eq '&METAOP_REDUCE_LIST' || $metaop eq '&METAOP_REDUCE_LISTINFIX' {
                    $args := thunkity_thunk($/,$t,QAST::Op.new( :op('call'), :name('&infix:<,>')),$args.list);
                    WANTALL($args,'reduce/args');
                }
                else {
                    $*W.throw($/, 'X::Comp::NYI',
                        feature => "Thunky pattern $t for $metaop");
                }
                # note("$metaop $t new\n" ~ $args.dump);
            }
            else {
                $args.name('&infix:<,>');
            }
            make QAST::Op.new(:node($/), :op<call>, WANTED($metapast,'reduce/meta'), WANTED($args,'reduce/args'));
        }
    }

    method infix_circumfix_meta_operator:sym«<< >>»($/) {
        make make_hyperop($/);
    }

    method infix_circumfix_meta_operator:sym<« »>($/) {
        make make_hyperop($/);
    }

    sub make_hyperop($/) {
        my $base     := $<infixish>;
        my $basesym  := ~ $base<OPER>;
        my $basepast := $base.ast
                          ?? $base.ast[0]
                          !! QAST::Var.new(:name("&infix" ~ $*W.canonicalize_pair('', $basesym)),
                                           :scope<lexical>);
        my $hpast    := QAST::Op.new(:op<call>, :name<&METAOP_HYPER>, WANTED($basepast,'hyperop'));
        if $<opening> eq '<<' || $<opening> eq '«' {
            my $dwim := $*W.add_constant('Int', 'int', 1);
            $dwim.named('dwim-left');
            $hpast.push($dwim);
        }
        if $<closing> eq '>>' || $<closing> eq '»' {
            my $dwim := $*W.add_constant('Int', 'int', 1);
            $dwim.named('dwim-right');
            $hpast.push($dwim);
        }
        QAST::Op.new( :node($/), :op<call>, $hpast )
    }

    method postfix:sym<ⁿ>($/) {
        my int $power := 0;
        for $<dig> {
            $power := $power * 10 + nqp::index("⁰¹²³⁴⁵⁶⁷⁸⁹", $_);
        }
        $power := -$power if $<sign> eq '⁻' || $<sign> eq '¯';
        make QAST::Op.new(:op<call>, :name('&postfix:<ⁿ>'), $*W.add_constant('Int', 'int', $power));
    }

    method postfixish($/) {
        if $<postfix_prefix_meta_operator> {
            my $past := $<OPER>.ast || QAST::Op.new( :name('&postfix' ~ $*W.canonicalize_pair('', $<OPER>.Str)),
                                                     :op<call> );
            if $past.isa(QAST::Op) && $past.op() eq 'callmethod' {
                if $past.name -> $name {
                    $past.unshift(QAST::SVal.new( :value($name) ));
                }
                $past.name('dispatch:<hyper>');
            }
            elsif $past.isa(QAST::Op) && $past.op() eq 'call' {
                if $<dotty> {
                    $past.name('&METAOP_HYPER_CALL');
                }
                else {
                    my $basepast := $past.name
                                    ?? QAST::Var.new( :name($past.name), :scope<lexical>)
                                    !! $past[0];
                    $past.push($basepast);
                    $past.name('&METAOP_HYPER_POSTFIX_ARGS');
                }
            }
            make $past;
        }
    }

    method postcircumfix:sym<[ ]>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<[ ]>'), :op('call'), :node($/) );
        if $<semilist> {
            my $c := $/.CURSOR;
            my $ast := $<semilist>.ast;
            $past.push($ast) if nqp::istype($ast, QAST::Stmts);
            if $ast.ann('multislice') {
                $past.name('&postcircumfix:<[; ]>');
            }
            for nqp::split(';', ~$<semilist>) {
                my $ix := $_ ~~ / [ ^ | '..' ] \s* <( '-' \d+ )> \s* $ /;
                if $ix {
                    $c.obs("a negative " ~ $ix ~ " subscript to index from the end", "a function such as *" ~ $ix);
                }
            }
        }
        make WANTED($past, '.[]');
    }

    method postcircumfix:sym<{ }>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        if $<semilist> {
            my $ast := $<semilist>.ast;
            $past.push($ast) if nqp::istype($ast, QAST::Stmts);
            if $ast.ann('multislice') {
                $past.name('&postcircumfix:<{; }>');
            }
        }
        make WANTED($past, '.{}');
    }

    method postcircumfix:sym<ang>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) &&
                   nqp::istype($nib[0], QAST::Op) && $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym«<< >>»($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) &&
                   nqp::istype($nib[0], QAST::Op) && $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym<« »>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) &&
                   nqp::istype($nib[0], QAST::Op) && $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym<( )>($/) {
        make $<arglist>.ast;
    }

    method value:sym<quote>($/) {
        make $<quote>.ast;
    }

    method value:sym<number>($/) {
        make $<number>.ast;
    }
    method value:sym<version>($/) {
        make $<version>.ast;
    }

    method version($/) {
        my $v := $*W.find_symbol(['Version']).new(~$<vstr>);
        $*W.add_object($v);
        make QAST::WVal.new( :value($v) );
    }

    method decint($/) { make string_to_bigint( $/, 10); }
    method hexint($/) { make string_to_bigint( $/, 16); }
    method octint($/) { make string_to_bigint( $/, 8 ); }
    method binint($/) { make string_to_bigint( $/, 2 ); }

    method number:sym<numish>($/) {
        make $<numish>.ast;
    }

    method signed-integer($/) {
        make $*W.add_numeric_constant($/, 'Int',
            $<sign> eq '-' ?? -$<integer>.ast !! $<integer>.ast);
    }

    method signed-number($/) {
        my $qast := $<number>.ast;
        $qast := QAST::Op.new( :op('call'), :name('&infix:<->'), $qast) if $<sign> eq '-';
        make $qast;
    }

    my $nuprop := nqp::existskey(nqp::backendconfig(), 'moarlib') ?? nqp::unipropcode("NumericValueNumerator") !! '';
    my $deprop := nqp::existskey(nqp::backendconfig(), 'moarlib') ?? nqp::unipropcode("NumericValueDenominator") !! '';

    method numish($/) {
        if $<integer> {
            make $*W.add_numeric_constant($/, 'Int', $<integer>.ast);
        }
        elsif $<dec_number>     { make $<dec_number>.ast; }
        elsif $<rad_number>     { make $<rad_number>.ast; }
        elsif $<rat_number>     { make $<rat_number>.ast; }
        elsif $<complex_number> { make $<complex_number>.ast; }
        elsif $<unum> {
            my $code := nqp::ord($/.Str);
            my int $nu := +nqp::getuniprop_str($code, $nuprop);
            my int $de := +nqp::getuniprop_str($code, $deprop);
            if !$de || $de == '1' {
                make $*W.add_numeric_constant($/, 'Int', +$nu)
            }
            else {
                my $ast := $*W.add_constant('Rat', 'type_new', $nu, $de, :nocache(1));
                $ast.node($/);
                make $ast;
            }
        }
        else {
            make $*W.add_numeric_constant($/, 'Num', +$/);
        }
    }

    method escale($/) {
        make $<sign> eq '-'
            ??  nqp::neg_I($<decint>.ast, $<decint>.ast)
            !! $<decint>.ast;
    }

    method dec_number($/) {
        my $Int := $*W.find_symbol(['Int']);
        my $parti;
        my $partf;

        # we build up the number in parts
        if nqp::chars($<int>) {
            $parti := $<int>.ast;
        } else {
            $parti := nqp::box_i(0, $Int);
        }

        if nqp::chars($<frac>) {
            $partf := nqp::radix_I(10, $<frac>.Str, 0, 4, $Int);

            $parti := nqp::mul_I($parti, $partf[1], $Int);
            $parti := nqp::add_I($parti, $partf[0], $Int);

            $partf := nqp::tonum_I($partf[1]);
        } else {
            $partf := 1.0;
        }

        if $<escale> { # wants a Num
            $parti := nqp::tonum_I($parti);

            if $parti != 0.0 {
                $parti := nqp::mul_n($parti, nqp::pow_n(10, nqp::tonum_I($<escale>.ast)));
            }
            $parti := nqp::div_n($parti, $partf);

            make $*W.add_numeric_constant($/, 'Num', $parti);
        } else { # wants a Rat
            my $ast := $*W.add_constant('Rat', 'type_new', $parti, nqp::fromnum_I($partf, $Int), :nocache(1));
            $ast.node($/);
            make $ast;
        }
    }

    method rad_number($/) {
        my $radix    := +($<radix>.Str);

        if $<bracket> { # the "list of place values" case
            make QAST::Op.new(:name('&UNBASE_BRACKET'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<bracket>.ast);
        }
        elsif $<circumfix> { # the "conversion function" case
            make QAST::Op.new(:name('&UNBASE'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<circumfix>.ast);
        } else { # the "string literal" case
            my $Int := $*W.find_symbol(['Int']);

            $*W.throw($/, 'X::Syntax::Number::RadixOutOfRange', :$radix) unless (2 <= $radix) && ($radix <= 36);

            # override $radix if necessary
            if nqp::chars($<ohradix>) {
                my $ohradstr := $<ohradix>.Str;
                if $ohradstr eq "0x" {
                    $radix := 16;
                } elsif $ohradstr eq "0o" {
                    $radix := 8;
                } elsif $ohradstr eq "0d" {
                    $radix := 10;
                } elsif $ohradstr eq "0b" {
                    $radix := 2;
                } else {
                    $/.CURSOR.panic("Unknown radix prefix '$ohradstr'.");
                }
            }

            my $ipart := nqp::radix_I($radix, $<intpart>.Str, 0, 0, $Int);
            my $fpart := nqp::radix_I($radix, nqp::chars($<fracpart>) ?? $<fracpart>.Str !! ".0", 1, 4, $Int);
            my $bpart := $<base> ?? nqp::tonum_I($<base>[0].ast) !! $radix;
            my $epart := $<exp> ?? nqp::tonum_I($<exp>[0].ast) !! 0;

            if $ipart[2] < nqp::chars($<intpart>.Str) || $fpart[2] < nqp::chars($<fracpart>.Str) {
                $/.CURSOR.panic("Couldn't process entire number: {$ipart[2]}/{nqp::chars($<intpart>.Str)} int chars, {$fpart[2]}/{nqp::chars($<fracpart>.Str) - 1} fractional chars");
            }

            $ipart := nqp::mul_I($ipart[0], $fpart[1], $Int);
            $ipart := nqp::add_I($ipart, $fpart[0], $Int);
            $fpart := $fpart[1];

            my $scientific := nqp::pow_n($bpart, $epart);
            $ipart := nqp::mul_I($ipart, nqp::fromnum_I($scientific, $Int), $Int);

            if $fpart != 1 { # non-unit fractional part, wants Rat
                my $ast := $*W.add_constant('Rat', 'type_new', $ipart, $fpart, :nocache(1));
                $ast.node($/);
                make $ast;
            } else { # wants Int
                make $*W.add_numeric_constant($/, 'Int', $ipart);
            }
        }
    }

    method radint($/) {
        # XXX fix this when fixing its grammar side
        make $<integer>.ast;
    }

    method complex_number($/) { make $<bare_complex_number>.ast }

    method rat_number($/) { make $<bare_rat_number>.ast }

    method bare_rat_number($/) {
        my $nu := $<nu>.ast.compile_time_value;
        my $de := $<de>.ast;
        my $ast := $*W.add_constant('Rat', 'type_new', $nu, $de, :nocache(1));
        $ast.node($/);
        make $ast;
    }

    method bare_complex_number($/) {
        my $re := $*W.add_constant('Num', 'num', +$<re>.Str);
        my $im := $*W.add_constant('Num', 'num', +$<im>.Str);
        my $rv := $re.compile_time_value;
        my $iv := $im.compile_time_value;
        my $ast := $*W.add_constant('Complex', 'type_new', $rv, $iv, :nocache(1));
        $ast.node($/);
        make $ast;
    }

    method typename($/) {
        # Locate the type object and make that. Anything that wants a PAST
        # reference to it can obtain one, but many things really want the
        # actual type object to build up some data structure or make a trait
        # dispatch with. Note that for '::T' style things we need to make a
        # GenericHOW, though whether/how it's used depends on context.
        if $<longname> {
            my str $str_longname := ~$<longname>;
            if !nqp::eqat($str_longname, '::', 0) {
                my $longname := $*W.dissect_longname($<longname>);
                my $type := $*W.find_symbol($longname.type_name_parts('type name'));
                if $<arglist> {
                    $type := $*W.handle-begin-time-exceptions($/, "parameterizing $str_longname",
                        {
                            WANTALL($<arglist>.ast,'typename');
                            $*W.parameterize_type($type, WANTED($<arglist>.ast,'typename'), $/)
                        }
                    );
                }

                if $<colonpairs><D> {
                    $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $type, 1);
                }
                elsif $<colonpairs><U> {
                    $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $type, 0);
                }

                if $<accept> || $<accept_any> {
                    if $<typename> {
                        $/.CURSOR.panic("Cannot put 'of' constraint on a coercion type");
                    }
                    $type := $*W.create_coercion_type($/, $type,
                        $<accept> ?? $<accept>.ast !! $*W.find_symbol(['Any']));
                }
                elsif $<typename> {
                    $type := $*W.parameterize_type_with_args($/, $type,
                        [$<typename>.ast], hash());
                }
                make $type;
            }
            else {
                if $<arglist> || $<typename> {
                    $/.CURSOR.panic("Cannot put type parameters on a type capture");
                }
                if $<accepts> || $<accepts_any> {
                    $/.CURSOR.panic("Cannot base a coercion type on a type capture");
                }
                if $str_longname eq '::' {
                    $/.CURSOR.panic("Cannot use :: as a type name");
                }
                if $*W.cur_lexpad.symbol(nqp::substr($str_longname, 2)) {
                    $*W.throw($/, ['X', 'Redeclaration'],
                        symbol => nqp::substr($str_longname, 2));
                }
                make $*W.pkg_create_mo($/, $*W.resolve_mo($/, 'generic'), :name(nqp::substr($str_longname, 2)));
            }
        }
        else {
            make $*W.find_symbol(['::?' ~ ~$<identifier>]);
        }
    }

    my %SUBST_ALLOWED_ADVERBS;
    my %TRANS_ALLOWED_ADVERBS;
    my %SHARED_ALLOWED_ADVERBS;
    my %MATCH_ALLOWED_ADVERBS;
    my %MATCH_ADVERBS_MULTIPLE := hash(
        x       => 1,
        g       => 1,
        global  => 1,
        ov      => 1,
        overlap => 1,
        ex      => 1,
        exhaustive => 1,
    );
    my %REGEX_ADVERBS_CANONICAL := hash(
        ignorecase  => 'i',
        ignoremark  => 'm',
        ratchet     => 'r',
        sigspace    => 's',
        continue    => 'c',
        pos         => 'p',
        th          => 'nth',
        st          => 'nth',
        nd          => 'nth',
        rd          => 'nth',
        global      => 'g',
        overlap     => 'ov',
        exhaustive  => 'ex',
        Perl5       => 'P5',
        samecase    => 'ii',
        samespace   => 'ss',
        samemark    => 'mm',
        squash      => 's',
        complement  => 'c',
        delete      => 'd',
    );
    my %REGEX_ADVERB_IMPLIES := hash(
        ii        => 'i',
        ss        => 's',
        mm        => 'm',
    );
    INIT {
        my str $mods := 'i ignorecase m ignoremark s sigspace r ratchet Perl5 P5';
        for nqp::split(' ', $mods) {
            %SHARED_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'g global ii samecase ss samespace mm samemark x c continue p pos nth th st nd rd';
        for nqp::split(' ', $mods) {
            %SUBST_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'x c continue p pos nth th st nd rd g global ov overlap ex exhaustive';
        for nqp::split(' ', $mods) {
            %MATCH_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'd delete c complement s squash';
        for nqp::split(' ', $mods) {
            %TRANS_ALLOWED_ADVERBS{$_} := 1;
        }
    }


    method quotepair($/) {
        unless $*value ~~ QAST::Node {
            if $*purpose eq 'rxadverb' && ($*key eq 'c' || $*key eq 'continue'
            || $*key eq 'p' || $*key eq 'pos') && $*value == 1 {
                $*value := QAST::Op.new(
                    :node($/),
                    :op<if>,
                    QAST::Var.new(:name('$/'), :scope('lexical')),
                    QAST::Op.new(:op('callmethod'),
                        QAST::Var.new(:name('$/'), :scope<lexical>),
                        :name<to>
                    ),
                    QAST::IVal.new(:value(0)),
                );
            } else {
                $*value := QAST::IVal.new( :value($*value) );
            }
        }
        $*value.named(~$*key);
        make $*value;
    }

    method rx_adverbs($/) {
        my @pairs;
        for $<quotepair> {
            nqp::push(@pairs, $_.ast);
        }
        make @pairs;
    }

    method setup_quotepair($/) {
        my %h;
        my $key := $*ADVERB.ast.named;
        my $value := $*ADVERB.ast;
        if $value ~~ QAST::IVal || $value ~~ QAST::SVal {
            $value := $value.value;
        }
        elsif $value.has_compile_time_value {
            $value := $value.compile_time_value;
        }
        else {
            if %SHARED_ALLOWED_ADVERBS{$key} {
                $*W.throw($/, ['X', 'Value', 'Dynamic'], what => "Adverb $key");
            }
        }
        $key := %REGEX_ADVERBS_CANONICAL{$key} // $key;
        %*RX{$key} := $value;
        if %REGEX_ADVERB_IMPLIES{$key} {
            %*RX{%REGEX_ADVERB_IMPLIES{$key}} := $value
        }
        $value;
    }

    method quote:sym<apos>($/) { make $<nibble>.ast; }
    method quote:sym<sapos>($/){ make $<nibble>.ast; }
    method quote:sym<lapos>($/){ make $<nibble>.ast; }
    method quote:sym<hapos>($/){ make $<nibble>.ast; }
    method quote:sym<dblq>($/) { make $<nibble>.ast; }
    method quote:sym<sdblq>($/){ make $<nibble>.ast; }
    method quote:sym<ldblq>($/){ make $<nibble>.ast; }
    method quote:sym<hdblq>($/){ make $<nibble>.ast; }
    method quote:sym<crnr>($/) { make $<nibble>.ast; }
    method quote:sym<qq>($/)   { make $<quibble>.ast; }
    method quote:sym<q>($/)    { make $<quibble>.ast; }
    method quote:sym<Q>($/)    { make $<quibble>.ast; }
    method quote:sym</ />($/) {
        my %sig_info := hash(parameters => []);
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<nibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<nibble>.ast;
        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure.annotate('sink_ast', QAST::Op.new( :op<callmethod>, :name<Bool>, $closure));
        make $closure;
    }

    method quote:sym<rx>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx', $block);
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<quibble>.ast;
        my $past := block_closure($coderef);
        $past.annotate('sink_ast', QAST::Op.new(:op<callmethod>, :name<Bool>, $past));
        make $past;
    }
    method quote:sym<m>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<quibble>.ast;

        my $past := QAST::Op.new(
            :node($/),
            :op('callmethod'), :name('match'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'m'),
            block_closure($coderef)
        );
        if self.handle_and_check_adverbs($/, %MATCH_ALLOWED_ADVERBS, 'm', $past) {
            # if this match returns a list of matches instead of a single
            # match, don't assign to $/ (which imposes item context)
            make $past;
        } else {
            make QAST::Op.new( :op('p6store'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $past
            );
        }
    }

    # returns 1 if the adverbs indicate that the return value of the
    # match will be a List of matches rather than a single match
    method handle_and_check_adverbs($/, %adverbs, $what, $past?) {
        my int $multiple := 0;
        for $<rx_adverbs>.ast {
            $multiple := 1 if %MATCH_ADVERBS_MULTIPLE{$_.named};
            unless %SHARED_ALLOWED_ADVERBS{$_.named} || %adverbs{$_.named} {
                $*W.throw($/, 'X::Syntax::Regex::Adverb',
                    adverb    => $_.named,
                    construct => $what,
                );
            }
            if $past {
                $past.push($_);
            }
        }
        $multiple;
    }

    method quote:sym<tr>($/) {
        my $left  := $<tribble><left>.ast;
        my $right := $<tribble><right>.ast;

        # First we get ourselves a local variable to store the original LHS,
        # which might be $_ or a LHS to ~~ - in both cases $_ holds the value.
        my $orig_lhs := $*W.cur_lexpad.unique('orig_lhs');

        # Next we build a Pair to pass to .trans.
        $left.named('key');
        $right.named('value');
        my $pair := QAST::Op.new(
            :op('callmethod'), :name('new'), :returns($*W.find_symbol(['Pair'])), :node($/),
            QAST::Var.new( :name('Pair'), :scope('lexical'), :node($/) ),
            $left, $right
        );

        # We store the value currently in $_ to get the distance later.
        my $store := QAST::Op.new( :op<bind>,
            QAST::Var.new( :name($orig_lhs), :scope<lexical>, :decl<var> ),
            QAST::Op.new( :op<decont>,
                WANTED(QAST::Var.new( :name('$_'), :scope<lexical> ),'tr')
            )
        );

        # ...pass the Pair we build to &Str.trans.
        my $trans := QAST::Op.new(
            :node($/),
            :op<callmethod>, :name<trans>,
            WANTED(QAST::Var.new(:name('$_'), :scope<lexical>),'tr/call'),
            $pair
        );

        if nqp::elems($<rx_adverbs><quotepair>) {
            self.handle_and_check_adverbs($/, %TRANS_ALLOWED_ADVERBS, 'transliteration', $trans);
        }

        my $StrDistance := $*W.find_symbol(['StrDistance']);
        # Putting it all together.
        my $past := make QAST::Stmt.new(
            $store,
            QAST::Op.new(
                :op<call>, :name('&infix:<=>'),
                WANTED(QAST::Var.new(:name('$_'), :scope<lexical>),'tr/assign'),
                $trans,
            ),
            # We build a StrDistance here, which lazily gets us the distance.
            QAST::Op.new(
                :op<callmethod>, :name<new>, :returns($StrDistance),
                WANTED(QAST::Var.new( :name<StrDistance>, :scope<lexical> ),'tr'),
                QAST::Var.new( :name($orig_lhs), :scope<lexical>, :named('before') ),
                QAST::Var.new( :name('$_'), :scope<lexical>, :named('after') )
            )
        );

        $past
    }

    method quote:sym<s>($/) {
        # We are emulating Str.subst/subst-mutate here, by calling match, assigning the result to
        # a temporary variable etc.

        # Build the regex.
        my $rx_block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my %sig_info := hash(parameters => []);
        my $rx_coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<sibble><left>.ast, 'anon', '', %sig_info, $rx_block, :use_outer_match(1));

        # Quote needs to be closure-i-fied.
        my $infixish := $<sibble><infixish>;
        my $right;
        if !$infixish || $infixish.Str eq '=' {
            $right := WANTED($<sibble><right>.ast,'quote:s///');
        }
        else {
            $right := $infixish.ast;
            $right.push(QAST::Op.new(
                :op('assign'),
                QAST::Op.new( :op('p6scalarfromdesc'), QAST::Op.new( :op('null') ) ),
                QAST::Var.new( :name('$/'), :scope('lexical') )
            ));
            $right.push(WANTED($<sibble><right>.ast,'quote:s'));
        }
        my $closure := block_closure(make_thunk_ref($right, $<sibble><right>));

        # self.match($rx_coderef, |%options);
        my $past := QAST::Op.new( :node($/), :op('callmethod'), :name('match'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'s'),
            $rx_coderef
        );
        self.handle_and_check_adverbs($/, %SUBST_ALLOWED_ADVERBS, 'substitution', $past);
        if $/[0] {
            $past.push(QAST::IVal.new(:named('samespace'), :value(1)));
        }

        my $samespace := +$/[0];
        my $sigspace := $samespace;
        my $samecase := 0;
        my $samemark := 0;
        my $global   := 0;
        for $<rx_adverbs>.ast {
            if $_.named eq 'samecase' || $_.named eq 'ii' {
                $samecase := 1;
            }
            elsif $_.named eq 'samemark' || $_.named eq 'mm' {
                $samemark := 1;
            }
            elsif $_.named eq 'global' || $_.named eq 'g' {
                $global := 1;
            }
            elsif $_.named eq 'samespace' || $_.named eq 'ss' {
                $samespace := 1;
                $sigspace := 1;
            }
            elsif $_.named eq 'sigspace' || $_.named eq 's' {
                $sigspace := 1;
            }
        }

        my $result        := $past.unique('subst_result');
        my $global_result := $past.unique('subst_global_result');
        my $List          := $*W.find_symbol(['List']);

        my $apply_matches := QAST::Op.new( :op('callmethod'), :name('dispatch:<!>'),
            QAST::Op.new( :op('callmethod'),  :name('Str'),
                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'s/apply') ),
            QAST::SVal.new( :value('APPLY-MATCHES') ),
            QAST::WVal.new( :value($*W.find_symbol(['Str'])) ),
            QAST::Var.new( :name($result), :scope('local') ),
            $closure,
            QAST::Var.new( :name('$/'), :scope('lexical') ), # caller dollar slash
            QAST::IVal.new( :value(1) ),                     # set dollar slash
            QAST::IVal.new( :value($sigspace) ),
            QAST::IVal.new( :value($samespace) ),
            QAST::IVal.new( :value($samecase) ),
            QAST::IVal.new( :value($samemark) ),
        );

        $past := QAST::Op.new( :op('locallifetime'), :node($/),
            QAST::Stmt.new(

                # my $result;
                QAST::Var.new( :name($result), :scope('local'), :decl('var') ),

                # $result := self.match(...
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($result), :scope('local') ),
                    $past
                ),

                # ($/,) = $result - We do this so the replacement closure can close
                # over the current match.
                QAST::Op.new( :op('p6store'),
                    QAST::Op.new( :op('call'), :name('&infix:<,>'),
                        QAST::Var.new( :name('$/'), :scope('lexical') ) ),
                    QAST::Var.new( :name($result), :scope('local') ),
                ),

                # It matched something. Either a single item or a list of matches.
                QAST::Op.new( :op('if'),
                    QAST::Op.new( :op('unless'),# :name('&infix:<||>'),
                        QAST::Op.new( :op('istype'),
                            QAST::Var.new( :name($result), :scope('local') ),
                            QAST::WVal.new( :value($*W.find_symbol(['Match'])) )
                        ),
                        QAST::Op.new( :op('if'),
                            QAST::Op.new( :op('istype'),
                                QAST::Var.new( :name($result), :scope('local') ),
                                QAST::WVal.new( :value($*W.find_symbol(['Positional'])) )
                            ),
                            QAST::Op.new( :op('callmethod'), :name('elems'),
                                QAST::Var.new( :name($result), :scope('local') )
                            )
                        )
                    ),

                    QAST::Op.new( :op('call'), :name('&infix:<=>'),
                        WANTED(QAST::Var.new( :name($<sym> eq 's' ?? '$_' !! '$/'), :scope('lexical') ),'s/assign'),
                        $apply_matches
                    ),
                    ( $<sym> eq 'S'
                        ?? QAST::Op.new( :op('p6store'),
                                QAST::Op.new( :op('call'), :name('&infix:<,>'),
                                    QAST::Var.new( :name('$/'), :scope('lexical') ) ),
                                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'S'),
                           )
                        !! QAST::Stmt.new()
                    ),
                ),

                # It will return a list of matches when we match globally, and a single
                # match otherwise.
                $<sym> eq 's' ?? (
                    $global ??
                    QAST::Op.new( :op('p6store'),
                        QAST::Var.new( :name('$/'), :scope('lexical') ),
                        QAST::Stmts.new(
                            QAST::Op.new( :op('bind'),
                                QAST::Var.new( :name($global_result), :scope('local'), :decl('var') ),
                                QAST::Op.new( :op('callmethod'), :name('CREATE'),
                                    QAST::WVal.new( :value($List) )
                                )
                            ),
                            QAST::Op.new( :op('bindattr'),
                                QAST::Var.new( :name($global_result), :scope('local') ),
                                QAST::WVal.new( :value($List) ),
                                QAST::SVal.new( :value('$!reified') ),
                                QAST::Op.new( :op('getattr'),
                                    QAST::Var.new( :name($result), :scope('local') ),
                                    QAST::WVal.new( :value($List) ),
                                    QAST::SVal.new( :value('$!reified') )
                                )
                            ),
                            QAST::Var.new( :name($global_result), :scope('local') )
                        )
                    ) !!
                    QAST::Op.new( :op('p6store'),
                        QAST::Op.new( :op('call'), :name('&infix:<,>'),
                            QAST::Var.new( :name('$/'), :scope('lexical') ) ),
                        QAST::Var.new( :name($result), :scope('local') )
                    )
                ) !! QAST::Stmt.new(),

                # The result of this operation.
                QAST::Var.new( :name('$/'), :scope('lexical') )
            ),
        );
        $past.annotate('is_S', $<sym> eq 'S');
        make WANTED($past, 's///');  # never carp about s/// in sink context
    }

    method quote:sym<quasi>($/) {
        my $ast_class := $*W.find_symbol(['AST']);
        my $quasi_ast := $ast_class.new();
        my $past := $<block>.ast.ann('past_block').pop;
        nqp::bindattr($quasi_ast, $ast_class, '$!past', $past);
        nqp::bindattr($quasi_ast, $ast_class, '$!Str', $/.Str());
        $*W.add_object($quasi_ast);
        my $throwaway_block := QAST::Block.new();
        my $quasi_context := block_closure(
            reference_to_code_object(
                $*W.create_simple_code_object($throwaway_block, 'Block'),
                $throwaway_block
            ));
        make QAST::Op.new(:op<callmethod>, :name<incarnate>,
                          QAST::WVal.new( :value($quasi_ast) ),
                          $quasi_context,
                          QAST::Op.new( :op('list'), |@*UNQUOTE_ASTS ));
    }

    # Adds code to do the signature binding.
    my $use_vm_binder;
    sub add_signature_binding_code($block, $sig_obj, @params) {
        # Set arity.
        my int $arity := 0;
        for @params {
            last if $_<optional> || $_<named_names> ||
               $_<pos_slurpy> || $_<pos_onearg> ||  $_<named_slurpy>;
            $arity := $arity + 1;
        }
        $block.arity($arity);

        # Consider using the VM binder on backends where it will work out
        # (e.g. we can get the same errors).
        my $need_full_binder := 1;
        unless nqp::defined($use_vm_binder) {
            $use_vm_binder := nqp::getcomp('perl6').backend.name eq 'moar';
        }
        if $use_vm_binder {
            # If there are zero parameters, then we can trivially leave it to
            # the VM, with no extra work.
            if nqp::elems(@params) == 0 {
                $need_full_binder := 0;
            }

            # If there is one anonymous capture parameter with no sub-signature
            # or additional constraint, as is common with protos, then we've
            # nothing to check or store; flag as custom bind but don't invoke
            # the binder.
            elsif is_anon_capture(@params) {
                $block.custom_args(1);
                $need_full_binder := 0;
            }

            # If there is a single $_ and it takes its value from outer, then
            # this is easily handled too. Very common case.
            elsif is_default_topic(@params) {
                my $var := find_var_decl($block, '$_');
                $var.decl('param');
                $var.default(QAST::Op.new(
                    :op('getlexouter'),
                    QAST::SVal.new( :value('$_') )
                ));
                $need_full_binder := 0;
            }

            # Otherwise, need to go through and do a fuller analysis.
            else {
                if lower_signature($block, $sig_obj, @params) -> @todo {
                    for @todo {
                        $block[0].push($_);
                    }
                    $need_full_binder := 0;
                }
            }
        }

        # If we need the full binder, invoke it; mark we do custom args
        # handling.
        if $need_full_binder {
            $block.custom_args(1);
            $block[0].push(QAST::Op.new( :op('p6bindsig') ));
        }

        $block;
    }
    sub is_anon_capture(@params) {
        if nqp::elems(@params) == 1 {
            my $only := @params[0];
            if $only<is_capture> && !nqp::existskey($only, 'variable_name')
                                 && !nqp::existskey($only, 'sub_signature')
                                 && !nqp::existskey($only, 'post_constraints') {
                if !nqp::istype($*W.find_symbol(['Capture']), $only<nominal_type>) {
                    $only<node>.CURSOR.panic("Capture parameter must have a type accepting a Capture");
                }
                else {
                    return 1;
                }
            }
        }
        0
    }
    sub is_default_topic(@params) {
        if nqp::elems(@params) == 1 {
            my $only := @params[0];
            if $only<default_from_outer> && $only<is_raw> && $only<variable_name> eq '$_' {
                if $only<nominal_type> =:= $*W.find_symbol(['Mu']) {
                    return 1;
                }
            }
        }
        0
    }
    my $SIG_ELEM_IS_RW       := 256;
    my $SIG_ELEM_IS_RAW      := 1024;
    my $SIG_ELEM_IS_OPTIONAL := 2048;
    my @iscont_ops := ['iscont', 'iscont_i', 'iscont_n', 'iscont_s'];
    sub lower_signature($block, $sig, @params) {
        my @result;
        my $clear_topic_bind;
        my $saw_slurpy;
        my $Sig      := $*W.find_symbol(['Signature'], :setting-only);
        my $Param    := $*W.find_symbol(['Parameter'], :setting-only);
        my $Iterable := $*W.find_symbol(['Iterable']);
        my @p_objs := nqp::getattr($sig, $Sig, '$!params');
        my int $i  := 0;
        my int $n  := nqp::elems(@params);
        while $i < $n {
            # Some things need the full binder to do.
            my %info      := @params[$i];
            my $param_obj := @p_objs[$i];
            my int $flags := nqp::getattr_i($param_obj, $Param, '$!flags');
            return 0 if nqp::existskey(%info, 'sub_signature');
            return 0 if %info<bind_accessor>;                   # XXX Support later
            return 0 if %info<default_from_outer>;

            # Generate a var to bind into.
            my $name := QAST::Node.unique("__lowered_param_");
            my $var  := QAST::Var.new( :$name, :scope('local'), :decl('param') );
            if %info<is_capture> {
                # If this is a final and anonymous capture, then we're good.
                # Otherwise, bail out for now.
                return 0 if $saw_slurpy;
                return 0 unless $i + 1 == $n ||
                                $i + 2 == $n && @params[$i + 1]<named_slurpy>;
                if !nqp::istype($*W.find_symbol(['Capture']), %info<nominal_type>) {
                    %info<node>.CURSOR.panic("Capture parameter must have a type accepting a Capture");
                }
                $var.slurpy(1);
                my $hash_name := $name ~ '_hash';
                @result.push(QAST::Var.new(
                    :name($hash_name), :scope('local'), :decl('param'),
                    :slurpy(1), :named(1)
                ));
                if nqp::existskey(%info, 'variable_name') {
                    # Build a capture object.
                    my $Capture := QAST::WVal.new( :value($*W.find_symbol(['Capture'])) );
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Op.new(
                            :op('p6bindattrinvres'),
                            QAST::Op.new(
                                :op('p6bindattrinvres'),
                                QAST::Op.new( :op('create'), $Capture ),
                                $Capture,
                                QAST::SVal.new( :value('$!list') ),
                                QAST::Var.new( :name($name), :scope('local') )
                            ),
                            $Capture,
                            QAST::SVal.new( :value('$!hash') ),
                            QAST::Var.new( :name($hash_name), :scope('local') )
                        )));
                }
            }
            elsif nqp::existskey(%info, 'named_names') {
                my @names := %info<named_names>;
                if nqp::elems(@names) == 1 {
                    $var.named(@names[0]);
                }
                elsif nqp::elems(@names) == 2 {
                    $var.named(@names);
                }
                else {
                    return 0;
                }
            }
            elsif %info<pos_slurpy> || %info<pos_lol> || %info<pos_onearg> {
                $var.slurpy(1);
                my $type := $*W.find_symbol(
                    [$flags +& $SIG_ELEM_IS_RAW || $flags +& $SIG_ELEM_IS_RW ?? 'List' !! 'Array' ]);
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('callmethod'),
                        :name(%info<pos_onearg> ?? 'from-slurpy-onearg' !! %info<pos_lol> ?? 'from-slurpy' !! 'from-slurpy-flat'),
                        QAST::WVal.new( :value($type) ),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                $saw_slurpy := 1;
            }
            elsif %info<named_slurpy> {
                $var.slurpy(1);
                $var.named(1);
                my $slurpy_setup := QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('p6bindattrinvres'),
                        QAST::Op.new(
                            :op('create'),
                            QAST::WVal.new( :value($*W.find_symbol(['Hash'])) )
                        ),
                        QAST::WVal.new( :value($*W.find_symbol(['Map'])) ),
                        QAST::SVal.new( :value('$!storage') ),
                        QAST::Var.new( :name($name), :scope('local') )
                    ));
                if nqp::existskey(%info, 'variable_name') && %info<variable_name> eq '%_' {
                    $slurpy_setup.annotate('autoslurpy', 1);
                }
                $var.push($slurpy_setup);
                $saw_slurpy := 1;
            }

            # Add type checks.
            my $nomtype   := %info<nominal_type>;
            my int $is_generic := %info<nominal_generic>;
            my int $is_rw := $flags +& $SIG_ELEM_IS_RW;
            my int $spec  := nqp::objprimspec($nomtype);
            if $spec && !%info<nominal_generic> {
                if $is_rw {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op(@iscont_ops[$spec]),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
                else {
                    $var.returns($nomtype);
                }
            }
            elsif !$saw_slurpy {
                # Must hll-ize before we go on.
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('hllize'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));

                # Type-check, unless it's Mu, in which case skip it.
                if $is_generic {
                    my $genericname := $nomtype.HOW.name(%info<attr_package>);
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('istype'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Var.new( :name($genericname), :scope<typevar> )
                    )));
                } elsif !($nomtype =:= $*W.find_symbol(['Mu'])) {
                    if $nomtype.HOW.archetypes.generic {
                        return 0 unless %info<is_invocant>;
                    }
                    else {
                        if $nomtype =:= $*W.find_symbol(['Positional']) {
                            $var.push(QAST::Op.new(
                                :op('if'),
                                QAST::Op.new(
                                    :op('istype'),
                                    QAST::Var.new( :name($name), :scope('local') ),
                                    QAST::WVal.new( :value($*W.find_symbol(['PositionalBindFailover'])) )
                                ),
                                QAST::Op.new(
                                    :op('bind'),
                                    QAST::Var.new( :name($name), :scope('local') ),
                                    QAST::Op.new(
                                        :op('callmethod'), :name('cache'),
                                        QAST::Var.new( :name($name), :scope('local') )
                                    ))));
                        }
                        $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                            :op('istype'),
                            QAST::Var.new( :name($name), :scope('local') ),
                            QAST::WVal.new( :value($nomtype) )
                        )));
                    }
                }
                if %info<undefined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('not_i'),
                        QAST::Op.new(
                            :op('isconcrete'),
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
                }
                if %info<defined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isconcrete'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
                if $is_rw {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isrwcont'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
            }

            # Handle coercion.
            my $coerce_to := nqp::getattr($param_obj, $Param, '$!coerce_type');
            unless nqp::isnull($coerce_to) {
                if $coerce_to.HOW.archetypes.generic {
                    return 0;
                }
                $var.push(QAST::Op.new(
                    :op('unless'),
                    QAST::Op.new(
                        :op('istype'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::WVal.new( :value($coerce_to) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Op.new(
                            :op('callmethod'),
                            :name(nqp::getattr_s($param_obj, $Param, '$!coerce_method')),
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
            }

            # If it's optional, do any default handling.
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                if nqp::existskey(%info, 'default_value') {
                    my $wval := QAST::WVal.new( :value(%info<default_value>) );
                    if %info<default_is_literal> {
                        $var.default($wval);
                    }
                    else {
                        $var.default(QAST::Op.new(
                            :op('call'),
                            QAST::Op.new(
                                :op('p6capturelex'),
                                QAST::Op.new( :op('callmethod'), :name('clone'), $wval )
                            )));
                    }
                }
                else {
                    if %info<sigil> eq '@' {
                        $var.default(
                                QAST::Op.new( :op<create>,
                                              QAST::WVal.new( :value($*W.find_symbol(['Array'])) )
                            ));
                    }
                    elsif %info<sigil> eq '%' {
                        $var.default(
                                QAST::Op.new( :op<create>,
                                              QAST::WVal.new( :value($*W.find_symbol(['Hash'])) )
                            ));
                    }
                    else {
                        if $spec == 1 {
                            $var.default(QAST::IVal.new( :value(0) ));
                        }
                        elsif $spec == 2 {
                            $var.default(QAST::NVal.new( :value(0.0) ));
                        }
                        elsif $spec == 3 {
                            $var.default(QAST::SVal.new( :value('') ));
                        }
                        else {
                            $var.default(QAST::WVal.new( :value($nomtype) ));
                        }
                    }
                }
            }

            # If there are type captures involved - most commonly $?CLASS and
            # ::?CLASS - we emit a piece of code for each target that gets the
            # WHAT of the given value and binds it.
            #
            # In theory, we could bind a local with the result of the WHAT
            # operation, but I'm not convinced it's sufficiently expensive.
            if %info<type_captures> {
                for %info<type_captures> {
                    $var.push( QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new( :name($_), :scope<lexical> ),
                        QAST::Op.new( :op<what>,
                            QAST::Var.new( :name($name), :scope<local> ) )
                        )
                    );
                }
            }


            # If it's the invocant, needs to go into self also.
            if %info<is_invocant> {
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name('self'), :scope('lexical') ),
                    QAST::Op.new(
                        :op('decont'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
            }

            # Bind to lexical if needed.
            if nqp::existskey(%info, 'variable_name') && !%info<bind_attr> {
                if nqp::objprimspec($nomtype) || $is_rw || $flags +& $SIG_ELEM_IS_RAW {
                    my $scope := $spec && $is_rw ?? 'lexicalref' !! 'lexical';
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name(%info<variable_name>), :$scope, :returns($nomtype) ),
                        QAST::Var.new( :name($name), :scope('local') )
                    ));
                }
                elsif %info<sigil> eq '@' {
                    if $flags +& $SIG_ELEM_IS_COPY {
                        $var.push(
                            QAST::Op.new( :op<bind>,
                                QAST::Var.new( :name( my $array_copy_var := $block.unique('array_copy_var') ), :scope<local>, :decl<var> ),
                                QAST::Op.new( :op<create>,
                                        QAST::WVal.new( :value($*W.find_symbol(['Array'])) )
                                    )));
                        $var.push(
                            QAST::Op.new( :op<callmethod>, :name<STORE>,
                                QAST::Var.new( :name($array_copy_var), :scope<local> ),
                                QAST::Op.new( :op('decont'),
                                    QAST::Var.new( :name($name), :scope('local') )
                            ) ));
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Var.new( :name($array_copy_var), :scope<local> )
                            ));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                }
                elsif %info<sigil> eq '%' {
                    if $flags +& $SIG_ELEM_IS_COPY {
                        $var.push(
                            QAST::Op.new( :op<bind>,
                                QAST::Var.new( :name( my $hash_copy_var := $block.unique('hash_copy_var') ), :scope<local>, :decl<var> ),
                                QAST::Op.new( :op<create>,
                                        QAST::WVal.new( :value($*W.find_symbol(['Hash'])) )
                                    )));
                        $var.push(
                            QAST::Op.new( :op<callmethod>, :name<STORE>,
                                QAST::Var.new( :name($hash_copy_var), :scope<local> ),
                                QAST::Op.new( :op('decont'),
                                    QAST::Var.new( :name($name), :scope('local') )
                            ) ));
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Var.new( :name($hash_copy_var), :scope<local> )
                            ));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                }
                else {
                    # If the type means there's no way this could possible be
                    # Iterable, we know it can't flatten. This in turn means
                    # we need not wrap it in a read-only scalar.
                    my $wrap := $flags +& $SIG_ELEM_IS_COPY;
                    unless $wrap {
                        $wrap := nqp::istype($nomtype, $Iterable) || nqp::istype($Iterable, $nomtype);
                    }
                    if $wrap {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            WANTED(QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),'lower_signature/wrap'),
                            QAST::Op.new(
                                :op('assignunchecked'),
                                QAST::Op.new(
                                    :op('p6scalarfromdesc'),
                                    QAST::WVal.new( :value(%info<container_descriptor>) )
                                ),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            WANTED(QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),'lower_signature'),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }

                    # Take care we don't undo explicit $_ bindings.
                    if %info<variable_name> eq '$_' && $*IMPLICIT {
                        for $block[0].list {
                            if nqp::istype($_, QAST::Op) && $_.op eq 'bind' &&
                               nqp::istype($_[0], QAST::Var) && $_[0].name eq '$_' {
                                $clear_topic_bind := $_;
                                last;
                            }
                        }
                    }
                }
            }

            # Apply post-constraints (must come after variable bind, as constraints can
            # refer to the var).
            if %info<post_constraints> {
                for %info<post_constraints> {
                    my $wval := QAST::WVal.new( :value($_) );
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('istrue'),
                        QAST::Op.new(
                            :op('callmethod'), :name('ACCEPTS'),
                            nqp::istype($_, $*W.find_symbol(['Code'], :setting-only))
                                ?? QAST::Op.new( :op('p6capturelex'),
                                      QAST::Op.new( :op('callmethod'), :name('clone'), $wval ) )
                                !! $wval,
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
                }
            }

            # If it's an attributive parameter, do the bind.
            if %info<bind_attr> {
                # If the type given for the attr_package is generic, we're
                # dealing with a role and have to look up what type it's
                # supposed to grab the attribute from during run-time.
                if %info<attr_package>.HOW.archetypes.generic {
                    my $packagename := %info<attr_package>.HOW.name(%info<attr_package>);
                    $var.push(QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new(
                            :name(%info<variable_name>), :scope('attribute'),
                            QAST::Var.new( :name('self'), :scope('lexical') ),
                            QAST::Var.new( :name($packagename), :scope('typevar') )
                        ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($name), :scope('local') )
                        )));
                }
                else {
                    $var.push(QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new(
                            :name(%info<variable_name>), :scope('attribute'),
                            QAST::Var.new( :name('self'), :scope('lexical') ),
                            QAST::WVal.new( :value(%info<attr_package>) )
                        ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($name), :scope('local') )
                        )));
                }
            }

            # Add the generated var.
            nqp::push(@result, $var);

            $i++;
        }
        if $clear_topic_bind {
            $clear_topic_bind.shift(); $clear_topic_bind.shift();
            $clear_topic_bind.op('null');
        }
        @result
    }
    sub find_var_decl($block, $name) {
        for $block[0].list {
            if nqp::istype($_, QAST::Var) && $_.name eq $name && $_.decl {
                return $_;
            }
        }
        nqp::die("Internal error: find_var_decl could not find $name");
    }

    # Adds a placeholder parameter to this block's signature.
    sub add_placeholder_parameter($/, $sigil, $ident, :$named, :$pos_slurpy, :$named_slurpy, :$full_name) {
        my $block := $*W.cur_lexpad();

        # don't allow $^A..Z as placeholders, as per spec
        if nqp::chars($full_name) == 3 && nqp::substr($full_name,2,1) ~~ /^<[A..Z]>$/ {
            $*W.throw($/, ['X', 'Syntax', 'Perl5Var'], name => $full_name );
        }

        # ensure we're not trying to put a placeholder in the mainline.
        elsif $block.ann('IN_DECL') eq 'mainline' || $block.ann('IN_DECL') eq 'eval' {
            $*W.throw($/, ['X', 'Placeholder', 'Mainline'],
                placeholder => $full_name,
            );
        }

        # Obtain/create placeholder parameter list.
        my @params := $block.ann('placeholder_sig') || $block.annotate('placeholder_sig', []);

        # If we already declared this as a placeholder, we're done.
        my $name := ~$sigil ~ ~$ident;
        if $block.ann('also_uses') && $block.ann('also_uses'){$name} {
            $*W.throw($/, ['X', 'Placeholder', 'NonPlaceholder'],
                placeholder   => $full_name,
                variable_name => $name,
                decl          => ~$block.ann('IN_DECL'),
            )
        }
        for @params {
            if $_<variable_name> eq $name {
                return QAST::Var.new( :name($name), :scope('lexical') );
            }
        }

        # Make descriptor.
        my %param_info := hash(
            variable_name     => $name,
            pos_slurpy        => $pos_slurpy,
            named_slurpy      => $named_slurpy,
            placeholder       => $full_name,
            is_multi_invocant => 1,
            sigil             => ~$sigil);

        # Apply any type implied by the sigil.
        if $sigil eq '@' {
            %param_info<nominal_type> := $*W.find_symbol(['Positional']);
        }
        elsif $sigil eq '%' {
            %param_info<nominal_type> := $*W.find_symbol(['Associative']);
        }
        elsif $sigil eq '&' {
            %param_info<nominal_type> := $*W.find_symbol(['Callable']);
        }

        # If it's slurpy, just goes on the end.
        if $pos_slurpy || $named_slurpy {
            @params.push(%param_info);
        }

        # If it's named, just shove it on the end, but before any slurpies.
        elsif $named {
            %param_info<named_names> := [$ident];
            my @popped;
            while @params
                    && (@params[+@params - 1]<pos_slurpy> || @params[+@params - 1]<named_slurpy>) {
                @popped.push(@params.pop);
            }
            @params.push(%param_info);
            while @popped { @params.push(@popped.pop) }
        }

        # Otherwise, put it in correct lexicographic position.
        else {
            my int $insert_at := 0;
            for @params {
                last if $_<pos_slurpy> || $_<named_slurpy> ||
                        $_<named_names> ||
                        nqp::substr($_<variable_name>, 1) gt $ident;
                $insert_at := $insert_at + 1;
            }
            nqp::splice(@params, [%param_info], $insert_at, 0);
        }

        # Add variable declaration, and evaluate to a lookup of it.
        my %existing := $block.symbol($name);
        if +%existing && !%existing<placeholder_parameter> {
            $*W.throw($/, ['X', 'Redeclaration'],
                symbol  => ~$/,
                postfix => ' as a placeholder parameter',
            );
        }
        $block[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        $block.symbol($name, :scope('lexical'), :placeholder_parameter(1));
        QAST::Var.new( :name($name), :scope('lexical') )
    }

    sub reference_to_code_object($code_obj, $past_block) {
        my $ref := QAST::WVal.new( :value($code_obj) );
        $ref.annotate('past_block', $past_block);
        $ref.annotate('code_object', $code_obj);
        $ref
    }

    our sub make_thunk_ref($to_thunk, $/) {
        my $block := $*W.push_lexpad($/);
        fatalize($to_thunk) if %*PRAGMAS<fatal>;
        $block.push(QAST::Stmts.new(autosink($to_thunk)));
        $*W.pop_lexpad();
        reference_to_code_object(
            $*W.create_simple_code_object($block, 'Code'),
            $block);
    }

    sub make_topic_block_ref($/, $past, :$copy, :$migrate_stmt_id) {
        my $block := $*W.push_lexpad($/);
        $block[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
        $block.push($past);
        $*W.pop_lexpad();
        if nqp::defined($migrate_stmt_id) {
            migrate_blocks($*W.cur_lexpad(), $block, -> $b {
                !$b.ann('in_stmt_mod') && ($b.ann('statement_id') // -1) == $migrate_stmt_id
            });
        }
        ($*W.cur_lexpad())[0].push($block);
        my $param := hash( :variable_name('$_'), :nominal_type($*W.find_symbol(['Mu'])));
        if $copy {
            $param<container_descriptor> := $*W.create_container_descriptor(
                    $*W.find_symbol(['Mu']), 0, '$_'
            );
        }
        my $param_obj := $*W.create_parameter($/, $param);
        if $copy { $param_obj.set_copy() } else { $param_obj.set_raw() }
        my $sig := $*W.create_signature(nqp::hash('parameter_objects', [$param_obj]));
        add_signature_binding_code($block, $sig, [$param]);
        reference_to_code_object(
            $*W.create_code_object($block, 'Block', $sig),
            $block)
    }

    sub make_where_block($/, $expr, $operand = WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'where') ) {
        # If it's already a block, nothing to do at all.
        if $expr.ann('past_block') {
            $expr.annotate('past_block', wanted($expr.ann('past_block'),'make_where_block'));
            return $expr.ann('code_object');
        }

        # Build a block that'll smartmatch the topic against the
        # expression.
        check_smartmatch($/,$expr);
        my $past := QAST::Block.new(
            QAST::Stmts.new(
                QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') )
            ),
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('callmethod'), :name('ACCEPTS'),
                    $expr,
                    $operand,
                )));
        ($*W.cur_lexpad())[0].push($past);

        # Give it a signature and create code object.
        my $param := hash(
            variable_name => '$_',
            nominal_type => $*W.find_symbol(['Mu']));
        my $sig := $*W.create_signature(nqp::hash('parameter_objects',
            [$*W.create_parameter($/, $param)]));
        add_signature_binding_code($past, $sig, [$param]);
        $*W.create_code_object($past, 'Block', $sig)
    }

    sub when_handler_helper($when_block) {
        unless nqp::existskey(%*HANDLERS, 'SUCCEED') {
            %*HANDLERS<SUCCEED> := QAST::Op.new(
                :op('p6return'),
                wrap_return_type_check(
                    QAST::Op.new(
                        :op('getpayload'),
                        QAST::Op.new( :op('exception') )
                    ),
                    $*DECLARAND) );
        }

        # if this is not an immediate block create a call
        if $when_block.ann('past_block') {
            $when_block := QAST::Op.new( :op('call'), block_closure($when_block) );
        }

        # call succeed with the block return value, succeed will throw
        # a BREAK exception to be caught by the above handler
        my $result := WANTED(QAST::Op.new(
            :op('call'),
            :name('&succeed'),
            $when_block,
        ), 'when_handler_helper');

        # wrap it in a handle op so that we can use a PROCEED exception
        # to skip the succeed call
        QAST::Op.new(
            :op('handle'),
            $result,
            'PROCEED',
            QAST::Op.new(
                :op('getpayload'),
                QAST::Op.new( :op('exception') )
            )
        )
    }

    sub make_dot_equals($target, $call) {
        $call.unshift($*W.add_string_constant($call.name)) if $call.name || !$call.list;
        $call.unshift(WANTED($target,'make_dot_equals'));
        $call.name('dispatch:<.=>');
        $call.op('callmethod');
        wantall($call, 'make_dot_equals');
        $call;
    }

    sub make_dot($target, $call) {
        $*W.add_string_constant($call.name);
        $call.unshift(WANTED($target,'make_dot'));
        $call.op('callmethod');
        wantall($call, 'make_dot');
        $call;
    }

    # XXX This isn't quite right yet... need to evaluate these semantics
    sub set_block_handler($/, $handler, $type) {
        # Handler needs its own $/ and $!.
        $*W.install_lexical_magical($handler.ann('past_block'), '$!');
        $*W.install_lexical_magical($handler.ann('past_block'), '$/');

        # unshift handler preamble: create exception object and store it into $_
        my $exceptionreg := $handler.unique('exception_');
        $handler.ann('past_block')[0].unshift(QAST::Var.new(
            :scope('local'), :name($exceptionreg), :decl('param')
        ));
        $handler.ann('past_block')[0].push(QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                WANTED(QAST::Var.new( :scope('lexical'), :name('$_') ),'set_block_handler'),
                QAST::Op.new(
                    :op('call'), :name('&EXCEPTION'),
                    QAST::Var.new( :scope('local'), :name($exceptionreg) )
                )
            ),
            QAST::Op.new( :op('p6store'),
                QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$!') ) ),
                WANTED(QAST::Var.new( :scope('lexical'), :name('$_') ),'set_block_handler'),
            )
        ));

        # If the handler has a succeed handler, then make sure we sink
        # the exception it will produce.
        if $handler.ann('past_block').ann('handlers') && nqp::existskey($handler.ann('past_block').ann('handlers'), 'SUCCEED') {
            my $suc := $handler.ann('past_block').ann('handlers')<SUCCEED>;
            $suc[0] := QAST::Stmts.new(
                sink(QAST::Op.new(
                    :op('getpayload'),
                    QAST::Op.new( :op('exception') )
                )),
                QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
            );
        }

        # set up a generic exception rethrow, so that exception
        # handlers from unwanted frames will get skipped if the
        # code in our handler throws an exception.
        my $ex := QAST::Op.new( :op('exception') );
        if $handler.ann('past_block').ann('handlers') && nqp::existskey($handler.ann('past_block').ann('handlers'), $type) {
        }
        else {
            my $prev_content := QAST::Stmts.new();
            $prev_content.push($handler.ann('past_block').shift()) while +@($handler.ann('past_block'));
            $prev_content.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
            $handler.ann('past_block').push(QAST::Op.new(
                :op('handle'),
                $prev_content,
                'CATCH',
                QAST::Op.new(
                    :op('rethrow'),
                    QAST::Op.new( :op('exception') )
                )));

            # rethrow the exception if we reach the end of the handler
            # (if a when {} clause matches this will get skipped due
            # to the BREAK exception)
            $handler.ann('past_block').push(QAST::Op.new(
                :op('rethrow'),
                QAST::Var.new( :name($exceptionreg), :scope('local') )));
        }

        # Install the handler, taking care over scoping.
        my $handler_lex_name := $handler.unique('__HANDLER_');
        $*W.cur_lexpad()[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($handler_lex_name), :scope('lexical'), :decl('var') ),
            block_closure($handler)
        ));
        %*HANDLERS{$type} := QAST::Stmts.new(
            :node($/),
            QAST::Op.new(
                :op('p6invokehandler'),
                QAST::Var.new( :name($handler_lex_name), :scope('lexical') ),
                $ex
            ),
            QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
        );
    }

    # Handles the case where we have a default value closure for an
    # attribute.
    method install_attr_init($/, $attr, $initializer, $block) {
        # Construct signature and anonymous method.
        if $block.ann('placeholder_sig') {
            $*W.throw($/, 'X::Placeholder::Attribute',
                precursor => '1',
                placeholder => $block.ann('placeholder_sig')[0]<placeholder>,
            );
        }
        my @params := [
            hash( is_invocant => 1, nominal_type => $*PACKAGE),
            hash( variable_name => '$_', nominal_type => $*W.find_symbol(['Mu']))
        ];
        my $sig := $*W.create_signature(nqp::hash('parameter_objects', [
            $*W.create_parameter($/, @params[0]),
            $*W.create_parameter($/, @params[1])
        ]));
        $block[0].push(QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') ));
        $block[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
        $block.push(QAST::Stmts.new( WANTED($initializer, 'install_attr_init'), :node($/) ));
        $block.symbol('self', :scope('lexical'));
        add_signature_binding_code($block, $sig, @params);
        $block.blocktype('declaration_static');
        my $code := $*W.create_code_object($block, 'Method', $sig);

        # Block should go in current lexpad, in correct lexical context.
        ($*W.cur_lexpad())[0].push($block);

        # Dispatch trait.
        $*W.apply_trait($/, '&trait_mod:<will>', $attr, :build($code));
    }

    # This is the hook where, in the future, we'll use this as the hook to check
    # if we have a proto or other declaration in scope that states that this sub
    # has a signature of the form :(\|$raw), in which case we don't promote
    # the raw object to a Capture when calling it. For now, we just worry about the
    # special case, return.
    sub capture_or_raw($/,$args, $name) {
        if $name eq 'sink' {
            return $args;   # Note that sink itself wants its args, to eat 'em...
        }
        if $name eq 'return' {
            my $ret := %*SIG_INFO<returns>;
            if nqp::isconcrete($ret) || $ret.HOW.name($ret) eq 'Nil' {
                if nqp::elems($args) {
                    $*W.throw($/, 'X::Comp::AdHoc',
                        payload => "No return arguments allowed when return value {$ret.perl} is already specified in the signature");
                }
                $args.push(QAST::WVal.new( :value($ret) ));
            }
            # Need to demote pairs again.
            my $raw := QAST::Op.new( :op('call') );
            for @($args) {
                $raw.push($_.ann('before_promotion') || $_);
            }
            $args := $raw;
        }
        elsif $name eq 'EVAL' {
            my $all_literal := 1;
            for @($args) {
                $all_literal := 0 unless nqp::istype($_,QAST::SpecialArg) ||
                    nqp::istype($_,QAST::Want) && nqp::istype($_[0],QAST::WVal) && $_[1] eq 'Ss' && nqp::istype($_[2],QAST::SVal);
            }
            $*W.throw($/, 'X::SecurityPolicy::Eval') unless $all_literal || monkey_see_no_eval();
        }
        WANTALL($args, 'capture_or_raw');
        $args;
    }

    # This checks if we have something of the form * op *, * op <thing> or
    # <thing> op * and if so, and if it's not one of the ops we do not
    # auto-curry for, emits a closure instead. We hard-code the things not
    # to curry for now; in the future, we will inspect the multi signatures
    # of the op to decide, or likely store things in this hash from that
    # introspection and keep it as a quick cache.

    # %curried == 0 means do not curry
    # %curried == 1 means curry Whatever only
    # %curried == 2 means curry WhateverCode only
    # %curried == 3 means curry both Whatever and WhateverCode (default)

    my %curried;
    INIT {
        %curried{'&infix:<...>'}  := 0;
        %curried{'&infix:<…>'}  := 0;
        %curried{'&infix:<...^>'} := 0;
        %curried{'&infix:<…^>'} := 0;
        %curried{'&infix:<=>'}    := 0;
        %curried{'&infix:<:=>'}   := 0;
        %curried{'&infix:<~~>'}   := 1;
        %curried{'&infix:<∘>'}   := 1;
        %curried{'&infix:<o>'}   := 1;
        %curried{'&infix:<..>'}   := 2;
        %curried{'&infix:<..^>'}  := 2;
        %curried{'&infix:<^..>'}  := 2;
        %curried{'&infix:<^..^>'} := 2;
        %curried{'&infix:<xx>'}   := 2;
        %curried{'callmethod'}    := 3;
        %curried{'p6callmethodhow'}      := 3;
        %curried{'&postcircumfix:<[ ]>'} := 3;
        %curried{'&postcircumfix:<{ }>'} := 3;
    }
    method whatever_curry($/, $past, $upto_arity) {
        my int $curried :=
            # It must be an op and...
            nqp::istype($past, QAST::Op) && (

            # Either a call that we're allowed to curry...
                (($past.op eq 'call' || $past.op eq 'chain') &&
                    (nqp::eqat($past.name, '&infix:', 0) ||
                     nqp::eqat($past.name, '&prefix:', 0) ||
                     nqp::eqat($past.name, '&postfix:', 0) ||
                     (nqp::istype($past[0], QAST::Op) &&
                        nqp::eqat($past[0].name, '&METAOP', 0))) &&
                    %curried{$past.name} // 3)

            # Or not a call and an op in the list of alloweds.
                || ($past.op ne 'call' && %curried{$past.op} // 0)

            # or one of our new postcircumfix subs that used to be methods
                || ($past.op eq 'call' && nqp::eqat($past.name, '&postcircumfix:', 0) &&
                    %curried{$past.name} // 0)
            );

        return $past unless $curried;

        my int $offset := 0;
        $offset := nqp::elems($past) - $upto_arity if $past.op eq 'call' && !nqp::eqat($past.name,"&postcircumfix:",0);
        my int $i := $offset;
        my int $e := $upto_arity + $offset;
        my int $whatevers := 0;
        my int $hyperwhatever := 0;

        my $Whatever := $*W.find_symbol(['Whatever'], :setting-only);
        my $WhateverCode := $*W.find_symbol(['WhateverCode'], :setting-only);
        my $HyperWhatever := $*W.find_symbol(['HyperWhatever']);

        while $i < $e {
            my $check := $past[$i];
            $check := $check[0] if (nqp::istype($check, QAST::Stmts) ||
                                    nqp::istype($check, QAST::Stmt)) &&
                                   +@($check) == 1;
            $whatevers++ if nqp::bitand_i($curried, 1) && istype($check.returns, $Whatever) && nqp::isconcrete($check.value)
                         || nqp::bitand_i($curried, 2) && istype($check.returns, $WhateverCode) && $check ~~ QAST::Op;
            if nqp::bitand_i($curried, 1) && istype($check.returns, $HyperWhatever) {
                $hyperwhatever := 1;
                $whatevers++;
            }
            $i++;
        }
        if $whatevers {
            if $hyperwhatever && $whatevers > 1 {
                $*W.throw($/, ['X', 'HyperWhatever', 'Multiple']);
            }
            my $was_chain := $past.op eq 'chain' ?? $past.name !! NQPMu;
            my int $i := 0;
            my @params;
            my @old_args;
            my $block := QAST::Block.new(QAST::Stmts.new(), $past);
            $*W.cur_lexpad()[0].push($block);
            while $i < $e {
                my $old := $past[$i];
                $old := $old[0] if (nqp::istype($old, QAST::Stmts) ||
                                    nqp::istype($old, QAST::Stmt)) &&
                                   +@($old) == 1;
                if nqp::bitand_i($curried, 2) && istype($old.returns, $WhateverCode) && $old ~~ QAST::Op {
                    my $new;
                    if $was_chain && $old.has_ann("chain_args") {
                        $new := QAST::Op.new( :op<chain>, :name($old.ann('chain_name')), :node($/) );
                        $old.ann('chain_block')[1] := QAST::Op.new( :op<die>, QAST::SVal.new( :value('This WhateverCode has been inlined into another WhateverCode and should not have been called!') ) );
                        $old.ann('chain_block')[0] := QAST::Stmts.new( );
                        for $old.ann('chain_past') {
                            $new.push($_);
                        }
                        for $old.ann('chain_args') -> %arg {
                            @params.push(%arg);
                            $block[0].push(QAST::Var.new(:name(%arg<variable_name>), :scope<lexical>, :decl<var>));
                        }
                        nqp::push(@old_args, $new);
                    } else {
                        # Have to move the nested thunk inside this one, to get the
                        # correct lexical scoping.
                        my $old_ast := $old.ann('past_block');
                        remove_block($*W.cur_lexpad(), $old_ast);
                        $block[0].push($old_ast);
                        $new := QAST::Op.new( :op<call>, :node($/), $old );
                        my $acount := 0;
                        while $acount < $old.arity {
                            my $pname := $*W.cur_lexpad()[0].unique('$whatevercode_arg');
                            @params.push(hash(
                                :variable_name($pname),
                                :nominal_type($*W.find_symbol(['Mu'])),
                                :is_raw(1),
                            ));
                            $block[0].push(QAST::Var.new(:name($pname), :scope<lexical>, :decl<var>));
                            my $to_push := QAST::Var.new(:name($pname), :scope<lexical>);
                            $new.push($to_push);
                            nqp::push(@old_args, $to_push) if $was_chain;
                            $acount++;
                        }
                    }
                    $past[$i] := $new;
                }
                elsif nqp::bitand_i($curried, 1) && (istype($old.returns, $Whatever) || istype($old.returns, $HyperWhatever)) && nqp::isconcrete($old.value) {
                    my $pname := $*W.cur_lexpad()[0].unique('$whatevercode_arg');
                    @params.push(hash(
                        :variable_name($pname),
                        :nominal_type($*W.find_symbol(['Mu'])),
                        :is_raw(1),
                    ));
                    $block[0].push(QAST::Var.new(:name($pname), :scope<lexical>, :decl('var')));
                    $past[$i] := QAST::Var.new(:name($pname), :scope<lexical>);
                    nqp::push(@old_args, $past[$i]) if $was_chain;
                } else {
                    nqp::push(@old_args, $past[$i]) if $was_chain;
                }
                $i++;
            }
            my %sig_info := hash(parameters => @params);
            my $signature := $*W.create_signature_and_params($/, %sig_info, $block, 'Mu');
            add_signature_binding_code($block, $signature, @params);
            my $code := $*W.create_code_object($block, 'WhateverCode', $signature);
            $past := block_closure(reference_to_code_object($code, $block));
            $past.returns($WhateverCode);
            $past.arity(+@params);
            if $was_chain {
                $past.annotate('chain_past', @old_args);
                $past.annotate('chain_args', @params);
                $past.annotate('chain_name', $was_chain);
                $past.annotate('chain_block', $block);
            }
            if $hyperwhatever {
                $past := QAST::Op.new( :op<call>, :name<&HYPERWHATEVER>, $past );
            }
        }
        $past;
    }

    sub remove_block($from, $block) {
        # Remove the QAST::Block $block from $from[0]; die if not found.
        my @decls := $from[0].list;
        my int $i := 0;
        my int $n := nqp::elems(@decls);
        while $i < $n {
            my $consider := @decls[$i];
            if $consider =:= $block {
                @decls[$i] := QAST::Op.new( :op('null') );
                return 1;
            }
            elsif nqp::istype($consider, QAST::Stmt) || nqp::istype($consider, QAST::Stmts) {
                if $consider[0] =:= $block {
                    $consider[0] := QAST::Op.new( :op('null') );
                    return 1;
                }
            }
            $i++;
        }
        nqp::die('Internal error: failed to remove block');
    }

    sub wrap_return_type_check($wrappee, $code_obj) {
        my $ret := %*SIG_INFO<returns>;
        return $wrappee if nqp::isconcrete($ret) || $ret.HOW.name($ret) eq 'Nil';
        QAST::Op.new(
            :op('p6typecheckrv'),
            $wrappee,
            QAST::WVal.new( :value($code_obj) ),
            QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
        );
    }

    sub wrap_return_handler($past) {
        wrap_return_type_check(
            QAST::Stmts.new(
                :resultchild(0),
                QAST::Op.new(
                    :op<lexotic>, :name<RETURN>,
                    # If we fall off the bottom, decontainerize if
                    # rw not set.
                    QAST::Op.new( :op('p6decontrv'), QAST::WVal.new( :value($*DECLARAND) ), $past )
                ),
                QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:name<RETURN>, :scope<lexical>),
                    QAST::Var.new(:name<&EXHAUST>, :scope<lexical>))
            ),
            $*DECLARAND
        )
    }

    # Works out how to look up a type. If it's not generic and is in an SC, we
    # statically resolve it. Otherwise, we punt to a runtime lexical lookup.
    sub instantiated_type(@name, $/) {
        CATCH {
            $*W.throw($/, ['X', 'NoSuchSymbol'], symbol => join('::', @name));
        }
        my $type := $*W.find_symbol(@name);
        my $is_generic := 0;
        try { $is_generic := $type.HOW.archetypes.generic }
        my $past;
        if $is_generic || nqp::isnull(nqp::getobjsc($type)) {
            $past := $*W.symbol_lookup(@name, $/);
            $past.set_compile_time_value($type);
        }
        else {
            $past := QAST::WVal.new( :value($type) );
        }
        $past.returns($type.WHAT);
        $past
    }

    # Ensures that the given PAST node has a value known at compile
    # time and if so obtains it. Otherwise reports an error, involving
    # the $usage parameter to make it more helpful.
    sub compile_time_value_str($past, $usage, $/) {
        if $past.has_compile_time_value {
            nqp::unbox_s($past.compile_time_value);
        }
        else {
            $*W.throw($/, ['X', 'Value', 'Dynamic'], what => $usage);
        }
    }

    sub istype($val, $type) {
        try { return nqp::istype($val, $type) }
        0
    }
}

class Perl6::QActions is HLL::Actions does STDActions {
    method nibbler($/) {
        my @asts;
        my $lastlit := '';

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                if nqp::istype($_.ast, QAST::Node) {
                    if $lastlit ne '' {
                        @asts.push($*W.add_string_constant($lastlit));
                        $lastlit := '';
                    }
                    @asts.push($_.ast.ann('ww_atom')
                        ?? WANTED($_.ast, 'nibbler1')
                        !! QAST::Op.new( :op('callmethod'), :name('Stringy'),  WANTED($_.ast, 'nibbler2') ));
                }
                else {
                    $lastlit := $lastlit ~ $_.ast;
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }
        if $lastlit ne '' || !@asts {
            @asts.push($*W.add_string_constant($lastlit));
        }

        my $past := @asts.shift();
        for @asts {
            $past := QAST::Op.new( :op('call'), :name('&infix:<~>'), $past, $_ );
        }

        if nqp::can($/.CURSOR, 'postprocessors') {
            for $/.CURSOR.postprocessors -> $pp {
                $past := self."postprocess_$pp"($/, $past);
            }
        }

        $past.node($/);
        make $past;
    }

    method postprocess_run($/, $past) {
        QAST::Op.new( :name('&QX'), :op('call'), :node($/), $past )
    }

    method postprocess_val($/, $qast) {
        if nqp::istype($qast, QAST::Stmts) && nqp::istype($qast[0], QAST::Op) && $qast[0].name eq '&infix:<,>' { # qw/qqww list
            my @results := [];

            for $qast[0].list -> $thisq {
                if $thisq.has_compile_time_value {
                    try {
                        my $result := $*W.find_symbol(['&val'])($thisq.compile_time_value);
                        $*W.add_object($result);
                        nqp::push(@results, QAST::WVal.new(:value($result), :node($/)));

                        CATCH { nqp::push(@results, $thisq) }
                    }
                } else {
                   nqp::push(@results, QAST::Op.new(:name('&val'), :op('call'), :node($/), $thisq));
                }
            }

            # replace the existing children with what we processed
            $qast[0].set_children(@results);
            $qast[0].annotate("qw",1);
        } elsif $qast.has_compile_time_value { # a single string that we can handle
            try {
                my $result := $*W.find_symbol(['&val'])($qast.compile_time_value);
                $*W.add_object($result);
                $qast := QAST::WVal.new(:value($result));

                CATCH { }
            }
        } else { # no compile time value, resort to the run-time call
            $qast := QAST::Op.new(:name('&val'), :op('call'), :node($/), $qast);
        }

        $qast
    }

    method postprocess_words($/, $past) {
        if $past.has_compile_time_value {
            my @words := HLL::Grammar::split_words($/,
                nqp::unbox_s($past.compile_time_value));
            if +@words != 1 {
                $past := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
                for @words { $past.push($*W.add_string_constant(~$_)); }
                $past := QAST::Stmts.new($past);
            }
            else {
                $past := $*W.add_string_constant(~@words[0]);
            }
        }
        else {
            $past := QAST::Op.new( :op('callmethod'), :name('words'), :node($/), $past, QAST::IVal.new( :value(1), :named('autoderef') ) );
        }
        $past
    }

    method postprocess_quotewords($/, $past) {
        my $result := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
        sub walk($node) {
            if $node.ann('ww_atom') {
                $result.push($node);
            }
            elsif nqp::istype($node, QAST::Op) && $node.name eq '&infix:<~>' {
                walk($node[0]);
                walk($node[1]);
            }
            # (can't just use postprocess_words here because it introduces spurious comma operations)
            elsif $node.has_compile_time_value {
                my @words := HLL::Grammar::split_words($/,
                    nqp::unbox_s($node.compile_time_value));
                for @words { $result.push($*W.add_string_constant(~$_)); }
            }
            else {
                $result.push(
                    QAST::Op.new(
                        :op('callmethod'),
                        :name('Slip'),
                        QAST::Op.new(
                            :op('callmethod'),
                            :name('words'),
                            :node($/),
                            $node,
                            QAST::IVal.new( :value(1), :named('autoderef') )
                        )
                    )
                );
            }
        }
        walk($past);
        +@($result) == 1 ?? $result[0] !! QAST::Stmts.new( $result )
    }

    method postprocess_heredoc($/, $past) {
        QAST::Stmts.new(
            QAST::Op.new( :op<die_s>, QAST::SVal.new( :value("Premature heredoc consumption") ) ),
            $past)
    }

    method escape:sym<\\>($/) { make $<item>.ast; }
    method backslash:sym<qq>($/) { make $<quote>.ast; }
    method backslash:sym<\\>($/) { make $<text>.Str; }
    method backslash:delim ($/) { make $<text>.Str; }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }

    method backslash:sym<a>($/) { make nqp::chr(7) }
    method backslash:sym<b>($/) { make "\b" }
    method backslash:sym<c>($/) { make $<charspec>.ast }
    method backslash:sym<e>($/) { make "\c[27]" }
    method backslash:sym<f>($/) { make "\c[12]" }
    method backslash:sym<n>($/) {
        my str $nl := nqp::unbox_s($*W.find_symbol(['$?NL']));
        if nqp::can($/.CURSOR, 'parsing_heredoc') {
            # In heredocs, we spit out a QAST::SVal here to prevent newlines
            # being taken literally and affecting the dedent.
            make QAST::SVal.new( :value($nl) );
        }
        else {
            make $nl;
        }
    }
    method backslash:sym<o>($/) { make self.ints_to_string( $<octint> ?? $<octint> !! $<octints><octint> ) }
    method backslash:sym<r>($/) {
        make nqp::can($/.CURSOR, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r") )
            !! "\r";
    }
    method backslash:sym<rn>($/) {
        make nqp::can($/.CURSOR, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r\n") )
            !! "\r\n";
    }
    method backslash:sym<t>($/) {
        make nqp::can($/.CURSOR, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\t") )
            !! "\t";
    }
    method backslash:sym<x>($/) { make self.ints_to_string( $<hexint> ?? $<hexint> !! $<hexints><hexint> ) }
    method backslash:sym<0>($/) { make "\c[0]" }

    method escape:sym<{ }>($/) {
        my $blast := $<block>.ast;
        $blast.annotate('past_block', WANTALL($blast.ann('past_block'),'escape{}'));
        make QAST::Op.new(
            :op('callmethod'), :name('Stringy'),
            WANTED(QAST::Op.new(
                :op('call'),
                QAST::Op.new( :op('p6capturelex'), $blast ),
                :node($/)), 'escape{}'));
    }

    # The next three are currently only used for tr///.
    method escape:ch ($/)     { make ~$/; }
    method escape:sym<..>($/) { make ~$/; }
    method escape:ws ($/)     { make ~$/; }

    method escape:sym<$>($/) { make $<EXPR>.ast; }
    method escape:sym<@>($/) { make $<EXPR>.ast; }
    method escape:sym<%>($/) { make $<EXPR>.ast; }
    method escape:sym<&>($/) { make $<EXPR>.ast; }

    method escape:sym<' '>($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<" ">($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<‘ ’>($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<“ ”>($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<colonpair>($/) { make mark_ww_atom($<colonpair>.ast); }
    sub mark_ww_atom($ast) {
        $ast.annotate('ww_atom', 1);
        $ast;
    }
}

class Perl6::RegexActions is QRegex::P6Regex::Actions does STDActions {

    method metachar:sym<:my>($/) {
        my $past := $<statement>.ast;
        make QAST::Regex.new( $past, :rxtype('qastnode'), :subtype('declarative') );
    }

    method metachar:sym<{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }

    method metachar:sym<qw>($/) {
        my $qast := QAST::Regex.new( :rxtype<alt>, :node($/) );
        my $nib  := $<nibble>.ast[0];
        my @nibs := +@($nib) ?? @($nib) !! [$nib];
        for @nibs {
            unless $_.has_compile_time_value {
                $/.CURSOR.panic("Quote words construct too complex to use in a regex");
            }
            $qast.push(%*RX<i>
                ?? QAST::Regex.new( $_.compile_time_value, :rxtype<literal>, :subtype<ignorecase> )
                !! QAST::Regex.new( $_.compile_time_value, :rxtype<literal> ));
        }
        make $qast;
    }

    method metachar:sym<'>($/) { self.rxquote($/) }

    method rxquote($/) {
        my $quote := $<quote>.ast;
        if $quote.has_compile_time_value {
            my $qast := QAST::Regex.new( :rxtype<literal>, nqp::unbox_s($quote.compile_time_value) );
            if %*RX<i> && %*RX<m> { # >
                $qast.subtype('ignorecase+ignoremark')
            }
            elsif %*RX<i> {
                $qast.subtype('ignorecase')
            }
            elsif %*RX<m> { # >
                $qast.subtype('ignoremark')
            }
            make $qast;
        }
        else {
            make QAST::Regex.new( QAST::NodeList.new(
                                        QAST::SVal.new( :value('!LITERAL') ),
                                        $quote,
                                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ) ),
                                :rxtype<subrule>, :subtype<method>, :node($/));
        }
    }

    method quantifier:sym<**>($/) {
        my $qast;
        if $<codeblock> {
            $qast := QAST::Regex.new( :rxtype<dynquant>, :node($/),
                QAST::Op.new( :op('callmethod'), :name('DYNQUANT_LIMITS'),
                    QAST::Var.new( :name('$¢'), :scope('lexical') ),
                    $<codeblock>.ast
                ),
            );
        }
        else {
            my $min := 0;
            if $<min> { $min := $<min>.ast; }

            my $max := -1;
            my $upto := $<upto>;

            if $<from> eq '^' { $min++ }

            if ! $<max> {
                $max := $min
            }
            elsif $<max> ne '*' {
                $max := $<max>.ast;
                if $<upto> eq '^' {
                    $max--;
                }
                $/.CURSOR.panic("Empty range") if $min > $max;
            }
            $qast := QAST::Regex.new( :rxtype<quant>, :min($min), :max($max), :node($/) );
        }
        make backmod($qast, $<backmod>);
    }

    sub backmod($ast, $backmod) {
        if $backmod eq ':' { $ast.backtrack('r') }
        elsif $backmod eq ':?' || $backmod eq '?' { $ast.backtrack('f') }
        elsif $backmod eq ':!' || $backmod eq '!' { $ast.backtrack('g') }
        $ast;
    }

    method metachar:sym<rakvar>($/) {
        my $varast := $<var>.ast;
        if nqp::istype($varast, QAST::Var) {
            # See if it's a constant Scalar, in which case we can turn it to
            # a Str and use the value as a literal, so we get LTM.
            if nqp::substr($varast.name, 0, 1) eq '$' {
                my $constant;
                try {
                    my $found := $*W.find_symbol([$varast.name]);
                    $constant := $found.Str if nqp::isconcrete($found);
                }
                if nqp::isconcrete($constant) {
                    make self.apply_literal_modifiers(QAST::Regex.new(
                        nqp::unbox_s($constant), :rxtype<literal>, :node($/)
                    ));
                    return;
                }
            }

            # If it's a variable, but statically typed as a string, we know
            # it's a simple interpolation; use LITERAL.
            if nqp::istype($varast.returns, $*W.find_symbol(['Str'])) {
                make QAST::Regex.new(QAST::NodeList.new(
                        QAST::SVal.new( :value('!LITERAL') ),
                        $varast,
                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) )
                    ),
                    :rxtype<subrule>, :subtype<method>, :node($/));
                return;
            }
        }

        # Otherwise, slow path that checks what we have.
        make QAST::Regex.new(QAST::NodeList.new(
                QAST::SVal.new( :value('INTERPOLATE') ),
                $varast,
                QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                QAST::IVal.new( :value(%*RX<m> ?? 1 !! 0) ),
                QAST::IVal.new( :value(monkey_see_no_eval()) ),
                QAST::IVal.new( :value($*SEQ ?? 1 !! 0) )
            ),
            QAST::Op.new( :op<callmethod>, :name<new>,
                QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
            ),
            :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<{ }>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(%*RX<m> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval()) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<?{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( 0 ),
                              :rxtype<qastnode>, :node($/) );
    }

    method assertion:sym<!{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( 1 ),
                              :rxtype<qastnode>, :node($/) );
    }

    method assertion:sym<var>($/) {
        if $<arglist> {
            my $ast := make QAST::Regex.new(
                QAST::NodeList.new(
                    QAST::SVal.new( :value('CALL_SUBRULE') ),
                    $<var>.ast ),
                :rxtype<subrule>, :subtype<method>, :node($/));
            for @($<arglist>.ast) {
                $ast[0].push(wanted($_, 'assertvar1'));
            }
        } else {
            make QAST::Regex.new(
                QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE') ),
                    wanted($<var>.ast, 'assertvar2'),
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(%*RX<m> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval()) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                ),
                :rxtype<subrule>, :subtype<method>, :node($/));
        }
    }

    method assertion:sym<name>($/) {
        my $lng := $*W.dissect_longname($<longname>);
        my $qast;
        # We got something like <::($foo)>
        if $lng.contains_indirect_lookup() {
            $qast := QAST::Regex.new( :rxtype<subrule>, :subtype<method>, :node($/),
                QAST::NodeList.new(QAST::SVal.new( :value('INDMETHOD') ), $lng.name_past()) );
        }
        else {
            my @parts := $lng.components();
            my $name  := @parts.pop();
            my $c     := $/.CURSOR;
            if $<assertion> {
                if +@parts {
                    $c.panic("Can only alias to a short name (without '::')");
                }
                $qast := $<assertion>.ast;
                if $qast.rxtype eq 'subrule' {
                    self.subrule_alias($qast, $name);
                }
                else {
                    $qast := QAST::Regex.new( $qast, :name($name),
                                              :rxtype<subcapture>, :node($/) );
                }
            }
            elsif !@parts && $name eq 'sym' {
                my str $fullrxname := %*RX<name>;
                my int $loc := nqp::index($fullrxname, ':sym<');
                $loc := nqp::index($fullrxname, ':sym«')
                    if $loc < 0;
                my str $rxname := nqp::substr($fullrxname, $loc + 5, nqp::chars($fullrxname) - $loc - 6);
                $qast := QAST::Regex.new(:name('sym'), :rxtype<subcapture>, :node($/),
                    QAST::Regex.new(:rxtype<literal>, $rxname, :node($/)));
            }
            else {
                if +@parts {
                    my $gref := QAST::WVal.new( :value($*W.find_symbol(@parts)) );
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new(
                                                QAST::SVal.new( :value('OTHERGRAMMAR') ),
                                                $gref, QAST::SVal.new( :value($name) )),
                                             :name(~$<longname>) );
                } elsif $*W.regex_in_scope('&' ~ $name) && nqp::substr($c.orig, $/.from - 1, 1) ne '.' {
                    # The lookbehind for . is because we do not yet call $~MAIN's methodop, and our recognizer for
                    # . <assertion>, which is a somewhat bogus recursion, comes from QRegex, not our own grammar.
                    my $coderef := $*W.find_symbol(['&' ~ $name]);
                    my $var := QAST::Var.new( :name('&' ~ $name), :scope<lexical> );
                    $var.annotate('coderef',$coderef);
                    my $c := $var.ann('coderef');
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new($var),
                                             :name($name) );
                }
                else {
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new(QAST::SVal.new( :value($name) )),
                                             :name($name) );
                }
                if $<arglist> {
                    for $<arglist>.ast.list { $qast[0].push(wanted($_, 'assertname')) }
                }
                elsif $<nibbler> {
                    my $nibbled := $name eq 'after'
                        ?? self.flip_ast($<nibbler>.ast)
                        !! $<nibbler>.ast;
                    my $sub := %*LANG<Regex-actions>.qbuildsub($nibbled, :anon(1), :addself(1));
                    $qast[0].push($sub);
                }
            }
        }
        make $qast;
    }

    method assertion:sym<~~>($/) {
        if $<num> {
            $/.CURSOR.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        elsif $<desigilname> {
            $/.CURSOR.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        else {
            make QAST::Regex.new( :rxtype<subrule>, :subtype<method>,
                QAST::NodeList.new(QAST::SVal.new( :value('RECURSE') )), :node($/) );
        }
    }

    method codeblock($/) {
        my $blockref := $<block>.ast;
        my $past :=
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope<lexical> ),
                    QAST::Op.new(
                        QAST::Var.new( :name('$¢'), :scope<lexical> ),
                        :name('MATCH'),
                        :op('callmethod')
                    )
                ),
                QAST::Op.new( :op<call>, QAST::Op.new( :op('p6capturelex'), $blockref ) )
            );
        make $past;
    }

    method arglist($/) {
        my $arglist := $<arglist>.ast;
        make $arglist;
    }

    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameter_objects', [])))
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}

class Perl6::P5RegexActions is QRegex::P5Regex::Actions does STDActions {
    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameter_objects', [])))
    }

    method p5metachar:sym<(?{ })>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }

    method p5metachar:sym<(??{ })>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::IVal.new( :value(monkey_see_no_eval()) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method p5metachar:sym<var>($/) {
        make QAST::Regex.new( QAST::NodeList.new(
                                    QAST::SVal.new( :value('INTERPOLATE') ),
                                    wanted($<var>.ast, 'p5var'),
                                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                                    QAST::IVal.new( :value(0) ),
                                    QAST::IVal.new( :value(monkey_see_no_eval()) ),
                                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                                    QAST::IVal.new( :value($*INTERPOLATION) ) ),
                              :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method codeblock($/) {
        my $blockref := $<block>.ast;
        my $past :=
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope<lexical> ),
                    QAST::Op.new(
                        QAST::Var.new( :name('$¢'), :scope<lexical> ),
                        :name('MATCH'),
                        :op('callmethod')
                    )
                ),
                QAST::Op.new(:op<call>, $blockref)
            );
        make $past;
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}

# vim: ft=perl6 et sw=4
