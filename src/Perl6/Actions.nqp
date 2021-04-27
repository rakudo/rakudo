use NQPP6QRegex;
use NQPP5QRegex;
use Perl6::Pod;
use Perl6::Ops;
use QRegex;
use QAST;

my $wantwant := Mu;

# block types
my $para-block   := 'paragraph';
my $delim-block  := 'delimited';
my $abbrev-block := 'abbreviated';

# 2147483648 == 2**31. By adding 1 to it with add_i op, on 32-bit boxes it will overflow
my int $?BITS := nqp::isgt_i(nqp::add_i(2147483648, 1), 0) ?? 64 !! 32;

sub block_closure($code, :$regex) {
    my $clone := QAST::Op.new( :op('callmethod'), :name('clone'), $code );
    if $regex {
        if $*W.lang-rev-before('d') {
            my $marker := $*W.find_symbol(['Rakudo', 'Internals', 'RegexBoolification6cMarker']);
            $clone.push(QAST::WVal.new( :value($marker), :named('topic') ));
        }
        else {
            $clone.push(QAST::Var.new( :name('$_'), :scope('lexical'), :named('topic') ));
            $clone.push(QAST::Var.new( :name('$/'), :scope('lexical'), :named('slash') ));
        }
    }
    QAST::Op.new( :op('p6capturelex'), $clone ).annotate_self(
        'past_block', $code.ann('past_block')
    ).annotate_self(
        'code_object', $code.ann('code_object'))
}

sub wantall($ast, $by) {
    my int $e := $ast ?? nqp::elems(@($ast)) !! 0;
    my int $i := -1;
    while ++$i < $e {
        $ast[$i] := wanted($ast[$i], $by ~ ' wa');
    }
    Nil;
}

sub WANTALL($ast, $by) {
    my int $e := $ast ?? nqp::elems(@($ast)) !! 0;
    my int $i := -1;
    while ++$i < $e {
        $ast[$i] := WANTED($ast[$i], $by ~ ' WA');
    }
    Nil;
}

sub unwantall($ast, $by) {
    my int $e := $ast ?? nqp::elems(@($ast)) !! 0;
    my int $i := -1;
    while ++$i < $e {
        $ast[$i] := unwanted($ast[$i], $by ~ ' ua');
    }
    Nil;
}

sub UNWANTALL($ast, $by) {
    my int $e := $ast ?? nqp::elems(@($ast)) !! 0;
    my int $i := -1;
    while ++$i < $e {
        $ast[$i] := UNWANTED($ast[$i], $by ~ ' ua');
    }
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
    return $ast if $ast.wanted;  # already marked from here down
    return $ast if $ast.sunk; # already marked from here down
    note('wanted ' ~ $addr ~ ' by ' ~ $by ~ "\n" ~ $ast.dump) if $wantwant;
#    if $ast.sunk {
#        note("Oops, already sunk node is now wanted!?! \n" ~ $ast.dump);
#        $ast.sunk(0);
#    }
    my $e := nqp::elems(@($ast)) - 1;
    $ast.annotate('BY',$byby) if $wantwant;

    if nqp::istype($ast,QAST::Stmt) || nqp::istype($ast,QAST::Stmts) {
        my $resultchild := $ast.resultchild // $e;
        my int $i := -1;
        while ++$i <= $e {
            $ast[$i] := $i == $resultchild ?? wanted($ast[$i], $byby) !! unwanted($ast[$i], $byby);
        }
        $ast.wanted(1);
    }
    elsif nqp::istype($ast,QAST::Block) {
        my int $i := 1;
        my $*WANTEDOUTERBLOCK := $ast;
        while $i <= $e {
            $ast[$i] := WANTED($ast[$i], $byby);
            ++$i;
        }
        $ast.wanted(1);
    }
    elsif nqp::istype($ast,QAST::Op) {
        if $ast.op eq 'call' && (
                !$ast.name ||
                $ast.name eq '&infix:<,>' ||
                $ast.name eq '&infix:<andthen>' ||
                $ast.name eq '&infix:<orelse>' ||
                $ast.name eq '&infix:<notandthen>' ||
                $ast.name eq '&infix:<xx>') {
            WANTALL($ast,$byby);
        }
        elsif $ast.op eq 'callmethod' {
            WANTALL($ast,$byby);
        }
        elsif $ast.op eq 'p6capturelex' {
            $ast.annotate('past_block', wanted($ast.ann('past_block'), $byby));
            $ast.wanted(1);
        }
        elsif $ast.op eq 'call' ||
              $ast.op eq 'callstatic' ||
              $ast.op eq 'handle' ||
              $ast.op eq 'locallifetime' ||
              $ast.op eq 'p6typecheckrv' ||
              $ast.op eq 'handlepayload' {
            $ast[0] := WANTED($ast[0], $byby) if nqp::elems(@($ast));
            $ast.wanted(1);
        }
        elsif $ast.op eq 'p6decontrv' || $ast.op eq 'p6decontrv_6c' {
            $ast[1] := WANTED($ast[1], $byby) if nqp::elems(@($ast));
            $ast.wanted(1);
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
            my $block;
            my $block-closure;
            if $body.ann('loop-already-block-first-phaser') -> $loop-goods {
                $block := $loop-goods[0][1][0];
                $block-closure := QAST::Op.new: :node($body.node),
                    :op<p6setfirstflag>, block_closure($block);

                # get rid of now-useless var and other bits of the QAST.
                # If we .shift off all items, the QAST::Stmts gets a null
                # in them that I can't figure out where it's coming from,
                # so shove an empty QAST::Smts to replace last item.
                $loop-goods.shift;
                $loop-goods[0] := QAST::Stmts.new;
            }
            else {
                $block := Perl6::Actions::make_thunk_ref($body, $body.node);
                $block-closure := block_closure($block);
            }

            # make sure from-loop knows about existing label
            my $label := QAST::WVal.new( :value($*W.find_single_symbol('Any')), :named('label') );
            for @($ast) {
                $label := $_ if nqp::istype($_, QAST::WVal) && nqp::istype($_.value, $*W.find_single_symbol('Label'));
            }

            my $past := QAST::Op.new: :node($body.node),
                :op<callmethod>, :name<from-loop>,
                QAST::WVal.new(:value($*W.find_single_symbol('Seq'))),
                $block-closure,
                $label;

            # Elevate statevars to enclosing thunk
            if $body.has_ann('has_statevar') && $block.has_ann('past_block') {
                Perl6::Actions::migrate_blocks(
                    $body, $block.ann('past_block'),
                    -> $n { nqp::istype($n, QAST::Var) && $n.decl eq 'statevar' }
                )
            }

            # conditional (if not always true (or if repeat))
            if $repeat || !$cond.has_compile_time_value || !$cond.compile_time_value == $while {
                $cond := QAST::Op.new( :op<callmethod>, :name<not>, $cond ) unless $while;
                $block := Perl6::Actions::make_thunk_ref($cond, nqp::can($cond,'node') ?? $cond.node !! $body.node);
                $past.push( block_closure($block) );
            }

            # 3rd part of loop, if any
            if nqp::elems(@($ast)) > 2 {
                $block := Perl6::Actions::make_thunk_ref($ast[2], $ast[2].node);
                $block.annotate('outer',$*WANTEDOUTERBLOCK) if $*WANTEDOUTERBLOCK;
                $past.push( UNWANTED(block_closure($block),$byby) )
            }

            if $repeat {
                my $wval := QAST::WVal.new( :value($*W.find_single_symbol('True')) );
                $wval.named('repeat');
                $past.push($wval);
            }

            $ast := $past;
            $ast.wanted(1);
        }
        elsif $ast.op eq 'if' ||
              $ast.op eq 'unless' ||
              $ast.op eq 'with' ||
              $ast.op eq 'without' {
            $ast[1] := WANTED($ast[1], $byby);
            $ast[2] := WANTED($ast[2], $byby)
                if nqp::elems(@($ast)) > 2 && nqp::istype($ast[2],QAST::Node);
            $ast.wanted(1);
        }
    }
    elsif nqp::istype($ast,QAST::Want) {
        $ast.wanted(1);
        my $node := $ast[0];
        if nqp::istype($node,QAST::Op) {
            if $node.op eq 'call' && (!$node.name || $node.name eq '&infix:<xx>') {
                $node := $node[0];
                if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                    $node.annotate('past_block', WANTED($node.ann('past_block'), $byby));
                }
            }
            elsif $node.op eq 'call' || $node.op eq 'handle' {
                $ast[0] := WANTED($node,$byby);
            }
            elsif $node.op eq 'callstatic' || $node.op eq 'hllize' {
                $node[0] := WANTED($node[0], $byby);
            }
            elsif $node.op eq 'p6for' || $node.op eq 'p6forstmt' {
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
                $node.wanted(1);
            }
            elsif $node.op eq 'if' ||
                  $node.op eq 'unless' ||
                  $node.op eq 'with' ||
                  $node.op eq 'without' {
                $node[1] := WANTED($node[1], $byby);
                $node[2] := WANTED($node[2], $byby)
                    if nqp::elems(@($node)) > 2
                        && nqp::istype($node[2],QAST::Node);
                $node.wanted(1);
            }
        }
    }
    else {
        $ast.wanted: 1;
    }
    $ast;
}

sub WANTED($ast, $by) {
    if nqp::istype($ast, QAST::Node) {
        $ast := wanted($ast, $by ~ ' W');
        $ast.wanted(1);  # force in case it's just a thunk
    }
    else {
        note("Non ast passed to WANTED: " ~ $ast.HOW.name($ast));
    }
    $ast;
}

my %nosink := nqp::hash('sink',1,'push',1,'append',1,'unshift',1,'prepend',1,'splice',1);

sub unwanted($ast, $by) {
    my $byby := $by ~ ' u';
    return $ast unless nqp::can($ast,'ann');
    my $addr := nqp::where($ast);
    return $ast if $ast.sunk;
    return $ast if $ast.wanted;  # probably a loose thunk just stashed somewhere random
    $ast.annotate('BY',$byby) if $wantwant;
    my $e := nqp::elems(@($ast)) - 1;
    note('unwanted ' ~ $addr ~ ' by ' ~ $by ~ "\n" ~ $ast.dump) if $wantwant;
    if nqp::istype($ast,QAST::Stmt) || nqp::istype($ast,QAST::Stmts) {
        # Unwant all kids, not just last one, so we recurse into blocks and such,
        # don't just rely on the optimizer to default to void.
        my int $i := -1;
        while ++$i <= $e {
            $ast[$i] := unwanted($ast[$i], $byby);
        }
        $ast.sunk(1);
        $ast.push(QAST::WVal.new( :value($*W.find_single_symbol('True')) ))
            if $e >= 0 && nqp::istype($ast[$e],QAST::Op) && $ast[$e].op eq 'bind';
    }
    elsif nqp::istype($ast,QAST::Block) {
        my int $i := 1;
        my $*WANTEDOUTERBLOCK := $ast;
        while $i <= $e {
            $ast[$i] := UNWANTED($ast[$i], $byby);
            ++$i;
        }
        $ast.sunk(1);
    }
    elsif nqp::istype($ast,QAST::Op) {
        if $ast.op eq 'call' {
            if $ast.name eq '&infix:<,>' || $ast.name eq '&infix:<xx>' {
                UNWANTALL($ast,$byby);
            }
            elsif $ast.name eq '&term:<now>' {
                $ast.node.worry("Useless use of 'now' in sink context");
            }
            $ast.sunk(1);
        }
        elsif $ast.op eq 'p6capturelex' {
            $ast.annotate('past_block', unwanted($ast.ann('past_block'), $byby));
            $ast.sunk(1);
        }
        elsif $ast.op eq 'callstatic' ||
              $ast.op eq 'handle' ||
              $ast.op eq 'locallifetime' ||
              $ast.op eq 'p6typecheckrv' ||
              $ast.op eq 'handlepayload' ||
              $ast.op eq 'ifnull' {
            $ast[0] := UNWANTED($ast[0], $byby) if nqp::elems(@($ast));
            $ast.sunk(1);
        }
        elsif $ast.op eq 'hllize' {
            my $node := $ast[0];
            if $node.op eq 'callmethod' && !$ast.nosink {
                if !$node.nosink && !$*COMPILING_CORE_SETTING && !%nosink{$node.name} {
                    $ast.sunk(1);
                    $ast := QAST::Op.new(:op<callmethod>, :name<sink>, $ast);
                    $ast.sunk(1);
                    return $ast;
                }
            }
            $ast.sunk(1);
        }
        elsif $ast.op eq 'callmethod' {
            if !$ast.nosink && !$*COMPILING_CORE_SETTING && !%nosink{$ast.name} {
                return $ast if $*ALREADY_ADDED_SINK_CALL;
                $ast.sunk(1);
                $ast := QAST::Op.new(:op<callmethod>, :name<sink>, $ast);
                $ast.sunk(1);
                return $ast;
            }
            $ast[0] := UNWANTED($ast[0], $byby) if nqp::elems(@($ast));
            $ast.sunk(1);
        }
        elsif $ast.op eq 'p6decontrv' || $ast.op eq 'p6decontrv_6c' {
            $ast[1] := UNWANTED($ast[1], $byby) if nqp::elems(@($ast));
            $ast.sunk(1);
        }
        elsif $ast.op eq 'while' ||
              $ast.op eq 'until' ||
              $ast.op eq 'repeat_while' ||
              $ast.op eq 'repeat_until' {
            # Do we need to force loop to produce return values for internal reasons?
            if !$*COMPILING_CORE_SETTING && $ast[1].ann('WANTMEPLEASE') {
                $ast := QAST::Op.new(:op<callmethod>, :name<sink>, WANTED($ast, $byby));
                $ast.sunk(1);
                return $ast;
            }
            $ast[1] := UNWANTED($ast[1], $byby);
            $ast.sunk(1);
        }
        elsif $ast.op eq 'if' ||
              $ast.op eq 'unless' ||
              $ast.op eq 'with' ||
              $ast.op eq 'without' {
            $ast[1] := UNWANTED($ast[1], $byby);
            $ast[2] := UNWANTED($ast[2], $byby)
                if nqp::elems(@($ast)) > 2 && nqp::istype($ast[2],QAST::Node);
            $ast.sunk(1);
        }
        elsif $ast.op eq 'bind' {
            $ast.sunk(1);
        }
        elsif $ast.op eq 'xor' {
            my int $i := 1;
            my int $elems := nqp::elems($ast);
            while $i <= $e {
                $ast[$i] := UNWANTED($ast[$i], $byby);
                ++$i;
            }
            $ast.sunk: 1;
        }
    }
    elsif nqp::istype($ast,QAST::Want) {
        $ast.sunk(1);
        my $node := $ast[0];

        if nqp::istype($node,QAST::WVal) {
            $node.sunk(1);
            $ast[2].sunk(1);
        }
        elsif nqp::istype($node,QAST::Op) {
            if $node.op eq 'call' {
                $node.sunk(1);
                if !$node.name {
                    my $node0 := $node[0];
                    unwanted($node0, $byby);
                    if nqp::istype($node0,QAST::Op) && $node0.op eq 'call' && nqp::eqat($node0.name, '&META', 0) {
                        my $op := $node.node.Str;
                        my $t := nqp::index($op,' ');
                        $op := nqp::substr($op, 0, $t) if $t > 0;
                        my $purity := 0;
                        if $node0[0].ann('is_pure') {
                            $purity := 1 unless $node0.name eq '&METAOP_ASSIGN';
                        }
                        else {
                            my $subname := $node0[0].name;
                            my $subfun := try $*W.find_single_symbol($subname);
                            if $subfun {
                                if nqp::index($node0.name, 'ASSIGN') < 0 && nqp::can($subfun, 'is-pure') {
                                    $purity := 1;
                                }
                            }
                            else {
                                $purity := 1;  # "can't happen" except in setting, so assume will be pure
                            }
                        }
                        $node.node.PRECURSOR.worry("Useless use of $op in sink context") if $purity;
                    }
                }
                else {
                    my $infix := $node.node<infix>;
                    if $infix {
                        my $sym := $infix<sym>;
                        if $sym eq ',' || $sym eq 'xx' { unwantall($node, $byby) }
                        elsif $sym eq '...'
                           || $sym eq '...^'
                           || $sym eq '^...'
                           || $sym eq '^...^'
                           || $sym eq '…'
                           || $sym eq '…^'
                           || $sym eq '^…'
                           || $sym eq '^…^'
                        {
                            $node.annotate('useless', $sym);
                            $node.node.worry("Useless use of $sym in sink context");
                        }
                    }
                    elsif $node.name eq '&term:<now>' {
                        $node.annotate('useless', "'now'");
                        $node.node.worry("Useless use of 'now' in sink context");
                    }
                }
            }
            elsif $node.op eq 'hllize' {
                $ast[0] := UNWANTED($node,$byby);
            }
            elsif $node.op eq 'callmethod' {
                if !$node.nosink && !%nosink{$node.name} {
                    $ast := QAST::Op.new(:op<callmethod>, :name<sink>, unwanted($node, $byby));
                    $ast.sunk(1);
                    return $ast;
                }
                $node.sunk(1);
            }
            elsif $node.op eq 'p6for' || $node.op eq 'p6forstmt' {
                $node := $node[1];
                if nqp::istype($node,QAST::Op) && $node.op eq 'p6capturelex' {
                    unless $*COMPILING_CORE_SETTING {
                        add-sink-to-final-call($node.ann('past_block'), 1);
                        my $*ALREADY_ADDED_SINK_CALL := 1;
                        $node.annotate('past_block', UNWANTED($node.ann('past_block'), $byby));
                    }
                }
            }
            elsif $node.op eq 'while' || $node.op eq 'until' {
                if !$*COMPILING_CORE_SETTING && $node[1].ann('WANTMEPLEASE') {
                    $ast := QAST::Op.new(:op<callmethod>, :name<sink>, WANTED($node, $byby));
                    $ast.sunk(1);
                    return $ast;
                }
                $node[1] := UNWANTED($node[1], $byby);
                $node.sunk(1);
            }
            elsif $node.op eq 'if' || $node.op eq 'unless' || $node.op eq 'with' || $node.op eq 'without' {
                for 1,2 {
                    if nqp::elems(@($node)) > $_
                    && nqp::istype($node[$_],QAST::Node) {
                        if nqp::istype($node[$_],QAST::Op) && $node[$_].op eq 'bind' {
                            $node[$_] := QAST::Stmts.new(
                                            $node[$_],
                                            QAST::WVal.new( :value($*W.find_single_symbol('True'))));
                        }
                        $node[$_] := UNWANTED($node[$_], $byby);
                    }
                }
                $node.sunk(1);
            }
            elsif $node.op eq 'callmethod' && $node.name eq 'new' {
                $node.sunk(1);
            }
        }
    }
    $ast;
}

sub add-sink-to-final-call($parent, $pos, $qast = $parent[$pos]) {
    if (nqp::istype($qast, QAST::Stmts) || nqp::istype($qast, QAST::Stmt))
    && nqp::elems($qast) {
        add-sink-to-final-call($qast, nqp::elems($qast)-1)
    }
    elsif nqp::istype($qast, QAST::Want) {
        add-sink-to-final-call($parent, $pos, $qast[0])
    }
    elsif nqp::istype($qast, QAST::Op) && $qast.op eq 'call' && !$qast.nosink {
        $parent[$pos] := QAST::Op.new: :op<callmethod>, :name<sink>, $qast
    }
}

sub UNWANTED($ast, $by) {
    if nqp::istype($ast, QAST::Node) {
        $ast := unwanted($ast, $by ~ ' U');
        $ast.sunk(1);
    }
    else {
        note("Non ast passed to UNWANTED: " ~ $ast.HOW.name($ast));
    }
    $ast;
}

register_op_desugar('p6box_i', -> $qast {
    QAST::Op.new( :op('box_i'), $qast[0], QAST::Op.new( :op('hllboxtype_i') ) )
});
register_op_desugar('p6box_n', -> $qast {
    QAST::Op.new( :op('box_n'), $qast[0], QAST::Op.new( :op('hllboxtype_n') ) )
});
register_op_desugar('p6box_s', -> $qast {
    QAST::Op.new( :op('box_s'), $qast[0], QAST::Op.new( :op('hllboxtype_s') ) )
});
register_op_desugar('p6box_u', -> $qast {
    QAST::Op.new( :op('box_u'), $qast[0], QAST::Op.new( :op('hllboxtype_i') ) )
});
register_op_desugar('p6reprname', -> $qast {
    QAST::Op.new( :op('box_s'), QAST::Op.new( :op('reprname'), $qast[0]), QAST::Op.new( :op('hllboxtype_s') ) )
});
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
    # Figure out the execution mode.
    my $mode := $qast.ann('mode') || 'serial';
    my $after-mode;
    if $mode eq 'lazy' {
        $after-mode := 'lazy';
        $mode := 'serial';
    }
    else {
        $after-mode := $qast.sunk ?? 'sink' !! 'eager';
    }

    my $cond := $qast[0];
    my $block := $qast[1];
    my $label := $qast[2];
    my $for-list-name := QAST::Node.unique('for-list');
    my $call := QAST::Op.new(
        :op('if'),
        QAST::Op.new( :op('iscont'), QAST::Var.new( :name($for-list-name), :scope('local') ) ),
        QAST::Op.new(
            :op<callmethod>, :name<map>, :node($qast),
            QAST::Var.new( :name($for-list-name), :scope('local') ),
            $block,
            QAST::IVal.new( :value(1), :named('item') )
        ),
        QAST::Op.new(
            :op<callmethod>, :name<map>, :node($qast),
            QAST::Op.new(
                :op<callmethod>, :name($mode), :node($qast),
                QAST::Var.new( :name($for-list-name), :scope('local') )
            ),
            $block
        )
    );
    if $label {
        $call[1].push($label);
        $call[2].push($label);
    }
    my $bind := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($for-list-name), :scope('local'), :decl('var') ),
        $cond,
    );
    QAST::Stmts.new(
        $bind,
        QAST::Op.new( :op<callmethod>, :name($after-mode), $call )
    );
});
register_op_desugar('p6forstmt', -> $qast {
    my $for-target-name := QAST::Node.unique('for_target');
    my $for-target := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($for-target-name), :scope('local'), :decl('var') ),
        $qast[0]
    );

    my $iterator-name := QAST::Node.unique('for_iterator');
    my $iterator := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($iterator-name), :scope('local'), :decl('var') ),
        QAST::Op.new(
            :op('callmethod'), :name('iterator'),
            QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('iscont'),
                    QAST::Var.new( :name($for-target-name), :scope('local') )
                ),
                QAST::Op.new(
                    :op('callstatic'), :name('&infix:<,>'),
                    QAST::Var.new( :name($for-target-name), :scope('local') )
                ),
                QAST::Var.new( :name($for-target-name), :scope('local') )
            )));

    my $iteration-end-name := QAST::Node.unique('for_iterationend');
    my $iteration-end := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($iteration-end-name), :scope('local'), :decl('var') ),
        QAST::WVal.new( :value($qast.ann('IterationEnd')) )
    );

    my $block-name := QAST::Node.unique('for_block');
    my $block := QAST::Op.new(
        :op('bind'),
        QAST::Var.new( :name($block-name), :scope('local'), :decl('var') ),
        $qast[1]
    );

    my $iter-val-name := QAST::Node.unique('for_iterval');
    my $loop := QAST::Op.new(
        :op('until'),
        QAST::Op.new(
            :op('eqaddr'),
            QAST::Op.new(
                :op('decont'),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($iter-val-name), :scope('local'), :decl('var') ),
                    QAST::Op.new(
                        :op('callmethod'), :name('pull-one'),
                        QAST::Var.new( :name($iterator-name), :scope('local') )
                    )
                )
            ),
            QAST::Var.new( :name($iteration-end-name), :scope('local') )
        ),
        QAST::Op.new(
            :op('call'),
            QAST::Var.new( :name($block-name), :scope('local') ),
            QAST::Var.new( :name($iter-val-name), :scope('local') )
        ));
    if $qast[2] {
        $loop.push($qast[2]);
    }

    QAST::Stmts.new(
        $for-target,
        $iterator,
        $iteration-end,
        $block,
        $loop,
        QAST::WVal.new( :value($qast.ann('Nil')) )
    )
});
register_op_desugar('p6scalarfromdesc', -> $qast {
    my $desc := QAST::Node.unique('descriptor');
    my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
    my $default_cont_spec := nqp::gethllsym('Raku', 'default_cont_spec');
    QAST::Stmt.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($desc), :scope('local'), :decl('var') ),
            $qast[0]
        ),
        QAST::Op.new(
            :op('unless'),
            QAST::Op.new(
                :op('isconcrete'),
                QAST::Var.new( :name($desc), :scope('local') ),
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($desc), :scope('local') ),
                QAST::WVal.new( :value($default_cont_spec) )
            )
        ),
        QAST::Op.new(
            :op('p6bindattrinvres'),
            QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new( :op('create'), $Scalar ),
                $Scalar,
                QAST::SVal.new( :value('$!descriptor') ),
                QAST::Var.new( :name($desc), :scope('local') )
            ),
            $Scalar,
            QAST::SVal.new( :value('$!value') ),
            QAST::Op.new(
                :op('callmethod'), :name('default'),
                QAST::Var.new( :name($desc), :scope('local') )
            )
        )
    )
});
# The "certain" variant is allowed to assume the container descriptor is
# reliably provided, so need not map it to the default one. Ideally, we'll
# eventually have everything using this version of the op.
register_op_desugar('p6scalarfromcertaindesc', -> $qast {
    my $desc := QAST::Node.unique('descriptor');
    my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
    QAST::Stmt.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($desc), :scope('local'), :decl('var') ),
            $qast[0]
        ),
        QAST::Op.new(
            :op('p6bindattrinvres'),
            QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new( :op('create'), $Scalar ),
                $Scalar,
                QAST::SVal.new( :value('$!descriptor') ),
                QAST::Var.new( :name($desc), :scope('local') )
            ),
            $Scalar,
            QAST::SVal.new( :value('$!value') ),
            QAST::Op.new(
                :op('callmethod'), :name('default'),
                QAST::Var.new( :name($desc), :scope('local') )
            )
        )
    )
});
register_op_desugar('p6scalarwithvalue', -> $qast {
    my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
    QAST::Op.new(
        :op('p6assign'),
        QAST::Op.new(
            :op('p6bindattrinvres'),
            QAST::Op.new( :op('create'), $Scalar ),
            $Scalar,
            QAST::SVal.new( :value('$!descriptor') ),
            $qast[0]
        ),
        $qast[1]
    )
});
register_op_desugar('p6recont_ro', -> $qast {
    my $result := QAST::Node.unique('result');
    my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
    QAST::Stmt.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($result), :scope('local'), :decl('var') ),
            $qast[0]
        ),
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('isconcrete_nd'),
                    QAST::Var.new( :name($result), :scope('local') )
                ),
                QAST::Op.new(
                    :op('isrwcont'),
                    QAST::Var.new( :name($result), :scope('local') )
                )
            ),
            QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new( :op('create'), $Scalar ),
                $Scalar,
                QAST::SVal.new( :value('$!value') ),
                QAST::Op.new(
                    :op('decont'),
                    QAST::Var.new( :name($result), :scope('local') )
                )
            ),
            QAST::Var.new( :name($result), :scope('local') )
        )
    )
});
register_op_desugar('p6var', -> $qast {
    my $result := QAST::Node.unique('result');
    my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
    QAST::Stmt.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($result), :scope('local'), :decl('var') ),
            $qast[0]
        ),
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('isconcrete_nd'),
                    QAST::Var.new( :name($result), :scope('local') )
                ),
                QAST::Op.new(
                    :op('iscont'),
                    QAST::Var.new( :name($result), :scope('local') )
                )
            ),
            QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new( :op('create'), $Scalar ),
                $Scalar,
                QAST::SVal.new( :value('$!value') ),
                QAST::Var.new( :name($result), :scope('local') )
            ),
            QAST::Var.new( :name($result), :scope('local') )
        )
    )
});
register_op_desugar('time_i', -> $qast {
    QAST::Op.new( :op('div_i'), QAST::Op.new( :op('time' ) ), QAST::IVal.new( :value(1000000000) ) )
});
register_op_desugar('time_n', -> $qast {
    QAST::Op.new( :op('div_n'), QAST::Op.new( :op('time' ) ), QAST::NVal.new( :value(1000000000e0) ) )
});
{
    my $is_moar;
    register_op_desugar('p6decontrv_internal', -> $qast {
        unless nqp::isconcrete($is_moar) {
            $is_moar := nqp::getcomp('Raku').backend.name eq 'moar';
        }
        if $is_moar {
            my $result := QAST::Node.unique('result');
            QAST::Stmt.new(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($result), :scope('local'), :decl('var') ),
                    QAST::Op.new( :op('wantdecont'), $qast[0] )
                ),
                QAST::Op.new(
                    :op('call'),
                    QAST::Op.new(
                        :op('speshresolve'),
                        QAST::SVal.new( :value($qast[1] eq '6c' ?? 'decontrv_6c' !! 'decontrv') ),
                        QAST::Var.new( :name($result), :scope('local') )
                    ),
                    QAST::Var.new( :name($result), :scope('local') ),
                )
            )
        }
        else {
            my $result := QAST::Node.unique('result');
            my $Scalar := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Scalar')) );
            my $Iterable := QAST::WVal.new( :value(nqp::gethllsym('Raku', 'Iterable')) );
            QAST::Stmt.new(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($result), :scope('local'), :decl('var') ),
                    QAST::Op.new( :op('wantdecont'), $qast[0] )
                ),
                QAST::Op.new(
                    # If it's a container...
                    :op('if'),
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('isconcrete_nd'),
                            QAST::Var.new( :name($result), :scope('local') )
                        ),
                        QAST::Op.new(
                            :op('iscont'),
                            QAST::Var.new( :name($result), :scope('local') )
                        )
                    ),
                    # It's a container; is it an rw one?
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('isrwcont'),
                            QAST::Var.new( :name($result), :scope('local') )
                        ),
                        # Yes; does it contain an Iterable? If so, rewrap it. If
                        # not, strip it.
                        QAST::Op.new(
                            :op('if'),
                            QAST::Op.new(
                                :op('istype'),
                                QAST::Var.new( :name($result), :scope('local') ),
                                $Iterable
                            ),
                            QAST::Op.new(
                                :op('p6bindattrinvres'),
                                QAST::Op.new( :op('create'), $Scalar ),
                                $Scalar,
                                QAST::SVal.new( :value('$!value') ),
                                QAST::Op.new(
                                    :op('decont'),
                                    QAST::Var.new( :name($result), :scope('local') )
                                )
                            ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($result), :scope('local') )
                            )
                        ),
                        # Not rw, so leave container in place.
                        QAST::Var.new( :name($result), :scope('local') )
                    ),
                    # Not a container, so just hand back value
                    QAST::Var.new( :name($result), :scope('local') )
                )
            )
        }
    });
}
{
    my $is_moar;
    register_op_desugar('p6assign', -> $qast {
        unless nqp::isconcrete($is_moar) {
            $is_moar := nqp::getcomp('Raku').backend.name eq 'moar';
        }
        if $is_moar {
            my $cont := QAST::Node.unique('assign_cont');
            my $value := QAST::Node.unique('assign_value');
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($cont), :scope('local'), :decl('var') ),
                    $qast[0]
                ),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($value), :scope('local'), :decl('var') ),
                    QAST::Op.new( :op('decont'), $qast[1] )
                ),
                QAST::Op.new(
                    :op('call'),
                    QAST::Op.new(
                        :op('speshresolve'),
                        QAST::SVal.new( :value('assign') ),
                        QAST::Var.new( :name($cont), :scope('local') ),
                        QAST::Var.new( :name($value), :scope('local') ),
                    ),
                    QAST::Var.new( :name($cont), :scope('local') ),
                    QAST::Var.new( :name($value), :scope('local') ),
                ),
                QAST::Var.new( :name($cont), :scope('local') )
            )
        }
        else {
            QAST::Op.new( :op('assign'), $qast[0], $qast[1] )
        }
    });
}

sub can-use-p6forstmt($block) {
    my $past_block := $block.ann('past_block');
    my $count := $past_block.ann('count');
    return 0 unless nqp::isconcrete($count) && $count == 1;
    my $code := $block.ann('code_object');
    my $block_type := $*W.find_single_symbol('Block', :setting-only);
    return 1 unless nqp::istype($code, $block_type);
    my $p := nqp::getattr($code, $block_type, '$!phasers');
    nqp::isnull($p) ||
        !(nqp::existskey($p, 'FIRST') || nqp::existskey($p, 'LAST') || nqp::existskey($p, 'NEXT'))
}

sub monkey_see_no_eval($/) {
    my $msne := $*LANG.pragma('MONKEY-SEE-NO-EVAL');
    nqp::defined($msne)
        ?? $msne   # prevails if defined, can be either 1 or 0
        !! $*COMPILING_CORE_SETTING
            || try { $*W.find_single_symbol('&MONKEY-SEE-NO-EVAL')() };
}

role STDActions {
    method quibble($/) {
        make $<nibble>.ast;
    }

    method trim_heredoc($/, $doc, $stop, $origast) {
        $origast.pop();
        $origast.pop();

        my str $ws := $stop.MATCH<ws>.Str;
        my int $actualchars := nqp::chars($ws);
        my int $indent := -$actualchars;

        my int $tabstop := $*W.find_single_symbol('$?TABSTOP');
        my int $checkidx := -1;
        while ++$checkidx < $actualchars {
            if nqp::eqat($ws, "\t", $checkidx) {
                $indent := $indent - ($tabstop - 1);
            }
        }

        my $docast := $doc.MATCH.ast;
        if $docast.has_compile_time_value {
            my str $dedented := nqp::unbox_s($docast.compile_time_value.indent($indent));
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
                                my $strbox := nqp::box_s(nqp::x(" ", -$indent) ~ nqp::unbox_s($strval), $*W.find_single_symbol("Str"));
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

        CONTROL {
            if nqp::getextype($_) == nqp::const::CONTROL_WARN {
                $/.worry(nqp::getmessage($_));
                nqp::resume($_);
            }
            nqp::rethrow($_);
        }

        $origast;
    }
}

class Perl6::Actions is HLL::Actions does STDActions {
    #================================================================
    # AMBIENT AND POD-COMMON CODE HANDLERS
    #================================================================

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
        # If, e.g., we support Raku up to v6.1.2, set
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
        nqp::istype($past, QAST::Op) && %sinkable{$past.op} && $*statement_level && !$past.nosink
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

    sub string_to_int($src, int $base, int $chars) {
        my $res := nqp::radix($base, ~$src, 0, 2);
        $src.panic("'$src' is not a valid number")
            unless nqp::iseq_i(nqp::atpos($res, 2), $chars);
        nqp::box_i(nqp::atpos($res, 0), $*W.find_single_symbol('Int'));
    }

    sub string_to_bigint($src, int $base, int $chars) {
        my $res := nqp::radix_I($base, ~$src, 0, 2, $*W.find_single_symbol('Int'));
        $src.panic("'$src' is not a valid number")
            unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), $chars);
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
        my $fornode := QAST::Op.new(
            :op<p6for>, :node($/),
            QAST::Op.new(:op<call>, :name<&lines>),
            block_closure(make_topic_block_ref($/, $code, copy => 1)),
        );
        if can-use-p6forstmt($fornode[1]) {
            $fornode.op('p6forstmt');
            $fornode.annotate('IterationEnd', $*W.find_single_symbol('IterationEnd'));
            $fornode.annotate('Nil', $*W.find_single_symbol('Nil'));
        }
        return $fornode;
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

        # Errors/warnings in sinking pass should ignore highwater mark.
        $/.'!clear_highwater'();

        unless $*NEED_RESULT {
            # Evaluate last statement in sink context, by pushing another
            # statement after it, unless we need the result.
            unwantall($mainline, 'comp_unit');
            $mainline.push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
        }
        fatalize($mainline) if $*FATAL;

        # Emit any worries.  Note that unwanting $mainline can produce worries.
        if @*WORRIES {
            stderr().print($*W.group_exception().gist());
        }

        if %*COMPILING<%?OPTIONS><p> { # also covers the -np case, like Perl
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

        # If the unit defines &MAIN, and this is in the mainline,
        # add a call to &RUN-MAIN
        if !$*W.is_precompilation_mode
          && !$*INSIDE-EVAL
          && +(@*MODULES // []) == 0
          && $unit.symbol('&MAIN') -> $main {
            $mainline := QAST::Op.new(
              :op('call'),
              :name('&RUN-MAIN'),
              QAST::WVal.new(:value($main<value>)),
              $mainline             # run the mainline and get its result
            );
            unless $*W.lang-rev-before('d') {
                $mainline.push(
                  QAST::WVal.new( # $*IN as $*ARGSFILES
                    value => $*W.find_symbol(['Bool','True'], :setting-only),
                    :named('in-as-argsfiles')
                  )
                );
            }
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
        make QAST::CompUnit.new(
            :hll('Raku'),

            # Serialization related bits.
            :sc($*W.sc()),
            :code_ref_blocks($*W.code_ref_blocks()),
            :compilation_mode($*W.is_precompilation_mode()),
            :pre_deserialize($*W.load_dependency_tasks()),
            :post_deserialize($*W.fixup_tasks()),
            :is_nested($*W.is_nested()),
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
        ).annotate_self( # Pass some extra bits along to the optimizer.
            'UNIT', $unit
        ).annotate_self(
            'CAN_LOWER_TOPIC', $*CAN_LOWER_TOPIC
        ).annotate_self('GLOBALish', $*GLOBALish).annotate_self('W', $*W)
    }

    method install_doc_phaser($/) {
        # Add a default DOC INIT phaser
        my $doc := %*COMPILING<%?OPTIONS><doc>;
        if $doc {
            my $block := $*W.push_lexpad($/);

            my $renderer := "Pod::To::$doc";

            my $module := $*W.load_module($/, $renderer, {}, $block);

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
                $/, 'INIT', $*W.create_code_obj_and_add_child($block, 'Block'), $block
            );
        }
    }

    method unitstart($/) {
        # Use SET_BLOCK_OUTER_CTX (inherited from HLL::Actions)
        # to set dynamic outer lexical context and namespace details
        # for the compilation unit.
        self.SET_BLOCK_OUTER_CTX($*UNIT_OUTER);
    }

    method lang-version($/) {
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
                        if nqp::istype($ast,QAST::Op) && ($ast.op eq 'while' || $ast.op eq 'until' || $ast.op eq 'repeat_while' || $ast.op eq 'repeat_until') {
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
        if nqp::elems($past.list) < 1 {
            $past.push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
        }
        else {
            my $pl := $past[nqp::elems($past) - 1];
            if $pl.sunk {
                $past.push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
            }
            else {
                $pl.final(1);
                $past.returns($pl.returns);
            }
        }
        make $past;
    }

    # Produces a LoL from a semicolon list
    method semilist($/) {
        if $<statement> -> $statements {
            my $past := QAST::Stmts.new( :node($/) );
            if nqp::elems($statements) > 1 {
                my $l := QAST::Op.new( :name('&infix:<,>'), :op('call') );
                for $statements {
                    my $sast := $_.ast || QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
                    $l.push(wanted($sast, 'semilist'));
                }
                $past.push($l);
                $past.annotate('multislice', 1);
            }
            else {
                my $ast := $statements[0].ast;

                # an op and 6e or higher?
                if nqp::istype($ast,QAST::Op) && !$*W.lang-rev-before("e") {
                    sub is-pipe-pipe($ast) {
                        nqp::istype($ast,QAST::Op)
                          && $ast.name eq '&prefix:<|>'
                          && nqp::istype($ast[0],QAST::Op)
                          && $ast[0].name eq '&prefix:<|>'
                    }

                    if is-pipe-pipe($ast) {
                        $ast := $ast[0][0];  # cut out the || ops
                        $past.annotate('multislice', 1);
                    }
                    elsif $ast.name eq '&infix:<,>' && is-pipe-pipe($ast[0]) {
                        $ast[0] := $ast[0][0][0];  # cut out the || ops
                        $past.annotate('multislice', 1);
                    }
                }

                $past.push($ast || QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
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
                    unless $cond_block.ann('placeholder_sig') {
                        $cond_block.arity(0);
                        $cond_block.annotate('count', 0);
                    }
                    $past := $cond_block;
                }
                $mc_ast.push($past);
                $mc_ast.push(QAST::WVal.new( :value($*W.find_single_symbol('Empty')) ));
                $past := $mc_ast;
            }
            if $ml {
                $past.okifnil(1);
                $past[0].okifnil(1) if +@($past);
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
                    my $fornode := QAST::Op.new(
                            :op<p6for>, :node($/),
                            $cond,
                            block_closure($past),
                        );
                    $past := QAST::Want.new(
                        $fornode,
                        'v', QAST::Op.new(:op<p6sink>, $fornode),
                    );
                    $past[2].sunk(1);
                    my $sinkee := $past[0];
                    $past.annotate('statement_level', -> {
                        UNWANTED($sinkee, 'force for mod');
                        $fornode.op('p6forstmt') if can-use-p6forstmt($fornode[1]);
                        $fornode.annotate('IterationEnd', $*W.find_single_symbol('IterationEnd'));
                        $fornode.annotate('Nil', $*W.find_single_symbol('Nil'));
                    });
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
            if $/.pragma('trace') && !$*W.is_precompilation_mode {
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
                            :op<writefh>,
                            QAST::Op.new(:op<getstderr>),
                            QAST::Op.new(
                                :op('encode'),
                                QAST::SVal.new(:value("$id ($file line $line)\n$code\n")),
                                QAST::SVal.new(:value('utf8')),
                                QAST::Op.new(
                                    :op('callmethod'), :name('new'),
                                    QAST::WVal.new( :value($*W.find_single_symbol('Blob')) )
                                )
                            )
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
                    my $optional := $*IMPLICIT == 1;
                    @params.push(hash(
                        :variable_name('$_'), :$optional,
                        :type($*W.find_single_symbol('Mu')),
                        :default_from_outer($optional), :is_raw(1),
                    ));
                }
                elsif !$block.symbol('$_') {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ),'pblock/sawone'),
                        WANTED(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ),'pblock/sawone')
                    ));
                    $block.symbol('$_', :scope('lexical'), :type($*W.find_single_symbol('Mu')));
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
            $*W.push_inner_block(my $uninst := QAST::Stmts.new($block));
            Perl6::Pod::document($/, $*DECLARAND, $*POD_BLOCK, :leading);
            $*W.attach_signature($*DECLARAND, $signature);
            $*W.finish_code_object($*DECLARAND, $block);
            $*W.add_phasers_handling_code($*DECLARAND, $block);
            make reference_to_code_object($*DECLARAND, $block).annotate_self(
                'uninstall_if_immediately_used', $uninst
            )
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
        $*W.push_inner_block(my $uninst := QAST::Stmts.new($block));
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
            fatalize($past) if $*FATAL;
            make $BLOCK;
        }
        else {
            if $*HAS_YOU_ARE_HERE {
                $/.panic('{YOU_ARE_HERE} may only appear once in a setting');
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
            my %handlers := %*HANDLERS;
            for sorted_keys(%handlers) {
                $past.push($_);
                $past.push(%handlers{$_});
            }
        }
        $past
    }

    # Under "use fatal", re-write all calls to fatalize their return value
    # unless we can see they are in a boolean context.
    my %boolify_first_child_ops := nqp::hash(
        'if', 1, 'unless', 1, 'defor', 1, 'hllbool', 1,
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
                        $ast.push(QAST::WVal.new( :value($*W.find_single_symbol('Failure')) ));
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
        $*W.cur_lexpad().annotate_self('IN_DECL', $*IN_DECL);
    }

    method finishpad($/) {
        # Generate the $_, $/, and $! lexicals for routines if they aren't
        # already declared. For blocks, $_ will come from the outer if it
        # isn't already declared.
        my $BLOCK := $*W.cur_lexpad();
        my $type := $BLOCK.ann('IN_DECL');
        if $type eq 'mainline' && %*COMPILING<%?OPTIONS><setting> eq 'NULL.c' {
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
                $BLOCK.symbol('$_', :scope('lexical'), :type($*W.find_single_symbol('Mu')));
            }
        }
    }


    ## Statement control

    method statement_control:sym<if>($/) {
        my $count := nqp::elems($<xblock>) - 1;
        my $past;
        (my $empty := QAST::WVal.new: :value($*W.find_symbol: ['Empty'])
        ).annotate: 'ok_to_null_if_sunk', 1;
        if ~$<sym>[$count] ~~ /with/ {
            $past := xblock_immediate_with( $<xblock>[$count].ast );
            $past.op('with');
            $past.push: $<else> ?? pblock_immediate_with($<else>.ast) !! $empty;
        }
        else {
            $past := xblock_immediate( $<xblock>[$count].ast );
            $past.op('if');
            $past.push: $<else> ?? pblock_immediate($<else>.ast) !! $empty;
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
        my $past := xblock_immediate( $<xblock>.ast );
        $past.push(QAST::WVal.new( :value($*W.find_single_symbol('Empty')) ));
        $past.op('unless');
        make $past;
    }

    method statement_control:sym<without>($/) {
        my $past := xblock_immediate_with( $<xblock>.ast );
        $past.push(QAST::WVal.new( :value($*W.find_single_symbol('Empty')) ));
        $past.op('without');
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
        my $fornode := QAST::Op.new(
                :op<p6for>, :node($/),
                $xblock[0],
                block_closure($xblock[1]),
            );
        my $past := QAST::Want.new(
            $fornode,
            'v', QAST::Op.new(:op<p6sink>, $fornode),
        );
        if $*LABEL {
            my $label := QAST::WVal.new( :value($*W.find_single_symbol($*LABEL)), :named('label') );
            $past[0].push($label);
        }
        $past[2].sunk(1);
        my $sinkee := $past[0];
        $past.annotate('statement_level', -> {
            UNWANTED($sinkee,'force for');
            if can-use-p6forstmt($fornode[1]) {
                $fornode.op('p6forstmt');
                $fornode.annotate('IterationEnd', $*W.find_single_symbol('IterationEnd'));
                $fornode.annotate('Nil', $*W.find_single_symbol('Nil'));
            }
        });
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
        my $sinkee := $loop[1];
        $loop.annotate('statement_level', -> {
            UNWANTED($sinkee,'force loop');
            if $<e1> {
                $loop.push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
            }
        });
        make $loop;
    }

    sub tweak_loop($loop) {
        if $*LABEL {
            $loop.push(QAST::WVal.new( :value($*W.find_single_symbol($*LABEL)), :named('label') ));
        }
        # Handle phasers.
        my $code := $loop[1].ann('code_object');
        my $block_type := $*W.find_single_symbol('Block', :setting-only);
        my $phasers := nqp::getattr($code, $block_type, '$!phasers');
        if nqp::isnull($phasers) {
            $loop[1] := pblock_immediate($loop[1]);
        }
        else {
            my $node := $loop.node;
            if nqp::existskey($phasers, 'NEXT') {
                my $phascode := $*W.run_phasers_code($code, $loop[1], $block_type, 'NEXT');
                if +@($loop) == 2 {
                    $loop.push($phascode);
                }
                else {
                    $loop[2] := QAST::Stmts.new: :$node, $phascode, $loop[2];
                }
            }
            if nqp::existskey($phasers, 'FIRST') {
                my $tmp  := QAST::Node.unique('LOOP_BLOCK');
                my $var  := QAST::Var.new: :$node, :name($tmp), :scope<local>;
                $loop := QAST::Stmts.new(:$node,
                    QAST::Op.new(:$node, :op<bind>, $var.decl_as('var'),
                      QAST::Op.new: :$node, :op<p6setfirstflag>, $loop[1]),
                    $loop);
                $loop[1][1] := QAST::Op.new(:$node, :op<call>, $var
                  ).annotate_self: 'loop-already-block-first-phaser', $loop;
            }
            else {
                $loop[1] := pblock_immediate($loop[1]);
            }
            if nqp::existskey($phasers, 'LAST') {
                $loop := QAST::Stmts.new(:$node, :resultchild(0), $loop,
                  $*W.run_phasers_code: $code, $loop[1], $block_type, 'LAST');
            }
        }
        $loop
    }

    method statement_control:sym<need>($/) {
        my $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
        make $past;
    }

    method statement_control:sym<import>($/) {
        # NB: Grammar already passed arglist directly to World, but this seems soon enough to want it.
        if $<arglist> && $<arglist><EXPR> {
            WANTED($<arglist><EXPR>.ast, 'import');
        }
        my $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
        make $past;
    }

    method statement_control:sym<use>($/) {
        my $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
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
#                    $/.panic("Perl $<version> required--this is only v$mpv")
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
        my $*SCOPE := 'my';
        if $<module_name> {
            for $<module_name><longname><colonpair> -> $colonpair {
                if ~$colonpair<identifier> eq 'file' {
                    $has_file := $colonpair.ast[2];
                    last;
                }
            }
            $longname := $*W.dissect_longname($<module_name><longname>);
            $target_package := $longname.name_past;
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
        my $lexpad := $*W.cur_lexpad();
        my $block := $lexpad.ann('code_object');
        $block := $*W.blocks[+$*W.blocks - 2] if $block.HOW.name($block) eq 'Code';
        if !$lexpad.symbol('%REQUIRE_SYMBOLS') {
            declare_variable($/, $past, '%', '', 'REQUIRE_SYMBOLS', []);
            $*W.mark_lexical_used_implicitly($lexpad, '%REQUIRE_SYMBOLS');
        }
        my $require_past := WANTED(QAST::Op.new(:node($/), :op<call>,
                                        :name<&REQUIRE_IMPORT>,
                                        $compunit_past,
                                        ),'require');

        # An list of the components of the pre-existing outer symbols name (if any)
        my $existing_path := $*W.symbol_lookup(['Any'], $/);
        # The top level package object of the  pre-existing outer package (if any)
        my $top-existing := $*W.symbol_lookup(['Any'], $/);
        # The name of the lexical stub we insert (if any)
        my $lexical_stub;
        if $target_package && !$longname.contains_indirect_lookup() {
            my $current;
            my @components := nqp::clone($longname.components);
            my $top := @components.shift;
            $existing_path := QAST::Op.new(:op<call>, :name('&infix:<,>'));
            my $existing := try $*W.find_single_symbol($top);

            if $existing =:= NQPMu {
                my $stub := $*W.pkg_create_mo($/, $/.how('package'), :name($top));
                $*W.pkg_compose($/, $stub);
                $*W.install_lexical_symbol($lexpad,$top,$stub);
                $current := nqp::who($stub);
                $lexical_stub := QAST::SVal.new(:value($top));
            }
            else {
                $top-existing := QAST::WVal.new(:value($existing));
                $current := nqp::who($existing);
                $existing_path.push: QAST::SVal.new(:value($top));
            }

            for @components -> $component {
                if nqp::existskey($current,$component) {
                    $current := nqp::who($current{$component});
                    $existing_path.push: QAST::SVal.new(:value($component));
                }
                else {
                    $lexical_stub := QAST::SVal.new(:value($component)) unless $lexical_stub;
                    my $stub := $*W.pkg_create_mo($/, $/.how('package'), :name($component));
                    $*W.pkg_compose($/, $stub);
                    $current{$component} := $stub;
                    $current := nqp::who($stub);
                }
            }
        }

        $require_past.push($existing_path);
        $require_past.push($top-existing);
        $require_past.push($lexical_stub // $*W.symbol_lookup(['Any'], $/));

        if $<EXPR> {
            my $p6_argiter   := $*W.compile_time_evaluate($/, WANTED($<EXPR>.ast,'require')).eager.iterator;
            my $IterationEnd := $*W.find_single_symbol('IterationEnd');

            while !((my $arg := $p6_argiter.pull-one) =:= $IterationEnd) {
                my str $symbol := nqp::unbox_s($arg.Str());
                $*W.throw($/, ['X', 'Redeclaration'], :$symbol)
                    if $lexpad.symbol($symbol);
                $*W.install_lexical_symbol(
                    $lexpad,
                    $symbol,
                    $*W.find_symbol(['Metamodel', 'GenericHOW']).new_type(:name($symbol))
                );
                $require_past.push($*W.add_string_constant($symbol));
            }
        }
        $past.push($require_past);
        my $unwanted := $past.shallow_clone();
        $past.push($<module_name>
                   ?? self.make_indirect_lookup($longname.components())
                   !! $<file>.ast);
        make QAST::Want.new(
            $past,
            'v',
            $unwanted
        );
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

        # Handle the smartmatch, but try to gen some more optimized code first.
        my $match_past;

        # Fast path for types, e.g., `when Int { ... }`
        if nqp::istype($sm_exp, QAST::WVal) && !nqp::isconcrete($sm_exp.value) {
            $match_past :=
                QAST::Op.new( :op('istype'),
                  WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'when'),
                  $sm_exp );
        }
        # Fast path for things like literals, e.g., `when 5 { ... }`
        elsif nqp::istype($sm_exp, QAST::Want) {
            my $op_type;
            my $method;
            if nqp::istype($sm_exp[2], QAST::IVal) {
                $op_type := '==';
                $method  := 'Numeric';
            }
            elsif nqp::istype($sm_exp[2], QAST::SVal) {
                $op_type := 'eq';
                $method  := 'Stringy';
            }
            elsif nqp::istype($sm_exp[2], QAST::NVal) {
                $op_type := '==';
                $method  := 'Numeric';
            }
            else {
                die("Unknown QAST::Want type " ~ $sm_exp[2].HOW.name($sm_exp[2]));
            }

            # Coercion method to call on the given value
            my $method_call :=
                QAST::Op.new( :op('callmethod'), :name($method),
                  QAST::Var.new( :name('$_'), :scope('lexical') ) );

            # We don't need/want `val()` to `fail()` if `Numeric()` ends up calling it
            # and it doesn't succeed, that creates an expensive Backtrace that we just
            # end up throwing away
            if $method eq 'Numeric' {
                my $fail-or-nil := QAST::Op.new( :op('hllbool'), QAST::IVal.new( :value(1) ) );
                $fail-or-nil.named('fail-or-nil');
                $method_call.push($fail-or-nil);
            }

            # Make sure we're not comparing against a type object, since those could
            # coerce to the value, so gen the equivalent of
            # `isconcrete($_) && <literal> ==|eq $_."$method"`
            my $is_eq :=
                QAST::Op.new( :op('if'),
                  QAST::Op.new( :op('isconcrete' ),
                    QAST::Var.new( :name('$_'), :scope('lexical') ) ),
                  QAST::Op.new( :op('call'), :name("&infix:<$op_type>"),
                    $sm_exp[2],
                    WANTED($method_call,'when') ) );

            # Needed so we can `handle` the code below
            fatalize($is_eq);

            # This is the equivalent of sticking a `try` before the genned `iseq_*`, which is
            # needed because otherwise it'll die with an error when the `$_` is a different type.
            $match_past :=
                QAST::Op.new( :op('handle'),

                  # Success path evaluates to the block.
                  $is_eq,

                  # On failure, just evaluate to False
                  'CATCH',

                  QAST::WVal.new( :value( $*W.find_single_symbol('False') ) ) );
        }
        # The fallback path that does a full smartmatch
        else {
            $match_past :=
              QAST::Op.new( :op('callmethod'), :name('ACCEPTS'),
                $sm_exp,
                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'when') );
        }

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
        make QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
    }

    method statement_control:sym<CONTROL>($/) {
        if nqp::existskey(%*HANDLERS, 'CONTROL') {
            $*W.throw($/, ['X', 'Phaser', 'Multiple'], block => 'CONTROL');
        }
        my $block := $<block>.ast;
        set_block_handler($/, $block, 'CONTROL');
        make QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
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
                QAST::WVal.new( :value($*W.find_single_symbol('Nil')) )
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
        my $qast_block := $<blorst>.ast.ann('past_block');
        begin_time_lexical_fixup($qast_block);
        $qast_block.annotate('BEGINISH', 1);
        make $*W.add_phaser($/, 'BEGIN', wanted($<blorst>.ast,'BEGIN').ann('code_object'));
    }
    method statement_prefix:sym<CHECK>($/) {
        my $qast_block := $<blorst>.ast.ann('past_block');
        begin_time_lexical_fixup($qast_block);
        $qast_block.annotate('BEGINISH', 1);
        make $*W.add_phaser($/, 'CHECK', wanted($<blorst>.ast,'CHECK').ann('code_object'));
    }
    method statement_prefix:sym<COMPOSE>($/) { make $*W.add_phaser($/, 'COMPOSE', unwanted($<blorst>.ast,'COMPOSE').ann('code_object')); }
    method statement_prefix:sym<INIT>($/)    { make $*W.add_phaser($/, 'INIT', wanted($<blorst>.ast,'INIT').ann('code_object'), ($<blorst>.ast).ann('past_block')); }
    method statement_prefix:sym<ENTER>($/)   { make $*W.add_phaser($/, 'ENTER', wanted($<blorst>.ast,'ENTER').ann('code_object')); }
    method statement_prefix:sym<FIRST>($/)   { make $*W.add_phaser($/, 'FIRST', wanted($<blorst>.ast,'FIRST').ann('code_object')); }

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
            make QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
        }
    }

    method statement_prefix:sym<do>($/) {
        make QAST::Op.new( :op('call'), $<blorst>.ast );
    }

    method statement_prefix:sym<gather>($/) {
        my $past := unwanted($<blorst>.ast,'gather');
        $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
        make QAST::Op.new( :op('call'), :name('&GATHER'), $past );
    }

    method statement_prefix:sym<supply>($/) {
        # Case-analyze what's inside of the Supply, to spot cases that can be
        # turned into something cheap rather than needing the whole supply
        # concurrency control mechanism.
        my $past := $<blorst>.ast;
        my $block := $past.ann('past_block');
        if $*WHENEVER_COUNT == 0 {
            my $stmts := $block[1];
            if nqp::istype($stmts, QAST::Stmts) && nqp::elems($stmts) == 1 {
                my $stmt := $stmts[0];
                $stmt := $stmt[0] if nqp::istype($stmt, QAST::Want);
                if nqp::istype($stmt, QAST::Op) && $stmt.op eq 'call' && $stmt.name eq '&emit'
                    && nqp::elems($stmt.list) == 1 {
                    # Single statement emit; make block just return the expression
                    # (or die) and pass it to something that'll cheaply do a one
                    # shot emit.
                    $stmts[0] := $stmt[0];
                    make QAST::Op.new( :op('call'), :name('&SUPPLY-ONE-EMIT'), $past );
                    return 1;
                }
            }
        }
        elsif single_top_level_whenever($block) {
            $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
            make QAST::Op.new( :op('call'), :name('&SUPPLY-ONE-WHENEVER'), $past );
            return 1;
        }
        $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
        make QAST::Op.new( :op('call'), :name('&SUPPLY'), $past );
    }

    sub single_top_level_whenever($block) {
        if $*WHENEVER_COUNT == 1
        && nqp::getcomp('Raku').language_version ne '6.c' {
            my $stmts := $block[1];
            if nqp::istype($stmts, QAST::Stmts) {
                my @stmts := $stmts.list;
                my $last := @stmts[nqp::elems(@stmts) - 1];
                if nqp::istype($last, QAST::Stmt) {
                    return 0 if nqp::elems($last.list) != 1;
                    $last := $last[0];
                }
                if nqp::istype($last, QAST::Want) {
                    $last := $last[0];
                }
                if nqp::istype($last, QAST::Op) && $last.op eq 'call' && $last.name eq '&WHENEVER' {
                    return 1;
                }
            }
        }
        return 0;
    }

    method statement_prefix:sym<react>($/) {
        my $past := $<blorst>.ast;
        my $block := $past.ann('past_block');
        if single_top_level_whenever($block) {
            $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
            make QAST::Op.new( :op('call'), :name('&REACT-ONE-WHENEVER'), $past );
        }
        else {
            $past.ann('past_block').push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
            make QAST::Op.new( :op('call'), :name('&REACT'), $past );
        }
    }

    method statement_prefix:sym<once>($/) {
        # create state variable to remember whether we ran the block
        my $pad := $*W.cur_lexpad();
        my $sym := $pad.unique('once_');
        my $mu := $*W.find_single_symbol('Mu');
        my $descriptor := $*W.create_container_descriptor($mu, $sym);
        my %info;
        %info<container_type> := %info<container_base> := $*W.find_single_symbol('Scalar');
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
          :op('decont'),
          QAST::Op.new(
            :op('if'),
            QAST::Op.new( :op('p6stateinit') ),
            QAST::Op.new(
              :op('p6store'),
              WANTED(QAST::Var.new(:name($sym), :scope('lexical')),'once'),
              QAST::Op.new( :op('call'), wanted($<blorst>.ast,'once') )
            ),
            WANTED(QAST::Var.new( :name($sym), :scope('lexical') ),'once')
          )
        );
    }

    method statement_prefix:sym<start>($/) {
        my $block := $<blorst>.ast.ann('past_block');
        unless $block.symbol('$/') {
            $*W.install_lexical_magical($block, '$/');
        }
        unless $block.symbol('$!') {
            $*W.install_lexical_magical($block, '$!');
        }
        my $qast := QAST::Op.new(
            :op('callmethod'),
            :name('start'),
            :returns($*W.find_single_symbol('Promise')),
            QAST::WVal.new( :value($*W.find_single_symbol('Promise')) ),
            $<blorst>.ast
        );
        unless $*W.lang-rev-before('d') {
            $qast.push(QAST::WVal.new(
                :value($*W.find_symbol(['Bool', 'True'])),
                :named('report-broken-if-sunk')
            ));
        }
        make $qast;
    }

    method statement_prefix:sym<lazy>($/) {
        if $<for> {
            my $ast := $<for>.ast;
            $ast[0].annotate('mode', 'lazy');
            $ast.annotate('statement_level', NQPMu);
            make $ast;
        }
        else {
            make QAST::Op.new(
                :op('callmethod'), :name('lazy'),
                QAST::Op.new( :op('call'), $<blorst>.ast )
            );
        }
    }

    method statement_prefix:sym<eager>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('eager'),
            QAST::Op.new( :op('call'), $<blorst>.ast )
        );
    }

    method statement_prefix:sym<hyper>($/) {
        if $<for> {
            my $ast := $<for>.ast;
            $ast[0].annotate('mode', 'hyper');
            $ast.annotate('statement_level', NQPMu);
            make $ast;
        }
        else {
            make QAST::Op.new(
                :op('callmethod'), :name('hyper'),
                QAST::Op.new( :op('call'), $<blorst>.ast )
            );
        }
    }

    method statement_prefix:sym<race>($/) {
        if $<for> {
            my $ast := $<for>.ast;
            $ast[0].annotate('mode', 'race');
            $ast.annotate('statement_level', NQPMu);
            make $ast;
        }
        else {
            make QAST::Op.new(
                :op('callmethod'), :name('race'),
                QAST::Op.new( :op('call'), $<blorst>.ast )
            );
        }
    }

    method statement_prefix:sym<sink>($/) {
        my $qast :=
        QAST::Stmts.new: :node($/),
          QAST::Op.new(:op<callmethod>, :name<sink>,
            QAST::Op.new(:op<call>, $<blorst>.ast)),
          QAST::Var.new: :name<Nil>, :scope<lexical>;

        # if user is trying to sink a variable, don't complain about uselessness
        my $block-stmts := $<blorst>.ast.ann('past_block')[1];
        $block-stmts[0] := WANTED($block-stmts[0], 'statement_prefix/sink')
          if nqp::istype($block-stmts[0], QAST::Var);

        make $qast;
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
                        :value( $*W.find_single_symbol('Nil') ),
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
            !! QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
    }

    method colonpair($/) {
        if $*key {
            if $<var> {
                make make_pair($/,$*key, $<var>.ast);
            }
            elsif $<num> {
                make make_pair($/,$*key, $*W.add_numeric_constant($/, 'Int', $*value));
            }
            elsif $*value ~~ NQPCapture {
                my $val_ast := $*value.ast;
                if nqp::istype($val_ast, QAST::Stmts) && +@($val_ast) == 1 {
                    $val_ast := $val_ast[0];
                }
                make make_pair($/,$*key, $val_ast);
            }
            else {
                make make_pair($/,$*key, QAST::Op.new(
                    :op('hllbool'),
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
        my $pair := QAST::Op.new(
            :op('callmethod'), :name('new'), :returns($*W.find_single_symbol('Pair')), :node($/),
            WANTED(QAST::Var.new( :name('Pair'), :scope('lexical'), :node($/) ),'make_pair'),
            $key, WANTED($value, 'make_pair')
        );
        $pair.nosink(1);
        $pair;
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
                $*W.add_constant('Int', 'int', nqp::radix(10, $<index>, 0, 0)[0]),
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
                    $past.nosink(1);
                }
                else {
                    $past := make_variable($/, [$name]);
                }
            }
        }
        if $*IN_DECL eq 'variable' {
            $past.sinkok(1);
        }

        make $past;
    }

    method contextualizer($/) {
        my $past := $<coercee>.ast;
        my $has_magic := $*W.lang-rev-before('d') && $<coercee> eq '';

        if $has_magic && $<sigil> eq '$' { # for '$()'
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
            $past := QAST::Var.new( :name('$/'), :scope('lexical') ) if $has_magic;

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
              name       => $name,
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
        elsif ($twigil eq '.' || $twigil eq '.^') && $*IN_DECL ne 'variable' {
            if !$*HAS_SELF {
                $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => $name);
            } elsif $*HAS_SELF eq 'partial' {
                $*W.throw($/, ['X', 'Syntax', 'VirtualCall'], call => $name);
            }
            # Need to transform this to a method call.
            $past := $<arglist> ?? $<arglist>.ast !! QAST::Op.new();
            if $twigil eq '.^' {
                $past.op('p6callmethodhow');
            }
            else {
                $past.op('callmethod');
            }
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
            $past := QAST::Op.new(
                :op<callmethod>, :name<new>, :returns($*W.find_single_symbol('Slang')),
                QAST::Var.new( :name<Slang>, :scope<lexical> ));
            my $g := $/.slang_grammar($desigilname);
            $*W.add_object_if_no_sc($g);
            my $a := $/.slang_actions($desigilname);
            if !nqp::isnull($g) {
                my $wval := QAST::WVal.new( :value($g) );
                $wval.named('grammar');
                $past.push($wval);
                $wval := QAST::WVal.new( :value($a) );
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
        elsif $name eq '$?LANG' || $name eq '$?LINE' || $name eq '$?FILE' {
            if $*IN_DECL eq 'variable' {
                $*W.throw($/, 'X::Syntax::Variable::Twigil',
                  name       => $name,
                  twigil     => '?',
                  scope      => $*SCOPE,
                  additional => " because it is reserved",
                );
            }
            if $name eq '$?LANG' {
                my $cursor := $/;
                $*W.add_object_if_no_sc($cursor);
                $past := QAST::WVal.new(:value($cursor));
            }
            elsif $name eq '$?LINE' {
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
                    $*W.add_object_if_no_sc($resources);
                }
            }
            else {
                $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
            }
        }
        elsif $name eq '$?DISTRIBUTION' {
            my $distribution := nqp::getlexdyn('$*DISTRIBUTION');
            unless $distribution {
                my $Distribution := $*W.find_symbol(['CompUnit', 'Repository', 'Distribution']);
                $distribution := $Distribution.from-precomp();
            }
            if $distribution {
                $past := QAST::WVal.new( :value($distribution) );
                if nqp::isnull(nqp::getobjsc($distribution)) {
                    $*W.add_object_if_no_sc($distribution);
                }
            }
            else {
                $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
            }
        }
        elsif $name eq '&?BLOCK' || $name eq '&?ROUTINE' {
            if $*IN_DECL eq 'variable' {
                $*W.throw($/, 'X::Syntax::Variable::Twigil',
                  name       => $name,
                  twigil     => '?',
                  scope      => $*SCOPE,
                  additional => " because it is reserved",
                );
            }
            my $Routine := $*W.find_single_symbol('Routine', :setting-only);
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
                    QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
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
        $*W.apply_trait($/, '&trait_mod:<trusts>', $/.package, $<typename>.ast);
    }

    method package_declarator:sym<also>($/) {
        $*W.apply_traits($<trait>, $*DECLARAND);
    }

    method package_def($/) {
        my $*LEAF := $/;
        my $package := $*LEAF.package;
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
        if is_yada($/) {
            unless $*PKGDECL eq 'role' {
                $*W.add_stub_to_check($package);
            }
            $block.blocktype('declaration');
            make QAST::Stmts.new( $block, QAST::WVal.new( :value($package) ) );
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
                type_captures     => nqp::list_s('$?CLASS', '::?CLASS')
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
                QAST::WVal.new( :value($package) ),
                QAST::Op.new( :op('curlexpad') )));

            # Finish code object and add it as the role's body block.
            my $code := $*CODE_OBJECT;
            $*W.attach_signature($code, $sig);
            $*W.finish_code_object($code, $block, 0);
            $*W.add_phasers_handling_code($code, $block);
            $*W.pkg_set_role_body_block($/, $package, $code, $block);

            # Compose before we add the role to the group, so the group sees
            # it composed.
            $*W.pkg_compose($/, $package);

            # Add this role to the group if needed.
            my $group := $package.HOW.group($package);
            unless $group =:= $package {
                $*W.pkg_add_role_group_possibility($/, $group, $package);
            }
        }
        else {
            # Compose.
            $*W.pkg_compose($/, $package);

            # If we have a grammar on our hands, precompute some NFAs
            if $*PKGDECL eq 'grammar' {
                if nqp::can($package, '!precompute_nfas') {
                    $package.'!precompute_nfas'();
                }
            }

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
        Perl6::Pod::document($/, $package, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($package);
        }

        make QAST::Stmts.new(
            $block, QAST::WVal.new( :value($package) )
        );
    }

    # When code runs at BEGIN time, such as role bodies and BEGIN
    # blocks, we need to ensure we get lexical outers fixed up
    # properly when deserializing after pre-comp. To do this we
    # make a list of closures, which each point to the outer
    # context. These survive serialization and thus point at what
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
    method scope_declarator:sym<HAS>($/)     {
        my $scoped := $<scoped>.ast;
        my $attr   := $scoped.ann('metaattr');
        if $attr.package.REPR ne 'CStruct'
        && $attr.package.REPR ne 'CPPStruct'
        && $attr.package.REPR ne 'CUnion' {
            $*W.throw($/, ['X', 'Attribute', 'Scope', 'Package'], :scope<HAS>,
                :allowed('classes with CStruct, CPPStruct and CUnion representation are supported'),
                :disallowed('package with ' ~ $attr.package.REPR ~ ' representation'));
        }
        if nqp::objprimspec($attr.type) != 0 {
            $/.worry('Useless use of HAS scope on ' ~ $attr.type.HOW.name($attr.type) ~ ' typed attribute.');
        }

        if $attr.type.REPR eq 'CArray' {
            if $<scoped><DECL><declarator><variable_declarator><semilist> -> $semilist {
                my @dimensions := nqp::list_i();
                for $semilist -> $dimension {
                    my $elems := nqp::unbox_i($*W.compile_time_evaluate($/, $dimension.ast, :mark-wanted));
                    nqp::push_i(@dimensions, $elems);
                }
                nqp::bindattr($attr, $attr.WHAT, '$!dimensions', @dimensions);
            }
        }

        # Mark $attr as inlined, that's why we do all this.
        nqp::bindattr_i($attr, $attr.WHAT, '$!inlined', 1);
        make $scoped;
    }

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
                        my $type := nqp::defined($*OFTYPE)
                          ?? $*W.maybe-nominalize($*OFTYPE.ast) !! $*W.find_symbol: ['Any'];
                          # ?? $*W.maybe-definite-how-base($*OFTYPE.ast) !! $*W.find_symbol: ['Any'];
                        my $dot_equals := $initast;
                        $dot_equals.unshift(QAST::WVal.new(:value($type)));
                        $dot_equals.returns($type);
                        self.install_attr_init($<initializer>,
                            $past.ann('metaattr'),
                            $dot_equals, $*ATTR_INIT_BLOCK);
                    }
                    else {
                        $/.panic("Cannot use " ~ $<initializer><sym> ~
                            " to initialize an attribute");
                    }
                }
                elsif $<initializer><sym> eq '=' {
                    if $past.ann('init_removal') -> $remove {
                        $remove();
                    }
                    $past := assign_op($/, $past, $initast, :initialize);
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
                    if nqp::istype( (my $outer_block := $*W.cur_lexpad), QAST::Block) {
                        $outer_block.annotate('has_statevar', $outer_block[0]);
                    }

                    $past := QAST::Op.new( :op('if'),
                        QAST::Op.new( :op('p6stateinit') ),
                        $past,
                        $orig_past);
                    $past.nosink(1);
                }
            }
            # No initializer, check that the (specified) type accepts the default value.
            elsif $<variable_declarator><variable><sigil> eq '$' {
                if nqp::istype($past, QAST::Var) {
                    if $*W.cur_lexpad.symbol($past.name) -> %sym {
                        if %sym<descriptor> {
                            check_default_value_type($/, %sym<descriptor>, %sym<type>, 'variable');
                        }
                    }
                }
                elsif $past.ann('metaattr') -> $attr {
                    if !$attr.required && !$attr.type.HOW.archetypes.generic {
                        check_default_value_type($/, $attr.container_descriptor, $attr.type, 'attribute');
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
                        ($_<node> // $<signature>).typed_sorry(
                            'X::Syntax::Variable::ConflictingTypes',
                            outer => $common_of.ast, inner => $_<of_type>);
                    }
                    $*OFTYPE := $_<of_type_match>;
                    $*OFTYPE.make($_<of_type>);
                }

                my $post := $_<post_constraints> ?? $_<post_constraints> !! [];
                if $_<variable_name> {
                    my $name := $_<variable_name>;
                    my $sigil := $_<sigil>;
                    my $twigil := $_<twigil>;
                    my $desigilname := $_<desigilname>;
                    if $desigilname {
                        ensure_unused_in_scope($/, $name, $twigil)
                    }
                    my $past := QAST::Var.new( :$name );
                    $past := declare_variable($/, $past, $sigil, $twigil,
                        $desigilname, $<trait>, :$post);
                    unless nqp::istype($past, QAST::Op) && $past.op eq 'null' {
                        $list.push($past);
                        if $sigil eq '' {
                            nqp::push(@nosigil, ~$desigilname);
                        }
                    }
                }
                else {
                    $*W.handle_OFTYPE_for_pragma($/,'parameters');
                    my %cont_info := $*W.container_type_info($/, :$post,
                      $_<sigil> || '$', $*OFTYPE ?? [$*OFTYPE.ast] !! [], []);
                    $list.push($*W.build_container_past(
                      %cont_info,
                      $*W.create_container_descriptor(
                        %cont_info<value_type>, 'anon', %cont_info<default_value>)));
                }
            }

            if $<initializer> {
                my $orig_list := $list;
                my $initast := $<initializer>.ast;
                if $<initializer><sym> eq '=' {
                    $/.panic("Cannot assign to a list of 'has' scoped declarations")
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
                    $/.panic("Cannot use .= initializer with a list of declarations");
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
                $/.typed_panic('X::Syntax::Term::MissingInitializer');
            }
            else {
                $list := QAST::Want.new($list, 'v', QAST::Op.new( :op('null')));
            }

            make $list;
        }
        elsif $<deftermnow> {
            # 'my \foo' style declaration
            my $name      := $<deftermnow>.ast;
            my $init-qast := $<term_init>.ast;

            $init-qast.unshift:
              QAST::WVal.new: value => nqp::defined($*OFTYPE)
                ?? $*W.maybe-nominalize($*OFTYPE.ast) !! $*W.find_symbol: ['Mu']
                # ?? $*W.maybe-definite-how-base($*OFTYPE.ast) !! $*W.find_symbol: ['Mu']
            if $<term_init><sym> eq '.=';

            my $qast;
            if $*OFTYPE {
                my $type := $*OFTYPE.ast;
                $qast := QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                    $type =:= $*W.find_single_symbol('Mu')
                        ?? WANTED($init-qast, 'declarator/deftermnow3')
                        !! QAST::Op.new(
                            :op('p6bindassert'),
                            WANTED($init-qast, 'declarator/deftermnow2'),
                            QAST::WVal.new( :value($type) ),
                        )
                );
            }
            else {
                $qast := QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                        WANTED($init-qast, 'declarator/bind')
                );
            }
            make $<term_init><sym> eq '.='
              ?? $qast.annotate_self: 'fake_infix_adverb_target', $init-qast
              !! $qast
        }
        else {
            $/.panic('Unknown declarator type');
        }
    }

    sub check_default_value_type($/, $descriptor, $bind_constraint, $what) {
        my $matches;
        my $maybe := 0;
        try {
            $matches := nqp::istype($descriptor.default, $bind_constraint);
            CATCH {
                $maybe := 1;
                my $pl := nqp::getpayload($_);
                if nqp::istype($pl, $*W.find_single_symbol('Exception')) {
                    @*SORROWS.push($pl); # XXX Perhaps a method on Grammer similar to typed_sorry but which accepts an exception?
                } else {
                    # Don't be too verbose, report only the actual line with the error.
                    $/.sorry(nqp::getmessage($_), "\n", nqp::shift(nqp::backtracestrings($_)));
                }
            }
        }
        unless $matches {
            $/.typed_sorry('X::Syntax::Variable::MissingInitializer',
                what     => $what,
                type     => nqp::how($bind_constraint).name($bind_constraint),
                maybe    => $maybe,
                implicit => !nqp::istype($*OFTYPE, NQPMatch) || !$*OFTYPE<colonpairs> || $*OFTYPE<colonpairs> && !$*OFTYPE<colonpairs>.ast<D> && !$*OFTYPE<colonpairs>.ast<U>
                         ?? ':' ~ $/.pragma($what ~ 's') ~ ' by pragma'
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
        my $qast   := $<variable>.ast;
        my $sigil  := $<variable><sigil>;
        my $twigil := $<variable><twigil>;
        my $desigilname := ~$<variable><desigilname>;
        my $name := $sigil ~ $twigil ~ $desigilname;

        # Don't know why this doesn't work all the time.
        if $desigilname ~~ /\w ':' <![:]>/ {
            $name   := $<variable>.ast.name;  # is already canonicalized in <variable>
            $desigilname := nqp::substr($name, nqp::chars($sigil ~ $twigil));
        }

        if $<variable><desigilname> {
            my $lex := $*W.cur_lexpad();
            if $lex.symbol($name) {
                $/.typed_worry('X::Redeclaration', symbol => $name);
                unless $name eq '$_' {
                    $qast.scope('lexical') if nqp::istype($qast, QAST::Var) && !$qast.scope;
                    make $qast;
                    return;
                }
            }
            else {
                ensure_unused_in_scope($/, $name, $twigil);
            }
        }
        if nqp::elems($<semilist>) > 1 {
            $/.panic('Multiple shapes not yet understood');
        }
        my @post;
        for $<post_constraint> {
            @post.push($_.ast);
        }
        make declare_variable($/, $qast, ~$sigil, ~$twigil, $desigilname, $<trait>, $<semilist>, :@post);
    }

    sub ensure_unused_in_scope($/, $name, $twigil) {
        my $lex := $*W.cur_lexpad();
        if $lex.ann('also_uses') && $lex.ann('also_uses'){$name} {
            if ~$twigil eq '*' {
                $/.typed_sorry('X::Dynamic::Postdeclaration', symbol => $name);
            }
            else {
                $/.typed_sorry('X::Redeclaration::Outer', symbol => $name);
            }
        }
    }

    sub declare_variable($/, $past, $sigil, $twigil, $desigilname, $trait_list, $shape?, :@post) {
        my $name  := $sigil ~ $twigil ~ $desigilname;
        my $BLOCK := $*W.cur_lexpad();
        my $package := $/.package;

        my $of_type := nqp::null;
        my $is_type := nqp::null;

        $*W.handle_OFTYPE_for_pragma($/, $*SCOPE eq 'has' ?? 'attributes' !! 'variables');
        if $*OFTYPE {
            $of_type := $*OFTYPE.ast;
            my $archetypes := $of_type.HOW.archetypes;
            unless $archetypes.nominal
                || $archetypes.nominalizable
                || $archetypes.generic
                || $archetypes.definite
                || $archetypes.coercive {
                $*OFTYPE.typed_sorry('X::Syntax::Variable::BadType', type => $of_type);
            }
        }

        sub check_type($thrower, $outer, $inner) {
            unless nqp::isnull($outer) {
                $thrower.typed_sorry(
                  'X::Syntax::Variable::ConflictingTypes',
                  outer => $outer, inner => $inner
                );
            }
            $inner
        }

        # Process traits for `is Type` and `of Type`, which get special
        # handling by the compiler.
        my @late_traits;
        for $trait_list {
            my $trait := $_.ast;
            if $trait {
                my str $mod := $trait.mod;
                if $mod eq '&trait_mod:<of>' {
                    $of_type := check_type($_, $of_type, $trait.args[0]);
                    next;
                }

                # see if we need to handle "my $a is Foo"
                elsif $mod eq '&trait_mod:<is>' {
                    if nqp::elems($trait.args) -> $elems {
                        my $type := $trait.args[0];

                        # an actual type
                        unless nqp::isconcrete($type) {

                            # is Foo
                            if $elems == 1 {
                                $is_type := check_type($_, $is_type, $type);
                                next;  # handled the trait now
                            }

                            # is Foo[Bar]
                            elsif $elems == 2 && $trait.args[1] -> $params {
                                if nqp::istype($params,$*W.find_single_symbol('List')) {
                                    $is_type := check_type(
                                      $_, $is_type,
                                      $type.HOW.parameterize(
                                        $type, |$params.FLATTENABLE_LIST)
                                    );
                                    next;  # handled the trait now
                                }
                            }
                        }
                    }
                }
                nqp::push(@late_traits, $_);
            }
        }

        if $*SCOPE eq 'has' {
            # Ensure current package can take attributes.
            unless nqp::can($package.HOW, 'add_attribute') {
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
                $/.panic("Cannot declare an anonymous attribute");
            }
            my $attrname   := ~$sigil ~ '!' ~ $desigilname;
            my %cont_info  := $*W.container_type_info($/, $sigil,
                nqp::isnull($of_type) ?? [] !! [$of_type],
                nqp::isnull($is_type) ?? [] !! [$is_type],
                $shape, :@post);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, $attrname, %cont_info<default_value>);

            # Create meta-attribute and add it.
            my $metaattr := $*W.resolve_mo($/, $*PKGDECL ~ '-attr');
            my %config := hash(
                name => $attrname,
                has_accessor => $twigil eq '.',
                container_descriptor => $descriptor,
                type => %cont_info<bind_constraint>,
                package => $*W.find_single_symbol('$?CLASS'));
            if %cont_info<build_ast> {
                %config<container_initializer> := $*W.create_thunk($/,
                    %cont_info<build_ast>);
            }
            my $attr := $*W.pkg_add_attribute($/, $package, $metaattr,
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
            $past := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
            $past.annotate('metaattr', $attr);
        }
        elsif $*SCOPE eq 'my' || $*SCOPE eq 'our' || $*SCOPE eq 'state' {
            # Some things can't be done to our vars.
            my $varname;
            if $*SCOPE eq 'our' {
                if !nqp::isnull($of_type) || @post {
                    $/.panic("Cannot put a type constraint on an 'our'-scoped variable");
                }
                elsif $shape {
                    $/.panic("Cannot put a shape on an 'our'-scoped variable");
                }
                elsif $desigilname eq '' {
                    $/.panic("Cannot have an anonymous 'our'-scoped variable");
                }
                if nqp::can($package.HOW, 'archetypes') && $package.HOW.archetypes.parametric {
                    $*W.throw($/, 'X::Declaration::OurScopeInRole',
                        declaration => 'variable'
                    );
                }
            }
            elsif $desigilname eq '' {
                if $twigil {
                    $/.panic("Cannot have an anonymous variable with a twigil");
                }
                $name    := QAST::Node.unique($sigil ~ 'ANON_VAR_');
                $varname := $sigil;
            }

            # Create a container descriptor. Default to rw and set a
            # type if we have one; a trait may twiddle with that later.
            my %cont_info  := $*W.container_type_info($/, $sigil,
                nqp::isnull($of_type) ?? [] !! [$of_type],
                nqp::isnull($is_type) ?? [] !! [$is_type],
                $shape, :@post);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, $varname || $name, %cont_info<default_value>);

            # Install the container.
            my $cont := $*W.install_lexical_container($BLOCK, $name, %cont_info, $descriptor,
                :scope($*SCOPE), :package($package), :init_removal($past));

            # Set scope and type on container, and if needed emit code to
            # reify a generic type or create a fresh container.
            if nqp::istype($past, QAST::Var) {
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
                my $Variable := $*W.find_single_symbol('Variable');
                my $varvar   := nqp::create($Variable);
                nqp::bindattr_s($varvar, $Variable, '$!name', $name);
                nqp::bindattr_s($varvar, $Variable, '$!scope', $*SCOPE);
                nqp::bindattr($varvar, $Variable, '$!var', $cont);
                nqp::bindattr($varvar, $Variable, '$!block', $*CODE_OBJECT);
                nqp::bindattr($varvar, $Variable, '$!slash', $/);
                nqp::assign(
                    nqp::getattr($varvar, $Variable, '$!implicit-lexical-usage'),
                    $*W.find_symbol(['Bool', 'True']));
                $*W.apply_traits(@late_traits, $varvar);
                if $varvar.implicit-lexical-usage {
                    $*W.mark_lexical_used_implicitly($BLOCK, $name);
                }
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
        my $invocant_type := $*W.find_single_symbol($*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu');
        my %sig_info := hash(parameters => []);
        my $signature := $*W.create_signature_and_params($/, %sig_info, $a_past, 'Any',
            :method, :$invocant_type);
        my $code := methodize_block($/, $*W.stub_code_object('Method'),
            $a_past, $signature, %sig_info);
        $*W.add_phasers_handling_code($code, $a_past);

        install_method($/, $meth_name, 'has', $code, $install_in, :gen-accessor);
    }

    method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
    method routine_declarator:sym<method>($/) { make $<method_def>.ast; }
    method routine_declarator:sym<submethod>($/) { make $<method_def>.ast; }

    sub decontrv_op() {
        $*W.lang-rev-before('d') && nqp::getcomp('Raku').backend.name eq 'moar'
            ?? 'p6decontrv_6c'
            !! 'p6decontrv'
    }

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
            if $*MAY_USE_RETURN {
                $block[1] := wrap_return_handler($block[1]);
            }
            else {
                $block[1] := QAST::Op.new(
                    :op(decontrv_op()),
                    QAST::WVal.new( :value($*DECLARAND) ),
                    $block[1]);
                $block[1] := wrap_return_type_check($block[1], $*DECLARAND);
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
            $*W.install_lexical_symbol($block, '$*DISPATCHER', $*W.find_single_symbol('MultiDispatcher'));
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

        # Set name if we have one
        if $<deflongname> {
            my $name := ~$<deflongname>.ast;
            $block.name($name);

            # Check for 'is rw' parameters if MAIN
            if $name eq 'MAIN' {
                for $signature.params.FLATTENABLE_LIST -> $param {
                    $/.worry("'is rw' on parameters of 'sub MAIN' usually cannot be satisfied.\nDid you mean 'is copy'?")
                      if $param.rw;
                }
            }
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
                    postfix => "(previous return type was "
                                ~ $prev_returns.HOW.name($prev_returns)
                                ~ ')',
                );
            }
            $sig.set_returns($*OFTYPE.ast);
        }
        # and mixin the parameterize callable for type checks
        if $signature.has_returns {
            my $callable := $*W.find_single_symbol('Callable');
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

        $*W.push_inner_block(QAST::Stmt.new($block));

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
                        $proto := $*W.find_single_symbol($name);
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
            elsif $*SCOPE eq 'anon' {
                # don't install anything
            }
            else {
                # Always install in lexpad unless predeclared.
                my $predeclared := $outer.symbol($name);
                if $predeclared {
                    my $Routine := $*W.find_single_symbol('Routine', :setting-only);
                    unless nqp::istype($predeclared<value>, $Routine)
                        && $predeclared<value>.yada {
                        $*W.throw($/, ['X', 'Redeclaration'],
                                symbol => ~$<deflongname>.ast,
                                what   => 'routine',
                        );
                    }
                }
                $*W.install_lexical_symbol($outer, $name, $code, :$clone);

                if $*SCOPE eq 'our' || $*SCOPE eq 'unit' {
                    # Also install in package, and set up code to
                    # re-bind it per invocation of its outer.
                    $*W.install_lexical_symbol($outer, $name, $code, :$clone);
                    my $package := $/.package;
                    if nqp::existskey($package.WHO, $name) {
                        $*W.throw($/, ['X', 'Redeclaration'],
                            symbol  => ~$<deflongname>.ast,
                            what    => 'routine',
                            postfix => '(already defined in package ' ~ $package.HOW.name($package) ~ ')'
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
                elsif $*SCOPE ne '' && $*SCOPE ne 'my' {
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
            if $/.pragma('soft') {
                $*W.find_single_symbol('&infix:<does>')($code, $*W.find_single_symbol('SoftRoutine', :setting-only));
            }
            else {
                self.maybe_add_inlining_info($/, $code, $signature, $block, @params);
            }
        }

        # If it's a proto, add it to the sort-at-CHECK-time queue.
        if $*MULTINESS eq 'proto' {
            $*W.add_proto_to_sort($code);
        }

        make block_closure(
            reference_to_code_object($code, $block)
        ).annotate_self('sink_ast', QAST::Op.new( :op('null') ));
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
                        QAST::WVal.new( :value($*W.find_single_symbol('Routine', :setting-only)) ),
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
        my @p_params := [hash(is_capture => 1, type => $*W.find_single_symbol('Mu') )];
        my $p_sig := $*W.create_signature(nqp::hash('parameter_objects',
            [$*W.create_parameter($/, @p_params[0])]));
        add_signature_binding_code($p_past, $p_sig, @p_params);
        my $code := $*W.create_code_object($p_past, 'Sub', $p_sig, 1);
        $*W.apply_trait($/, '&trait_mod:<is>', $code, :onlystar(1));
        $*W.add_proto_to_sort($code);
        $code
    }

    # There are two kinds of inlining that happen: that done by the virtual
    # machine we run on, and that done by Rakudo. The VM can do it based on
    # discovered hot paths, and in far more situations than we can here. At
    # the same time, we can generate much better code as a starting point
    # for the VM if we inline various operators on natively typed things. So,
    # that is our distinction: if all of the arguments of the sub are native
    # types, we figure there's a good chance it's a high-value inline and we
    # can try to do it at compile time, so we generate inlining info here. In
    # any other cases, we will not.
    my $SIG_ELEM_IS_COPY := 512;
    method maybe_add_inlining_info($/, $code, $sig, $past, @params) {
        # Cannot inline things with custom invocation handler or phasers.
        return 0 if nqp::can($code, 'CALL-ME');
        my $phasers := nqp::getattr($code,$*W.find_single_symbol('Block', :setting-only),'$!phasers');
        return 0 unless nqp::isnull($phasers) || !$phasers;

        # Make sure the block has the common structure we expect
        # (decls then statements).
        return 0 unless nqp::elems($past) == 2;

        # Ensure all parameters are native, simple, and build placeholders for
        # them. No parameters also means no inlining.
        return 0 unless @params;
        my $Param  := $*W.find_single_symbol('Parameter', :setting-only);
        my @p_objs := nqp::getattr($sig, $*W.find_single_symbol('Signature', :setting-only), '@!params');
        my %arg_placeholders;
        my int $n  := nqp::elems(@params);
        my int $i  := -1;
        while ++$i < $n {
            my %info := @params[$i];
            return 0 unless nqp::objprimspec(%info<type>); # non-native
            return 0 if %info<optional> || %info<post_constraints> ||  %info<bind_attr> ||
                %info<bind_accessor> || %info<named_names> || %info<type_captures>;
            my $param_obj := @p_objs[$i];
            my int $flags := nqp::getattr_i($param_obj, $Param, '$!flags');
            return 0 if $flags +& $SIG_ELEM_IS_COPY;
            %arg_placeholders{%info<variable_name>} :=
                QAST::InlinePlaceholder.new( :position($i) );
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
        try $PseudoStash := $*W.find_single_symbol('PseudoStash');
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
                if nqp::getcomp('QAST').operations.is_inlinable('Raku', $node.op) {
                    my $replacement := $node.shallow_clone();
                    my int $n := nqp::elems($node);
                    my int $i := -1;
                    while ++$i < $n {
                        $replacement[$i] := node_walker($node[$i]);
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
                my int $n := nqp::elems($node);
                my int $i := -1;
                while ++$i < $n {
                    $replacement[$i] := node_walker($node[$i]);
                }
                return clear_node($replacement);
            }

            # Want nodes need copying and every other child visiting.
            elsif nqp::istype($node, QAST::Want) {
                my $replacement := $node.shallow_clone();
                my int $i := 0;
                my int $n := nqp::elems($node);
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
            $past := WANTED($<blockoid>.ast,'method_def');
            if $*MAY_USE_RETURN {
                $past[1] := wrap_return_handler($past[1]);
            }
            else {
                $past[1] := QAST::Op.new(
                    :op(decontrv_op()),
                    QAST::WVal.new( :value($*DECLARAND) ),
                    $past[1]);
                $past[1] := wrap_return_type_check($past[1], $*DECLARAND);
            }
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

        if $past.ann('placeholder_sig') {
            my $placeholders := nqp::iterator($past.ann('placeholder_sig'));
            my @non-placeholder-names;
            my $method-name := $past.name;
            while $placeholders {
                my $placeholder := nqp::shift($placeholders);
                my $name := $placeholder<placeholder>;
                my $non-placeholder-name;
                if $placeholder<pos_slurpy> || $placeholder<named_slurpy> {
                    $non-placeholder-name := nqp::concat('*', $name);
                } elsif $placeholder<named_names> {
                    $non-placeholder-name := nqp::concat(':', nqp::concat(nqp::substr($name, 0, 1), nqp::substr($name, 2)));
                } else {
                    $non-placeholder-name := nqp::concat(nqp::substr($name, 0, 1), nqp::substr($name, 2));
                }
                nqp::push( @non-placeholder-names, $non-placeholder-name);
            }

            my $non-placeholder-names := nqp::join(', ', @non-placeholder-names);

            my $first-placeholder := $past.ann('placeholder_sig')[0];
            my $first-placeholder-name := $first-placeholder<placeholder>;

            $first-placeholder<node>.PRECURSOR.panic("Placeholder variables (eg. $first-placeholder-name) cannot be used in a method.\nPlease specify an explicit signature, like $*METHODTYPE $method-name ($non-placeholder-names) \{ ... \}");
        }

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
                    postfix => "(previous return type was "
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

        make block_closure(
            reference_to_code_object($code, $past)
        ).annotate_self('sink_ast', QAST::Op.new( :op('null') ))
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
                $*W.install_package_symbol($/, $/.package, $name, $code, 'macro');
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

        make block_closure(
            reference_to_code_object($code, $block)
        ).annotate_self('sink_ast', QAST::Op.new( :op('null') ))
    }

    sub methodize_block($/, $code, $past, $signature, %sig_info, :$yada) {
        my $*LEAF := $/;
        # Add signature binding code.
        add_signature_binding_code($past, $signature, %sig_info<parameters>);

        # Place to store invocant.
        $past[0].unshift(QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') ));
        $past.symbol('self', :scope('lexical'));

        # Needs a slot to hold a multi or method dispatcher.
        $*W.install_lexical_symbol($past, '$*DISPATCHER',
            $*W.find_single_symbol($*MULTINESS eq 'multi' ?? 'MultiDispatcher' !! 'MethodDispatcher'));
        $past[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));

        # Finish up code object.
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $past, $*MULTINESS eq 'proto', :yada($yada));

        $code
    }

    # Installs a method into the various places it needs to go.
    sub install_method($/, $name, $scope, $code, $outer, :$private, :$meta, :$gen-accessor) {
        my $meta_meth;
        my $package := $/.package;
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
            if nqp::can($package.HOW, $meta_meth) {
                $*W.pkg_add_method($/, $package, $meta_meth, $name, $code);
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
            if nqp::existskey($package.WHO, $name) {
                $*W.throw($/, ['X', 'Redeclaration'],
                    symbol  => $name,
                    what    => 'method',
                    postfix => '(already defined in package ' ~ $package.HOW.name($package) ~ ')'
                );
            }
            $*W.install_package_symbol($/, $package, '&' ~ $name, $code, 'method');
        }
    }

    sub is_yada($/) {
        if $<blockoid><statementlist> && nqp::elems($<blockoid><statementlist><statement>) == 1 {
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
        my int $n := +@($BLOCK[0]);
        my int $i := -1;
        while ++$i < $n {
            my $consider := $BLOCK[0][$i];
            if nqp::istype($consider, QAST::Var) {
                my $name := $consider.name;
                if $name eq '$_' || $name eq '$/' || $name eq '$!' || $name eq '$¢' {
                    $BLOCK[0][$i] := QAST::Op.new( :op('null') );
                }
            }
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
                        QAST::WVal.new( :value($*W.find_single_symbol('Routine', :setting-only)) ),
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
            $/.typed_panic("X::Syntax::Regex::NullRegex");
        }

        # Document it
        Perl6::Pod::document($/, $*DECLARAND, $*POD_BLOCK, :leading);
        if ~$*POD_BLOCK ne '' {
            $*POD_BLOCK.set_docee($*DECLARAND);
        }

        # Return closure if not in sink context.
        make block_closure($coderef, :regex).annotate_self(
            'sink_ast', QAST::Op.new( :op('null') ))
    }

    sub regex_coderef($/, $code, $qast, $scope, $name, %sig_info, $block, $traits?, :$proto) {
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
            $*W.install_lexical_magical($block, '$/');
            $past := %*RX<P5>
                ?? $/.slang_actions('P5Regex').qbuildsub($qast, $block, code_obj => $code)
                !! $/.slang_actions('Regex').qbuildsub($qast, $block, code_obj => $code);
        }
        $past.name($name);
        $past.annotate_self('statement_id', $*STATEMENT_ID
            ).annotate_self( 'in_stmt_mod', $*IN_STMT_MOD,
            ).annotate_self(       'outer', $*W.cur_lexpad
            ).blocktype("declaration_static");

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
        my $invocant_type := $*W.find_single_symbol( # XXX Maybe Cursor below, not Mu...
            $name && $*SCOPE ne 'my' && $*SCOPE ne 'our' && $*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu');
        my $signature := $*W.create_signature_and_params($/, %sig_info, $past, 'Any',
            :method, :$invocant_type);
        methodize_block($/, $code, $past, $signature, %sig_info);
        $*W.add_phasers_handling_code($code, $past);

        # Need to put self into a register for the regex engine.
        $past[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name('self'), :scope('local'), :decl('var') ),
            QAST::Var.new( :name('self'), :scope('lexical') )));

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*W.cur_lexpad();
        find_block_calls_and_migrate($outer, $past, $qast);
        $outer[0].push($past);

        # Apply traits.
        $*W.apply_traits($traits, $code) if $traits;

        # Install in needed scopes.
        install_method($/, $name, $scope, $code, $outer) if $name ne '';

        # Bind original source to $!source
        my $Regex  := $*W.find_single_symbol('Regex', :setting-only);
        my str $source := ($*METHODTYPE ?? $*METHODTYPE ~ ' ' !! '') ~ $/;
        my $match  := $source ~~ /\s+$/;

        if $match {
            $source := nqp::substr($source, 0, $match.from());
        }
        nqp::bindattr_s($code, $Regex, '$!source', $source);

        # Return a reference to the code object
        reference_to_code_object($code, $past);
    }

    method type_declarator:sym<enum>($/) {
        # Get, or find, enumeration base type and create type object with
        # correct base type.
        my $longname   := $<longname> ?? $*W.dissect_longname($<longname>) !! 0;
        my $name       := $<longname> ?? $longname.name() !! $<variable><desigilname> || '';
        my @name_parts := $<longname> ?? $longname.type_name_parts('enum name', :decl(1)) !! [];
        my $package := $/.package;

        my $type_obj;
        my sub make_type_obj($base_type) {
            $type_obj := $*W.pkg_create_mo($/, $*W.resolve_mo($/, 'enum'), :$name, :$base_type);
            # Add roles (which will provide the enum-related methods).
            $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_single_symbol('Enumeration'));

            if istype($type_obj, $*W.find_single_symbol('Numeric')) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_single_symbol(
                    istype($type_obj, $*W.find_single_symbol('Stringy')) # handle allomorphs
                        ?? 'NumericStringyEnumeration'
                        !! 'NumericEnumeration'
                ));
            }
            elsif istype($type_obj, $*W.find_single_symbol('Stringy')) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj,
                  $*W.find_single_symbol('StringyEnumeration'));
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
        my $Pair := $*W.find_single_symbol('Pair');
        my @values;
        my $term_ast := $<term>.ast;

        # remove val call on a single item
        if nqp::istype($term_ast, QAST::Op) && $term_ast.name eq '&val' {
            $term_ast := $term_ast[0];
        }

        if nqp::istype($term_ast, QAST::Stmts) && +@($term_ast) == 1 {
            $term_ast := $term_ast[0];
        }
        $term_ast := WANTED($term_ast,'enum');
        if $*COMPILING_CORE_SETTING {  # must handle bootstrapping enums here
            if nqp::istype($term_ast, QAST::Op)
            && $term_ast.name eq '&infix:<,>' {
                wantall($term_ast, 'enum');
                for @($term_ast) {
                    my $item_ast := $_;
                    if nqp::istype($item_ast, QAST::Op)
                    && $item_ast.name eq '&val' {
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
        }
        # After we finish compiling the setting, everyone uses the same evaluator without special cases.
        unless @values {
            @values := $*W.compile_time_evaluate($<term>, $term_ast).List.FLATTENABLE_LIST;
        }

        # Now we have them, we can go about computing the value
        # for each of the keys, unless they have them supplied.
        # XXX Should not assume integers, and should use lexically
        # scoped &postfix:<++> or so.
        my $cur_value := nqp::box_i(-1, $*W.find_single_symbol('Int'));
        my @redecl;
        my $block := $*W.cur_lexpad();
        my $index := -1;
        unless nqp::elems(@values) {
            my $term := $<term>.Str;
            $term := nqp::substr($term,1,nqp::chars($term) - 2);
            $<term>.worry("No values supplied to enum (does $term need to be declared constant?)") if $term ~~ /\w/;
        }
        for @values {
            # If it's a pair, take that as the value; also find
            # key.
            my $cur_key;
            if nqp::istype($_, QAST::Node) && istype($_.returns(), $Pair) {
                $cur_key := $_[1].compile_time_value;
                $cur_value := $*W.compile_time_evaluate($<term>, $_[2]);
                if $has_base_type {
                    unless istype($cur_value, $base_type) {
                        $/.panic("Type error in enum. Got '"
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
                    if nqp::istype($_, $Pair) {
                        $base_type := $_.value.WHAT;
                    }
                    else {
                        $base_type := $*W.find_single_symbol('Int');
                    }
                    make_type_obj($base_type);
                    $has_base_type := 1;
                }

                if nqp::istype($_, QAST::Node) {
                    $cur_key := $*W.compile_time_evaluate($<term>, $_);
                    $cur_value := $cur_value.succ();
                }
                elsif nqp::istype($_, $Pair) {
                    $cur_key := $_.key;
                    $cur_value := $_.value;
                }
                else {
                    $cur_key := $_;
                    $cur_value := $cur_value.succ();
                }
            }
            unless nqp::istype($cur_key, $*W.find_single_symbol('Str')) {
                $cur_key := $cur_key.Str;
            }
            unless nqp::defined($cur_value) {
                $*W.throw($/, 'X::Comp::NYI',
                    feature => "Using a type object as a value for an enum",
                );
            }

            # Create and install value.
            my $val_obj := $*W.create_enum_value($type_obj, $cur_key, $cur_value, $index := $index + 1);
            $cur_key    := nqp::unbox_s($cur_key);
            $*W.install_package_symbol_unchecked($type_obj, $cur_key, $val_obj);
            if $block.symbol($cur_key) {
                nqp::push(@redecl, $cur_key);
                $*W.install_lexical_symbol($block, $cur_key,
                    $*W.find_single_symbol('Failure').new(
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
                $*W.install_package_symbol_unchecked($package, $cur_key, $val_obj);
            }
        }

        if +@redecl -> $amount {
            if $amount > 2 {
                @redecl[$amount - 2] := @redecl[$amount - 2] ~ ' and ' ~ nqp::pop(@redecl);
                $/.typed_worry('X::Redeclaration', symbol => nqp::join(', ', @redecl));
            }
            elsif $amount > 1 {
                $/.typed_worry('X::Redeclaration', symbol => nqp::join(' and ', @redecl));
            }
            else {
                $/.typed_worry('X::Redeclaration', symbol => @redecl[0]);
            }
        }


        # create a type object even for empty enums
        make_type_obj($*W.find_single_symbol('Int')) unless $has_base_type;

        $*W.install_package($/, @name_parts,
            ($*SCOPE || 'our'), 'enum', $package, $block, $type_obj);

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
        make QAST::Op.new(
            :op('call'), :name('&ENUM_VALUES'), $term_ast
        ).annotate_self('sink_ast', QAST::Op.new( :op('null') ))
    }

    method type_declarator:sym<subset>($/) {
        # We refine Any by default; "of" may override.
        my $refinee := $*W.find_single_symbol(~($*OFTYPE // 'Any'));

        # If we have a refinement, make sure it's thunked if needed. If none,
        # just always true.
        my $refinement := $<EXPR> ?? make_where_block($<EXPR>, $<EXPR>.ast) !! nqp::null();

        # Create the meta-object.
        my $subset;
        my $longname := $<longname> && $*W.dissect_longname($<longname>);
        my @name := $longname ?? $longname.type_name_parts('subset name', :decl(1)) !! [];
        if @name {
            my $target_package := $longname.is_declared_in_global()
                ?? $*GLOBALish
                !! $/.package;
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
        my $W := $*W;
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
                    $W.throw($/, 'X::Syntax::Variable::Twigil',
                      name       => ~$<variable>,
                      what       => 'constant',
                      twigil     => $twigil,
                      scope      => $*SCOPE,
                      additional => ' because values cannot be constant and dynamic at the same time',
                    );
                }

                # Don't handle other twigil'd case yet.
                else {
                    $W.throw($/, 'X::Comp::NYI',
                      feature => "Constants with a '$twigil' twigil");
                }
            }
            $name := ~$<variable>;
        }

        # Get constant value.
        my $Mu := $W.find_symbol: ['Mu'];
        my $type := nqp::defined($*OFTYPE) ?? $*OFTYPE.ast !! $Mu;
        if $<initializer><sym> eq '.=' {
            my $init-type := $*W.maybe-nominalize: $type;
            # my $init-type := $*W.maybe-definite-how-base: $type;
            $value_ast.unshift: QAST::WVal.new: :value($init-type);
            $value_ast.returns: $init-type;
        }
        else {
            $value_ast.returns($type);
        }

        my $con_block := $W.pop_lexpad();
        my $value;
        if $value_ast.has_compile_time_value {
            $value := $value_ast.compile_time_value;
        }
        else {
            $con_block.push($value_ast);
            $con_block.annotate('BEGINISH', 1);
            my $value_thunk := $W.create_code_obj_and_add_child($con_block, 'Block');
            $value := $W.handle-begin-time-exceptions($/, 'evaluating a constant', $value_thunk);
            $*W.add_constant_folded_result($value);
        }

        sub check-type ($expected) {
            nqp::istype($value, $expected)
            || $W.throw: $/, 'X::TypeCheck', :operation(
                "constant declaration of " ~ ($name || '<anon>')
              ), :$expected, :got($value);
        }
        sub check-type-maybe-coerce($meth, $expected) {
            unless nqp::istype($value, $expected) {
                $value := $value."$meth"();
                check-type($expected);
            }
        }
        if $sigil eq '%' {
            nqp::defined($*OFTYPE) && $W.throw: $/, 'X::ParametricConstant';
            $W.lang-rev-before('d')
              ?? check-type($W.find_symbol: ['Associative'])
              !! check-type-maybe-coerce('Map', $W.find_symbol: ['Associative'])
        }
        elsif $sigil eq '@' {
            nqp::defined($*OFTYPE) && $W.throw: $/, 'X::ParametricConstant';
            check-type-maybe-coerce('cache', $*W.find_symbol: ['Positional']);
        }
        elsif $sigil eq '&' {
            nqp::defined($*OFTYPE) && $W.throw: $/, 'X::ParametricConstant';
            check-type($W.find_symbol: ['Callable']);
        }
        elsif !($type =:= $Mu) && ! nqp::objprimspec($type) {
            check-type($type);
        }

        if $name {
            my $cur_pad := $W.cur_lexpad();
            if $cur_pad.symbol($name) {
                $W.throw($/, ['X', 'Redeclaration'], symbol => $name);
            }

            $W.install_package($/, [$name], ($*SCOPE || 'our'),
                'constant', $/.package, $cur_pad, $value);
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
        $past.unshift(QAST::WVal.new( :value($*W.find_single_symbol('Capture') ) ));
        $past.op('callmethod');
        $past.name('from-args');
        make $past;
    }

    method multisig($/) {
        make $<signature>.ast;
    }

    method fakesignature($/) {
        my $fake_pad := $*W.pop_lexpad();
        for <$/ $! $_> {
            unless $fake_pad.symbol($_) {
                $*W.install_lexical_magical($fake_pad, $_);
            }
        }
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
        # Sanity checks.
        my $quant := $<quant>;
        if $<default_value> {
            my $name := %*PARAM_INFO<variable_name> // '';
            if $quant eq '*'  || $quant eq '|'
            || $quant eq '**' || $quant eq '+' {
                $/.typed_sorry('X::Parameter::Default', how => 'slurpy',
                            parameter => $name);
            }
            if $quant eq '!' {
                $/.typed_sorry('X::Parameter::Default', how => 'required',
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
                    $*W.create_thunk($<default_value>[0], $val, $*CURTHUNK);
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
                $/.typed_sorry('X::Parameter::TypedSlurpy', kind => 'positional');
            }
            elsif %*PARAM_INFO<named_slurpy> {
                $/.typed_sorry('X::Parameter::TypedSlurpy', kind => 'named');
            }
            elsif %*PARAM_INFO<sigil> eq '&' && nqp::existskey(%*PARAM_INFO, 'subsig_returns')
                    && !(%*PARAM_INFO<subsig_returns> =:= $*W.find_single_symbol('Mu')) {
                $/.'!fresh_highexpect'();
                $*W.throw($/, 'X::Redeclaration',
                    what    => 'return type for',
                    symbol  => $<param_var>.Str,
                    postfix => "(previous return type was "
                                ~ $<type_constraint>[0].Str
                                ~ ')',
                );
            }
        }

        # Result is the parameter info hash.
        make %*PARAM_INFO;
    }

    sub check_param_default_type($/, $value) {
        if nqp::existskey(%*PARAM_INFO, 'type') {
            my $expected := %*PARAM_INFO<type>;
            if nqp::objprimspec($expected) == 0 {
                unless nqp::istype($value, $expected) {
                    # Ensure both types are composed before complaining,
                    # or we give spurious errors on stubbed things or
                    # things we're in the middle of compiling.
                    my $got_comp := try $value.HOW.is_composed($value);
                    my $exp_comp := try $expected.HOW.is_composed($expected);
                    if $got_comp && $exp_comp {
                        $<default_value>[0].typed_sorry(
                            'X::Parameter::Default::TypeCheck',
                            got => $value,
                            expected => %*PARAM_INFO<type>);
                    }
                }
            }
        }
    }

    method param_var($/) {
        if $<signature> {
            if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                $/.panic('Cannot have more than one sub-signature for a parameter');
            }
            %*PARAM_INFO<sub_signature_params> := $<signature>.ast;
            if nqp::eqat(~$/, '[', 0) {
                %*PARAM_INFO<sigil> := '@';
                %*PARAM_INFO<type> := $*W.find_single_symbol('Positional');
            }
        }
        else {
            # Set name, if there is one.
            if $<name> {
                %*PARAM_INFO<variable_name> := ~$<declname>;
                %*PARAM_INFO<desigilname> := ~$<name>;
            }
            %*PARAM_INFO<sigil> := my $sigil := ~$<sigil>;

            # Depending on sigil, use appropriate role.
            my int $need_role;
            my $role_type;
            if $sigil eq '@' {
                $role_type := $*W.find_single_symbol('Positional');
                $need_role := 1;
            }
            elsif $sigil eq '%' {
                $role_type := $*W.find_single_symbol('Associative');
                $need_role := 1;
            }
            elsif $sigil eq '&' {
                $role_type := $*W.find_single_symbol('Callable');
                $need_role := 1;
            }
            if $need_role {
                if nqp::existskey(%*PARAM_INFO, 'type') {
                    %*PARAM_INFO<type> := $*W.parameterize_type_with_args($/,
                        $role_type, [%*PARAM_INFO<type>], nqp::hash());
                }
                else {
                    %*PARAM_INFO<type> := $role_type;
                }
            }

            # Handle twigil.
            my $twigil := $<twigil> ?? ~$<twigil> !! '';
            %*PARAM_INFO<twigil> := $twigil;
            if $twigil eq '' || $twigil eq '*' {
                # Need to add the name.
                if $<name> {
                    my $name := ~$<declname>;
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
                    %*PARAM_INFO<attr_package> := $*W.find_single_symbol('$?CLASS');
                    CATCH {
                        $succ := 0;
                    }
                }
                unless $succ {
                    $/.panic('cannot use a $! parameter in a signature where no $?CLASS is available');
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
                unless $<name> {
                    $/.panic("Cannot declare $. parameter in signature without an accessor name");
                }
            }
            else {
                if $twigil eq ':' {
                    $/.typed_sorry('X::Parameter::Placeholder',
                        type      => "named",
                        parameter => ~$/,
                        right     => ':' ~ $<sigil> ~ ~$<name>,
                    );
                }
                elsif $twigil eq '^' {
                    $/.typed_sorry('X::Parameter::Placeholder',
                        type      => "positional",
                        parameter => ~$/,
                        right     => $<sigil> ~ $<name>,
                    );
                }
                else {
                    $/.typed_sorry('X::Parameter::Twigil',
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
        my $par_type := $*W.find_single_symbol('Parameter', :setting-only);
        if nqp::istype($*PRECEDING_DECL, $par_type) {
            %*PARAM_INFO<dummy> := $*PRECEDING_DECL;
        }

        if $<name><sigterm> || $<sigterm> -> $sig {
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            my $get_signature_past := QAST::Op.new(
                :op('callmethod'),
                :name('signature'),
                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'param_var')
            );
            my $fakesig := $sig<fakesignature>;
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

    method param_term($/) {
        if $<defterm> {
            my $name := $<defterm>.ast;
            %*PARAM_INFO<variable_name> := $name;
            %*PARAM_INFO<desigilname>   := $name;
            %*PARAM_INFO<sigil>         := '';
            self.declare_param($/, $name);
        }
    }

    method declare_param($/, $name) {
        my $cur_pad := $*W.cur_lexpad();
        if $cur_pad.symbol($name) {
            $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
        }
        if nqp::existskey(%*PARAM_INFO, 'type') {
            $cur_pad[0].push(QAST::Var.new( :$name, :scope('lexical'),
                :decl('var'), :returns(%*PARAM_INFO<type>) ));
            $cur_pad.symbol(%*PARAM_INFO<variable_name>, :type(%*PARAM_INFO<type>));
        } else {
            $cur_pad[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        }
        $cur_pad.symbol($name, :scope('lexical'));
    }

    method named_param($/) {
        %*PARAM_INFO<named_names> := %*PARAM_INFO<named_names> || nqp::list_s();
        if $<name>               { nqp::push_s(%*PARAM_INFO<named_names>, ~$<name>); }
        elsif $<param_var><name> { nqp::push_s(%*PARAM_INFO<named_names>, ~$<param_var><name>); }
        else                     { nqp::push_s(%*PARAM_INFO<named_names>, ''); }
    }

    method default_value($/) {
        make WANTED($<EXPR>.ast, 'default_value');
    }

    method type_constraint($/) {
        if $<typename> {
            my str $typename := ~$<typename>;
            if nqp::eqat($typename, '::', 0) && !nqp::eqat($typename, '?', 2) {
                # Set up signature so it will find the typename.
                my str $desigilname := nqp::substr($typename, 2);
                unless %*PARAM_INFO<type_captures> {
                    %*PARAM_INFO<type_captures> := nqp::list_s()
                }
                nqp::push_s(%*PARAM_INFO<type_captures>, $desigilname);

                # Install type variable in the static lexpad. Of course,
                # we'll find the real thing at runtime, but in the static
                # view it's a type variable to be reified.
                $*W.install_lexical_symbol($*W.cur_lexpad(), $desigilname,
                    $<typename>.ast);
            }
            else {
                if nqp::existskey(%*PARAM_INFO, 'type') {
                    $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                    );
                }
                dissect_type_into_parameter($/, $<typename>.ast);
            }
        }
        elsif $<value> {
            if nqp::existskey(%*PARAM_INFO, 'type') {
                $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                );
            }

            my $val := wanted(
                $<value>.ast, 'type_constraint'
            ).compile_time_value;

            if $*NEGATE_VALUE {
                my $neg-op := $*W.find_single_symbol('&prefix:<->');
                $val := $neg-op($val);
                $*W.add_object_if_no_sc($val);
            }

            %*PARAM_INFO<type> := $val.WHAT;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push($val);
        }
        else {
            $/.panic('Cannot do non-typename cases of type_constraint yet');
        }
    }

    sub dissect_type_into_parameter($/, $type) {
        my $archetypes := $type.HOW.archetypes;
        if nqp::isconcrete($type) {
            if nqp::istype($type, $*W.find_single_symbol('Bool')) {
                my $val := $type.gist;
                $/.worry(
                    "Literal values in signatures are smartmatched against and "
                    ~ "smartmatch with `$val` will always "
                    ~ ($val eq 'True' ?? 'succeed' !! 'fail')
                    ~ ". Use the `where` clause instead."
                );
            }

            # Actual a value that parses type-ish.
            %*PARAM_INFO<type> := $type.WHAT;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push($type);
        }
        elsif $archetypes.nominal || $archetypes.coercive {
            %*PARAM_INFO<type> := $type;
        }
        elsif $archetypes.definite && nqp::eqaddr($type.HOW.wrappee($type, :definite), $type) {
            dissect_type_into_parameter($/, $type.HOW.base_type($type));
        }
        elsif $type.HOW.archetypes.generic {
            %*PARAM_INFO<type> := $type;
        }
        elsif $archetypes.nominalizable {
            # XXX The actual nominalization is likely to be done by Parameter class itself.
            my $nom := $type.HOW.nominalize($type);
            %*PARAM_INFO<type> := $nom;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push($type);
        }
        else {
            $<typename>.typed_sorry('X::Parameter::BadType', :$type);
        }
        %*PARAM_INFO<type_generic>   := 1 if $archetypes.generic;
        %*PARAM_INFO<of_type>        := %*PARAM_INFO<type>;
        %*PARAM_INFO<of_type_match>  := $<typename>;
        %*PARAM_INFO<defined_only>   := 1 if $<typename><colonpairs> && $<typename><colonpairs>.ast<D>;
        %*PARAM_INFO<undefined_only> := 1 if $<typename><colonpairs> && $<typename><colonpairs>.ast<U>;
    }

    method post_constraint($/) {
        if $*CONSTRAINT_USAGE eq 'param' {
            if $<signature> {
                if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                    $/.panic('Cannot have more than one sub-signature for a parameter');
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
                $/.NYI('Signatures as constraints on variables');
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
                $/.panic("is repr(...) trait needs a parameter");
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

    method revO($/) {
        my $O := nqp::clone($*FROM);
        if    $O<assoc> eq 'right' { $O<assoc> := 'left' }
        elsif $O<assoc> eq 'left'  { $O<assoc> := 'right' }
        make $O;
    }

    method dotty:sym<.>($/) { make $<dottyop>.ast; }

    method dotty:sym<.*>($/) {
        my $past := $<dottyop>.ast;
        unless nqp::istype($past, QAST::Op) && $past.op() eq 'callmethod' {
            $/.panic("Cannot use " ~ $<sym>.Str ~ " on a non-identifier method call");
        }
        if $<sym> eq '.^' {
            $past.op('p6callmethodhow');
        }
        else {
            $past.unshift($*W.add_string_constant($past.name))
                if $past.name ne '';
            $past.name('dispatch' ~ $*W.canonicalize_pair('', ~$<sym>));
            $past.nosink(1);
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
        my $package := $/.package;
        if $<methodop><longname> {
            my @parts   := $*W.dissect_longname($<methodop><longname>).components();
            my $name    := @parts.pop;
            if @parts {
                my $methpkg := $*W.find_symbol(@parts);
                unless nqp::can($methpkg.HOW, 'is_trusted') && $methpkg.HOW.is_trusted($methpkg, $package) {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Permission'],
                        :method(         $name),
                        :source-package( $methpkg.HOW.name($methpkg)),
                        :calling-package( $package.HOW.name($package)),
                    );
                }
                $past[1].returns($methpkg);
            }
            else {
                unless nqp::can($package.HOW, 'find_private_method') {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Unqualified'],
                        :method($name),
                    );
                }
                if $package.HOW.archetypes.parametric {
                    $past.unshift(typevar_or_lexical_lookup('::?CLASS'));
                }
                else {
                    $past.unshift(QAST::WVal.new( :value($package) ));
                    $past[0].returns($package);
                }
                $past.unshift($*W.add_string_constant($name));
            }
            $past.name('dispatch:<!>');
        }
        elsif $<methodop><quote> {
            my $name := $past.shift;
            if $package.HOW.archetypes.parametric {
                $past.unshift(typevar_or_lexical_lookup('::?CLASS'));
            }
            else {
                $past.unshift(QAST::WVal.new( :value($package) ));
            }
            $past.unshift($name);
            $past.name('dispatch:<!>');
        }
        else {
            $/.panic("Cannot use this form of method call with a private method");
        }
        make $past;
    }

    # We can generate typevar scope when we're in a method and the enclosing
    # role declares the symbol we're looking for.
    sub typevar_or_lexical_lookup($name) {
        if $*HAS_SELF {
            my $outer := $*W.cur_lexpad().ann('outer');
            if $outer && $outer.symbol($name) {
                return QAST::Var.new( :$name, :scope('typevar') );
            }
        }
        return QAST::Var.new( :$name, :scope('lexical') );
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
                my int $found_wval := 0;
                try {
                    my $sym := $*W.find_symbol(@parts);
                    unless $sym.HOW.archetypes.generic {
                        $past.unshift(QAST::WVal.new( :value($sym) ));
                        $found_wval := 1;
                    }
                }
                unless $found_wval {
                    $past.unshift($*W.symbol_lookup(@parts, $/));
                }
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
                $*W.cur_lexpad().no_inline(1) if $name eq 'EVAL';
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
        if nqp::elems($past) > 0 {
           $*W.throw($/, ['X', 'Syntax', 'Argument', 'MOPMacro'], macro => $name);
        }
    }

    method term:sym<::?IDENT>($/) {
        make instantiated_type([~$/], $/);
    }

    method term:sym<self>($/) {
        make QAST::Var.new( :name('self'), :scope('lexical'), :returns($/.package), :node($/) );
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
        unless nqp::elems($past.list()) {
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
            if istype($routine, $*W.find_single_symbol('Macro')) {
                return $routine;
            }
        }
        0
    }

    sub expand_macro($macro, $name, $/, &collect_argument_asts) {
        my @argument_asts := &collect_argument_asts();
        my $macro_ast := $*W.ex-handle($/, { $macro(|@argument_asts) });
        my $nil_class := $*W.find_single_symbol('Nil');
        if istype($macro_ast, $nil_class) {
            return QAST::WVal.new( :value($nil_class) );
        }
        my $ast_class := $*W.find_single_symbol('AST');
        unless istype($macro_ast, $ast_class) {
            $*W.throw($/, 'X::TypeCheck::Splice',
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
            return QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
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
            my $past := handle_special_call_names($/,$<args>.ast, ~$<identifier>);
            $past.name('&' ~ $<identifier>);
            $past.node($/);
            make $past;
        }
    }

    sub add_macro_arguments($expr, @argument_asts, $code_string) {
        my $ast_class := $*W.find_single_symbol('AST');

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
                QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash')) )
            )
        );
        $past.push($*W.add_string_constant($sigil)) if $sigil;
        for @components {
            if nqp::istype($_, QAST::Node) {
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
                $/.panic("Combination of indirect name lookup and call not supported");
            }
            elsif $<arglist> {
                $/.panic("Combination of indirect name lookup and type arguments not supported");
            }
            elsif $<accept> || $<accept_any> {
                $/.panic("Combination of indirect name lookup and coercion type construction not supported");
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
            my $final := @name[nqp::elems(@name) - 1];
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
                $past := handle_special_call_names($/,$<args>.ast, ~$<longname>);
                if nqp::elems(@name) == 1 {
                    $past.name(@name[0]);
                    $/.add_mystery(@name[0], $<args>.from, 'termish');
                    if nqp::elems($past.list) == 1 && %commatrap{@name[0]} {
                        my $prelen := $<longname>.from;
                        $prelen := 100 if $prelen > 100;
                        my $pre := nqp::substr($/.orig, $<longname>.from - $prelen, $prelen);
                        my $post := nqp::substr($/.orig, $<args>.to, 100);
                        if nqp::index($pre, "==>") < 0 && nqp::index($post, "<==") < 0 && $<args>.Str ~~ /^\s*['{'|'->'|'<->']/ {
                            $/.missing("comma after block argument to " ~ nqp::substr(@name[0],1));
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
            elsif nqp::elems(@name) == 0 {
                $past := QAST::Op.new(
                    :op<callmethod>, :name<new>,
                    QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash')) )
                );
            }
            elsif $*W.is_pseudo_package(@name[0]) {
                $past := $*W.symbol_lookup(@name, $/);
            }
            elsif nqp::elems(@name) == 1 && $*W.is_name(@name)
                    && !$*W.symbol_has_compile_time_value(@name) {
                # it's a sigilless param or variable
                $past := make_variable_from_parts($/, @name, '', '', @name[0]);
            }
            elsif nqp::elems(@name) && @name[0] eq 'EXPORT' {
                my int $i := 1;
                my int $m := nqp::elems(@name);
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

            if $<colonpairs> && $<colonpairs>.ast<D> {
                unless nqp::istype($past, QAST::WVal) {
                    $/.panic("Type too complex to form a definite type");
                }
                my $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $past.value, 1); # XXX add constants
                $past    := QAST::WVal.new( :value($type) );
            }
            elsif $<colonpairs> && $<colonpairs>.ast<U> {
                unless nqp::istype($past, QAST::WVal) {
                    $/.panic("Type too complex to form a definite type");
                }
                my $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $past.value, 0);
                $past    := QAST::WVal.new( :value($type) );
            }

            # If needed, try to form a coercion type.
            unless nqp::isnull(my $accept := $*W.can_has_coercerz: $/) {
                my $value;
                if nqp::istype($past, QAST::WVal) {
                    $value := $past.value;
                }
                elsif $past.has_compile_time_value {
                    $value := $past.compile_time_value;
                }
                else {
                    $/.panic("Target type too complex to form a coercion type");
                }

                my $type := $*W.create_coercion_type($/, $value, $accept);
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
        unless $/.pragma('MONKEY-GUTS') || $/.pragma('nqp') || $*COMPILING_CORE_SETTING {
            $/.typed_panic('X::NQP::NotFound', op => $op);
        }

        my $past := QAST::Op.new( :$op, |@args );
        if $op eq 'handle' || $op eq 'handlepayload' {
            my int $i := 1;
            my int $n := nqp::elems($past.list);
            while $i < $n {
                $past[$i] := compile_time_value_str($past[$i], 'want specification', $/);
                $i := $i + 2;
            }
        }
        $past.node($/);
        nqp::getcomp('QAST').operations.attach_result_type('Raku', $past);
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
                            QAST::WVal.new( :value($*W.find_single_symbol('Routine', :setting-only)) ),
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
        if nqp::elems($<arglist>) == 1 {
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

    sub migrate_colonpairs($/, @qast) {
        my $Pair := $*W.find_single_symbol('Pair');
        my $ridx1 := 0;
        my $sidx1 := 1;
        while $ridx1 < +@qast {
            my $ridx2 := 3;
            my $q := @qast[$ridx1];
            if nqp::istype($q, QAST::Op) && $q.op eq 'callmethod' && $q.name eq 'new' && nqp::istype($q[0], QAST::Var) && $q[0].name eq 'Pair' {
                while $ridx2 < +@(@qast[$ridx1]) {
                    my $clone := @(@qast[$ridx1])[$ridx2].shallow_clone;
                    nqp::splice(
                        @qast,
                        nqp::list(wanted(QAST::Op.new(
                            :op('callmethod'), :name('new'), :returns($Pair), :node($clone.node // $/),
                            QAST::Var.new( :name('Pair'), :scope('lexical'), :node($clone.node // $/)),
                            $*W.add_string_constant($clone.named),
                            $clone
                        ), 'circumfix()/pair')),
                        $sidx1,
                        0);
                    $clone.named(NQPMu);
                    $sidx1++;
                    $ridx2++;
                }
                if $ridx2 > 3 {
                    nqp::splice(@qast[$ridx1], nqp::list, 3, +@(@qast[$ridx1]) - 3);
                    $ridx1 := $sidx1;
                    $sidx1++;
                }
                else {
                    $ridx1++;
                    $sidx1++;
                }
            }
            else {
                $ridx1++;
                $sidx1++;
            }
        }
    }

    method arglist($/) {
        my $Pair := $*W.find_single_symbol('Pair');
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
            if $*FAKE_INFIX_FOUND {
                migrate_colonpairs($/, @args);
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
			unless $_[2].has_compile_time_value {
                            $past.push(QAST::Stmts.new(
                                $_[2], QAST::Op.new(:op('list')), :flat(1)));
                        }
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

    sub handle-list-semis($/, $past) {
        if !nqp::elems($past.list) {
            $past := QAST::Stmts.new( :node($/) );
            $past.push(QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/)));
        }
        # Look for any chained adverb pairs and relocate them.
        # Try to reuse existing QAST where possible.
        elsif $*FAKE_INFIX_FOUND {
            my @EXPR;
            my $semis := $<semilist><statement>;
            my $numsemis := nqp::elems($semis);

            my int $i := -1;
            while ++$i < $numsemis {
                my $EXPR := $semis[$i]<EXPR>;
                if nqp::defined($EXPR) {
                    @EXPR.push($EXPR);
                }
            }
            $numsemis := nqp::elems(@EXPR);

            if $numsemis > 1 {
                $past := QAST::Stmts.new( :node($/) );
                $past.push(QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/)));
            }

            my int $semi := -1;
            while ++$semi < $numsemis {
                my $EXPR := @EXPR[$semi];
                if $EXPR<colonpair> { # might start with a colonpair
                    my @fan := nqp::list($EXPR.ast);
                    migrate_colonpairs($/, @fan);
                    if (nqp::elems(@fan) > 1) {
                        my $comma := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/));
                        for @fan { $comma.push($_) }
                        if ($numsemis == 1) {
                            $past := QAST::Stmts.new( :node($/) );
                            $past.push($comma);
                        }
                        else {
                            $past[0].push($comma);
                        }
                    }
                    elsif ($numsemis > 1) {
                        $past[0].push($EXPR.ast);
                    }
                }
                else {
                    migrate_colonpairs($/, $EXPR.ast.list);
                    if ($numsemis > 1) {
                        $past[0].push($EXPR.ast);
                    }
                }
            }
            $past := wanted($past, 'circumfix()/pair');
        }
        $past
    }

    method circumfix:sym<( )>($/) {
        make handle-list-semis($/, $<semilist>.ast)
    }

    method circumfix:sym<[ ]>($/) {
        make QAST::Op.new(
          :op('call'),
          :name('&circumfix:<[ ]>'),
          handle-list-semis($/, $<semilist>.ast),
          :node($/)
        )
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
        my $Pair := $*W.find_single_symbol('Pair');
        my int $is_hash   := 0;
        my int $has_stuff := 1;
        my $stmts := nqp::elems($<pblock><blockoid><statementlist><statement>);
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
            $elem := $elem[0] if nqp::istype($elem, QAST::Want);
            $elem := $elem[0] if nqp::istype($elem, QAST::Op) && $elem.op eq 'p6fatalize';
            if nqp::istype($elem, QAST::Op) && $elem.name eq '&infix:<,>' {
                # block contains a list, so test the first element
                $elem := $elem[0];
            }
            $elem := $elem[0] if nqp::istype($elem, QAST::Op) && $elem.op eq 'p6fatalize';
            my $subelem := $elem;
            while nqp::istype($subelem, QAST::Op)
            && ($subelem.op eq 'p6capturelex' || $subelem.op eq 'if'
              || $subelem.op eq 'unless') {
                $subelem := $subelem[0];
                if nqp::istype($subelem, QAST::Op)
                && $subelem.op eq 'callmethod' && $subelem.name eq 'clone' {
                    $subelem := $subelem[0];
                    if nqp::istype($subelem, QAST::WVal)
                    && nqp::istype($subelem.value, $*W.find_single_symbol('WhateverCode', :setting-only)) {
                        $/.malformed("double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *");
                    }
                }
            }
            if nqp::istype($elem, QAST::Op)
                    && (istype($elem.returns, $Pair) || $elem.name eq '&infix:«=>»') {
                # first item is a pair
                $is_hash := 1;
            }
            elsif nqp::istype($elem, QAST::Op) && $elem.op eq 'call'
                && nqp::istype($elem[0], QAST::Op) && $elem[0].name eq '&METAOP_REVERSE' &&
                    nqp::istype($elem[0][0], QAST::Var) && $elem[0][0].name eq '&infix:«=>»' {
                # first item is a pair constructed with R=>
                $is_hash := 1;
            }
            elsif nqp::istype($elem, QAST::Var) && nqp::eqat($elem.name, '%', 0) {
                # first item is a hash (%foo or %!foo)
                $is_hash := 1;
            }
            elsif nqp::istype($elem, QAST::Op) && $elem.name eq '&DYNAMIC' &&
                    nqp::istype($elem[0], QAST::Want) && $elem[0][1] eq 'Ss' &&
                    nqp::istype($elem[0][2], QAST::SVal)
                    && nqp::eqat($elem[0][2].value, '%', 0) {
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
            my $orig_block := $past.ann('past_block');
            migrate_blocks($orig_block, $*W.cur_lexpad());
            my @children := @($orig_block[1]);
            $past := QAST::Op.new(
                :op('call'),
                :name(
                    $/.from && nqp::eqat($/.orig, ':', $/.from - 1) ?? '&circumfix:<:{ }>' !! '&circumfix:<{ }>'
                ),
                :node($/)
            );
            if $has_stuff {
                my $c := 0; # follow $p in the match tree
                for @children -> $p {
                    my $pp := $p;
                    if nqp::istype($p, QAST::Stmt) {
                        # Mustn't use QAST::Stmt here, as it will cause register
                        # re-use within a statement, which is a problem.
                        $p := QAST::Stmts.new( |$p.list );
                    }
                    # Look for any chained adverb pairs and relocate them.
                    # Try to reuse existing QAST where possible.
                    if $*FAKE_INFIX_FOUND {
                        $pp := nqp::istype($p[0], QAST::Want) ?? $pp[0][0] !! $pp[0];
                        if $<pblock><blockoid><statementlist><statement>[$c]<EXPR><colonpair> {
                            my @fan := nqp::list($pp);
                            migrate_colonpairs($/, @fan);
                            if nqp::elems(@fan) > 1 {
                                $pp := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/));
                                for @fan { $pp.push($_) }
                            }
                        }
                        else {
                            migrate_colonpairs($/, $pp.list);
                        }
                        if nqp::istype($p, QAST::Want) {
                            $p[0][0] := $pp;
                        }
                        else {
                            $p[0] := $pp;
                        }
                        $pp := $p;
                    }
                    $past.push($pp);
                    $c++;
                }
            }
            # Clear out the now-unused QAST::Block, so we don't leave it behind in
            # the AST.
            $orig_block.shift() while @($orig_block);
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
    our sub migrate_blocks($from, $to, $predicate?) {
        my @decls := @($from[0]);
        my int $n := nqp::elems(@decls);
        my int $i := -1;
        while ++$i < $n {
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
        }
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
    my %worrisome := nqp::hash(
        '..', 1,
        '^..', 1,
        '..^', 1,
        '^..^', 1,
        'R..', 1,
        'R^..', 1,
        'R..^', 1,
        'R^..^', 1
    );
    method EXPR($/, $KEY?) {
        unless $KEY { return 0; }
        my $past := $/.ast // $<OPER>.ast;
        my $key := nqp::lc($KEY // 'infix');
        $key := 'infix' if $key eq 'list';
        my $sym := ~$/{$key}<sym>;
        my $thunky := $/{$key} ?? $/{$key}<O>.made<thunky> !! 0;
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
            my $test_type;
            if $past.name eq '&METAOP_TEST_ASSIGN:<||>' { $test_type := 'unless' }
            elsif $past.name eq '&METAOP_TEST_ASSIGN:<//>' { $test_type := 'defor' }
            elsif $past.name eq '&METAOP_TEST_ASSIGN:<&&>' { $test_type := 'if' }
            if $test_type {
                my $sym := QAST::Node.unique('meta_op_test');
                $past := QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($sym), :scope('local'), :decl('var') ),
                        WANTED($/[0].ast, 'EXPR/META')
                    ),
                    QAST::Op.new(
                        :op($test_type),
                        QAST::Var.new( :name($sym), :scope('local') ),
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($sym), :scope('local') ),
                            WANTED($/[1].ast, 'EXPR/META')
                        )
                    )).annotate_self: 'METAOP_opt_result', 1;
            }
            else {
                $past.push(WANTED($/[0].ast, 'EXPR/META'));
                $past.push(block_closure(make_thunk_ref(WANTED($/[1].ast, 'EXPR/META'), $/)));
            }
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
        elsif !$past && $thunky && (
               $sym eq 'xx'     || $sym eq 'andthen'
            || $sym eq 'orelse' || $sym eq 'notandthen'
        ) {
            $past := thunkity_thunk($/, $thunky,
                QAST::Op.new( :op('call'), :name("&infix" ~ $*W.canonicalize_pair('', $sym))),
                $/.list);

            unless $sym eq 'xx' {
                # andthen/notandthen/orelse ops can be chained and since all but last
                # args affect the outcome of the chain, all but last args are wanted
                my $up-to := @($past) - 2;
                my $i     := 1; # first arg gets marked as wanted elsewhere; skip it
                wanted($past[$i++], "$sym/args") while $i <= $up-to;
            }

            make $past;
            return 1;
        }
        unless $past {
            if $<OPER><O>.made<pasttype> {
                $past := QAST::Op.new( :node($/), :op( ~$<OPER><O>.made<pasttype> ) );
            }
            else {
                $past := QAST::Op.new( :node($/), :op('call') );
            }
            if $<OPER><sym> {
                my $name;
                if nqp::istype($past, QAST::Op) && !$past.name {
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

                my $push-target := $target;
                $/.typed_panic: 'X::Syntax::Adverb', what =>
                  nqp::can($target, 'name') && $target.name
                    ?? $target.name !! ~$/[0]
                unless nqp::istype($target, QAST::Op)
                && ($target.op eq 'call' || $target.op eq 'callmethod')
                || $target.has_ann('fake_infix_adverb_target')
                && ($push-target := $target.ann: 'fake_infix_adverb_target');

                my $cpast := $<colonpair>.ast;
                $/.typed_panic: 'X::Syntax::Adverb', what => ~$/
                  unless try $cpast[1];
                $cpast[2].named(compile_time_value_str($cpast[1], 'LHS of pair', $/));
                $push-target.push(WANTED($cpast[2],'EXPR/POSTFIX'));

                if nqp::istype($past, QAST::Op) && $past.op eq 'hllize' {
                    $past[0] := WANTED($target,'EXPR/POSTFIX');
                }
                else {
                    $past := WANTED($target,'EXPR/POSTFIX');
                }

                make $past;
                return 1;
            }

            # To allow currying, we must note that postfix:<ⁿ> is really infix:<**>
            # in disguise, with arity 2.
            $key := 'infix'
                if nqp::istype($past, QAST::Op) && $past.op eq 'call' && $past.name eq '&postfix:<ⁿ>';

            # Method calls may be to a foreign language, and thus return
            # values may need type mapping into Raku land.
            $past.unshift(WANTED($/[0].ast,'EXPR/POSTFIX'));
            if nqp::istype($past, QAST::Op) && $past.op eq 'callmethod' {
                unless $<OPER> && ($<OPER><sym> eq '.=' || $<OPER><sym> eq '.+' || $<OPER><sym> eq '.?') {
                    $return_map := 1
                }

                if $past.name eq 'dispatch:<var>' {
                    # Unpack the method call into nameless-calling the argument:
                    $past.op: 'call';
                    $past.name: '';
                    $past.annotate: 'curriable-call-offset', 1;
                    my $invocant := $past[0];
                    $past[0] := $past[1];
                    $past[1] := $invocant;
                }
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
        elsif $thunky {
            for $/.list { if $_.ast { WANTED($_.ast, "EXPR/thunky") if nqp::eqat($thunky,'.',$arity); $past.push($_.ast); ++$arity; } }
        }
        else {
            for $/.list { if $_.ast { $past.push(WANTED($_.ast,'EXPR/list')); ++$arity; } }
            if $key eq 'infix' && $<OPER><O>.made<assoc> eq 'chain' {
                $past.op('chain');
            }
        }
        if $past.op eq 'xor' {
            $past.push(QAST::WVal.new( :named<false>, :value($*W.find_single_symbol('Nil')) ));
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
        if $key eq 'infix' && nqp::existskey(%worrisome, ~$/<OPER>) {
            if ~$/[0]<prefix> eq '|' {
                $/[0].typed_worry('X::Worry::Precedence::Range', action => "apply a Slip flattener to", precursor => 1);
            }
            elsif ~$/[0]<prefix> eq '~' {
                $/[0].typed_worry('X::Worry::Precedence::Range', action => "stringify", precursor => 1);
            }
        }

        make $past;
    }

    sub make_feed($/) {
        # Assemble into list of AST of each step in the pipeline.
        my @stages;
        if $/<infix><sym> eq '==>' {
            for @($/) { @stages.push($_); }
        }
        elsif $/<infix><sym> eq '<==' {
            for @($/) { @stages.unshift($_); }
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
        $result    := WANTED($result.ast, $/<infix><sym>);
        for @stages {
            my $stage := WANTED($_.ast, $/<infix><sym>);
            # Wrap current result in a block, so it's thunked and can be
            # called at the right point.
            $result := QAST::Block.new( $result );

            # Check what we have. XXX Real first step should be looking
            # for @(*) since if we find that it overrides all other things.
            # But that's todo...soon. :-)
            if nqp::istype($stage, QAST::Op) && $stage.op eq 'call' {
                # It's a call. Stick a call to the current supplier in
                # as its last argument.
                $stage.push(QAST::Op.new( :op('call'), $result ));
            }
            elsif nqp::istype($stage, QAST::Var) {
                # It's a variable. We need code that gets the results, pushes
                # them onto the variable and then returns them (since this
                # could well be a tap.
                my $tmp := QAST::Node.unique('feed_tmp');
                $stage := QAST::Stmts.new(
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
                        $stage,
                        QAST::Var.new( :scope('local'), :name($tmp) )
                    ),
                    QAST::Var.new( :scope('local'), :name($tmp) )
                );
                $stage := QAST::Op.new( :op('locallifetime'), $stage, $tmp );
            }
            else {
                my str $error := "Only routine calls or variables that can '.push' may appear on either side of feed operators.";
                if nqp::istype($stage, QAST::Op) && $stage.op eq 'ifnull'
                && nqp::istype($stage[0], QAST::Var)
                && nqp::eqat($stage[0].name, '&', 0) {
                    $error := "A feed may not sink values into a code object. Did you mean a call like '"
                            ~ nqp::substr($stage[0].name, 1) ~ "()' instead?";
                }
                $_.PRECURSOR.panic($error);
            }
            $result := $stage;
        }

        # WANTED($result,'make_feed');
        $result
    }

    sub check_smartmatch($/,$pat) {
        if nqp::can($pat,'ann') && $pat.ann('is_S') {
            $/.PRECURSOR.worry('Smartmatch with S/// is not useful. You can use given instead: S/// given $foo');
        }

        if nqp::istype($pat, QAST::WVal)
        && istype($pat.returns, $*W.find_single_symbol('Bool'))
        && nqp::isconcrete($pat.value) {
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
        my $lhs := wanted($/[0].ast,'smartmatch/lhs');
        my $rhs := wanted($/[1].ast,'smartmatch/rhs');
        check_smartmatch($/[1],$rhs);
        # autoprime only on Whatever with explicit *
        return 0 if nqp::istype($lhs, QAST::WVal)
            && istype($lhs.returns, $*W.find_single_symbol('Whatever'))
            && nqp::isconcrete($lhs.value);
        return 0 if nqp::istype($rhs, QAST::WVal)
            && istype($rhs.returns, $*W.find_single_symbol('Whatever'))
            && nqp::isconcrete($rhs.value);

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
        if nqp::istype($target, QAST::Var) {
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
                    my $package := $/.package;
                    $type := $package.HOW.get_attribute_for_usage(
                        $package, $target.name
                    ).type;
                }
                unless $type =:= $*W.find_single_symbol('Mu') {
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
                    unless $type =:= $*W.find_single_symbol('Mu') {
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
        elsif nqp::istype($target, QAST::Op) && $target.op eq 'hllize' &&
                nqp::istype($target[0], QAST::Op) && $target[0].op eq 'call' &&
                ($target[0].name eq '&postcircumfix:<[ ]>' ||
                 $target[0].name eq '&postcircumfix:<{ }>' ||
                 $target[0].name eq '&postcircumfix:<[; ]>') {
            $source.named('BIND');
            $target[0].push($source);
            $target.nosink(1);
            make $target;
        }
        elsif nqp::istype($target, QAST::Op) && $target.op eq 'call' &&
              ($target.name eq '&postcircumfix:<[ ]>' ||
               $target.name eq '&postcircumfix:<{ }>' ||
               $target.name eq '&postcircumfix:<[; ]>') {
            $source.named('BIND');
            $target.push($source);
            $target.nosink(1);
            make $target;
        }
        elsif nqp::istype($target, QAST::WVal)
        && nqp::istype($target.value,
          $*W.find_single_symbol('Signature', :setting-only)) {
            make QAST::Op.new(
                :op('p6bindcaptosig'),
                $target,
                QAST::Op.new(
                    :op('callmethod'), :name('Capture'),
                    $source
                ));
        }
        elsif nqp::istype($target, QAST::Op) && $target.op eq 'call'
        && $target.name eq '&DYNAMIC' && $target[0][1] eq 'Ss' {
            my $complain := QAST::Op.new(
                :op('die_s'),
                QAST::SVal.new( :value('Contextual ' ~ ~$/ ~ ' not found') )
            );
            my $contextual := QAST::VarWithFallback.new(
                :name($target[0][2].value), :scope('contextual'), :fallback($complain) );
            my $dynbind := QAST::Op.new( :op('bind'), $contextual, $source);
            $dynbind.nosink(1);
            make $dynbind;
        }
        # XXX Several more cases to do...
        else {
            $*W.throw($/, ['X', 'Bind']);
        }
    }

    # The _i64 and _u64 are only used on backends that emulate int64/uint64
    my @native_assign_ops := ['', 'assign_i', 'assign_n', 'assign_s', 'assign_i64', 'assign_u64'];
    sub assign_op($/, $lhs_ast, $rhs_ast, :$initialize) {
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
                        $/.worry("Useless use of hash composer on right side of hash assignment; did you mean := instead?");
                    }
                }
            }
        }

        # get the sigil out of the my %h is Set = case
        elsif nqp::istype($lhs_ast,QAST::Op) && $lhs_ast.op eq 'bind'
          && nqp::istype($lhs_ast[0], QAST::Var) {
            $var_sigil := nqp::substr($lhs_ast[0].name, 0, 1);
        }

        if nqp::istype($lhs_ast, QAST::Var)
                && nqp::objprimspec($lhs_ast.returns) -> $spec {
            # Native assignment is only possible to a reference; complain now
            # rather than at runtime since we'll inevitably fail.
            my $scope := $lhs_ast.scope;
            if $scope ne 'lexicalref' && $scope ne 'attributeref' {
                $lhs_ast.node.typed_sorry('X::Assignment::RO::Comp',
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

            # let STORE know if this is the first time
            if $initialize {
                $past.push(QAST::WVal.new(
                  :named('INITIALIZE'),
                  :value($*W.find_symbol(['Bool', 'True']))
                ));
            }
            $past.nosink(1);
        }
        elsif $var_sigil eq '$' {
            # If it's a $ scalar, we can assume it's some kind of scalar
            # container with a container spec, so can go directly for a
            # Scalar assign op (via. a level of indirection so that any
            # platform that wants to optimize this somewhat can).
            $past := QAST::Op.new( :op('p6assign'), $lhs_ast, $rhs_ast );
        }
        elsif nqp::istype($lhs_ast, QAST::Op) && $lhs_ast.op eq 'call' &&
              ($lhs_ast.name eq '&postcircumfix:<[ ]>' ||
               $lhs_ast.name eq '&postcircumfix:<{ }>' ||
               $lhs_ast.name eq '&postcircumfix:<[; ]>') &&
                +@($lhs_ast) == 2 { # no adverbs
            $lhs_ast.push($rhs_ast);
            $past := $lhs_ast;
            $past.nosink(1);
        }
        elsif nqp::istype($lhs_ast, QAST::Op) && $lhs_ast.op eq 'hllize' &&
                nqp::istype($lhs_ast[0],QAST::Op) && $lhs_ast[0].op eq 'call' &&
                ($lhs_ast[0].name eq '&postcircumfix:<[ ]>' || $lhs_ast[0].name eq '&postcircumfix:<{ }>') &&
                +@($lhs_ast[0]) == 2 { # no adverbs
            $lhs_ast[0].push($rhs_ast);
            $past := $lhs_ast;
            $past.nosink(1);
        }
        else {
            $past := QAST::Op.new( :node($/), :op('p6store'),
                $lhs_ast, $rhs_ast);
        }
        $past
    }

    sub mixin_op($/, $sym) {
        my $rhs  := $/[1].ast;
        my $lhs  := $/[0].ast;
        $lhs := WANTED($lhs,'mixin_op') if $sym eq 'does';
        my $past := QAST::Op.new(
            :op('call'), :name('&infix' ~ $*W.canonicalize_pair('', $sym)),
            $lhs);
        if nqp::istype($rhs, QAST::Op) && $rhs.op eq 'call' {
            if $rhs.name && +@($rhs) == 1 {
                try {
                    $past.push(QAST::WVal.new( :value($*W.find_single_symbol(nqp::substr($rhs.name, 1))) ));
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
        elsif nqp::istype($rhs, QAST::Stmts) && +@($rhs) == 1 &&
                nqp::istype($rhs[0], QAST::Op) && $rhs[0].name eq '&infix:<,>' {
            for @($rhs[0]) {
                $past.push($_);
            }
        }
        else {
            $past.push($rhs);
        }
        $past
    }

    sub mark_blocks_as_andnotelse_first_arg($ast) {
        if $ast && nqp::can($ast, 'ann') && $ast.ann('past_block') {
            $ast.ann('past_block').annotate: 'in_stmt_mod_andnotelse', 1;
            $ast.ann('past_block').annotate: 'in_stmt_mod', 0;
        }
        elsif nqp::istype($ast, QAST::Op)
        || nqp::istype($ast, QAST::Stmt)
        || nqp::istype($ast, QAST::Stmts)
        || nqp::istype($ast, QAST::Want) {
            mark_blocks_as_andnotelse_first_arg($_) for @($ast)
        }
    }

    sub thunkity_thunk($/,$thunky,$past,@clause) {
        my int $i := 0;
        my int $e := nqp::elems(@clause);
        my int $te := nqp::chars($thunky);
        my $type := nqp::substr($thunky,0,1);
        my $andnotelse_thunk := nqp::istype($past, QAST::Op)
          && $past.op eq 'call'
          && ( $past.name eq '&infix:<andthen>'
            || $past.name eq '&infix:<notandthen>'
            || $past.name eq '&infix:<orelse>');

        while $i < $e {
            my $ast := @clause[$i];
            $ast := $ast.ast if nqp::can($ast,'ast');  # reduce already passes ast...
            mark_blocks_as_andnotelse_first_arg($ast)
                if $andnotelse_thunk && $i == 0;

            if $type eq 'T' || $type eq 'B' || $type eq 'A' {
                my $argast := $ast;
                $argast := $argast[0] if nqp::istype($argast,QAST::Stmts);
                if nqp::istype($argast,QAST::Op) && $argast.op eq 'call' && $argast.name eq '&infix:<,>' {
#                    note("thunky $type bingo:\n" ~ $argast.dump);
                    my int $ae := nqp::elems($argast);
                    my int $a := -1;
                    while ++$a < $ae {
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
                    }
                }
            }
            if $type eq '.' || $type eq 'T' || $type eq 'B' || $type eq 'A' || nqp::istype($ast,QAST::SpecialArg) {
                $ast := WANTED($ast, 'thunkity') if $type eq '.';
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
                    $ast := block_closure(make_topic_block_ref(
                      @clause[$i], $ast, :$andnotelse_thunk,
                      migrate_stmt_id => $*STATEMENT_ID,
                    ));
                }
                $past.push($ast);
            }
            elsif $type eq 'a' {  # always thunk  (XXX not ever needed?)
                $past.push(block_closure(make_thunk_ref($ast, $/)));
            }
            else {
                $/.panic("Unknown thunk spec '$type'");
            }
            $type := nqp::substr($thunky,$i,1) if ++$i < $te;  # repeat last thunk spec as necessary
        }
        $past;
    }

    sub flipflop($lhs, $rhs, $min_excl, $max_excl, $one_only) {
        # Need various constants.
        my $zero  := $*W.add_numeric_constant(NQPMu, 'Int', 0);
        my $one   := $*W.add_numeric_constant(NQPMu, 'Int', 1);
        my $nil   := QAST::WVal.new( :value($*W.find_single_symbol('Nil')) );
        my $false := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'False'])) );
        my $true  := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'True'])) );
        my $topic := WANTED(QAST::Var.new( :name('$_'), :scope<lexical> ),'ff');

        # Need a state variable to track the state.
        my %cont;
        my $id    := $lhs.unique('FLIPFLOP_STATE_');
        my $state := '!' ~ $id;
        %cont{'bind_constraint'} := $*W.find_single_symbol('Mu');
        %cont{'container_type'}  := $*W.find_single_symbol('Scalar');
        %cont{'container_base'}  := %cont{'container_type'};
        %cont{'default_value'}   := $zero.compile_time_value;
        %cont{'scalar_value'}    := $zero.compile_time_value;
        $*W.install_lexical_container($*W.cur_lexpad(), $state, %cont,
            $*W.create_container_descriptor(%cont{'bind_constraint'}, $state),
            :scope('state'));

        # Twiddle to make special-case RHS * work.
        if istype($rhs.returns, $*W.find_single_symbol('Whatever')) {
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
           || $<assoc> eq 'chain'
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
            my $purity := 0;
            if nqp::istype($basepast, QAST::Var) {
                my $subfun := try $*W.find_single_symbol($basepast.name);
                if $subfun {
                    $purity := 1 if nqp::can($subfun, 'is-pure');
                }
                else {
                    $purity := 1;   # assume will be defined pure
                }
            }
            else {
                $purity := 1 if $basepast.ann('is_pure');
            }
            my $t        := $basepast.ann('thunky') || $base<OPER><O>.made<thunky>;
            my $helper   := '';
            my $astop    := 'call';

            if $metasym eq '!' {
                $helper := '&METAOP_NEGATE';
                if $base<OPER><O>.made<pasttype> eq 'chain' { $astop := 'chain' }
            }
            if    $metasym eq 'R' { $helper := '&METAOP_REVERSE'; $t := nqp::flip($t) if $t; }
            elsif $metasym eq 'X' { $helper := '&METAOP_CROSS'; $t := nqp::uc($t); }  # disable transitive thunking for now
            elsif $metasym eq 'Z' { $helper := '&METAOP_ZIP'; $t := nqp::uc($t); }

            my $metapast := QAST::Op.new( :op<call>, :name($helper), WANTED($basepast,'infixish') );
            $metapast.push(QAST::Var.new(:name(baseop_reduce($base<OPER><O>.made)),
                                         :scope<lexical>))
                if $metasym eq 'X' || $metasym eq 'Z';
            $metapast.annotate('thunky', $t) if $t;
            $metapast.annotate('is_pure', $purity) if $purity;
            $ast := QAST::Op.new( :node($/), :op($astop), $metapast );
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
            if $basesym eq '||' || $basesym eq '&&'  || $basesym eq '//'
            || $basesym eq 'or' || $basesym eq 'and' || $basesym eq 'orelse'
            || $basesym eq 'andthen' || $basesym eq 'notandthen' {
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
        my $metaop   := baseop_reduce($base<OPER><O>.made);
        my $metapast := QAST::Op.new( :op<call>, :name($metaop), WANTED($basepast,'reduce'));
        my $t        := $basepast.ann('thunky') || $base<OPER><O>.made<thunky>;
        if $<triangle> {
            $metapast.push($*W.add_constant('Int', 'int', 1));
        }
        my $args := $<args>.ast;
        # one-arg rule?
        if +$args.list == 1 && !$args[0].flat && !$args[0].named {
            make QAST::Op.new(:node($/), :op<call>, WANTED($metapast,'reduce/meta'), WANTED($args[0],'reduce/meta'));
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
        my $Int := $*W.find_single_symbol('Int');
        my $power := nqp::box_i(0, $Int);
        for $<dig> {
            $power := nqp::add_I(
                nqp::mul_I($power, nqp::box_i(10, $Int), $Int),
                nqp::box_i(nqp::index("⁰¹²³⁴⁵⁶⁷⁸⁹", $_), $Int),
                $Int);
        }

        $power := nqp::neg_I($power, $Int) if $<sign> eq '⁻' || $<sign> eq '¯';
        make QAST::Op.new(:op<call>, :name('&postfix:<ⁿ>'), $*W.add_numeric_constant($/, 'Int', $power));
    }

    method hyper-nodal-name-tweak ($past) {
        unless my $name := $past.name {
            $past.unshift: QAST::SVal.new: :value('');
            return;
        }
        $past.unshift: QAST::SVal.new: :value($name);

        unless nqp::eqat($name, 'dispatch:<', 0) && (
               $name eq 'dispatch:<var>' || $name eq 'dispatch:<.*>'
            || $name eq 'dispatch:<.+>'  || $name eq 'dispatch:<.?>'
            || $name eq 'dispatch:<::>'
        ) {
            $past.unshift: QAST::SVal.new: :value('');
            return;
        }

        # the call we're hypering is another `dispatch:<` call, so we need
        # to figure out what's the actual method name we'll call, inside that
        # dispatch, so we can pass it to our hyper call for nodality calculation
        my $nodal-name := $past[1];

        if nqp::istype($nodal-name, QAST::Want) && nqp::elems($nodal-name) == 3
        && nqp::istype($nodal-name[2], QAST::SVal) {
            # the new nodal name might be another level of
            # dispatch:<...>; dig one level deeper for real name
            $nodal-name := $past[2]
                if $nodal-name[2].value eq 'dispatch:<var>'
                || $nodal-name[2].value eq 'dispatch:<::>';
        }
        else {
            # at this point, we could have method name in a string that
            # has a code block to run. We need to ensure the code runs just
            # once, so we'll store the result in a variable, to use for the
            # logic that figures out nodality. We'll also stick that variable
            # into the second `dispatch` call, to use as a name there.
            my $name-var := $*W.cur_lexpad.unique: 'nodal-name';
            $nodal-name := QAST::Op.new: :op<bind>,
                QAST::Var.new(:name($name-var), :scope<lexical>, :decl<var>),
                $past[1];
            $past[1] := QAST::Var.new: :name($name-var), :scope<lexical>;
        }

        $past.unshift: $nodal-name;
    }

    method postfixish($/) {
        if $<postfix_prefix_meta_operator> {
            my $past := $<OPER>.ast || QAST::Op.new( :name('&postfix' ~ $*W.canonicalize_pair('', $<OPER>.Str)),
                                                     :op<call> );
            if nqp::istype($past, QAST::Op) && $past.op() eq 'callmethod' {
                self.hyper-nodal-name-tweak($past);
                $past.name('dispatch:<hyper>');
            }
            elsif nqp::istype($past, QAST::Op) && $past.op() eq 'call' {
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

            # Check if we are inside «...» quoters and complain if the hyper creates
            # ambiguity with the quoters, since user may not wanted to have a hyper
            if ($/.pragma("STOPPER") // '')
                eq (my $sym := $<postfix_prefix_meta_operator>[0]<sym>)
            {
                $/.worry(
                    "Ambiguous use of $sym; use "
                    ~ ($sym eq '>>' ?? '»' !! '>>')
                    ~ " instead to mean hyper, or insert whitespace before"
                    ~ " $sym to mean a quote terminator (or use different delimiters?)"
                )
            }

            make $past;
        }
    }

    method postcircumfix:sym<[ ]>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<[ ]>'), :op('call'), :node($/) );
        if $<semilist> {
            my $c := $/;
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
        make wanted($<arglist>.ast, 'postcircumfix()/args');
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
        my $v := $*W.find_single_symbol('Version').new(~$<vstr>);
        $*W.add_object_if_no_sc($v);
        make QAST::WVal.new( :value($v) );
    }

    method decint($/) {
        my int $chars := nqp::chars($/);
        # 15 chars keeps us below 2**52 limit ((2**52).chars-1) which in double can be
        # represented exactly. See https://github.com/perl6/nqp/issues/363 for why this is desired
        make $chars > ($?BITS == 64 ?? 15 !! 9)
          ?? string_to_bigint($/, 10, $chars)
          !! string_to_int($/, 10, $chars);
    }
    method hexint($/) {
        my int $chars := nqp::chars($/);
        # 13 chars keeps us below 2**52 limit ((2**52).base(16).chars-1) which in double can be
        # represented exactly. See https://github.com/perl6/nqp/issues/363 for why this is desired
        make $chars > ($?BITS == 64 ?? 13 !! 7)
          ?? string_to_bigint($/, 16, $chars)
          !! string_to_int($/, 16, $chars);
    }
    method octint($/) {
        my int $chars := nqp::chars($/);
        # 17 chars keeps us below 2**52 limit ((2**52).base(8).chars-1) which in double can be
        # represented exactly. See https://github.com/perl6/nqp/issues/363 for why this is desired
        make $chars > ($?BITS == 64 ?? 17 !! 10)
          ?? string_to_bigint($/, 8, $chars)
          !! string_to_int($/, 8, $chars);
    }
    method binint($/) {
        my int $chars := nqp::chars($/);
        # 52 chars keeps us below 2**52 limit ((2**52).base(2).chars-1) which in double can be
        # represented exactly. See https://github.com/perl6/nqp/issues/363 for why this is desired
        make $chars > ($?BITS == 64 ?? 52 !! 30)
          ?? string_to_bigint($/, 2, $chars)
          !! string_to_int($/, 2, $chars);
    }

    method number:sym<numish>($/) {
        make $<numish>.ast;
    }

    method signed-integer($/) {
        make $*W.add_numeric_constant($/, 'Int',
            $<sign> eq '-' || $<sign> eq '−'
                ?? -$<integer>.ast !! $<integer>.ast
        );
    }

    method signed-number($/) {
        my $qast := $<number>.ast;
        $qast := QAST::Op.new( :op('call'), :name('&infix:<->'), $qast)
            if $<sign> eq '-' || $<sign> eq '−';
        make $qast;
    }

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
            my int $nu := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Numerator"));
            my int $de := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Denominator"));
            if !$de || $de == 1 {
                make $*W.add_numeric_constant($/, 'Int', +$nu)
            }
            else {
                my $ast := $*W.add_constant('Rat', 'type_new', $nu, $de, :nocache(1));
                $ast.node($/);
                make $ast;
            }
        }
        elsif $<uinf> {
            make $*W.add_numeric_constant($/, 'Num', nqp::inf);
        }
        else {
            make $*W.add_numeric_constant($/, 'Num', nqp::numify($/));
        }
    }

    method escale($/) {
        make $<sign> eq '-' || $<sign> eq '−'
            ??  nqp::neg_I($<decint>.ast, $<decint>.ast)
            !! $<decint>.ast;
    }

    method dec_number($/) {
        if $<escale> { # wants a Num
            make $*W.add_numeric_constant: $/, 'Num', nqp::numify($/);
        } else { # wants a Rat
            my $Int := $*W.find_single_symbol('Int');
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

                $partf := $partf[1];
            } else {
                $partf := nqp::box_i(1, $Int);
            }

            my $ast := $*W.add_constant('Rat', 'type_new', $parti, $partf, :nocache(1));
            $ast.node($/);
            make $ast;
        }
    }

    method rad_number($/) {
        my int $radix := nqp::radix(10, $<radix>, 0, 0)[0];

        if $<bracket> { # the "list of place values" case
            make QAST::Op.new(:name('&UNBASE_BRACKET'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<bracket>.ast);
        }
        elsif $<circumfix> { # the "conversion function" case
            make QAST::Op.new(:name('&UNBASE'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<circumfix>.ast);
        } else { # the "string literal" case
            my $Int := $*W.find_single_symbol('Int');

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
                    $/.panic("Unknown radix prefix '$ohradstr'.");
                }
            }

            my $ipart := nqp::radix_I($radix, $<intpart>.Str, 0, 0, $Int);
            my $fpart := nqp::radix_I($radix, nqp::chars($<fracpart>) ?? $<fracpart>.Str !! ".0", 1, 4, $Int);
            my $bpart := $<base> ?? nqp::tonum_I($<base>[0].ast) !! $radix;
            my $epart := $<exp> ?? nqp::tonum_I($<exp>[0].ast) !! 0;

            if $ipart[2] < nqp::chars($<intpart>.Str) {
                $*W.throw($/, 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :pos($ipart[2] < 0 ?? 0 !! $ipart[2]),
                    :reason("malformed base-$radix number"),
                );
            }
            if $fpart[2] < nqp::chars($<fracpart>.Str) {
                $*W.throw($/, 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :reason("malformed base-$radix number"),
                    :pos( # the -1 dance is due to nqp::radix returning -1 for
                        # failure to parse the first char, instead of 0;
                        # we return `1` to cover the decimal dot in that case
                        $ipart[2] + ($fpart[2] == -1 ?? 1 !! $fpart[2])
                    ),
                );
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
        my $ast := $*W.add_constant: 'Complex', 'type_new', :nocache(1),
            $*W.add_constant('Num', 'num',
                $<re><sign> eq '-' || $<re><sign> eq '−'
                  ??  nqp::neg_n($<re><number>.ast.compile_time_value)
                  !!  $<re><number>.ast.compile_time_value.Num
            ).compile_time_value,
            $*W.add_constant('Num', 'num',
                $<im><sign> eq '-' || $<im><sign> eq '−'
                  ??  nqp::neg_n($<im><number>.ast.compile_time_value)
                  !!  $<im><number>.ast.compile_time_value.Num
            ).compile_time_value;
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

                if $<colonpairs> && $<colonpairs>.ast<D> {
                    $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $type, 1);
                }
                elsif $<colonpairs> && $<colonpairs>.ast<U> {
                    $type := $*W.create_definite_type($*W.resolve_mo($/, 'definite'), $type, 0);
                }

                if ! nqp::isnull(my $accept := $*W.can_has_coercerz: $/) {
                    if $<typename> {
                        $/.panic("Cannot put 'of' constraint on a coercion type");
                    }
                    $type := $*W.create_coercion_type($/, $type, $accept);
                }
                elsif $<typename> {
                    $type := $*W.parameterize_type_with_args($/, $type,
                        [$<typename>.ast], hash());
                }
                make $type;
            }
            else {
                if $<arglist> || $<typename> {
                    $/.panic("Cannot put type parameters on a type capture");
                }
                if $<accepts> || $<accepts_any> {
                    $/.panic("Cannot base a coercion type on a type capture");
                }
                if $str_longname eq '::' {
                    $/.panic("Cannot use :: as a type name");
                }
                if $*W.cur_lexpad.symbol(nqp::substr($str_longname, 2)) {
                    $*W.throw($/, ['X', 'Redeclaration'],
                        symbol => nqp::substr($str_longname, 2));
                }
                make $*W.pkg_create_mo($/, $*W.resolve_mo($/, 'generic'), :name(nqp::substr($str_longname, 2)));
            }
        }
        else {
            make $*W.find_single_symbol('::?' ~ ~$<identifier>);
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
        unless nqp::istype($*value, QAST::Node) {
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
            nqp::push(@pairs, wanted($_.ast,'rx_adverbs'));
        }
        make @pairs;
    }

    method setup_quotepair($/) {
        my %h;
        my $key := $*ADVERB.ast.named;
        my $value := $*ADVERB.ast;
        if nqp::istype($value, QAST::IVal) || nqp::istype($value, QAST::SVal) {
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
            $<nibble>.ast, 'anon', '', %sig_info, $block) if $<nibble>.ast;
        # Return closure if not in sink context.
        my $closure := block_closure($coderef, :regex);
        $closure.annotate('sink_ast', QAST::Op.new( :op<callmethod>, :name<Bool>, $closure));
        make $closure;
    }

    method quote:sym<rx>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx', $block);
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block) if $<quibble>.ast;
        my $past := block_closure($coderef, :regex);
        $past.annotate('sink_ast', QAST::Op.new(:op<callmethod>, :name<Bool>, $past));
        make $past;
    }
    method quote:sym<m>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block) if $<quibble>.ast;

        my $past := QAST::Op.new(
            :node($/),
            :op('callmethod'), :name('match'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'m'),
            block_closure($coderef, :regex)
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
        # Prep our .trans() call QAST
        # $_.trans( Pair.new: left tribble, right tribble )
        my $trans := QAST::Op.new: :node($/),
            WANTED(QAST::Var.new(:name<$_>, :scope<lexical>), 'tr/call'),
            :op<callmethod>, :name<trans>,
                QAST::Op.new: :node($/),
                    :returns($*W.find_symbol: ['Pair']),
                    QAST::Var.new(:name<Pair>, :scope<lexical>),
                    :op<callmethod>, :name<new>,
                        $<tribble><left>.ast,  # key
                        $<tribble><right>.ast; # value

        self.handle_and_check_adverbs:
            $/, %TRANS_ALLOWED_ADVERBS, 'transliteration', $trans
        if nqp::elems($<rx_adverbs><quotepair>);

        # If we're just calling `TR///`, then we've got what we need already
        return make $trans if $<sym> eq 'TR';

        # We got here, that means we're using the `tr///`, so we'll grab
        # ourselves a temporary variable and save $_ into it. Then we'll
        # use our $trans QAST to call .trans() and will then save its result
        # into $_. Once that's done, we'll create a StrDistance object,
        # passing it our temp var with original $_ value and the new
        # result now stored in $_. That's the object we'll use as our return
        # value. Sounds fun and awesome! Let's do this \o/

        # ask for a unique name for our temp var
        my $original := $*W.cur_lexpad.unique: 'original_value_to_trans';

        make QAST::Stmt.new:
            QAST::Op.new( # save original $_ into our temp var
                QAST::Var.new(:name($original), :scope<lexical>, :decl<var>),
                :op<bind>, QAST::Op.new: :op<decont>,
                WANTED(QAST::Var.new(:name<$_>, :scope<lexical>), 'tr')
            ),
            QAST::Op.new( # call .trans() and assign result to $_
                WANTED(QAST::Var.new(:name<$_>, :scope<lexical>), 'tr/assign'),
                :op<call>, :name('&infix:<=>'),
                $trans,
            ),
            QAST::Op.new: # our return value: the StrDistance object
                :returns($*W.find_symbol: ['StrDistance']),
                WANTED(QAST::Var.new(
                  :name<StrDistance>, :scope<lexical> ), 'tr'
                ), :op<callmethod>, :name<new>,
                    QAST::Var.new(
                      :named<before>, :name($original), :scope<lexical>),
                    QAST::Var.new:
                      :named<after>,  :name<$_>, :scope<lexical>
    }

    method quote:sym<s>($/) {
        # We are emulating Str.subst/subst-mutate here, by calling match, assigning the result to
        # a temporary variable etc.

        # Build the regex.
        my $rx_block := $*SUBST_LHS_BLOCK;
        $rx_block.push(QAST::Stmts.new);
        my %sig_info := hash(parameters => []);
        my $rx_coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<sibble><left>.ast, 'anon', '', %sig_info, $rx_block);

        # Quote needs to be closure-i-fied.
        my $infixish := $<sibble><infixish>;
        my $right;
        if !$infixish || $infixish.Str eq '=' {
            $right := WANTED($<sibble><right>.ast,'quote:s///');
        }
        else {
            $right := $infixish.ast;
            $right.push(QAST::Op.new(
                :op('p6assign'),
                QAST::Op.new( :op('p6scalarfromdesc'), QAST::Op.new( :op('null') ) ),
                QAST::Var.new( :name('$/'), :scope('lexical') )
            ));
            $right.push(WANTED($<sibble><right>.ast,'quote:s'));
        }
        my $rep_block := $*SUBST_RHS_BLOCK;
        $rep_block.push(QAST::Stmts.new($right, :node($<sibble><right>)));
        my $closure := block_closure(reference_to_code_object(
            $*W.create_code_obj_and_add_child($rep_block, 'Code'),
            $rep_block));

        # self.match($rx_coderef, |%options);
        my $past := QAST::Op.new( :node($/), :op('callmethod'), :name('match'),
            WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'s'),
            block_closure($rx_coderef)
        );
        self.handle_and_check_adverbs($/, %SUBST_ALLOWED_ADVERBS, 'substitution', $past);
        if $/[0] {
            $past.push(QAST::IVal.new(:named('samespace'), :value(1)));
        }

        my $samespace := +$/[0];
        my $sigspace := $samespace;
        my $samecase := 0;
        my $samemark := 0;
        for $<rx_adverbs>.ast {
            if $_.named eq 'samecase' || $_.named eq 'ii' {
                $samecase := 1;
            }
            elsif $_.named eq 'samemark' || $_.named eq 'mm' {
                $samemark := 1;
            }
            elsif $_.named eq 'samespace' || $_.named eq 'ss' {
                $samespace := 1;
                $sigspace := 1;
            }
            elsif $_.named eq 'sigspace' || $_.named eq 's' {
                $sigspace := 1;
            }
        }

        my $S_result    := $past.unique('subst_S_result');
        my $result      := $past.unique('subst_result');
        my $list_result := $past.unique('subst_list_result');
        my $List        := $*W.find_single_symbol('List');

        my $apply_matches := QAST::Op.new( :op('callmethod'), :name('dispatch:<!>'),
            QAST::Op.new( :op('callmethod'),  :name('Str'),
                WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'s/apply') ),
            QAST::SVal.new( :value('APPLY-MATCHES') ),
            QAST::WVal.new( :value($*W.find_single_symbol('Str')) ),
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
                # var for final result string of S///
                $<sym> eq 'S' ?? QAST::Var.new(
                    :name($S_result), :scope('local'), :decl('var')
                ) !! QAST::Stmt.new(),

                # my $result;
                QAST::Var.new( :name($result), :scope('local'), :decl('var') ),

                # $result := self.match(...
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($result), :scope('local') ),
                    $past
                ),

                QAST::Op.new( :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope('lexical') ),
                    QAST::Var.new( :name($result), :scope('local') ),
                ),

                # It matched something. Either a single item or a list of matches.
                QAST::Op.new( :op('if'),
                    QAST::Op.new( :op('unless'),# :name('&infix:<||>'),
                        QAST::Op.new( :op('istype'),
                            QAST::Var.new( :name($result), :scope('local') ),
                            QAST::WVal.new( :value($*W.find_single_symbol('Match')) )
                        ),
                        QAST::Op.new( :op('if'),
                            QAST::Op.new( :op('istype'),
                                QAST::Var.new( :name($result), :scope('local') ),
                                QAST::WVal.new( :value($*W.find_single_symbol('Positional')) )
                            ),
                            QAST::Op.new( :op('callmethod'), :name('elems'),
                                QAST::Var.new( :name($result), :scope('local') )
                            )
                        )
                    ),

                    $<sym> eq 'S'
                    ?? QAST::Op.new( :op('bind'),
                        WANTED(QAST::Var.new( :name($S_result), :scope('local') ),'s/assign'),
                        $apply_matches
                    ) !! QAST::Op.new( :op('call'), :name('&infix:<=>'),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'s/assign'),
                        $apply_matches
                    ),

                    $<sym> eq 'S'
                    ?? QAST::Op.new( :op('bind'),
                        QAST::Var.new( :name($S_result), :scope('local') ),
                        WANTED(QAST::Var.new( :name('$_'), :scope('lexical') ),'S'),
                    ) !! QAST::Stmt.new(),
                ),

                # If we have a list of matches, then put them into $/,
                # otherwise, $/ already has the Match object we want it to have
                QAST::Op.new( :op('if'),
                    QAST::Op.new( :op('istype'),
                        QAST::Var.new( :name($result), :scope('local') ),
                        QAST::WVal.new( :value($*W.find_single_symbol('Positional')) )
                    ),
                    QAST::Op.new( :op('p6store'),
                        QAST::Var.new( :name('$/'), :scope('lexical') ),
                        QAST::Stmts.new(
                            QAST::Op.new( :op('bind'),
                                QAST::Var.new( :name($list_result), :scope('local'), :decl('var') ),
                                QAST::Op.new( :op('create'),
                                    QAST::WVal.new( :value($List) )
                                )
                            ),
                            QAST::Op.new( :op('bindattr'),
                                QAST::Var.new( :name($list_result), :scope('local') ),
                                QAST::WVal.new( :value($List) ),
                                QAST::SVal.new( :value('$!reified') ),
                                QAST::Op.new( :op('getattr'),
                                    QAST::Var.new( :name($result), :scope('local') ),
                                    QAST::WVal.new( :value($List) ),
                                    QAST::SVal.new( :value('$!reified') )
                                )
                            ),
                            QAST::Var.new( :name($list_result), :scope('local') )
                        )
                    ),
                ),

                # The result of this operation.
                $<sym> eq 's'
                ?? QAST::Var.new( :name('$/'), :scope('lexical') )
                !! QAST::Var.new( :name($S_result), :scope('local') ),
            ),
        );
        $past.annotate('is_S', $<sym> eq 'S');
        make WANTED($past, 's///');  # never carp about s/// in sink context
    }

    method quote:sym<quasi>($/) {
        my $ast_class := $*W.find_single_symbol('AST');
        my $quasi_ast := $ast_class.new();
        my $past := $<block>.ast.ann('past_block').pop;
        nqp::bindattr($quasi_ast, $ast_class, '$!past', $past);
        nqp::bindattr($quasi_ast, $ast_class, '$!Str', $/.Str());
        $*W.add_object_if_no_sc($quasi_ast);
        my $throwaway_block := QAST::Block.new();
        my $quasi_context := block_closure(
            reference_to_code_object(
                $*W.create_code_obj_and_add_child($throwaway_block, 'Block'),
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
        my $count := 0;
        for @params {
            next if $_<named_names> || $_<named_slurpy>;
            if $_<pos_slurpy> || $_<pos_onearg> {
                $count := nqp::inf;
                last;
            }
            $arity := $arity + 1 unless $_<optional>;
            $count := $count + 1;
        }
        $block.arity($arity);
        $block.annotate('count', $count);

        # Consider using the VM binder on backends where it will work out
        # (e.g. we can get the same errors).
        my $need_full_binder := 1;
        unless nqp::defined($use_vm_binder) {
            $use_vm_binder := nqp::getcomp('Raku').backend.name eq 'moar' || nqp::getcomp('Raku').backend.name eq 'js';
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

            # If there is a single raw $_ then handle this very common case
            # also. Two possibilities: optional and not.
            elsif default_topic_kind(@params) -> $kind {
                my $var := find_var_decl($block, '$_');
                $var.decl('param');
                if $kind eq 'optional' {
                    $var.default(QAST::Op.new(
                        :op('getlexouter'),
                        QAST::SVal.new( :value('$_') )
                    ));
                }
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
                if !nqp::istype($*W.find_single_symbol('Capture'), $only<type>) {
                    $only<node>.panic("Capture parameter must have a type accepting a Capture");
                }
                else {
                    return 1;
                }
            }
        }
        0
    }
    sub default_topic_kind(@params) {
        if nqp::elems(@params) == 1 {
            my $only := @params[0];
            if $only<is_raw> && $only<variable_name> eq '$_' &&
                    $only<type> =:= $*W.find_single_symbol('Mu') {
                return $only<default_from_outer> ?? 'optional' !! 'required';
            }
        }
        return '';
    }
    my $SIG_ELEM_IS_RW       := 256;
    my $SIG_ELEM_IS_RAW      := 1024;
    my $SIG_ELEM_IS_OPTIONAL := 2048;
    my $SIG_ELEM_IS_COERCIVE := 67108864;
    my @iscont_ops := ['iscont', 'iscont_i', 'iscont_n', 'iscont_s'];
    sub lower_signature($block, $sig, @params) {
        my @result;
        my $clear_topic_bind;
        my $saw_slurpy;
        my $Code     := $*W.find_single_symbol('Code', :setting-only);
        my $Sig      := $*W.find_single_symbol('Signature', :setting-only);
        my $Param    := $*W.find_single_symbol('Parameter', :setting-only);
        my $Iterable := $*W.find_single_symbol('Iterable');
        my $Scalar := $*W.find_single_symbol('Scalar');
        my @p_objs := nqp::getattr($sig, $Sig, '@!params');
        my int $i  := 0;
        my int $n  := nqp::elems(@params);
        # @!params attribute of the code object. Should only be used when handling of generics is needed.
        my $ins_params;

        sub signature_params($var) {
            unless $ins_params {
                $ins_params := QAST::Node.unique('__lowered_parameters_');
                $var.push(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new(:name($ins_params), :scope('local'), :decl('var')),
                        QAST::Op.new( # Get @!params on the signature
                            :op('getattr'),
                            QAST::Op.new( # Get signature object
                                :op('getattr'),
                                QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                                QAST::WVal.new(:value($Code)),
                                QAST::SVal.new(:value('$!signature'))
                            ),
                            QAST::WVal.new(:value($Sig)),
                            QAST::SVal.new(:value('@!params'))
                        )
                    )
                );
            }
            QAST::Var.new(:name($ins_params), :scope('local'))
        }

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
                if !nqp::istype($*W.find_single_symbol('Capture'), %info<type>) {
                    %info<node>.panic("Capture parameter must have a type accepting a Capture");
                }
                $var.slurpy(1);
                my $hash_name := $name ~ '_hash';
                @result.push(QAST::Var.new(
                    :name($hash_name), :scope('local'), :decl('param'),
                    :slurpy(1), :named(1)
                ));
                if nqp::existskey(%info, 'variable_name') {
                    # Build a capture object.
                    my $Capture := QAST::WVal.new( :value($*W.find_single_symbol('Capture')) );
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Op.new(
                            :op('p6bindattrinvres'),
                            QAST::Op.new(
                                :op('p6bindattrinvres'),
                                QAST::Op.new( :op('create'), $Capture ),
                                $Capture,
                                QAST::SVal.new( :value('@!list') ),
                                QAST::Var.new( :name($name), :scope('local') )
                            ),
                            $Capture,
                            QAST::SVal.new( :value('%!hash') ),
                            QAST::Var.new( :name($hash_name), :scope('local') )
                        )));
                }
            }
            elsif nqp::existskey(%info, 'named_names') {
                my @names := %info<named_names>;
                if nqp::elems(@names) == 1 {
                    $var.named(nqp::atpos_s(@names, 0));
                }
                elsif nqp::elems(@names) == 2 {
                    my @names_copy;
                    @names_copy[0] := nqp::atpos_s(@names, 0);
                    @names_copy[1] := nqp::atpos_s(@names, 1);
                    $var.named(@names_copy);
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
                            QAST::WVal.new( :value($*W.find_single_symbol('Hash')) )
                        ),
                        QAST::WVal.new( :value($*W.find_single_symbol('Map')) ),
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
            my $param_type := %info<type>;
            my $ptype_archetypes := $param_type.HOW.archetypes;
            my int $is_generic := %info<type_generic>;
            my int $is_coercive := $ptype_archetypes.coercive;
            my $nomtype    := !$is_generic && $ptype_archetypes.nominalizable
                                ?? $param_type.HOW.nominalize($param_type)
                                !! $param_type;
            my int $is_rw := $flags +& $SIG_ELEM_IS_RW;
            my int $spec  := nqp::objprimspec($nomtype);
            my $decont_name;
            my int $decont_name_invalid := 0;
            sub get_decont_name() {
                return NQPMu if $decont_name_invalid;
                unless $decont_name {
                    # We decont it once before checks that need a decont value,
                    # to avoid doing so repeatedly.
                    $decont_name := QAST::Node.unique("__lowered_param_decont_");
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($decont_name), :scope('local'), :decl('var') ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($name), :scope('local') )
                        )));
                }
                return $decont_name;
            }
            if !$is_generic && $spec {
                if $is_rw {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op(@iscont_ops[$spec]),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
                else {
                    $var.returns($param_type);
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
                if $is_generic && !$is_coercive {
                    my $genericname := $param_type.HOW.name(%info<attr_package>);
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('istype_nd'),
                        QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                        QAST::Var.new( :name($genericname), :scope<typevar> )
                    )));
                } elsif !($param_type =:= $*W.find_single_symbol('Mu')) {
                    if $ptype_archetypes.generic {
                        return 0 unless %info<is_invocant>;
                    }
                    else {
                        if $param_type =:= $*W.find_single_symbol('Positional') {
                            $var.push(QAST::Op.new(
                                :op('if'),
                                QAST::Op.new(
                                    :op('istype_nd'),
                                    QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                                    QAST::WVal.new( :value($*W.find_single_symbol('PositionalBindFailover')) )
                                ),
                                QAST::Op.new(
                                    :op('bind'),
                                    QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                                    QAST::Op.new(
                                        :op('decont'),
                                        QAST::Op.new(
                                            :op('bind'),
                                            QAST::Var.new( :name($name), :scope('local') ),
                                            QAST::Op.new(
                                                :op('callmethod'), :name('cache'),
                                                QAST::Var.new( :name(get_decont_name()), :scope('local') )
                                            ))))));
                        }

                        # Try to be smarter with coercions. We don't have to do full typecheck on them, which results in
                        # additional call to a HOW method. Instead it's ok to check if value matches target or
                        # constraint types.
                        if $is_coercive && nqp::can($param_type.HOW, 'target_type') {
                            $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                                    :op('unless'),
                                    QAST::Op.new(
                                        :op('istype_nd'),
                                        QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                                        QAST::WVal.new( :value($param_type.HOW.target_type($param_type) ))),
                                    QAST::Op.new(
                                        :op('istype_nd'),
                                        QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                                        QAST::WVal.new( :value($param_type.HOW.constraint_type($param_type) ))))));
                        }
                        else {
                            $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                                :op('istype_nd'),
                                QAST::Var.new( :name(get_decont_name()), :scope('local') ),
                                QAST::WVal.new( :value($param_type) )
                            )));
                        }
                    }
                }
                if %info<undefined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('not_i'),
                        QAST::Op.new(
                            :op('isconcrete_nd'),
                            QAST::Var.new( :name(get_decont_name()), :scope('local') )
                        ))));
                }
                if %info<defined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isconcrete_nd'),
                        QAST::Var.new( :name(get_decont_name()), :scope('local') )
                    )));
                }
                if $is_rw {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isrwcont'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
            }

            # If there are type captures involved - most commonly $?CLASS and
            # ::?CLASS - we emit a piece of code for each target that gets the
            # WHAT of the given value and binds it.
            #
            # In theory, we could bind a local with the result of the WHAT
            # operation, but I'm not convinced it's sufficiently expensive.
            if %info<type_captures> {
                my $iter := nqp::iterator(%info<type_captures>);
                while $iter {
                    $var.push( QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new( :name(nqp::shift($iter)), :scope<lexical> ),
                        get_decont_name()
                            ?? QAST::Op.new( :op<what_nd>, QAST::Var.new( :name(get_decont_name()), :scope<local> ) )
                            !! QAST::Op.new( :op<what>, QAST::Var.new( :name($name), :scope<local> ) )
                        )
                    );
                }
            }

            # Handle coercion.
            # For a generic we can't know beforehand if it's going to be a coercive or any other nominalizable. Thus
            # we have to fetch the instantiated parameter object and do run-time processing.
            if $ptype_archetypes.generic {
                # For a generic-typed parameter get its instantiated clone and see if its type is a coercion.
                $decont_name_invalid := 1;
                my $inst_param := QAST::Node.unique('__lowered_param_obj_');
                my $low_param_type := QAST::Node.unique('__lowered_param_type');
                $var.push( # Fetch instantiated Parameter object
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($inst_param), :scope('local'), :decl('var') ),
                        QAST::Op.new(
                            :op('atpos'),
                            signature_params($var),
                            QAST::IVal.new(:value($i)))));
                $var.push( # Get actual parameter type
                                QAST::Op.new(
                                    :op('bind'),
                                    QAST::Var.new(:name($low_param_type), :scope('local'), :decl('var')),
                                    QAST::Op.new(
                                        :op('getattr'),
                                        QAST::Var.new(:name($inst_param), :scope('local')),
                                        QAST::WVal.new(:value($Param)),
                                        QAST::SVal.new(:value('$!type')))));
                $var.push(
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('istype'),
                            QAST::Var.new(:name($name), :scope('local')),
                            QAST::Var.new(:name($low_param_type), :scope('local'))
                        ),
                        QAST::Op.new(
                            :op('if'),
                            QAST::Op.new(
                                :op('bitand_i'),
                                QAST::Op.new(
                                    :op('getattr'),
                                    QAST::Var.new(:name($inst_param), :scope('local')),
                                    QAST::WVal.new(:value($Param)),
                                    QAST::SVal.new(:value('$!flags'))
                                ),
                                QAST::IVal.new(:value($SIG_ELEM_IS_COERCIVE))
                            ),
                            QAST::Op.new(
                                :op('bind'),
                                QAST::Var.new(:name($name), :scope('local')),
                                QAST::Op.new(
                                    :op('callmethod'),
                                    :name('coerce'),
                                    QAST::Op.new(
                                        :op('how'),
                                        QAST::Var.new(:name($low_param_type), :scope('local'))),
                                    QAST::Var.new(:name($low_param_type), :scope('local')),
                                    QAST::Var.new(:name($name), :scope('local')))))));
            }
            elsif nqp::can($ptype_archetypes, 'coercive') && $ptype_archetypes.coercive {
                $decont_name_invalid := 1;
                my $coercion_type := $param_type.HOW.wrappee($param_type, :coercion);
                my $target_type := $coercion_type.HOW.target_type($coercion_type);
                $*W.add_object_if_no_sc($param_type.HOW);
                $*W.add_object_if_no_sc($target_type);
                $var.push(
                    QAST::Op.new(
                        :op('unless'),
                        QAST::Op.new(
                            :op('istype'),
                            QAST::Var.new( :name($name), :scope('local') ),
                            QAST::WVal.new( :value($target_type) )
                        ),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($name), :scope('local') ),
                            QAST::Op.new(
                                :op('callmethod'),
                                :name('coerce'),
                                QAST::WVal.new(:value($param_type.HOW)),
                                QAST::WVal.new(:value($param_type)),
                                QAST::Var.new( :name($name), :scope('local') )))));
            }

            # If it's optional, do any default handling.
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                $decont_name_invalid := 1;
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
                                QAST::Op.new( :op('callmethod'), :name('clone'), $wval ))));
                    }
                }
                else {
                    if (my $is-array := %info<sigil> eq '@') || %info<sigil> eq '%' {

                        my $role      := $is-array ?? 'Positional' !! 'Associative';
                        my $base-type := $is-array ?? 'Array'      !! 'Hash';

                        $var.default(
                            QAST::Op.new(
                                :op<create>,
                                QAST::WVal.new(
                                    value => nqp::istype($nomtype, $*W.find_single_symbol($role)) && nqp::can($nomtype.HOW, 'role_arguments')
                                               ?? $*W.parameterize_type_with_args(NQPMu, $*W.find_single_symbol($base-type), $nomtype.HOW.role_arguments($nomtype), nqp::hash)
                                               !! $*W.find_single_symbol($base-type)
                            )));
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
                            # XXX This fails for generics because a generic has to be instantiated. I don't get this
                            # fixed because it'd take more than worth it considering the upcoming RakuAST. //vrurg
                            $var.default(
                                QAST::WVal.new( :value($is_coercive
                                                        ?? $param_type.HOW.target_type($param_type)
                                                        !! $nomtype)));
                        }
                    }
                }
            }


            # If it's the invocant, needs to go into self also.
            if %info<is_invocant> {
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name('self'), :scope('lexical') ),
                    get_decont_name()
                        ?? QAST::Var.new( :name(get_decont_name()), :scope('local') )
                        !! QAST::Op.new(
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
                                        QAST::WVal.new( :value($*W.find_single_symbol('Array')) )
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
                                        QAST::WVal.new( :value($*W.find_single_symbol('Hash')) )
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
                    # If the type means there's no way this could possibly be
                    # Iterable, we know it can't flatten. This in turn means
                    # we need not wrap it in a read-only scalar.
                    my $wrap := $flags +& $SIG_ELEM_IS_COPY;
                    unless $wrap {
                        if !$param_obj.coercive {
                            $wrap := nqp::istype($nomtype, $Iterable) || nqp::istype($Iterable, $nomtype);
                        }
                        else {
                            my $coercion_type := $param_type.HOW.wrappee($param_type, :coercion);
                            my $coerce_nom := $coercion_type.HOW.nominal_target($coercion_type);
                            $wrap := nqp::istype($coerce_nom, $Iterable) || nqp::istype($Iterable, $coerce_nom);
                        }
                    }
                    if $wrap {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            WANTED(QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),'lower_signature/wrap'),
                            nqp::existskey(%info, 'container_descriptor')
                                ?? QAST::Op.new(
                                        :op('p6scalarwithvalue'),
                                        QAST::WVal.new( :value(%info<container_descriptor>) ),
                                        QAST::Var.new( :name(get_decont_name() || $name), :scope('local') )
                                   )
                                !! QAST::Op.new(
                                        :op('p6bindattrinvres'),
                                        QAST::Op.new(
                                            :op('create'),
                                            QAST::WVal.new( :value($Scalar) )
                                        ),
                                        QAST::WVal.new( :value($Scalar) ),
                                        QAST::SVal.new( :value('$!value') ),
                                        get_decont_name()
                                            ?? QAST::Var.new( :name(get_decont_name()), :scope('local') )
                                            !! QAST::Op.new(
                                                :op('decont'),
                                                QAST::Var.new( :name($name), :scope('local') )
                                            )
                                   )
                            ));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            WANTED(QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),'lower_signature'),
                            get_decont_name()
                                ?? QAST::Var.new( :name(get_decont_name()), :scope('local') )
                                !! QAST::Op.new(
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
                my $wInt := $*W.find_symbol: ['Int'], :setting-only;
                my $wStr := $*W.find_symbol: ['Str'], :setting-only;
                my $wNum := $*W.find_symbol: ['Num'], :setting-only;

                for %info<post_constraints> {
                    my $var-qast := QAST::Var.new: :$name, :scope<local>;
                    my $wval     := QAST::WVal.new: :value($_);
                    my $what     := nqp::what($_);
                    my $isCode   := nqp::defined($_) && nqp::istype($_,
                        $*W.find_symbol: ['Code'], :setting-only);
                    my $param    := QAST::ParamTypeCheck.new(
                        nqp::eqaddr($what, $wInt)
                          ?? QAST::Op.new(:op<if>,
                              QAST::Op.new(:op<isconcrete>, $var-qast),
                              QAST::Op.new(:op<iseq_I>, $wval,
                                QAST::Op.new: :op<decont>, $var-qast))
                          !! nqp::eqaddr($what, $wNum)
                            ?? QAST::Op.new(:op<if>,
                                QAST::Op.new(:op<isconcrete>, $var-qast),
                                QAST::Op.new(:op<unless>,
                                  QAST::Op.new(:op<iseq_n>, $wval, $var-qast), # equal
                                  QAST::Op.new(:op<if>, # or both are NaNs
                                    QAST::Op.new(:op<isne_n>, $wval, $wval),
                                    QAST::Op.new(:op<isne_n>, $var-qast, $var-qast))))
                            !! nqp::eqaddr($what, $wStr)
                              ?? QAST::Op.new(:op<if>,
                                  QAST::Op.new(:op<isconcrete>, $var-qast),
                                  QAST::Op.new(:op<iseq_s>, $wval, $var-qast))
                              !! QAST::Op.new: :op<istrue>, QAST::Op.new: :op<callmethod>,
                                  :name<ACCEPTS>,
                                  $isCode
                                    ?? QAST::Op.new(:op<p6capturelex>,
                                      QAST::Op.new: :op<callmethod>, :name<clone>, $wval)
                                    !! $wval,

                                  $var-qast
                    );

                    if $isCode {
                        $param.annotate: 'code-post-constraint', 1;
                        # Some optimizations we must handle specially if no
                        # autothreading of Junctions will happen:
                        $param.annotate: 'no-autothread', 1
                          unless nqp::istype(%info<type>,
                            $*W.find_symbol: ['Any'], :setting-only);
                    }

                    $var.push: $param;
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
                    my $attr_package := %info<attr_package>;
                    my $attr_name := %info<variable_name>;
                    my $attr_type := try $attr_package.HOW.get_attribute_for_usage($attr_package, $attr_name).type;
                    if nqp::objprimspec($attr_type) {
                         $var.push(QAST::Op.new(
                             :op('bind'),
                             QAST::Var.new(
                                 :name($attr_name), :scope('attribute'), :returns($attr_type),
                                 QAST::Var.new( :name('self'), :scope('lexical') ),
                                 QAST::WVal.new( :value($attr_package) )
                             ),
                             QAST::Var.new( :name($name), :scope('local') )));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new(
                                :name($attr_name), :scope('attribute'),
                                QAST::Var.new( :name('self'), :scope('lexical') ),
                                QAST::WVal.new( :value($attr_package) )
                            ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
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
            node              => $/,
            is_multi_invocant => 1,
            sigil             => ~$sigil);

        # Apply any type implied by the sigil.
        if $sigil eq '@' {
            %param_info<type> := $*W.find_single_symbol('Positional');
        }
        elsif $sigil eq '%' {
            %param_info<type> := $*W.find_single_symbol('Associative');
        }
        elsif $sigil eq '&' {
            %param_info<type> := $*W.find_single_symbol('Callable');
        }

        # If it's slurpy, just goes on the end.
        if $pos_slurpy || $named_slurpy {
            @params.push(%param_info);
        }

        # If it's named, just shove it on the end, but before any slurpies.
        elsif $named {
            %param_info<named_names> := nqp::list_s($ident);
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
                postfix => 'as a placeholder parameter',
            );
        }
        $block[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        $block.symbol($name, :scope('lexical'), :placeholder_parameter(1));
        QAST::Var.new( :name($name), :scope('lexical') )
    }

    sub reference_to_code_object($code_obj, $past_block) {
        QAST::WVal.new(
            :value($code_obj)
        ).annotate_self(
            'past_block', $past_block
        ).annotate_self('code_object', $code_obj)
    }

    our sub make_thunk_ref($to_thunk, $/) {
        my $block := $*W.push_lexpad($/);
        $block.blocktype('declaration_static');
        if !$*SUPPOSING {  # don't actually copy the thunk if inside <?before>
            fatalize($to_thunk) if $*FATAL || (nqp::can($/,'CURSOR') ?? $/.pragma('fatal') !! $*LANG.pragma('fatal'));
            $block.push(QAST::Stmts.new(autosink($to_thunk)));
        }
        $*W.pop_lexpad();

        reference_to_code_object(
            $*W.create_code_obj_and_add_child($block, 'Code'),
            $block);
    }

    sub make_topic_block_ref(
        $/, $past, :$copy, :$migrate_stmt_id, :$andnotelse_thunk,
    ) {
        my $block := $*W.push_lexpad($/);
        $block.annotate: 'andnotelse_thunk', 1 if $andnotelse_thunk;

        $block[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
        $block.push($past);
        $*W.pop_lexpad();
        if nqp::defined($migrate_stmt_id) {
            migrate_blocks($*W.cur_lexpad(), $block, -> $b {
                (    (! $b.ann('in_stmt_mod_andnotelse') &&   $andnotelse_thunk)
                  || (! $b.ann('in_stmt_mod')            && ! $andnotelse_thunk)
                )
                && ($b.ann('statement_id') // -1) >= $migrate_stmt_id
                && ! $b.has_ann('andnotelse_thunk')
            });
        }
        ($*W.cur_lexpad())[0].push($block);
        my $param := hash( :variable_name('$_'), :type($*W.find_single_symbol('Mu')));
        if $copy {
            $param<container_descriptor> := $*W.create_container_descriptor(
                $*W.find_single_symbol('Mu'), '$_');
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
                ))).annotate_self: 'outer', $*W.cur_lexpad;
        ($*W.cur_lexpad())[0].push($past);

        # Give it a signature and create code object.
        my $param := hash(
            variable_name => '$_',
            type => $*W.find_single_symbol('Mu'));
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
        $call.nosink(1);
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
                QAST::WVal.new( :value($*W.find_single_symbol('Nil')) )
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
            $prev_content.push($handler.ann('past_block').shift()) while nqp::elems($handler.ann('past_block'));
            $prev_content.push(QAST::WVal.new( :value($*W.find_single_symbol('Nil')) ));
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
                :op('call'),
                QAST::Var.new( :name($handler_lex_name), :scope('lexical') ),
                $ex
            ),
            QAST::WVal.new( :value($*W.find_single_symbol('Nil')) )
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
        if $initializer.has_compile_time_value {
            my $build := $initializer.compile_time_value;
            if nqp::isconcrete($build) {  # can't handle type values yet
                return $*W.apply_trait($/, '&trait_mod:<will>', $attr, :$build);
            }
        }

        # Need to construct and install an initializer method
        my @params := [
          hash( is_invocant => 1, type => $/.package),
          hash( variable_name => '$_', type => $*W.find_single_symbol('Mu'))
        ];
        my $sig := $*W.create_signature(nqp::hash('parameter_objects', [
          $*W.create_parameter($/, @params[0]),
          $*W.create_parameter($/, @params[1])
        ]));

        $block[0].push(
          QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') )
        );
        $block[0].push(
          QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') )
        );
        $block.push(
          QAST::Stmts.new(
            WANTED($initializer, 'install_attr_init'), :node($/)
          )
        );

        add_signature_binding_code($block, $sig, @params);
        $block.blocktype('declaration_static');

        # Block should go in current lexpad, in correct lexical context.
        ($*W.cur_lexpad())[0].push($block);

        # Dispatch trait.
        my $code := $*W.create_code_object($block, 'Method', $sig);
        $*W.apply_trait($/, '&trait_mod:<will>', $attr, :build($code));
    }

    # Some calls are handled specially by their name.
    my %dispatchered := nqp::hash(
        'callsame', NQPMu,
        'callwith', NQPMu,
        'nextsame', NQPMu,
        'nextwith', NQPMu,
        'nextcallee', NQPMu,
        'lastcall', NQPMu,
        'samewith', NQPMu,
    );
    sub handle_special_call_names($/, $args, $name) {
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
            $*W.throw($/, 'X::SecurityPolicy::Eval') unless $all_literal || monkey_see_no_eval($/);
            $*W.cur_lexpad().no_inline(1);
        }
        elsif nqp::existskey(%dispatchered, $name) {
            $*W.mark_no_inline_upto_dispatcher();
        }
        WANTALL($args, 'handle_special_call_names');
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
        %curried{'&infix:<...>'}   := 0;
        %curried{'&infix:<…>'}     := 0;
        %curried{'&infix:<...^>'}  := 0;
        %curried{'&infix:<…^>'}    := 0;
        %curried{'&infix:<^...>'}  := 0;
        %curried{'&infix:<^…>'}    := 0;
        %curried{'&infix:<^...^>'} := 0;
        %curried{'&infix:<^…^>'}   := 0;
        %curried{'&infix:<=>'}     := 0;
        %curried{'&infix:<:=>'}    := 0;
        %curried{'&infix:<~~>'} := 1;
        %curried{'&infix:<∘>'}  := 1;
        %curried{'&infix:<o>'}  := 1;
        %curried{'&infix:<..>'}   := 2;
        %curried{'&infix:<..^>'}  := 2;
        %curried{'&infix:<^..>'}  := 2;
        %curried{'&infix:<^..^>'} := 2;
        %curried{'&infix:<xx>'}   := 2;
        %curried{'callmethod'}           := 3;
        %curried{'p6callmethodhow'}      := 3;
        %curried{'&postcircumfix:<[ ]>'} := 3;
        %curried{'&postcircumfix:<{ }>'} := 3;
    }
    my $whatever_sym := nqp::null;
    method whatever_sym() {
        nqp::isnull($whatever_sym)
            ?? ($whatever_sym := $*W.find_symbol: ['Whatever'], :setting-only)
            !! $whatever_sym
    }
    my $whatever_code_sym := nqp::null;
    method whatever_code_sym() {
        nqp::isnull($whatever_code_sym)
            ?? ($whatever_code_sym
                := $*W.find_symbol: ['WhateverCode'], :setting-only)
            !! $whatever_code_sym
    }
    my $hyper_whatever_sym := nqp::null;
    method hyper_whatever_sym() {
        nqp::isnull($hyper_whatever_sym)
            ?? ($hyper_whatever_sym
                := $*W.find_symbol: ['HyperWhatever'], :setting-only)
            !! $hyper_whatever_sym
    }
    method whatever_curry($/, $qast, $upto_arity) {
        my int $curried :=
            # It must be an op and...
            nqp::istype($qast, QAST::Op) && (

            # Either a call that we're allowed to curry...
                (($qast.op eq 'call' || $qast.op eq 'chain') &&
                    (nqp::eqat($qast.name, '&infix:', 0) ||
                     nqp::eqat($qast.name, '&prefix:', 0) ||
                     nqp::eqat($qast.name, '&postfix:', 0) ||
                     (nqp::istype($qast[0], QAST::Op) &&
                        nqp::eqat($qast[0].name, '&METAOP', 0))) &&
                    %curried{$qast.name} // 3)

            # Or not a call and an op in the list of alloweds.
                || ($qast.op ne 'call' && %curried{$qast.op} // 0)

            # or one of our new postcircumfix subs that used to be methods
                || ($qast.op eq 'call' &&
                  (nqp::eqat($qast.name, '&postcircumfix:', 0) &&
                    %curried{$qast.name} // 0)
                  # or it's one of the curriable things rewritten to `call`
                  || $qast.has_ann('curriable-call-offset') && 3)
            );

        return $qast unless $curried;

        # Some constructs, like &METAOP things, have metaop construction as
        # first few kids of the QAST. We'll skip them while raking for Whatevers
        my int $offset := $qast.has_ann('curriable-call-offset')
          ?? $qast.ann('curriable-call-offset')
          !! $qast.op eq 'call' && ! nqp::eqat($qast.name,'&postcircumfix:', 0)
            ?? nqp::elems($qast) - $upto_arity !! 0;
        my int $i := $offset;
        my int $e := $upto_arity + $offset;
        my int $whatevers     := 0;
        my int $hyperwhatever := 0;

        my $Whatever      := self.whatever_sym;
        my $WhateverCode  := self.whatever_code_sym;
        my $HyperWhatever := self.hyper_whatever_sym;

        # Find anything we might need to curry and bail out if there's nothing
        while $i < $e {
            my $check := $qast[$i];
            $check := $check[0]
                if ( nqp::istype($check, QAST::Stmts)
                  || nqp::istype($check, QAST::Stmt))
                && nqp::elems($check) == 1;

            $whatevers++
                if nqp::bitand_i($curried, 1)
                  && istype($check.returns, $Whatever)
                  && nqp::isconcrete($check.value)
                || nqp::bitand_i($curried, 2)
                  && istype($check.returns, $WhateverCode)
                  && nqp::istype($check, QAST::Op);

            if nqp::bitand_i($curried, 1)
            && istype($check.returns, $HyperWhatever)
            && nqp::isconcrete($check.value) {
                $hyperwhatever := 1;
                $whatevers++;
            }
            $i++;
        }
        return $qast unless $whatevers;

        $*W.throw: $/, ['X', 'HyperWhatever', 'Multiple']
            if $hyperwhatever && $whatevers > 1;

        my $was_chain := $qast.op eq 'chain' ?? $qast.name !! NQPMu;
        my @params;
        my @old_args;
        my $cur_lexpad := $*W.cur_lexpad;
        my $curry := QAST::Block.new(QAST::Stmts.new, $qast
            ).annotate_self('statement_id', $*STATEMENT_ID
            ).annotate_self( 'in_stmt_mod', $*IN_STMT_MOD,
            ).annotate_self: 'outer',       $cur_lexpad;
        $cur_lexpad[0].push: $curry;

        $i := 0;
        while $i < $e {
            my $orig := $qast[$i];
            $orig := $orig[0]
              if (nqp::istype($orig, QAST::Stmts)
              ||  nqp::istype($orig, QAST::Stmt)
            ) && nqp::elems($orig) == 1;

            if nqp::bitand_i($curried, 2) # can curry WhateverCode for this op
            && istype($orig.returns, $WhateverCode)
            && nqp::istype($orig, QAST::Op) {
                my $orig-past := $orig.ann: 'past_block';
                $qast[$i] := $orig-past[1];
                for @($orig-past[0]) {
                    if nqp::istype($_, QAST::Var) {
                        # For QAST::Vars, ignore the params signature maker made
                        unless $_.decl eq 'param' {
                            $curry[0].push: $_;
                            @params.push: hash(
                              :variable_name($_.name),
                              :type(
                                  $*W.find_symbol: ['Mu'], :setting-only),
                              :is_raw(1))
                        }
                    }
                    elsif ! (nqp::istype($_, QAST::Op)
                    && $_.op eq 'p6bindsig') {
                        $curry[0].push: $_;
                    }
                }
                $orig-past[0] := QAST::Stmts.new;
                $orig-past[1] := QAST::Op.new: :op<die>,
                    QAST::SVal.new: :value('This WhateverCode has been inlined into another WhateverCode and should not have been called!');
            }
            elsif nqp::bitand_i($curried, 1) # can curry [Hyper]Whatever for op
            && ( istype($orig.returns, $Whatever)
              || istype($orig.returns, $HyperWhatever))
            && nqp::isconcrete($orig.value) {
                # simply replace this child with a variable that will be set
                # from the param of the curry we're making
                my $param := QAST::Var.new(:scope<lexical>,
                    :name($cur_lexpad[0].unique: '$whatevercode_arg')
                  ).annotate_self: 'whatever-var', 1;
                @params.push: hash(
                    :variable_name($param.name),
                    :type($*W.find_symbol: ['Mu'], :setting-only),
                    :is_raw(1));
                $curry[0].push: $param.decl_as: <var>;
                $qast[$i] := $param;
                nqp::push(@old_args, $param) if $was_chain;
            } else {
                # This child is not one of the Whatevers or we're in an op
                # that's not allowed to curry some of them. Simply ignore it,
                # but ensure we migrate any QAST::Blocks, for correct scoping
                find_block_calls_and_migrate($cur_lexpad, $curry, $orig);
            }
            $i++;
        }

        # go through any remaining children and just migrate QAST::Blocks
        my $qels := nqp::elems($qast);
        while $i < $qels {
            find_block_calls_and_migrate($cur_lexpad, $curry, $qast[$i]);
            $i++;
        }

        # Bake the signature for our curry
        my %sig_info := hash(parameters => @params);
        my $signature := $*W.create_signature_and_params:
            $/, %sig_info, $curry, 'Mu';
        add_signature_binding_code($curry, $signature, @params);

        fatalize($curry[1]) if $*FATAL;

        # Create a code object for our curry
        my $code := $*W.create_code_object: $curry, 'WhateverCode', $signature;
        $qast := block_closure(reference_to_code_object($code, $curry));
        $qast.returns: $WhateverCode;
        $qast.arity: nqp::elems(@params);

        # Hyperspace!
        $qast := QAST::Op.new: :op<call>, :name<&HYPERWHATEVER>, $qast
            if $hyperwhatever;
        $qast;
    }

    sub find_block_calls_and_migrate($from, $to, $qast) {
        if nqp::can($qast, 'ann') && $qast.ann('past_block') -> $block {
            $to[0].push: $block;
            remove_block($from, $block, :ignore-not-found);
        }
        elsif nqp::istype($qast, QAST::Node) {
            for @($qast) {
                find_block_calls_and_migrate($from, $to, $_);
            }
        }
    }

    sub remove_block($from, $block, :$ignore-not-found) {
        # Remove the QAST::Block $block from $from[0]; die if not found.
        my @decls := $from[0].list;
        my int $n := nqp::elems(@decls);
        my int $i := -1;
        while ++$i < $n {
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
        }
        nqp::die('Internal error: failed to remove block')
            unless $ignore-not-found;
    }

    sub wrap_return_type_check($wrappee, $code_obj) {
        my $ret := %*SIG_INFO<returns>;
        return $wrappee if nqp::isconcrete($ret) || $ret.HOW.name($ret) eq 'Nil';
        QAST::Op.new(
            :op('p6typecheckrv'),
            $wrappee,
            QAST::WVal.new( :value($code_obj) ),
            QAST::WVal.new( :value($*W.find_single_symbol('Nil')) )
        );
    }

    sub wrap_return_handler($past) {
        wrap_return_type_check(
            QAST::Op.new(
                :op<handlepayload>,
                # If we fall off the bottom, decontainerize if
                # rw not set.
                QAST::Op.new( :op(decontrv_op()), QAST::WVal.new( :value($*DECLARAND) ), $past ),
                'RETURN',
                QAST::Op.new( :op<lastexpayload> )
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
        if $is_generic || nqp::isnull(nqp::getobjsc($type)) || istype($type.HOW,$/.how('package')) {
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

    #================================================================
    # POD-ONLY CODE HANDLERS
    #================================================================
    # move ALL Pod-only action objects here

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

    # TODO The spaces arg from Grammar.nqp seems
    #      NOT to be handled. That shows up
    #      in testing for config continuation lines.
    method pod_configuration($/) {
        make Perl6::Pod::make_config($/);
    }

    method pod_block:sym<delimited>($/) {
        if $<type>.Str ~~ /^defn/ {
            make Perl6::Pod::defn($/, $delim-block);
        }
        else {
            make Perl6::Pod::any_block($/, $delim-block);
        }
    }

    method pod_block:sym<delimited_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<delimited_table>($/) {
        make Perl6::Pod::table($/, $delim-block);
    }

    method pod_block:sym<delimited_code>($/) {
        # TODO add numbered-alias handling
        my $config   := $<pod_configuration>.ast;
        my @contents := $<delimited_code_content>.ast;
        @contents    := Perl6::Pod::serialize_array(@contents).compile_time_value;
        make Perl6::Pod::serialize_object('Pod::Block::Code',
                                          :@contents,:$config).compile_time_value
    }

    method delimited_code_content($/) {
        my @contents := [];
        for $/[0] {
            if $_<pod_string> {
                nqp::splice(@contents,
                            Perl6::Pod::pod_strings_from_matches($_<pod_string>),
                            +@contents, 0);
                nqp::push(@contents, $*W.add_constant(
                    'Str', 'str', ~$_<pod_newline>
                ).compile_time_value);
            } else {
                @contents.push($*W.add_constant('Str', 'str', "\n").compile_time_value);
            }
        }
        make @contents;
    }

    method pod_block:sym<paragraph>($/) {
        if $<type>.Str ~~ /^defn/ {
            make Perl6::Pod::defn($/, $para-block);
        }
        else {
            make Perl6::Pod::any_block($/, $para-block);
        }
    }

    method pod_block:sym<paragraph_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<paragraph_table>($/) {
        make Perl6::Pod::table($/, $para-block);
    }

    method pod_block:sym<paragraph_code>($/) {
        # TODO make config via call to make_config in Pod.nqp
        my $config := $<pod_configuration>.ast;
        my @contents := [];
        for $<pod_line> {
            nqp::splice(@contents, $_.ast, +@contents, 0);
        }
        @contents  := Perl6::Pod::serialize_array(@contents).compile_time_value;
        make Perl6::Pod::serialize_object('Pod::Block::Code',
                                          :@contents,:$config).compile_time_value;
    }

    method pod_block:sym<abbreviated>($/) {
        if $<type>.Str ~~ /^defn/ {
            make Perl6::Pod::defn($/, $abbrev-block);
        }
        else {
            make Perl6::Pod::any_block($/, $abbrev-block);
        }
    }

    method pod_block:sym<abbreviated_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<abbreviated_table>($/) {
        make Perl6::Pod::table($/, $abbrev-block);
    }

    method pod_block:sym<abbreviated_code>($/) {
        my @contents := [];
        for $<pod_line> {
            nqp::splice(@contents, $_.ast, +@contents, 0);
        }
        @contents := Perl6::Pod::serialize_array(@contents).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :@contents
        ).compile_time_value
    }

    method pod_line ($/) {
        my @contents := Perl6::Pod::pod_strings_from_matches($<pod_string>);
        @contents.push($*W.add_constant(
            'Str', 'str', ~$<pod_newline>
        ).compile_time_value);
        make @contents;
    }

    method pod_block:sym<finish>($/) {
        $*W.install_lexical_symbol(
          $*UNIT,'$=finish', nqp::hllizefor(~$<finish>, 'Raku'));
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
        my @contents := Perl6::Pod::pod_strings_from_matches($<pod_string>);
        @contents    := Perl6::Pod::serialize_array(@contents).compile_time_value;
        make Perl6::Pod::serialize_object('Pod::Block::Para', :@contents).compile_time_value
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
                    #   !! $/.worry("\"$_\" is not a valid HTML5 entity.");
                } else {
                    my $n := $_<integer>
                          ?? $_<integer>.made
                          !! nqp::codepointfromname(~$_);
                    if $n >= 0 {
                        @contents.push(nqp::chr($n));
                        @meta.push($n);
                    } else {
                        $/.worry("\"$_\" is not a valid Unicode character name or code point.");
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
            my @chars := Perl6::Pod::build_pod_chars($<pod_string_character>);
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
            my @contents  := Perl6::Pod::build_pod_strings([@chars]);
            @contents := Perl6::Pod::serialize_array(@contents).compile_time_value;
            my $past := Perl6::Pod::serialize_object(
                'Pod::FormattingCode',
                :type($*W.add_string_constant(~$<code>).compile_time_value),
                :@contents,
                :meta(@meta),
            );
            make $past.compile_time_value;
        }
    }

    method pod_string($/) {
        make Perl6::Pod::build_pod_chars($<pod_string_character>);
    }

    method pod_balanced_braces($/) {
        if $<endtag> {
            my @chars := Perl6::Pod::build_pod_chars($<pod_string_character>);
            @chars.unshift(~$<start>);
            @chars.push(~$<endtag>);
            make @chars;
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



    #================================================================
    # end of class Perl6::Actions block
    #================================================================
}

class Perl6::QActions is HLL::Actions does STDActions {
    # This overrides NQP during the deprecation period for Unicode 1 names not covered by Alias Names
    method charname-panic($/) { $/.panic("Unrecognized character name [$/]") }
    method charname($/) {
        my $codepoint := $<integer>
                         ?? nqp::chr($<integer>.made)
                         !! nqp::strfromname(~$/);
        $codepoint := self.charname-notfound($/) if $codepoint eq '';
        make $codepoint;
    }
    method charname-notfound($/) {
        my @worry-text := ( "LINE FEED, NEW LINE, END OF LINE, LF, NL or EOL",
                            "FORM FEED or FF",
                            "CARRIAGE RETURN or CR",
                            "NEXT LINE or NEL" );
        my $text := "Deprecated character name [%s] in lookup of Unicode character by name.\n" ~
                    "Unicode 1 names are deprecated.\nPlease use %s";
        if ~$/ eq "LINE FEED (LF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[0]) ) );
            return nqp::strfromname("LINE FEED");
        }
        if ~$/ eq "FORM FEED (FF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[1]) ) );
            return nqp::strfromname("FORM FEED");
        }
        if ~$/ eq "CARRIAGE RETURN (CR)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[2]) ) );
            return nqp::strfromname("CARRIAGE RETURN");
        }
        if ~$/ eq "NEXT LINE (NEL)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[3]) ) );
            return nqp::strfromname("NEXT LINE");
        }

        self.charname-panic($/);
    }
    method nibbler($/) {
        my @asts;
        my $lastlit := '';
        my $atom;

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                if nqp::istype($_.ast, QAST::Node) {
                    if $lastlit ne '' {
                        @asts.push($*W.add_string_constant($lastlit));
                        $lastlit := '';
                    }
                    $atom := $_.ast.ann('ww_atom');
                    @asts.push(WANTED($_.ast, 'nibbler'));
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

        # make sure single var interpolation actually stringifies
        elsif +@asts == 1 && !$atom {
             @asts[0] :=
               QAST::Op.new( :op('callmethod'), :name('Stringy'), @asts[0] );
        }

        my $past := @asts.shift();
        for @asts {
            $past := QAST::Op.new( :op('call'), :name('&infix:<~>'), $past, $_ );
        }

        if nqp::can($/, 'postprocessors') {
            for $/.postprocessors -> $pp {
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
                        my $result := $*W.find_single_symbol('&val')($thisq.compile_time_value);
                        $*W.add_object_if_no_sc($result);
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
                my $result := $*W.find_single_symbol('&val')($qast.compile_time_value);
                $*W.add_object_if_no_sc($result);
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
            $past := QAST::Op.new(
                :op('callmethod'),
                :name('WORDS_AUTODEREF'),
                :node($/),
                $past
            );
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
                            :name('WORDS_AUTODEREF'),
                            :node($/),
                            QAST::Op.new(
                                :op('callmethod'),
                                :name('Stringy'),
                                $node
                            )
                        )
                    )
                );
            }
        }
        walk($past);

        # Strip out list op and possible Slip if only one resulting word
        nqp::elems($result) == 1
            ?? nqp::istype($result[0], QAST::Op) && $result[0].name eq 'Slip'
                ?? $result[0][0]
                !! $result[0]
            !! QAST::Stmts.new( $result );
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
        my str $nl := nqp::unbox_s($*W.find_single_symbol('$?NL'));
        if nqp::can($/, 'parsing_heredoc') {
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
        make nqp::can($/, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r") )
            !! "\r";
    }
    method backslash:sym<rn>($/) {
        make nqp::can($/, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r\n") )
            !! "\r\n";
    }
    method backslash:sym<t>($/) {
        make nqp::can($/, 'parsing_heredoc')
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
            WANTED(
                QAST::Op.new( :op('call'), block_closure($blast), :node($/) ),
                'escape{}'));
    }

    # The next three are currently only used for tr///.
    method escape:ch ($/)     { make ~$/; }
    method escape:sym<..>($/) { make ~$/; }
    method escape:ws ($/)     { make ~$/; }

    method escape:sym<$>($/) { make $<EXPR>.ast; }
    method escape:sym<@>($/) { make $<EXPR>.ast; }
    method escape:sym<%>($/) { make $<EXPR>.ast; }
    method escape:sym<&>($/) { make $<EXPR>.ast; }

    method escape:sym<'>($/) { make mark_ww_atom($<quote>.ast); }
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
                $/.panic("Quote words construct too complex to use in a regex");
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

                $/.typed_panic(
                  'X::Syntax::Regex::QuantifierValue', :empty-range
                ) if $min > $max;
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
            if nqp::eqat($varast.name, '$', 0) {
                my $constant;
                try {
                    my $found := $*W.find_single_symbol($varast.name);
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
            if nqp::istype($varast.returns, $*W.find_single_symbol('Str')) {
                make QAST::Regex.new(QAST::NodeList.new(
                        QAST::SVal.new( :value('!LITERAL') ),
                        $varast,
                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) )
                    ),
                    :rxtype<subrule>, :subtype<method>, :node($/));
                return;
            }
        }

        if $<var><sigil> eq '%' {
            $<var>.typed_panic('X::Syntax::Reserved', :reserved('use of hash variables in regexes'))
        }

        # Otherwise, slow path that checks what we have.
        make QAST::Regex.new(QAST::NodeList.new(
                QAST::SVal.new( :value('INTERPOLATE') ),
                $varast,
                QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                QAST::IVal.new( :value(monkey_see_no_eval($/) ?? 1 !! 0) ),
                QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                QAST::IVal.new( :value(0) ),
                QAST::Op.new( :op<callmethod>, :name<new>,
                    QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash'))),
                )
            ),
            :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<{ }>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE_ASSERTION') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/) ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash'))),
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
                    QAST::SVal.new( :value('INTERPOLATE_ASSERTION') ),
                    wanted($<var>.ast, 'assertvar2'),
                    QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/) ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash'))),
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
            if $<assertion> {
                if nqp::elems($lng.components()) > 1 {
                    $/.typed_panic('X::Syntax::Regex::Alias::LongName');
                }
                else {
                    # If ever implemented, take care with RESTRICTED
                    $/.typed_panic('X::Syntax::Reserved', :reserved('dynamic alias name in regex'));
                }
            }
            if nqp::elems($lng.components()) > 1 {
                # If ever implemented, take care with RESTRICTED
                $/.typed_panic('X::NYI', :feature('long dynamic name in regex assertion'));
            }
            if $*RESTRICTED {
                $/.typed_panic('X::SecurityPolicy::Eval', :payload($*RESTRICTED));
            }
            $qast := QAST::Regex.new( :rxtype<subrule>, :subtype<method>, :node($/),
                QAST::NodeList.new(QAST::SVal.new( :value('INDMETHOD') ), $lng.name_past()) );
        }
        else {
            my @parts := $lng.components();
            my $name  := @parts.pop();
            my $c     := $/;
            if $<assertion> {
                if nqp::elems(@parts) {
                    $c.typed_panic('X::Syntax::Regex::Alias::LongName');
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
                if $loc < 0 {
                    $c.panic('Can only use <sym> token in a proto regex')
                        if ($loc := nqp::index($fullrxname, ':sym«')) < 0;
                }
                my str $rxname := nqp::substr($fullrxname, $loc + 5, nqp::chars($fullrxname) - $loc - 6);
                $qast := QAST::Regex.new(:name('sym'), :rxtype<subcapture>, :node($/),
                    QAST::Regex.new(:rxtype<literal>, $rxname, :node($/)));
            }
            else {
                if nqp::elems(@parts) {
                    my $gref := QAST::WVal.new( :value($*W.find_symbol(@parts)) );
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new(
                                                QAST::SVal.new( :value('OTHERGRAMMAR') ),
                                                $gref, QAST::SVal.new( :value($name) )),
                                             :name(~$<longname>) );
                } elsif $*W.regex_in_scope('&' ~ $name) && nqp::substr($c.orig, $/.from - 1, 1) ne '.' {
                    # The lookbehind for . is because we do not yet call $~MAIN's methodop, and our recognizer for
                    # . <assertion>, which is a somewhat bogus recursion, comes from QRegex, not our own grammar.
                    my $coderef := $*W.find_single_symbol('&' ~ $name);
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
                    my $sub := $/.slang_actions('Regex').qbuildsub($nibbled, :anon(1), :addself(1));
                    $qast[0].push($sub);
                }
            }
        }
        make $qast;
    }

    method assertion:sym<~~>($/) {
        if $<num> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        elsif $<desigilname> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        else {
            make QAST::Regex.new( :rxtype<subrule>, :subtype<method>,
                QAST::NodeList.new(QAST::SVal.new( :value('RECURSE') )), :node($/) );
        }
    }

    method codeblock($/) {
        make QAST::Stmts.new(
            QAST::Op.new(
                :op('p6store'),
                QAST::Var.new( :name('$/'), :scope<lexical> ),
                QAST::Op.new(
                    QAST::Var.new( :name('$¢'), :scope<lexical> ),
                    :name('MATCH'),
                    :op('callmethod')
                )
            ),
            QAST::Op.new( :op<call>, block_closure($<block>.ast) )
        );
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
                    QAST::SVal.new( :value($*INTERPOLATION ?? 'INTERPOLATE_ASSERTION' !! 'INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/) ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*INTERPOLATION ?? 1 !! 0) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash'))),
                    ),
                 ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method p5metachar:sym<var>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value($*INTERPOLATION ?? 'INTERPOLATE_ASSERTION' !! 'INTERPOLATE') ),
                    wanted($<var>.ast, 'p5var'),
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/) ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*INTERPOLATION ?? 1 !! 0) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_single_symbol('PseudoStash'))),
                    ),
                 ),
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

# vim: expandtab sw=4
