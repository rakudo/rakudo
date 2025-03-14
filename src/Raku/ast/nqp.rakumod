# Create NQP ops without Call::Name roundabout
class RakuAST::Nqp
  is RakuAST::Expression
{
    has Str $.op;
    has RakuAST::ArgList $.args;

    method new(Str $op, *@args) {
        nqp::die('RakuAST::Nqp does not support nqp::const.  Use RakuAST::Nqp::Const')
          if $op eq 'const';

        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp, '$!op', $op);

        my $args;
        if nqp::elems(@args) == 1 {
            my $it := @args[0];
            if nqp::istype($it, RakuAST::ArgList) {
                nqp::bindattr($obj, RakuAST::Nqp, '$!args', $it);
                $args := nqp::getattr(@args[0], RakuAST::ArgList, '$!args');
            }
            elsif nqp::istype($it, List) {
                $it.elems;  # reify
                $obj.set-args($args := nqp::getattr($it, List, '$!reified'));
            }
            else {
                $obj.set-args($args := @args);
            }
        }
        else {
            $obj.set-args($args := @args);
        }

        # We want to make use of nqp ops as simple as possible, so
        # we automatically convert common types to their RakuAST
        # equivalents.
        my int $i;
        my int $n := nqp::elems($args);
        while $i < $n {
            my $arg := $args[$i];
            if nqp::istype($arg,Str) {
                $args[$i] := RakuAST::StrLiteral.new($arg);
            }
            elsif nqp::istype($arg,Int) {
                $args[$i] := RakuAST::IntLiteral.new($arg);
            }
            ++$i;
        }

        $obj
    }

    method needs-sink-call() { False }

    method set-args($args) {
        my $arglist := nqp::create(RakuAST::ArgList);
        nqp::bindattr($arglist, RakuAST::ArgList, '$!args', $args);
        nqp::bindattr(self, RakuAST::Nqp, '$!args', $arglist);
    }

    method visit-children(Code $visitor) {
        my @args := nqp::getattr(self.args, RakuAST::ArgList, '$!args');
        for @args {
            $visitor($_);
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $call := QAST::Op.new(:op($!op));
        $!args.IMPL-ADD-QAST-ARGS($context, $call);

        # We generally want to send unboxed string/int values in for dispatch
        # arguments (although leave normal ones alone); we can't really
        # know which are which, but if we're writing out an `nqp::op`
        # just assume that they should all be unboxed; most situations
        # will see the dispatch op generated anyway.
        my int $i;
        my int $n := nqp::elems($call.list);
        while $i < $n {
            my $arg := $call[$i];
            if nqp::istype($arg, QAST::Want)
              && ($arg[1] eq 'Ss' || $arg[1] eq 'Ii') {
                $call[$i] := $arg[2];
            }
            ++$i;
        }
        $call
    }
}

class RakuAST::Nqp::Const
  is RakuAST::Expression
{
    has Str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp::Const, '$!name', $name);
        $obj
    }

    method needs-sink-call() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<const>, :name($!name));
    }
}
