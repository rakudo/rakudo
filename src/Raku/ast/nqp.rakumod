class RakuAST::Nqp
  is RakuAST::Node
{
    has Str $.op;
    has RakuAST::ArgList $.args;

    method new(Str $op, *@args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp, '$!op', $op);
        $obj.set-args(@args);
        $obj
    }

    method set-args(@args) {
        my $arglist := nqp::create(RakuAST::ArgList);
        nqp::bindattr($arglist, RakuAST::ArgList, '$!args', @args);
        nqp::bindattr(self, RakuAST::Nqp, '$!args', $arglist),
    }

    method visit-children(Code $visitor) {
        my @args := nqp::getattr(
          nqp::getattr(self, RakuAST::Nqp, '$!args'), RakuAST::ArgList, '$!args'
        );
        for @args {
            $visitor($_);
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::die('RakuAST::Nqp does not support nqp::const.  Use RakuAST::Nqp::Const')
          if $!op eq 'const';

        my $call := QAST::Op.new(:op($!op));
        my @args := nqp::getattr($!args, RakuAST::ArgList, '$!args');

        # We want to make use of nqp ops as simple as possible, so
        # we automatically convert common types to their RakuAST
        # equivalents.
        my int $i := 0;
        my int $n := nqp::elems(@args);
        while $i < $n {
            my $arg := @args[$i];
            if nqp::istype($arg,Match) {
                @args[$i] := RakuAST::StrLiteral.new(~$arg);
            }
            elsif nqp::istype($arg,Str) {
                @args[$i] := RakuAST::StrLiteral.new($arg);
            }
            elsif nqp::istype($arg,Int) {
                @args[$i] := RakuAST::IntLiteral.new($arg);
            }
            ++$i;
        }

        # We generally want to send unboxed string/int values in for dispatch
        # arguments (although leave normal ones alone); we can't really
        # know which are which, but if we're writing out an `nqp::op`
        # just assume that they should all be unboxed; most situations
        # will see the dispatch op generated anyway.
        $!args.IMPL-ADD-QAST-ARGS($context, $call);
        $i := 0;
        $n := nqp::elems($call.list);
        while $i < $n {
            my $arg := $call[$i];
            if nqp::istype($arg, QAST::Want)
              && ($arg[1] eq 'Ss' || $arg[1] eq 'Ii') {
                $call[$i] := $arg[2];
            }
            $i++;
        }
        $call
    }
}

class RakuAST::Nqp::Const
  is RakuAST::Node
{
    has Str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp::Const, '$!name', $name);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<const>, :name($!name));
    }
    method IMPL-CURRIED() { False }
}
