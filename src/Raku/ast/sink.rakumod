# Marks nodes that are "sink boundaries": they are entry points to sinking, and
# we don't walk into them when we are doing an outer sink. The compiler will
# trigger sinking after it has parsed routines or the top-level compilation unit;
# check time will make sure that a sink boundary has
class RakuAST::SinkBoundary
  is RakuAST::Node
{
    has int $!sink-calculated;

    # Calculates the sink for this bounded unit.
    method calculate-sink() {
        unless $!sink-calculated {
            self.get-boundary-sink-propagator().propagate-sink(self.is-boundary-sunk(), :has-block-parent);
            nqp::bindattr_i(self, RakuAST::SinkBoundary, '$!sink-calculated', 1);
        }
        Nil
    }

    # Checks if sink context has been calculated for this sink unit.
    method sink-calculated() {
        $!sink-calculated ?? True !! False
    }

    # Returns True if we should sink even the final statement of this sink unit,
    # and False if not. For example, for a compilation unit, this would be True
    # in the case of a normal compilation, but in an EVAL or REPL context we'd
    # want the result. For routines, a `--> Nil` in the signature would produce
    # True here, but otherwise their final statement is not sunk.
    method is-boundary-sunk() {
        nqp::die('Missing is-boundary-sunk implementation')
    }

    # Gets the top-level sink propagator.
    method get-boundary-sink-propagator() {
        nqp::die('Missing get-boundary-sink-propagator implementation')
    }
}

# Marks things that know how to propagate sink context inside of themselves.
# They may introduce sinking of their own, propagate-sink will be called even
# if they are not in sink context.
class RakuAST::SinkPropagator
  is RakuAST::Node
{
    method propagate-sink(Bool $is-sunk) {
        nqp::die('Missing propagate-sink');
    }
}

# Marks nodes that want to know if they are sunk, because they will produce code
# or warnings differently if they are.
class RakuAST::Sinkable
  is RakuAST::Node
{
    has int $!sunk;

    method mark-sunk() {
        nqp::bindattr_i(self, RakuAST::Sinkable, '$!sunk', 1);
    }

    method sunk() { $!sunk ?? True !! False }

    # Things that take care of their own sinking do not need us to call
    # the sink method on them
    method needs-sink-call() { False }
}

# Marks nodes that want to know if they are block-level statements or not.
# This is used for loops, which at statement level produce Nil even if not
# sunk.
class RakuAST::BlockStatementSensitive
  is RakuAST::Node
{
    has int $!block-statement;

    method mark-block-statement() {
        nqp::bindattr_i(self, RakuAST::BlockStatementSensitive, '$!block-statement', 1);
    }

    method is-block-statement() { $!block-statement ?? True !! False }
}
