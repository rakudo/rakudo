# Done by every AST node that can report CHECK-time problems.
class RakuAST::CheckTime is RakuAST::Node {
    # A list of sorries, lazily allocated if there are any.
    has Mu $!sorries;

    # A list of worries, lazily allocated if there are any.
    has Mu $!worries;

    # Clears any CHECK-time problems that have been produced
    method clear-check-time-problems() {
        nqp::bindattr(self, RakuAST::CheckTime, '$!sorries', Mu);
        nqp::bindattr(self, RakuAST::CheckTime, '$!worries', Mu);
        Nil
    }

    # Returns True if any check-time problems (sorries or worries) have been
    # identified, and False otherwise.
    method has-check-time-problems() {
        $!sorries || $!worries ?? True !! False
    }

    # Get a list of any sorry-level check-time problems.
    method sorries() {
        self.IMPL-WRAP-LIST($!sorries // [])
    }

    # Get a list of any worry-level check-time problems.
    method worries() {
        self.IMPL-WRAP-LIST($!worries // [])
    }

    # Add a sorry check-time problem (which will produce a SORRY output in the
    # compiler).
    method add-sorry(Any $exception) {
        unless $!sorries {
            nqp::bindattr(self, RakuAST::CheckTime, '$!sorries', []);
        }
        nqp::push($!sorries, $exception);
        Nil
    }

    # Add a worry check-time problem (which will produce a potential difficulties
    # output in the compiler).
    method add-worry(Any $exception) {
        unless $!worries {
            nqp::bindattr(self, RakuAST::CheckTime, '$!worries', []);
        }
        nqp::push($!worries, $exception);
        Nil
    }

    # Method to be implemented by nodes that perform CHECK-time checks. Should
    # call add-sorry and add-worry with the constructed exception objects.
    method PERFORM-CHECK(RakuAST::Resolver $resolver) {
        nqp::die('Missing PERFORM-CHECK implementation for ' ~ self.HOW.name(self));
    }
}
