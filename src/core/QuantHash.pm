my role QuantHash does Associative {
    method Int     ( --> Int)     { self.total.Int }
    method Num     ( --> Num)     { self.total.Num }
    method Numeric ( --> Numeric) { self.total.Numeric }
    method Real    ( --> Real)    { self.total.Real }
}
