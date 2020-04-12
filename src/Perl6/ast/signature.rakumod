# A signature, typically part of a block though also contained within a
# signature literal or a signature-based variable declarator.
class RakuAST::Signature is RakuAST::Meta is RakuAST::ImplicitLookups {
    has List $.parameters;

    method new(List :$parameters!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Signature, '$!parameters', $parameters);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Simple.new('Signature'),
        ])
    }

    method PRODUCE-META-OBJECT() {
        # Produce meta-objects for each parameter.
        my @parameters;
        for self.IMPL-UNWRAP-LIST($!parameters) {
            @parameters.push($_.meta-object);
        }

        # Build signature object.
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $signature-type := @lookups[0].resolution.compile-time-value;
        my $signature := nqp::create($signature-type);
        nqp::bindattr($signature, $signature-type, '@!params', @parameters);
        $signature
    }

    method visit-children(Code $visitor) {
        for self.IMPL-UNWRAP-LIST($!parameters) {
            $visitor($_);
        }
    }
}
