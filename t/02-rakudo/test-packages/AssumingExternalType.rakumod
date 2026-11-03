unit class AssumingExternalType;

has Int $.value;

method new(Int $value) { self.bless(:$value) }
