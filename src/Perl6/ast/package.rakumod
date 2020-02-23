class RakuAST::Package is RakuAST::StubbyMeta is RakuAST::Term is RakuAST::LexicalScope {
    has Str $.package-declarator;
    has Mu $.how;
    has Str $.name;
    has Str $.repr;

    method new(Str :$package-declarator!, Mu :$how!, Str :$name, Str :$repr) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Package, '$!package-declarator', $package-declarator);
        nqp::bindattr($obj, RakuAST::Package, '$!how', $how);
        nqp::bindattr($obj, RakuAST::Package, '$!name', $name // Str);
        nqp::bindattr($obj, RakuAST::Package, '$!repr', $repr // Str);
        $obj
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        # Create the type object and return it; this stubs the type.
        my %options;
        %options<name> := $!name if $!name;
        %options<repr> := $!repr if $!repr;
        $!how.new_type(|%options)
    }

    method PRODUCE-META-OBJECT() {
        # Obtain the stubbed meta-object, which is the type object.
        my $type := self.stubbed-meta-object();

        # Compose the meta-object and return it.
        $type.HOW.compose($type);
        $type
    }
}
