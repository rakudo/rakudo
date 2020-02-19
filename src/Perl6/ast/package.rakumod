class RakuAST::Package is RakuAST::Meta {
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

    method PRODUCE-META-OBJECT() {
        # Create the meta-object.
        my %options;
        %options<name> := $!name if $!name;
        %options<repr> := $!repr if $!repr;
        my $type := $!how.new_type(|%options);

        # Compose the meta-object and return it.
        $type.HOW.compose($type);
        $type
    }
}
