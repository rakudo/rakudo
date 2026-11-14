# Represent an attribute without twigil (has $foo vs has $!foo)
class RakuAST::VarDeclaration::AttributeAlias
  is RakuAST::Node
  does RakuAST::VarDeclaration
{
    has RakuAST::Name $.desigilname;
    has str $.sigil;
    has RakuAST::VarDeclaration::Simple $.attribute;

    method new(RakuAST::Name :$desigilname!, str :$sigil!, RakuAST::VarDeclaration::Simple :$attribute!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::AttributeAlias, '$!desigilname', $desigilname);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::AttributeAlias, '$!sigil', $sigil);
        nqp::bindattr($obj, RakuAST::VarDeclaration::AttributeAlias, '$!attribute', $attribute);
        $obj.replace-scope('my');
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!desigilname);
    }

    method generate-lookup() {
        RakuAST::Var::Attribute.new(self.IMPL-ATTRIBUTE-NAME);
    }

    method lexical-name() {
        $!sigil ~ $!desigilname.canonicalize
    }

    method default-scope() {
        'my'
    }

    method can-be-bound-to() {
        # stubbed-meta-object gives you the type object without trying to compose it first.
        my $package := $!attribute.attribute-package.stubbed-meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            return nqp::objprimspec($attr-type) ?? False !! True
        }

        # Return True so we don't have multi error for a non-existent attribute.
        True
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<null>)
    }

    method IMPL-ATTRIBUTE-NAME() {
        $!sigil ~ '!' ~ $!desigilname.canonicalize
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        my $package := $!attribute.attribute-package.meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            $context.ensure-sc($package);
            return QAST::Var.new(
                :scope(nqp::objprimspec($attr-type) ?? 'attributeref' !! 'attribute'),
                :name($name), :returns($attr-type),
                QAST::Var.new(:scope('lexical'), :name('self')),
                QAST::WVal.new( :value($package) ),
            )
        }
        QAST::Op.new(:op<null>)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        my $package := $!attribute.attribute-package.meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            unless nqp::eqaddr($attr-type, Mu) {
                $context.ensure-sc($attr-type);
                $source-qast := QAST::Op.new(
                    :op('p6bindassert'),
                    $source-qast,
                    QAST::WVal.new( :value($attr-type) )
                );
            }
            return QAST::Op.new(
                :op('bind'),
                QAST::Var.new(
                    :scope('attribute'), :name($name), :returns($attr-type),
                    QAST::Var.new(:scope('lexical'), :name('self')),
                    QAST::WVal.new( :value($package) ),
                ),
                $source-qast
            )
        }
        QAST::Op.new(:op<null>)
    }
}
