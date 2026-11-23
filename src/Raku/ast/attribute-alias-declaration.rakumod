# Represent an attribute without twigil (has $foo vs has $!foo)
class RakuAST::VarDeclaration::AttributeAlias
  is RakuAST::Node
  does RakuAST::VarDeclaration
{
    has str                             $.sigil;
    has RakuAST::Name                   $.desigilname;
    has RakuAST::VarDeclaration::Simple $.attribute;

    method new(
                                  str :$sigil!,
                        RakuAST::Name :$desigilname!,
      RakuAST::VarDeclaration::Simple :$attribute!
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,RakuAST::VarDeclaration::AttributeAlias,
          '$!sigil', $sigil
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::AttributeAlias,
          '$!desigilname', $desigilname
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::AttributeAlias,
          '$!attribute', $attribute
        );
        $obj.replace-scope('my');
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!desigilname);
    }

    method generate-lookup() {
        RakuAST::Var::Attribute.new(self.attribute-name);
    }

    method default-scope() { 'my' }
    method lexical-name()   { $!sigil       ~ $!desigilname.canonicalize }
    method attribute-name() { $!sigil ~ '!' ~ $!desigilname.canonicalize }

    method can-be-bound-to(--> Bool) {
        # stubbed-meta-object gives you the type object without trying to compose it first.
        my $package  := $!attribute.attribute-package.stubbed-meta-object;
        my str $name := self.attribute-name;
        $package.HOW.has_attribute($package, $name)
          ?? nqp::not_i(nqp::objprimspec(
               $package.HOW.get_attribute_for_usage($package, $name).type
             ))
          # So we don't have multi error for a non-existent attribute.
          !! 1
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<null>)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        my $package  := $!attribute.attribute-package.meta-object;
        my str $name := self.attribute-name;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            $context.ensure-sc($package);
            QAST::Var.new(
              :$name, :returns($attr-type),
              :scope(nqp::objprimspec($attr-type)
                ?? 'attributeref'
                !! 'attribute'
              ),
              QAST::Var.new(:name('self'), :scope<lexical>),
              QAST::WVal.new(:value($package))
            )
        }
        else {
            QAST::Op.new(:op<null>)
        }
    }

    method IMPL-BIND-QAST(
      RakuAST::IMPL::QASTContext $context,
                      QAST::Node $qast
    ) {
        my $package  := $!attribute.attribute-package.meta-object;
        my str $name := self.attribute-name;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type :=
              $package.HOW.get_attribute_for_usage($package, $name).type;
            unless nqp::eqaddr($attr-type, Mu) {
                $context.ensure-sc($attr-type);
                $qast := QAST::Op.new(:op<p6bindassert>,
                  $qast,
                  QAST::WVal.new(:value($attr-type))
                );
            }
            QAST::Op.new(:op<bind>,
              QAST::Var.new(
                :$name, :scope<attribute>, :returns($attr-type),
                QAST::Var.new(:name('self'), :scope<lexical>),
                QAST::WVal.new(:value($package)),
              ),
              $qast
            )
        }
        else {
            QAST::Op.new(:op<null>)
        }
    }
}
