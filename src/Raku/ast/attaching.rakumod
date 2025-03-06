# Done by any AST node that is the target for being attached to by another node.
# An attaching AST node is one that wants to be somehow attached to a parent
# element, because it has a semantic relationship with it. For example, a
# a (has-scoped) method will want to attach to the immediately enclosing
# package, while a placeholder parameter like $^a will want to attach to the
# nearest scope that can carry a signature. Attachment happens at parse time
# or BEGIN time.
class RakuAST::AttachTarget
  is RakuAST::Node
{
    # Expected to return a (possibly empty) List of attach target names
    # for this node.  Must be supplied by the consuming class.
    method attach-target-names() {
        nqp::die('attach-target-names not implemented for ' ~ self.HOW.name(self));
    }

    # Expected to clear any existing attachments, so we don't attach
    # things more than once.  Must be supplied by the consuming class.
    method clear-attachments() {
        nqp::die('clear-attachments not implemented for ' ~ self.HOW.name(self));
    }
}

# Fake up an attach target for an existing package object
class RakuAST::Declaration::External::Package
  is RakuAST::Declaration::External::Constant
  is RakuAST::AttachTarget
{
    method attach-target-names() {
        ['package']
    }

    method clear-attachments() {
    }

    method declarator() {
        my $how := self.compile-time-value.HOW;
        nqp::istype($how, Perl6::Metamodel::PackageHOW)
            ?? 'package'
            !! nqp::istype($how, Perl6::Metamodel::ModuleHOW)
                ?? 'module'
                !! nqp::istype($how, Perl6::Metamodel::RoleHOW)
                    ?? 'role'
                    !! nqp::istype($how, Perl6::Metamodel::ClassHOW)
                        ?? 'class'
                        !! nqp::die("Unexpected HOW: " ~ $how.HOW.name($how));
    }

    method stubbed-meta-object() {
        self.compile-time-value
    }

    method meta-object() {
        self.compile-time-value
    }

    method can-have-methods() {
        nqp::istype(self.compile-time-value.HOW, Perl6::Metamodel::MethodContainer)
    }

    method can-have-attributes() {
        False
    }

    method attribute-type() {
        Attribute
    }

    method ATTACH-METHOD(RakuAST::Method $method) {
        my $type := self.compile-time-value;
        my $how := $type.HOW;
        my $name := $method.name.canonicalize;
        my $meta-object := $method.meta-object;

        if nqp::istype($method, RakuAST::Method) && $method.private {
            $how.add_private_method($type, $name, $meta-object);
        }
        elsif nqp::istype($method, RakuAST::Method) && $method.meta {
            $how.add_meta_method($type, $name, $meta-object);
        }
        elsif $method.multiness eq 'multi' {
            $how.add_multi_method($type, $name, $meta-object);
        }
        else {
            $how.add_method($type, $name, $meta-object);
        }
    }
}
