# This is a proof-of-concept renderer of safe RakuDoc, based on a given
# RakuAST tree.
#
# Most common way is to use the --rakudoc commandline option:
#
# $ RAKUDO_RAKUAST=1 raku --rakudoc filename.raku
#
# Alternately, one can call this as a class method with a given AST on the
# RakuDoc::To::SafeRakuDoc class:
#
# use RakuDoc::To::RakuDoc;
# say RakuDoc::To::RakuDoc.render($ast)
#
# Note that a RakuAST of a source / documentation file can easily be
# obtained as follows:
#
# my $ast = $filename.IO.slurp.AST;

use v6.e.PREVIEW;

unit class RakuDoc::To::RakuDoc;

#- render ----------------------------------------------------------------------

multi method render(RakuAST::CompUnit:D $ast) {
    self.render($ast.statement-list.statements)
}

multi method render(RakuAST::StatementList:D $ast) {
    self.render($ast.statements)
}

multi method render(@ast) {
    my $*SPACES = "";
    my %*SEEN;
    self!inspect(@ast)
}

method !inspect(@ast) {

    sub handle($_) {
        when RakuAST::Statement::Expression {
            handle(.expression)
        }
        when RakuAST::Doc::Declarator {
            self!render(.WHEREFORE, $_)
        }
        when RakuAST::Doc {
            .DEPARSE
        }
        default {
            if .WHY -> $WHY {
                self!render($_, $WHY)
            }
        }
    }

    @ast.map(&handle).join.chomp
}

method !render($_, $WHY) {
    if !%*SEEN{.WHICH}++ && $WHY {
        when RakuAST::Block {
            self!render-block('', $_, $WHY)
        }
        when RakuAST::Submethod {  # must precede RakuAST::Method
            self!render-routine('submethod', $_, $WHY)
        }
        when RakuAST::Method {
            self!render-routine('method', $_, $WHY)
        }
        when RakuAST::Package {
            self!render-package($_, $WHY)
        }
        when RakuAST::Parameter {
            self!render-parameter($_, $WHY)
        }
        when RakuAST::PointyBlock {
            self!render-block('pointy-', $_, $WHY)
        }
        when RakuAST::RegexDeclaration {
            self!render-routine(.declarator, $_, $WHY)
        }
        when RakuAST::Sub {
            self!render-routine('sub', $_, $WHY)
        }
        when RakuAST::Type::Enum {
            self!render-enum($_, $WHY)
        }
        when RakuAST::Type::Subset {
            self!render-subset($_, $WHY)
        }
        when RakuAST::VarDeclaration::Simple {
            self!render-variable($_, $WHY)
        }
        default {
            note "Unexpected type '$_.^name()' with declarator doc, ignoring";
            ""
        }
    }
    else {
        ""
    }
}

#- block -----------------------------------------------------------------------

method !render-block(str $kind, $_, $WHY) {
    my str $type  = "doc-{$kind}block";
    my str @parts = "$*SPACES=begin $type\n";

    $*SPACES ~= "  ";
    @parts.push("$*SPACES$_") with $WHY.leading;
    @parts.push("$*SPACES$_") with $WHY.trailing;
    @parts.push("\n");

    @parts.push(self!inspect($_)) with .rakudoc;

    $*SPACES .= chop(2);
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- enum ------------------------------------------------------------------------

method !render-enum($_, $WHY) {
    my str $type  = "doc-enum";
    my     $meta  = .meta-object;
    my str @parts = "$*SPACES=begin $type :name<$meta.raku()>\n";

    @parts.push("$*SPACES  $_") with $WHY.leading;
    @parts.push("$*SPACES  $_") with $WHY.trailing;
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- package ---------------------------------------------------------------------

method !render-package($_, $WHY) {
    my str $type  = "doc-$_.declarator()";
    my str @parts = "$*SPACES=begin $type :name<$_.name.canonicalize()>\n";

    $*SPACES ~= "  ";
    @parts.push("$*SPACES$_") with $WHY.leading;
    @parts.push("$*SPACES$_") with $WHY.trailing;
    @parts.push("\n");

    @parts.push(self!inspect($_)) with .rakudoc;

    $*SPACES .= chop(2);
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- parameter -------------------------------------------------------------------

method !render-parameter($_, $WHY) {
    my str $type  = "doc-parameter";
    my     $meta  = .meta-object;
    my str @parts = "$*SPACES=begin $type :name<$meta.raku()>\n";

    @parts.push("$*SPACES  $_") with $WHY.leading;
    @parts.push("$*SPACES  $_") with $WHY.trailing;
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- routine ---------------------------------------------------------------------

method !render-routine(str $kind, $_, $WHY) {
    my str $type  = "doc-$kind";
    my str $multi = .multiness
      ?? " :$_.multiness()"
      !! "";
    my str @parts = "$*SPACES=begin $type$multi :name<$_.name.canonicalize()>\n";

    $*SPACES ~= "  ";
    @parts.push("$*SPACES$_") with $WHY.leading;
    @parts.push("$*SPACES$_") with $WHY.trailing;
    @parts.push("\n");

    @parts.push(self!inspect($_)) with .rakudoc;

    $*SPACES .= chop(2);
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- subset ----------------------------------------------------------------------

method !render-subset($_, $WHY) {
    my str $type  = "doc-subset";
    my     $meta  = .meta-object;
    my str @parts = "$*SPACES=begin $type :name<$meta.raku()>\n";

    @parts.push("$*SPACES  $_") with $WHY.leading;
    @parts.push("$*SPACES  $_") with $WHY.trailing;
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

#- variable --------------------------------------------------------------------

method !render-variable($_, $WHY) {
    my str $scope = .scope;
    my str $type  = $scope.lc eq 'has' ?? "doc-attribute" !! "doc-variable";
    my str @parts =
      "$*SPACES=begin $type :name<$_.name()>, :scope<$scope>\n";

    $*SPACES ~= "  ";
    @parts.push("$*SPACES$_") with $WHY.leading;
    @parts.push("$*SPACES$_") with $WHY.trailing;

    @parts.push(self!inspect($_)) with .rakudoc;

    $*SPACES .= chop(2);
    @parts.push("$*SPACES=end $type\n\n");
    @parts.join
}

# vim: expandtab shiftwidth=4
