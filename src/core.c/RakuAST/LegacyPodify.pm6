# This file contains the default class for turning RakUAST::Doc::xxx
# classes into legacy pod6 objects.

class RakuAST::LegacyPodify {

    proto method podify(|) {*}

    # Base class catcher
    multi method podify(RakuAST::Doc:D $ast) {
        NYI("Podifying $ast.^name() objects").throw
    }

    # Odd value catcher, avoiding long dispatch options in error message
    multi method podify(Mu:D $ast) {
        die "You cannot podify a $ast.^name() instance: $ast.raku()";
    }
    multi method podify(Mu:U $ast) {
        die "You cannot podify a $ast.^name() type object";
    }

    multi method podify(RakuAST::Doc::Markup:D $ast) {
        Pod::FormattingCode.new(
          type     => $ast.letter,
          meta     => $ast.meta.map(*.Array),
          contents => $ast.atoms.map({
              nqp::istype($_,RakuAST::Doc::Markup)
                ?? self.podify($_)
                !! $_
          }).List
        )
    }

    multi method podify(RakuAST::Doc::Paragraph:D $ast) {
        Pod::Block::Para.new(
          contents => $ast.atoms.map({
              nqp::istype($_,Str)
                ?? .lines.map(*.words.join(' ')).join(' ').trim
                !! self.podify($_)
          }).Slip
        )
    }

    multi method podify(RakuAST::Doc::Block:D $ast) {
        my str $type  = $ast.type;
        my str $level = $ast.level;

        # this needs code of its own, as the new grammar only collects
        # and does not do any interpretation
        return self.podify-table($ast) if $type eq 'table' and !$level;

        my $config   := $ast.config;
        my $contents := $ast.paragraphs.map({
            nqp::istype($_,Str)
              ?? .lines.join(' ').trim
              !! self.podify($_)
        }).List;

        $type
          ?? $type eq 'item'
            ?? Pod::Item.new(
                 level => $level ?? $level.Int !! 1, :$config, :$contents
               )
            !! $level
              ?? $type eq 'head'
                ?? Pod::Heading.new(:level($level.Int), :$config, :$contents)
                !! Pod::Block::Named.new(
                     :name($type ~ $level), :$config, :$contents
                   )
              # from here on without level
              !! $type eq 'comment'
                ?? Pod::Block::Comment.new(:contents([$ast.paragraphs.head]))
                !! $type eq 'input' | 'output' | 'code'
                  ?? Pod::Block::Code.new(:contents([
                       $ast.paragraphs.head.split("\n", :v, :skip-empty)
                     ]))
                  !! Pod::Block::Named.new(:name($type), :$config, :$contents)
          !! $contents  # no type means just a string
    }

    method podify-table(RakuAST::Doc::Block:D $ast) {
        X::NYI.new(feature => "legacy pod support for =table").throw;
    }

    multi method podify(RakuAST::Doc::Declarator:D $ast, $WHEREFORE) {
        sub normalize(@paragraphs) {
            @paragraphs.map(*.lines.map({.trim if $_}).Slip).join(' ')
        }
        my $pod := Pod::Block::Declarator.new(
          WHEREFORE => $WHEREFORE,
          leading   => [%*ENV<RAKUDO_POD_DECL_BLOCK_USER_FORMAT>
            ?? $ast.leading.join("\n")
            !! normalize($ast.leading)
          ],
          trailing  => [[normalize $ast.trailing],]
        );
        $WHEREFORE.set_why($pod);
        $pod
    }
}

# vim: expandtab shiftwidth=4
