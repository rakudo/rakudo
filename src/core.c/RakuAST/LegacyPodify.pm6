# This file contains the default class for turning RakUAST::Doc::xxx
# classes into legacy pod6 objects.

class RakuAST::LegacyPodify {

    # basically mangle text to just single spaces
    my sub sanitize(Str:D $string --> Str:D) {
        $string eq "\n"
          ?? ' '
          !! $string
               .subst(/ \n+ $/)
               .subst("\n",  ' ', :global)
               .subst(/\s+/, ' ', :global)
    }

    # hide the outer markup
    my sub hide(RakuAST::Doc::Markup:D $markup) {
        my @atoms = $markup.atoms;
        given @atoms.head {
            nqp::istype($_,Str)
              ?? ($_ = $markup.opener ~ $_)
              !! @atoms.unshift($markup.opener)
        }
        given @atoms.tail {
            nqp::istype($_,Str)
              ?? ($_ = $_ ~ $markup.closer)
              !! @atoms.posh($markup.closer)
        }
        nqp::istype(@atoms.are,Str)
          ?? @atoms.join
          !! @atoms.map({ nqp::istype($_,Str) ?? $_ !! .podify }).Slip
    }

    # flatten the markup into a string, needed for V<>
    my sub flatten(RakuAST::Doc::Markup:D $markup, :$render --> Str:D) {
        my str @parts;
        for $markup.atoms {
            @parts.push: nqp::isstr($_) ?? $_ !! flatten($_, :render);
        }

        # V<> inside V<> *are* rendered
        if $render {
            @parts.unshift: '<';
            @parts.unshift: $markup.letter;
            @parts.push: '>';
        }

        nqp::join('',@parts)
    }

    # produce list without last if last is \n
    my sub no-last-nl(\list) {
        my @parts = list;
        @parts.pop if nqp::istype($_,Str) && $_ eq "\n" given @parts.tail;
        @parts
    }

    # create podified contents for atoms
    method !contentify-atoms($ast) {
        my str @parts;
        my @atoms = $ast.atoms.map({
            nqp::istype($_,Str) ?? $_ !! .podify  # may Slip
        }).map({

            # collect any strings
            if nqp::istype($_,Str) {
                @parts.push: $_;
                Empty
            }

            # something else, produce with any strings preceding
            elsif @parts {
                my str $string = @parts.join;
                @parts = ();
                ($string, $_).Slip
            }

            # just produce, already podified
            else {
                $_
            }
        });

        # collect any uncollected strings so far
        @atoms.push: @parts.join if @parts;

        # string at left needs to be trimmed left
        if @atoms.head <-> $_ {
            $_ = .trim-leading if nqp::istype($_,Str);
        }

        # return strings if just strings
        nqp::istype(@atoms.are,Str) ?? @atoms.join !! @atoms
    }

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
        my str $letter = $ast.letter;
        $letter eq ""
          ?? hide($ast)
          !! $letter eq 'V'
            ?? flatten($ast)
            !! Pod::FormattingCode.new(
                 type     => $letter,
                 meta     => $ast.meta,
                 contents => self!contentify-atoms($ast)
               )
    }

    multi method podify(RakuAST::Doc::Paragraph:D $ast) {
        Pod::Block::Para.new(
          contents => no-last-nl($ast.atoms).map({
              nqp::istype($_,Str)
                ?? sanitize($_) || Empty
                !! self.podify($_)
          }).Slip
        )
    }

    multi method podify(RakuAST::Doc::Block:D $ast) {
        my str $type  = $ast.type;
        my str $level = $ast.level;

        # these need code of its own, as the new grammar only collects
        # and does not do any interpretation
        unless $level {
            return self.podify-table($ast)
              if $type eq 'table';
            return self.podify-code($ast)
              if $type eq 'code' | 'input' | 'output';
            return self.podify-defn($ast)
              if $type eq 'defn';
        }

        my $config   := $ast.config;
        my $contents := no-last-nl($ast.paragraphs).map({
            if nqp::istype($_,Str) {
                if sanitize(.trim-leading) -> $contents {
                    Pod::Block::Para.new(:$contents)
                }
            }
            else {
                self.podify($_)
            }
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
                ?? Pod::Block::Comment.new(
                     :$config, :contents([$ast.paragraphs.head])
                   )
                !! Pod::Block::Named.new(:name($type), :$config, :$contents)
          !! $contents  # no type means just a string
    }

    proto method podify-table(|) {*}
    multi method podify-table(|) {  # placeholder for now
        X::NYI.new(feature => "legacy pod support for =table").throw;
    }

    method podify-code(RakuAST::Doc::Block:D $ast) {
        my $contents := $ast.paragraphs.head;

        $contents := nqp::istype($contents,Str)
          ?? no-last-nl($contents.split("\n", :v, :skip-empty)).List
          # assume a paragraph with string / markup atoms
          !! $contents.atoms.map({
                nqp::istype($_,Str)
                  ?? .split("\n", :v, :skip-empty).Slip
                  !! .podify
            }).List;

        Pod::Block::Code.new: :$contents, :config($ast.config)
    }

    method podify-defn(RakuAST::Doc::Block:D $ast) {
        my @paragraphs = $ast.paragraphs;

        my $first := @paragraphs.shift;
        $first    := $first.atoms.map(*.Str).join
          unless nqp::istype($first,Str);
        my ($term, $para) = $first.split("\n",2).map(&sanitize);

        my @contents = Pod::Block::Para.new(:contents($para));
        for @paragraphs {
            @contents.push: nqp::istype($_,Str)
              ?? Pod::Block::Para.new(:contents(.chomp))
              !! .podify
        }

        Pod::Defn.new: :$term, :@contents, :config($ast.config)
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
