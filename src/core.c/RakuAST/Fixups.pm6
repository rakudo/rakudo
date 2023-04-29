# This file contains augmentations to classes that are created in the
# RakuAST bootstrap to allow a lot of logic (which will **NOT** be
# needed to compile the Raku setting) to be written in Raku rather
# than in NQP.

# The "make-legacy-pod" methods will make a legacy compatible Pod
# object, including all of its weird nesting and use of mutable
# Arrays rather than immutable Lists

#=foo B<bar> L<C<foo>|zippo>>
#
#Pod::Block::Named.new(
#  name => "foo",
#  config => {},
#  contents => [
#    Pod::Block::Para.new(
#      config => {},
#      contents => [
#        Pod::FormattingCode.new(type => "B", meta => [], config => {}, contents => ["bar"]),
#        " ",
#        Pod::FormattingCode.new(type => "L", meta => ["zippo"], config => {}, contents => [
#          Pod::FormattingCode.new(type => "C", meta => [], config => {}, contents => ["foo"])
#        ]),
#        ">"
#      ]
#    )
#  ]
#);
augment class RakuAST::Doc::Markup {

    method make-legacy-pod(RakuAST::Doc::Markup:D:) {
        Pod::FormattingCode.new(
          type     => self.letter,
          meta     => self.meta,
          contents => self.atoms.map({
              nqp::istype($_,RakuAST::Doc::Markup)
                ?? .make-legacy-pod
                !! $_
          }).List
        )
    }

    # convert the contents of E<> to a codepoint
    method convert-entity(RakuAST::Doc::Markup: Str:D $entity) {
        my $codepoint := val $entity;

        my $string;
        if nqp::not_i(nqp::istype($codepoint,Allomorph)) {  # not numeric
            $string := RakuAST::HTML::Entities.parse($entity);
            unless $string {
                my $worry := qq/"$entity" is not a valid HTML5 entity./;
                if $*RESOLVER -> $resolver {
                    self.worry-ad-hoc($resolver, $worry);
                }
                else {
                    warn $worry;
                }
            }
        }
        elsif try $codepoint.chr -> $chr {
            $string := $chr;
        }
        else {
            $string := '';
            my $sorry :=
              "Codepoint $codepoint ($codepoint.base(16)) is out of bounds in E<>";
            if $*RESOLVER -> $resolver {
                self.sorry-ad-hoc($resolver, $sorry);
            }
            else {
                die $sorry;
            }
        }
        $string
    }

    # set up meta info from the last atom as appropriate
    method check-meta(RakuAST::Doc::Markup:D:) {
        my str $letter = self.letter;
        if $letter eq 'L' | 'E' {
            my @atoms = self.atoms;
            if nqp::istype(@atoms.tail,Str) {
                if $letter eq 'L' {
                    my ($str,$uri) = @atoms.tail.split('|',2);
                    self.set-meta($uri) if $uri;
                    $str
                      ?? (@atoms.tail = $str)
                      !! @atoms.pop;
                    self.set-atoms(@atoms.List);
                }
                else { # $letter eq 'E'
                    my $str = self.convert-entity(my $meta := @atoms.pop);
                    self.set-atoms($str);
                    self.set-meta($meta);
                }
            }
        }
    }
}

augment class RakuAST::Doc::Paragraph {
    method make-legacy-pod(RakuAST::Doc::Paragraph:D:) {
        Pod::Block::Para.new(
          contents => self.atoms.map({
              nqp::istype($_,Str)
                ?? .lines.join(' ').trim
                !! .make-legacy-pod
          }).Slip
        )
    }

    # easy integer checks
    my int32 $A     = nqp::ord('A');
    my int32 $Z     = nqp::ord('Z');
    my int32 $open  = nqp::ord('<');
    my int32 $close = nqp::ord('>');

    # create object from string, parsing any markup sequences
    method from-string(RakuAST::Doc::Paragraph:U: Str:D $string) {
        my int32 $this;       # the current grapheme
        my int32 $prev;       # the previous grapheme
        my int32 @codes;      # the graphemes of the given string
        my int32 @graphemes;  # graphemes collected so far

        my $paragraph := RakuAST::Doc::Paragraph.new;
        my $markups   := nqp::list;  # stack of Markup objects
        my $current;                 # current Markup object

        nqp::strtocodes($string,nqp::const::NORMALIZE_NFC,@codes);
        my int $i     = -1;
        my int $elems = nqp::elems(@codes);

        # return the object currently collecting
        sub collector() {
            nqp::elems($markups)
              ?? nqp::atpos($markups,nqp::sub_i(nqp::elems($markups),1))
              !! $paragraph
        }

        # add collected graphemes to given object, if any, and reset
        sub add-graphemes($markup --> Nil) {
            nqp::if(
              nqp::elems(@graphemes),
              nqp::stmts(
                $markup.add-atom(nqp::strfromcodes(@graphemes)),
                nqp::setelems(@graphemes, 0)
              )
            );
        }

        # do all of the markup parsing in one pass
        nqp::while(
          nqp::islt_i(++$i,$elems),                             # all graphemes
          nqp::stmts(
            nqp::if(
              nqp::iseq_i(($this = nqp::atpos_i(@codes,$i)),$open),
              nqp::if(                                          # <
                nqp::isge_i($prev,$A) && nqp::isle_i($prev,$Z),
                nqp::stmts(                                     # A<
                  nqp::pop_i(@graphemes),  # letter is not part of string
                  add-graphemes(collector),
                  nqp::push($markups,RakuAST::Doc::Markup.new(
                    :letter(nqp::chr($prev))
                  ))
                ),
                nqp::push_i(@graphemes,$this),                  # bare <
              ),
              nqp::if(                                          # not <
                nqp::iseq_i($this,$close),
                nqp::if(                                        # >
                  nqp::elems($markups),
                  nqp::stmts(                                   # markups left
                    add-graphemes($current := nqp::pop($markups)),
                    nqp::if(
                      nqp::istype($current,RakuAST::Doc::Markup),
                      $current.check-meta
                    ),
                    collector.add-atom($current)
                  ),
                  nqp::push_i(@graphemes,$this),                # bare >
                ),
                nqp::push_i(@graphemes,$this),                  # grapheme
              )
            ),
            ($prev = $this)
          )
        );

        # we have open markups left
        if nqp::elems($markups) -> int $elems {
            my $worry := 'Pod formatting code '
              ~ nqp::atpos($markups,nqp::sub_i($elems,1)).letter
              ~ " missing endtag '>'.";
            if $*RESOLVER -> $resolver {
                self.worry-ad-hoc($resolver, $worry);
            }
            else {
                warn $worry;
            }

            nqp::while(
              nqp::elems($markups),
              nqp::stmts(
                add-graphemes($current := nqp::pop($markups)),
                collector.add-atom($current)
              )
            );
        }

        # add any (remaining) graphemes
        add-graphemes($paragraph);

        $paragraph
    }
}

augment class RakuAST::Doc::Block {
    method make-legacy-pod(RakuAST::Doc::Block:D:) {
        my str $type  = self.type;
        my str $level = self.level;

        # this needs code of its own, as the new grammar only collects
        # and does not do any interpretation
        return self.make-legacy-pod-table if $type eq 'table' and !$level;

        my $config   := self.config;
        my $contents := self.paragraphs.map({
            nqp::istype($_,Str)
              ?? .lines.join(' ').trim
              !! .make-legacy-pod
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
                ?? Pod::Block::Comment.new(:contents([self.paragraphs.head]))
                !! $type eq 'input' | 'output' | 'code'
                  ?? Pod::Block::Code.new(:contents([
                       self.paragraphs.head.split("\n", :v, :skip-empty)
                     ]))
                  !! Pod::Block::Named.new(:name($type), :$config, :$contents)
          !! $contents  # no type means just a string
    }

    method make-legacy-pod-table() {
        X::NYI.new(feature => "legacy pod support for =table").throw;
    }

    # create block with type/paragraph introspection
    method from-paragraphs(:$spaces, :$type, :@paragraphs, *%_) {
        my $block  := self.new(:$type, |%_);
        my $offset := $spaces.chars;

        # these need verbatim stuff
        if $type eq 'comment' {
            $block.add-paragraph(@paragraphs.join);
        }

        # these need to be handled as code
        elsif $type eq 'code' | 'input' | 'output' {
            $block.add-paragraph(@paragraphs.map(*.substr($offset)).join)
        }

        # potentially need introspection
        elsif $type eq 'pod' | 'doc' {

            for @paragraphs -> $paragraph {

                # need further introspection
                if nqp::istype($paragraph,Str) {
                    my @lines;
                    my @codes;

                    sub add-lines() {
                        $block.add-paragraph(
                          RakuAST::Doc::Paragraph.from-string(@lines.join)
                        );
                        @lines = ();
                    }
                    sub add-codes() {
                        my $code := @codes.join("\n");
                        my $last := $block.paragraphs.tail;

                        $last && $last.type && $last.type eq 'code'
                          ?? $last.set-paragraphs(  # combine with previous
                               $last.paragraphs.head ~ "\n" ~ $code
                             )
                          !! $block.add-paragraph(  # create new
                               RakuAST::Doc::Block.new(
                                 :type<code>, :paragraphs(@codes.join("\n"))
                               )
                             );
                        @codes = ();
                    }

                    my str $last-leading-ws =
                      $paragraph.leading-whitespace.substr($offset);
                    for $paragraph.lines(:!chomp) {

                        if .is-whitespace {
                            if @codes {
                                my int $ws-offset = $last-leading-ws.chars;
                                @codes.push((
                                  .chars >= $ws-offset ?? .substr($ws-offset) !! ""
                                ).chomp);
                            }
                            else {
                                @lines.push($_);
                            }
                        }

                        # not just whitespace
                        else {
                            my $line := (.starts-with($spaces)
                              ?? .substr($offset)
                              !! .trim-leading
                            ).chomp;
                            if $line.leading-whitespace -> str $leading-ws {
                                add-lines if @lines;

                                if $leading-ws ne $last-leading-ws {
                                    add-codes if @codes;
                                    $last-leading-ws = $leading-ws;
                                    @codes = $line.substr($leading-ws.chars);
                                }
                                else {
                                    @codes.push: $line.substr($leading-ws.chars);
                                }
                            }
                            else {
                                add-codes if @codes;
                                @lines.push($line);
                            }
                        }
                    }
                    add-codes if @codes;
                    add-lines if @lines;
                }

                # already introspected
                else {
                    $block.add-paragraph($paragraph);
                }
            }
        }

        # these just need the paragraphs
        else {
            $block.add-paragraph(
              nqp::istype($_,Str)
                ?? RakuAST::Doc::Paragraph.from-string(.substr($offset))
                !! $_
            ) for @paragraphs;
        }

        $block
    }
}

augment class RakuAST::Doc::Declarator {
    method make-legacy-pod($WHEREFORE) {
        sub normalize(@paragraphs) {
            @paragraphs.map(*.lines.map({.trim if $_}).Slip).join(' ')
        }
        my $pod := Pod::Block::Declarator.new(
          WHEREFORE => $WHEREFORE,
          leading   => [%*ENV<RAKUDO_POD_DECL_BLOCK_USER_FORMAT>
            ?? self.leading.join("\n")
            !! normalize(self.leading)
          ],
          trailing  => [[normalize self.trailing],]
        );
        $WHEREFORE.set_why($pod);
        $pod
    }
}

# vim: expandtab shiftwidth=4
