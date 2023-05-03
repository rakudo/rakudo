
# This file contains augmentations to classes that are created in the
# RakuAST bootstrap to allow a lot of logic (which will **NOT** be
# needed to compile the Raku setting) to be written in Raku rather
# than in NQP.

augment class RakuAST::Doc {

    # just pass it on for now, make using another class possible
    # in the not too distant future
    proto method podify(|) {*}
    multi method podify() {
        RakuAST::LegacyPodify.podify(self)
    }
    multi method podify($WHEREFORE) {
        RakuAST::LegacyPodify.podify(self, $WHEREFORE)
    }
}

augment class RakuAST::Doc::Markup {

    # convert the contents of E<> to a codepoint
    method convert-entity(RakuAST::Doc::Markup: Str:D $entity) {
        my $codepoint := val $entity;

        my $string;
        if nqp::not_i(nqp::istype($codepoint,Allomorph)) {  # not numeric
            $string := RakuAST::HTML::Entities.parse($entity);
            unless $string {
                $string := $entity.uniparse;
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
        if $letter eq 'E' | 'L' | 'X' {
            my @atoms = self.atoms;
            if nqp::istype(@atoms.tail,Str) {
                if $letter eq 'E' {
                    self.set-atoms;  # reset so we can add again
                    for @atoms.pop.split(';') {
                        self.add-meta($_);
                        self.add-atom(self.convert-entity($_));
                    }
                }
                elsif $letter eq 'L' {
                    my ($str,$uri) = @atoms.tail.split('|',2);
                    self.set-meta($uri) if $uri;
                    $str
                      ?? (@atoms.tail = $str)
                      !! @atoms.pop;
                    self.set-atoms(@atoms.List);
                }
                else { # $letter eq 'X' {
                    my ($str,$refs) = @atoms.tail.split('|',2);
                    if $refs {
                        self.add-meta(.split(',').List)
                          for $refs.split(';');
                    }
                    $str
                      ?? (@atoms.tail = $str)
                      !! @atoms.pop;
                    self.set-atoms(@atoms.List);
                }
            }
        }
    }

    multi method Str(RakuAST::Doc::Markup:D:) {
        my str $letter = self.letter;
        $letter eq 'E'
          ?? 'E<' ~ self.meta.join(';') ~ '>'
          !! $letter eq 'L'
            ?? 'L<' ~ self.atoms.join
                  ~ '|' ~ self.meta ~ '>'
            !! $letter eq 'X'
              ?? 'X<' ~ self.atoms.join
                    ~ '|' ~ self.meta.join(self.separator) ~ '>'
              !! $letter ~ '<' ~ self.atoms.join ~ '>'
    }
}

augment class RakuAST::Doc::Paragraph {

    # conceptual leading whitespace of first element
    method leading-whitespace() {
        nqp::istype((my $first := self.atoms.head),Str)
          ?? $first.leading-whitespace
          !! ""
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

    # conceptual leading whitespace of first element
    method leading-whitespace() {
        self.paragraphs.head.leading-whitespace;
    }

    # create block with type/paragraph introspection
    method from-paragraphs(:$type, :$config, :@paragraphs, *%_) {

        # Normalize True/False values in config
        for $config.kv -> $key, $value {
            if nqp::istype($value,Str) {
                if $value eq '⊨' {
                    $config.BIND-KEY($key,True);
                }
                elsif $value eq '⊭' {
                    $config.BIND-KEY($key,False);
                }
            }
        }

        # set up basic block
        my $block := self.new(:$type, :$config, |%_);

        # these need verbatim stuff
        if $type eq 'comment' {
            $block.add-paragraph(@paragraphs.join);
        }

        # these need to be handled as code
        elsif $type eq 'code' | 'input' | 'output' {
            my int $offset = @paragraphs.head.leading-whitespace.chars;
            $block.add-paragraph(@paragraphs.map(*.substr($offset)).join)
        }

        # potentially need introspection
        elsif $type eq 'pod' | 'doc' {

            my str $last-leading-ws;
            my int $offset;
            my sub set-leading-ws($leading-ws) {
                $last-leading-ws = $leading-ws // "";
                $offset          = nqp::chars($last-leading-ws);
            }

            set-leading-ws(@paragraphs.head.leading-whitespace);
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

                    for $paragraph.lines(:!chomp) {

                        if .is-whitespace {
                            if @codes {
                                @codes.push((
                                  .chars >= $offset ?? .substr($offset) !! ""
                                ).chomp);
                            }
                            else {
                                @lines.push($_);
                            }
                        }

                        # not just whitespace
                        else {
                            my $line := (.starts-with($last-leading-ws)
                              ?? .substr($offset)
                              !! .trim-leading
                            ).chomp;
                            if $line.leading-whitespace -> str $leading-ws {
                                add-lines if @lines;

                                if $leading-ws ne $last-leading-ws {
                                    add-codes if @codes;
                                    $last-leading-ws = $leading-ws;
                                    $offset          = nqp::chars($leading-ws);
                                    @codes = $line.substr($offset);
                                }
                                else {
                                    @codes.push: $line.substr($offset);
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
                    set-leading-ws($paragraph.leading-whitespace);
                    $block.add-paragraph($paragraph);
                }
            }
        }

        # these just need the paragraphs
        else {
            $block.add-paragraph(
              nqp::istype($_,Str)
                ?? RakuAST::Doc::Paragraph.from-string($_)
                !! $_
            ) for @paragraphs;
        }

        $block
    }
}

# vim: expandtab shiftwidth=4
