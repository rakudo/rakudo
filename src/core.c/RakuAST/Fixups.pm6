
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
        my str @parts  = $letter, self.opener;
        if $letter eq 'E' {
            @parts.push: self.meta.join(';');
        }
        else {
            @parts.push: self.atoms.join;

            if $letter eq 'L' {
                @parts.push: '|';
                @parts.push: self.meta.join;
            }
            elsif $letter eq 'X' {
                @parts.push: '|';
                @parts.push: self.meta.join(self.separator);
            }
        }
        @parts.push: self.closer;
        @parts.join
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
    my int32 $open   = nqp::ord('<');
    my int32 $close  = nqp::ord('>');
    my int32 $oopen  = nqp::ord('«');
    my int32 $cclose = nqp::ord('»');

    # default letters allowed: A..Z
    my constant $default-allowed := nqp::hash(
      '65',1, '66',1, '67',1, '68',1, '69',1, '70',1, '71',1, '72',1, '73',1,
      '74',1, '75',1, '76',1, '77',1, '78',1, '79',1, '80',1, '81',1, '82',1,
      '83',1, '84',1, '85',1, '86',1, '87',1, '88',1, '89',1, '90',1
    );

    # create object from string, parsing any markup sequences
    method from-string(RakuAST::Doc::Paragraph:U: Str:D $string, :$allow) {

        # set up hash with allowed codes
        my $allowed;
        if $allow {
            $allowed := nqp::hash;
            nqp::bindkey($allowed,nqp::ord($_),1) for @$allow;
        }
        else {
            $allowed := $default-allowed;
        }

        my int32 $this;       # the current grapheme
        my int32 $prev;       # the previous grapheme
        my int32 @codes;      # the graphemes of the given string
        my int32 @graphemes;  # graphemes collected so far

        my $paragraph := RakuAST::Doc::Paragraph.new;
        my $markups   := nqp::list;  # stack of Markup objects
        my $current;                 # current Markup object

        nqp::strtocodes($string,nqp::const::NORMALIZE_NFC,@codes);
        my int $i     = -1;                  # index of current char
        my int $elems = nqp::elems(@codes);  # number of codes to parse
        my int $openers;                     # number of consecutive openers

        # return the object currently collecting
        sub collector() {
            nqp::elems($markups)
              ?? nqp::atpos($markups,nqp::sub_i(nqp::elems($markups),1))
              !! $paragraph
        }

        # return the opener of the Markup object currently collecting
        sub opener() {
            nqp::elems($markups)
              ?? nqp::atpos($markups,nqp::sub_i(nqp::elems($markups),1)).opener
              !! ""
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

        # calculate the number of openers
        sub calculate-openers() {
            ($openers = 1),
            nqp::if(
              nqp::iseq_i($this,$open),
              nqp::stmts(
                nqp::while(
                  nqp::islt_i(++$i,$elems)
                    && nqp::iseq_i(nqp::atpos_i(@codes,$i),$open),
                  ++$openers
                ),
                --$i  # gone one too far
              )
            )
        }

        # create new Markup object for given letter and stack it
        sub push-markup(str $letter --> Nil) {
            nqp::push($markups,$current := RakuAST::Doc::Markup.new(
              :letter($letter),
              :opener(nqp::x(
                nqp::if(nqp::iseq_i($this,$open),'<','«'),$openers
              )),
              :closer(nqp::x(
                nqp::if(nqp::iseq_i($this,$open),'>','»'),$openers
              ))
            ))
        }

        # Do all of the markup parsing in one pass.  The idea behind this
        # is that we don't need to create a Match object for every character
        # being checked.  And we also work on integer codepoints to prevent
        # having to create a string object for each codepoint being checked:
        # comparing integers is what computers do very well.  The actual
        # string can also be accessed at the same index: that is used to
        # quickly check matching the current opener / closer, which may be
        # multi character in the case of << >>.
        nqp::while(
          nqp::islt_i(++$i,$elems),                        # for all graphemes
          nqp::stmts(
            nqp::if(
              nqp::iseq_i(($this = nqp::atpos_i(@codes,$i)),$open)
                || nqp::iseq_i($this,$oopen),
              nqp::if(                                     # < or «
                nqp::existskey($allowed,$prev),
                nqp::stmts(                                # A<
                  nqp::pop_i(@graphemes),  # letter is not part of string
                  add-graphemes(collector),
                  calculate-openers,
                  push-markup(nqp::chr($prev))
                ),
                nqp::if(                                   # bare <
                  nqp::eqat($string,opener,$i),
                  nqp::stmts(                              # same, must balance
                    add-graphemes(collector),
                    calculate-openers,
                    push-markup("")        # fake markup to ensure balanced
                  ),
                  nqp::push_i(@graphemes,$this)            # bare < or «
                )
              ),

              nqp::if(                                     # not < or «
                $current && (
                  (nqp::iseq_i($this,$close)
                    && nqp::eqat($string,$current.closer,$i)
                  ) || nqp::iseq_i($this,$cclose)
                ),
                nqp::if(                                   # > or »
                  nqp::elems($markups),
                  nqp::stmts(                              # markups left
                    ($i = $i + nqp::chars($current.closer) - 1),
                    add-graphemes($current := nqp::pop($markups)),
                    nqp::if(
                      nqp::istype($current,RakuAST::Doc::Markup),
                      $current.check-meta
                    ),
                    collector.add-atom($current)
                  ),
                  nqp::push_i(@graphemes,$this)            # bare > or »
                ),
                nqp::push_i(@graphemes,$this)              # other grapheme
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

        # no markup seen, so the string itself is fine
        if nqp::elems(@graphemes) == $elems {
            $string
        }

        # some markup created
        else {
            add-graphemes($paragraph);
            $paragraph
        }
    }
}

augment class RakuAST::Doc::Block {

    # conceptual leading whitespace of first element
    method leading-whitespace() {
        self.paragraphs.head.leading-whitespace;
    }

    # create block with type/paragraph introspection
    method from-paragraphs(:$spaces = '', :$type, :$config, :@paragraphs, *%_) {

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

            set-leading-ws($spaces);
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

                        $last
                          && nqp::not_i(nqp::istype($last,Str))
                          && $last.type
                          && $last.type eq 'code'
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
                                @lines.push("\n");  # lose any other whitespace
                            }
                        }

                        # not just whitespace
                        else {
                            my $line := (.starts-with($last-leading-ws)
                              ?? .substr($offset)
                              !! .trim-leading
                            );
                            if $line.leading-whitespace -> str $leading-ws {
                                add-lines if @lines;

                                if $leading-ws ne $last-leading-ws {
                                    add-codes if @codes;
                                    $last-leading-ws = $leading-ws;
                                    $offset          = nqp::chars($leading-ws);
                                    @codes = $line.substr($offset).chomp;
                                }
                                else {
                                    @codes.push: $line.substr($offset).chomp;
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
