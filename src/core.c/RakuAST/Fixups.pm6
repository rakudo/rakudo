my class RakuAST::LegacyPodify { ... }

# This file contains augmentations to classes that are created in the
# RakuAST bootstrap to allow a lot of logic (which will **NOT** be
# needed to compile the Raku setting) to be written in Raku rather
# than in NQP.

augment class RakuAST::Node {

    # Helper method to produce the outer Rakudoc objects of a given
    # AST (aka, the RakuAST::Doc::Block and RakuAST::Doc::Declarator
    # objects).  Note that the RakuAST::Doc::Block may have embedded
    # RakuAST::Doc::Block in its .paragraphs, so recursion may be
    # necessary.
    method rakudoc(RakuAST::Node:D:) {
        my $nodes := IterationBuffer.new;

        my sub visitor($ast --> Nil) {
            if nqp::istype($ast,RakuAST::Doc::Block) {
                $nodes.push($ast);
            }
            elsif nqp::istype($ast,RakuAST::Doc::DeclaratorTarget) {
                $nodes.push($_) with $ast.WHY;
            }
            else {
                $ast.visit-children(&visitor);
            }
        }

        self.visit-children(&visitor);
        $nodes.List
    }
}

my class RakuAST::Doc::Row is RakuAST::Node {
    has str  $.column-dividers;
    has      $.column-offsets is built(:bind);  # native int array
    has      $.cells          is built(:bind);  # Str or Markup
    has Bool $.multi-line     is built(False);  # columns are multi-line

    # Merge the cells of one or more rows with the current, by
    # concatenating the corresponding cells with a newline.
    method merge-rows(RakuAST::Doc::Row:D: *@rows --> Nil) {
        if @rows {
            my str @merged = $!cells;
            $!multi-line := True;

            # simplified for now, assuming no format strings in cells
            for @rows -> $row {
                my $other    := $row.cells;
                my int $elems = $other.elems;

                if nqp::isgt_i($elems,nqp::elems(@merged)) {
                    nqp::until(
                      nqp::isge_i(nqp::elems(@merged),$elems),
                      nqp::push_s(@merged,"")
                    );
                }
                else {
                    $elems = nqp::elems(@merged);
                }

                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,$elems) && $other.AT-POS($i),
                  nqp::bindpos_s(@merged,$i,
                    nqp::concat(
                      nqp::atpos_s(@merged,$i),
                      nqp::concat("\n", $other.AT-POS($i))
                    )
                  )
                );
            }
            $!cells := @merged;
        }
    }

    multi method raku(RakuAST::Doc::Row:D:) {
        RakuAST::Node.^find_private_method('nameds')(
          self, <column-dividers column-offsets cells>
        )
    }

    multi method Str(RakuAST::Doc::Row:D:) {
        my str $dividers = nqp::hllizefor($!column-dividers,'Raku') // '';

        # Stringify the given strings with the current dividers / offsets
        my sub stringify-cells(\cells) {
            if $dividers {
                my int $columns = cells.elems;
                my int $i = -1;  # column index
                my int $j = -1;  # offset index

                my str @parts;   # atoms of string to be assembled

                # push divider if first cell started with divider
                if nqp::iseq_i(nqp::atpos_i($!column-offsets,0),2) {
                    @parts.push(nqp::substr($dividers,++$j,1));
                    @parts.push(' ');
                }

                nqp::while(
                  nqp::islt_i(++$i,$columns),
                  nqp::stmts(
                    @parts.push(cells.AT-POS($i).Str),
                    @parts.push(' '),
                    @parts.push(nqp::substr($dividers,++$j,1)),
                    @parts.push(' ')
                  )
                );
                @parts.join.trim-trailing
            }
            else {
                cells.join('  ')
            }
        }

        # cells may contain multiple lines, implies visual dividers
        if $!multi-line {
            my @rows;
            for $!cells -> $cell {
                my int $row = -1;
                for $cell.lines {
                    (@rows[++$row] // (@rows[$row] := my str @)).push: $_;
                }
            }
            @rows.map(&stringify-cells).join("\n")
        }

        # only a single line
        else {
            stringify-cells($!cells)
        }
    }

    # conceptual leading whitespace of first element
    method leading-whitespace() {
        $!cells.head.leading-whitespace
    }
}

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
                    self.worry-ad-hoc:
                      qq/"$entity" is not a valid HTML5 entity./,
                      "dummy argument that is somehow needed";
                }
            }
        }
        elsif try $codepoint.chr -> $chr {
            $string := $chr;
        }
        else {
            self.sorry-ad-hoc:
              "Codepoint $codepoint ($codepoint.base(16)) is out of bounds in E<>",
              "dummy argument that is somehow needed";
            $string := '';
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
    my int32 $open   =  60;  # <
    my int32 $close  =  62;  # >
    my int32 $oopen  = 171;  # «
    my int32 $cclose = 187;  # »

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
              !! "\0"  # never matches
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
            self.worry-ad-hoc: 'Pod formatting code '
              ~ nqp::atpos($markups,nqp::sub_i($elems,1)).letter
              ~ " missing endtag '>'.",
              "dummy argument that is somehow needed";

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

    multi method Str(RakuAST::Doc::Paragraph:D:) {
        self.atoms.map(*.Str).join
    }
}

augment class RakuAST::Doc::Block {

    # conceptual leading whitespace of first element
    method leading-whitespace() {
        self.paragraphs.head.leading-whitespace
    }

    # create block with type/paragraph introspection
    method from-paragraphs(:$spaces = '', :$type, :$config, :@paragraphs, *%_) {

        # set up basic block
        my $block := self.new(:$type, :$config, |%_);

        # always verbatim
        if $type eq 'comment' {
            $block.add-paragraph(@paragraphs.head.join);
        }

        # originally verbatim, but may need postprocessing
        elsif $type eq 'code' | 'input' | 'output' {
            my int $offset = $spaces.chars;
            my str @allow  = nqp::hllizefor($config<allow>,'Raku') // ();

            if $offset || @allow {
                for @paragraphs -> $pod is copy {
                    $pod = $pod.lines(:!chomp).map({
                        .starts-with($spaces) ?? .substr($offset) !! $_
                    }).join if $offset;

                    @allow
                      ?? $block.add-paragraph(
                           RakuAST::Doc::Paragraph.from-string($pod, :@allow)
                         )
                      !! $block.add-paragraph($pod);
                }
            }
            else {
                $block.add-paragraph($_) for @paragraphs;
            }
        }

        elsif $type eq 'table' {
            $block.interpret-as-table($spaces, @paragraphs);
        }

        # potentially need introspection
        elsif $type eq 'pod' | 'doc' {
            $block.interpret-implicit-code-blocks($spaces, @paragraphs);
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

    my int @row-dividers;
    @row-dividers[.ord] = 1 for ' ', '_', '-', '+', '|', '=';

    my int32 $space  =  32;  # " "
    my int32 $plus   =  43;  # "+"
    my int32 $pipe   = 124;  # "|"
    my int   $gcprop = nqp::unipropcode("General_Category");

    method interpret-as-table(RakuAST::Doc::Block:D: $spaces, @matched --> Nil) {

        # Set up the lines to be parsed
        my str @lines = @matched.join.subst(/ \n+ $/).lines;
        return unless @lines;  # nothing to do

        # Remove common leading whitespace from all lines
        if @lines[0].leading-whitespace.chars -> int $leading is copy {
            my int $i;
            my int $elems = nqp::elems(@lines);
            my int $offset;
            my str $line;
            nqp::while(
              $leading && nqp::islt_i(++$i,$elems),
              nqp::stmts(
                ($line = nqp::atpos_s(@lines,$i)),
                nqp::if(
                  nqp::islt_i(
                    ($offset = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE,$line,0,nqp::chars($line)
                    )),
                    $leading
                  ),
                  ($leading = $offset)
                )
              )
            );

            # found common whitespace
            if $leading {
                $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,$elems),
                  nqp::bindpos_s(@lines,$i,
                    nqp::substr(nqp::atpos_s(@lines,$i),$leading)
                  )
                );
            }
        }

        # Error handling for mixed column divider types
        my sub mixed-up($line) {
            self.sorry-ad-hoc(
              "Table has a mixture of visible and invisible column-separator types
in line '$line'",
              "dummy argument that is somehow needed"
            );
        }

        my str @allow = self.config<allow> // Empty;
        @allow.push: 'Z' unless @allow.first('Z');

        # Parse the given lines assuming virtual dividers were used.
        # Quits if actual dividers were found after it found rows with
        # virtual dividers, or any empty array if none were found so far.
        # Otherwise returns a Seq of RakuAST::Doc::Row objects with Str
        # row dividers.
        my sub parse-assuming-virtual-dividers() {
            my int   $start;
            my @codes-per-row;
            my @offsets-per-line;

            for @lines -> str $line {
                nqp::strtocodes($line,nqp::const::NORMALIZE_NFC,my int32 @codes);
                my int $elems = nqp::elems(@codes);
                my int @offsets;

                my int $is-row;
                my int $no-more-leading;
                my int $prev;
                my int $curr;
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,$elems),
                  nqp::stmts(                              # for all chars
                    nqp::if(
                      nqp::iseq_i(($curr = nqp::atpos_i(@codes,$i)),$space)
                        && nqp::iseq_i($prev,$space),
                      nqp::if(                             # found 2 spaces
                        $no-more-leading,
                        nqp::stmts(                        # in a column
                          nqp::while(                      # eat next spaces
                            nqp::islt_i(++$i,$elems)
                              && nqp::iseq_i(nqp::atpos_i(@codes,$i),$space),
                            nqp::null
                          ),
                          nqp::if(                         # done eating spaces
                            nqp::islt_i($i,$elems),
                            nqp::stmts(                    # NOT at end
                              ($no-more-leading = 1),
                              @offsets.push($i),
                            ),
                          ),
                          --$i                             # one too far
                        ),
                      ),
                      nqp::stmts(                          # not 2 spaces
                        nqp::if(
                          nqp::atpos_i(@row-dividers,$curr),
                          nqp::if(                         # a divider
                            $is-row
                              && (nqp::iseq_i($curr,$pipe)
                                   || nqp::iseq_i($curr,$plus)),
                            nqp::if(                       # visual divider
                              @codes-per-row.elems,
                              mixed-up($line),             # mixed, give up
                              (return ())                  # handle elsewhere
                            )
                          ),
                          ($is-row = 1)                    # NOT a divider
                        ),
                        nqp::if(
                          nqp::isne_i($curr,$space),
                          ($no-more-leading = 1)
                        )
                      )
                    ),
                    ($prev = $curr)
                  )
                );

                # offsets on divider lines do not count
                if $is-row {
                    @codes-per-row.push: @codes;
                    @offsets-per-line.push: @offsets;
                }
                else {
                    @offsets-per-line.push: Any;
                }
            }

            # Calculate the valid column offsets from the offsets seen
            # so far.  Only offsets that are either past the end of a
            # row, or which only have a space at *each* row two positions
            # before that offset, are accepted.  Return them in ascending
            # order.
            my int @offsets = @offsets-per-line.map({
                .Slip if $_
            }).unique.grep({
                my int $offset = $_ - 2;  # must have 2 spaces before
                # disqualify any offset that has a defined non-space
                # char on any of the rows
                !@codes-per-row.first: -> @codes {
                    $offset < nqp::elems(@codes)
                      && nqp::isne_i(nqp::atpos_i(@codes,$offset),$space)
                }
            }).sort;

            # To provide consistency with offsets produced by
            # columnify, we prefix the offset of the first
            # column
            my int @column-offsets = @offsets;
            @column-offsets.unshift(0);

            # Process all of the info into the final Seq
            @lines.kv.map: -> $index, str $line {

                # it's a row, build it from cells and offsets
                if @offsets-per-line[$index].defined {
                    my     $cells := nqp::create(IterationBuffer);
                    my int $chars  = nqp::chars($line);
                    my int $start;

                    for @offsets -> int $offset {
                        $cells.push: $start > $chars
                          ?? ''
                          !! RakuAST::Doc::Paragraph.from-string(
                               nqp::substr($line,$start,$offset - $start - 2),
                               :@allow
                             );
                        $start = $offset;
                    }

                    $cells.push: $start > $chars
                      ?? ''
                      !! RakuAST::Doc::Paragraph.from-string(
                           nqp::substr($line,$start), :@allow
                         );
                    RakuAST::Doc::Row.new(:@column-offsets, :cells($cells.List))
                }

                #divider
                else {
                    $line
                }
            }
        }

        # Parse the given line and find out offsets of columns and dividers
        my sub columnify($line) {

            # is a given codepoint horizontal whitespace
            my sub is-ws(int $codepoint) {
                nqp::iseq_i($codepoint,$space)
                  || nqp::iseq_s(nqp::getuniprop_str($codepoint,$gcprop),'Zs')
            }

            nqp::strtocodes($line,nqp::const::NORMALIZE_NFC,my int32 @codes);

            my int $elems = nqp::elems(@codes);
            @codes.push($space);  # create virtual space at end for trailing |
            my str @dividers;     # strings of dividers encountered
            my int @offsets;      # offsets where columns start (except first)

            # Check the current line for column dividers.  Sets the @dividers
            # and @offsets arrays, returns whether this line should be
            # considered a row (any char that is not a row|column divider).
            my sub inspect-real-dividers() {
                my int32 $prev = $space;  # fake space at start for leading |
                my int32 $curr;
                my int   $is-row;
                my int   $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,$elems),
                  nqp::if(                              # for all chars
                    nqp::iseq_i(($curr = nqp::atpos_i(@codes,$i)),$pipe)
                      || nqp::iseq_i($curr,$plus),
                    nqp::stmts(                         # | or +
                      nqp::push_s(@dividers,nqp::chr($curr)),
                      nqp::if(
                        is-ws($prev) && is-ws(nqp::atpos_i(@codes,$i + 1)),
                        nqp::stmts(                     # real column divider
                          nqp::push_i(@offsets,nqp::add_i(++$i,1)),
                          ($prev = 0),
                        )
                      )
                    ),
                    nqp::stmts(                         # NOT | or +
                      nqp::unless(
                        nqp::atpos_i(@row-dividers,$curr),
                        ($is-row = 1),                  # not a row divider
                      ),
                      ($prev = $curr)
                    )
                  )
                );
                $is-row
            }

            # is it a row
            if inspect-real-dividers() {

                # no dividers found, must have at least one
                mixed-up($line) unless nqp::elems(@dividers);

                my     $cells := nqp::create(IterationBuffer);
                my int $chars  = nqp::chars($line);
                my int $start;

                for @offsets -> int $offset {
                    # If the first offset is 2, then this implies that
                    # the line started with a divider, so there is no
                    # cell to push here, as there is no cell before it
                    unless $offset == 2 {
                        $cells.push: RakuAST::Doc::Paragraph.from-string(
                          nqp::substr($line,$start,$offset - $start - 3),
                          :@allow
                        ) unless $start > $chars;
                    }
                    $start = $offset;
                }
                $cells.push: RakuAST::Doc::Paragraph.from-string(
                  nqp::substr($line,$start), :@allow
                ) unless $start > $chars;

                RakuAST::Doc::Row.new(
                  :column-dividers(@dividers.join),
                  :column-offsets(@offsets),
                  :cells($cells.List)
                )
            }

            # not a row, so a row divider, so return as is
            else {
                $line
            }
        }

        my @sofar;       # rows collected so far
        my @paragraphs;  # ready made paragraphs for Block object
        my $merge-multi-row;       # whether to merge multiple rows
        my str @leading-dividers;  # leading dividers, to be prepended at end

        # Add the rows collected so far, merge them if so specified
        # or implied by the occurrence of multiple dividers
        sub add-rows-collected-sofar(:$merge = $merge-multi-row--> Nil) {
            if $merge && @sofar > 1 {
                my $first := @sofar.shift;
                $first.merge-rows(@sofar.splice);
                @paragraphs.push: $first;
            }
            else {
                @paragraphs.append: @sofar;
            }
            @sofar = ();
        }

        # First try virtual dividers, then visual if failed
        my @rows = parse-assuming-virtual-dividers;
        @rows = @lines.map(&columnify) unless @rows;

        # get rid of any last divider to make multi-row merge checks
        # easier
        my $last-divider := @rows.pop if nqp::istype(@rows.tail,Str);

        # Post-process rows, merging where appropriat
        for @rows {
            # a divider
            if nqp::istype($_,Str) {

                # not first divider, implies multi-line mode from now on
                if @paragraphs {
                    if @sofar {
                        $merge-multi-row := True;
                        add-rows-collected-sofar;
                    }
                    @paragraphs.push: $_;
                }

                # first divider will *always* merge multiple rows
                elsif @sofar {
                    add-rows-collected-sofar(:merge);
                    @paragraphs.push: $_;
                }

                # divider *before* any data row, keep them for later
                else {
                    @leading-dividers.push: $_;
                }
            }
            else {  # NOT a divider
                @sofar.push: $_;
            }
        }

        add-rows-collected-sofar;
        @paragraphs.prepend(@leading-dividers) if @leading-dividers;
        @paragraphs.push($_) with $last-divider;
        self.set-paragraphs(@paragraphs);
    }

    method interpret-implicit-code-blocks(RakuAST::Doc::Block:D: $spaces, @paragraphs) {
        my str $initial-ws     = $spaces;
        my int $initial-offset = nqp::chars($spaces);

        my str $current-ws;
        my int $current-offset;

        # set current whitespace / offset conveniently
        sub set-current-ws($ws) {
            $current-ws = $ws // '';
            $current-offset = nqp::chars($current-ws);
        }

        # store collected lines as the next paragraph
        my @lines;
        sub add-lines() {
            self.add-paragraph(
              RakuAST::Doc::Paragraph.from-string(@lines.join)
            );
            @lines = ();
        }

        # store collected code as the next paragraph
        my @codes;
        sub add-codes() {
            my $code := @codes.join("\n");
            my $last := self.paragraphs.tail;

            self.add-paragraph(
              RakuAST::Doc::Block.new(
                :type<implicit-code>, :paragraphs(@codes.join("\n"))
              )
            );
            @codes = ();
        }

        set-current-ws($initial-ws);
        for @paragraphs -> $paragraph {

            # need further introspection
            if nqp::istype($paragraph,Str) {

                for $paragraph.lines(:!chomp) {

                    # only whitespace means adding to what we're collecting
                    if .is-whitespace {
                        @codes
                          ?? @codes.push('')
                          !! @lines.push("\n");
                    }

                    # leading whitespace is the same, or we're collecting
                    # lines and the last line was not empty
                    elsif .leading-whitespace eq $current-ws
                      || (@lines && @lines.tail ne "\n") {
                        @codes
                          ?? @codes.push(.substr($current-offset).chomp)
                          !! @lines.push(.trim-leading);
                    }

                    # change in leading whitespace
                    else {
                        my str $ws      = .leading-whitespace;
                        my int $leading = nqp::chars($ws);

                        # deeper indented, start / continue code block
                        if $leading > $current-offset {
                            add-lines if @lines;
                            set-current-ws($ws) unless @codes;
                            @codes.push: .substr($current-offset).chomp;
                        }

                        # (still) indented, so start new code block
                        elsif $leading > $initial-offset {
                            add-codes if @codes;
                            set-current-ws($ws);
                            @codes.push: .substr($current-offset).chomp;
                        }

                        # back to original level, or even less
                        else {
                            add-codes if @codes;
                            @lines.push: .trim-leading;
                        }
                    }
                }
                add-lines if @lines;
            }

            # already introspected
            else {
                add-codes if @codes;
                add-lines if @lines;
                set-current-ws($paragraph.leading-whitespace);
                self.add-paragraph($paragraph);
            }
        }

        add-codes if @codes;
        add-lines if @lines;
    }
}

# vim: expandtab shiftwidth=4
