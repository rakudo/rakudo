my class RakuAST::LegacyPodify { ... }

# This file contains augmentations to classes that are created in the
# RakuAST bootstrap to allow a lot of logic (which will **NOT** be
# needed to compile the Raku setting) to be written in Raku rather
# than in NQP.

my class RakuAST::Doc::Row is RakuAST::Node {
    has str  $.column-dividers;
    has      $.column-offsets is built(:bind);  # native int array
    has      $.cells          is built(:bind);  # native str array
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
                my int $elems = nqp::elems(@merged) max nqp::elems($other);

                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,$elems) && nqp::atpos_s($other,$i),
                  nqp::bindpos_s(@merged,$i,
                    nqp::concat(
                      nqp::atpos_s(@merged,$i),
                      nqp::concat("\n", nqp::atpos_s($other,$i))
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
        my str $dividers = $!column-dividers;
        my $offsets     := $!column-offsets;

        # Stringify the given strings with the current dividers / offsets
        my sub stringify-cells(\cells) {
            my int $columns = cells.elems;

            my str @parts;    # atoms of string to be assembled
            my int $start;    # start of the next cell contents
            my str $divider;  # the current divider
            my int $offset;   # the current offset
            my int $times;    # number of spaces that should be inserted

            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,$columns),
              nqp::stmts(                                    # for all columns
                @parts.push(nqp::atpos_s(cells,$i)),
                nqp::if(
                  ($divider = nqp::substr($dividers,$i,1)),
                  nqp::stmts(                                # not last/virtual
                    @parts.push(' '),
                    @parts.push($divider),
                    @parts.push(' ')
                  ),
                  @parts.push('  ')
                )
              )
            );
            @parts.join.trim-trailing
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

        # always verbatim
        if $type eq 'comment' {
            $block.add-paragraph(@paragraphs.head.join);
        }

        # originally verbatim, but may need postprocessing
        elsif $type eq 'code' | 'input' | 'output' {
            if @paragraphs.head -> $pod {
                if nqp::hllizefor($config<allow>,'Raku') -> $allow {
                    $block.add-paragraph($_)
                      for RakuAST::Doc::Paragraph.from-string($pod, :$allow);
                }
                else {
                    $block.add-paragraph($pod);
                }
            }
        }

        elsif $type eq 'table' {
            $block.interpret-as-table($spaces, @paragraphs);
        }

        # potentially need introspection
        elsif $type eq 'pod' | 'doc' {

            my str $initial-ws     = $spaces;
            my int $initial-offset = nqp::chars($spaces);

            my str $current-ws;
            my int $current-offset;

            # set current whitespace / offset conveniently
            sub set-current-ws($ws) {
                $current-ws = $ws // '';
                $current-offset = nqp::chars($current-ws);
            }

            set-current-ws($initial-ws);
            for @paragraphs -> $paragraph {

                # need further introspection
                if nqp::istype($paragraph,Str) {
                    my @lines;
                    my @codes;

                    # store collected lines as the next paragraph
                    sub add-lines() {
                        $block.add-paragraph(
                          RakuAST::Doc::Paragraph.from-string(@lines.join)
                        );
                        @lines = ();
                    }

                    # store collected code as the next paragraph
                    sub add-codes() {
                        my $code := @codes.join("\n");
                        my $last := $block.paragraphs.tail;

                        $block.add-paragraph(
                          RakuAST::Doc::Block.new(
                            :type<code>, :paragraphs(@codes.join("\n"))
                          )
                        );
                        @codes = ();
                    }

                    for $paragraph.lines(:!chomp) {

                        if .is-whitespace {
                            @codes
                              ?? @codes.push('')
                              !! @lines.push("\n");
                        }

                        elsif .leading-whitespace eq $current-ws {
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
                    add-codes if @codes;
                    add-lines if @lines;
                }

                # already introspected
                else {
                    set-current-ws($paragraph.leading-whitespace);
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

    my int @row-dividers;
    @row-dividers[.ord] = 1 for ' ', '_', '-', '+', '|', '=';

    # Parse the given line and find out the offsets of columns and any dividers
    my sub columnify($line) {

        nqp::strtocodes($line,nqp::const::NORMALIZE_NFC,my int32 @codes);

        my int32 $space =  32;  # " "
        my int32 $plus  =  43;  # "+"
        my int32 $pipe  = 124;  # "|"

        my int $elems = nqp::elems(@codes);
        @codes.push($space);  # create virtual space at end for trailing |
        my str @dividers;     # strings of dividers encountered
        my int @offsets;      # offsets where columns start (except first)

        # Check the current line for column dividers.  Sets the @dividers
        # and @offsets arrays, returns whether this line should be considered
        # a row (any char that is not a row|column divider).
        my sub inspect-real-dividers() {
            my int $prev = $space;
            my int $curr;
            my int $is-row;
            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::if(                                    # for all chars
                nqp::iseq_i(($curr = nqp::atpos_i(@codes,$i)),$pipe)
                  || nqp::iseq_i($curr,$plus),
                nqp::stmts(                               # | or +
                  nqp::push_s(@dividers,nqp::chr($curr)),
                  nqp::if(
                    nqp::iseq_i($prev,$space)
                      && nqp::iseq_i(
                           nqp::atpos_i(@codes,nqp::add_i($i,1)),
                           $space
                         ),
                    nqp::stmts(                           # real column divider
                      nqp::push_i(@offsets,++$i),
                      ($prev = 0),
                    )
                  )
                ),
                nqp::stmts(                               # NOT | or +
                  nqp::unless(
                    nqp::atpos_i(@row-dividers,$curr),
                    ($is-row = 1),                        # not a row divider
                  ),
                  ($prev = $curr)
                )
              )
            );
            $is-row
        }

        # Already determined this is a row, but no column dividers were
        # seen.  Test whether virtual dividers (two+ consecutive spaces)
        # can be found, and use the positions after that as offsets.
        my sub inspect-virtual-dividers() {
            my int $seen;    # number of spaces seen
            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::if(                                    # for all chars
                nqp::iseq_i(nqp::atpos_i(@codes,$i),$space),
                ++$seen,                                  # a space!
                nqp::stmts(                               # NOT a space
                  nqp::if(
                    nqp::isge_i($seen,2),
                    nqp::push_i(@offsets,$i - 1)          # found start of cell
                  ),
                  ($seen = 0)
                )
              )
            );
        }

        # is it a row
        if inspect-real-dividers() {

            # no dividers yet, means virtual dividers
            inspect-virtual-dividers unless @dividers;

            my int $start;
            my str @cells;

            for @offsets -> int $offset {
                @cells.push: nqp::substr($line,$start,$offset - $start - 2);
                $start = $offset + 1;
            }
            @cells.push: nqp::substr($line,$start);

            RakuAST::Doc::Row.new(
              :column-dividers(@dividers.join),
              :column-offsets(@offsets),
              :@cells
            )
        }

        # not a row, so a row divider, so return as is
        else {
            $line
        }
    }

    method interpret-as-table(RakuAST::Doc::Block:D: $spaces, @paragraphs --> Nil) {
        my @rows;                  # paragraphs as they should be stored
        my $merge-multi-row;       # whether to merge multiple rows
        my str @leading-dividers;  # leading dividers, to be prepended at end
        my @sofar;                 # rows collected so far

        # Add the rows collected so far, merge them if so specified
        # or implied by the occurrence of multiple dividers
        sub add-rows-collected-sofar(:$merge = $merge-multi-row--> Nil) {
            if $merge && @sofar > 1 {
                my $first := @sofar.shift;
                $first.merge-rows(@sofar.splice);
                @rows.push: $first;
            }
            else {
                @rows.append: @sofar;
            }
            @sofar = ();
        }

        my $has-data;  # flag: True if actual rows where found
        my $no-column-dividers;  # if defined, 0 = column dividers found, 1 = not
        for @paragraphs.join.subst(/ \n+ $/).lines.map(&columnify) {
            if nqp::istype($_,Str) {  # a divider
                if @rows {  # not the first divider, implies mult-line mode
                    if @sofar {
                        $merge-multi-row := True;
                        add-rows-collected-sofar;
                    }
                }

                else {  # first divider will *always* merge multiple rows
                    add-rows-collected-sofar(:merge);
                }
                @rows.push: $_;
            }
            else {  # NOT a divider
                my str $column-dividers = .column-dividers;
                with $no-column-dividers {
                    self.sorry-ad-hoc(
                      "Table has a mixture of visible and invisible column-separator types.",
                      "dummy argument that is somehow needed"
                    ) if $no-column-dividers
                           != nqp::not_i(nqp::chars($column-dividers));
                }
                else {
                    $no-column-dividers =
                      nqp::not_i(nqp::chars($column-dividers));
                }

                @sofar.push: $_;
                $has-data := True;
            }
        }

        add-rows-collected-sofar;
        @rows.prepend(@leading-dividers) if @leading-dividers;
        self.set-paragraphs(@rows);
    }
}

# vim: expandtab shiftwidth=4
