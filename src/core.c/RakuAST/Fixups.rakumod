my class RakuAST::LegacyPodify { ... }

# A class that acts as a Hash as well as an Array, with $=data semantics.
# This needs to live rather late to have "handles" support actually working
# in the setting.
my class Hashray does Iterable {
    has %.Hash handles <
      AT-KEY ASSIGN-KEY BIND-KEY EXISTS-KEY DELETE-KEY Map
      keys kv pairs anti-pairs
    >;
    has @.Array handles <
      AT-POS ASSIGN-POS BIND-POS EXISTS-POS DELETE-POS List
      values push pop shift unshift splice slice iterator
    >;
}

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
        gather self.visit-children: -> $ast --> Nil {
            if nqp::istype($ast,RakuAST::Doc::Block) {
                take $ast;
            }
            elsif nqp::istype($ast,RakuAST::Doc::DeclaratorTarget) {
                take $_ with $ast.WHY;
                $ast.visit-children(&?BLOCK);
            }
            else {
                $ast.visit-children(&?BLOCK);
            }
        }
    }

    # Helper sub to set @*LINEAGE inside visitor code
    my sub beget($parent, &doula --> Nil) {
        @*LINEAGE.unshift: $parent;
        $parent.visit-children(&doula);
        @*LINEAGE.shift;
    }

    # Process all nodes with given mapper
    multi method map(RakuAST::Node:D: &mapper, :$depth-first) {
        gather {
            my @*LINEAGE;
            self.visit-children: $depth-first
              ?? -> $ast --> Nil {
                     beget $ast, &?BLOCK;
                     my $result := mapper($ast);
                     take $result unless nqp::eqaddr($result,Empty);
                 }
              !! -> $ast --> Nil {
                     my $result := mapper($ast);
                     take $result unless nqp::eqaddr($result,Empty);
                     beget $ast, &?BLOCK;
                 }
        }
    }

    # Return list of RakuAST nodes that match, potentially depth-first
    multi method grep(RakuAST::Node:D: $test, :$depth-first!) {
        my int $index = -1;
        my sub k($ast --> Nil) {
            beget $ast, &?ROUTINE;
            take ++$index if $test.ACCEPTS($ast);
        }
        my sub kv($ast --> Nil) {
            beget $ast, &?ROUTINE;
            if $test.ACCEPTS($ast) {
                take ++$index;
                take $ast;
            }
        }
        my sub p($ast --> Nil) {
            beget $ast, &?ROUTINE;
            take Pair.new(++$index, $ast) if $test.ACCEPTS($ast);
        }
        my sub v($ast --> Nil) {
            beget $ast, &?ROUTINE;
            take $ast if $test.ACCEPTS($ast);
        }

        $depth-first
          ?? gather {
                 my @*LINEAGE;
                 self.visit-children:
                   %_<k> ?? &k !! %_<kv> ?? &kv !! %_<p> ?? &p !! &v;
             }
          !! self.grep($test)
    }

    # Return list of RakuAST nodes that match, breadth first
    multi method grep(RakuAST::Node:D: $test) {
        my int $index = -1;
        my sub k($ast --> Nil) {
            take ++$index if $test.ACCEPTS($ast);
            beget $ast, &?ROUTINE;
        }
        my sub kv($ast --> Nil) {
            if $test.ACCEPTS($ast) {
                take ++$index;
                take $ast;
            }
            beget $ast, &?ROUTINE;
        }
        my sub p($ast --> Nil) {
            take Pair.new(++$index, $ast) if $test.ACCEPTS($ast);
            beget $ast, &?ROUTINE;
        }
        my sub v($ast --> Nil) {
            take $ast if $test.ACCEPTS($ast);
            beget $ast, &?ROUTINE;
        }

        gather {
            my @*LINEAGE;
            self.visit-children:
              %_<k> ?? &k !! %_<kv> ?? &kv !! %_<p> ?? &p !! &v;
        }
    }

    # Return first of RakuAST nodes that match
    multi method first(RakuAST::Node:D: $test, :$end, :$depth-first) {
        if $end {
            my $nodes := nqp::create(IterationBuffer);
            my @*LINEAGE;

            self.visit-children: $depth-first
              ?? -> $ast --> Nil {
                     beget $ast, &?BLOCK;
                     $nodes.push($ast);
                     $nodes.push(@*LINEAGE.List);
                 }
              !! -> $ast --> Nil {
                     $nodes.push($ast);
                     $nodes.push(@*LINEAGE.List);
                     beget $ast, &?BLOCK;
                 }

            my $found := Nil;
            # must use .map as .grep doesn't take 2 arg Callables
            $nodes.List.reverse.map: -> @*LINEAGE, $ast {
                if $test.ACCEPTS($ast) {
                    $found := $ast;
                    last;
                }
            }
            $found
        }
        else {
            (gather {
                my @*LINEAGE;
                self.visit-children: $depth-first
                  ?? -> $ast --> Nil {
                         beget $ast, &?BLOCK;
                         if $test.ACCEPTS($ast) {
                             take $ast;
                             last;
                         }
                     }
                  !! -> $ast --> Nil {
                         if $test.ACCEPTS($ast) {
                             take $ast;
                             last;
                         }
                         beget $ast, &?BLOCK;
                     }
            }).head
        }
    }
}

my class RakuAST::Doc::LegacyRow is RakuAST::Node {
    has str  $.column-dividers;
    has      $.column-offsets is built(:bind);  # native int array
    has      $.cells          is built(:bind);  # Str or Markup
    has Bool $.multi-line     is built(False);  # columns are multi-line

    # Stringify all cells (needed for headers)
    method stringify-cells(RakuAST::Doc::LegacyRow:D: --> Nil) {
        $!cells := $!cells.map(*.Str).List;
    }

    # Merge the cells of one or more rows with the current, by
    # concatenating the corresponding cells with a newline,
    # assuming no markup in cells.
    method merge-rows(RakuAST::Doc::LegacyRow:D: *@rows --> Nil) {
        if @rows && nqp::istype($!cells.are,Str) {
            my str @merged = $!cells;
            $!multi-line := True;

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
                  nqp::islt_i(++$i,$elems)
                    && nqp::istype($other.AT-POS($i),Str),
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

    multi method raku(RakuAST::Doc::LegacyRow:D:) {
        my sub nameds() {
            RakuAST::Node.^find_private_method('nameds')(
              self, <column-dividers column-offsets cells>
            )
        }

        # No $*INDENT yet
        if nqp::istype($*INDENT,Failure) {
            my $*INDENT = "";
            nameds;
        }

        # has an $*INDENT already
        else {
            nameds;
        }
    }

    multi method Str(RakuAST::Doc::LegacyRow:D:) {
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
                @parts.join.trim-trailing ~ "\n"
            }
            else {
                cells.join('  ') ~ "\n"
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
            @rows.map(&stringify-cells).join
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

class RakuAST::Doc::Row is RakuAST::Doc::LegacyRow is DEPRECATED { }

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
    method !convert-entity(RakuAST::Doc::Markup: Str:D $entity) {
        my $codepoint := val $entity;

        my $string;
        if nqp::not_i(nqp::istype($codepoint,Allomorph)) {  # not numeric
            if $entity.is-whitespace {
                $string := Nil;
            }
            else {
                $string := RakuAST::HTML::Entities.parse($entity);
                unless $string {
                    $string := $entity.uniparse;
                    unless $string {
                        die # self.worry-ad-hoc:  XXX need better solution
                          qq/"$entity" is not a valid HTML5 entity./;
                    }
                }
            }
        }
        elsif try $codepoint.chr -> $chr {
            $string := $chr;
        }
        else {
            die # self.sorry-ad-hoc:  XXX need better solution
              "Codepoint $codepoint ($codepoint.base(16)) is out of bounds in E<>";
            $string := '';
        }
        $string
    }

    # Extract any meta information from the atoms, perform the expected
    # flattening of 'V' and letterless markup, and set that in the
    # meta information of the given markup
    method !extract-meta(--> Nil) {
        my @atoms;
        my @meta;
        for self.atoms {
            if nqp::istype($_,RakuAST::Doc::Markup) {
                if @meta {
                    my str $letter = .letter;
                    # flatten verbatim markup here
                    if $letter eq "" {
                        my $atom := .opener ~ .atoms ~ .closer;
                        @meta.push: nqp::istype(@meta.tail,Str)
                          ?? @meta.pop ~ $atom
                          !! $atom;
                    }
                    else {
#                        .set-atoms(.atoms.join) if $letter eq 'V';
                        @meta.push($_);
                    }
                }
                else {
                    @atoms.push($_);
                }
            }

            # it's a string
            elsif @meta {
                @meta.push: nqp::istype(@meta.tail,RakuAST::Doc::Markup)
                  ?? $_
                  !! @meta.pop ~ $_;
            }
            else {
                my ($before, $after) = nqp::hllize($_).split("|", 2);
                @atoms.push($before) if $before;
                @meta.push($_)     with $after;
            }
        }

        if @meta {
            self.set-atoms(@atoms);
            @meta.shift if @meta > 1 && !@meta.head;  # empty leading string
            self.set-meta(@meta);
        }
    }

    # set up meta info from the last atom as appropriate
    method check-meta(RakuAST::Doc::Markup:D:) {
        my str $letter = $!letter;
        if $letter eq 'D' | 'F' | 'L' | 'M' | 'X' {
            self!extract-meta;
        }
        elsif $letter eq 'E' {
            my @atoms = self.atoms;
            if nqp::istype(@atoms.tail,Str) {
                self.set-atoms;  # reset so we can add again
                for @atoms.pop.split(';') -> $entity {
                    with self!convert-entity($entity) -> $converted {
                        self.add-meta($entity);
                        self.add-atom($converted);
                    }
                    else {
                        self.add-atom($entity);
                    }
                }
            }
        }
        elsif $letter eq 'A' {
            my $aliases := $*DOC-ALIASES;
            unless nqp::istype($aliases,Failure) {
                if nqp::atkey($aliases,self.atoms.head) -> $alias {
                    self.set-meta($alias);
                }
            }
        }
    }

    # flatten this markup recursively
    method flatten(RakuAST::Doc::Markup:D: :$container--> Str:D) {
        my str @parts = self.atoms.map: {
            nqp::istype($_,RakuAST::Doc::Markup) ?? .flatten(:container) !! $_
        }

        if $container {
            @parts.unshift: $!opener;
            @parts.unshift: $!letter;
        }

        if self.meta -> @meta {
            $!letter eq 'E'
              ?? @parts.pop # stringification so far is incorrect
              !! @parts.push('|');
            @parts.push: @meta.join
        }

        @parts.push: $!closer if $container;
        nqp::join('',@parts)
    }

    # splat letterless markups
    method splat-letterless(RakuAST::Doc::Markup:D: --> Nil) {
        my @atoms;

        # join any string atom with previous string atom, else push
        sub splat($atom) {
            nqp::istype($atom,Str) && nqp::istype(@atoms.tail,Str)
              ?? (@atoms.tail ~= $atom)
              !! @atoms.push($atom)
        }

        for self.atoms -> $atom {
            if nqp::istype($atom,RakuAST::Doc::Markup) {
                $atom.splat-letterless;  # recurse first

                if $atom.letter {
                    splat($atom)
                }
                else {
                    splat($atom.opener);
                    splat($_) for $atom.atoms;
                    splat($atom.closer);
                }
            }
            else {
                splat($atom)
            }
        }
        self.set-atoms(@atoms.List);
    }

    multi method Str(RakuAST::Doc::Markup:D:) {
        my str $letter = self.letter;
        my str @parts  = $letter, self.opener;
        if $letter eq 'E' {
            @parts.push: self.meta.join(';');
        }
        else {
            @parts.push: self.atoms.join;

            if $letter eq 'F' | 'L' {
                if self.meta.join -> $meta {
                    @parts.push: '|';
                    @parts.push: $meta;
                }
            }
            elsif $letter eq 'D' | 'M' | 'X' {
                if self.meta -> @meta {
                    @parts.push: '|';
                    @parts.push:
                      @meta.map({ $_.join(", ") }).join(' ');
                }
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
    my int   $gcprop = BEGIN nqp::unipropcode("General_Category");

    # create object from string, parsing any markup sequences
    method from-string(RakuAST::Doc::Paragraph:U: Str:D $string) {

        my int32 $this;       # the current grapheme
        my int32 $prev;       # the previous grapheme
        my int32 $stopper;    # the current (first) stopper grapheme
        my int32 @codes;      # the graphemes of the given string
        my int32 @graphemes;  # graphemes collected so far

        my $paragraph := RakuAST::Doc::Paragraph.new;
        my $markups   := nqp::list;  # stack of Markup objects
        my $current;                 # current Markup object

        # Sadly, NFC normalization will not normalize synthetics
        # to their internal value, but will instead drop them in
        # here decomposed.  This means that the index $i can NOT
        # be used to do an eqat in the original string, as the
        # the index would get out of sync when a synthetic is
        # encountered.
        nqp::strtocodes($string,nqp::const::NORMALIZE_NFC,@codes);
        my int $i     = -1;                  # index of current char
        my int $elems = nqp::elems(@codes);  # number of codes to parse
        my int $openers;                     # number of consecutive openers
        my int $verbatims;                   # number of V's seen

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
        sub add-graphemes($ast --> Nil) {
            nqp::if(
              nqp::elems(@graphemes),
              nqp::stmts(
                $ast.add-atom(nqp::strfromcodes(@graphemes)),
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
            ));
            $stopper = nqp::iseq_i($this,$open) ?? $close !! $cclose;
            ++$verbatims if nqp::iseq_s($letter,'V');
        }

        # Whether we're at a real stopper (after the initial stopper
        # matched, but there are potentially multiple stoppers needed
        # e.g. in case of >> as a stopper.
        sub is-real-stopper() {
            nqp::if(
              nqp::istype($current,RakuAST::Doc::Markup)
                && (my int $todo = $current.closer.chars - 1),
              nqp::stmts(
                (my int $j = $i),
                nqp::while(
                  nqp::iseq_i(nqp::atpos_i(@codes,++$j),$stopper)
                    && --$todo,
                  nqp::null
                ),
                nqp::if(
                  $todo,
                  0,         # not all stoppers found
                  ($i = $j)  # advance index, also: True
                )
              ),
              1  # single char stopper, or no Markup
            )
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
                nqp::iseq_s(nqp::getuniprop_str($prev,$gcprop),'Lu'),
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
                nqp::iseq_i($this,$stopper) && is-real-stopper,
                nqp::if(                                   # > or »
                  nqp::elems($markups),
                  nqp::stmts(                              # markups left
                    add-graphemes(nqp::pop($markups)),
                    nqp::if(
                      nqp::istype($current,RakuAST::Doc::Markup)
                        && nqp::not_i($verbatims),
                      $current.check-meta
                    ),
                    collector.add-atom($current),
                    nqp::if(
                      nqp::istype($current,RakuAST::Doc::Markup)
                        && nqp::iseq_s($current.letter,"V"),
                      --$verbatims
                    ),
                    ($stopper =
                      nqp::istype(($current := collector),RakuAST::Doc::Markup)
                        && nqp::ord($current.closer)
                    )
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
            my $markup := nqp::atpos($markups,nqp::sub_i($elems,1));
            die # self.worry-ad-hoc:  XXX need better solution
              "RakuDoc markup code $markup.letter() missing endtag '$markup.closer()'.";

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
            .splat-letterless for $paragraph.atoms.grep(RakuAST::Doc::Markup);
            $paragraph
        }
    }

    multi method Str(RakuAST::Doc::Paragraph:D:) {
        self.atoms.map(*.Str).join
    }
}

augment class RakuAST::Doc::Block {

    # return a new Hashray class instance
    method Hashray() is implementation-detail { Hashray.new }

    # conceptual leading whitespace of first element
    method leading-whitespace() is implementation-detail {
        self.paragraphs.head.leading-whitespace
    }

    # return True if a new, procedural table type
    method procedural(RakuAST::Doc::Block:D:) {
        $!type eq 'table'
          && !nqp::istype($!paragraphs[0],RakuAST::Doc::LegacyRow)
    }

    # return a Map with allowed markup codes as keys, conceptually
    method allowed-markup(RakuAST::Doc::Block:D:) {

        # default for allowable markup letters
        my class OK is Map {
            method AT-KEY(Str:D $letter --> Bool:D) { $letter.uniprop eq 'Lu' }
        }
        my class NOK is Map {
            method AT-KEY(Str:D $ --> False) { }
        }

        # a specific set
        my $config := self.resolved-config;
        if $config && $config<allow> -> $allow {
            Map.new( @$allow.map( { $_ => True } ) )
        }

        # all or nothing
        else {
            $!type eq <code defn implicit-code table>.any
              ?? NOK
              !! OK
        }
    }

    # remove left margin whitespace, if any
    method !marginalize(@raw) {

        # some whitespace at margin
        if self.margin.chars -> int $margin {
            my $buffer := nqp::create(IterationBuffer);

            for @raw -> $lines {
                $buffer.push: $lines.lines(:!chomp).map({
                    if .leading-whitespace.chars >= $margin {
                        .substr($margin)
                    }
                    elsif .is-whitespace {
                        "\n"
                    }
                    else {
                        die # self.worry-ad-hoc:  XXX need better solution
                          "'$_.chomp()'
does not have enough whitespace to allow for a margin of $margin positions";
                        .trim-leading
                    }
                }).join;
            }

            $buffer.List
        }

        # no whitespace at left margin
        else {
            @raw
        }
    }

    # create block from =alias
    method from-alias(
      :$lemma, :paragraphs(@raw), *%_
    --> RakuAST::Doc::Block:D) is implementation-detail {

        # set up basic block
        my $block      := self.new(|%_);
        my @paragraphs := $block!marginalize(@raw);

        # add rest with possible markup
        my $paragraph :=
          RakuAST::Doc::Paragraph.from-string(@paragraphs.join("\n"));

        # collect alias info if being collected
        my $aliases := $*DOC-ALIASES;
        nqp::bindkey($aliases,$lemma,$paragraph)
          unless nqp::istype($aliases,Failure);

        $block.add-paragraph($lemma);
        $block.add-paragraph($paragraph);

        $block
    }

    # create block from =config
    method from-config(:$key, *%_) is implementation-detail {
        my $block := self.new(:paragraphs(nqp::list($key)), |%_);

        # Save the configuration in the dynamic config if possible.
        # Note that the values in the configuration hash are Maps
        # of which the values are RakuAST objects that will need
        # literalization before actually usable.
        my $CONFIG := $*DOC-CONFIG;  # may be a BOOTHash
        $CONFIG{$key} := $block.config unless nqp::istype($CONFIG,Failure);

        $block
    }

    # create block with type/paragraph introspection
    method from-paragraphs(:paragraphs(@raw), *%_ --> RakuAST::Doc::Block:D) {
        my constant %implicit =
          :1cell, :1defn, :1item, :1nested, :1pod, :1rakudoc, :1section;

        # set up basic block
        my $block      := self.new(|%_);
        my @paragraphs := $block!marginalize(@raw);

        # verbatim, no postprocessing
        my str $type = $block.type;
        if $type eq 'comment' | 'data' {
            $block.add-paragraph($_) for @paragraphs;
        }

        # verbatim, needs postprocessing
        elsif $type eq 'code' | 'input' | 'output' {
            $block.add-paragraph(
              RakuAST::Doc::Paragraph.from-string($_)
            ) for @paragraphs;
        }

        elsif $type eq 'table' {
            if nqp::istype(@paragraphs.head,Str) {
                $block!interpret-as-table(@paragraphs);
            }
            else {
                $block.add-paragraph($_) for @paragraphs;
            }
        }

        elsif $type eq 'defn' | 'numdefn' {
            my @parts = @paragraphs;
            # first line is the lemma, separate that
            @parts.splice(0,1,@parts.head.split("\n",2));

            # lemma does not allow markup
            $block.add-paragraph(@parts.shift);

            # add rest with implicit code block detection
            $block!interpret-implicit-code-blocks(@parts);
        }

        elsif %implicit.AT-KEY($type) {
            $block!interpret-implicit-code-blocks(@paragraphs);
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

    my int32 $space     =  32;  # " "
    my int32 $plus      =  43;  # "+"
    my int32 $backslash =  92;  # "\\"
    my int32 $pipe      = 124;  # "|"
    my int   $gcprop = nqp::unipropcode("General_Category");

    method !interpret-as-table(@matched --> Nil) {

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
            die # self.sorry-ad-hoc:  XXX need better solution
              "Table has a mixture of visible and invisible column-separator types
in line '$line'";
        }

        my %config = self.config;

        # Parse the given lines assuming virtual dividers were used.
        # Quits if actual dividers were found after it found rows with
        # virtual dividers, or any empty array if none were found so far.
        # Otherwise returns a Seq of RakuAST::Doc::LegacyRow objects with
        # Str row dividers.
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
                              && nqp::isne_i($prev,$backslash)
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
                               nqp::substr($line,$start,$offset - $start - 2)
                             );
                        $start = $offset;
                    }

                    $cells.push: $start > $chars
                      ?? ''
                      !! RakuAST::Doc::Paragraph.from-string(
                           nqp::substr($line,$start)
                         );
                    RakuAST::Doc::LegacyRow.new(
                      :@column-offsets, :cells($cells.List)
                    )
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
                          nqp::substr($line,$start,$offset - $start - 3)
                        ) unless $start > $chars;
                    }
                    $start = $offset;
                }
                $cells.push: RakuAST::Doc::Paragraph.from-string(
                  nqp::substr($line,$start)
                ) unless $start > $chars;

                RakuAST::Doc::LegacyRow.new(
                  :column-dividers(@dividers.join),
                  :column-offsets(@offsets),
                  :cells($cells.List)
                )
            }

            # not a row, so a row divider, so return as is
            else {
                $line ~ "\n"
            }
        }

        my @sofar;       # rows collected so far
        my @paragraphs;  # ready made paragraphs for Block object
        my $merge-multi-row;       # whether to merge multiple rows
        my str @leading-dividers;  # leading dividers, to be prepended at end

        # Add the rows collected so far, merge them if so specified
        # or implied by the occurrence of multiple dividers
        sub add-rows-collected-sofar(:$merge = $merge-multi-row --> Nil) {
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

        # Post-process rows, merging where appropriate
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
                    .stringify-cells for @sofar;
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

        # no explicit header specification: use legacy heuristic of
        # second divider being different from the first divider
        unless %config<header-row> {
            my $seen-row;
            my $first-divider;
            my int $other-dividers;

            for @paragraphs {
                # is it a divider?
                if nqp::istype($_,Str) {

                    # seen a divider after a row before?
                    if $first-divider.defined {
                        if $_ ne $first-divider {
                            %config<header-row> := RakuAST::IntLiteral.new(0);
                            last;  # different, we're done!
                        }
                        ++$other-dividers;
                    }

                    # seen a row before?
                    elsif $seen-row {
                        $first-divider := $_;
                    }
                }

                # it's a row
                else {
                    $seen-row = True;
                }
            }

            # set headers if only one divider was seen after the first row
            %config<header-row> := RakuAST::IntLiteral.new(0)
              if %config<header-row>:!exists
              && $first-divider.defined
              && !$other-dividers;
        }

        # post-process and save
        @paragraphs.prepend(@leading-dividers) if @leading-dividers;
        @paragraphs.push($_) with $last-divider;
        self.set-config(%config.Map);
        self.set-paragraphs(@paragraphs);
    }

    method !interpret-implicit-code-blocks(@paragraphs) {
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
            self.add-paragraph(
              RakuAST::Doc::Block.new(
                :margin($current-ws), :type<implicit-code>,
                :paragraphs(RakuAST::Doc::Paragraph.from-string(@codes.join))
              )
            );
            @codes = ();
        }

        set-current-ws("");
        for @paragraphs -> $paragraph {

            # need further introspection
            if nqp::istype($paragraph,Str) {

                for $paragraph.lines(:!chomp) {

                    # only whitespace means adding to what we're collecting
                    if .is-whitespace {
                        (@codes || @lines).push("\n");
                    }

                    # leading whitespace is the same, or we're collecting
                    # lines and the last line was not empty
                    elsif .leading-whitespace eq $current-ws
                      || (@lines && @lines.tail ne "\n") {
                        @codes
                          ?? @codes.push(.substr($current-offset))
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
                            @codes.push: .substr($current-offset);
                        }

                        # (still) indented, so start new code block
                        elsif $leading {
                            add-codes if @codes;
                            set-current-ws($ws);
                            @codes.push: .substr($current-offset);
                        }

                        # back to original level, or even less
                        else {
                            add-codes if @codes;
                            set-current-ws($ws);
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

    multi method Str(RakuAST::Doc::Block:D:) {
        self.paragraphs.map(*.Str).join
    }

    # Post-process any unresolved asts in the config
    method literalize-config() {
        my $config   := self.config;
        my %resolved := $config.Hash;
        %resolved.deepmap({
            if nqp::istype($_,RakuAST::Node) {
                my $value := .literalize;
                $value.defined
                  ?? ($_ = $value)       # ok, update in hash
                  !! (return .DEPARSE);  # failed, stop now, and return
            }
        });
        nqp::bindattr(self, RakuAST::Doc::Block, '$!resolved-config',
          %resolved.Map);
        Nil
    }

    method resolved-config() {
        nqp::getattr(self, RakuAST::Doc::Block, '$!resolved-config') // Map.new
    }
}

augment class RakuAST::Type::Enum {

    # Hidden enumeration traits get mixed in.  We don't want to expose
    # these for .raku and .DEPARSE.  This creates a clone with a clean
    # set of traits and returns that.
    method clean-clone(RakuAST::Type::Enum:D:) {
        my $enum := self.clone;
        $enum.set-traits(self.traits.grep({
            !(nqp::istype($_,RakuAST::Trait::Does)
              && .type.name.canonicalize.ends-with('Enumeration'))
        }).List);
        $enum
    }
}

augment class RakuAST::Doc::Declarator {

    # Return a 2-element list with all of the leading doc joined and
    # parsed as the first elements, and the trailing doc joined and
    # parsed as the second element
    method paragraphs() {
        $!paragraphs
          // nqp::bindattr(self,RakuAST::Doc::Declarator,'$!paragraphs',
               (self.leading, self.trailing).map({
                 $_
                   ?? RakuAST::Doc::Paragraph.from-string(.join("\n"))
                   !! ''
               }).List
             )
    }
}

# vim: expandtab shiftwidth=4
