# This file contains the default class for turning RakUAST::Doc::xxx
# classes into legacy pod6 objects.

class RakuAST::LegacyPodify {

    my int32 $nl     =     10;  # "\n"
    my int32 $space  =     32;  # " "
    my int32 $nbsp   = 0x00A0;  # NO-BREAK SPACE
    my int32 $nnbsp  = 0x202F;  # NARROW NO-BREAK SPACE
    my int32 $wj     = 0x2060;  # WORD JOINER
    my int32 $zwnbsp = 0xFEFF;  # ZERO WIDTH NO-BREAK SPACE
    my int   $gcprop = nqp::unipropcode("General_Category");

    # basically mangle text to just single spaces
    my sub sanitize(str $string, :$add-space --> Str:D) {
        return ' ' if $string eq "\n";

        # work with integers instead of characters for speed
        nqp::strtocodes($string,nqp::const::NORMALIZE_NFC,my int32 @input);

        # remove any trailing newlines
        my int $elems = nqp::elems(@input);
        nqp::while(
          $elems && nqp::iseq_i(nqp::atpos_i(@input,--$elems),$nl),
          nqp::pop_i(@input)
        );
        return '' unless $elems = nqp::elems(@input);

        my int32 @output;
        my int32 $curr;
        my int32 $prev;
        my str   $prop;
        my int $i = -1;

        # step through all codes, make the non-breaking whitespace act as
        # normal characters, and collapse all other consecutive whitespace
        # into a single space character
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::if(                                    # for all codes
            nqp::iseq_i(($curr = nqp::atpos_i(@input,$i)),$nbsp)
              || nqp::iseq_i($curr,$nnbsp)
              || nqp::iseq_i($curr,$wj)
              || nqp::iseq_i($curr,$zwnbsp),
            nqp::push_i(@output,$prev = $curr),       # non-breaking whitespace
            nqp::if(                                  # not nb whitespace
              nqp::iseq_s(($prop = nqp::getuniprop_str($curr,$gcprop)),'Zs')
                || nqp::iseq_s($prop,'Cf')
                || nqp::iseq_s($prop,'Cc'),
              nqp::if(                                # all other whitespace
                nqp::isne_i($prev,$space),
                nqp::push_i(@output,$prev = $space),  # after non-ws, add space
              ),
              nqp::push_i(@output,$prev = $curr)      # all ok, just copy
            )
          )
        );

        # add a space if there is something and were asked to add one
        @output.push($space) if $add-space && nqp::elems(@output);
        nqp::strfromcodes(@output)
    }

    # sanitize the given cell, including any handling of markup
    my sub table-sanitize($cell --> Str:D) {
        sanitize(nqp::istype($cell,RakuAST::Doc::Paragraph)
          ?? $cell.atoms.map({
               nqp::istype($_,Str)
                 ?? $_
                 !! .letter eq 'Z'
                   ?? ""
                   !! .Str
             }).join
          !! $cell
        ).trim.subst(Q/\+/, '+', :global).subst(Q/\|/, '|', :global)
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
              !! @atoms.push($markup.closer)
        }
        nqp::istype(@atoms.are,Str)
          ?? @atoms.join
          !! @atoms.map({ nqp::istype($_,Str) ?? $_ !! .podify }).Slip
    }

    # produce list without last if last is \n
    my sub no-last-nl(\list) {
        my @parts = list;
        @parts.pop if nqp::istype($_,Str) && $_ eq "\n" given @parts.tail;
        @parts
    }

    # create podified contents for atoms
    method !contentify(@source) {
        my str @parts;
        my @atoms = @source.map({
            nqp::istype($_,Str)
              ?? sanitize(.subst("\n", ' ', :g))
              !! .podify  # may Slip
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
        # This markup is allowed
        my str $letter = $ast.letter;

        # make sure we have properly podified meta data
        my @meta = do if $letter ne 'E' && $ast.meta -> @_ {
            self!contentify(@_)
        }

        # Markup is allowed
        if %*OK{$letter} {
            $letter eq 'V'
              ?? $ast.atoms.map(*.Str).join.subst("\n", ' ', :g)
              !! Pod::FormattingCode.new(
                   type     => $letter,
                   meta     => $letter eq 'E'
                     ?? $ast.meta.map(*.key)
                     !! @meta,
                   contents => $letter eq 'C'
                     ?? $ast.atoms.join.subst("\n", ' ', :g)
                     !! $letter eq 'E'
                       ?? $ast.meta.map(*.value)
                       !! self!contentify($ast.atoms)
                 )
        }

        # Meta on markup that itself is not allowed
        elsif @meta {
            my @atoms  = self!contentify($ast.atoms);

            @atoms.unshift: nqp::istype(@atoms.head,Str)
              ?? $letter ~ $ast.opener ~ @atoms.shift
              !! $letter ~ $ast.opener;

            @atoms.push: nqp::istype(@atoms.tail,Str)
              ?? @atoms.pop ~ "|"
              !! "|";

            @atoms.push: nqp::istype(@meta.head,Str)
              ?? @atoms.pop ~ @meta.shift
              !! @meta.shift;
            @atoms.append: @meta;

            @atoms.push: nqp::istype(@atoms.tail,Str)
              ?? @atoms.pop ~ $ast.closer
              !! $ast.closer;

            @atoms.List
        }

        # No meta on unallowed markup
        else {
            $ast.Str
        }
    }

    multi method podify(RakuAST::Doc::Paragraph:D $ast) {
        my     @atoms := $ast.atoms;
        my int $left   = @atoms.elems;
        my @contents = no-last-nl(@atoms).map: {
            --$left;
            nqp::istype($_,Str)
              ?? sanitize($_, :add-space($left && .ends-with("\n"))) || Empty
              !! self.podify($_)
        }

        my int $i;  # start at 2nd element
        my int $elems = @contents.elems;
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::if(
            nqp::istype(@contents[$i],Str)
              && nqp::istype(@contents[my int $j = $i - 1],Str),
            nqp::stmts(
              (@contents[$j] ~= @contents.splice($i,1).head),
              --$i,
              --$elems
            )
          )
        );

        Pod::Block::Para.new(:@contents)
    }

    multi method podify(RakuAST::Doc::Block:D $ast) {

        # Set up dynamic lookup for allowable markup letters
        my %*OK := $ast.allowed-markup;

        # These need code of its own, as the new grammar only collects
        # and does not do any interpretation
        my str $type  = $ast.type;
        return self.podify-table($ast)
          if $type eq 'table' | 'numtable';
        return self.podify-verbatim($ast, $type)
          if $type eq 'code' | 'input' | 'output';
        return self.podify-implicit-code($ast)
          if $type eq 'implicit-code';
        return self.podify-defn($ast)
          if $type eq 'defn' | 'numdefn';

        # no more special casing
        my $config   := $ast.resolved-config;
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

        if $type {
            my str $level = $ast.level;
            if $type eq 'item' | 'head' {
                ($type eq 'item' ?? Pod::Item !! Pod::Heading).new(
                  :level($level ?? $level.Int !! 1), :$config, :$contents
                )
            }
            elsif $type eq 'numitem' | 'numhead' {
                my %config = $config;
                %config<numbered> := True;
                ($type eq 'numitem' ?? Pod::Item !! Pod::Heading).new(
                  :level($level ?? $level.Int !! 1), :%config, :$contents
                )
            }
            elsif $level {
                Pod::Block::Named.new(
                  :name($type ~ $level), :$config, :$contents
                )
            }

            # from here on without level
            else {
                $type eq 'comment'
                  ?? Pod::Block::Comment.new(
                       :$config, :contents([
                         $ast.paragraphs.head.trim-trailing ~ "\n"
                       ])
                     )
                  !! $type eq 'config' && $ast.abbreviated
                    ?? Pod::Config.new(
                         :type($ast.paragraphs.head), :$config
                       )
                    !! Pod::Block::Named.new(:name($type), :$config, :$contents)
            }
        }

        # no type means just a string
        else {
            $contents
        }
    }

    method podify-table(RakuAST::Doc::Block:D $ast) {
        my $config := $ast.resolved-config;
        my @rows    = $ast.paragraphs.grep(RakuAST::Doc::LegacyRow);

        # Make sure that all rows have the same number of cells
        my $nr-columns := @rows.map(*.cells.elems).max;
        my sub spread(\cells) {
            cells.elems == $nr-columns
              ?? cells
              !! (cells.Slip, ("" xx $nr-columns - cells.elems).Slip)
        }

        # determine whether we have headers
        my $headers;
        with $config<header-row> -> $index {
            $headers := @rows.splice($index, 1).head;
        }

        # some legacy sanity checks
        my $has-data;              # flag: True if actual rows where found
        my $previous-was-divider;  # flag: True if previous row was divider
        for $ast.paragraphs -> $row {
            if nqp::istype($row,Str) {
                if $previous-was-divider {
                    die # $ast.sorry-ad-hoc:  XXX need better solution
                      "Table has multiple interior row separator lines.";
                    last;
                }
                $previous-was-divider := True;
            }
            else {
                $has-data             := True;
                $previous-was-divider := False;
            }
        }
        die( # $ast.sorry-ad-hoc(  XXX need better solution
          "Table has no data."
        ) unless $has-data;

        # wrap up
        $headers := [spread .cells.map(&table-sanitize)] with $headers;
        Pod::Block::Table.new(
          caption  => $config<caption> // "",
          headers  => $headers // [],
          config   => $config,
          contents => @rows.map({ [spread .cells.map(&table-sanitize)] })
        )
    }

    method podify-verbatim(RakuAST::Doc::Block:D $ast, Str:D $type) {
        my $config  := $ast.resolved-config;
        my @contents = $ast.paragraphs.map({
            (nqp::istype($_,Str)
              ?? .split("\n", :v, :skip-empty)
              # assume a paragraph with string / markup atoms
              !! .atoms.map({
                    nqp::istype($_,Str)
                      ?? .split("\n", :v, :skip-empty).Slip
                      !! nqp::istype($_,RakuAST::Doc::Markup)
                        ?? .podify.Slip
                        !! .Str
                 })
            ).Slip
        });

        # only keep one "\n" at end
        @contents.pop while @contents.tail eq "\n";
        @contents.push("\n");

        ::("Pod::Block::$type.tc()").new: :@contents, :$config
    }

    method podify-implicit-code(RakuAST::Doc::Block:D $ast) {
        Pod::Block::Code.new:
          :contents($ast.paragraphs.head.Str.trim),
          :config($ast.resolved-config)
    }

    method podify-defn(RakuAST::Doc::Block:D $ast) {
        my $term    := sanitize $ast.paragraphs.head.Str;
        my @contents = $ast.paragraphs.skip.map: {
            Pod::Block::Para.new(:contents(sanitize(.Str)))
        }
        my $config := $ast.resolved-config;
        $config := %(|$config, :numbered) if $ast.type.starts-with('num');

        Pod::Defn.new: :$term, :@contents, :$config
    }

    multi method podify(RakuAST::Doc::Declarator:D $ast, $WHEREFORE) {
        sub normalize(@paragraphs) {
            @paragraphs
              .map(*.lines.map({.trim if $_}).Slip)
              .join(' ')
              .trim-trailing
        }

        my $leading := %*ENV<RAKUDO_POD_DECL_BLOCK_USER_FORMAT>
          ?? $ast.leading>>.chomp.grep(?*).join("\n")
          !! normalize($ast.leading);
        my $trailing := normalize $ast.trailing;

        my %args;
        %args<WHEREFORE> = $WHEREFORE;
        %args<leading>   =  $leading   if $leading;
        %args<trailing>  = [$trailing] if $trailing;

        my $pod := Pod::Block::Declarator.new(|%args);
        $WHEREFORE.set_why($pod);
        $pod
    }
}

# vim: expandtab shiftwidth=4
