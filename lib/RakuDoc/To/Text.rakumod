# This is a proof-of-concept renderer of RakuDoc, based on a given
# RakuAST tree.  One either calls this as a class method:
#
# use RakuDoc::To::Text;
# say RakuDoc::To::Text.render($ast)
#
# or using the exported "rakudoc2text" subroutine:
#
# use RakuDoc::To::Text;
# say rakudoc2text($ast);
#
# Note that a RakuAST of a source / documentation file can easily be
# obtained as follows:
#
# my $ast = $filename.IO.slurp.AST;

use experimental :rakuast;

unit class RakuDoc::To::Text;

method render($ast) { $ast.map(&rakudoc2text).join }

# colorless ANSI constants
my constant RESET         = "\e[0m";
my constant BOLD-ON       = "\e[1m";
my constant ITALIC-ON     = "\e[3m";
my constant UNDERLINE-ON  = "\e[4m";
my constant INVERSE-ON    = "\e[7m";
my constant BOLD-OFF      = "\e[22m";
my constant ITALIC-OFF    = "\e[23m";
my constant UNDERLINE-OFF = "\e[24m";
my constant INVERSE-OFF   = "\e[27m";

my sub bold(str $text)      { BOLD-ON      ~ $text ~ BOLD-OFF      }
my sub italic(str $text)    { ITALIC-ON    ~ $text ~ ITALIC-OFF    }
my sub underline(str $text) { UNDERLINE-ON ~ $text ~ UNDERLINE-OFF }
my sub inverse(str $text)   { INVERSE-ON   ~ $text ~ INVERSE-OFF   }

# ANSI formatting allowed
my constant %formats =
  B => &bold,
  C => &bold,
  L => &underline,
  D => &underline,
  R => &inverse,
;

#-- primary dispatchers --------------------------------------------------------

my proto sub rakudoc2text(|) is export {

    # not the first time we call
    if @*NOTES.defined {
        {*}
    }

    # the first time we call
    else {
        my @*NOTES;
        my @*XREFS;
        my str @parts = {*}

        if @*NOTES -> @notes {
            my $index = 0;

            @parts.push: "\nNOTES\n-----\n";
            @parts.push: (++$index).Str(:superscript) ~ " $_\n" for @notes;
        }

        if @*XREFS -> @xrefs {
            my $index = 0;

            @parts.push: "\nREFERENCES\n----------\n";
            @parts.push: (++$index).Str(:superscript) ~ " $_\n" for @xrefs;
        }
        @parts.join
    }
}

# basically make sure Cool stuff that crept in doesn't bomb
my multi sub rakudoc2text(Str:D $string --> Str:D) { $string   }
my multi sub rakudoc2text(Cool:D $cool  --> Str:D) { $cool.Str }

# make sure we only look at interesting ::Doc objects
my multi sub rakudoc2text(RakuAST::Node:D $ast --> Str:D) {
    $ast.rakudoc.map(&rakudoc2text).join
}

# the general handler, with specific sub-actions
my multi sub rakudoc2text(RakuAST::Doc::Block:D $ast --> Str:D) {

    # Set up dynamic lookup for allowable markup letters
    my %*OK := $ast.allowed-markup;

    given $ast.type {
        when 'alias'         { ''                 }
        when 'code'          { code2text($ast)    }
        when 'comment'       { ''                 }
        when 'config'        { ''                 }
        when 'head'          { heading2text($ast) }
        when 'implicit-code' { code2text($ast)    }
        when 'item'          { item2text($ast)    }
        when 'pod'           { paragraphify($ast) }
        when 'rakudoc'       { paragraphify($ast) }
        when 'table'         { table2text($ast)   }
        default              { block2text($ast)   }
    }
}

# handle any declarator targets
my multi sub rakudoc2text(RakuAST::Doc::DeclaratorTarget:D $ast --> Str:D) {
    my str @parts;

    # an empty body so that scopes will be rendered as { ... }
    my constant $empty-body := RakuAST::Blockoid.new(
      RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::Stub::Fail.new
        )
      )
    );

    # get the subject of the documentation
    sub accept($_) {
        .cut-WHY;
        my str $deparsed = .DEPARSE;
        @parts.push(bold($deparsed));
        @parts.push('-' x $deparsed.lines.map(*.chars).max);
    }

    given $ast.clone {
        when RakuAST::Routine | RakuAST::Package {
            .replace-body($empty-body);
            accept($_);
        }
        when RakuAST::VarDeclaration::Simple {
            accept($_);
        }
        default {
            accept($_);
        }
    }

    # normalize text somewhat
    sub normalize($doc) {
        $doc.join("\n").lines.map(*.trim-leading).join("\n")
    }

    @parts.push(normalize($_)) with try $ast.WHY.leading;
    @parts.push(normalize($_)) with try $ast.WHY.trailing;
    @parts.push("");
    @parts.join("\n");
}

# handle any markup such as B<foo>
my multi sub rakudoc2text(RakuAST::Doc::Markup:D $ast --> Str:D) {
    my str $letter = $ast.letter;
    if !%*OK{$letter} {
        if $letter ne 'E' && $ast.meta -> @meta {
            $letter
              ~ $ast.opener
              ~ $ast.atoms.map(&rakudoc2text).join
              ~ "|"
              ~ @meta.map(&rakudoc2text).join
              ~ $ast.closer
        }
        else {
            $ast.Str
        }
    }
    elsif $letter eq 'Z' {
        ''
    }
    elsif $letter eq 'A' {
        rakudoc2text $ast.meta.head
    }
    else {
        my str $text = $ast.atoms.map(&rakudoc2text).join;

        if $letter eq 'L' {
            $text = underline($text);

            # remember the URL as a note
            if $ast.meta.head -> $url {
                @*NOTES.push: $url;
                $text ~ @*NOTES.elems.Str(:superscript)
            }

            # no URL specified
            else {
                $text
            }
        }
        elsif $letter eq 'X' {
            $text = bold($text);

            # remember the xref as a note
            if $ast.meta -> @meta {
                @*XREFS.push: @meta.map(*.join(', ')).join('; ');
                $text ~ @*XREFS.elems.Str(:subscript)
            }

            # no URL specified
            else {
                $text
            }
        }
        elsif %formats{$letter} -> &hilight {
            hilight($text)
        }
        else {
             $text
        }
    }
}

# handle simple paragraphs (that will be word-wrapped)
my multi sub rakudoc2text(RakuAST::Doc::Paragraph:D $ast --> Str:D) {
    $ast.atoms.map(&rakudoc2text).join.naive-word-wrapper ~ "\n\n"
}

# handle a row in a table
my multi sub rakudoc2text(RakuAST::Doc::LegacyRow:D $ast --> Str:D) {
    $ast.DEPARSE
}

#-- textification helpers ------------------------------------------------------

# produce a flattened text version of the given ast where each string
# is being considered a paragraph that needs to be word-wrapped
my sub paragraphify($ast) {
    $ast.paragraphs.map({
        when Str { .naive-word-wrapper ~ "\n\n" }
        default  { rakudoc2text($_) }
    }).join
}

# produce a flattened text version of the given ast without furter modifications
my sub textify(RakuAST::Doc::Block:D $ast --> Str:D) {
    $ast.paragraphs.map(&rakudoc2text).join
}

# handle (implicit) code blocks
my sub code2text(RakuAST::Doc::Block:D $ast --> Str:D) {
    textify($ast).indent(4)
}

# handle =head
my sub heading2text(RakuAST::Doc::Block:D $ast --> Str:D) {
    my str $text = textify($ast).trim-trailing;
    $text = $text ~ "\n" ~ ('-' x $text.chars) ~ "\n";

    my int $level = $ast.level.Int;
    $text.indent($level > 2 ?? 4 !! ($level - 1) * 2)
}

# handle =item
my sub item2text(RakuAST::Doc::Block:D $ast --> Str:D)  {
    ('* ' ~ textify($ast)).indent(2 * $ast.level)
}

# handle =table
my sub table2text(RakuAST::Doc::Block:D $ast) {
    my $config := $ast.resolved-config;

    my str @parts;
    my int $header-row = $config<header-row> // -1;
    my int $header-width;
    my int $row        = -1;
    @parts.push("  $_") for $ast.paragraphs.map({
        # a divider row
        when Str {
            $_;
        }
        # an actual row
        default {
            my str $text = rakudoc2text($_);
            if ++$row == $header-row {
                $header-width = $text.chars;
                bold($text)
            }
            else {
                $text
            }
        }
    });

    # center and underline any caption on top
    if $config<caption> -> $caption {
        my str $text = $caption.Str;  # also handle :caption<foo bar>
        my int $caption-width = $text.chars;
        $text = underline($text);
        @parts.unshift: '  ' ~ ($caption-width >= $header-width
          ?? $text
          !! (' ' x ($header-width - $caption-width) / 2) ~ $text
        );
    }

    @parts.join("\n") ~ "\n\n"
}

# all other =foo
my sub block2text(RakuAST::Doc::Block:D $ast --> Str:D) {
    my str $type = $ast.type;

    bold($type)
      ~ "\n" ~ ('-' x $type.chars)
      ~ "\n" ~ paragraphify($ast)
}

# vim: expandtab shiftwidth=4
