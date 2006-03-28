## TITLE
##     The Perl 6 grammar
##
## DESCRIPTION
##
## These are the rules used to compile Perl 6 programs.
## This is just a first draft of a grammar for parsing
## Perl 6 programs, undoubtedly more rules will be added
## soon.  Much of the work is hidden in the <opparse>
## rule, which is defined in L<Perl6/parse.pir>
## and handles most expressions using a bottom-up
## parsing algorithm.  

grammar Perl6::Grammar ;


## This rule handles whitespace and comments between tokens.
## XXX:  Add pod directive handling

rule ws { [ \# \N+ | \s+ ]* ::: }


rule program { ^ <statement_list> <?ws> [ $ | <?syntax_error> ] }


rule statement_list { <statement> [ <?statement_end> <statement> ]* }


rule statement_end {
    <after ;> ::
    | <after \}> :: <before \s*? [ \n | \# ]>
}


## XXX: Eventually this rule will probably be done
## using a %statement_control hash.  Since PGE doesn't
## support hashes in rules yet, we're just using
## an alternation for now.

rule statement { 
    <if_statement>
    | <while_statement>
    | <expression> 
}


## XXX: TODO:  Add "elsif" and "else"
rule if_statement {:w
    if <expression> <block>
}


rule while_statement {:w
    while <expression> <block>
}


rule block { <simple_block> | <pointy_sub> }


## A <simple_block> is just a statement_list inside of a pair of braces.

rule simple_block { 
    <?ws>:
    \{ 
        <?ws>: <statement_list> <?ws>: 
    [ \} | <?syntax_error> ] 
}


## XXX: This isn't the real <pointy_sub> rule -- it doesn't know
## how to parse arguments yet.  It's just here as a placeholder
## for now.

rule pointy_sub { --\> <simple_block> }


## We handle Perl 6 expressions using PGE's operator 
## precedence parser.  The tokens and for this are 
## defined in L<Perl6/parser.pir>.

rule expression { <opparse> }


## The <term> rule gets called from the operator precedence
## parser whenever it needs a term.

rule term {
    <sigil> <name>
    | <block>
    | <number>
    | <integer>
    | <PGE::Text::bracketed: '">
}


## The <listop> rule gets called from the operator precedence
## parser whenever it's looking for a term.  At the moment
## it primarily grabs bareword terms.

rule listop { <reserved_word> ::: <fail> | <ident> }

rule reserved_word { [ if | unless | while | until | for | loop ] \b }


## XXX: These are just placeholder rules for demonstration,
## they certainly need to be expanded to be more complete.

rule sigil { <[$@%^]> }
rule integer { \d+ }
rule number { \d+ \. \d+ }


## The <syntax_error> rule generates a simple syntax
## error message, and displays the line number and context
## of the error.

rule syntax_error { <?die: Syntax error> }

