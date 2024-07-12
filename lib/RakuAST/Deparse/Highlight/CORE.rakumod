# This file contains that "null" highlighting logic Raku Programming Language
# syntax features.  It is not installed, as it just serves as an example
# and documentation for other highlighting modules.
#
# Please note that as the Raku Programming Language evolves, further
# elements may be added, so any translations will probably need to be
# updated by then as well.
#
# The first parameter is a type indication of the syntax.  The following
# type indications exist so far:
#
#   adverb-q-xxx        quoting language adverb ("to","val","x", etc.)
#   block-xxx           syntax with a block (if, elsif, loop, while, etc.)
#   capture-xxx         match captures ($<foo>, $0)
#   comment             any inline comment (only if activated)
#   constraint-xxx      type of constraint applied ("where")
#   core-xxx            all core functions ("say","bag","set","put", etc.)
#   doc-xxx             declator doc (leading, trailing)
#   infix-xxx           any infix operator ("eq","==", etc.)
#   invocant            self
#   label               any label (FOO:)
#   literal             any literal value ('foo', 42, ...)
#   markup-xxx          rakudoc markup (B<foo>, ...) with xxx being the letter
#   meta-xxx            meta operator ("!","=","R","X","Z",">>","<<")
#   modifier-xxx        statement modifiers (if, while, when, with, ...)
#   multi-xxx           types of multi (only, proto, multi)
#   named-xxx           named argument on core function("absolute", etc.)
#   nqp-xxx             any nqp:: ops (nqp::while, nqp::iseq_i, ...)
#   package-xxx         any package declarator (class, grammar, ...)
#   param               any parameter in a signature
#   phaser-xxx          phaser ("BEGIN","CATCH","LEAVE", etc.)
#   postfix-xxx         any postfix operator
#   pragma-xxx          any pragma (strict, fatal, ...)
#   prefix-xxx          any prefix operator
#   quote-lang-xxx      quoting language indicator ("q","qq","Q", etc.)
#   rakudoc-xxx         types of rakudoc (type,config,verbatim,table,content)
#   routine-xxx         named code blocks (sub, method, submethod, ...)
#   scope-xxx           scope xxx (my, our, state, ...)
#   stmt-prefix-xxx     statement prefixes ("do","eager","lazy", etc.)
#   stub                any stub code (..., !!!, ???)
#   system-xxx          system methods ("ACCEPTS","TWEAK", etc.)
#   term-xxx            system terms ("time","pi","tau", etc.)
#   ternary             ternary operator(??, !!)
#   trait-is-xxx        core supported "is" traits ("copy","export", etc.)
#   traitmod-xxx        types of trait_mods ("is","does","returns", etc.)
#   type                any type object
#   typer-xxx           syntax for creating types ("enum", "subset")
#   use-xxx             module loading related ("use","require", etc.)
#   var-attribute       any type of attribute variable ($.foo, $!foo)
#   var-compiler        compile-time var ($?FILE, ...)
#   var-dynamic         dynamic var ($*IN, $*OUT, ...)
#   var-implicit        implicit var ($/, $_, $!)
#   var-lexical         lexical var ($foo)
#   var-package         multi-part package var ($Foo::Bar::baz)
#   var-placeholder     placeholders ($^a, :$^b, @_, %_)
#   var-rakudoc         rakudoc var ($=pod, $=data)
#   var-setting         setting var (&sum, ...)
#   var-term            lexical term (\foo)
#
# If you call the .DEPARSE method with a role compatible with this role,
# it will automatically mix it in with the default RakuAST::Deparse class.

unit role RakuAST::Deparse::Highlight::CORE;

# Implement basic "null" highlighting by just returning the content
method hsyn(str $type, str $content) { $content }

# vim: expandtab shiftwidth=4
