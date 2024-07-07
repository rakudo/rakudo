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
#   block-xxx           syntax with a block (if, elsif, loop, while, ...)
#   cap-named           named capture ($<foo>)
#   cap-positional      positional capture ($0, $1, ...)
#   comment             any inline comment (only if activated)
#   doc-xxx             declator doc (leading, trailing)
#   infix               any infix operator
#   invocant            self
#   label               any label (FOO:)
#   literal             any literal value ('foo', 42, ...)
#   markup-L            rakudoc markup (B<foo>, ...) with L being the letter
#   modifier-xxx        statement modifiers (if, while, when, with, ...)
#   multi               types of multi (only, proto, multi)
#   nqp                 any nqp:: ops (nqp::while, nqp::iseq_i, ...)
#   package-xxx         any package declarator (class, grammar, ...)
#   param               any parameter in a signature
#   pragma              any pragma (strict, fatal, ...)
#   prefix              any prefix operator
#   rakudoc-xxx         types of rakudoc (type,config,verbatim,table,content)
#   routine-xxx         named code blocks (sub, method, submethod, ...)
#   scope-xxx           scope xxx (my, our, state, ...)
#   stmt-prefix         statement prefixes ("do","eager","lazy", etc.)
#   stub                any stub code (..., !!!, ???)
#   ternary             ternary operator(??, !!)
#   traitmod            types of trait_mods ("is","does","returns", etc.)
#   type                any type object
#   typer               syntax for creating types (enum, subset)
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
