package EXPORT::crlf { BEGIN OUR::<$?NL> := "\x0D\x0A" }
package EXPORT::cr   { BEGIN OUR::<$?NL> := "\x0D"     }
package EXPORT::lf   { BEGIN OUR::<$?NL> := "\x0A"     }

# vim: expandtab shiftwidth=4
