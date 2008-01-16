## $Id$

=head1 NAME

src/builtins/string.pir - Perl6 builtin string functions

=head1 Functions

=over 4

=cut

.namespace

.include 'cclass.pasm'


=item lc

 our Str multi Str::lc ( Str $string )

Returns the input string after converting each character to its lowercase
form, if uppercase.

=cut

.sub 'lc'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'lc'()
.end


=item lcfirst

 our Str multi Str::lcfirst ( Str $string )

Like C<lc>, but only affects the first character.

=cut

.sub 'lcfirst'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'lcfirst'()
.end


=item uc

 our Str multi Str::uc ( Str $string )

Returns the input string after converting each character to its uppercase
form, if lowercase. This is not a Unicode "titlecase" operation, but a
full "uppercase".

=cut

.sub 'uc'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'uc'()
.end


=item ucfirst

 our Str multi Str::ucfirst ( Str $string )

Performs a Unicode "titlecase" operation on the first character of the string.

=cut

.sub 'ucfirst'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'ucfirst'()
.end


=item capitalize

 our Str multi Str::capitalize ( Str $string )

Has the effect of first doing an C<lc> on the entire string, then performing a
C<s:g/(\w+)/{ucfirst $1}/> on it.

=cut

.sub 'capitalize'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'capitalize'()
.end


=item split

 our List multi Str::split ( Str $delimiter ,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Rule $delimiter = /\s+/,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Str $input :  Str $delimiter          , Int $limit = inf )
 our List multi Str::split ( Str $input : Rule $delimiter          , Int $limit = inf )

String delimiters must not be treated as rules but as constants.  The
default is no longer S<' '> since that would be interpreted as a constant.
P5's C<< split('S< >') >> will translate to C<.words> or some such.  Null trailing fields
are no longer trimmed by default.  We might add some kind of :trim flag or
introduce a trimlist function of some sort.

B<Note:> partial implementation only

=cut

.sub 'split'
    .param string sep
    .param string target
    .local pmc a, b

    a = new 'Perl6Str'
    b = new 'Perl6Str'

    a = target
    b = sep

    .return a.'split'(b)
.end


=item join

B<Note:> partial implementation only

=cut

.sub 'join'
    .param pmc args            :slurpy
    .local pmc flatargs
    .local string sep

    flatargs = new 'List'
    sep = ''
    unless args goto have_flatargs
    $P0 = args[0]
    $I0 = isa $P0, 'List'
    if $I0 goto have_sep
    $P0 = shift args
    sep = $P0
  have_sep:
  arg_loop:
    unless args goto have_flatargs
    $P0 = shift args
    $I0 = isa $P0, 'List'
    if $I0 goto arg_array
    push flatargs, $P0
    goto arg_loop
  arg_array:
    $I0 = elements flatargs
    splice flatargs, $P0, $I0, 0
    goto arg_loop
  have_flatargs:
    $S0 = join sep, flatargs
    .return ($S0)
.end


=item substr

 multi substr (Str $s, StrPos $start  : StrPos $end,      $replace)
 multi substr (Str $s, StrPos $start,   StrLen $length  : $replace)
 multi substr (Str $s, StrLen $offset : StrLen $length,   $replace)

B<Note:> partial implementation only

=cut

.sub 'substr'
    .param string x
    .param int start
    .param int len
    $S0 = substr x, start, len
    .return ($S0)
.end


=back

=head2 TODO Functions

=over 4

=item p5chop

 our Char multi P5emul::Str::p5chop ( Str  $string is rw )
 our Char multi P5emul::Str::p5chop ( Str *@strings = ($+_) is rw )

Trims the last character from C<$string>, and returns it. Called with a
list, it chops each item in turn, and returns the last character
chopped.

=item chop

 our Str method Str::chop ( Str  $string: )

Returns string with one Char removed from the end.

=item p5chomp

 our Int multi P5emul::Str::p5chomp ( Str  $string is rw )
 our Int multi P5emul::Str::p5chomp ( Str *@strings = ($+_) is rw )

Related to C<p5chop>, only removes trailing chars that match C</\n/>. In
either case, it returns the number of chars removed.

=item chomp

 our Str method Str::chomp ( Str $string: )

Returns string with newline removed from the end.  An arbitrary
terminator can be removed if the input filehandle has marked the
string for where the "newline" begins.  (Presumably this is stored
as a property of the string.)  Otherwise a standard newline is removed.

Note: Most users should just let their I/O handles autochomp instead.
(Autochomping is the default.)

=item length

This word is banned in Perl 6.  You must specify units.

=item index

Needs to be in terms of StrPos, not Int.

=item pack

=item pos

=item quotemeta

=item rindex

Needs to be in terms of StrPos, not Int.

=item sprintf

=item unpack

=item vec

Should replace vec with declared arrays of bit, uint2, uint4, etc.

=item words

 our List multi Str::words ( Rule $matcher = /\S+/,  Str $input = $+_, Int $limit = inf )
 our List multi Str::words ( Str $input : Rule $matcher = /\S+/, Int $limit = inf )

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
