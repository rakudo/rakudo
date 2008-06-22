## $Id$

=head1 TITLE

Str - Perl 6 Str class and related functions

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=head1 Methods

=over 4

=cut

.namespace ['Perl6Str']

.include 'cclass.pasm'

.sub 'onload' :anon :init :load
    .local pmc p6meta, strproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    strproto = p6meta.'new_class'('Str', 'parent'=>'Perl6Str Any')
    p6meta.'register'('Perl6Str', 'parent'=>strproto, 'protoobject'=>strproto)
    p6meta.'register'('String', 'parent'=>strproto, 'protoobject'=>strproto)

    $P0 = get_hll_namespace ['String']
    '!EXPORT'('sprintf', 'from'=>$P0)
.end


.sub 'ACCEPTS' :method
    .param string topic
    .return 'infix:eq'(topic, self)
.end

.sub 'chars' :method
    .local pmc retv

    retv = new 'Integer'
    $S0  = self
    $I0  = length $S0
    retv = $I0

    .return (retv)
.end

.sub 'reverse' :method
    .local pmc retv

    retv = self.'split'('')
    retv = retv.'reverse'()
    retv = retv.join('')

    .return(retv)
.end

.sub split :method :multi('Perl6Str')
    .param string delim
    .local string objst
    .local pmc pieces
    .local pmc tmps
    .local pmc retv
    .local int len
    .local int i

    retv = new 'List'

    objst = self
    split pieces, delim, objst

    len = pieces
    i = 0
  loop:
    if i == len goto done

    tmps = new 'Perl6Str'
    tmps = pieces[i]

    retv.'push'(tmps)

    inc i
    goto loop
  done:
    .return(retv)
.end

.sub lc :method
    .local string tmps
    .local pmc retv

    tmps = self
    downcase tmps

    retv = new 'Perl6Str'
    retv = tmps

    .return(retv)
.end

.sub uc :method
    .local string tmps
    .local pmc retv

    tmps = self
    upcase tmps

    retv = new 'Perl6Str'
    retv = tmps

    .return(retv)
.end

.sub lcfirst :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    substr fchr, tmps, 0, 1
    downcase fchr

    concat retv, fchr
    substr tmps, tmps, 1
    concat retv, tmps

  done:
    .return(retv)
.end

.sub ucfirst :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    substr fchr, tmps, 0, 1
    upcase fchr

    concat retv, fchr
    substr tmps, tmps, 1
    concat retv, tmps

  done:
    .return(retv)
.end

.sub capitalize :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    downcase tmps

    .local int pos, is_ws, is_lc
    pos = 0
    goto first_char
  next_grapheme:
    if pos == len goto done
    is_ws = is_cclass .CCLASS_WHITESPACE, tmps, pos
    if is_ws goto ws
  advance:
    pos += 1
    goto next_grapheme
  ws:
    pos += 1
  first_char:
    is_lc = is_cclass .CCLASS_LOWERCASE, tmps, pos
    unless is_lc goto advance
    $S1 = substr tmps, pos, 1
    upcase $S1
    substr tmps, pos, 1, $S1
    ## the length may have changed after replacement, so measure it again
    len = length tmps
    goto advance
  done:
    retv = tmps
    .return (retv)
.end

.sub 'chop' :method
    .local string tmps
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
    tmps = self

    len = length tmps
    if len == 0 goto done
    dec len
    substr tmps,tmps, 0, len
  done:
    retv = tmps
    .return(retv)
.end


=item perl()

Returns a Perl representation of the Str.

=cut

.sub 'perl' :method
    $S0 = "\""
    $S1 = self
    $S1 = escape $S1
    concat $S0, $S1
    concat $S0, "\""
    .return ($S0)
.end

=item sprintf( *@args )

=cut

.sub 'sprintf' :method
    .param pmc args            :slurpy
    args.'!flatten'()
    $P0 = new 'Str'
    sprintf $P0, self, args
    .return ($P0)
.end


=item substr()

=cut

.sub 'substr' :method
    .param int start
    .param int len     :optional
    .param int has_len :opt_flag
    .local pmc s

    if has_len goto check_len
    len = self.'chars'()

  check_len:
    if len > 0 goto end
    $I0 = self.'chars'()
    len = $I0 + len
    len = len - start

  end:
    $S0 = substr self, start, len
    s = new 'Str'
    s = $S0
    .return (s)
.end

=back

=head1 Functions

=over 4

=cut

.namespace []

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


=item substr

 multi substr (Str $s, StrPos $start  : StrPos $end,      $replace)
 multi substr (Str $s, StrPos $start,   StrLen $length  : $replace)
 multi substr (Str $s, StrLen $offset : StrLen $length,   $replace)

B<Note:> partial implementation only

=cut

.sub 'substr'
    .param string x
    .param int start
    .param int len     :optional
    .local pmc s

    s = new 'Perl6Str'
    s = x
	.return s.'substr'(start, len)
.end

=item chop

 our Str method Str::chop ( Str  $string: )

Returns string with one Char removed from the end.

=cut

.sub 'chop'
    .param string a
    .local pmc s
    s = new 'Perl6Str'
    s = a
    .return s.'chop'()
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
# vim: expandtab shiftwidth=4 ft=pir:
