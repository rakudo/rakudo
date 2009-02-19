=head1 TITLE

Str - Perl 6 Str class and related functions

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=head1 Methods

=over 4

=cut

.namespace ['Str']

.include 'cclass.pasm'

.sub 'onload' :anon :init :load
    .local pmc p6meta, strproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    strproto = p6meta.'new_class'('Str', 'parent'=>'Perl6Str Any')
    strproto.'!IMMUTABLE'()
    p6meta.'register'('Perl6Str', 'parent'=>strproto, 'protoobject'=>strproto)
    p6meta.'register'('String', 'parent'=>strproto, 'protoobject'=>strproto)

    $P0 = get_hll_namespace ['Str']
    '!EXPORT'('sprintf', 'from'=>$P0)
.end


## special method to cast Parrot String into Rakudo Str.
.namespace ['String']
.sub 'Scalar' :method
    $P0 = new 'Str'
    assign $P0, self
    copy self, $P0
    .return (self)
.end


.namespace ['Str']
.sub 'ACCEPTS' :method
    .param string topic
    .tailcall 'infix:eq'(topic, self)
.end


=item perl()

Returns a Perl representation of the Str.

=cut

.sub 'perl' :method
    .local string str, result
    str = self
    result = '"'
    .local int pos
    pos = 0
    .local pmc arr
    arr = new 'ResizablePMCArray'
  loop:
    .local string ch
    ch = substr str, pos, 1
    if ch == '' goto done
    if ch == ' ' goto loop_ch
    ##  check for special escapes
    $I0 = index  "$ @ % & { \b \n \r \t \\ \"", ch
    if $I0 < 0 goto loop_nonprint
    ch = substr  "\\$\\@\\%\\&\\{\\b\\n\\r\\t\\\\\\\"", $I0, 2
    goto loop_ch
  loop_nonprint:
    $I0 = is_cclass .CCLASS_PRINTING, ch, 0
    if $I0 goto loop_ch
    $I0 = ord ch
    arr[0] = $I0
    ch = sprintf '\x[%x]', arr
  loop_ch:
    result .= ch
    inc pos
    goto loop
  done:
    result .= '"'
    .return (result)
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

=item succ and pred

Increment and Decrement Methods

=cut

.sub 'pred' :method
    $P0 = clone self
    dec $P0
    .return ($P0)
.end

.sub 'succ' :method
    $P0 = clone self
    inc $P0
    .return ($P0)
.end


=item WHICH()

Returns the identify value.

=cut

.sub 'WHICH' :method
    $S0 = self
    .return ($S0)
.end


=back

=head1 Functions

=over 4

=cut

.namespace []

.include 'cclass.pasm'


=item infix:===

Overridden for Str.

=cut

.namespace []
.sub 'infix:===' :multi(String,String)
    .param string a
    .param string b
    .tailcall 'infix:eq'(a, b)
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

=item length

This word is banned in Perl 6.  You must specify units.

=item index

Needs to be in terms of StrPos, not Int.

=item pack

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
