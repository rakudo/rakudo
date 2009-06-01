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
    strproto = p6meta.'new_class'('Str', 'parent'=>'parrot;Perl6Str Any')
    strproto.'!IMMUTABLE'()
    p6meta.'register'('Perl6Str', 'parent'=>strproto, 'protoobject'=>strproto)
    p6meta.'register'('String', 'parent'=>strproto, 'protoobject'=>strproto)

    $P0 = get_hll_namespace ['Str']
    '!EXPORT'('sprintf', 'from'=>$P0)
.end


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
    arr = root_new ['parrot';'ResizablePMCArray']
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
    $P0 = new ['Str']
    sprintf $P0, self, args
    .return ($P0)
.end

=item succ and pred

Increment and Decrement Methods

=cut

.namespace ['Str']
.const string RANGES = "01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZAabcdefghijklmnopqrstuvwxyza"

.sub '!range_pos' :anon
    .param string str

    .local int len, pos, r0, r1
    len = length str

    # Scan from the end of a string for a character that is in RANGES.
    # This is the potential end of the substring to be incremented.
    pos = len
  scan_loop:
    # Reset range positions to indicate that we haven't found a valid substr
    r0 = 0
    r1 = -1
  scan_end_loop:
    unless pos > 0 goto done
    dec pos
    $S0 = substr str, pos, 1
    $I0 = index RANGES, $S0
    if $I0 < 0 goto scan_end_loop

    # we found a candidate end of the range, now scan for start
    r1 = pos
  scan_start_loop:
    # if we reach the beginning of the string, the range starts at pos 0
    unless pos > 0 goto done
    dec pos
    $S0 = substr str, pos, 1
    # if we find a dot: this isn't a valid range, scan again
    if $S0 == '.' goto scan_loop
    # if we find a valid character, keep scanning
    $I0 = index RANGES, $S0
    if $I0 >= 0 goto scan_start_loop
    # pos + 1 is the start of the range, we're done
    r0 = pos + 1

  done:
    .return (r0, r1)
.end


.sub 'pred' :method
    .local string str
    str = self
    str = clone str

    .local int r0, r1, ipos
    (r0, r1) = '!range_pos'(str)
    if r1 < 0 goto done

  dec_1:
    .local string orig, repl
    orig = substr str, r1, 1
    ipos = index RANGES, orig 
    $I0 = ipos + 1
    $I0 = index RANGES, orig, $I0
    if $I0 < 0 goto dec_2
    ipos = $I0
  dec_2:
    dec ipos
    repl = substr RANGES, ipos, 1
    substr str, r1, 1, repl
    # if the replacement wasn't a carry, we're done
    if orig > repl goto done
  carry:
    # if there are more characters in the range, decrement those first
    dec r1
    if r1 >= r0 goto dec_1
  extend:
    .tailcall '!FAIL'('Decrement out of range')

  done:
    .return (str)
.end


.sub 'succ' :method
    .local string str
    str = self
    str = clone str

    .local int r0, r1, ipos
    (r0, r1) = '!range_pos'(str)
    if r1 < 0 goto done

  inc_1:
    .local string orig, repl
    orig = substr str, r1, 1
    ipos = index RANGES, orig 
    inc ipos
    .local string repl
    repl = substr RANGES, ipos, 1
    substr str, r1, 1, repl
    # if the replacement wasn't a carry, we're done
    if orig < repl goto done
  carry:
    # if there are more characters in the range, increment those first
    dec r1
    if r1 >= r0 goto inc_1
  extend:
    # insert a new character based on the previous one
    unless repl == '0' goto extend_1
    repl = '1'
  extend_1:
    substr str, r0, 0, repl

  done:
    .return (str)
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
