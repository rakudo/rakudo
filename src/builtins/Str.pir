=head1 TITLE

Str - Perl 6 Str class and related functions

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=head2 Methods

=over 4

=cut

.namespace ['Str']

.include 'cclass.pasm'

.sub 'onload' :anon :init :load
    .local pmc p6meta, strproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    strproto = p6meta.'new_class'('Str', 'parent'=>'parrot;Perl6Str Any')
.end

.sub 'ACCEPTS' :method
    .param string topic
    .tailcall '&infix:<eq>'(topic, self)
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


=item succ and pred

Increment and Decrement Methods

=cut

.namespace ['Str']

## The RANGES constant indicates the successor to each character in
## a defined range.  ## Currently supported increment/decrement ranges:
##    0..9   ASCII digits  (U+0030..U+0039)
##    A..Z   ASCII uc       (U+0041..U+005a)
##    a..z   ASCII lc       (U+0061..U+006a)
##    Α..Ω   Greek uc       (U+0391..U+03a9, skip u+03a2)
##    α..ω   Greek lc       (U+03b1..U+03c9, skip u+03c2)
##    Ⅰ..Ⅻ   clock roman uc (U+2160..U+216b)
##    ⅰ..ⅻ   clock roman lc (U+2170..U+217b)
##    ①..⑳   circled digits (U+2460..U+2473)
##    ⑴..⒇   parenth digits (U+2474..U+2487)
##    ⒜..⒵   parenth lc     (U+249c..U+24b5)
##    ⚀..⚅   die faces      (U+2680..U+2685)
## Note that in each cycle, the first character of the cycle is repeated
## at the end of the cycle (to indicate carries).

.const string RANGES = unicode:"01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZAabcdefghijklmnopqrstuvwxyza\u0391\u0392\u0393\u0394\u0395\u0396\u0397\u0398\u0399\u039a\u039b\u039c\u039d\u039e\u039f\u03a0\u03a1\u03a3\u03a4\u03a5\u03a6\u03a7\u03a8\u03a9\u0391\u03b1\u03b2\u03b3\u03b4\u03b5\u03b6\u03b7\u03b8\u03b9\u03ba\u03bb\u03bc\u03bd\u03be\u03bf\u03c0\u03c1\u03c3\u03c4\u03c5\u03c6\u03c7\u03c8\u03c9\u03b1\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216a\u216b\u2160\u2170\u2171\u2172\u2173\u2174\u2175\u2176\u2177\u2178\u2179\u217a\u217b\u2170\u2460\u2461\u2462\u2463\u2464\u2465\u2466\u2467\u2468\u2469\u246a\u246b\u246c\u246d\u246e\u246f\u2470\u2471\u2472\u2473\u2460\u2474\u2475\u2476\u2477\u2478\u2479\u247a\u247b\u247c\u247d\u247e\u247f\u2480\u2481\u2482\u2483\u2484\u2485\u2486\u2487\u2474\u249c\u249d\u249e\u249f\u24a0\u24a1\u24a2\u24a3\u24a4\u24a5\u24a6\u24a7\u24a8\u24a9\u24aa\u24ab\u24ac\u24ad\u24ae\u24af\u24b0\u24b1\u24b2\u24b3\u24b4\u24b5\u249c\u2680\u2681\u2682\u2683\u2684\u2685\u2680"

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

=item File Test Methods

These return various information about the file.

=cut

.namespace ['Str']
.sub 'e' :method
    #does it exist?
    .local string filename
    filename = self
    $I42 = stat filename, 0
    if $I42 goto lisreal
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
  lisreal:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

.sub 'z' :method
    #is the length of this file zero?
    .local string filename
    filename = self
    $I42 = stat filename, 1
    if $I42==0 goto worthnothing
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
  worthnothing:
    $P0 = get_hll_global ['Bool'], 'True'
    .return($P0)
.end

.sub 's' :method
    #how big is the file (in bytes)?
    .local string filename
    filename = self
    $I42 = stat filename, 1
    .return($I42) #if we need a conditional to return a special type of False instead of just 0, then look up
.end

=back

=head2 Functions

=over 4

=item &infix:<===>

Overridden for Str.

=cut

.namespace []
.sub '&infix:<===>' :multi(String,String)
    .param string a
    .param string b
    $I0 = iseq a, b
    .tailcall '&prefix:<?>'($I0)
.end

=back

=cut

.sub '!qx'
    .param string cmd
    .local pmc pio
#   '!hash_to_env'()   # ng: TODO
    pio = open cmd, 'rp'
    unless pio goto err_qx
    pio.'encoding'('utf8')
    $P0 = pio.'readall'()
    pio.'close'()
    .return ($P0)
  err_qx:
    .tailcall '!FAIL'('Unable to execute "', cmd, '"')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
