## FIXME: These are just placeholder operators until we have
## true (lazy) Range objects.  These simply build an array
## based on the operands and return it.

.sub "infix:.."
    .param num a
    .param num b
    .local pmc range
    range = new 'List'
  loop:
    if a > b goto end
    push range, a
    inc a
    goto loop
  end:
    .return (range)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
