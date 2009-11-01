=item !FAIL(args)

Return a Failure object using C<args> as the failure message.

=cut

.namespace []
.sub '!FAIL'
    .param pmc args            :slurpy
    $P0 = new ['Undef']
    .return ($P0)
.end
