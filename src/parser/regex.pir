## $Id$

=head1 TITLE

regex.pir - The <regex> subrule

=head2 DESCRIPTION

=over 4

=item C<regex(PMC mob)>

Handles parsing of "slash regexes" -- i.e., regexes that are 
terminated by a slash.  For this, we just call PGE's p6 regex 
parser, telling it to stop parsing on the closing slash.  

FIXME: This is just a temporary sub to get things working -- 
it will likely change.

=cut

.sub 'regex'
    .param pmc mob
    .param pmc args            :slurpy
    .param pmc adverbs         :slurpy :named
    .local string stop

    stop = ''
    if null adverbs goto with_stop_adverb
    stop = adverbs['stop']
    if stop > '' goto with_stop
  with_stop_adverb:
    unless args goto with_stop
    stop = shift args
  with_stop: 
    .include 'interpinfo.pasm'
    $P0 = get_root_namespace
    $P0 = $P0['parrot';'PGE::Grammar';'regex']
    $P1 = $P0(mob, 'stop'=>stop)
    .return ($P1)
.end

=back

=cut
