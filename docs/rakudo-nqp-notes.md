# Notes and hints for working with Rakudo NQP

## Pod block text content handling

Text inside pod blocks that are contents rather than markup is comprised of
intermixed text and formatting code characters. Newlines and contiguous
whitespace may or may not be significant depending upon the general block type
(abbreviated, paragraph, delimited, or declarator) or block identifier (e.g.,
code, input, output, defn, comment, or data).

The content as it is parsed in Grammar.nqp is first broken into individual
characters which are then assigned to one of three token groups: regular text, text with
formatting code, and text that is to be unchanged from its input form
(code, input, and output).

The regular text and intermingled formatted text are then divided into two more
categories: text that will form one or more paragraphs and text that is part
of a table.  Ultimately, each paragraph of text should be grouped into the
@contents array of a single Pod::Block::Para, but not all pod handling per S26
has been fully implemented.

Some notable, not-yet-implemented (NYI) features (in order of one dev's TODO list)

+ formatting code in table cells is not yet handled

+ formatting code in defn block terms is not yet handled

+ pod configuration aliasing is not yet handled

+ pod data blocks are not yet handled

+ %config :numbered aliasing with '#' is not handled for paragraph or delmited blocks

+ formatting code in declarator blocks is not yet handled

+ inconsistent use of the Pod::Block::Para as the leaf parent of all regular text

Anyone wanting to work on any of the NYI items please coordinate on IRC #perl6-dev to
avoid duplicate efforts.  Most of the items are being worked on in a generally logical
order of need and knowledge gained during the process of implementing pod features.
