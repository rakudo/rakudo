
The following text (section *Damian's reply*) is a verbatim quote from
an e-mail [1] received from Damian Conway in response to a query for
his view on proper handling of text in declarator blocks.

Note the verbatim text has been slightly edited by additional
formatting (but no word changes) for this Markdown document.

# Damian's reply

You asked:

> what do you think is appropriate treatment of blank lines between lines of text?

In my opinion, declarator comments should preserve all whitespace on
every line (including the terminating newline and any empty lines),
unchanged.

The reasoning is simple: if the whitespace is preserved exactly then
it can be retrieved exactly. And, if the user wants the text as a
single-line string (i.e. as it is currently supplied), then they can
get that very easily with just:

    my $single_line_text  =  &foo.WHY.words.join(' ');

But if the whitespace is removed during compilation (as it currently
is), then there is no possible way to get back the original structure
of the comment text. And we have no idea whether or not that structure
was important (as it might be of the comment is something like a
table).

So, in order to maximize the information that the user has access to,
the right choice is to preserve all the whitespace (including newlines
and empty lines).

The attached .raku file is a small test case that summarizes my
expectations of how declarator blocks should be processed and what
.WHY, .WHY.leading, and .WHY.trailing should return. I hope it is
helpful.

# His example program that was attached to the e-mail:

See [declarator_block_expectations.raku](./declarator_block_expectations.raku).

# References

1. E-mail =>
    From: Damian Conway <damian@conway.org>
    Date: Sun, 24 May 2020 05:11:12 +0000
    Message-ID: <CAATtAp7btwPGjE=YYnXZFxEpStZqNkzj5vKkuApZ6y76QYp3Mg@mail.gmail.com>
    Subject: Re: S26: Leading and trailing declarator blocks
    To: Tom Browder <tom.browder@gmail.com>
