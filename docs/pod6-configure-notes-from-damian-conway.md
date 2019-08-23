
The following text (section *Damian's reply*) is a verbatim quote from
an e-mail [1] received from Damian Conway in response to a query for
his view on legitimate values for pod configuration keys.

Note the verbatim text has been slightly edited by additional
formatting for this Markdown document plus some minor typographical
edits indicated by *[sic]*.

# Damian's reply

My intention was always that the meaning of adverbials in Perl 6 Pod
should be exactly the same as everywhere else in Perl 6. Hence:

```
:a{}              ---> 'a' => %empty-hash
:b()              ---> 'b' => ()
:c('')            ---> 'c' => ''
:d[]              ---> 'd' => @empty-array
:e<>              ---> 'e' => ''
:f{f=> 4, g=> ''} ---> 'f' => { 'f'=>4, 'g' => '' }
```
So they are all perfectly valid, in my view.

As to whether they are *meaningful* or *useful*...that depends
entirely on the actual key of the adverbial (and sometimes, perhaps,
on the block type they're attached to).

I think the correct answer is that we should think of the values of
these adverbials as being strictly typed, rather than coercively
typed.

For example, the *:numbered* option should be thought of as having the
underlying value type of Bool, or in actual Perl 6 notation [sic]:

    subset Numbered of Pair where *.value ~~ Bool;

So any argument other than a strict Bool value should be a
compile-time error.  Hence:

    =head2 :numbered{}
    =head2 :numbered[]
    =head2 :numbered<>
    =head2 :numbered()
    =head2 :numbered('')
    =head2 :numbered{f=>4, g=>''}

...should all die with something like:

    Invalid :numbered option. Expected Bool value but found Hash: {}
    Invalid :numbered option. Expected Bool value but found Array: []
    Invalid :numbered option. Expected Bool value but found String: <>
    Invalid :numbered option. Expected Bool value but found List: ()
    Invalid :numbered option. Expected Bool value but found String: ''
    Invalid :numbered option. Expected Bool value but found Hash: {f=>4, g=>''}

In the same way, the following options should all be thought of as
taking values of the following type:

    Option         Allowed values
    ==========     =====================

    :caption       Str

    :margin        Str where /^ <+print -space> $/

    :nested        Bool  |  Int where 0..*

    :numbered      Bool

    :continued     Bool

    :allow         Positional[Str where &is-valid-formatting-code]
                            | Str where &is-valid-formatting-code

    :formatted     Positional[Str where &is-valid-formatting-code]
                            | Str where &is-valid-formatting-code

    :like          Positional[Str where &is-existing-block-name]
                            | Str where &is-existing-block-name


Note that this means, for consistency, that "plausible" configuration
options such as *:numbered(1)* or *:continued(0)* should throw exceptions
too:

    Invalid :numbered option. Expected Bool value but found Int: 1
    (Did you mean :numbered or :numbered(True)?)

    Invalid :continued option. Expected Bool value but found Int: 0
    (Did you mean :!continued or :continued(False)?)


It also means that an "empty" option value is still sometimes
acceptable:

    :caption('')      Explicitly specify table has no caption

    :allow<>          Don't allow any formatting codes in block

    :formatted<>      Explicitly specify no block-wide formatting


Whereas other "empty" values are never acceptable:

    :like('')         die q{:like option expected a block type but found: ""}

    :margin<>         die q{:margin option expected visible margin character but found: <>}

    :margin(' ')      die q{:margin option expected visible margin character but found: ' '}


I imagine this is not the answer you were hoping for, as it's much
easier to just ban every "empty" configuration value...and maybe it's
even more comprehensible to do so.

If you feel that the above guidance is too complicated or unwieldy, or
will not reduce the general levels of confusion, then I can accept a
blanket ban on "empty" values (for the present).

However, if you decide to go that route, I fear that you will merely
replace one set of "this was unexpected" complaints with a different
set of "this was unexpected" complaints. Because DWIM is always hard
(and often impossible) to achieve universally.

I am, of course, more than happy to discuss or debate this further (or
to clarify my clarification, if I wasn't sufficiently clear this time
round ;-)

# References

1. E-mail from Damian Conway (@thoughtstream;
   <thoughtstream@gmail.com>) to Tom Browder (@tbrowder;
   <tom.browder@gmail.com>); Aug 9, 2019, 6:41 PM; subject: Re: S26
   and pod key values.
