class Whatever is also {
    method perl() {
        return '*';
    }
    method ACCEPTS(Any $topic) {
        return Bool::True;
    }
}


# See src/classes/.pir for a description of the WhateverCodeX()
# function.  Essentially it does the equivalent of creating a
# WhateverCode closure.  Ideally we'd prefer
#    multi sub infix:<+>(Whatever $a, $b) {
#        WhateverCode(-> $_ { $_ + $b })
#    }
# but Rakudo doesn't support block coercion yet, so we have
# a helper function to build it for us.

multi sub infix:<+>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:+', $a, $b) }
multi sub infix:<+>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:+', $a, $b) }
multi sub infix:<+>($a, Whatever $b)
    { WhateverCodeX('infix:+', $a, $b) }
multi sub infix:<+>($a, Whatever $b)
    { WhateverCodeX('infix:+', $a, $b) }

multi sub infix:<->(Whatever $a, $b) is default 
    { WhateverCodeX('infix:-', $a, $b) }
multi sub infix:<->(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:-', $a, $b) }
multi sub infix:<->($a, Whatever $b)
    { WhateverCodeX('infix:-', $a, $b) }
multi sub infix:<->($a, Whatever $b)
    { WhateverCodeX('infix:-', $a, $b) }

multi sub infix:<*>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:*', $a, $b) }
multi sub infix:<*>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:*', $a, $b) }
multi sub infix:<*>($a, Whatever $b)
    { WhateverCodeX('infix:*', $a, $b) }
multi sub infix:<*>($a, Whatever $b)
    { WhateverCodeX('infix:*', $a, $b) }

multi sub infix:</>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:/', $a, $b) }
multi sub infix:</>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:/', $a, $b) }
multi sub infix:</>($a, Whatever $b)
    { WhateverCodeX('infix:/', $a, $b) }
multi sub infix:</>($a, Whatever $b)
    { WhateverCodeX('infix:/', $a, $b) }

multi sub infix:<%>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:%', $a, $b) }
multi sub infix:<%>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:%', $a, $b) }
multi sub infix:<%>($a, Whatever $b)
    { WhateverCodeX('infix:%', $a, $b) }
multi sub infix:<%>($a, Whatever $b)
    { WhateverCodeX('infix:%', $a, $b) }

multi sub infix:<div>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:div', $a, $b) }
multi sub infix:<div>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:div', $a, $b) }
multi sub infix:<div>($a, Whatever $b)
    { WhateverCodeX('infix:div', $a, $b) }
multi sub infix:<div>($a, Whatever $b)
    { WhateverCodeX('infix:div', $a, $b) }

multi sub infix:<mod>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:mod', $a, $b) }
multi sub infix:<mod>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:mod', $a, $b) }
multi sub infix:<mod>($a, Whatever $b)
    { WhateverCodeX('infix:mod', $a, $b) }
multi sub infix:<mod>($a, Whatever $b)
    { WhateverCodeX('infix:mod', $a, $b) }

multi sub infix:<**>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>($a, Whatever $b)
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>($a, Whatever $b)
    { WhateverCodeX('infix:**', $a, $b) }

multi sub prefix:<->(Whatever $a)
    { WhateverCodeX('prefix:-', $a) }
multi sub prefix:<->(WhateverCode $a)
    { WhateverCodeX('prefix:-', $a) }

multi sub prefix:<~>(Whatever $a)
    { WhateverCodeX('prefix:~', $a) }
multi sub prefix:<~>(WhateverCode $a)
    { WhateverCodeX('prefix:~', $a) }

# vim: ft=perl6
