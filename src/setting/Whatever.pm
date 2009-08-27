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

multi sub infix:<**>(Whatever $a, $b) is default 
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>(WhateverCode $a, $b) is default 
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>($a, Whatever $b)
    { WhateverCodeX('infix:**', $a, $b) }
multi sub infix:<**>($a, Whatever $b)
    { WhateverCodeX('infix:**', $a, $b) }

# vim: ft=perl6
