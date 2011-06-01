class RangeIter is Iterator {
    has $!value;
    has $!max;
    has $!excludes_max;
    has $!reify;

    method infinite() { $!max eqv Inf }

    method reify() {
        Q:PIR {
            .local pmc self, reify, value, max, excl_max
            self = find_lex 'self'
            reify = getattribute self, '$!reify'
            if null reify goto reify_start
            $I0 = defined reify
            if $I0 goto reify_done
          reify_start:
            reify = new ['Parcel']
            setattribute self, '$!reify', reify
            value    = getattribute self, '$!value'
            max      = getattribute self, '$!max'
            excl_max = getattribute self, '$!excludes_max'
            .local int count
            count = 8
          reify_loop:
            $I0 = '&infix:<cmp>'(value, max)
            if $I0 < 0 goto reify_value
            if $I0 > 0 goto reify_done
            if excl_max goto reify_done
            push reify, value
            goto reify_done
          reify_value:
            push reify, value
            value = value.'succ'()
            dec count
            if count > 0 goto reify_loop
            .local pmc nextiter
            nextiter = new ['RangeIter']
            setattribute nextiter, '$!value', value
            setattribute nextiter, '$!max', max
            setattribute nextiter, '$!excludes_max', excl_max
            push reify, nextiter
          reify_done:
            %r = reify
        }
    }
}
