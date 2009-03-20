class Any is also {
    our Int multi method ceiling (Num $x:) is export {
        return Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $I0 = ceil $N0
            %r = box $I0
        }
    }

    our Int multi method floor (Num $x:) is export {
        return Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $I0 = floor $N0
            %r = box $I0
        }
    }

    our Int multi method round (Num $x:) is export {
        return Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N0 = $N0 + 0.5
            $I0 = floor $N0
            %r = box $I0
        }
    }
}
