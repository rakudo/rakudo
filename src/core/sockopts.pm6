my role SockOptLike {
    multi method CALL-ME(Int() $sonum) {
        return self unless $sonum;
        nextsame
    }
}

my enum SockOpt does SockOptLike (
    |nqp::stmts(
        ( my \res  := nqp::list ),
        ( my \iter := nqp::iterator(nqp::getsockopts()) ),
        nqp::while(
            iter,
            nqp::stmts(
                ( my \p := nqp::p6bindattrinvres(nqp::create(Pair), Pair, '$!key', nqp::shift(iter)) ),
                nqp::bindattr(p, Pair, '$!value', nqp::abs_i(nqp::shift(iter)) ),
                nqp::push(res, p)
            )
        ),
        res
    )
);
