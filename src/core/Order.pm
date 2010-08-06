package Order {
    INIT {
        # Order::Increase, Order::Same, Order::Decrease
        # we'd like this to be an enum some day
        pir::set_global__vsP('Increase', -1);
        pir::set_global__vsP('Same', 0);
        pir::set_global__vsP('Decrease', 1);
    }
}
