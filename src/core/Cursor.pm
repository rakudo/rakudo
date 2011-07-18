my class Cursor does NQPCursorRole { 
    method MATCH() {
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        my Mu $caphash := pir::find_method__PPs(Cursor, 'CAPHASH')(self);
        my Mu $capiter := nqp::iterator($caphash);
        while $capiter {
            my Mu $pair := nqp::shift($capiter);
            my str $key = $pair.key;
            my Mu $value := $pair.value;
            $value := nqp::p6list($value, List, Mu)
                if pir::isa__IPs($value, 'ResizablePMCArray');
            nqp::iscclass(pir::const::CCLASS_NUMERIC, $key, 0)
              ?? nqp::bindpos($list, $key, $value)
              !! nqp::bindkey($hash, $key, $value);
        }
        my $match := Match.new(
            orig   => nqp::getattr(self, Cursor, '$!orig'),
            from   => nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!from')),
            to     => nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!pos')),
        );
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        $match;
    }
}

