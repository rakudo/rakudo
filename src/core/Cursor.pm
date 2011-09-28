my class Cursor does NQPCursorRole {
    method MATCH() {
        my $match := nqp::getattr(self, Cursor, '$!match');
        return $match if pir::type_check__IPP($match, Match) && $match.defined;
        $match := Match.new(
            orig   => nqp::getattr(self, Cursor, '$!orig'),
            from   => nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!from')),
            to     => nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!pos')),
            cursor => self,
        );
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        if $match.Bool {
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
        }
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);
        $match;
    }

    method INTERPOLATE($var) {
        $var ~~ Callable ?? $var(self) !! self."!LITERAL"($var)
    }

}

sub MAKE_REGEX($arg) {
    $arg ~~ Regex ?? $arg !! eval("my \$x = anon regex \{ $arg \}")
}


