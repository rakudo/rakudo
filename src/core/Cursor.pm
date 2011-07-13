my class Cursor does NQPCursorRole { 
#    method MATCHBUILD(|$) {
#        my Mu $args := pir::perl6_current_args_rpa__P();
#        nqp::shift($args);
#        my Mu $orig := nqp::shift($args);
#        my Mu $from := nqp::shift($args);
#        my Mu $to := nqp::shift($args);
#        my Mu $capsiter := nqp::iterator(nqp::shift($args));
#        my Mu $list := nqp::list();
#        my Mu $hash := nqp::hash();
#        while $capsiter {
#            pir::say('iter');
#            my Mu $pair := nqp::shift($capsiter);
#            my Mu $key := $pair.key;
#            my Mu $value := $pair.value;
#            pir::say($key);
#            $value := nqp::p6list($value, List, Bool::True)
#                if pir::isa__IPs($value, 'ResizablePMCArray');
#            nqp::iscclass(pir::const::CCLASS_NUMERIC, $key, 0)
#              ?? nqp::bindpos($list, $key, $value)
#              !! nqp::bindkey($list, $key, $value);
#        }
#        Match.new(:$orig, :$from, :$to,
#                    :list(nqp::p6list($list, List, Mu)))
#    }
}
