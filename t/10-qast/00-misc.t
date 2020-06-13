use lib <t/packages>;
use Test::Helpers::QAST;
use Test;
use nqp;

plan 1;

# https://github.com/rakudo/rakudo/issues/1526
qast-is ｢(* + * + *)(1, 2, 3)｣, :target<ast>, -> \v {
      my $seen = 0;
      sub rake-it ($qast) {
          if nqp::istype($qast, QAST::Op) && $qast.op eq 'p6bindsig' {
              $seen++;
          }
          elsif qast-descendable $qast {
              for $qast.list { rake-it $_ }
          }
      }
      $seen ≤ 1;
}, 'whatever curries with 3+ args do not duplicate p6bindsig op';

# vim: expandtab shiftwidth=4
