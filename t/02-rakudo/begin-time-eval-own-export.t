use MONKEY-SEE-NO-EVAL;
use Test;

plan 2;

{
    my package EXPORT { our sub in-nested-export { } }
    ok (BEGIN EVAL(q{BEGIN ::('EXPORT')}).WHO<&in-nested-export>:exists),
      'BEGIN-time code in a BEGIN-time EVAL finds an EXPORT declared around the EVAL';
}
ok (BEGIN EVAL(q{my package EXPORT { our sub in-eval-export { } }; BEGIN ::('EXPORT')}).WHO<&in-eval-export>:exists),
  'BEGIN-time code in a BEGIN-time EVAL finds the EXPORT the EVAL declares';
