unit module ConstantCallAssign;

our class P { has $.v }

our P constant closure-argument .= new(v => * + 1);
