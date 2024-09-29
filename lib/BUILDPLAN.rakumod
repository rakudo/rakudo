# This module is primarily intended for debugging the object creation
# internals, specifically the so-called BUILDPLAN and BUILDALLPLAN, as
# created by src/Perl6/Metamodel/BUILDPLAN.nqp.
#
# If you're just interested in seeing the BUILDPLAN of one or more
# classes, you can specify these classes as arguments to "use":
#
#    $ raku -e 'use BUILDPLAN Instant'
#    class Instant BUILDPLAN:
#     0: nqp::getattr(obj,Instant,'$!tai') = :$tai if possible
#     1: vivify nqp::getattr(obj,Instant,'$!tai') if part of a mixin

use nqp;

# primspec extensions to nqp::getattr
my constant @ps = "","_i","_n","_s","","","","","","","_u";
# primspec mapping to reason for failure if absent
my constant @nr = "","== 0","== 0e0","nqp::isnull_s","","","","","","","== 0";

# HOW name mapping to syntax
my %HOW2syntax = (
  "Perl6::Metamodel::ClassHOW"               => 'class',
  "Perl6::Metamodel::EnumHOW"                => 'enum',
  "Perl6::Metamodel::GrammarHOW"             => 'grammar',
  "Perl6::Metamodel::ParametricRoleGroupHOW" => 'role',
);
sub HOW(\obj --> Str:D) {
    my $name := obj.HOW.^name;
    %HOW2syntax{$name} // $name.subst("Perl6::Metamodel::")
}

# text of BUILDPLAN of given object/type
sub BUILDPLAN(\obj --> Str:D) is export {
    object-description(obj,'BUILDPLAN')
      ~ ":\n"
      ~ build-description(HOW(obj) eq 'role'
          ?? obj.^pun.^BUILDPLAN
          !! obj.^BUILDPLAN
        )
}

# text of BUILDALLPLAN of given object/type
sub BUILDALLPLAN(\obj --> Str:D) is export {
    object-description(obj,'BUILDALLPLAN')
      ~ ":\n"
      ~ (build-description(HOW(obj) eq 'role'
           ?? obj.^pun.^BUILDALLPLAN
           !! obj.^BUILDALLPLAN
         ) || "No actions found")
}

# description of given object/type
sub object-description($obj, $name --> Str:D) is export {
    "&HOW($obj) $obj.^name() $name"
}

# array with build steps of given BUILDPLAN/BUILDALLPLAN
sub build-steps(@plan) is export {
    my @steps;

    for @plan.kv -> $i, \action {
        @steps.push:
          nqp::istype(action,List)
            ?? showop(action)
            !! "call obj.{ action.package.^name }::{ action.name }"
    }

    @steps
}

# description of given BUILDPLAN/BUILDALLPLAN
sub build-description(@plan --> Str:D) is export {
    if build-steps(@plan) -> @steps {
        @steps.kv.map( -> \step, $text { sprintf('%2d ', step) ~ $text } )
        .join("\n")
    }
    else {
        "No actions found"
    }
}

#====== from src/Perl6/Metamodel/BUILDPLAN.nqp =====
# The plan is an array of code objects / arrays. If the element
# is a code object, it should be called as a method without any
# further parameters.  If it is an array, then the first element
# of each array is an "op" # representing the task to perform:
#   code = call as method (for BUILD or TWEAK)
#    0 class name attr_name = set attribute from init hash
#    1 class name attr_name = set a native int attribute from init hash
#    2 class name attr_name = set a native num attribute from init hash
#    3 class name attr_name = set a native str attribute from init hash
#   10 class name attr_name = set a native uint attribute from init hash
#  400 class attr_name code = call default value closure if needed
#  401 class attr_name code = call default value closure if needed, int attr
#  402 class attr_name code = call default value closure if needed, num attr
#  403 class attr_name code = call default value closure if needed, str attr
#  410 class attr_name code = call default value closure if needed, uint attr
#  800 die if a required attribute is not present
#  900 class attr_name code = run attribute container initializer
# 1000 class attr_name = touch/vivify attribute if part of mixin
# 1100 same as 0, but init to nqp::list if value absent (nqp only)
# 1200 same as 0, but init to nqp::hash if value absent (nqp only)
# 1300 same as 0 but *bind* the received value + optional type constraint
# 1400 same as 400 but *bind* the default value + optional type constraint
# 1501 die if a required int attribute is 0
# 1502 die if a required num attribute is 0e0
# 1503 die if a required str attribute is null_s (will be '' in the future)
# 1510 die if a required uint attribute is 0
#===================================================

# description of the action of a single op
sub showop(@actions --> Str:D) {
    my $op   := @actions[0];
    my $type := @actions[1].^name;
    my $attr := "'@actions[2]'";

    if $op == 0 {
        "nqp::getattr(obj,$type,$attr) = :\$@actions[3] if possible"
    }
    elsif $op == 1 | 2 | 3 | 10 {
        "nqp::bindattr@ps[$op]\(obj,$type,$attr,:\$@actions[3]) if possible"
    }
    elsif $op == 400 {
        "nqp::getattr(obj,$type,$attr) = "
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ " if not set"
    }
    elsif $op == 401 | 402 | 403 | 410 {
        "nqp::bindattr@ps[$op - 400]\(obj,$type,$attr,"
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ ") if not set"
    }
    elsif $op == 800 {
        my $reason = @actions[3] == 1
          ?? "it is required"
          !! @actions[3];
        qq/die "because $reason" if @actions[2] has not been initialized/
    }
    elsif $op == 900 {
        "nqp::getattr(obj,$type,$attr) := initializer-code()"
    }
    elsif $op == 1000 {
        "vivify nqp::getattr(obj,$type,$attr) if part of a mixin"
    }
    elsif $op == 1100 {
        "nqp::getattr\(obj,$type,$attr) //:= nqp::list"
    }
    elsif $op == 1200 {
        "nqp::getattr\(obj,$type,$attr) //:= nqp::hash"
    }
    elsif $op == 1300 {
        @actions == 5
          ?? "nqp::bindattr\(obj,$type,{@actions[4].^name} $attr,:\$@actions[3]) if specified"
          !! "nqp::bindattr\(obj,$type,$attr,:\$@actions[3]) if specified"
    }
    elsif $op == 1400 {
        my $attrspec = @actions == 5
          ?? "{@actions[4].^name} $attr"
          !! $attr;
        "nqp::bindattr(obj,$type,$attrspec,"
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ ") if not set"
    }
    elsif $op == 1501 | 1502 | 1503 | 1510 {
        my $reason = @actions[3] == 1
          ?? "it is required"
          !! @actions[3];
        qq/die "because $reason" if @actions[2] @nr[$op - 1500]/
    }
    else {
        "Don't know how to handle: @actions.raku()"
    }
}

sub EXPORT(*@classes) {
    note BUILDPLAN(nqp::isconcrete($_) ?? ::($_) !! $_)
      for @classes;

    EXPORT::DEFAULT::
}

# vim: expandtab shiftwidth=4
