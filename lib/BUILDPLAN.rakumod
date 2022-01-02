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
my @ps = "","_i","_n","_s";

# HOW name mapping to syntax
my %HOW2syntax = (
  "Perl6::Metamodel::ClassHOW"               => 'class',
  "Perl6::Metamodel::EnumHOW"                => 'enum',
  "Perl6::Metamodel::GrammarHOW"             => 'grammar',
  "Perl6::Metamodel::ParametricRoleGroupHOW" => 'role',
);
sub HOW(\obj --> Str:D) {
    %HOW2syntax{obj.HOW.^name} // obj.HOW.^name.substr(18)
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
        @steps.kv.map( -> \step, $text { sprintf "%2d: $text", step } )
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
#    4 class attr_name code = call default value closure if needed
#    5 class attr_name code = call default value closure if needed, int attr
#    6 class attr_name code = call default value closure if needed, num attr
#    7 class attr_name code = call default value closure if needed, str attr
#    8 die if a required attribute is not present
#    9 class attr_name code = run attribute container initializer
#   10 class attr_name = touch/vivify attribute if part of mixin
#   11 same as 0, but init to nqp::list if value absent (nqp only)
#   12 same as 0, but init to nqp::hash if value absent (nqp only)
#   13 same as 0 but *bind* the received value + optional type constraint
#   14 same as 4 but *bind* the default value + optional type constraint
#===================================================

# description of the action of a single op
sub showop(@actions --> Str:D) {
    my $op   := @actions[0];
    my $type := @actions[1].^name;
    my $attr := "'@actions[2]'";

    if $op < 0 {
        "Don't know how to handle: {@actions.raku}"
    }
    elsif $op == 0 {
        "nqp::getattr(obj,$type,$attr) = :\$@actions[3] if possible"
    }
    elsif $op < 4 {
        "nqp::bindattr@ps[$op]\(obj,$type,$attr,:\$@actions[3]) if possible"
    }
    elsif $op == 4 {
        "nqp::getattr(obj,$type,$attr) = "
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ " if not set"
    }
    elsif $op < 8 {
        "nqp::bindattr@ps[$op - 4]\(obj,$type,$attr,"
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ ") if not set"
    }
    elsif $op == 8 {
        my $reason = @actions[3] === 1
          ?? "it is required"
          !! @actions[3];
        qq/die "because $reason" unless nqp::attrinited(obj,$type,$attr)/
    }
    elsif $op == 9 {
        "nqp::getattr(obj,$type,$attr) := initializer-code()"
    }
    elsif $op == 10 {
        "vivify nqp::getattr(obj,$type,$attr) if part of a mixin"
    }
    elsif $op < 13 {
        "nqp::getattr@ps[$op]\(obj,$type,$attr) //:= nqp::"
          ~ $op == 11 ?? 'list' !! 'hash'
    }
    elsif $op == 13 {
        @actions == 5
          ?? "nqp::bindattr\(obj,$type,{@actions[4].^name} $attr,:\$@actions[3]) if specified"
          !! "nqp::bindattr\(obj,$type,$attr,:\$@actions[3]) if specified"
    }
    elsif $op == 14 {
        my $attrspec = @actions == 5
          ?? "{@actions[4].^name} $attr"
          !! $attr;
        "nqp::bindattr(obj,$type,$attrspec,"
          ~ (nqp::istype(@actions[3],Callable)
              ?? "execute-code()"
              !! @actions[3].raku)
          ~ ") if not set"
    }
    else {
        "Don't know how to handle: {@actions.raku}"
    }
}

sub EXPORT(*@classes) {
    note BUILDPLAN(nqp::isconcrete($_) ?? ::($_) !! $_)
      for @classes;

    EXPORT::DEFAULT::
}

# vim: expandtab shiftwidth=4
