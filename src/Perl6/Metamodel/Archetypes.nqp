use Perl6::Ops;

#- Metamodel::Archetypes -------------------------------------------------------
# Provides various properties of the type of type a given meta-object
# implements. This are used in various ways by the compiler and meta-model
# to do correct code generation or to detect illegal use of types in
# contexts with certain requirements.
class Perl6::Metamodel::Archetypes {
    # Can this serve as a nominal type? Implies memoizability
    # amongst other things.
    has int $!nominal;

    # If it's not nominal, does it know how to provide a nominal
    # type part of itself?
    has int $!nominalizable;

    # Can this be inherited from?
    has int $!inheritable;

    # If it's not inheritable, does it know how to produce something
    # that is?
    has int $!inheritalizable;

    # Can this be composed (either with flattening composition, or used
    # as a mixin)?
    has int $!composable;

    # If it's not composable, does it know how to produce something
    # that is?
    has int $!composalizable;

    # Is it generic, in the sense of "we don't know what type this is
    # yet"? Note that a parametric type would not be generic - even if
    # it has missing parts, it defines a type. A type variable is generic,
    # however. This tends to cause various kinds of late (or at least
    # delayed) reification. In some contexts, an unresolved generic is
    # fatal.
    has int $!generic;

    # Is it a parametric type - that is, it has missing bits that need
    # to be filled out before it can be used? Unlike generic, something
    # that is parametric does define a type - though we may need the gaps
    # filled it before it's useful in some way.
    has int $!parametric;

    # Is it a coercive type?
    has int $!coercive;

    # Is it a definite type?  In other words: does definiteness needs to
    # be checked in typechecks (so either :D or :U, *not* :_, which means
    # that we're not interested in definiteness)
    has int $!definite;

    # Are we allowed to augment the type?
    has int $!augmentable;

    # Create archetype from its arguments
    method new(*%_) {
        my $obj := nqp::create(self);

        nqp::bindattr_i($obj, self, '$!nominal', 1)
          if nqp::atkey(%_, 'nominal');
        nqp::bindattr_i($obj, self, '$!nominalizable', 1)
          if nqp::atkey(%_, 'nominalizable');
        nqp::bindattr_i($obj, self, '$!inheritable', 1)
          if nqp::atkey(%_, 'inheritable');
        nqp::bindattr_i($obj, self, '$!inheritalizable', 1)
          if nqp::atkey(%_, 'inheritalizable');
        nqp::bindattr_i($obj, self, '$!composable', 1)
          if nqp::atkey(%_, 'composable');
        nqp::bindattr_i($obj, self, '$!composalizable', 1)
          if nqp::atkey(%_, 'composalizable');
        nqp::bindattr_i($obj, self, '$!generic', 1)
          if nqp::atkey(%_, 'generic');
        nqp::bindattr_i($obj, self, '$!parametric', 1)
          if nqp::atkey(%_, 'parametric');
        nqp::bindattr_i($obj, self, '$!coercive', 1)
          if nqp::atkey(%_, 'coercive');
        nqp::bindattr_i($obj, self, '$!definite', 1)
          if nqp::atkey(%_, 'definite');
        nqp::bindattr_i($obj, self, '$!augmentable', 1)
          if nqp::atkey(%_, 'augmentable');

        $obj
    }

    method nominal()         { $!nominal         }
    method nominalizable()   { $!nominalizable   }
    method inheritable()     { $!inheritable     }
    method inheritalizable() { $!inheritalizable }
    method composable()      { $!composable      }
    method composalizable()  { $!composalizable  }
    method generic()         { $!generic         }
    method parametric()      { $!parametric      }
    method coercive()        { $!coercive        }
    method definite()        { $!definite        }
    method augmentable()     { $!augmentable     }

    # Shortcuts to often occurring checks
    method nominalish()      { $!nominal     || $!nominalizable   }
    method inheritablish()   { $!inheritable || $!inheritalizable }
    method composablish()    { $!composable  || $!composalizable  }
}

# vim: expandtab sw=4
