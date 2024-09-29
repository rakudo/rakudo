#- Metamodel::Mixins -----------------------------------------------------------
role Perl6::Metamodel::Mixins {
    has     $!mixin_cache;
    has int $!is_mixin;
    has     $!mixin_attribute;

    method set_is_mixin()                  { $!is_mixin := 1                 }
    method set_mixin_attribute($attribute) { $!mixin_attribute := $attribute }

    method is_mixin($XXX?)        { $!is_mixin        }
    method mixin_attribute($XXX?) { $!mixin_attribute }
    method flush_cache($XXX?)     {                   }

    method setup_mixin_cache($target) {

        my class MixinCacheHOW {
            method new_type($class) {
                my $type := nqp::newtype(nqp::create(self), 'Uninstantiable');

                nqp::setparameterizer($type, sub ($type, @roles) {
                    $class.HOW.generate_mixin($class, @roles);
                });

                nqp::setdebugtypename(
                  $type, $class.HOW.name($class) ~ ' mixin cache'
                );

                $type
            }
        }

        $!mixin_cache := MixinCacheHOW.new_type($target.WHAT);
    }

    method mixin($target, *@roles, :$need-mixin-attribute) {

        # Lookup mixin, generating it if needed.
        my int $m := nqp::elems(@roles);
        my int $i;
        # Get rid of containers on roles
        while $i < $m {
            nqp::bindpos(@roles, $i, nqp::decont(nqp::atpos(@roles, $i)));
            ++$i;
        }

        my $mixin_type := nqp::parameterizetype($!mixin_cache, @roles);
        nqp::setdebugtypename(
          $mixin_type, $mixin_type.HOW.name($mixin_type) ~ ' mixin'
        );

        # Ensure there's a mixin attribute, if we need it.
        self.no_single_attribute(nqp::atpos(@roles, 0))
          if $need-mixin-attribute
          && !$mixin_type.HOW.mixin_attribute($mixin_type);

        # If the original object was concrete, change its type by calling a
        # low level op. Otherwise, we just return the new type object
        nqp::isconcrete($target)
          ?? nqp::rebless($target, $mixin_type)
          !! $mixin_type
    }

    # Generates a new mixin. Not intended for direct use; use mixin, to hit
    # the mixin cache.  Assumes it is being run in a threadsafe manner.
    method generate_mixin($target, @roles) {
        # Flush its cache as promised, otherwise outdated NFAs will stick
        # around.
        self.flush_cache($target)
          if nqp::not_i(nqp::isnull($target)) || self.is_mixin($target);

        # Work out a type name for the post-mixed-in role.
        my $names      := nqp::list_s;
        my $shortnames := nqp::list_s;
        my $lang_rev   := nqp::getcomp('Raku').language_revision;

        my int $m := nqp::elems(@roles);
        my int $i;
        while $i < $m {
            my $role := nqp::atpos(@roles, $i);
            nqp::push_s($names,      ~$role.HOW.name($role));
            nqp::push_s($shortnames, ~$role.HOW.shortname($role));
            if nqp::can($role.HOW, 'language_revision') {
                my $role_lang_rev := $role.HOW.language_revision;
                $lang_rev := $role_lang_rev if $lang_rev < $role_lang_rev;
            }

            ++$i;
        }

        # Helper sub to create a name for given role names
        my sub namify(@parts) { '+{' ~ nqp::join(',', @parts) ~ '}' }

        my $name      := self.name($target)      ~ namify($names);
        my $shortname := self.shortname($target) ~ namify($shortnames);

        # Create new type, derive it from ourself and then add
        # all the roles we're mixing it.
        my $type := self.new_type(:$name, :repr($target.REPR), :is_mixin);
        my $HOW  := $type.HOW;

        $HOW.set_is_mixin;
        $HOW.set_language_revision($type, $lang_rev);
        $HOW.add_parent($type, $target.WHAT);

        $i := 0;
        while $i < $m {
            $HOW.add_role($type, nqp::atpos(@roles, $i));
            ++$i;
        }

        $HOW.compose($type);
        $HOW.set_shortname($type, $shortname);
        $HOW.set_boolification_mode(
          $type,
          $HOW.declares_method($type, 'Bool')
            ?? 0
            !! self.get_boolification_mode($target)
        );
        $HOW.publish_boolification_spec($type);

        # Locate an attribute that can serve as the initialization attribute,
        # if there is only one.
        my $is_built;
        my $attributes := $HOW.attributes($type, :local);

        $m := nqp::elems($attributes);
        $i := 0;
        while $i < $m {
            my $attribute := nqp::atpos($attributes, $i);
            if $attribute.is_built {
                $is_built                       # not first attribute built?
                  ?? (return $type)             # done, return here and now
                  !! ($is_built := $attribute)  # first attribute seen
            }
            ++$i;
        }

        # If we got here, there's only one attribute to be built
        $type.HOW.set_mixin_attribute($is_built);

        $type
    }

    method mixin_base($target) {
        my $mro := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $class := nqp::atpos($mro, $i);
            $class.HOW.is_mixin
              ?? ++$i
              !! (return $class);
        }
    }

    method no_single_attribute($role) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Initialization',
          "Can only supply an initialization value for a role if it has a single public attribute, but this is not the case for '"
            ~ $role.HOW.name($role)
            ~ "'",
          :$role
        );
    }
}

# vim: expandtab sw=4
