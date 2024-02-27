# This role is for metaclasses with language-revision dependent behavior.
role Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Versioning
{
    # Internal representation, where 1 stands for 'c'
    has int $!lang_rev;

    method !set_type_ver($target, $internal, :$force) {
        self.set_ver($target, nqp::getcomp('Raku').lvs.as-public-repr($internal, :as-str))
            if ($*COMPILING_CORE_SETTING || $force) && !self.ver($target);
    }

    # The only allowed version formats are 6.X or v6.X
    method set_language_version($target, $ver?, :$force = 0) {
        my @lang-ver;
        my $comp := nqp::getcomp('Raku');
        if nqp::isconcrete($ver) {
            @lang-ver := $comp.lvs.from-public-repr($ver);
        }
        elsif $!lang_rev && !$ver {
            @lang-ver.push: $!lang_rev;
        }
        elsif $comp {
            # When CORE is being compiled compiler's language revision may not represent the CORE's revision. But the
            # World's instance knows it.
            # TODO RakuAST needs different approach.
            if $*COMPILING_CORE_SETTING && $*W {
                @lang-ver.push: $*W.setting_revision;
            }
            else {
                @lang-ver.push: $comp.language_revision;
            }
        }
        else {
            return
        }
        self."!set_type_ver"($target, @lang-ver, :$force);
        $!lang_rev := @lang-ver[0] if !$!lang_rev || $ver; # Always set if $ver is explicit
    }

    method set_language_revision($target, int $rev, :$force = 0) {
        if nqp::isconcrete($rev) {
            if nqp::chars($rev) < 1 {
                nqp::die("Language revision cannot be less than 1, got " ~ $rev);
            }
            self."!set_type_ver"($target, $rev, :$force);
            $!lang_rev := $rev;
        }
        else {
            nqp::die("Language revision must be a concrete value");
        }
    }

    # Check if we're compatible with type object $type. I.e. it doesn't come from language version newer than we're
    # compatible with. For example, 6.c/d classes cannot consume 6.e roles.
    # Because there could be more than one such boundary in the future they can be passed in as an array.
    method check-type-compat($target, $type, @revs) {
        unless nqp::isnull(self.incompat-revisions($target, $!lang_rev, $type.HOW.language_revision, @revs)) {
            my $comp := nqp::getcomp('Raku');
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Language::IncompatRevisions',
                "Type object " ~ $target.HOW.name($target) ~ " of v" ~ $comp.lvs.as-public-repr($!lang_rev, :as-str)
                    ~ " is not compatible with " ~ $type.HOW.name($type)
                    ~ " of v" ~ $comp.lvs.as-public-repr($type.HOW.language_revision, :as-str),
                :type-a($target),
                :type-b($type)
            )
        }
    }

    method incompat-revisions($XXX, int $rev-a, int $rev-b, @revs) {
        for @revs -> $rev {
            if $rev-a < $rev && $rev-b >= $rev {
                return $rev
            }
        }
        nqp::null()
    }

    # Public interface to conform to S14-roles/versioning.t behavior but still maintain compatibility with numeric
    # internal representation of language revisions.
    method language-revision($XXX?) {
        my $lang-rev-type := Perl6::Metamodel::Configuration.language_revision_type;
        nqp::isnull($lang-rev-type)
            ?? $!lang_rev
            !! nqp::box_i($!lang_rev, $lang-rev-type)
    }

    # This method is a private interface always returning an int, akin to compiler's object method of the same name.
    method language_revision($XXX?) { $!lang_rev }

    method language-version($XXX?) {
        nqp::getcomp('Raku').lvs.as-public-repr: $!lang_rev, :as-str
    }
}

# vim: expandtab sw=4
