# This role is for metaclasses with languare-revision dependent behavior.
role Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Versioning
{
    has $!lang_rev;

    # The only allowed version format is 6.X
    method set_language_version($obj, $ver) {
        (nqp::iseq_i(nqp::chars($ver), 3) && nqp::eqat($ver, '6.', 0))
            || nqp::die("Language version must be a string in '6.<rev>' format, got `$ver`.");
        self.set_ver($obj, $ver);
        $!lang_rev := nqp::substr($ver, 2, 1);
    }

    method lang-rev-before($rev) {
        nqp::iseq_i(nqp::chars($rev), 1)
            || nqp::die("Language revision must be a single letter, got `$rev`.");
        nqp::iseq_i(nqp::cmp_s($!lang_rev, $rev), -1)
    }
}
