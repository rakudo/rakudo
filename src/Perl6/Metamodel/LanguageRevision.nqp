# This role is for metaclasses with languare-revision dependent behavior.
role Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Versioning
{
    has $!lang_rev;

    # The only allowed version format is 6.X
    method set_language_version($obj, $ver = NQPMu) {
        # Don't override if version is already set
        if self.ver($obj) {
            return
        }
        if nqp::isconcrete($ver) {
            nqp::die("Language version must be a string in '6.<rev>' format, got `$ver`.")
                unless (nqp::iseq_i(nqp::chars($ver), 3) && nqp::eqat($ver, '6.', 0))
        }
        else {
            # NOTE: It turns out that nqp::getcomp path for obtaining the language version isn't reliable as sometimes
            # language_version method report wrong version.
            my $rev;
            if $*W {
                $rev := $*W.find_symbol(['CORE-SETTING-REV'], :setting-only) || $*W.setting_revision;
            }
            $ver := nqp::p6clientcorever()                      # 1st: try the run-time code
                    || ($rev && '6.' ~ $rev)                    # 2nd: compile-time if CORE is available
                    || nqp::getcomp('perl6').language_version;  # otherwise try the compiler
        }
        self.set_ver($obj, $ver);
        $!lang_rev := nqp::substr($ver, 2, 1);
    }

    method lang-rev-before($rev) {
        nqp::iseq_i(nqp::chars($rev), 1)
            || nqp::die("Language revision must be a single letter, got `$rev`.");
        nqp::iseq_i(nqp::cmp_s($!lang_rev, $rev), -1)
    }
}
