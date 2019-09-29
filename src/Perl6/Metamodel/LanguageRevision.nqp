# This role is for metaclasses with language-revision dependent behavior.
role Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Versioning
{
    has $!lang_rev;

    # The only allowed version format is 6.X
    method set_language_version($obj, $ver = NQPMu, :$force = 0) {
        if nqp::isconcrete($ver) {
            nqp::die("Language version must be a string in '6.<rev>' format, got `$ver`.")
                unless (nqp::iseq_i(nqp::chars($ver), 3) && nqp::eqat($ver, '6.', 0))
        }
        elsif nqp::getcomp('perl6') {
            # NOTE: It turns out that nqp::getcomp path for obtaining the language version isn't reliable as sometimes
            # language_version method report wrong version.
            my $rev;
            # $*W cannot be used at optimization stage.
            if $*W && !$*OPTIMIZER-SYMBOLS {
                $rev := $*W.find_symbol(['CORE-SETTING-REV'], :setting-only) || $*W.setting_revision;
            }
            $ver := nqp::p6clientcorever()                      # 1st: try the run-time code
                    || ($rev && '6.' ~ $rev)                    # 2nd: compile-time if CORE is available
                    || nqp::getcomp('perl6').language_version;  # otherwise try the compiler
        }
        else {
            return
        }
        if ($*COMPILING_CORE_SETTING || $force) && !self.ver($obj) {
            self.set_ver($obj, $ver);
        }
        $!lang_rev := nqp::substr($ver, 2, 1);
    }

    method set_language_revision($obj, $rev, :$force = 0) {
        if nqp::isconcrete($rev) {
            if nqp::chars($rev) != 1
                || nqp::islt_s($rev, 'c')
                || nqp::isgt_s($rev, 'z')
            {
                nqp::die("Language revision must be a char between 'c' and 'z'");
            }
            self.set_language_version($obj, "6.$rev", :$force)
        }
        else {
            nqp::die("Language revision must be a concrete string");
        }
    }

    method lang-rev-before($rev) {
        nqp::iseq_i(nqp::chars($rev), 1)
            || nqp::die("Language revision must be a single letter, got `$rev`.");
        nqp::iseq_i(nqp::cmp_s($!lang_rev, $rev), -1)
    }

    method language-revision($obj) {
        $!lang_rev
    }
}
