package org.perl6.rakudo;

import org.perl6.nqp.runtime.ThreadContext;
import org.perl6.nqp.sixmodel.SixModelObject;

/**
 * Contains implementation of nqp:: ops specific to Rakudo Perl 6.
 */
public final class Ops {
    public static SixModelObject p6settypes(SixModelObject conf, ThreadContext tc) {
        return conf;
    }
}
