package org.raku.rakudo;

import org.raku.nqp.runtime.ThreadContext;
import org.raku.nqp.runtime.*;
import org.raku.nqp.sixmodel.*;

public class RakudoContainerConfigurer extends ContainerConfigurer {
    /* Sets this container spec in place for the specified STable. */
    public void setContainerSpec(ThreadContext tc, STable st) {
        st.ContainerSpec = new RakudoContainerSpec();
    }

    /* Configures the container spec with the specified info. */
    public void configureContainerSpec(ThreadContext tc, STable st, SixModelObject config) {
        RakudoContainerSpec cs = (RakudoContainerSpec)st.ContainerSpec;
        cs.store = grabOneValue(tc, config, "store");
        cs.storeUnchecked = grabOneValue(tc, config, "store_unchecked");
        cs.cas = grabOneValue(tc, config, "cas");
        cs.atomicStore = grabOneValue(tc, config, "atomic_store");
    }
    private static SixModelObject grabOneValue(ThreadContext tc, SixModelObject config, String key) {
        SixModelObject value = config.at_key_boxed(tc, key);
        if (value == null)
            throw ExceptionHandling.dieInternal(tc,
                "Container spec must be configured with a '" + key + "'");
        return value;
    }

}
