package org.perl6.rakudo;

import org.perl6.nqp.runtime.ThreadContext;
import org.perl6.nqp.sixmodel.*;

public class RakudoContainerConfigurer extends ContainerConfigurer {
    /* Sets this container spec in place for the specified STable. */ 
    public void setContainerSpec(ThreadContext tc, STable st) {
        st.ContainerSpec = new RakudoContainerSpec();
    }
    
    /* Configures the container spec with the specified info. */
    public void configureContainerSpec(ThreadContext tc, STable st, SixModelObject config) {
        /* Nothing to configure here. */
    }
}
