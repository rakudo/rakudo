package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.STable;
import org.perl6.nqp.sixmodel.SixModelObject;

public class RakudoJavaInterop extends BootJavaInterop {
    public RakudoJavaInterop(GlobalContext gc) {
        super(gc);
    }
    
    protected SixModelObject computeHOW(ThreadContext tc, String name) {
        RakOps.GlobalExt gcx = RakOps.key.getGC(tc);
        SixModelObject mo = gcx.JavaHOW.st.REPR.allocate(tc, gcx.JavaHOW.st);
        mo.bind_attribute_boxed(tc, gcx.JavaHOW, "$!name", STable.NO_HINT,
            RakOps.p6box_s(name, tc));
        return mo;
    }
}
