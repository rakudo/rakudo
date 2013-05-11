package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;

/**
 * Contains implementation of nqp:: ops specific to Rakudo Perl 6.
 */
public final class Ops {
    private static SixModelObject Parcel;
    private static SixModelObject Code;
    private static SixModelObject Signature;
    public static SixModelObject Parameter;
    public static SixModelObject Int;
    public static SixModelObject Num;
    public static SixModelObject Str;
    public static SixModelObject Junction;
    public static SixModelObject Scalar;
    private static SixModelObject False;
    private static SixModelObject True;
    private static boolean initialized = false;
    
    /* Parameter hints for fast lookups. */
    private static final int HINT_PARCEL_STORAGE = 0;
    private static final int HINT_CODE_DO = 0;
    private static final int HINT_CODE_SIG = 1;
    private static final int HINT_SIG_PARAMS = 0;
    
    public static SixModelObject p6init(ThreadContext tc) {
        if (!initialized) {
            tc.gc.contConfigs.put("rakudo_scalar", new RakudoContainerConfigurer());
            initialized = true;
        }
        return null;
    }
    
    public static SixModelObject p6settypes(SixModelObject conf, ThreadContext tc) {
        Parcel = conf.at_key_boxed(tc, "Parcel");
        Code = conf.at_key_boxed(tc, "Code");
        Signature = conf.at_key_boxed(tc, "Signature");
        Parameter = conf.at_key_boxed(tc, "Parameter");
        Int = conf.at_key_boxed(tc, "Int");
        Num = conf.at_key_boxed(tc, "Num");
        Str = conf.at_key_boxed(tc, "Str");
        Junction = conf.at_key_boxed(tc, "Junction");
        Scalar = conf.at_key_boxed(tc, "Scalar");
        False = conf.at_key_boxed(tc, "False");
        True = conf.at_key_boxed(tc, "True");
        return conf;
    }
    
    public static SixModelObject booleanize(int x) {
        return x == 0 ? False : True;
    }
    
    public static CallSiteDescriptor p6bindsig(ThreadContext tc, CallSiteDescriptor csd, Object[] args) {
        /* Do any flattening before processing begins. */
        CallFrame cf = tc.curFrame;
        if (csd.hasFlattening) {
            csd = csd.explodeFlattening(cf, args);
            args = tc.flatArgs;
        }
        else {
            tc.flatArgs = args;
        }
        
        /* Look up parameters to bind. */
        SixModelObject sig = cf.codeRef.codeObject
            .get_attribute_boxed(tc, Code, "$!signature", HINT_CODE_SIG);
        SixModelObject params = sig
            .get_attribute_boxed(tc, Signature, "$!params", HINT_SIG_PARAMS);
        
        /* Run binder, and handle any errors. */
        switch (Binder.bind(tc, cf, params, csd, args, false, true)) {
            case Binder.BIND_RESULT_FAIL:
                throw ExceptionHandling.dieInternal(tc, Binder.lastError(tc));
            case Binder.BIND_RESULT_JUNCTION:
                throw ExceptionHandling.dieInternal(tc, "Junction re-dispatch NYI");
        }
        
        return csd;
    }
    
    public static long p6isbindable(SixModelObject signature, SixModelObject capture, ThreadContext tc) {
        /* TODO */
        System.err.println("p6isbindable NYI (always returns true)");
        return 1;
    }
    
    public static SixModelObject p6parcel(SixModelObject array, SixModelObject fill, ThreadContext tc) {
        SixModelObject parcel = Parcel.st.REPR.allocate(tc, Parcel.st);
        parcel.initialize(tc);
        parcel.bind_attribute_boxed(tc, Parcel, "$!storage", HINT_PARCEL_STORAGE, array);

        if (fill != null) {
            long elems = array.elems(tc);
            for (long i = 0; i < elems; i++) {
                if (array.at_pos_boxed(tc, i) == null)
                    array.bind_pos_boxed(tc, i, fill);
            }
        }

        return parcel;
    }
}
