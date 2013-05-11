package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;

/**
 * Contains implementation of nqp:: ops specific to Rakudo Perl 6.
 */
public final class Ops {
    private static SixModelObject Mu;
    private static SixModelObject Parcel;
    private static SixModelObject Code;
    private static SixModelObject Routine;
    private static SixModelObject Signature;
    public static SixModelObject Parameter;
    public static SixModelObject Int;
    public static SixModelObject Num;
    public static SixModelObject Str;
    public static SixModelObject Junction;
    public static SixModelObject Scalar;
    private static SixModelObject ContainerDescriptor;
    private static SixModelObject False;
    private static SixModelObject True;
    private static boolean initialized = false;
    
    /* Parameter hints for fast lookups. */
    private static final int HINT_PARCEL_STORAGE = 0;
    private static final int HINT_CODE_DO = 0;
    private static final int HINT_CODE_SIG = 1;
    private static final int HINT_ROUTINE_RW = 7;
    private static final int HINT_SIG_PARAMS = 0;
    private static final int HINT_CD_RW = 1;
    
    public static SixModelObject p6init(ThreadContext tc) {
        if (!initialized) {
            tc.gc.contConfigs.put("rakudo_scalar", new RakudoContainerConfigurer());
            initialized = true;
        }
        return null;
    }
    
    public static SixModelObject p6settypes(SixModelObject conf, ThreadContext tc) {
        Mu = conf.at_key_boxed(tc, "Mu");
        Parcel = conf.at_key_boxed(tc, "Parcel");
        Code = conf.at_key_boxed(tc, "Code");
        Routine = conf.at_key_boxed(tc, "Routine");
        Signature = conf.at_key_boxed(tc, "Signature");
        Parameter = conf.at_key_boxed(tc, "Parameter");
        Int = conf.at_key_boxed(tc, "Int");
        Num = conf.at_key_boxed(tc, "Num");
        Str = conf.at_key_boxed(tc, "Str");
        Junction = conf.at_key_boxed(tc, "Junction");
        Scalar = conf.at_key_boxed(tc, "Scalar");
        ContainerDescriptor = conf.at_key_boxed(tc, "ContainerDescriptor");
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
    
    public static SixModelObject p6decontrv(SixModelObject cont, ThreadContext tc) {
        if (isRWScalar(tc, cont)) {
            tc.curFrame.codeRef.codeObject.get_attribute_native(tc, Routine, "$!rw", HINT_ROUTINE_RW);
            if (tc.native_i == 0) {
                /* Recontainerize to RO. */
                SixModelObject roCont = Scalar.st.REPR.allocate(tc, Scalar.st);
                roCont.bind_attribute_boxed(tc, Ops.Scalar, "$!value",
                    RakudoContainerSpec.HINT_value,
                    cont.st.ContainerSpec.fetch(tc, cont));
                return roCont;
            }
        }
        return cont;
    }
    
    private static boolean isRWScalar(ThreadContext tc, SixModelObject check) {
        if (!(check instanceof TypeObject) && check.st.WHAT == Scalar) {
            SixModelObject desc = check.get_attribute_boxed(tc, Scalar, "$!descriptor",
                RakudoContainerSpec.HINT_descriptor);
            desc.get_attribute_native(tc, ContainerDescriptor, "$!rw", HINT_CD_RW);
            return tc.native_i != 0;
        }
        return false;
    }
    
    public static SixModelObject p6typecheckrv(SixModelObject rv, SixModelObject routine, ThreadContext tc) {
        System.err.println("p6typecheckrv NYI");
        return rv;
    }
    
    private static final CallSiteDescriptor baThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6bindassert(SixModelObject value, SixModelObject type, ThreadContext tc) {
        if (type != Mu) {
            SixModelObject decont = org.perl6.nqp.runtime.Ops.decont(value, tc);
            if (org.perl6.nqp.runtime.Ops.istype(decont, type, tc) == 0) {
                SixModelObject thrower = getThrower(tc, "X::TypeCheck::Binding");
                if (thrower == null)
                    ExceptionHandling.dieInternal(tc,
                        "Type check failed in binding");
                else
                    org.perl6.nqp.runtime.Ops.invokeDirect(tc, thrower,
                        baThrower, new Object[] { value, type });
            }
        }
        return value;
    }
    
    public static SixModelObject p6captureouters(SixModelObject capList, ThreadContext tc) {
        CallFrame cf = tc.curFrame;
        long elems = capList.elems(tc);
        for (long i = 0; i < elems; i++) {
            SixModelObject codeObj = capList.at_pos_boxed(tc, i);
            CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                Code, "$!do", HINT_CODE_DO);
            closure.outer = cf;
        }
        return capList;
    }
    
    public static SixModelObject getThrower(ThreadContext tc, String type) {
        /* XXX TODO: thrower lookup. */
        return null;
    }
}
