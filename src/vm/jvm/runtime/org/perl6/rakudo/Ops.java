package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;
import org.perl6.nqp.sixmodel.reprs.LexoticInstance;
import org.perl6.nqp.sixmodel.reprs.VMArrayInstance;

/**
 * Contains implementation of nqp:: ops specific to Rakudo Perl 6.
 */
public final class Ops {
    public static final boolean DEBUG_MODE = false;

    public static class ThreadExt {
        // not currently used
        public ThreadExt(ThreadContext tc) { }
    }

    public static class GlobalExt {
        public SixModelObject Mu;
        public SixModelObject Parcel;
        public SixModelObject Code;
        public SixModelObject Routine;
        public SixModelObject Signature;
        public SixModelObject Parameter;
        public SixModelObject Int;
        public SixModelObject Num;
        public SixModelObject Str;
        public SixModelObject List;
        public SixModelObject ListIter;
        public SixModelObject Array;
        public SixModelObject LoL;
        public SixModelObject EnumMap;
        public SixModelObject Hash;
        public SixModelObject Junction;
        public SixModelObject Scalar;
        public SixModelObject Capture;
        public SixModelObject ContainerDescriptor;
        public SixModelObject False;
        public SixModelObject True;
        public SixModelObject AutoThreader;
        public SixModelObject EMPTYARR;
        boolean initialized;

        public GlobalExt(ThreadContext tc) { }
    }

    public static ContextKey<ThreadExt, GlobalExt> key = new ContextKey< >(ThreadExt.class, GlobalExt.class);

    /* Parameter hints for fast lookups. */
    private static final int HINT_PARCEL_STORAGE = 0;
    private static final int HINT_CODE_DO = 0;
    private static final int HINT_CODE_SIG = 1;
    private static final int HINT_ROUTINE_RW = 7;
    private static final int HINT_SIG_PARAMS = 0;
    private static final int HINT_CD_RW = 1;
    private static final int HINT_LIST_items = 0;
    private static final int HINT_LIST_flattens = 1;
    private static final int HINT_LIST_nextiter = 2;
    private static final int HINT_LISTITER_reified = 0;
    private static final int HINT_LISTITER_nextiter = 1;
    private static final int HINT_LISTITER_rest = 2;
    private static final int HINT_LISTITER_list = 3;
    
    public static SixModelObject p6init(ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (!gcx.initialized) {
            tc.gc.contConfigs.put("rakudo_scalar", new RakudoContainerConfigurer());
            SixModelObject BOOTArray = tc.gc.BOOTArray;
            gcx.EMPTYARR = BOOTArray.st.REPR.allocate(tc, BOOTArray.st);
            gcx.initialized = true;
        }
        return null;
    }
    
    public static SixModelObject p6settypes(SixModelObject conf, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.Mu = conf.at_key_boxed(tc, "Mu");
        gcx.Parcel = conf.at_key_boxed(tc, "Parcel");
        gcx.Code = conf.at_key_boxed(tc, "Code");
        gcx.Routine = conf.at_key_boxed(tc, "Routine");
        gcx.Signature = conf.at_key_boxed(tc, "Signature");
        gcx.Parameter = conf.at_key_boxed(tc, "Parameter");
        gcx.Int = conf.at_key_boxed(tc, "Int");
        gcx.Num = conf.at_key_boxed(tc, "Num");
        gcx.Str = conf.at_key_boxed(tc, "Str");
        gcx.List = conf.at_key_boxed(tc, "List");
        gcx.ListIter = conf.at_key_boxed(tc, "ListIter");
        gcx.Array = conf.at_key_boxed(tc, "Array");
        gcx.LoL = conf.at_key_boxed(tc, "LoL");
        gcx.EnumMap = conf.at_key_boxed(tc, "EnumMap");
        gcx.Hash = conf.at_key_boxed(tc, "Hash");
        gcx.Junction = conf.at_key_boxed(tc, "Junction");
        gcx.Scalar = conf.at_key_boxed(tc, "Scalar");
        gcx.Capture = conf.at_key_boxed(tc, "Capture");
        gcx.ContainerDescriptor = conf.at_key_boxed(tc, "ContainerDescriptor");
        gcx.False = conf.at_key_boxed(tc, "False");
        gcx.True = conf.at_key_boxed(tc, "True");
        return conf;
    }
    
    public static SixModelObject p6setautothreader(SixModelObject autoThreader, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.AutoThreader = autoThreader;
        return autoThreader;
    }
    
    public static SixModelObject booleanize(int x, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        return x == 0 ? gcx.False : gcx.True;
    }
    
    public static SixModelObject p6definite(SixModelObject obj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        obj = org.perl6.nqp.runtime.Ops.decont(obj, tc);
        return obj instanceof TypeObject ? gcx.False : gcx.True;
    }
    
    public static SixModelObject p6box_i(long value, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject res = gcx.Int.st.REPR.allocate(tc, gcx.Int.st);
        res.set_int(tc, value);
        return res;
    }
    
    public static SixModelObject p6box_n(double value, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject res = gcx.Num.st.REPR.allocate(tc, gcx.Num.st);
        res.set_num(tc, value);
        return res;
    }
    
    public static SixModelObject p6box_s(String value, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject res = gcx.Str.st.REPR.allocate(tc, gcx.Str.st);
        res.set_str(tc, value);
        return res;
    }
    
    public static SixModelObject p6list(SixModelObject arr, SixModelObject type, SixModelObject flattens, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject list = type.st.REPR.allocate(tc, type.st);
        if (arr != null) 
            list.bind_attribute_boxed(tc, gcx.List, "$!nextiter", HINT_LIST_nextiter,
                p6listiter(arr, list, tc));
        list.bind_attribute_boxed(tc, gcx.List, "$!flattens", HINT_LIST_flattens, flattens);
        return list;
    }
    
    public static SixModelObject p6listitems(SixModelObject list, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject items = list.get_attribute_boxed(tc, gcx.List, "$!items", HINT_LIST_items);
        if (!(items instanceof VMArrayInstance)) {
            items = gcx.EMPTYARR.clone(tc);
            list.bind_attribute_boxed(tc, gcx.List, "$!items", HINT_LIST_items, items);
        }
        return items;
    }
    
    public static long p6arrfindtypes(SixModelObject arr, SixModelObject types, long start, long last, ThreadContext tc) {
        int ntypes = (int)types.elems(tc);
        SixModelObject[] typeArr = new SixModelObject[ntypes];
        for (int i = 0; i < ntypes; i++)
            typeArr[i] = types.at_pos_boxed(tc, i);

        long elems = arr.elems(tc);
        if (elems < last)
            last = elems;

        long index;
        for (index = start; index < last; index++) {
            SixModelObject val = arr.at_pos_boxed(tc, index);
            if (val.st.ContainerSpec == null) {
                boolean found = false;
                for (int typeIndex = 0; typeIndex < ntypes; typeIndex++) {
                    if (org.perl6.nqp.runtime.Ops.istype(val, typeArr[typeIndex], tc) != 0) {
                        found = true;
                        break;
                    }
                }
                if (found)
                    break;
            }
        }

        return index;
    }
    
    public static SixModelObject p6shiftpush(SixModelObject a, SixModelObject b, long total, ThreadContext tc) {
        long count = total;
        long elems = b.elems(tc);
        if (count > elems)
            count = elems;

        if (a != null && total > 0) {
            long getPos = 0;
            long setPos = a.elems(tc);
            a.set_elems(tc, setPos + count);
            while (count > 0) {
                a.bind_pos_boxed(tc, setPos, b.at_pos_boxed(tc, getPos));
                count--;
                getPos++;
                setPos++;
            }
        }
        if (total > 0) {
            GlobalExt gcx = key.getGC(tc);
            b.splice(tc, gcx.EMPTYARR, 0, total);
        }
        
        return a;
    }
    
    public static SixModelObject p6listiter(SixModelObject arr, SixModelObject list, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject iter = gcx.ListIter.st.REPR.allocate(tc, gcx.ListIter.st);
        iter.bind_attribute_boxed(tc, gcx.ListIter, "$!rest", HINT_LISTITER_rest, arr);
        iter.bind_attribute_boxed(tc, gcx.ListIter, "$!list", HINT_LISTITER_list, list);
        return iter;
    }
    
    public static SixModelObject p6argvmarray(ThreadContext tc, CallSiteDescriptor csd, Object[] args) {
        SixModelObject BOOTArray = tc.gc.BOOTArray;
        SixModelObject res = BOOTArray.st.REPR.allocate(tc, BOOTArray.st);
        for (int i = 0; i < csd.numPositionals; i++) {
            SixModelObject toBind;
            switch (csd.argFlags[i]) {
                case CallSiteDescriptor.ARG_INT:
                    toBind = p6box_i((long)args[i], tc);
                    break;
                case CallSiteDescriptor.ARG_NUM:
                    toBind = p6box_n((double)args[i], tc);
                    break;
                case CallSiteDescriptor.ARG_STR:
                    toBind = p6box_s((String)args[i], tc);
                    break;
                default:
                    toBind = org.perl6.nqp.runtime.Ops.hllize((SixModelObject)args[i], tc);
                    break;
            }
            res.bind_pos_boxed(tc, i, toBind);
        }
        return res;
    }
    
    public static CallSiteDescriptor p6bindsig(ThreadContext tc, CallSiteDescriptor csd, Object[] args) {
        /* Do any flattening before processing begins. */
        CallFrame cf = tc.curFrame;
        if (csd.hasFlattening) {
            csd = csd.explodeFlattening(cf, args);
            args = tc.flatArgs;
        }

        /* Look up parameters to bind. */
        if (DEBUG_MODE) {
            if (cf.codeRef.staticInfo.name != null)
                System.err.println("Binding for " + cf.codeRef.staticInfo.name);
        }
        GlobalExt gcx = key.getGC(tc);
        SixModelObject sig = cf.codeRef.codeObject
            .get_attribute_boxed(tc, gcx.Code, "$!signature", HINT_CODE_SIG);
        SixModelObject params = sig
            .get_attribute_boxed(tc, gcx.Signature, "$!params", HINT_SIG_PARAMS);
        
        /* Run binder, and handle any errors. */
        String[] error = new String[1];
        switch (Binder.bind(tc, gcx, cf, params, csd, args, false, error)) {
            case Binder.BIND_RESULT_FAIL:
                throw ExceptionHandling.dieInternal(tc, error[0]);
            case Binder.BIND_RESULT_JUNCTION:
                throw ExceptionHandling.dieInternal(tc, "Junction re-dispatch NYI");
        }

        /* The binder may, for a variety of reasons, wind up calling Perl 6 code and overwriting flatArgs, so it needs to be set at the end to return reliably */
        tc.flatArgs = args;
        return csd;
    }
    
    public static long p6isbindable(SixModelObject signature, SixModelObject capture, ThreadContext tc) {
        /* TODO */
        if (DEBUG_MODE)
            System.err.println("p6isbindable NYI (always returns true)");
        return 1;
    }
    
    public static long p6trialbind(SixModelObject routine, SixModelObject values, SixModelObject flags, ThreadContext tc) {
        /* TODO */
        return Binder.TRIAL_BIND_NOT_SURE;
    }
    
    public static SixModelObject p6parcel(SixModelObject array, SixModelObject fill, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject parcel = gcx.Parcel.st.REPR.allocate(tc, gcx.Parcel.st);
        parcel.bind_attribute_boxed(tc, gcx.Parcel, "$!storage", HINT_PARCEL_STORAGE, array);

        if (fill != null) {
            long elems = array.elems(tc);
            for (long i = 0; i < elems; i++) {
                if (array.at_pos_boxed(tc, i) == null)
                    array.bind_pos_boxed(tc, i, fill);
            }
        }

        return parcel;
    }
    
    private static final CallSiteDescriptor STORE = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor storeThrower = new CallSiteDescriptor(
        new byte[] { }, null);
    public static SixModelObject p6store(SixModelObject cont, SixModelObject value, ThreadContext tc) {
        ContainerSpec spec = cont.st.ContainerSpec;
        if (spec != null) {
            spec.store(tc, cont, org.perl6.nqp.runtime.Ops.decont(value, tc));
        }
        else {
            SixModelObject meth = org.perl6.nqp.runtime.Ops.findmethod(value, "STORE", tc);
            if (meth != null) {
                org.perl6.nqp.runtime.Ops.invokeDirect(tc, meth,
                    STORE, new Object[] { cont, value });
            }
            else {
                SixModelObject thrower = getThrower(tc, "X::Assignment::RO");
                if (thrower == null)
                    ExceptionHandling.dieInternal(tc, "Cannot assign to a non-container");
                else
                    org.perl6.nqp.runtime.Ops.invokeDirect(tc, meth,
                        storeThrower, new Object[] { });
            }
        }
        return cont;
    }
    
    public static SixModelObject p6decontrv(SixModelObject cont, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (isRWScalar(tc, gcx, cont)) {
            tc.curFrame.codeRef.codeObject.get_attribute_native(tc, gcx.Routine, "$!rw", HINT_ROUTINE_RW);
            if (tc.native_i == 0) {
                /* Recontainerize to RO. */
                SixModelObject roCont = gcx.Scalar.st.REPR.allocate(tc, gcx.Scalar.st);
                roCont.bind_attribute_boxed(tc, gcx.Scalar, "$!value",
                    RakudoContainerSpec.HINT_value,
                    cont.st.ContainerSpec.fetch(tc, cont));
                return roCont;
            }
        }
        return cont;
    }
    
    public static SixModelObject p6recont_ro(SixModelObject cont, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (isRWScalar(tc, gcx, cont)) {
            SixModelObject roCont = gcx.Scalar.st.REPR.allocate(tc, gcx.Scalar.st);
            roCont.bind_attribute_boxed(tc, gcx.Scalar, "$!value",
                RakudoContainerSpec.HINT_value,
                cont.st.ContainerSpec.fetch(tc, cont));
            return roCont;
        }
        return cont;
    }
    
    private static boolean isRWScalar(ThreadContext tc, GlobalExt gcx, SixModelObject check) {
        if (!(check instanceof TypeObject) && check.st.WHAT == gcx.Scalar) {
            SixModelObject desc = check.get_attribute_boxed(tc, gcx.Scalar, "$!descriptor",
                RakudoContainerSpec.HINT_descriptor);
            if (desc == null)
                return false;
            desc.get_attribute_native(tc, gcx.ContainerDescriptor, "$!rw", HINT_CD_RW);
            return tc.native_i != 0;
        }
        return false;
    }
    
    public static SixModelObject p6var(SixModelObject cont, ThreadContext tc) {
        if (cont != null && cont.st.ContainerSpec != null) {
            GlobalExt gcx = key.getGC(tc);
            SixModelObject wrapper = gcx.Scalar.st.REPR.allocate(tc, gcx.Scalar.st);
            wrapper.bind_attribute_boxed(tc, gcx.Scalar, "$!value",
                RakudoContainerSpec.HINT_value,
                cont);
            return wrapper;
        }
        else {
            return cont;
        }
    }
    
    public static SixModelObject p6typecheckrv(SixModelObject rv, SixModelObject routine, ThreadContext tc) {
        if (DEBUG_MODE)
            System.err.println("p6typecheckrv NYI");
        return rv;
    }
    
    private static final CallSiteDescriptor baThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6bindassert(SixModelObject value, SixModelObject type, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (type != gcx.Mu) {
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
    
    public static SixModelObject p6capturelex(SixModelObject codeObj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                gcx.Code, "$!do", HINT_CODE_DO);
        closure.outer = tc.curFrame;
        return codeObj;
    }
    
    public static SixModelObject p6captureouters(SixModelObject capList, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        CallFrame cf = tc.curFrame;
        long elems = capList.elems(tc);
        for (long i = 0; i < elems; i++) {
            SixModelObject codeObj = capList.at_pos_boxed(tc, i);
            CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                gcx.Code, "$!do", HINT_CODE_DO);
            CallFrame ctxToDiddle = closure.outer;
            ctxToDiddle.outer = tc.curFrame;
        }
        return capList;
    }
    
    public static SixModelObject p6bindattrinvres(SixModelObject obj, SixModelObject ch, String name, SixModelObject value, ThreadContext tc) {
        obj.bind_attribute_boxed(tc, org.perl6.nqp.runtime.Ops.decont(ch, tc),
            name, STable.NO_HINT, value);
        if (obj.sc != null)
            org.perl6.nqp.runtime.Ops.scwbObject(tc, obj);
        return obj;
    }
    
    public static SixModelObject getThrower(ThreadContext tc, String type) {
        /* XXX TODO: thrower lookup. */
        return null;
    }

    private static CallFrame find_common_ctx(CallFrame ctx1, CallFrame ctx2) {
        int depth1 = 0;
        int depth2 = 0;
        CallFrame ctx;

        for (ctx = ctx1; ctx != null; ctx = ctx.caller, depth1++)
            if (ctx == ctx2)
                return ctx;
        for (ctx = ctx2; ctx != null; ctx = ctx.caller, depth2++)
            if (ctx == ctx1)
                return ctx;
        for (; depth1 > depth2; depth2++)
            ctx1 = ctx1.caller;
        for (; depth2 > depth1; depth1++)
            ctx2 = ctx2.caller;
        while (ctx1 != ctx2) {
            ctx1 = ctx1.caller;
            ctx2 = ctx2.caller;
        }
        return ctx1;
    }

    private static SixModelObject getremotelex(CallFrame pad, String name) { /* use for sub_find_pad */
        CallFrame curFrame = pad;
        while (curFrame != null) {
            Integer found = curFrame.codeRef.staticInfo.oTryGetLexicalIdx(name);
            if (found != null)
                return curFrame.oLex[found];
            curFrame = curFrame.outer;
        }
        return null;
    }

    public static SixModelObject p6routinereturn(SixModelObject in, ThreadContext tc) {
        CallFrame ctx = tc.curFrame;
        SixModelObject cont = null;

        for (ctx = ctx.caller; ctx != null; ctx = ctx.caller) {
            cont = getremotelex(ctx, "RETURN");
            if (cont != null) break;
        }

        if (!(cont instanceof LexoticInstance)) {
            SixModelObject thrower = getThrower(tc, "X::ControlFlow::Return");
            if (thrower == null)
                ExceptionHandling.dieInternal(tc, "Attempt to return outside of any Routine");
            else
                org.perl6.nqp.runtime.Ops.invokeArgless(tc, thrower);
        }

        // rewinding is handled by finally blocks in the generated subs
        LexoticException throwee = tc.theLexotic;
        throwee.target = ((LexoticInstance)cont).target;
        throwee.payload = in;
        throw throwee;
    }
}
