package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;
import org.perl6.nqp.sixmodel.reprs.VMArrayInstance;

/**
 * Contains implementation of nqp:: ops specific to Rakudo Perl 6.
 */
public final class Ops {
    public static SixModelObject Mu;
    public static SixModelObject Parcel;
    public static SixModelObject Code;
    public static SixModelObject Routine;
    public static SixModelObject Signature;
    public static SixModelObject Parameter;
    public static SixModelObject Int;
    public static SixModelObject Num;
    public static SixModelObject Str;
    public static SixModelObject List;
    public static SixModelObject ListIter;
    public static SixModelObject Array;
    public static SixModelObject EnumMap;
    public static SixModelObject Hash;
    public static SixModelObject Junction;
    public static SixModelObject Scalar;
    public static SixModelObject ContainerDescriptor;
    public static SixModelObject False;
    public static SixModelObject True;
    public static SixModelObject AutoThreader;
    private static boolean initialized = false;
    private static SixModelObject EMPTYARR;
    
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
        if (!initialized) {
            tc.gc.contConfigs.put("rakudo_scalar", new RakudoContainerConfigurer());
            SixModelObject BOOTArray = tc.gc.BOOTArray;
            EMPTYARR = BOOTArray.st.REPR.allocate(tc, BOOTArray.st);
            EMPTYARR.initialize(tc);
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
        List = conf.at_key_boxed(tc, "List");
        ListIter = conf.at_key_boxed(tc, "ListIter");
        Array = conf.at_key_boxed(tc, "Array");
        EnumMap = conf.at_key_boxed(tc, "EnumMap");
        Hash = conf.at_key_boxed(tc, "Hash");
        Junction = conf.at_key_boxed(tc, "Junction");
        Scalar = conf.at_key_boxed(tc, "Scalar");
        ContainerDescriptor = conf.at_key_boxed(tc, "ContainerDescriptor");
        False = conf.at_key_boxed(tc, "False");
        True = conf.at_key_boxed(tc, "True");
        return conf;
    }
    
    public static SixModelObject p6setautothreader(SixModelObject autoThreader, ThreadContext tc) {
        AutoThreader = autoThreader;
        return autoThreader;
    }
    
    public static SixModelObject booleanize(int x) {
        return x == 0 ? False : True;
    }
    
    public static SixModelObject p6box_i(long value, ThreadContext tc) {
        SixModelObject res = Int.st.REPR.allocate(tc, Int.st);
        res.set_int(tc, value);
        return res;
    }
    
    public static SixModelObject p6box_n(double value, ThreadContext tc) {
        SixModelObject res = Num.st.REPR.allocate(tc, Num.st);
        res.set_num(tc, value);
        return res;
    }
    
    public static SixModelObject p6box_s(String value, ThreadContext tc) {
        SixModelObject res = Str.st.REPR.allocate(tc, Str.st);
        res.set_str(tc, value);
        return res;
    }
    
    public static SixModelObject p6list(SixModelObject arr, SixModelObject type, SixModelObject flattens, ThreadContext tc) {
        SixModelObject list = type.st.REPR.allocate(tc, type.st);
        if (arr != null) 
            list.bind_attribute_boxed(tc, List, "$!nextiter", HINT_LIST_nextiter,
                p6listiter(arr, list, tc));
        list.bind_attribute_boxed(tc, List, "$!flattens", HINT_LIST_flattens, flattens);
        return list;
    }
    
    public static SixModelObject p6listitems(SixModelObject list, ThreadContext tc) {
        SixModelObject items = list.get_attribute_boxed(tc, List, "$!items", HINT_LIST_items);
        if (!(items instanceof VMArrayInstance)) {
            items = EMPTYARR.clone(tc);
            list.bind_attribute_boxed(tc, List, "$!items", HINT_LIST_items, items);
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
        if (total > 0)
            b.splice(tc, EMPTYARR, 0, total);
        
        return a;
    }
    
    public static SixModelObject p6listiter(SixModelObject arr, SixModelObject list, ThreadContext tc) {
        SixModelObject iter = ListIter.st.REPR.allocate(tc, ListIter.st);
        iter.bind_attribute_boxed(tc, ListIter, "$!rest", HINT_LISTITER_rest, arr);
        iter.bind_attribute_boxed(tc, ListIter, "$!list", HINT_LISTITER_list, list);
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
        else {
            tc.flatArgs = args;
        }
        
        /* Look up parameters to bind. */
if (cf.codeRef.staticInfo.name != null)
    System.err.println("Binding for " + cf.codeRef.staticInfo.name);
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
    
    public static long p6trialbind(SixModelObject routine, SixModelObject values, SixModelObject flags, ThreadContext tc) {
        /* TODO */
        return Binder.TRIAL_BIND_NOT_SURE;
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
    
    public static SixModelObject p6var(SixModelObject cont, ThreadContext tc) {
        if (cont != null && cont.st.ContainerSpec != null) {
            SixModelObject wrapper = Scalar.st.REPR.allocate(tc, Scalar.st);
            wrapper.bind_attribute_boxed(tc, Ops.Scalar, "$!value",
                RakudoContainerSpec.HINT_value,
                cont);
            return wrapper;
        }
        else {
            return cont;
        }
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
    
    public static SixModelObject p6capturelex(SixModelObject codeObj, ThreadContext tc) {
        CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                Code, "$!do", HINT_CODE_DO);
        closure.outer = tc.curFrame;
        return codeObj;
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
}
