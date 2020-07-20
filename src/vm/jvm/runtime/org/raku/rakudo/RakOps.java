package org.raku.rakudo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import org.raku.nqp.runtime.*;
import org.raku.nqp.sixmodel.*;
import org.raku.nqp.sixmodel.reprs.CallCaptureInstance;
import org.raku.nqp.sixmodel.reprs.ContextRefInstance;
import org.raku.nqp.sixmodel.reprs.NativeRefInstance;
import org.raku.nqp.sixmodel.reprs.VMArrayInstance;

/**
 * Contains implementation of nqp:: ops specific to Rakudo
 */
@SuppressWarnings("unused")
public final class RakOps {
    public static final boolean DEBUG_MODE = false;

    public static class ThreadExt {
        public SixModelObject firstPhaserCodeBlock;
        public ArrayList<CallFrame> prePhaserFrames = new ArrayList<CallFrame>();
        public ThreadExt(ThreadContext tc) { }
    }

    public static class GlobalExt {
        public SixModelObject Mu;
        public SixModelObject Any;
        public SixModelObject Code;
        public SixModelObject Routine;
        public SixModelObject Signature;
        public SixModelObject Parameter;
        public SixModelObject Int;
        public SixModelObject Num;
        public SixModelObject Str;
        public SixModelObject List;
        public SixModelObject IterationBuffer;
        public SixModelObject Iterable;
        public SixModelObject Array;
        public SixModelObject Nil;
        public SixModelObject Map;
        public SixModelObject Hash;
        public SixModelObject Junction;
        public SixModelObject Scalar;
        public SixModelObject Capture;
        public SixModelObject ContainerDescriptor;
        public SixModelObject False;
        public SixModelObject True;
        public SixModelObject AutoThreader;
        public SixModelObject Positional;
        public SixModelObject PositionalBindFailover;
        public SixModelObject Associative;
        public SixModelObject EMPTYARR;
        public SixModelObject EMPTYHASH;
        public RakudoJavaInterop rakudoInterop;
        public SixModelObject JavaHOW;
        boolean initialized;

        public GlobalExt(ThreadContext tc) {}
    }

    public static ContextKey<ThreadExt, GlobalExt> key = new ContextKey< >(ThreadExt.class, GlobalExt.class);

    /* Parameter hints for fast lookups. */
    private static final int HINT_CODE_DO = 0;
    private static final int HINT_CODE_SIG = 1;
    private static final int HINT_ROUTINE_RW = 8;
    private static final int HINT_SIG_PARAMS = 0;
    private static final int HINT_SIG_RETURNS = 1;
    private static final int HINT_SIG_CODE = 4;
    public static final int HINT_CD_OF = 0;
    public static final int HINT_CD_NAME = 1;
    public static final int HINT_CD_DEFAULT = 2;

    public static SixModelObject p6init(ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (!gcx.initialized) {
            tc.gc.contConfigs.put("value_desc_cont", new RakudoContainerConfigurer());
            SixModelObject BOOTArray = tc.gc.BOOTArray;
            gcx.EMPTYARR = BOOTArray.st.REPR.allocate(tc, BOOTArray.st);
            SixModelObject BOOTHash = tc.gc.BOOTHash;
            gcx.EMPTYHASH = BOOTHash.st.REPR.allocate(tc, BOOTHash.st);
            gcx.rakudoInterop = new RakudoJavaInterop(tc.gc);
            gcx.initialized = true;
        }
        return null;
    }

    public static SixModelObject p6setitertype(SixModelObject type, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.Iterable = type;
        return type;
    }

    public static SixModelObject p6setassociativetype(SixModelObject type, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.Associative = type;
        return type;
    }

    public static SixModelObject p6setiterbuftype(SixModelObject type, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.IterationBuffer = type;
        return type;
    }

    public static SixModelObject p6settypes(SixModelObject conf, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.Mu = conf.at_key_boxed(tc, "Mu");
        gcx.Any = conf.at_key_boxed(tc, "Any");
        gcx.Code = conf.at_key_boxed(tc, "Code");
        gcx.Routine = conf.at_key_boxed(tc, "Routine");
        gcx.Signature = conf.at_key_boxed(tc, "Signature");
        gcx.Parameter = conf.at_key_boxed(tc, "Parameter");
        gcx.Int = conf.at_key_boxed(tc, "Int");
        gcx.Num = conf.at_key_boxed(tc, "Num");
        gcx.Str = conf.at_key_boxed(tc, "Str");
        gcx.List = conf.at_key_boxed(tc, "List");
        gcx.IterationBuffer = conf.at_key_boxed(tc, "IterationBuffer");
        gcx.Iterable = conf.at_key_boxed(tc, "Iterable");
        gcx.Array = conf.at_key_boxed(tc, "Array");
        gcx.Nil = conf.at_key_boxed(tc, "Nil");
        gcx.Map = conf.at_key_boxed(tc, "Map");
        gcx.Hash = conf.at_key_boxed(tc, "Hash");
        gcx.Junction = conf.at_key_boxed(tc, "Junction");
        gcx.Scalar = conf.at_key_boxed(tc, "Scalar");
        gcx.Capture = conf.at_key_boxed(tc, "Capture");
        gcx.ContainerDescriptor = conf.at_key_boxed(tc, "ContainerDescriptor");
        gcx.False = conf.at_key_boxed(tc, "False");
        gcx.True = conf.at_key_boxed(tc, "True");
        gcx.Associative = conf.at_key_boxed(tc, "Associative");
        gcx.JavaHOW = conf.at_key_boxed(tc, "Metamodel").st.WHO.at_key_boxed(tc, "JavaHOW");
        return conf;
    }

    public static SixModelObject p6setautothreader(SixModelObject autoThreader, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.AutoThreader = autoThreader;
        return autoThreader;
    }

    public static SixModelObject p6configposbindfailover(SixModelObject p, SixModelObject pbf, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        gcx.Positional = p;
        gcx.PositionalBindFailover = pbf;
        return p;
    }

    public static SixModelObject booleanize(int x, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        return x == 0 ? gcx.False : gcx.True;
    }

    public static SixModelObject p6definite(SixModelObject obj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        return Ops.isnull(obj) == 1 || Ops.decont(obj, tc) instanceof TypeObject ? gcx.False : gcx.True;
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
                    toBind = Ops.hllize((SixModelObject)args[i], tc);
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
        cf.csd = csd;
        cf.args = args;

        /* Look up parameters to bind. */
        if (DEBUG_MODE) {
            if (cf.codeRef.name != null)
                System.err.println("Binding for " + cf.codeRef.name);
        }
        GlobalExt gcx = key.getGC(tc);
        SixModelObject sig = cf.codeRef.codeObject
            .get_attribute_boxed(tc, gcx.Code, "$!signature", HINT_CODE_SIG);
        SixModelObject params = sig
            .get_attribute_boxed(tc, gcx.Signature, "@!params", HINT_SIG_PARAMS);

        /* Run binder, and handle any errors. */
        Object[] error = new Object[3];
        switch (Binder.bind(tc, gcx, cf, params, csd, args, false, error)) {
            case Binder.BIND_RESULT_FAIL:
                if (error[0] instanceof String) {
                    throw ExceptionHandling.dieInternal(tc, (String) error[0]);
                }
                else {
                    Ops.invokeDirect(tc, (SixModelObject) error[0],
                        (CallSiteDescriptor) error[1], (Object[]) error[2]);
                }
            case Binder.BIND_RESULT_JUNCTION:
                /* Invoke the auto-threader. */
                csd = csd.injectInvokee(tc, args, cf.codeRef.codeObject);
                args = tc.flatArgs;
                Ops.invokeDirect(tc, gcx.AutoThreader, csd, args);
                Ops.return_o(
                    Ops.result_o(cf), cf);

                /* Return null to indicate immediate return to the routine. */
                return null;
        }

        /* The binder may, for a variety of reasons, wind up calling Raku code and overwriting flatArgs, so it needs to be set at the end to return reliably */
        tc.flatArgs = args;
        return csd;
    }

    public static SixModelObject p6bindcaptosig(SixModelObject sig, SixModelObject cap, ThreadContext tc) {
        CallFrame cf = tc.curFrame;

        GlobalExt gcx = key.getGC(tc);
        CallSiteDescriptor csd = Binder.explodeCapture(tc, gcx, cap);
        SixModelObject params = sig.get_attribute_boxed(tc, gcx.Signature,
            "@!params", HINT_SIG_PARAMS);

        Object[] error = new Object[3];
        switch (Binder.bind(tc, gcx, cf, params, csd, tc.flatArgs, false, error)) {
            case Binder.BIND_RESULT_FAIL:
            case Binder.BIND_RESULT_JUNCTION:
                if (error[0] instanceof String) {
                    throw ExceptionHandling.dieInternal(tc, (String) error[0]);
                }
                else {
                    Ops.invokeDirect(tc, (SixModelObject) error[0],
                        (CallSiteDescriptor) error[1], (Object[]) error[2]);
                }
            default:
                return sig;
        }
    }

    public static long p6isbindable(SixModelObject sig, SixModelObject cap, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);

        CallSiteDescriptor csd;
        Object[] args;
        if (cap instanceof CallCaptureInstance) {
            CallCaptureInstance cc = (CallCaptureInstance)cap;
            csd = cc.descriptor;
            args = cc.args;
        } else {
            csd = Binder.explodeCapture(tc, gcx, cap);
            args = tc.flatArgs;
        }

        SixModelObject params = sig.get_attribute_boxed(tc, gcx.Signature,
            "@!params", HINT_SIG_PARAMS);
        SixModelObject codeObj = sig.get_attribute_boxed(tc, gcx.Signature,
            "$!code", HINT_SIG_CODE);
        CodeRef cr = (CodeRef)codeObj.get_attribute_boxed(tc, gcx.Code,
            "$!do", HINT_CODE_DO);

        CallFrame cf = new CallFrame(tc, cr);
        try {
            switch (Binder.bind(tc, gcx, cf, params, csd, args, false, null)) {
                case Binder.BIND_RESULT_FAIL:
                    return 0;
                default:
                    return 1;
            }
        }
        finally {
            tc.curFrame = tc.curFrame.caller;
        }
    }

    private static final CallSiteDescriptor STORE = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor storeThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6store(SixModelObject cont, SixModelObject value, ThreadContext tc) {
        ContainerSpec spec = cont.st.ContainerSpec;
        if (spec != null) {
            spec.store(tc, cont, Ops.decont(value, tc));
        }
        else {
            SixModelObject meth = Ops.findmethodNonFatal(cont, "STORE", tc);
            if (Ops.isnull(meth) == 0) {
                Ops.invokeDirect(tc, meth,
                    STORE, new Object[] { cont, value });
            }
            else {
                SixModelObject thrower = getThrower(tc, "X::Assignment::RO");
                if (thrower == null)
                    ExceptionHandling.dieInternal(tc, "Cannot assign to a non-container");
                else
                    Ops.invokeDirect(tc, thrower,
                        storeThrower, new Object[] { cont });
            }
        }
        return cont;
    }

    private static final CallSiteDescriptor rvThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6typecheckrv(SixModelObject rv, SixModelObject routine, SixModelObject bypassType, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        SixModelObject sig = routine.get_attribute_boxed(tc, gcx.Code, "$!signature", HINT_CODE_SIG);
        SixModelObject rtype = sig.get_attribute_boxed(tc, gcx.Signature, "$!returns", HINT_SIG_RETURNS);
        if (rtype != null) {
            SixModelObject decontValue = Ops.decont(rv, tc);
            if (Ops.istype(decontValue, rtype, tc) == 0) {
                /* Straight type check failed, but it's possible we're returning
                 * an Int that can unbox into an int or similar. */
                StorageSpec spec = rtype.st.REPR.get_storage_spec(tc, rtype.st);
                if (spec.inlineable == 0 || Ops.istype(rtype, decontValue.st.WHAT, tc) == 0) {
                    if (Ops.istype(decontValue.st.WHAT, bypassType, tc) == 0) {
                        SixModelObject thrower = getThrower(tc, "X::TypeCheck::Return");
                        if (thrower == null)
                            throw ExceptionHandling.dieInternal(tc,
                                "Type check failed for return value");
                        else
                            Ops.invokeDirect(tc, thrower,
                                rvThrower, new Object[] { decontValue, rtype });
                    }
                }
            }
        }
        return rv;
    }

    private static final CallSiteDescriptor baThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6bindassert(SixModelObject value, SixModelObject type, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (type != gcx.Mu) {
            SixModelObject decont = Ops.decont(value, tc);
            if (Ops.istype(decont, type, tc) == 0) {
                SixModelObject thrower = getThrower(tc, "X::TypeCheck::Binding");
                if (thrower == null)
                    ExceptionHandling.dieInternal(tc,
                        "Type check failed in binding");
                else
                    Ops.invokeDirect(tc, thrower,
                        baThrower, new Object[] { value, type });
            }
        }
        return value;
    }

    public static SixModelObject p6capturelex(SixModelObject codeObj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                gcx.Code, "$!do", HINT_CODE_DO);
        StaticCodeInfo wantedStaticInfo = closure.staticInfo.outerStaticInfo;
        if (tc.curFrame.codeRef.staticInfo == wantedStaticInfo)
            closure.outer = tc.curFrame;
        else if (tc.curFrame.outer.codeRef.staticInfo == wantedStaticInfo)
            closure.outer = tc.curFrame.outer;
        return codeObj;
    }

    public static SixModelObject p6getouterctx(SixModelObject codeObj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        codeObj = Ops.decont(codeObj, tc);
        CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                gcx.Code, "$!do", HINT_CODE_DO);
        SixModelObject ContextRef = tc.gc.ContextRef;
        SixModelObject wrap = ContextRef.st.REPR.allocate(tc, ContextRef.st);
        ((ContextRefInstance)wrap).context = closure.outer;
        return wrap;
    }

    public static SixModelObject p6captureouters2(SixModelObject capList, SixModelObject target, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        if (!(target instanceof CodeRef))
            ExceptionHandling.dieInternal(tc, "p6captureouters target must be a CodeRef");
        CallFrame cf = ((CodeRef)target).outer;
        if (cf == null)
            return capList;
        long elems = capList.elems(tc);
        for (long i = 0; i < elems; i++) {
            SixModelObject codeObj = capList.at_pos_boxed(tc, i);
            CodeRef closure = (CodeRef)codeObj.get_attribute_boxed(tc,
                gcx.Code, "$!do", HINT_CODE_DO);
            CallFrame ctxToDiddle = closure.outer;
            ctxToDiddle.outer = cf;
        }
        return capList;
    }

    public static SixModelObject p6bindattrinvres(SixModelObject obj, SixModelObject ch, String name, SixModelObject value, ThreadContext tc) {
        obj.bind_attribute_boxed(tc, Ops.decont(ch, tc),
            name, STable.NO_HINT, value);
        if (obj.sc != null)
            Ops.scwbObject(tc, obj);
        return obj;
    }

    public static SixModelObject getThrower(ThreadContext tc, String type) {
        SixModelObject exHash = Ops.gethllsym("Raku", "P6EX", tc);
        return exHash == null ? null : Ops.atkey(exHash, type, tc);
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

    public static String tclc(String in, ThreadContext tc) {
        if (in.length() == 0)
            return in;
        int first = in.codePointAt(0);
        return new String(Character.toChars(Character.toTitleCase(first)))
            + in.substring(Character.charCount(first)).toLowerCase();
    }

    private static final CallSiteDescriptor SortCSD = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static SixModelObject p6sort(SixModelObject indices, final SixModelObject comparator, final ThreadContext tc) {
        int elems = (int)indices.elems(tc);
        SixModelObject[] sortable = new SixModelObject[elems];
        for (int i = 0; i < elems; i++)
            sortable[i] = indices.at_pos_boxed(tc, i);
        Arrays.sort(sortable, new Comparator<SixModelObject>() {
            public int compare(SixModelObject a, SixModelObject b) {
                Ops.invokeDirect(tc, comparator, SortCSD,
                    new Object[] { a, b });
                return (int)Ops.result_i(tc.curFrame);
            }
        });
        for (int i = 0; i < elems; i++)
            indices.bind_pos_boxed(tc, i, sortable[i]);
        return indices;
    }

    public static long p6stateinit(ThreadContext tc) {
        return tc.curFrame.stateInit ? 1 : 0;
    }

    public static SixModelObject p6setfirstflag(SixModelObject codeObj, ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        ThreadExt tcx = key.getTC(tc);
        tcx.firstPhaserCodeBlock = codeObj.get_attribute_boxed(tc,
            gcx.Code, "$!do", HINT_CODE_DO);
        return codeObj;
    }

    public static long p6takefirstflag(ThreadContext tc) {
        ThreadExt tcx = key.getTC(tc);
        boolean matches = tcx.firstPhaserCodeBlock == tc.curFrame.codeRef;
        tcx.firstPhaserCodeBlock = null;
        return matches ? 1 : 0;
    }

    public static SixModelObject p6setpre(ThreadContext tc) {
        ThreadExt tcx = key.getTC(tc);
        tcx.prePhaserFrames.add(tc.curFrame);
        return null;
    }

    public static SixModelObject p6clearpre(ThreadContext tc) {
        ThreadExt tcx = key.getTC(tc);
        tcx.prePhaserFrames.remove(tc.curFrame);
        return null;
    }

    public static long p6inpre(ThreadContext tc) {
        ThreadExt tcx = key.getTC(tc);
        return tcx.prePhaserFrames.remove(tc.curFrame.caller) ? 1 : 0;
    }

    private static final CallSiteDescriptor dispVivifier = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ,
                     CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor dispThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_STR }, null);
    public static SixModelObject p6finddispatcher(String usage, ThreadContext tc) {
        SixModelObject dispatcher = null;

        CallFrame ctx = tc.curFrame.caller;
        while (ctx != null) {
            /* Do we have a dispatcher here? */
            StaticCodeInfo sci = ctx.codeRef.staticInfo;
            Integer dispLexIdx = sci.oTryGetLexicalIdx("$*DISPATCHER");
            if (dispLexIdx != null) {
                SixModelObject maybeDispatcher = ctx.oLex[dispLexIdx];
                if (maybeDispatcher != null) {
                    dispatcher = maybeDispatcher;
                    if (dispatcher instanceof TypeObject) {
                        /* Need to vivify it. */
                        SixModelObject meth = Ops.findmethod(dispatcher, "vivify_for", tc);
                        SixModelObject p6sub = ctx.codeRef.codeObject;

                        SixModelObject ContextRef = tc.gc.ContextRef;
                        SixModelObject wrap = ContextRef.st.REPR.allocate(tc, ContextRef.st);
                        ((ContextRefInstance)wrap).context = ctx;

                        SixModelObject CallCapture = tc.gc.CallCapture;
                        CallCaptureInstance cc = (CallCaptureInstance)CallCapture.st.REPR.allocate(tc, CallCapture.st);
                        cc.descriptor = ctx.csd;
                        cc.args = ctx.args;

                        Ops.invokeDirect(tc, meth,
                            dispVivifier, new Object[] { dispatcher, p6sub, wrap, cc });
                        dispatcher = Ops.result_o(tc.curFrame);
                        ctx.oLex[dispLexIdx] = dispatcher;
                    }
                    break;
                }
            }

            /* Follow dynamic chain. */
            ctx = ctx.caller;
        }

        if (dispatcher == null) {
            SixModelObject thrower = getThrower(tc, "X::NoDispatcher");
            if (thrower == null) {
                ExceptionHandling.dieInternal(tc,
                    usage + " is not in the dynamic scope of a dispatcher");
            } else {
                Ops.invokeDirect(tc, thrower,
                    dispThrower, new Object[] { usage });
            }
        }

        return dispatcher;
    }

    public static SixModelObject p6argsfordispatcher(SixModelObject disp, ThreadContext tc) {
        SixModelObject result = null;

        CallFrame ctx = tc.curFrame;
        while (ctx != null) {
            /* Do we have the dispatcher we're looking for? */
            StaticCodeInfo sci = ctx.codeRef.staticInfo;
            Integer dispLexIdx = sci.oTryGetLexicalIdx("$*DISPATCHER");
            if (dispLexIdx != null) {
                SixModelObject maybeDispatcher = ctx.oLex[dispLexIdx];
                if (maybeDispatcher == disp) {
                    /* Found; grab args. */
                    SixModelObject CallCapture = tc.gc.CallCapture;
                    CallCaptureInstance cc = (CallCaptureInstance)CallCapture.st.REPR.allocate(tc, CallCapture.st);
                    cc.descriptor = ctx.csd;
                    cc.args = ctx.args;
                    result = cc;
                    break;
                }
            }

            /* Follow dynamic chain. */
            ctx = ctx.caller;
        }

        if (result == null)
            throw ExceptionHandling.dieInternal(tc,
                "Could not find arguments for dispatcher");
        return result;
    }

    public static SixModelObject p6staticouter(SixModelObject code, ThreadContext tc) {
        if (code instanceof CodeRef)
            return ((CodeRef)code).staticInfo.outerStaticInfo.staticCode;
        else
            throw ExceptionHandling.dieInternal(tc, "p6staticouter must be used on a CodeRef");
    }

    public static SixModelObject jvmrakudointerop(ThreadContext tc) {
        GlobalExt gcx = key.getGC(tc);
        return BootJavaInterop.RuntimeSupport.boxJava(gcx.rakudoInterop, gcx.rakudoInterop.getSTableForClass(RakudoJavaInterop.class));
    }
}
