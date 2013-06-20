package org.perl6.rakudo;

import java.util.*;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;

public final class Binder {
    /* Possible results of binding. */
    public static final int BIND_RESULT_OK       = 0;
    public static final int BIND_RESULT_FAIL     = 1;
    public static final int BIND_RESULT_JUNCTION = 2;
    
    /* Compile time trial binding result indicators. */
    public static final int TRIAL_BIND_NOT_SURE =  0;  /* Plausible, but need to check at runtime. */
    public static final int TRIAL_BIND_OK       =  1;  /* Bind will always work out. */
    public static final int TRIAL_BIND_NO_WAY   = -1;  /* Bind could never work out. */
    
    /* Flags. */
    private static final int SIG_ELEM_BIND_CAPTURE        = 1;
    private static final int SIG_ELEM_BIND_PRIVATE_ATTR   = 2;
    private static final int SIG_ELEM_BIND_PUBLIC_ATTR    = 4;
    private static final int SIG_ELEM_BIND_ATTRIBUTIVE    = (SIG_ELEM_BIND_PRIVATE_ATTR | SIG_ELEM_BIND_PUBLIC_ATTR);
    private static final int SIG_ELEM_SLURPY_POS          = 8;
    private static final int SIG_ELEM_SLURPY_NAMED        = 16;
    private static final int SIG_ELEM_SLURPY_LOL          = 32;
    private static final int SIG_ELEM_SLURPY              = (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED | SIG_ELEM_SLURPY_LOL);
    private static final int SIG_ELEM_INVOCANT            = 64;
    private static final int SIG_ELEM_MULTI_INVOCANT      = 128;
    private static final int SIG_ELEM_IS_RW               = 256;
    private static final int SIG_ELEM_IS_COPY             = 512;
    private static final int SIG_ELEM_IS_PARCEL           = 1024;
    private static final int SIG_ELEM_IS_OPTIONAL         = 2048;
    private static final int SIG_ELEM_ARRAY_SIGIL         = 4096;
    private static final int SIG_ELEM_HASH_SIGIL          = 8192;
    private static final int SIG_ELEM_DEFAULT_FROM_OUTER  = 16384;
    private static final int SIG_ELEM_IS_CAPTURE          = 32768;
    private static final int SIG_ELEM_UNDEFINED_ONLY      = 65536;
    private static final int SIG_ELEM_DEFINED_ONLY        = 131072;
    private static final int SIG_ELEM_DEFINEDNES_CHECK    = (SIG_ELEM_UNDEFINED_ONLY | SIG_ELEM_DEFINED_ONLY);
    private static final int SIG_ELEM_NOMINAL_GENERIC     = 524288;
    private static final int SIG_ELEM_DEFAULT_IS_LITERAL  = 1048576;
    private static final int SIG_ELEM_NATIVE_INT_VALUE    = 2097152;
    private static final int SIG_ELEM_NATIVE_NUM_VALUE    = 4194304;
    private static final int SIG_ELEM_NATIVE_STR_VALUE    = 8388608;
    private static final int SIG_ELEM_NATIVE_VALUE        = (SIG_ELEM_NATIVE_INT_VALUE | SIG_ELEM_NATIVE_NUM_VALUE | SIG_ELEM_NATIVE_STR_VALUE);
    
    /* Hints for Parameter attributes. */
    private static final int HINT_variable_name = 0;
    private static final int HINT_named_names = 1;
    private static final int HINT_type_captures = 2;
    private static final int HINT_flags = 3;
    private static final int HINT_nominal_type = 4;
    private static final int HINT_post_constraints = 5;
    private static final int HINT_coerce_type = 6;
    private static final int HINT_coerce_method = 7;
    private static final int HINT_sub_signature = 8;
    private static final int HINT_default_value = 9;
    private static final int HINT_container_descriptor = 10;
    private static final int HINT_attr_package = 11;
    
    /* Other hints. */
    private static final int HINT_ENUMMAP_storage = 0;
    private static final int HINT_CAPTURE_list = 0;
    private static final int HINT_CAPTURE_hash = 1;
    private static final int HINT_SIG_params = 0;
    
    /* Last error, per thread. */
    public static HashMap<ThreadContext, String> lastErrors = new HashMap<ThreadContext, String>();
    
    private static SixModelObject createBox(ThreadContext tc, Object arg, int flag) {
        switch (flag) {
            case CallSiteDescriptor.ARG_INT:
                return org.perl6.nqp.runtime.Ops.box_i((long)arg, Ops.Int, tc);
            case CallSiteDescriptor.ARG_NUM:
                return org.perl6.nqp.runtime.Ops.box_n((double)arg, Ops.Num, tc);
            case CallSiteDescriptor.ARG_STR:
                return org.perl6.nqp.runtime.Ops.box_s((String)arg, Ops.Str, tc);
            default:
                throw new RuntimeException("Impossible case reached in createBox");
        }
    }
    
    private static String arityFail(ThreadContext tc, SixModelObject params,
            int numParams, int numPosArgs, boolean tooMany) {
        int arity = 0;
        int count = 0;
        String fail = tooMany ? "Too many" : "Not enough";

        /* Work out how many we could have been passed. */
        for (int i = 0; i < numParams; i++) {
            SixModelObject param = params.at_pos_boxed(tc, i);
            param.get_attribute_native(tc, Ops.Parameter, "$!flags", HINT_flags);
            int flags = (int)tc.native_i;
            SixModelObject namedNames = param.get_attribute_boxed(tc,
                Ops.Parameter, "$!named_names", HINT_named_names);

            if (namedNames != null)
                continue;
            if ((flags & SIG_ELEM_SLURPY_NAMED) != 0)
                continue;
            if ((flags & SIG_ELEM_SLURPY_POS) != 0) {
                count = -1;
            }
            else if ((flags & SIG_ELEM_IS_OPTIONAL) != 0) {
                count++;
            }
            else {
                count++;
                arity++;
            }
        }

        /* Now generate decent error. */
        if (arity == count)
            return String.format(
                "%s positional parameters passed; got %d but expected %d",
                fail, numPosArgs, arity);
        else if (count == -1)
            return String.format(
                "%s positional parameters passed; got %d but expected at least %d",
                fail, numPosArgs, arity);
        else
            return String.format(
                "%s positional parameters passed; got %d but expected between %d and %d",
                fail, numPosArgs, arity, count);
    }
    
    /* Returns an appropriate failure mode (junction fail or normal fail). */
    private static int junc_or_fail(SixModelObject value) {
        if (value.st.WHAT == Ops.Junction)
            return BIND_RESULT_JUNCTION;
        else
            return BIND_RESULT_FAIL;
    }
    
    /* Binds any type captures. */
    public static void bindTypeCaptures(ThreadContext tc, SixModelObject typeCaps, CallFrame cf, SixModelObject type) {
        long elems = typeCaps.elems(tc);
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        for (long i = 0; i < elems; i++) {
            String name = typeCaps.at_pos_boxed(tc, i).get_str(tc);
            cf.oLex[sci.oTryGetLexicalIdx(name)] = type;
        }
    }
    
    /* Assigns an attributive parameter to the desired attribute. */
    private static int assignAttributive(ThreadContext tc, CallFrame cf, String varName,
            int paramFlags, SixModelObject attrPackage, SixModelObject value, boolean needError) {
        /* Find self. */
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        Integer selfIdx = sci.oTryGetLexicalIdx("self");
        if (selfIdx == null) {
            if (needError)
                lastErrors.put(tc, String.format(
                    "Unable to bind attributive parameter '%s' - could not find self",
                    varName));
            return BIND_RESULT_FAIL;
        }
        SixModelObject self = cf.oLex[selfIdx];

        /* If it's private, just need to fetch the attribute. */
        SixModelObject assignee;
        if ((paramFlags & SIG_ELEM_BIND_PRIVATE_ATTR) != 0) {
            assignee = self.get_attribute_boxed(tc, attrPackage, varName, STable.NO_HINT);
        }

        /* Otherwise if it's public, do a method call to get the assignee. */
        else {
            throw new RuntimeException("$.x parameters NYI");
        }

        Ops.p6store(assignee, value, tc);
        return BIND_RESULT_OK;
    }
    
    /* Binds a single argument into the lexpad, after doing any checks that are
     * needed. Also handles any type captures. If there is a sub signature, then
     * re-enters the binder. Returns one of the BIND_RESULT_* codes. */
    private static int bindOneParam(ThreadContext tc, CallFrame cf, SixModelObject param,
            Object origArg, byte origFlag, boolean noNomTypeCheck, boolean needError) {
        /* Get parameter flags and variable name. */
        param.get_attribute_native(tc, Ops.Parameter, "$!flags", HINT_flags);
        int paramFlags = (int)tc.native_i;
        param.get_attribute_native(tc, Ops.Parameter, "$!variable_name", HINT_variable_name);
        String varName = tc.native_s;
        if (Ops.DEBUG_MODE)
            System.err.println(varName);
        
        /* We'll put the value to bind into one of the following locals, and
         * flag will indicate what type of thing it is. */
        int flag;
        long arg_i = 0;
        double arg_n = 0.0;
        String arg_s = null;
        SixModelObject arg_o = null;
        
        /* Check if boxed/unboxed expections are met. */
        int desiredNative = paramFlags & SIG_ELEM_NATIVE_VALUE;
        int gotNative = origFlag & 7;
        if (desiredNative == 0 && gotNative == CallSiteDescriptor.ARG_OBJ) {
            flag = gotNative;
            arg_o = (SixModelObject)origArg;
        }
        else if (desiredNative == SIG_ELEM_NATIVE_INT_VALUE && gotNative == CallSiteDescriptor.ARG_INT) {
            flag = gotNative;
            arg_i = (long)origArg;
        }
        else if (desiredNative == SIG_ELEM_NATIVE_NUM_VALUE && gotNative == CallSiteDescriptor.ARG_NUM) {
            flag = gotNative;
            arg_n = (double)origArg;
        }
        else if (desiredNative == SIG_ELEM_NATIVE_STR_VALUE && gotNative == CallSiteDescriptor.ARG_STR) {
            flag = gotNative;
            arg_s = (String)origArg;
        }
        else if (desiredNative == 0) {
            /* We need to do a boxing operation. */
            flag = CallSiteDescriptor.ARG_OBJ;
            arg_o = createBox(tc, origArg, gotNative);
        }
        else {
            /* We need to do an unboxing opeation. */
            SixModelObject decontValue = org.perl6.nqp.runtime.Ops.decont((SixModelObject)origArg, tc);
            StorageSpec spec = decontValue.st.REPR.get_storage_spec(tc, decontValue.st);
            switch (desiredNative) {
                case SIG_ELEM_NATIVE_INT_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_INT) != 0) {
                        flag = CallSiteDescriptor.ARG_INT;
                        arg_i = decontValue.get_int(tc);
                    }
                    else {
                        if (needError)
                            lastErrors.put(tc, String.format(
                                "Cannot unbox argument to '%s' as a native int",
                                varName));
                        return BIND_RESULT_FAIL;
                    }
                    break;
                case SIG_ELEM_NATIVE_NUM_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_NUM) != 0) {
                        flag = CallSiteDescriptor.ARG_NUM;
                        arg_n = decontValue.get_num(tc);
                    }
                    else {
                        if (needError)
                            lastErrors.put(tc, String.format(
                                "Cannot unbox argument to '%s' as a native num",
                                varName));
                        return BIND_RESULT_FAIL;
                    }
                    break;
                case SIG_ELEM_NATIVE_STR_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_STR) != 0) {
                        flag = CallSiteDescriptor.ARG_STR;
                        arg_s = decontValue.get_str(tc);
                    }
                    else {
                        if (needError)
                            lastErrors.put(tc, String.format(
                                "Cannot unbox argument to '%s' as a native str",
                                varName));
                        return BIND_RESULT_FAIL;
                    }
                    break;
                default:
                    if (needError)
                        lastErrors.put(tc, String.format(
                            "Cannot unbox argument to '%s' as a native type",
                            varName));
                    return BIND_RESULT_FAIL;
            }
        }
        
        /* By this point, we'll either have an object that we might be able to
         * bind if it passes the type check, or a native value that needs no
         * further checking. */
        SixModelObject decontValue = null;
        if (flag == CallSiteDescriptor.ARG_OBJ) {
            /* We need to work on the decontainerized value. */
            decontValue = org.perl6.nqp.runtime.Ops.decont(arg_o, tc);
            
            /* HLL map it as needed. */
            decontValue = org.perl6.nqp.runtime.Ops.hllize(decontValue, tc);
            
            /* Skip nominal type check if not needed. */
            if (!noNomTypeCheck) {
                if (Ops.DEBUG_MODE)
                    System.err.println("Parameter type checking NYI");
            }
        }
        
        /* Type captures. */
        SixModelObject typeCaps = param.get_attribute_boxed(tc, Ops.Parameter,
            "$!type_captures", HINT_type_captures);
        if (typeCaps != null)
            bindTypeCaptures(tc, typeCaps, cf, decontValue.st.WHAT);
        
        /* TODO: Coercions. */
        
        /* If it's not got attributive binding, we'll go about binding it into the
         * lex pad. */
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        if ((paramFlags & SIG_ELEM_BIND_ATTRIBUTIVE) == 0 && varName != null) {
            /* Is it native? If so, just go ahead and bind it. */
            if (flag != CallSiteDescriptor.ARG_OBJ) {
                switch (flag) {
                    case CallSiteDescriptor.ARG_INT:
                        cf.iLex[sci.iTryGetLexicalIdx(varName)] = arg_i;
                        break;
                    case CallSiteDescriptor.ARG_NUM:
                        cf.nLex[sci.nTryGetLexicalIdx(varName)] = arg_n;
                        break;
                    case CallSiteDescriptor.ARG_STR:
                        cf.sLex[sci.sTryGetLexicalIdx(varName)] = arg_s;
                        break;
                }
            }
            
            /* Otherwise it's some objecty case. */
            else if ((paramFlags & SIG_ELEM_IS_RW) != 0) {
                /* XXX TODO Check if rw flag is set; also need to have a
                 * wrapper container that carries extra constraints. */
                cf.oLex[sci.oTryGetLexicalIdx(varName)] = arg_o;
            }
            else if ((paramFlags & SIG_ELEM_IS_PARCEL) != 0) {
                /* Just bind the thing as is into the lexpad. */
                cf.oLex[sci.oTryGetLexicalIdx(varName)] = arg_o;
            }
            else {
                /* If it's an array, copy means make a new one and store,
                 * and a normal bind is a straightforward binding plus
                 * adding a constraint. */
                if ((paramFlags & SIG_ELEM_ARRAY_SIGIL) != 0) {
                    SixModelObject bindee = decontValue;
                    if ((paramFlags & SIG_ELEM_IS_COPY) != 0) {
                        throw new RuntimeException("is copy array param NYI");
                    }
                    cf.oLex[sci.oTryGetLexicalIdx(varName)] = bindee;
                }
                
                /* If it's a hash, similar approach to array. */
                else if ((paramFlags & SIG_ELEM_HASH_SIGIL) != 0) {
                    SixModelObject bindee = decontValue;
                    if ((paramFlags & SIG_ELEM_IS_COPY) != 0) {
                        throw new RuntimeException("is copy hash param NYI");
                    }
                    cf.oLex[sci.oTryGetLexicalIdx(varName)] = bindee;
                }
                
                /* If it's a scalar, we always need to wrap it into a new
                 * container and store it, for copy or ro case (the rw bit
                 * in the container descriptor takes care of the rest). */
                else {
                    STable stScalar = Ops.Scalar.st;
                    SixModelObject new_cont = stScalar.REPR.allocate(tc, stScalar);
                    SixModelObject desc = param.get_attribute_boxed(tc, Ops.Parameter,
                        "$!container_descriptor", HINT_container_descriptor);
                    new_cont.bind_attribute_boxed(tc, Ops.Scalar, "$!descriptor",
                        RakudoContainerSpec.HINT_descriptor, desc);
                    new_cont.bind_attribute_boxed(tc, Ops.Scalar, "$!value",
                        RakudoContainerSpec.HINT_value, decontValue);
                    cf.oLex[sci.oTryGetLexicalIdx(varName)] = new_cont;
                }
            }
        }
        
        /* Is it the invocant? If so, also have to bind to self lexical. */
        if ((paramFlags & SIG_ELEM_INVOCANT) != 0)
            cf.oLex[sci.oTryGetLexicalIdx("self")] = decontValue;

        /* TODO: post_constraints. */

        /* TODO: attributives. */
        if ((paramFlags & SIG_ELEM_BIND_ATTRIBUTIVE) != 0) {
            if (flag != CallSiteDescriptor.ARG_OBJ) {
                if (needError)
                    lastErrors.put(tc,
                        "Native attributive binding not yet implemented");
                return BIND_RESULT_FAIL;
            }
            int result = assignAttributive(tc, cf, varName, paramFlags,
                param.get_attribute_boxed(tc, Ops.Parameter, "$!attr_package", HINT_attr_package),
                decontValue, needError);
            if (result != BIND_RESULT_OK)
                return result;
        }

        /* If it has a sub-signature, bind that. */
        SixModelObject subSignature = param.get_attribute_boxed(tc, Ops.Parameter,
            "$!sub_signature", HINT_sub_signature);
        if (subSignature != null && flag == CallSiteDescriptor.ARG_OBJ) {
            /* Turn value into a capture, unless we already have one. */
            SixModelObject capture = null;
            int result;
            if ((paramFlags & SIG_ELEM_IS_CAPTURE) != 0) {
                capture = decontValue;
            }
            else {
                SixModelObject meth = org.perl6.nqp.runtime.Ops.findmethod(decontValue, "Capture", tc);
                if (meth == null) {
                    if (needError)
                        lastErrors.put(tc, "Could not turn argument into capture");
                    return BIND_RESULT_FAIL;
                }
                org.perl6.nqp.runtime.Ops.invokeDirect(tc, meth, org.perl6.nqp.runtime.Ops.invocantCallSite, new Object[] { decontValue });
                capture = org.perl6.nqp.runtime.Ops.result_o(tc.curFrame);
            }

            SixModelObject subParams = subSignature
                .get_attribute_boxed(tc, Ops.Signature, "$!params", HINT_SIG_params);
            /* Recurse into signature binder. */
            CallSiteDescriptor subCsd = explodeCapture(tc, capture);
            result = bind(tc, cf, subParams, subCsd, tc.flatArgs, noNomTypeCheck, needError);
            if (result != BIND_RESULT_OK)
            {
                if (needError) {
                    /* Note in the error message that we're in a sub-signature. */
                    lastErrors.put(tc, lastErrors.get(tc) + " in sub-signature");

                    /* Have we a variable name? */
                    if (varName != null) {
                        lastErrors.put(tc, lastErrors.get(tc) + " of parameter " + varName);
                    }
                }
                return result;
            }
        }

        if (Ops.DEBUG_MODE)
            System.err.println("bindOneParam NYFI");
        
        return BIND_RESULT_OK;
    }

    private static final CallSiteDescriptor exploder = new CallSiteDescriptor(new byte[] {
        CallSiteDescriptor.ARG_OBJ | CallSiteDescriptor.ARG_FLAT,
            CallSiteDescriptor.ARG_OBJ | CallSiteDescriptor.ARG_FLAT | CallSiteDescriptor.ARG_NAMED
    }, null);
    private static CallSiteDescriptor explodeCapture(ThreadContext tc, SixModelObject capture) {
        capture = org.perl6.nqp.runtime.Ops.decont(capture, tc);

        SixModelObject capType = Ops.Capture;
        SixModelObject list = capture.get_attribute_boxed(tc, capType, "$!list", HINT_CAPTURE_list);
        SixModelObject hash = capture.get_attribute_boxed(tc, capType, "$!hash", HINT_CAPTURE_hash);

        return exploder.explodeFlattening(tc.curFrame, new Object[] { list, hash });
    }

    /* This takes a signature element and either runs the closure to get a default
     * value if there is one, or creates an appropriate undefined-ish thingy. */
    private static SixModelObject handleOptional(ThreadContext tc, int flags, SixModelObject param, CallFrame cf) {
        /* Is the "get default from outer" flag set? */
        if ((flags & SIG_ELEM_DEFAULT_FROM_OUTER) != 0) {
            param.get_attribute_native(tc, Ops.Parameter, "$!variable_name", HINT_variable_name);
            String varName = tc.native_s;
            return cf.outer.oLex[cf.outer.codeRef.staticInfo.oTryGetLexicalIdx(varName)];
        }

        /* Do we have a default value or value closure? */
        SixModelObject defaultValue = param.get_attribute_boxed(tc, Ops.Parameter,
            "$!default_value", HINT_default_value);
        if (defaultValue != null) {
            if ((flags & SIG_ELEM_DEFAULT_IS_LITERAL) != 0) {
                return defaultValue;
            }
            else {
                /* Thunk; run it to get a value. */
                org.perl6.nqp.runtime.Ops.invokeArgless(tc, defaultValue);
                return org.perl6.nqp.runtime.Ops.result_o(tc.curFrame);
            }
        }

        /* Otherwise, go by sigil to pick the correct default type of value. */
        else {
            if ((flags & SIG_ELEM_ARRAY_SIGIL) != 0) {
                return Ops.p6list(null, Ops.Array, Ops.True, tc);
            }
            else if ((flags & SIG_ELEM_HASH_SIGIL) != 0) {
                SixModelObject res = Ops.Hash.st.REPR.allocate(tc, Ops.Hash.st);
                return res;
            }
            else {
                return param.get_attribute_boxed(tc, Ops.Parameter, "$!nominal_type", HINT_nominal_type);
            }
        }
    }
    
    /* Takes a signature along with positional and named arguments and binds them
     * into the provided callframe. Returns BIND_RESULT_OK if binding works out,
     * BIND_RESULT_FAIL if there is a failure and BIND_RESULT_JUNCTION if the
     * failure was because of a Junction being passed (meaning we need to auto-thread). */
    public static int bind(ThreadContext tc, CallFrame cf, SixModelObject params,
            CallSiteDescriptor csd, Object[] args,
            boolean noNomTypeCheck, boolean needError) {
        int bindFail = BIND_RESULT_OK;
        int curPosArg = 0;
        
        /* If we have a |$foo that's followed by slurpies, then we can suppress
         * any future arity checks. */
        boolean suppressArityFail = false;
        
        /* If we do have some named args, we want to make a clone of the hash
         * to work on. We'll delete stuff from it as we bind, and what we have
         * left over can become the slurpy hash or - if we aren't meant to be
         * taking one - tell us we have a problem. */
        HashMap<String, Integer> namedArgsCopy = csd.nameMap == null
            ? null
            : new HashMap<String, Integer>(csd.nameMap);
        
        /* Now we'll walk through the signature and go about binding things. */
        int numPosArgs = csd.numPositionals;
        long numParams = params.elems(tc);
        for (long i = 0; i < numParams; i++) {
            /* Get parameter, its flags and any named names. */
            SixModelObject param = params.at_pos_boxed(tc, i);
            param.get_attribute_native(tc, Ops.Parameter, "$!flags", HINT_flags);
            int flags = (int)tc.native_i;
            SixModelObject namedNames = param.get_attribute_boxed(tc,
                Ops.Parameter, "$!named_names", HINT_named_names);
            
            /* Is it looking for us to bind a capture here? */
            if ((flags & SIG_ELEM_IS_CAPTURE) != 0) {
                /* Capture the arguments from this point forwards into a Capture.
                 * Of course, if there's no variable name we can (cheaply) do pretty
                 * much nothing. */
                param.get_attribute_native(tc, Ops.Parameter, "$!variable_name", HINT_variable_name);
                if (tc.native_s == null) {
                   bindFail = BIND_RESULT_OK;
                }
                else {
                    SixModelObject posArgs = Ops.EMPTYARR.clone(tc);
                    for (int k = curPosArg; k < numPosArgs; k++) {
                        switch (csd.argFlags[k]) {
                        case CallSiteDescriptor.ARG_OBJ:
                            posArgs.push_boxed(tc, (SixModelObject)args[k]);
                            break;
                        case CallSiteDescriptor.ARG_INT:
                            posArgs.push_boxed(tc, Ops.p6box_i((long)args[k], tc));
                            break;
                        case CallSiteDescriptor.ARG_NUM:
                            posArgs.push_boxed(tc, Ops.p6box_n((double)args[k], tc));
                            break;
                        case CallSiteDescriptor.ARG_STR:
                            posArgs.push_boxed(tc, Ops.p6box_s((String)args[k], tc));
                            break;
                        }
                    }                    
                    SixModelObject namedArgs = vmHashOfRemainingNameds(tc, namedArgsCopy, args);
                    
                    SixModelObject capType = Ops.Capture;
                    SixModelObject capSnap = capType.st.REPR.allocate(tc, capType.st);
                    capSnap.bind_attribute_boxed(tc, capType, "$!list", HINT_CAPTURE_list, posArgs);
                    capSnap.bind_attribute_boxed(tc, capType, "$!hash", HINT_CAPTURE_hash, namedArgs);
                    
                    bindFail = bindOneParam(tc, cf, param, capSnap, CallSiteDescriptor.ARG_OBJ,
                        noNomTypeCheck, needError);               
                }
                if (bindFail != 0) {
                    return bindFail;
                }
                else if (i + 1 == numParams) {
                    /* Since a capture acts as "the ultimate slurpy" in a sense, if
                     * this is the last parameter in the signature we can return
                     * success right off the bat. */
                    return BIND_RESULT_OK;
                }
                else {
                    SixModelObject nextParam = params.at_pos_boxed(tc, i + 1);
                    nextParam.get_attribute_native(tc, Ops.Parameter, "$!flags", HINT_flags);
                    if (((int)tc.native_i & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED)) != 0)
                        suppressArityFail = true;
                }
            }
            
            /* Could it be a named slurpy? */
            else if ((flags & SIG_ELEM_SLURPY_NAMED) != 0) {
                SixModelObject slurpy = vmHashOfRemainingNameds(tc, namedArgsCopy, args);
                SixModelObject bindee = Ops.Hash.st.REPR.allocate(tc, Ops.Hash.st);
                bindee.bind_attribute_boxed(tc, Ops.EnumMap, "$!storage",
                    HINT_ENUMMAP_storage, slurpy);
                bindFail = bindOneParam(tc, cf, param, bindee, CallSiteDescriptor.ARG_OBJ,
                    noNomTypeCheck, needError);
                if (bindFail != 0)
                    return bindFail;
                
                /* Nullify named arguments hash now we've consumed it, to mark all
                 * is well. */
                namedArgsCopy = null;
            }
            
            /* Otherwise, maybe it's a positional of some kind. */
            else if (namedNames == null) {
                /* Slurpy or LoL-slurpy? */
                if ((flags & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_LOL)) != 0) {
                    /* Create Perl 6 array, create VM array of all remaining things,
                     * then store it. */
                    SixModelObject slurpy = Ops.EMPTYARR.clone(tc);
                    while (curPosArg < numPosArgs) {
                        switch (csd.argFlags[curPosArg]) {
                        case CallSiteDescriptor.ARG_OBJ:
                            slurpy.push_boxed(tc, (SixModelObject)args[curPosArg]);
                            break;
                        case CallSiteDescriptor.ARG_INT:
                            slurpy.push_boxed(tc, Ops.p6box_i((long)args[curPosArg], tc));
                            break;
                        case CallSiteDescriptor.ARG_NUM:
                            slurpy.push_boxed(tc, Ops.p6box_n((double)args[curPosArg], tc));
                            break;
                        case CallSiteDescriptor.ARG_STR:
                            slurpy.push_boxed(tc, Ops.p6box_s((String)args[curPosArg], tc));
                            break;
                        }
                        curPosArg++;
                    }
                    
                    SixModelObject bindee;
                    if ((flags & SIG_ELEM_SLURPY_POS) != 0) {
                        if ((flags & SIG_ELEM_IS_RW) != 0)
                            bindee = Ops.p6list(slurpy, Ops.List, Ops.True, tc);
                        else
                            bindee = Ops.p6list(slurpy, Ops.Array, Ops.True, tc);
                    }
                    else {
                        bindee = Ops.p6list(slurpy, Ops.LoL, Ops.False, tc);
                    }
                    
                    bindFail = bindOneParam(tc, cf, param, bindee, CallSiteDescriptor.ARG_OBJ,
                        noNomTypeCheck, needError);
                    if (bindFail != 0)
                        return bindFail;
                }
                
                /* Otherwise, a positional. */
                else {
                    /* Do we have a value?. */
                    if (curPosArg < numPosArgs) {
                        /* Easy - just bind that. */
                        bindFail = bindOneParam(tc, cf, param, args[curPosArg],
                            csd.argFlags[curPosArg], noNomTypeCheck, needError);
                        if (bindFail != 0)
                            return bindFail;
                        curPosArg++;
                    }
                    else {
                        /* No value. If it's optional, fetch a default and bind that;
                         * if not, we're screwed. Note that we never nominal type check
                         * an optional with no value passed. */
                        if ((flags & SIG_ELEM_IS_OPTIONAL) != 0) {
                            bindFail = bindOneParam(tc, cf, param,
                                handleOptional(tc, flags, param, cf),
                                CallSiteDescriptor.ARG_OBJ, false, needError);
                            if (bindFail != 0)
                                return bindFail;
                        }
                        else {
                            if (needError)
                                lastErrors.put(tc, arityFail(tc, params, (int)numParams, numPosArgs, false));
                            return BIND_RESULT_FAIL;
                        }
                    }
                }
            }
            
            /* Else, it's a non-slurpy named. */
            else {
                /* Try and get hold of value. */
                Integer lookup = null;
                if (namedArgsCopy != null) {
                    long numNames = namedNames.elems(tc);
                    for (long j = 0; j < numNames; j++) {
                        String name = namedNames.at_pos_boxed(tc, j).get_str(tc);
                        lookup = namedArgsCopy.remove(name);
                        if (lookup != null)
                            break;
                    }
                }
                
                /* Did we get one? */
                if (lookup == null) {
                    /* Nope. We'd better hope this param was optional... */
                    if ((flags & SIG_ELEM_IS_OPTIONAL) != 0) {
                        bindFail = bindOneParam(tc, cf, param,
                            handleOptional(tc, flags, param, cf),
                            CallSiteDescriptor.ARG_OBJ, false, needError);
                    }
                    else if (!suppressArityFail) {
                        if (needError)
                            lastErrors.put(tc, "Required named parameter '" +
                                namedNames.at_pos_boxed(tc, 0).get_str(tc) +
                                "' not passed");
                        return BIND_RESULT_FAIL;
                    }
                }
                else {
                    bindFail = bindOneParam(tc, cf, param, args[lookup >> 3],
                        (byte)(lookup & 7), noNomTypeCheck, needError);
                }

                /* If we got a binding failure, return it. */
                if (bindFail != 0)
                    return bindFail;
            }
        }
        
        /* XXX TODO. */
        return BIND_RESULT_OK;
    }
    
    /* Takes any nameds we didn't capture yet and makes a VM Hash of them. */
    private static SixModelObject vmHashOfRemainingNameds(ThreadContext tc, HashMap<String, Integer> namedArgsCopy, Object[] args) {
        SixModelObject slurpy = Ops.Mu;
        if (namedArgsCopy != null) {
            SixModelObject BOOTHash = tc.gc.BOOTHash;
            slurpy = BOOTHash.st.REPR.allocate(tc, BOOTHash.st);
            for (String name : namedArgsCopy.keySet()) {
                int lookup = namedArgsCopy.get(name);
                switch (lookup & 7) {
                case CallSiteDescriptor.ARG_OBJ:
                    slurpy.bind_key_boxed(tc, name, (SixModelObject)args[lookup >> 3]);
                    break;
                case CallSiteDescriptor.ARG_INT:
                    slurpy.bind_key_boxed(tc, name, Ops.p6box_i((long)args[lookup >> 3], tc));
                    break;
                case CallSiteDescriptor.ARG_NUM:
                    slurpy.bind_key_boxed(tc, name, Ops.p6box_n((double)args[lookup >> 3], tc));
                    break;
                case CallSiteDescriptor.ARG_STR:
                    slurpy.bind_key_boxed(tc, name, Ops.p6box_s((String)args[lookup >> 3], tc));
                    break;
                }
            }
        }
        return slurpy;
    }
    
    public static String lastError(ThreadContext tc) {
        return lastErrors.get(tc);
    }
}
