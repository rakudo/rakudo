package org.raku.rakudo;

import java.util.*;

import org.raku.nqp.runtime.*;
import org.raku.nqp.sixmodel.*;
import org.raku.nqp.sixmodel.reprs.ContextRefInstance;
import org.raku.nqp.sixmodel.reprs.P6int;
import org.raku.nqp.sixmodel.reprs.P6str;
import org.raku.nqp.sixmodel.reprs.P6num;
import org.raku.nqp.sixmodel.reprs.P6OpaqueREPRData;

@SuppressWarnings("unused")
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
    private static final int SIG_ELEM_INVOCANT            = 64;
    private static final int SIG_ELEM_MULTI_INVOCANT      = 128;
    private static final int SIG_ELEM_IS_RW               = 256;
    private static final int SIG_ELEM_IS_COPY             = 512;
    private static final int SIG_ELEM_IS_RAW              = 1024;
    private static final int SIG_ELEM_IS_OPTIONAL         = 2048;
    private static final int SIG_ELEM_ARRAY_SIGIL         = 4096;
    private static final int SIG_ELEM_HASH_SIGIL          = 8192;
    private static final int SIG_ELEM_DEFAULT_FROM_OUTER  = 16384;
    private static final int SIG_ELEM_IS_CAPTURE          = 32768;
    private static final int SIG_ELEM_UNDEFINED_ONLY      = 65536;
    private static final int SIG_ELEM_DEFINED_ONLY        = 131072;
    private static final int SIG_ELEM_DEFINEDNES_CHECK    = (SIG_ELEM_UNDEFINED_ONLY | SIG_ELEM_DEFINED_ONLY);
    private static final int SIG_ELEM_TYPE_GENERIC        = 524288;
    private static final int SIG_ELEM_DEFAULT_IS_LITERAL  = 1048576;
    private static final int SIG_ELEM_NATIVE_INT_VALUE    = 2097152;
    private static final int SIG_ELEM_NATIVE_NUM_VALUE    = 4194304;
    private static final int SIG_ELEM_NATIVE_STR_VALUE    = 8388608;
    private static final int SIG_ELEM_NATIVE_VALUE        = (SIG_ELEM_NATIVE_INT_VALUE | SIG_ELEM_NATIVE_NUM_VALUE | SIG_ELEM_NATIVE_STR_VALUE);
    private static final int SIG_ELEM_SLURPY_ONEARG       = 16777216;
    private static final int SIG_ELEM_SLURPY              = (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED | SIG_ELEM_SLURPY_LOL | SIG_ELEM_SLURPY_ONEARG);
    private static final int SIG_ELEM_CODE_SIGIL          = 33554432;
    private static final int SIG_ELEM_IS_COERCIVE         = 67108864;

    /* Hints for Parameter attributes. */
    private static final int HINT_variable_name = 0;
    private static final int HINT_named_names = 1;
    private static final int HINT_type_captures = 2;
    private static final int HINT_flags = 3;
    private static final int HINT_type = 4;
    private static final int HINT_post_constraints = 5;
    private static final int HINT_sub_signature = 6;
    private static final int HINT_default_value = 7;
    private static final int HINT_container_descriptor = 8;
    private static final int HINT_attr_package = 9;

    /* Other hints. */
    private static final int HINT_ENUMMAP_storage = 0;
    private static final int HINT_CAPTURE_list = 0;
    private static final int HINT_CAPTURE_hash = 1;
    private static final int HINT_LIST_reified = 0;
    private static final int HINT_SIG_params = 0;

    private static SixModelObject createBox(ThreadContext tc, RakOps.GlobalExt gcx, Object arg, int flag) {
        switch (flag) {
            case CallSiteDescriptor.ARG_INT:
                return Ops.box_i((long)arg, gcx.Int, tc);
            case CallSiteDescriptor.ARG_NUM:
                return Ops.box_n((double)arg, gcx.Num, tc);
            case CallSiteDescriptor.ARG_STR:
                return Ops.box_s((String)arg, gcx.Str, tc);
            default:
                throw new RuntimeException("Impossible case reached in createBox");
        }
    }

    private static String arityFail(ThreadContext tc, RakOps.GlobalExt gcx, SixModelObject params,
            int numParams, int numPosArgs, boolean tooMany) {
        int arity = 0;
        int count = 0;
        String fail = tooMany ? "Too many" : "Too few";

        /* Work out how many we could have been passed. */
        for (int i = 0; i < numParams; i++) {
            SixModelObject param = params.at_pos_boxed(tc, i);
            param.get_attribute_native(tc, gcx.Parameter, "$!flags", HINT_flags);
            int flags = (int)tc.native_i;
            SixModelObject namedNames = param.get_attribute_boxed(tc,
                gcx.Parameter, "@!named_names", HINT_named_names);

            if (namedNames != null)
                continue;
            if ((flags & SIG_ELEM_SLURPY_NAMED) != 0)
                continue;
            if ((flags & SIG_ELEM_SLURPY) != 0) {
                count = -1000; // cargo-culted from BOOTSTRAP.nqp: "in case a pos can sneak past a slurpy somehow"
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
                "%s positionals passed; expected %d arguments but got %d",
                fail, arity, numPosArgs);
        else if (count <= -1)
            return String.format(
                "%s positionals passed; expected at least %d arguments but got only %d",
                fail, arity, numPosArgs);
        else
            return String.format(
                "%s positionals passed; expected %d %s %d arguments but got %d",
                fail, arity, arity + 1 == count ? "or" : "to" , count, numPosArgs);
    }

    /* Binds any type captures. */
    public static void bindTypeCaptures(ThreadContext tc, SixModelObject typeCaps, CallFrame cf, SixModelObject type) {
        long elems = typeCaps.elems(tc);
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        for (long i = 0; i < elems; i++) {
            typeCaps.at_pos_native(tc, i);
            String name = tc.native_s;
            cf.oLex[sci.oTryGetLexicalIdx(name)] = type;
        }
    }

    /* Assigns an attributive parameter to the desired attribute. */
    private static int assignAttributive(ThreadContext tc, CallFrame cf, String varName,
            int paramFlags, SixModelObject attrPackage, SixModelObject value, Object[] error) {
        /* Find self. */
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        Integer selfIdx = sci.oTryGetLexicalIdx("self");
        SixModelObject self = null;
        if (selfIdx == null) {
            self = Ops.getlexouter("self", tc);
            if (self == null) {
                if (error != null)
                    error[0] = String.format(
                        "Unable to bind attributive parameter '%s' - could not find self",
                        varName);
                return BIND_RESULT_FAIL;
            }
        }
        else {
            self = cf.oLex[selfIdx];
        }

        /* If it's private, just need to fetch the attribute. */
        SixModelObject assignee;
        if ((paramFlags & SIG_ELEM_BIND_PRIVATE_ATTR) != 0) {
            /* If we have a native Attribute we can't get a container for it, and
               since *trying* to get a container would throw already, we first check
               if the target Attribute is native. */
            int hint = -1;
            for (HashMap<String, Integer> map : ((P6OpaqueREPRData) (attrPackage.st.REPRData)).nameToHintMap) {
                try {
                    hint = map.get(varName);
                }
                catch (Exception e) {
                    continue;
                }
            }
            REPR attrREPR = null;
            if (((P6OpaqueREPRData) (attrPackage.st.REPRData)).flattenedSTables[hint] != null) {
                /* We sometimes don't have flattenedSTables. I'm not sure that's okay, honestly... */
                attrREPR = ((P6OpaqueREPRData) (attrPackage.st.REPRData)).flattenedSTables[hint].REPR;
            }
            if (attrREPR instanceof P6int) {
                Ops.bindattr_i(self, attrPackage, varName, Ops.unbox_i(value, tc), tc);
            }
            else if (attrREPR instanceof P6num) {
                Ops.bindattr_n(self, attrPackage, varName, Ops.unbox_n(value, tc), tc);
            }
            else if (attrREPR instanceof P6str) {
                Ops.bindattr_s(self, attrPackage, varName, Ops.unbox_s(value, tc), tc);
            }
            else {
                /* ...but we'll just assume it's probably some boxed Attribute. */
                assignee = self.get_attribute_boxed(tc, attrPackage, varName, STable.NO_HINT);
                RakOps.p6store(assignee, value, tc);
            }
        }
        /* Otherwise if it's public, do a method call to get the assignee. */
        else {
            throw new RuntimeException("$.x parameters NYI");
        }
        return BIND_RESULT_OK;
    }

    /* Returns an appropriate failure mode (junction fail or normal fail). */
    private static int juncOrFail(ThreadContext tc, RakOps.GlobalExt gcx, SixModelObject value) {
        if (value.st.WHAT == gcx.Junction && Ops.isconcrete(value, tc) != 0)
            return BIND_RESULT_JUNCTION;
        else
            return BIND_RESULT_FAIL;
    }

    /* Binds a single argument into the lexpad, after doing any checks that are
     * needed. Also handles any type captures. If there is a sub signature, then
     * re-enters the binder. Returns one of the BIND_RESULT_* codes. */
    private static final CallSiteDescriptor genIns = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor targetType = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor ACCEPTS_o = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor ACCEPTS_i = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_INT }, null);
    private static final CallSiteDescriptor ACCEPTS_n = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_NUM }, null);
    private static final CallSiteDescriptor ACCEPTS_s = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_STR }, null);
    private static final CallSiteDescriptor bindParamThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ,
            CallSiteDescriptor.ARG_STR, CallSiteDescriptor.ARG_OBJ,
            CallSiteDescriptor.ARG_INT
        }, null);
    private static final CallSiteDescriptor bindConcreteThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_STR, CallSiteDescriptor.ARG_STR,
            CallSiteDescriptor.ARG_STR, CallSiteDescriptor.ARG_STR,
            CallSiteDescriptor.ARG_INT, CallSiteDescriptor.ARG_INT
        }, null);
    private static final CallSiteDescriptor paramReadWriteThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_STR }, null);
    private static int bindOneParam(ThreadContext tc, RakOps.GlobalExt gcx, CallFrame cf, SixModelObject param,
            Object origArg, byte origFlag, boolean noNomTypeCheck, boolean isSlurpyNamed, Object[] error) {
        /* Get parameter flags and variable name. */
        param.get_attribute_native(tc, gcx.Parameter, "$!flags", HINT_flags);
        int paramFlags = (int)tc.native_i;
        param.get_attribute_native(tc, gcx.Parameter, "$!variable_name", HINT_variable_name);
        String varName = tc.native_s;
        boolean hasVarName = true;
        if (varName == null || varName.isEmpty()) {
            varName = "<anon>";
            hasVarName = false;
        }
        if (RakOps.DEBUG_MODE)
            System.err.println(varName);

        /* We'll put the value to bind into one of the following locals, and
         * flag will indicate what type of thing it is. */
        int flag;
        long arg_i = 0;
        double arg_n = 0.0;
        String arg_s = null;
        SixModelObject arg_o = null;

        /* Check if boxed/unboxed expectations are met. */
        int desiredNative = paramFlags & SIG_ELEM_NATIVE_VALUE;
        boolean is_rw = (paramFlags & SIG_ELEM_IS_RW) != 0;
        int gotNative = origFlag & (CallSiteDescriptor.ARG_INT | CallSiteDescriptor.ARG_NUM | CallSiteDescriptor.ARG_STR);
        if (is_rw && desiredNative != 0) {
            switch (desiredNative) {
            case SIG_ELEM_NATIVE_INT_VALUE:
                if (gotNative != 0 || Ops.iscont_i((SixModelObject)origArg) == 0) {
                    if (error != null)
                        error[0] = String.format(
                            "Expected a modifiable native int argument for '%s'",
                            varName);
                    return BIND_RESULT_FAIL;
                }
                break;
            case SIG_ELEM_NATIVE_NUM_VALUE:
                if (gotNative != 0 || Ops.iscont_n((SixModelObject)origArg) == 0) {
                    if (error != null)
                        error[0] = String.format(
                            "Expected a modifiable native num argument for '%s'",
                            varName);
                    return BIND_RESULT_FAIL;
                }
                break;
            case SIG_ELEM_NATIVE_STR_VALUE:
                if (gotNative != 0 || Ops.iscont_s((SixModelObject)origArg) == 0) {
                    if (error != null)
                        error[0] = String.format(
                            "Expected a modifiable native str argument for '%s'",
                            varName);
                    return BIND_RESULT_FAIL;
                }
                break;
            }
            flag = CallSiteDescriptor.ARG_OBJ;
            arg_o = (SixModelObject)origArg;
        }
        else if (desiredNative == 0 && gotNative == CallSiteDescriptor.ARG_OBJ) {
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
            arg_o = createBox(tc, gcx, origArg, gotNative);
        }
        else {
            /* We need to do an unboxing operation. */
            SixModelObject decontValue = Ops.decont((SixModelObject)origArg, tc);
            StorageSpec spec = decontValue.st.REPR.get_storage_spec(tc, decontValue.st);
            switch (desiredNative) {
                case SIG_ELEM_NATIVE_INT_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_INT) != 0) {
                        flag = CallSiteDescriptor.ARG_INT;
                        arg_i = decontValue.get_int(tc);
                    }
                    else {
                        if (error != null)
                            error[0] = String.format(
                                "Cannot unbox argument to '%s' as a native int",
                                varName);
                        return BIND_RESULT_FAIL;
                    }
                    break;
                case SIG_ELEM_NATIVE_NUM_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_NUM) != 0) {
                        flag = CallSiteDescriptor.ARG_NUM;
                        arg_n = decontValue.get_num(tc);
                    }
                    else {
                        if (error != null)
                            error[0] = String.format(
                                "Cannot unbox argument to '%s' as a native num",
                                varName);
                        return BIND_RESULT_FAIL;
                    }
                    break;
                case SIG_ELEM_NATIVE_STR_VALUE:
                    if ((spec.can_box & StorageSpec.CAN_BOX_STR) != 0) {
                        flag = CallSiteDescriptor.ARG_STR;
                        arg_s = decontValue.get_str(tc);
                    }
                    else {
                        if (error != null)
                            error[0] = String.format(
                                "Cannot unbox argument to '%s' as a native str",
                                varName);
                        return BIND_RESULT_FAIL;
                    }
                    break;
                default:
                    if (error != null)
                        error[0] = String.format(
                            "Cannot unbox argument to '%s' as a native type",
                            varName);
                    return BIND_RESULT_FAIL;
            }
        }

        /* By this point, we'll either have an object that we might be able to
         * bind if it passes the type check, or a native value that needs no
         * further checking. */
        SixModelObject decontValue = null;
        boolean didHLLTransform = false;
        SixModelObject paramType = param.get_attribute_boxed(tc, gcx.Parameter, "$!type", HINT_type);
        SixModelObject ContextRef = null;
        SixModelObject HOW = null;
        if (flag == CallSiteDescriptor.ARG_OBJ && !(is_rw && desiredNative != 0)) {
            /* We need to work on the decontainerized value. */
            decontValue = Ops.decont(arg_o, tc);

            /* HLL map it as needed. */
            SixModelObject beforeHLLize = decontValue;
            decontValue = Ops.hllize(decontValue, tc);
            if (decontValue != beforeHLLize)
                didHLLTransform = true;

            /* Skip nominal type check if not needed. */
            if (!noNomTypeCheck) {
                /* Is the nominal type generic and in need of instantiation? (This
                 * can happen in (::T, T) where we didn't learn about the type until
                 * during the signature bind.) */
                if ((paramFlags & SIG_ELEM_TYPE_GENERIC) != 0) {
                    HOW = paramType.st.HOW;
                    SixModelObject ig = Ops.findmethod(HOW,
                        "instantiate_generic", tc);
                    ContextRef = tc.gc.ContextRef;
                    SixModelObject cc = ContextRef.st.REPR.allocate(tc, ContextRef.st);
                    ((ContextRefInstance)cc).context = cf;
                    Ops.invokeDirect(tc, ig, genIns,
                        new Object[] { HOW, paramType, cc });
                    paramType = Ops.result_o(tc.curFrame);
                }

                /* If the expected type is Positional, see if we need to do the
                 * positional bind failover. */
                if (paramType == gcx.Positional) {
                    if (Ops.istype_nd(arg_o, gcx.PositionalBindFailover, tc) != 0) {
                        SixModelObject ig = Ops.findmethod(arg_o, "cache", tc);
                        Ops.invokeDirect(tc, ig, Ops.invocantCallSite, new Object[] { arg_o });
                        arg_o = Ops.result_o(tc.curFrame);
                        decontValue = Ops.decont(arg_o, tc);
                    }
                    else if (Ops.istype_nd(decontValue, gcx.PositionalBindFailover, tc) != 0) {
                        SixModelObject ig = Ops.findmethod(decontValue, "cache", tc);
                        Ops.invokeDirect(tc, ig, Ops.invocantCallSite, new Object[] { decontValue });
                        decontValue = Ops.result_o(tc.curFrame);
                    }
                }

                /* If not, do the check. If the wanted nominal type is Mu, then
                 * anything goes.
                 * When binding a slurpy named hash while compiling the setting don't check for Associative.
                 */
                if (paramType != gcx.Mu && !(isSlurpyNamed && paramType == gcx.Associative) && Ops.istype_nd(decontValue, paramType, tc) == 0) {
                    /* Type check failed; produce error if needed. */

                    /* Try to figure out the most helpful name for the expected. */
                    SixModelObject expectedType = null;
                    SixModelObject postConstraints = param.get_attribute_boxed(tc, gcx.Parameter,
                            "$!post_contraints", HINT_post_constraints);
                    if (postConstraints != null) {
                        SixModelObject consType = postConstraints.at_pos_boxed(tc, 0);
                        expectedType = (Ops.istype(consType, gcx.Code, tc) != 0)
                            ? paramType.st.WHAT
                            : consType.st.WHAT;
                    }
                    else {
                        expectedType = paramType.st.WHAT;
                    }

                    if (error != null) {
                        SixModelObject thrower = RakOps.getThrower(tc, "X::TypeCheck::Binding::Parameter");
                        if (thrower != null) {
                            error[0] = thrower;
                            error[1] = bindParamThrower;
                            error[2] = new Object[] { decontValue, expectedType.st.WHAT,
                                varName, param, (long)0 };
                        }
                        else {
                            error[0] = String.format(
                                "Nominal type check failed for parameter '%s'",
                                varName);
                        }
                    }

                    /* Report junction failure mode if it's a junction. */
                    return juncOrFail(tc, gcx, decontValue);
                }

                /* Also enforce definedness check */
                if ( (paramFlags & SIG_ELEM_DEFINEDNES_CHECK) != 0) {

                    /* Don't check decontValue for concreteness though, but arg_o,
                       seeing as we don't have a isconcrete_nodecont */
                    Boolean shouldBeConcrete = (paramFlags & SIG_ELEM_DEFINED_ONLY) != 0 && Ops.isconcrete(arg_o, tc) != 1;
                    if (shouldBeConcrete || ((paramFlags & SIG_ELEM_UNDEFINED_ONLY) != 0 && Ops.isconcrete(arg_o, tc) == 1)) {
                        if (error != null) {
                            String typeName = Ops.typeName(param.get_attribute_boxed(tc,
                                gcx.Parameter, "$!type", HINT_type), tc);
                            String argName = Ops.typeName(arg_o, tc);
                            String methodName = cf.codeRef.name;
                            SixModelObject thrower = RakOps.getThrower(tc, "X::Parameter::InvalidConcreteness");
                            if (thrower != null) {
                                error[0] = thrower;
                                error[1] = bindConcreteThrower;
                                error[2] = new Object[] { typeName, argName, methodName,
                                    varName, (long)(shouldBeConcrete ? 1 : 0),
                                    (long)(paramFlags & SIG_ELEM_INVOCANT) };
                            }
                            else {
                                if (methodName == null || methodName.isEmpty())
                                    methodName = "<anon>";
                                error[0] = ((paramFlags & SIG_ELEM_INVOCANT) != 0)
                                    ? shouldBeConcrete
                                        ? String.format(
                                            "Invocant of method '%s' must be an object instance of type '%s', not a type object of type '%s'.  Did you forget a '.new'?",
                                            methodName, typeName, argName)
                                        : String.format(
                                            "Invocant of method '%s' must be a type object of type '%s', not an object instance of type '%s'.  Did you forget a 'multi'?",
                                            methodName, typeName, argName)
                                    : shouldBeConcrete
                                        ? String.format(
                                            "Parameter '%s' of routine '%s' must be an object instance of type '%s', not a type object of type '%s'.  Did you forget a '.new'?",
                                            varName, methodName, typeName, argName)
                                        : String.format(
                                            "Parameter '%s' of routine '%s' must be a type object of type '%s', not an object instance of type '%s'.  Did you forget a 'multi'?",
                                            varName, methodName, typeName, argName);
                            }
                        }
                        return juncOrFail(tc, gcx, decontValue);
                    }
                }
            }
        }

        /* Type captures. */
        SixModelObject typeCaps = param.get_attribute_boxed(tc, gcx.Parameter,
            "@!type_captures", HINT_type_captures);
        if (typeCaps != null)
            bindTypeCaptures(tc, typeCaps, cf, decontValue.st.WHAT);

        /* Do a coercion, if one is needed. */
        HOW = paramType.st.HOW;
        SixModelObject archetypesMeth = Ops.findmethod(HOW, "archetypes", tc);
        Ops.invokeDirect(tc, archetypesMeth, Ops.invocantCallSite, new Object[] { HOW });
        SixModelObject Archetypes = Ops.result_o(tc.curFrame);
        SixModelObject coerciveMeth = Ops.findmethodNonFatal(Archetypes, "coercive", tc);
        if (coerciveMeth != null) {
            Ops.invokeDirect(tc, coerciveMeth, Ops.invocantCallSite, new Object[] { Archetypes });
            if (Ops.istrue(Ops.result_o(tc.curFrame), tc) == 1) {
                /* Coercing natives not possible - nothing to call a method on. */
                if (flag != CallSiteDescriptor.ARG_OBJ) {
                    if (error != null)
                        error[0] = String.format(
                            "Unable to coerce natively typed parameter '%s'",
                            varName);
                    return BIND_RESULT_FAIL;
                }

                SixModelObject coerceMeth = Ops.findmethod(HOW, "coerce", tc);
                Ops.invokeDirect(tc, coerceMeth, genIns, new Object[] { HOW, paramType, arg_o });
                arg_o = Ops.result_o(tc.curFrame);
                decontValue = Ops.decont(arg_o, tc);
            }
        }

        /* If it's not got attributive binding, we'll go about binding it into the
         * lex pad. */
        StaticCodeInfo sci = cf.codeRef.staticInfo;
        if ((paramFlags & SIG_ELEM_BIND_ATTRIBUTIVE) == 0) {
            /* Is it native? If so, just go ahead and bind it. */
            if (flag != CallSiteDescriptor.ARG_OBJ) {
                if (hasVarName) {
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
            }

            /* Otherwise it's some objecty case. */
            else if (is_rw) {
                if (Ops.isrwcont(arg_o, tc) == 1) {
                    if (hasVarName)
                        cf.oLex[sci.oTryGetLexicalIdx(varName)] = arg_o;
                } else {
                    SixModelObject thrower = RakOps.getThrower(tc, "X::Parameter::RW");
                    if (thrower == null) {
                        error[0] = "Parameter expected a writable container";
                    } else {
                        error[0] = thrower;
                        error[1] = paramReadWriteThrower;
                        error[2] = new Object[] { decontValue, varName};
                    }
                    return BIND_RESULT_FAIL;
                }

            }
            else if (hasVarName) {
                if ((paramFlags & SIG_ELEM_IS_RAW) != 0) {
                    /* Just bind the thing as is into the lexpad. */
                    cf.oLex[sci.oTryGetLexicalIdx(varName)] = didHLLTransform ? decontValue : arg_o;
                }
                else {
                    /* If it's an array, copy means make a new one and store,
                     * and a normal bind is a straightforward binding plus
                     * adding a constraint. */
                    if ((paramFlags & SIG_ELEM_ARRAY_SIGIL) != 0) {
                        SixModelObject bindee = decontValue;
                        if ((paramFlags & SIG_ELEM_IS_COPY) != 0) {
                            SixModelObject BOOTArray = tc.gc.BOOTArray;
                            bindee = gcx.Array.st.REPR.allocate(tc, gcx.Array.st);
                            bindee.bind_attribute_boxed(tc, gcx.List, "$!reified",
                                HINT_LIST_reified, BOOTArray.st.REPR.allocate(tc, BOOTArray.st));
                            RakOps.p6store(bindee, decontValue, tc);
                        }
                        cf.oLex[sci.oTryGetLexicalIdx(varName)] = bindee;
                    }

                    /* If it's a hash, similar approach to array. */
                    else if ((paramFlags & SIG_ELEM_HASH_SIGIL) != 0) {
                        SixModelObject bindee = decontValue;
                        if ((paramFlags & SIG_ELEM_IS_COPY) != 0) {
                            SixModelObject BOOTHash = tc.gc.BOOTHash;
                            bindee = gcx.Hash.st.REPR.allocate(tc, gcx.Hash.st);
                            bindee.bind_attribute_boxed(tc, gcx.Map, "$!storage",
                                HINT_ENUMMAP_storage, BOOTHash.st.REPR.allocate(tc, BOOTHash.st));
                            RakOps.p6store(bindee, decontValue, tc);
                        }
                        cf.oLex[sci.oTryGetLexicalIdx(varName)] = bindee;
                    }

                    /* If it's a scalar, we always need to wrap it into a new
                     * container and store it, for copy or ro case (the rw bit
                     * in the container descriptor takes care of the rest). */
                    else {
                        boolean wrap = (paramFlags & SIG_ELEM_IS_COPY) != 0;
                        if (!wrap && paramType != null && gcx.Iterable != null) {
                            wrap = Ops.istype(gcx.Iterable, paramType, tc) != 0
                                || Ops.istype(paramType, gcx.Iterable, tc) != 0;
                        }
                        if (wrap || varName.equals("$_")) {
                            STable stScalar = gcx.Scalar.st;
                            SixModelObject new_cont = stScalar.REPR.allocate(tc, stScalar);
                            SixModelObject desc = param.get_attribute_boxed(tc, gcx.Parameter,
                                "$!container_descriptor", HINT_container_descriptor);
                            new_cont.bind_attribute_boxed(tc, gcx.Scalar, "$!descriptor",
                                RakudoContainerSpec.HINT_descriptor, desc);
                            new_cont.bind_attribute_boxed(tc, gcx.Scalar, "$!value",
                                RakudoContainerSpec.HINT_value, decontValue);
                            cf.oLex[sci.oTryGetLexicalIdx(varName)] = new_cont;
                        }
                        else {
                            cf.oLex[sci.oTryGetLexicalIdx(varName)] = decontValue;
                        }
                    }
                }
            }
        }

        /* Is it the invocant? If so, also have to bind to self lexical. */
        if ((paramFlags & SIG_ELEM_INVOCANT) != 0)
            cf.oLex[sci.oTryGetLexicalIdx("self")] = decontValue;

        /* Handle any constraint types (note that they may refer to the parameter by
         * name, so we need to have bound it already). */
        SixModelObject postConstraints = param.get_attribute_boxed(tc, gcx.Parameter,
            "$!post_contraints", HINT_post_constraints);
        if (postConstraints != null) {
            long numConstraints = postConstraints.elems(tc);
            for (long i = 0; i < numConstraints; i++) {
                /* Check we meet the constraint. */
                SixModelObject consType = postConstraints.at_pos_boxed(tc, i);
                SixModelObject acceptsMeth = Ops.findmethod(consType, "ACCEPTS", tc);
                if (Ops.isconcrete(consType, tc) == 1 && Ops.istype(consType, gcx.Code, tc) != 0)
                    RakOps.p6capturelex(consType, tc);
                switch (flag) {
                    case CallSiteDescriptor.ARG_INT:
                        Ops.invokeDirect(tc, acceptsMeth,
                            ACCEPTS_i, new Object[] { consType, arg_i });
                        break;
                    case CallSiteDescriptor.ARG_NUM:
                        Ops.invokeDirect(tc, acceptsMeth,
                            ACCEPTS_n, new Object[] { consType, arg_n });
                        break;
                    case CallSiteDescriptor.ARG_STR:
                        Ops.invokeDirect(tc, acceptsMeth,
                            ACCEPTS_s, new Object[] { consType, arg_s });
                        break;
                    default:
                        Ops.invokeDirect(tc, acceptsMeth,
                            ACCEPTS_o, new Object[] { consType, arg_o });
                        break;
                }
                if (Ops.istrue(Ops.result_o(tc.curFrame), tc) == 0) {
                    /* Constraint type check failed; produce error if needed. */
                    if (error != null) {
                        SixModelObject thrower = RakOps.getThrower(tc, "X::TypeCheck::Binding::Parameter");
                        if (thrower != null) {
                            error[0] = thrower;
                            error[1] = bindParamThrower;
                            error[2] = new Object[] { (SixModelObject)origArg,
                                consType.st.WHAT, varName, param, (long)1 };
                        }
                        else {
                            error[0] = String.format(
                                "Constraint type check failed for parameter '%s'",
                                varName);
                        }
                    }
                    return BIND_RESULT_FAIL;
                }
            }
        }

        /* TODO: attributives. */
        if ((paramFlags & SIG_ELEM_BIND_ATTRIBUTIVE) != 0) {
            if (flag != CallSiteDescriptor.ARG_OBJ) {
                if (error != null)
                    error[0] = "Native attributive binding not yet implemented";
                return BIND_RESULT_FAIL;
            }
            int result = assignAttributive(tc, cf, varName, paramFlags,
                param.get_attribute_boxed(tc, gcx.Parameter, "$!attr_package", HINT_attr_package),
                decontValue, error);
            if (result != BIND_RESULT_OK)
                return result;
        }

        /* If it has a sub-signature, bind that. */
        SixModelObject subSignature = param.get_attribute_boxed(tc, gcx.Parameter,
            "$!sub_signature", HINT_sub_signature);
        if (subSignature != null && flag == CallSiteDescriptor.ARG_OBJ) {
            /* Turn value into a capture, unless we already have one. */
            SixModelObject capture = null;
            int result;
            if ((paramFlags & SIG_ELEM_IS_CAPTURE) != 0) {
                capture = decontValue;
            }
            else {
                SixModelObject meth = Ops.findmethodNonFatal(decontValue, "Capture", tc);
                if (meth == null) {
                    if (error != null)
                        error[0] = "Could not turn argument into capture";
                    return BIND_RESULT_FAIL;
                }
                Ops.invokeDirect(tc, meth, Ops.invocantCallSite, new Object[] { decontValue });
                capture = Ops.result_o(tc.curFrame);
            }

            SixModelObject subParams = subSignature
                .get_attribute_boxed(tc, gcx.Signature, "@!params", HINT_SIG_params);
            /* Recurse into signature binder. */
            CallSiteDescriptor subCsd = explodeCapture(tc, gcx, capture);
            result = bind(tc, gcx, cf, subParams, subCsd, tc.flatArgs, noNomTypeCheck, error);
            if (result != BIND_RESULT_OK)
            {
                if (error != null) {
                    /* Note in the error message that we're in a sub-signature. */
                    error[0] += " in sub-signature";

                    /* Have we a variable name? */
                    if (varName != null) {
                        error[0] += " of parameter " + varName;
                    }
                }
                return result;
            }
        }

        if (RakOps.DEBUG_MODE)
            System.err.println("bindOneParam NYFI");

        return BIND_RESULT_OK;
    }

    private static final CallSiteDescriptor exploder = new CallSiteDescriptor(new byte[] {
        CallSiteDescriptor.ARG_OBJ | CallSiteDescriptor.ARG_FLAT,
            CallSiteDescriptor.ARG_OBJ | CallSiteDescriptor.ARG_FLAT | CallSiteDescriptor.ARG_NAMED
    }, null);
    public static CallSiteDescriptor explodeCapture(ThreadContext tc, RakOps.GlobalExt gcx, SixModelObject capture) {
        capture = Ops.decont(capture, tc);

        SixModelObject capType = gcx.Capture;
        SixModelObject list = capture.get_attribute_boxed(tc, capType, "@!list", HINT_CAPTURE_list);
        SixModelObject hash = capture.get_attribute_boxed(tc, capType, "%!hash", HINT_CAPTURE_hash);
        if (list == null)
            list = gcx.EMPTYARR;
        if (hash == null)
            hash = gcx.EMPTYHASH;

        return exploder.explodeFlattening(tc.curFrame, new Object[] { list, hash });
    }

    /* This takes a signature element and either runs the closure to get a default
     * value if there is one, or creates an appropriate undefined-ish thingy. */
    private static SixModelObject handleOptional(ThreadContext tc, RakOps.GlobalExt gcx, int flags, SixModelObject param, CallFrame cf) {
        /* Is the "get default from outer" flag set? */
        if ((flags & SIG_ELEM_DEFAULT_FROM_OUTER) != 0) {
            param.get_attribute_native(tc, gcx.Parameter, "$!variable_name", HINT_variable_name);
            String varName = tc.native_s;
            CallFrame curOuter = cf.outer;
            while (curOuter != null) {
                Integer idx = curOuter.codeRef.staticInfo.oTryGetLexicalIdx(varName);
                if (idx != null)
                    return curOuter.oLex[idx];
                curOuter = curOuter.outer;
            }
            return null;
        }

        /* Do we have a default value or value closure? */
        SixModelObject defaultValue = param.get_attribute_boxed(tc, gcx.Parameter,
            "$!default_value", HINT_default_value);
        if (defaultValue != null) {
            if ((flags & SIG_ELEM_DEFAULT_IS_LITERAL) != 0) {
                return defaultValue;
            }
            else {
                /* Thunk; run it to get a value. */
                Ops.invokeArgless(tc, defaultValue);
                return Ops.result_o(tc.curFrame);
            }
        }

        /* Otherwise, go by sigil to pick the correct default type of value. */
        else {
            if ((flags & SIG_ELEM_ARRAY_SIGIL) != 0) {
                SixModelObject res = gcx.Array.st.REPR.allocate(tc, gcx.Array.st);
                return res;
            }
            else if ((flags & SIG_ELEM_HASH_SIGIL) != 0) {
                SixModelObject res = gcx.Hash.st.REPR.allocate(tc, gcx.Hash.st);
                return res;
            }
            else {
                param.get_attribute_native(tc, gcx.Parameter, "$!flags", HINT_flags);
                int paramFlags = (int)tc.native_i;
                switch (paramFlags & SIG_ELEM_NATIVE_VALUE) {
                    case SIG_ELEM_NATIVE_INT_VALUE:
                        return createBox(tc, gcx, (long)0, CallSiteDescriptor.ARG_INT);
                    case SIG_ELEM_NATIVE_NUM_VALUE:
                        return createBox(tc, gcx, (double)0.0, CallSiteDescriptor.ARG_NUM);
                    case SIG_ELEM_NATIVE_STR_VALUE:
                        return createBox(tc, gcx, null, CallSiteDescriptor.ARG_STR);
                    default:
                        /* Do a coercion, if one is needed. */
                        SixModelObject paramType = param.get_attribute_boxed(tc, gcx.Parameter, "$!type", HINT_type);
                        SixModelObject HOW = paramType.st.HOW;
                        SixModelObject archetypesMeth = Ops.findmethod(HOW, "archetypes", tc);
                        Ops.invokeDirect(tc, archetypesMeth, Ops.invocantCallSite, new Object[] { HOW });
                        SixModelObject Archetypes = Ops.result_o(tc.curFrame);
                        SixModelObject coerciveMeth = Ops.findmethodNonFatal(Archetypes, "coercive", tc);
                        if (coerciveMeth != null) {
                            Ops.invokeDirect(tc, coerciveMeth, Ops.invocantCallSite, new Object[] { Archetypes });
                            if (Ops.istrue(Ops.result_o(tc.curFrame), tc) == 1) {
                                SixModelObject targetTypeMeth = Ops.findmethod(HOW, "target_type", tc);
                                Ops.invokeDirect(tc, targetTypeMeth, targetType, new Object[] { HOW, paramType });
                                return Ops.result_o(tc.curFrame);
                            }
                        }
                        return paramType;
                }
            }
        }
    }

    /* Takes a signature along with positional and named arguments and binds them
     * into the provided callframe. Returns BIND_RESULT_OK if binding works out,
     * BIND_RESULT_FAIL if there is a failure and BIND_RESULT_JUNCTION if the
     * failure was because of a Junction being passed (meaning we need to auto-thread). */
     private static final CallSiteDescriptor slurpyFromArgs = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public static int bind(ThreadContext tc, RakOps.GlobalExt gcx, CallFrame cf, SixModelObject params,
            CallSiteDescriptor csd, Object[] args,
            boolean noNomTypeCheck, Object[] error) {
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
            param.get_attribute_native(tc, gcx.Parameter, "$!flags", HINT_flags);
            int flags = (int)tc.native_i;
            SixModelObject namedNames = param.get_attribute_boxed(tc,
                gcx.Parameter, "@!named_names", HINT_named_names);

            /* Is it looking for us to bind a capture here? */
            if ((flags & SIG_ELEM_IS_CAPTURE) != 0) {
                /* Capture the arguments from this point forwards into a Capture.
                 * Of course, if there's no variable name we can (cheaply) do pretty
                 * much nothing. */
                param.get_attribute_native(tc, gcx.Parameter, "$!variable_name", HINT_variable_name);
                if (tc.native_s == null) {
                   bindFail = BIND_RESULT_OK;
                }
                else {
                    SixModelObject posArgs = gcx.EMPTYARR.clone(tc);
                    for (int k = curPosArg; k < numPosArgs; k++) {
                        switch (csd.argFlags[k]) {
                        case CallSiteDescriptor.ARG_OBJ:
                            posArgs.push_boxed(tc, (SixModelObject)args[k]);
                            break;
                        case CallSiteDescriptor.ARG_INT:
                            posArgs.push_boxed(tc, RakOps.p6box_i((long)args[k], tc));
                            break;
                        case CallSiteDescriptor.ARG_NUM:
                            posArgs.push_boxed(tc, RakOps.p6box_n((double)args[k], tc));
                            break;
                        case CallSiteDescriptor.ARG_STR:
                            posArgs.push_boxed(tc, RakOps.p6box_s((String)args[k], tc));
                            break;
                        }
                    }
                    SixModelObject namedArgs = vmHashOfRemainingNameds(tc, gcx, namedArgsCopy, args);

                    SixModelObject capType = gcx.Capture;
                    SixModelObject capSnap = capType.st.REPR.allocate(tc, capType.st);
                    capSnap.bind_attribute_boxed(tc, capType, "@!list", HINT_CAPTURE_list, posArgs);
                    capSnap.bind_attribute_boxed(tc, capType, "%!hash", HINT_CAPTURE_hash, namedArgs);

                    bindFail = bindOneParam(tc, gcx, cf, param, capSnap, CallSiteDescriptor.ARG_OBJ,
                        noNomTypeCheck, false, error);
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
                    nextParam.get_attribute_native(tc, gcx.Parameter, "$!flags", HINT_flags);
                    if (((int)tc.native_i & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED)) != 0)
                        suppressArityFail = true;
                }
            }

            /* Could it be a named slurpy? */
            else if ((flags & SIG_ELEM_SLURPY_NAMED) != 0) {
                SixModelObject slurpy = vmHashOfRemainingNameds(tc, gcx, namedArgsCopy, args);
                SixModelObject bindee = gcx.Hash.st.REPR.allocate(tc, gcx.Hash.st);
                bindee.bind_attribute_boxed(tc, gcx.Map, "$!storage",
                    HINT_ENUMMAP_storage, slurpy);
                bindFail = bindOneParam(tc, gcx, cf, param, bindee, CallSiteDescriptor.ARG_OBJ,
                    noNomTypeCheck, true, error);
                if (bindFail != 0)
                    return bindFail;

                /* Nullify named arguments hash now we've consumed it, to mark all
                 * is well. */
                namedArgsCopy = null;
            }

            /* Otherwise, maybe it's a positional of some kind. */
            else if (namedNames == null) {
                /* Slurpy or LoL-slurpy? */
                if ((flags & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_LOL | SIG_ELEM_SLURPY_ONEARG)) != 0) {
                    /* Create Raku array, create VM array of all remaining things,
                     * then store it. */
                    SixModelObject slurpy = gcx.EMPTYARR.clone(tc);
                    while (curPosArg < numPosArgs) {
                        switch (csd.argFlags[curPosArg]) {
                        case CallSiteDescriptor.ARG_OBJ:
                            slurpy.push_boxed(tc, (SixModelObject)args[curPosArg]);
                            break;
                        case CallSiteDescriptor.ARG_INT:
                            slurpy.push_boxed(tc, RakOps.p6box_i((long)args[curPosArg], tc));
                            break;
                        case CallSiteDescriptor.ARG_NUM:
                            slurpy.push_boxed(tc, RakOps.p6box_n((double)args[curPosArg], tc));
                            break;
                        case CallSiteDescriptor.ARG_STR:
                            slurpy.push_boxed(tc, RakOps.p6box_s((String)args[curPosArg], tc));
                            break;
                        }
                        curPosArg++;
                    }

                    SixModelObject slurpyType = (flags & SIG_ELEM_IS_RAW) != 0 ? gcx.List : gcx.Array;
                    SixModelObject sm = Ops.findmethod(slurpyType,
                        (flags & SIG_ELEM_SLURPY_ONEARG) != 0 ? "from-slurpy-onearg" :
                        (flags & SIG_ELEM_SLURPY_POS) != 0 ? "from-slurpy-flat" : "from-slurpy",
                        tc);
                    Ops.invokeDirect(tc, sm, slurpyFromArgs, new Object[] { slurpyType, slurpy });
                    SixModelObject bindee = Ops.result_o(tc.curFrame);

                    bindFail = bindOneParam(tc, gcx, cf, param, bindee, CallSiteDescriptor.ARG_OBJ,
                        noNomTypeCheck, false, error);
                    if (bindFail != 0)
                        return bindFail;
                }

                /* Otherwise, a positional. */
                else {
                    /* Do we have a value? */
                    if (curPosArg < numPosArgs) {
                        /* Easy - just bind that. */
                        bindFail = bindOneParam(tc, gcx, cf, param, args[curPosArg],
                            csd.argFlags[curPosArg], noNomTypeCheck, false, error);
                        if (bindFail != 0)
                            return bindFail;
                        curPosArg++;
                    }
                    else {
                        /* No value. If it's optional, fetch a default and bind that;
                         * if not, we're screwed. Note that we never nominal type check
                         * an optional with no value passed. */
                        if ((flags & SIG_ELEM_IS_OPTIONAL) != 0) {
                            bindFail = bindOneParam(tc, gcx, cf, param,
                                handleOptional(tc, gcx, flags, param, cf),
                                CallSiteDescriptor.ARG_OBJ, false, false, error);
                            if (bindFail != 0)
                                return bindFail;
                        }
                        else {
                            if (error != null)
                                error[0] = arityFail(tc, gcx, params, (int)numParams, numPosArgs, false);
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
                        namedNames.at_pos_native(tc, j);
                        String name = tc.native_s;
                        lookup = namedArgsCopy.remove(name);
                        if (lookup != null)
                            break;
                    }
                }

                /* Did we get one? */
                if (lookup == null) {
                    /* Nope. We'd better hope this param was optional... */
                    if ((flags & SIG_ELEM_IS_OPTIONAL) != 0) {
                        bindFail = bindOneParam(tc, gcx, cf, param,
                            handleOptional(tc, gcx, flags, param, cf),
                            CallSiteDescriptor.ARG_OBJ, false, false, error);
                    }
                    else if (!suppressArityFail) {
                        if (error != null) {
                            namedNames.at_pos_native(tc, 0);
                            error[0] = "Required named argument '" +
                                tc.native_s +
                                "' not passed";
                        }
                        return BIND_RESULT_FAIL;
                    }
                }
                else {
                    bindFail = bindOneParam(tc, gcx, cf, param, args[lookup >> 3],
                        (byte)(lookup & 7), noNomTypeCheck, false, error);
                }

                /* If we got a binding failure, return it. */
                if (bindFail != 0)
                    return bindFail;
            }
        }

        /* Do we have any left-over args? */
        if (curPosArg < numPosArgs && !suppressArityFail) {
            /* Oh noes, too many positionals passed. */
            if (error != null)
                error[0] = arityFail(tc, gcx, params, (int)numParams, numPosArgs, true);
            return BIND_RESULT_FAIL;
        }
        if (namedArgsCopy != null && namedArgsCopy.size() > 0) {
            /* Oh noes, unexpected named args. */
            if (error != null) {
                int numExtra = namedArgsCopy.size();
                if (numExtra == 1) {
                    for (String name : namedArgsCopy.keySet())
                        error[0] = "Unexpected named argument '" + name + "' passed";
                }
                else {
                    boolean first = true;
                    error[0] = numExtra + " unexpected named arguments passed (";
                    for (String name : namedArgsCopy.keySet()) {
                        if (!first)
                            error[0] += ", ";
                        else
                            first = false;
                        error[0] += name;
                    }
                    error[0] += ")";
                }
            }
            return BIND_RESULT_FAIL;
        }

        /* If we get here, we're done. */
        return BIND_RESULT_OK;
    }

    /* Takes any nameds we didn't capture yet and makes a VM Hash of them. */
    private static SixModelObject vmHashOfRemainingNameds(ThreadContext tc, RakOps.GlobalExt gcx, HashMap<String, Integer> namedArgsCopy, Object[] args) {
        SixModelObject slurpy = gcx.Mu;
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
                    slurpy.bind_key_boxed(tc, name, RakOps.p6box_i((long)args[lookup >> 3], tc));
                    break;
                case CallSiteDescriptor.ARG_NUM:
                    slurpy.bind_key_boxed(tc, name, RakOps.p6box_n((double)args[lookup >> 3], tc));
                    break;
                case CallSiteDescriptor.ARG_STR:
                    slurpy.bind_key_boxed(tc, name, RakOps.p6box_s((String)args[lookup >> 3], tc));
                    break;
                }
            }
        }
        return slurpy;
    }
}
