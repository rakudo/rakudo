package org.raku.rakudo;

import org.raku.nqp.runtime.*;
import org.raku.nqp.sixmodel.STable;
import org.raku.nqp.sixmodel.SixModelObject;
import org.raku.nqp.sixmodel.StorageSpec;
import org.raku.nqp.sixmodel.reprs.P6Opaque;
import org.raku.nqp.sixmodel.reprs.JavaObjectWrapper;
import org.raku.nqp.sixmodel.reprs.VMArrayInstance;

import java.net.URL;
import java.net.MalformedURLException;
import java.net.URLClassLoader;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

import org.raku.rakudo.RakOps;
import org.raku.rakudo.RakOps.GlobalExt;

import java.lang.invoke.*;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.lang.reflect.InvocationTargetException;

import org.raku.nqp.sixmodel.reprs.NativeCall.ArgType;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Handle;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;


public class RakudoJavaInterop extends BootJavaInterop {

    static class DispatchCallSite extends MutableCallSite {

        private final String methname;
        private Object[] handleList;
        private boolean forCtors;
        private String declaringClass;
        private String[] handleDescs = null;
        private int handlePos = -1;
        private int offset;
        private ThreadContext tc = null;

        final MethodHandle fallback;

        public static CallFrame scf;

        public DispatchCallSite(String methname, MethodType type, Object[] handleList) {
            super(type);
            this.methname = methname;
            this.fallback = FALLBACK.bindTo(this);
            this.handleList = handleList;
            this.forCtors = false;
        }

        public DispatchCallSite(String methname, MethodType type, String declaringClass) {
            super(type);
            this.methname = methname;
            this.fallback = FALLBACK.bindTo(this);
            this.forCtors = true;
            this.declaringClass = declaringClass;
        }

        Object[] parseArgArray(Object[] inArgs) throws Throwable {
            // XXX: checking the first arg for concreteness is a hack to identify static methods
            offset = ( forCtors || Ops.isconcrete((SixModelObject) inArgs[0], tc) == 0 ) ? 1 : 0;
            GlobalExt gcx = RakOps.key.getGC(tc);
            Object[] outArgs = new Object[inArgs.length - offset];
            int i = offset;
            for(; i < inArgs.length; ++i) {
                if( Ops.islist( (SixModelObject) inArgs[i], tc ) == 1 ) {
                    outArgs[i - offset] = BootJavaInterop.marshalOutRecursive( (SixModelObject) inArgs[i], tc, null);
                }
                else if(Ops.istype( (SixModelObject) inArgs[i], gcx.List, tc ) == 1
                     || Ops.istype( (SixModelObject) inArgs[i], gcx.Array, tc ) == 1 ) {
                    outArgs[i - offset] = RakudoJavaInterop.marshalOutRecursive( (SixModelObject) inArgs[i], tc, null);
                }
                else {
                    outArgs[i - offset] = RakudoJavaInterop.parseSingleArg( (SixModelObject) inArgs[i], tc);
                }
            }
            return outArgs;
        }

        int findHandle(Object[] parsedArgs) throws Throwable {
            handlePos = -1;
            if( handleDescs == null ) {
                handleDescs = new String[handleList.length];
                for( int i = 0; i < handleList.length; ++i ) {
                    if(forCtors) {
                        handleDescs[i] = Type.getConstructorDescriptor((Constructor<?>) handleList[i]);
                    }
                    else {
                        handleDescs[i] = ((MethodHandle)handleList[i]).type().toMethodDescriptorString();
                    }
                }
            }
            for( int i = 0; i < handleDescs.length; ++i ) {
                if( argsMatch(handleDescs[i], parsedArgs) ) {
                    handlePos = i;
                }
            }
            return handlePos;
        }

        void failDispatch(Object[] parsedArgs) {
            String types = "void";
            boolean first = true;
            if( parsedArgs != null ) {
                for( Object arg : parsedArgs ) {
                    if( first ) {
                        types = arg.getClass().toString();
                        first = false;
                    }
                    else {
                        types += ", " + arg.getClass().toString();
                    }
                }
            }
            throw ExceptionHandling.dieInternal(tc,
                "Couldn't find a " + (forCtors ? "constructor" : "method" ) + " with types " + types +".");
        }

        boolean argsMatch(String desc, Object[] parsedArgs) {
            String fakeDesc = "";
            for( Object arg : parsedArgs ) {
                fakeDesc += Type.getType(arg.getClass());
            }
            desc = desc.substring(desc.indexOf("(") + 1, desc.lastIndexOf(")"));
            return desc.equals(fakeDesc);
        }

        Object deepArrayCast(Object obj, Type type) throws Throwable {
            Type elemType = type;
            int typeDepth = type.getDimensions();
            elemType = elemType.getElementType();

            int objDepth = 0;
            Object val = obj;
            while( val.getClass().getComponentType() != null ) {
                val = ((Object[])val)[0];
                objDepth++;
            }

            if( objDepth != typeDepth ) {
                return null;
            }

            Object targetType = castObjectToType(val, elemType);
            if( targetType != null ) {
                return deepArrayCast(obj, elemType, type.getDimensions());
            }

            return null;
        }

        Object deepArrayCast(Object obj, Type type, int depth) throws Throwable {
            Object retVal = null;
            Class<?> klass = null;
            switch( type.getSort() ) {
                case Type.BOOLEAN:
                    klass = boolean.class;
                    break;
                case Type.BYTE:
                    klass = byte.class;
                    break;
                case Type.SHORT:
                    klass = short.class;
                    break;
                case Type.INT:
                    klass = int.class;
                    break;
                case Type.LONG:
                    klass = long.class;
                    break;
                case Type.CHAR:
                    klass = char.class;
                    break;
                case Type.FLOAT:
                    klass = float.class;
                    break;
                case Type.DOUBLE:
                    klass = double.class;
                    break;
                case Type.OBJECT:
                    klass = Class.forName(type.getInternalName().replace('/', '.'), false, tc.gc.byteClassLoader);
                    break;
                case Type.ARRAY:
                default:

            }
            if( depth == 1 ) {
                retVal = Array.newInstance(klass, ((Object[]) obj).length);
                for( int i = 0; i < ((Object[]) obj).length; ++i ) {
                    Object val = castObjectToType(((Object[]) obj)[i], type);
                    Array.set(retVal, i, val);
                }
            }
            else {
                for( int i = 0; i < ((Object[]) obj).length; ++i ) {
                    Object val = deepArrayCast(((Object[]) obj)[i], type, depth - 1);
                    if( retVal == null )
                        retVal = Array.newInstance(val.getClass(), ((Object[]) obj).length);
                    Array.set(retVal, i, val);
                }
            }
            return retVal;
        }

        Object castObjectToType(Object obj, Type type) throws Throwable {
            Object retVal = null;
            switch( type.getSort() ) {
                case Type.BOOLEAN:
                    if( obj.getClass().equals(Long.class) ) {
                        retVal = obj != null
                            ? ((Long) obj) == 0
                                ? new Boolean(false)
                                : new Boolean(true)
                            : null;
                    }
                    else if( obj.getClass().equals(Boolean.class) ) {
                        retVal = obj;
                    }
                break;
                case Type.BYTE:
                    if( obj.getClass().equals(Long.class) ) {
                        retVal = obj != null ? ((Long)obj).byteValue() : null;
                    }
                    break;
                case Type.SHORT:
                    if( obj.getClass().equals(Long.class) ) {
                        retVal = obj != null ? ((Long)obj).shortValue() : null;
                    }
                    break;
                case Type.INT:
                    if( obj.getClass().equals(Long.class) ) {
                        retVal = obj != null ? ((Long)obj).intValue() : null;
                    }
                    break;
                case Type.LONG:
                    if( obj.getClass().equals(Long.class) ) {
                        retVal = obj;
                    }
                    break;
                case Type.CHAR:
                    if( obj.getClass().equals(String.class) ) {
                        retVal = obj;
                    }
                    break;
                case Type.FLOAT:
                    if( obj.getClass().equals(Double.class) ) {
                        retVal = obj != null ? ((Double)obj).floatValue() : null;
                    }
                    break;
                case Type.DOUBLE:
                    if( obj.getClass().equals(Double.class) ) {
                        retVal = obj;
                    }
                    break;
                case Type.OBJECT:
                    Class<?> argType = Class.forName(type.getInternalName().replace('/', '.'), false, tc.gc.byteClassLoader);
                    if( argType.isAssignableFrom(obj.getClass()) ) {
                        retVal = obj;
                    }
                    else if( argType.equals(Boolean.class) && obj.getClass().equals(Long.class) ){
                        retVal = new Boolean( ((Long)obj) == 0 ? false : true);
                    }
                    else if( argType.equals(Byte.class) && obj.getClass().equals(Long.class) ){
                        retVal = new Byte( ((Long)obj).byteValue() );
                    }
                    else if( argType.equals(Short.class) && obj.getClass().equals(Long.class) ){
                        retVal = new Short( ((Long)obj).shortValue() );
                    }
                    else if( argType.equals(Integer.class) && obj.getClass().equals(Long.class) ){
                        retVal = new Integer( ((Long)obj).intValue() );
                    }
                    else if( argType.equals(Long.class) && obj.getClass().equals(Long.class) ){
                        retVal = (Long) obj;
                    }
                    else if( argType.equals(Float.class) && obj.getClass().equals(Double.class) ){
                        retVal = new Float( ((Double)obj).floatValue() );
                    }
                    else if( argType.equals(Double.class) && obj.getClass().equals(Double.class) ){
                        retVal = (Double) obj;
                    }
                    else if( argType.equals(Character.class) && obj.getClass().equals(String.class) ){
                        retVal = new Character( ((String)obj).charAt(0));
                    }
                    else if( argType.equals(String.class) && obj.getClass().equals(String.class) ){
                        retVal = (String) obj;
                    }
                    break;
                case Type.ARRAY:
                    if( obj.getClass().getComponentType() != null ) {
                        retVal = deepArrayCast(obj, type);
                    }
                    break;
                default:
                    throw new ArrayIndexOutOfBoundsException(1);
            }

            return retVal;
        }

        int findHandleWithArgsCasting(Object[] parsedArgs) throws Throwable {
            for( int j = 0; j < handleDescs.length; ++j ) {
                boolean possible = false;
                Type[] mtypes = Type.getArgumentTypes(handleDescs[j]);
                if( mtypes.length != parsedArgs.length ) continue;

                for( int i = 0; i < mtypes.length; ++i ) {
                    Object newValue = castObjectToType(parsedArgs[i], mtypes[i]);

                    if( newValue != null ) {
                        possible = true;
                        parsedArgs[i] = newValue;
                    }
                    else {
                        possible = false;
                        break;
                    }
                }
                if(possible) {
                    return j;
                }
            }
            return -1;
        }

        Object fallback(Object intc, Object incf, Object incsd, Object[] args) throws Throwable {
            tc = (ThreadContext) intc;
            CallFrame cf = (CallFrame) incf;
            CallSiteDescriptor csd = (CallSiteDescriptor) incsd;
            Object[] parsedArgs = parseArgArray(args);

            Ops.debugnoop((SixModelObject) args[0], (ThreadContext) intc);

            /* debug
            for(int i = 0; i < parsedArgs.length; ++i ) {
                System.out.println("parsed arg " + i + " as " + parsedArgs[i].getClass());
            }
            // */

            if(forCtors) {
                this.handleList = Class.forName(Type.getObjectType(((String) declaringClass).replace('/', '.')).getInternalName(),
                    false, tc.gc.byteClassLoader).getConstructors();
            }

            // first, check for a cached handle, only recheck if it doesn't match
            if( handlePos == -1 || handleDescs != null && !argsMatch(handleDescs[handlePos], parsedArgs) ) {
                handlePos = -1;
                handlePos = findHandle(parsedArgs);
            }
            // we should have a handle now, unless we have to cast arguments around
            if( handlePos == -1 ) {
                handlePos = findHandleWithArgsCasting(parsedArgs);
            }
            // that should have worked, if not there's nothing we can dispatch to
            if( handlePos == -1 ) {
                failDispatch(parsedArgs);
            }

            /* debug
            if(forCtors) {
                System.out.println("ctor cand: " + ((Constructor) this.handleList[handlePos]).toGenericString());
            } else {
                System.out.println("mhand cand: " + (MethodHandle) this.handleList[handlePos]);
            }
            // */

            MethodHandle rfh;
            try {
                rfh = MethodHandles.lookup().findStatic(RakudoJavaInterop.class, "filterReturnValueMethod",
                    MethodType.fromMethodDescriptorString(
                        "(Ljava/lang/Object;Lorg/raku/nqp/runtime/ThreadContext;)Ljava/lang/Object;",
                        tc.gc.byteClassLoader));
            } catch (NoSuchMethodException|IllegalAccessException nsme) {
                throw ExceptionHandling.dieInternal(tc,
                    "Couldn't find the method for filtering return values from Java.");
            }

            Object out;
            if(forCtors) {
                Object instance = ((Constructor) handleList[handlePos]).newInstance(parsedArgs);
                out = rfh.invoke(instance, tc);
            }
            else {
                Object retVal = ((MethodHandle) handleList[handlePos]).invokeWithArguments(parsedArgs);
                out = rfh.invoke(retVal, tc);
            }

            return out;
        }

        private static final MethodHandle FALLBACK;

        static {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            try {
                FALLBACK = lookup.findVirtual(DispatchCallSite.class,
                        "fallback", MethodType.genericMethodType(3, true));
            } catch (ReflectiveOperationException e) {
                throw new LinkageError(e.getMessage(), e);
            }
        }
    }

    public RakudoJavaInterop(GlobalContext gc) {
        super(gc);
    }

    /**
     * Helper for not having to write recursive bytecode generation.
     * Public because of runtime visibility.
     */
    public static Object marshalOutRecursive(SixModelObject in, ThreadContext tc, Class<?> what) throws Throwable {
        Object out = null;
        GlobalExt gcx = RakOps.key.getGC(tc);
        long size = 0;
        if( Ops.islist(in, tc) == 1 ) {
            return BootJavaInterop.marshalOutRecursive(in, tc, what);
        }
        else if(Ops.istype(in, gcx.List, tc) == 1) {
            SixModelObject p6list = Ops.decont(in, tc);
            SixModelObject methElems = Ops.findmethod(p6list, "elems", tc);
            Ops.invokeDirect(tc, methElems, Ops.invocantCallSite, new Object[] { p6list });
            try {
                size = Ops.result_i(tc.curFrame);
            }
            catch(Throwable t) {
                ExceptionHandling.dieInternal(tc, "Cannot marshal a lazy list to Java");
            }

            // TODO get half the work of parseSingleArg() abstracted out of there
            //      i.e. the type mapping between Java and Rakudo, so we
            //      can actually do something with the "of" and thus
            //      can dispatch to e.g. int[] instead of just Object[]
            SixModelObject methOf = Ops.findmethod(p6list, "of", tc);
            Ops.invokeDirect(tc, methOf, Ops.invocantCallSite, new Object[] { p6list });
            SixModelObject ofType = Ops.result_o(tc.curFrame);
            SixModelObject methAtPos = Ops.findmethod(p6list, "AT-POS", tc);

            for(int i = 0; i < size; i++) {
                Ops.invokeDirect(tc, methAtPos,
                    Ops.storeCallSite,
                    new Object[] { p6list, Ops.box_i((long)i, gcx.Int, tc) });
                Object cur = Ops.result_o(tc.curFrame);
                Object value = null;
                if(Ops.islist((SixModelObject) cur, tc) == 1) {
                    ((Object[]) out)[i] = BootJavaInterop.marshalOutRecursive(in, tc, what);
                }
                else if(Ops.istype((SixModelObject) cur, gcx.List, tc) == 1) {
                    value = marshalOutRecursive((SixModelObject) cur, tc, what);
                }
                else {
                    value = parseSingleArg((SixModelObject) cur, tc);
                }
                if( out == null ) {
                    out = Array.newInstance(Object.class, (int)size);
                }
                Array.set(out, i, value);
            }
            if (List.class.isAssignableFrom(what)) {
                out = Arrays.asList((Object[]) out);
            }
        }
        else if(Ops.istype(in, gcx.Hash, tc) == 1) {
            SixModelObject p6hash = Ops.decont(in, tc);
            SixModelObject methElems = Ops.findmethod(p6hash, "elems", tc);
            Ops.invokeDirect(tc, methElems, Ops.invocantCallSite, new Object[] { p6hash });
            try {
                size = Ops.result_i(tc.curFrame);
            }
            catch(Throwable t) {
                ExceptionHandling.dieInternal(tc, "Cannot marshal a lazy hash to Java");
            }
            SixModelObject methKeys = Ops.findmethod(p6hash, "keys", tc);
            Ops.invokeDirect(tc, methKeys, Ops.invocantCallSite, new Object[] { p6hash });
            SixModelObject p6keyList = Ops.result_o(tc.curFrame);
            SixModelObject methAtPos = Ops.findmethod(p6keyList, "AT-POS", tc);
            SixModelObject methAtKey = Ops.findmethod(p6hash, "AT-KEY", tc);

            out = new HashMap<String, Object>();
            for(int i = 0; i < size; i++) {
                Ops.invokeDirect(tc, methAtPos, Ops.storeCallSite, new Object[] { p6keyList, Ops.box_i((long)i, gcx.Int, tc) });
                SixModelObject p6key = Ops.result_o(tc.curFrame);
                Ops.invokeDirect(tc, methAtKey, Ops.storeCallSite, new Object[] { p6hash, p6key });
                Object cur = Ops.result_o(tc.curFrame);
                Object value = null;
                if(Ops.islist((SixModelObject) cur, tc) == 1) {
                    value = BootJavaInterop.marshalOutRecursive(in, tc, what);
                }
                else if(Ops.istype((SixModelObject) cur, gcx.List, tc) == 1) {
                    value = marshalOutRecursive((SixModelObject) cur, tc, what);
                }
                else {
                    value = parseSingleArg((SixModelObject) cur, tc);
                }
                ((HashMap) out).put(Ops.unbox_s(p6key, tc), value);
            }
        }
        // TODO associative types, which could for starters default to Map<Object> similar
        //      to how Positionals currently do, but we will want "of" checking there too
        return out;
    }

    public static Object parseSingleArg(SixModelObject inArg, ThreadContext tc) {
        Object outArg = null;
        // there doesn't seem to be an actual type Bool in gc or gcx
        if( !Ops.typeName((SixModelObject) inArg, tc).equals("Bool") ) {
            // one decont for native types...
            StorageSpec outerSS = Ops.decont((SixModelObject) inArg, tc)
                .st.REPR.get_storage_spec(tc, ((SixModelObject)inArg).st);
            // ...and two for boxeds
            StorageSpec innerSS = Ops.decont(Ops.decont((SixModelObject) inArg, tc), tc)
                .st.REPR.get_storage_spec(tc, Ops.decont((SixModelObject)inArg, tc).st);
            if( (outerSS.can_box & StorageSpec.CAN_BOX_NUM) != 0 ) {
                Double value = Ops.unbox_n((SixModelObject) inArg, tc);
                outArg = value;
            }
            else if( (outerSS.can_box & StorageSpec.CAN_BOX_STR) != 0 ) {
                String value = Ops.unbox_s((SixModelObject) inArg, tc);
                outArg = value;
            }
            else if( (outerSS.can_box & StorageSpec.CAN_BOX_INT) != 0 ) {
                Long value = Ops.unbox_i((SixModelObject) inArg, tc);
                outArg = value;
            }
            else if( (innerSS.can_box & StorageSpec.CAN_BOX_NUM) != 0 ) {
                Double value = Ops.unbox_n((SixModelObject) inArg, tc);
                outArg = value;
            }
            else if( (innerSS.can_box & StorageSpec.CAN_BOX_STR) != 0 ) {
                String value = Ops.unbox_s((SixModelObject) inArg, tc);
                outArg = value;
            }
            else if( (innerSS.can_box & StorageSpec.CAN_BOX_INT) != 0 ) {
                Long value = Ops.unbox_i((SixModelObject) inArg, tc);
                outArg = value;
            }
            else {
                try {
                    outArg = RuntimeSupport.unboxJava(Ops.decont((SixModelObject) inArg, tc));
                } catch (Exception e) {
                    throw ExceptionHandling.dieInternal(tc,
                        "Couldn't parse arguments in Java call. (Did you pass a type object?)");
                }
            }
        }
        else {
            if( Ops.istrue((SixModelObject) inArg, tc) == 1 ) {
                Boolean value = new Boolean(true);
                outArg = value;
            }
            else if( Ops.isfalse((SixModelObject) inArg, tc) == 1 ) {
                Boolean value = new Boolean(false);
                outArg = value;
            }
        }
        return outArg;
    }

    @Override
    protected void marshalOut(MethodContext c, Class<?> what, int ix) {
        MethodVisitor mv = c.mv;

        if(what.getComponentType() != null
            || List.class.isAssignableFrom(what)
            || Map.class.isAssignableFrom(what)) {
            emitGetFromNQP(c, ix, storageForType(what));
            mv.visitVarInsn(Opcodes.ALOAD, c.tcLoc);
            mv.visitLdcInsn(Type.getType(what));
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, "org/raku/rakudo/RakudoJavaInterop", "marshalOutRecursive",
                Type.getMethodDescriptor(Type.getType(Object.class), TYPE_SMO, TYPE_TC, Type.getType(Class.class)));
        }

        else {
            super.marshalOut(c, what, ix);
        }
    }

    public static Object filterReturnValueMethod(Object in, ThreadContext tc) {
        GlobalExt gcx = RakOps.key.getGC(tc);
        if(in == null) {
            return gcx.Nil;
        }

        Class<?> what = in.getClass();
        Object out = null;
        if(what == void.class) {
            out = null;
        }
        else if(what == int.class || what == Integer.class) {
            out = new Long((int) in);
        }
        else if( what == short.class || what == Short.class) {
            out = new Long((short) in);
        } else if( what == byte.class || what == Byte.class) {
            out = new Long((byte) in);
        } else if( what == boolean.class || what == Boolean.class) {
            out = (boolean) in ? gcx.True : gcx.False;
        }
        else if (what == long.class || what == double.class || what == String.class || what == SixModelObject.class || what == Long.class || what == Double.class) {
            out = in;
        }
        else if (what == float.class || what == Float.class) {
            out = new Double((float) in);
        }
        else if (what == char.class || what == Character.class) {
            out = String.valueOf((char) in);
        }
        else {
            STable stable = null;
            if(gcx.rakudoInterop.commonSTable != null) {
                stable = gcx.rakudoInterop.commonSTable;
            }
            if (what.isArray()) {
                SixModelObject ARRAY = tc.gc.BOOTArray;
                out = ARRAY.st.REPR.allocate(tc, ARRAY.st);
                if(stable == null) {
                    stable = ARRAY.st;
                }
                if(what.getComponentType().isPrimitive()) {
                    if(what.getComponentType() == long.class
                    || what.getComponentType() == int.class
                    || what.getComponentType() == short.class
                    || what.getComponentType() == byte.class
                    || what.getComponentType() == boolean.class) {
                        for(int i = 0; i < ((int[])in).length; ++i) {
                            SixModelObject cur = RakOps.p6box_i(((int[])in)[i], tc);
                            Ops.bindpos((SixModelObject) out, i, cur, tc);
                        }
                    }
                    else if (what.getComponentType() == String.class
                    || what.getComponentType() == char.class) {
                        for(int i = 0; i < ((int[])in).length; ++i) {
                            SixModelObject cur = RakOps.p6box_s(((String[])in)[i], tc);
                            Ops.bindpos((SixModelObject) out, i, cur, tc);
                        }
                    }
                    else if (what.getComponentType() == float.class
                    || what.getComponentType() == double.class) {
                        for(int i = 0; i < ((int[])in).length; ++i) {
                            SixModelObject cur = RakOps.p6box_n(((double[])in)[i], tc);
                            Ops.bindpos((SixModelObject) out, i, cur, tc);
                        }
                    }
                }
                else {
                    for( int i = 0; i < ((Object[])in).length; ++i ) {
                        // need to special-case String.class here
                        SixModelObject cur;
                        if( what.getComponentType().equals(String.class) ) {
                            cur =  RakOps.p6box_s(((String[])in)[i], tc);
                        }
                        else {
                            cur = RuntimeSupport.boxJava(((Object[])in)[i],
                                    gcx.rakudoInterop.getSTableForClass(what.getComponentType()));
                        }
                        Ops.bindpos((SixModelObject) out, i, cur, tc);
                    }
                }
                SixModelObject outList = Ops.create(gcx.List, tc);
                SixModelObject iterbuffer = Ops.create(gcx.IterationBuffer, tc);
                Ops.bindattr(outList, gcx.List, "$!reified", iterbuffer, tc);
                long elems = Ops.elems((SixModelObject) out, tc);
                for( int i = 0; i < elems; ++i ) {
                    Ops.bindpos(iterbuffer, i, Ops.atpos((SixModelObject) out, i, tc), tc);
                }
                out = outList;
            }
            else {
                out = RuntimeSupport.boxJava(in, gcx.rakudoInterop.getSTableForClass(what));
            }
        }

        if (what == String.class || what == char.class || what == Character.class)
            Ops.return_s((String) out, tc.curFrame);
        else if (what == float.class || what == double.class || what == Double.class || what == Float.class)
            Ops.return_n(((Double)out).doubleValue(), tc.curFrame);
        else if (what != void.class && ( what.isPrimitive() || what == Long.class
              || what == Integer.class || what == Short.class || what == Byte.class ))
            Ops.return_i(((Long)out).longValue(), tc.curFrame);
        else
            Ops.return_o((SixModelObject) out, tc.curFrame);

        // the conditional is rather sketchy, but seems to be needed to
        // correctly return a new instance when we're called from
        // ConstructorDispatchCallSite, probably because of
        // Raku' .new creating a new CallFrame or something..?
        return Ops.result_o(tc.curFrame) != null ? Ops.result_o(tc.curFrame) : Ops.result_o(tc.curFrame.caller);
    }

    public static CallSite multiBootstrap(MethodHandles.Lookup lookup, String name, MethodType type, Object... hlist) {
        DispatchCallSite cs = new DispatchCallSite(name, type, hlist);
        cs.setTarget(cs.fallback);
        return cs;
    }

    protected MethodContext startVarArityCallout(ClassContext cc, String desc) {
        MethodContext mc = new MethodContext();
        mc.cc = cc;
        MethodVisitor mv = mc.mv = cc.cv.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "qb_"+(cc.nextCallout++),
                Type.getMethodDescriptor(Type.VOID_TYPE, TYPE_CU, TYPE_TC, TYPE_CR, TYPE_CSD, TYPE_AOBJ),
                null, null);
        AnnotationVisitor av = mv.visitAnnotation("Lorg/raku/nqp/runtime/CodeRefAnnotation;", true);
        av.visit("name", "callout "+cc.target.getName()+" "+desc);
        av.visitEnd();
        mv.visitCode();
        cc.descriptors.add(desc);

        mc.argsLoc = 4;
        mc.csdLoc = 3;
        mc.cfLoc = 5;
        mc.tcLoc = 1;

        mv.visitTypeInsn(Opcodes.NEW, "org/raku/nqp/runtime/CallFrame");
        mv.visitInsn(Opcodes.DUP);
        mv.visitVarInsn(Opcodes.ALOAD, 1); // tc
        mv.visitVarInsn(Opcodes.ALOAD, 2); // cr
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "org/raku/nqp/runtime/CallFrame", "<init>", Type.getMethodDescriptor(Type.VOID_TYPE, TYPE_TC, TYPE_CR));
        mv.visitVarInsn(Opcodes.ASTORE, 5); // cf;

        mv.visitLabel(mc.tryStart = new Label());

        mv.visitVarInsn(Opcodes.ALOAD, 5); // cf
        mv.visitVarInsn(Opcodes.ALOAD, 3); // csd
        mv.visitVarInsn(Opcodes.ALOAD, 4); // args
        emitInteger(mc, 1);
        emitInteger(mc, -1);
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, TYPE_OPS.getInternalName(), "checkarity", Type.getMethodDescriptor(TYPE_CSD, TYPE_CF, TYPE_CSD, TYPE_AOBJ, Type.INT_TYPE, Type.INT_TYPE));
        mv.visitVarInsn(Opcodes.ASTORE, 3); // csd
        mv.visitVarInsn(Opcodes.ALOAD, 1); // tc
        mv.visitFieldInsn(Opcodes.GETFIELD, TYPE_TC.getInternalName(), "flatArgs", TYPE_AOBJ.getDescriptor());
        mv.visitVarInsn(Opcodes.ASTORE, 4); // args

        return mc;
    }

    protected void createAdaptorMultiDispatch(ClassContext cc, ArrayList<Method> mlist) {
        String name = "mmd+" + mlist.get(0).getName();
        String desc = "method/" + name + "/([Ljava/lang/Object;)Ljava/lang/Object;";

        MethodContext mc = startVarArityCallout(cc, desc);

        // what if this is the only static one?
        if (!Modifier.isStatic(mlist.get(0).getModifiers())) marshalOut(mc, mlist.get(0).getDeclaringClass(), 0);
        Handle disphandle = new Handle(Opcodes.H_INVOKESTATIC, "org/raku/rakudo/RakudoJavaInterop", "multiBootstrap",
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)" +
                "Ljava/lang/invoke/CallSite;");
        Handle[] candhandles = new Handle[mlist.size()];
        int i = 0;
        for(Iterator<Method> it = mlist.iterator(); it.hasNext(); ) {
            Method next = it.next();
            candhandles[i++] = new Handle(Modifier.
                    isStatic(next.getModifiers()) ? Opcodes.H_INVOKESTATIC : Opcodes.H_INVOKEVIRTUAL,
                    next.getDeclaringClass().getName().replace('.', '/'),
                    next.getName(),
                    Type.getMethodDescriptor(next));
        }

        mc.mv.visitVarInsn(Opcodes.ALOAD, 1);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 5);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 3);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 4);
        mc.mv.visitInvokeDynamicInsn(mlist.get(0).getName(),
                "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;",
                disphandle, (Object[]) candhandles);

        endCallout(mc);
    }

    @Override
    protected SixModelObject computeHOW(ThreadContext tc, String name) {
        RakOps.GlobalExt gcx = RakOps.key.getGC(tc);
        SixModelObject mo = gcx.JavaHOW.st.REPR.allocate(tc, gcx.JavaHOW.st);
        mo.bind_attribute_boxed(tc, gcx.JavaHOW, "$!name", STable.NO_HINT,
            RakOps.p6box_s(name, tc));

        return mo;
    }

    public static CallSite constructorBootstrap(MethodHandles.Lookup lookup, String name, MethodType type, String declClass) {
        DispatchCallSite cs = new DispatchCallSite(name, type, declClass);
        cs.setTarget(cs.fallback);
        return cs;
    }

    protected void createConstructorDispatchAdaptor(ClassContext cc, Constructor<?>[] ks) {
        String desc = "method/mmd+new/([Ljava/lang/Object;)L" + ks[0].getDeclaringClass().getName().replace('.', '/') + ";";
        String className = Type.getInternalName(ks[0].getDeclaringClass());
        MethodContext mc = startVarArityCallout(cc, desc);

        Handle disphandle = new Handle(Opcodes.H_INVOKESTATIC, "org/raku/rakudo/RakudoJavaInterop", "constructorBootstrap",
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/String;)" +
                "Ljava/lang/invoke/CallSite;");

        preMarshalIn(mc, ks[0].getDeclaringClass(), 0);

        mc.mv.visitVarInsn(Opcodes.ALOAD, 1);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 5);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 3);
        mc.mv.visitVarInsn(Opcodes.ALOAD, 4);
        mc.mv.visitInvokeDynamicInsn("new",
                "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;",
                disphandle, className);

        endCallout(mc);
    }

    @Override
    protected ClassContext createAdaptor(Class<?> target) {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES);
        String className = "org/raku/nqp/generatedadaptor/"+target.getName().replace('.','/');
        cw.visit(Opcodes.V1_7, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, className, null, TYPE_CU.getInternalName(), null);

        cw.visitField(Opcodes.ACC_STATIC | Opcodes.ACC_PUBLIC, "constants", "[Ljava/lang/Object;", null, null).visitEnd();

        ClassContext cc = new ClassContext();
        cc.cv = cw;
        cc.className = className;
        cc.target = target;

        HashMap<String, Integer> multiDescs = new HashMap< >();
        for (Method m : target.getMethods()) {
            if( multiDescs.containsKey(m.getName()) ) {
                multiDescs.put(m.getName(), multiDescs.get(m.getName()) + 1);
            }
            else {
                multiDescs.put(m.getName(), new Integer(1));
            }
        }
        HashMap<String, ArrayList<Method>> multiMethods = new HashMap< >();
        for (Method m : target.getMethods()) {
            if( m.isSynthetic() ) {
                // synthetics don't get their own perl6-level method, because
                // they only exist as a visibility aid for the class we're
                // generating an adaptor for
                continue;
            }
            if( multiDescs.get(m.getName()) > 1 ) {
                if( multiMethods.get(m.getName()) == null ) {
                    multiMethods.put(m.getName(), new ArrayList<Method>());
                }
                multiMethods.get(m.getName()).add(m);
            }
            createAdaptorMethod(cc, m);
        }
        for (Iterator<Map.Entry<String, ArrayList<Method>>> msit = multiMethods.entrySet().iterator(); msit.hasNext(); ) {
            createAdaptorMultiDispatch(cc, msit.next().getValue());
        }
        for (Field f : target.getFields()) {
            if( f.isSynthetic() )
                continue;
            createAdaptorField(cc, f);
        }
        for (Constructor<?> c : target.getConstructors()) {
            if( c.isSynthetic() )
                continue;
            createAdaptorConstructor(cc, c);
        }
        // what we actually want to do is grab all the methods we generated in
        // the for() directly above and generate a varargs shortname
        // &new()-equivalent, which dispatches among the generated
        // adaptorConstructors - which aren't really constructors but static
        // methods
        if( target.getConstructors().length > 0 )
            createConstructorDispatchAdaptor(cc, target.getConstructors());
        createAdaptorSpecials(cc);
        compunitMethods(cc);

        finishClass(cc);
        /* debug
        try {
            java.nio.file.Files.write(new java.io.File(className.replace('/','_') + ".class").toPath(), cc.cv.toByteArray());
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
        // */

        return cc;
    }

    public void addToClassPath(String path) {
        path = "file:" + path;
        if (!path.endsWith("jar") && !path.endsWith("class"))
            path = path + "/";
        Class<?> klass;

        try {
            // ...but here's what we actually do:
            Method meth_addURL = URLClassLoader.class.getDeclaredMethod("addURL", new Class<?>[] { URL.class });
            meth_addURL.setAccessible(true);
            meth_addURL.invoke(getClass().getClassLoader(), new URL(path));
        }
        catch (NoSuchMethodException nsme) {
            throw ExceptionHandling.dieInternal(gc.getCurrentThreadContext(), nsme);
        }
        catch (InvocationTargetException ite) {
            throw ExceptionHandling.dieInternal(gc.getCurrentThreadContext(), ite);
        }
        catch (IllegalAccessException iae) {
            throw ExceptionHandling.dieInternal(gc.getCurrentThreadContext(), iae);
        }
        catch (MalformedURLException mue) {
            throw ExceptionHandling.dieInternal(gc.getCurrentThreadContext(), mue);
        }
    }

    @Override
    protected SixModelObject computeInterop(ThreadContext tc, Class<?> klass) {
        ClassContext adaptor = createAdaptor(klass);

        CompilationUnit adaptorUnit;
        try {
            adaptorUnit = (CompilationUnit) adaptor.constructed.newInstance();
        } catch (ReflectiveOperationException roe) {
            throw new RuntimeException(roe);
        }
        adaptorUnit.initializeCompilationUnit(tc);

        SixModelObject hash = gc.BOOTHash.st.REPR.allocate(tc, gc.BOOTHash.st);

        SixModelObject method_order = gc.BOOTArray.st.REPR.allocate(tc, gc.BOOTArray.st);
        SixModelObject methods = gc.BOOTHash.st.REPR.allocate(tc, gc.BOOTHash.st);
        SixModelObject submethods = gc.BOOTHash.st.REPR.allocate(tc, gc.BOOTHash.st);

        HashMap<String, SixModelObject> names = new HashMap< >();
        HashMap<String, ArrayList<SixModelObject>> multis = new HashMap< >();

        GlobalExt gcx = RakOps.key.getGC(tc);
        STable protoSt = gcx.JavaHOW.st;
        SixModelObject ThisHOW = computeHOW(tc, klass.getName());
        SixModelObject freshType = protoSt.REPR.type_object_for(tc, ThisHOW);

        HashMap<String, SixModelObject> mult = new HashMap< >();
        for (int i = 0; i < adaptor.descriptors.size(); i++) {
            String desc = adaptor.descriptors.get(i);
            SixModelObject cr = adaptorUnit.lookupCodeRef(i);

            int s1 = desc.indexOf('/');
            int s2 = desc.indexOf('/', s1+1);

            // dispatch methods *should* be last, but this might be a
            // spot to check if things start breaking...
            String shorten = desc.substring(s1+1, s2);
            if( shorten.contains("mmd+") ) {
                String shortmult = shorten.substring(shorten.indexOf("+") + 1);
                mult.put(shortmult, cr);
                // don't add multi candidates with mmd+ prefix
                continue;
            }
            if( names.containsKey(shorten) ) {
                names.put(shorten, null);
            }
            else {
                // there's probably a better way to do this
                if(shorten.equals("toString")) {
                    if(!names.containsKey("Str")) names.put("Str", cr);
                    if(!names.containsKey("gist")) names.put("gist", cr);
                }
                names.put(shorten, cr);
            }
            names.put(desc, cr);
        }

        for(Iterator<Map.Entry<String, SixModelObject>> it = mult.entrySet().iterator(); it.hasNext(); ) {
            Map.Entry<String, SixModelObject> ent = it.next();
            names.put(ent.getKey(), ent.getValue());
        }

        int pos = 0;
        for (Iterator<Map.Entry<String, SixModelObject>> it = names.entrySet().iterator(); it.hasNext(); ) {
            Map.Entry<String, SixModelObject> ent = it.next();
            if (ent.getValue() != null) {
                method_order.bind_pos_boxed(tc, pos++, ent.getValue());
                methods.bind_key_boxed(tc, ent.getKey(), ent.getValue());
                hash.bind_key_boxed(tc, ent.getKey(), ent.getValue());
            }
            else
                it.remove();
        }

        freshType.st.MethodCache = names;
        freshType.st.ModeFlags |= STable.METHOD_CACHE_AUTHORITATIVE;

        ThisHOW.bind_attribute_boxed(tc, gcx.JavaHOW, "%!submethods", STable.NO_HINT, submethods);
        ThisHOW.bind_attribute_boxed(tc, gcx.JavaHOW, "%!methods", STable.NO_HINT, Ops.hllizefor(methods, "Raku", tc));
        ThisHOW.bind_attribute_boxed(tc, gcx.JavaHOW, "@!method_order", STable.NO_HINT, method_order);

        hash.bind_key_boxed(tc, "/TYPE/", freshType);

        return hash;
    }
}
