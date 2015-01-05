package org.perl6.rakudo;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.STable;
import org.perl6.nqp.sixmodel.SixModelObject;
import org.perl6.nqp.sixmodel.StorageSpec;
import org.perl6.nqp.sixmodel.reprs.P6Opaque;
import org.perl6.nqp.sixmodel.reprs.JavaObjectWrapper;
import org.perl6.nqp.sixmodel.reprs.VMArrayInstance;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;

import org.perl6.rakudo.RakOps;
import org.perl6.rakudo.RakOps.GlobalExt;

import java.lang.invoke.*;

import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.perl6.nqp.sixmodel.reprs.NativeCall.ArgType;

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

        Object[] parseArgs(Object[] inArgs, ThreadContext tc) {
            // XXX: checking the first arg for concreteness is a hack to identify static methods
            int offset = ( forCtors || Ops.isconcrete((SixModelObject) inArgs[0], tc) == 0 ) ? 1 : 0;
            GlobalExt gcx = RakOps.key.getGC(tc);
            Object[] outArgs = new Object[inArgs.length - offset];
            int i = offset;
            for(; i < inArgs.length; ++i) {
                // there doesn't seem to be an actual type Bool in gc or gcx
                if( !Ops.typeName((SixModelObject) inArgs[i], tc).equals("Bool") ) {
                    if( Ops.isnum((SixModelObject) inArgs[i], tc) == 1 ) {
                        outArgs[i - offset] = Ops.unbox_n((SixModelObject) inArgs[i], tc);
                    }
                    else if( Ops.isstr((SixModelObject) inArgs[i], tc) == 1 ) {
                        outArgs[i - offset] = Ops.unbox_s((SixModelObject) inArgs[i], tc);
                    }
                    else if( Ops.isint((SixModelObject) inArgs[i], tc) == 1 ) {
                        outArgs[i - offset] = Ops.unbox_i((SixModelObject) inArgs[i], tc);
                    }
                    else {
                        try {
                            outArgs[i - offset] = RuntimeSupport.unboxJava(Ops.decont((SixModelObject) inArgs[i], tc));
                        } catch (Exception e) {
                            throw ExceptionHandling.dieInternal(tc,
                                "Couldn't parse arguments in Java call. (Did you pass a type object?)");
                        }
                    }
                }
                else {
                    if( Ops.istrue((SixModelObject) inArgs[i], tc) == 1 ) {
                        outArgs[i - offset] = true;
                    }
                    else if( Ops.isfalse((SixModelObject) inArgs[i], tc) == 1 ) {
                        outArgs[i - offset] = false;
                    }
                }
            }
            return outArgs;
        }

        int findHandle(Object[] parsedArgs, ThreadContext tc) throws Throwable {
            int handlePos = -1;
            Type[] argTypes = null;
            OUTER: for( int i = 0; i < handleList.length; ++i ) {
                if(forCtors) {
                    argTypes = Type.getArgumentTypes(Type.getConstructorDescriptor((Constructor<?>) handleList[i]));
                }
                else {
                    argTypes = Type.getArgumentTypes(((MethodHandle)handleList[i]).type().toMethodDescriptorString());
                }
                if(argTypes.length != parsedArgs.length) {
                    continue OUTER;
                }
                INNER: for( int j = 0; j < parsedArgs.length; ++j ) {
                    switch (argTypes[j].getSort()) {
                        case Type.BOOLEAN:
                            if( parsedArgs[j].getClass().equals(long.class) ) {
                                parsedArgs[j] = parsedArgs[j] != null ? ((Long) parsedArgs[j]) == 0 ? false : true : null;
                                continue INNER;
                            }
                            else if( parsedArgs[j].getClass().equals(boolean.class) ) {
                                continue INNER;
                            }
                            break;
                        case Type.BYTE:
                            if( parsedArgs[j].getClass().equals(long.class) ) {
                                parsedArgs[j] = parsedArgs[j] != null ? ((Long)parsedArgs[j]).byteValue() : null;
                                continue INNER;
                            }
                            break;
                        case Type.SHORT:
                            if( parsedArgs[j].getClass().equals(long.class) ) {
                                parsedArgs[j] = parsedArgs[j] != null ? ((Long)parsedArgs[j]).shortValue() : null;
                                continue INNER;
                            }
                            break;
                        case Type.INT:
                            if( parsedArgs[j].getClass().equals(long.class) ) {
                                parsedArgs[j] = parsedArgs[j] != null ? ((Long)parsedArgs[j]).intValue() : null;
                                continue INNER;
                            }
                            break;
                        case Type.LONG:
                            if( parsedArgs[j].getClass().equals(long.class) ) {
                                continue INNER; 
                            }
                            break;
                        case Type.CHAR:
                            if( parsedArgs[j].getClass().equals(String.class) ) {
                                continue INNER;
                            }
                            break;
                        case Type.FLOAT:
                            if( parsedArgs[j].getClass().equals(double.class) ) {
                                parsedArgs[j] = parsedArgs[j] != null ? ((Double)parsedArgs[j]).floatValue() : null;
                                continue INNER;
                            }
                            break;
                        case Type.DOUBLE:
                            if( parsedArgs[j].getClass().equals(double.class) ) {
                                continue INNER;
                            }
                            break;
                        case Type.OBJECT:
                            Class<?> argType = Class.forName(argTypes[j].getInternalName().replace('/', '.'), 
                                false, tc.gc.byteClassLoader);
                            if( argType.isAssignableFrom(parsedArgs[j].getClass()) ) {
                                // we can coerce
                                continue INNER;
                            }
                            break;
                        default:
                            // if we didn't continue INNER we failed to match types
                    }
                    continue OUTER;
                }
                handlePos = i;
                break;
            }
            if( handlePos == -1 ) {
                String types = "void";
                boolean first = true;
                if( argTypes != null ) {
                    for( Type type : argTypes ) {
                        if( first ) {
                            types = type.toString();
                            first = false;
                        }
                        else {
                            types += ", " + type;
                        }
                    }
                }
                throw ExceptionHandling.dieInternal(tc,
                    "Couldn't find a " + (forCtors ? "constructor" : "method" ) + " with types " + types +".");
            }
            else {
                return handlePos;
            }
        }

        Object fallback(Object intc, Object incf, Object incsd, Object[] args) throws Throwable {
            ThreadContext tc = (ThreadContext) intc;
            CallFrame cf = (CallFrame) incf;
            CallSiteDescriptor csd = (CallSiteDescriptor) incsd;
            Object[] parsedArgs = parseArgs(args, tc);

            if(forCtors) {
                this.handleList = Class.forName(Type.getObjectType(((String) declaringClass).replace('/', '.')).getInternalName(),
                    false, tc.gc.byteClassLoader).getConstructors();
            }

            int handlePos = findHandle(parsedArgs, tc);

            MethodHandle rfh;
            try {
                rfh = MethodHandles.lookup().findStatic(RakudoJavaInterop.class, "filterReturnValueMethod", 
                    MethodType.fromMethodDescriptorString(
                        "(Ljava/lang/Object;Lorg/perl6/nqp/runtime/ThreadContext;)Ljava/lang/Object;", 
                        tc.gc.byteClassLoader));
            } catch (NoSuchMethodException|IllegalAccessException nsme) {
                throw ExceptionHandling.dieInternal(tc,
                    "Couldn't find the method for filtering return values from Java.");
            }

            Object out;
            if(forCtors) {
                Object instance = ((Constructor) handleList[handlePos]).newInstance(parsedArgs);
                out = rfh.invokeExact(instance, tc);
            }
            else {
                Object retVal = ((MethodHandle) handleList[handlePos]).invokeWithArguments(parsedArgs);
                out = rfh.invokeExact(retVal, tc);
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
        else if(what == int.class || what == short.class || what == byte.class || what == boolean.class) {
            out = new Long((int) in);
        }
        else if (what == long.class || what == double.class || what == String.class || what == SixModelObject.class) {
            out = in;
        }
        else if (what == float.class) {
            out = new Double((double) in);
        }
        else if (what == char.class) {
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
                out = RakOps.p6list((SixModelObject) out, gcx.List, gcx.Mu, tc);
            }
            else {
                out = RuntimeSupport.boxJava(in, gcx.rakudoInterop.getSTableForClass(what));
            }
        }

        if (what == String.class || what == char.class)
            Ops.return_s((String) out, tc.curFrame);
        else if (what == float.class || what == double.class)
            Ops.return_n(((Double)out).doubleValue(), tc.curFrame);
        else if (what != void.class && what.isPrimitive())
            Ops.return_i(((Long)out).longValue(), tc.curFrame);
        else
            Ops.return_o((SixModelObject) out, tc.curFrame);

        // the conditional is rather sketchy, but seems to be needed to 
        // correctly return a new instance when we're called from 
        // ConstructorDispatchCallSite, probably because of
        // Perl 6' .new creating a new CallFrame or something..?
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
        AnnotationVisitor av = mv.visitAnnotation("Lorg/perl6/nqp/runtime/CodeRefAnnotation;", true);
        av.visit("name", "callout "+cc.target.getName()+" "+desc);
        av.visitEnd();
        mv.visitCode();
        cc.descriptors.add(desc);

        mc.argsLoc = 4;
        mc.csdLoc = 3;
        mc.cfLoc = 5;
        mc.tcLoc = 1;

        mv.visitTypeInsn(Opcodes.NEW, "org/perl6/nqp/runtime/CallFrame");
        mv.visitInsn(Opcodes.DUP);
        mv.visitVarInsn(Opcodes.ALOAD, 1); // tc
        mv.visitVarInsn(Opcodes.ALOAD, 2); // cr
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "org/perl6/nqp/runtime/CallFrame", "<init>", Type.getMethodDescriptor(Type.VOID_TYPE, TYPE_TC, TYPE_CR));
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
        Handle disphandle = new Handle(Opcodes.H_INVOKESTATIC, "org/perl6/rakudo/RakudoJavaInterop", "multiBootstrap", 
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)" +
                "Ljava/lang/invoke/CallSite;");
        Handle[] candhandles = new Handle[mlist.size()];
        for(int i = 0; i < mlist.size(); ++i) {
            candhandles[i] = new Handle(Modifier.
                    isStatic(mlist.get(i).getModifiers()) ? Opcodes.H_INVOKESTATIC : Opcodes.H_INVOKEVIRTUAL,
                    mlist.get(i).getDeclaringClass().getName().replace('.', '/'), 
                    mlist.get(i).getName(), 
                    Type.getMethodDescriptor(mlist.get(i)));
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

        Handle disphandle = new Handle(Opcodes.H_INVOKESTATIC, "org/perl6/rakudo/RakudoJavaInterop", "constructorBootstrap",
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
        String className = "org/perl6/nqp/generatedadaptor/"+target.getName().replace('.','/');
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
        for (Field f : target.getFields()) createAdaptorField(cc, f);
        for (Constructor<?> c : target.getConstructors()) createAdaptorConstructor(cc, c);
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
        // debug
        // try {
        //     java.nio.file.Files.write(new java.io.File(className.replace('/','_') + ".class").toPath(), cc.cv.toByteArray());
        // } catch (java.io.IOException e) {
        //     e.printStackTrace();
        // }

        return cc;
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

        HashMap<String, SixModelObject> names = new HashMap< >();
        HashMap<String, ArrayList<SixModelObject>> multis = new HashMap< >();

        STable protoSt = gc.BOOTJava.st;
        SixModelObject freshType = protoSt.REPR.type_object_for(tc, computeHOW(tc, klass.getName()));

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

        for (Iterator<Map.Entry<String, SixModelObject>> it = names.entrySet().iterator(); it.hasNext(); ) {
            Map.Entry<String, SixModelObject> ent = it.next();
            if (ent.getValue() != null) {
                hash.bind_key_boxed(tc, ent.getKey(), ent.getValue());
            }
            else
                it.remove();
        }

        freshType.st.MethodCache = names;
        freshType.st.ModeFlags |= STable.METHOD_CACHE_AUTHORITATIVE;

        hash.bind_key_boxed(tc, "/TYPE/", freshType);

        return hash;
    }

}
