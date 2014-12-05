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

        final MethodHandle fallback;

        private Object[] hlist;
        public static CallFrame scf;

        public DispatchCallSite(String methname, MethodType type, Object[] hlist) {
            super(type);
            this.methname = methname;
            this.hlist = hlist;
            this.fallback = FALLBACK.bindTo(this);
            System.out.println("ok here");
        }

        Object fallback(Object intc, Object incf, Object incsd, Object... args) throws Throwable {
            ThreadContext tc = (ThreadContext) intc;
            CallFrame cf = (CallFrame) incf;
            CallSiteDescriptor csd = (CallSiteDescriptor) incsd;
            Object[] argVals = new Object[args.length];
            for(int i = 0; i < args.length; ++i) {
                System.out.println("have HOW: " + ((SixModelObject)args[i]).st.HOW.toString());
                System.out.println("have WHAT: " + ((SixModelObject)args[i]).st.WHAT.toString());
                if( Ops.isnum((SixModelObject) args[i], tc) == 1 ) {
                    argVals[i] = Ops.unbox_n((SixModelObject) args[i], tc);
                }
                else if( Ops.isstr((SixModelObject) args[i], tc) == 1 ) {
                    argVals[i] = Ops.unbox_s((SixModelObject) args[i], tc);
                }
                else if( Ops.isint((SixModelObject) args[i], tc) == 1 ) {
                    argVals[i] = Ops.unbox_i((SixModelObject) args[i], tc);
                }
                else {
                    argVals[i] = RuntimeSupport.unboxJava(Ops.decont((SixModelObject) args[i], tc));
                }
                System.out.println("after smo2jo: " + argVals[i].getClass());
            }

            int handlePos = -1;
            OUTER: for( int i = 0; i < hlist.length; ++i ) {
                Type[] argTypes = Type.getArgumentTypes(((MethodHandle)hlist[i]).type().toMethodDescriptorString());
                if(argTypes.length != argVals.length) {
                    System.out.println("skipping mh " + i);
                    continue OUTER;
                }
                    INNER: for( int j = 1; j < argTypes.length; ++j ) {
                    if( !argTypes[j].equals(Type.getType(argVals[j].getClass())) ) {
                        switch (argTypes[j].getSort()) {
                            case Type.BOOLEAN:
                                if( argVals[j].getClass().equals(Long.class) ) {
                                    argVals[j] = argVals[j] != null ? argVals[j] == 0 ? false : true : null;
                                    continue INNER;
                                }
                            case Type.BYTE:
                                if( argVals[j].getClass().equals(Long.class) ) {
                                    argVals[j] = argVals[j] != null ? ((Long)argVals[j]).byteValue() : null;
                                    continue INNER;
                                }
                            case Type.SHORT:
                                if( argVals[j].getClass().equals(Long.class) ) {
                                    argVals[j] = argVals[j] != null ? ((Long)argVals[j]).shortValue() : null;
                                    continue INNER;
                                }
                            case Type.INT:
                                if( argVals[j].getClass().equals(Long.class) ) {
                                    argVals[j] = argVals[j] != null ? ((Long)argVals[j]).intValue() : null;
                                    continue INNER;
                                }
                            case Type.LONG:
                                if( argVals[j].getClass().equals(Long.class) )
                                    continue INNER; 
                            case Type.CHAR:
                                if( argVals[j].getClass().equals(String.class) )
                                    continue INNER;
                            case Type.FLOAT:
                                if( argVals[j].getClass().equals(Double.class) ) {
                                    argVals[j] = argVals[j] != null ? ((Double)argVals[j]).floatValue() : null;
                                    continue INNER;
                                }
                            case Type.DOUBLE:
                                if( argVals[j].getClass().equals(Double.class) )
                                    continue INNER;
                            default:
                        } // if we didn't continue INNER we failed to match types
                        System.out.println("not equal: " + argTypes[j].toString() + " and " + Type.getType(argVals[j].getClass()));
                        continue OUTER;
                    }
                }
                handlePos = i;
                break;
            }
            if( handlePos == -1 ) {
                throw ExceptionHandling.dieInternal(tc,
                    "Couldn't find a handle for a method with the supplied argument types.");
            }

            System.out.println("fine here");
            MethodType objRetType = ((MethodHandle) hlist[handlePos]).type().changeReturnType(Object.class);
            MethodHandle resHandle = ((MethodHandle) hlist[handlePos]).asType(objRetType);

            MethodHandle rfh;
            try {
               rfh = MethodHandles.lookup().findVirtual(RakudoJavaInterop.class, "filterReturnValueMethod", 
                    MethodType.fromMethodDescriptorString(
                        "(Ljava/lang/Object;)Lorg/perl6/nqp/sixmodel/SixModelObject;", 
                        tc.gc.byteClassLoader));
               rfh = MethodHandles.insertArguments(rfh, 0, RakOps.key.getGC(tc).rakudoInterop);
            } catch (NoSuchMethodException|IllegalAccessException nsme) {
                throw ExceptionHandling.dieInternal(tc,
                    "Couldn't find the method for filtering return values from Java.");
            }
            System.out.println("still fine here");

            MethodHandle rethandle = MethodHandles.filterReturnValue(resHandle, (MethodHandle) rfh);
            return ((MethodHandle)rethandle).invokeWithArguments(argVals);
        }

        private static final MethodHandle FALLBACK;

        static {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            try {
                FALLBACK = lookup.findVirtual(DispatchCallSite.class,
                        "fallback", MethodType.genericMethodType(3, true));
            } catch (ReflectiveOperationException e) {
                System.out.println("dying here");
                throw new LinkageError(e.getMessage(), e);
            }
        }
    }

    public RakudoJavaInterop(GlobalContext gc) {
        super(gc);
    }

    public SixModelObject filterReturnValueMethod(Object in) {
        Class<?> what = in.getClass();
        System.out.println("we have a " + what.toString() + " here.");
        Object out = null;
        if(what == void.class) {
            System.out.println("void case");
            out = null;
        }
        else if(what == int.class || what == short.class || what == byte.class || what == boolean.class) {
            System.out.println("int short etc case");
            out = new Long((int) in);
        }
        else if (what == long.class || what == double.class || what == String.class || what == SixModelObject.class) {
            System.out.println("long double String case");
            out = in;
        }
        else if (what == float.class) {
            System.out.println("float case");
            out = new Double((double) in);
        }
        else if (what == char.class) {
            System.out.println("char case");
            out = String.valueOf((char) in);
        }
        else {
            ThreadContext tc = currentTC();
            GlobalExt gcx = RakOps.key.getGC(tc);
            STable stable = null;
            if(gcx.rakudoInterop.commonSTable != null) {
                stable = gcx.rakudoInterop.commonSTable;
            }
            if (what.isArray() && what.getComponentType().isPrimitive()) {
                SixModelObject BOOTIntArray = Ops.bootintarray(tc);
                out = BOOTIntArray.st.REPR.allocate(tc, BOOTIntArray.st); 
                if(stable == null) {
                    stable = BOOTIntArray.st;
                }
                if(what.getComponentType().isPrimitive()) {
                    if(what.getComponentType() == int.class 
                            || what.getComponentType() == short.class 
                            || what.getComponentType() == byte.class 
                            || what.getComponentType() == boolean.class) {
                        for(int i = 0; i < ((int[])in).length; ++i) {
                            Ops.bindpos_i((SixModelObject) out, i, ((int[])in)[i], tc);
                        }
                        out = Ops.hllizefor((SixModelObject) out, "perl6", tc);
                    }
                    else if (what.getComponentType() == long.class 
                            || what.getComponentType() == double.class 
                            || what.getComponentType() == String.class 
                            || what.getComponentType() == SixModelObject.class) {
                        System.out.println("long double String case");
                        out = in;
                    }
                    else if (what.getComponentType() == float.class) {
                        System.out.println("float case");
                        out = new Double((double) in);
                    }
                    else if (what.getComponentType() == char.class) {
                        System.out.println("char case");
                        out = String.valueOf((char) in);
                    }
                }
                else {
                    System.out.println("we have a reference type array here, of type " + what.getComponentType().toString());
                }
            }
            else {
                out = RuntimeSupport.boxJava(out, stable);
            }
        }

        if (what == String.class || what == char.class)
            Ops.return_s((String) out, currentTC().curFrame);
        else if (what == float.class || what == double.class)
            Ops.return_n(((Double)out).doubleValue(), currentTC().curFrame);
        else if (what != void.class && what.isPrimitive())
            Ops.return_i(((Long)out).longValue(), currentTC().curFrame);
        else
            Ops.return_o((SixModelObject) out, currentTC().curFrame);

 
        return Ops.result_o(currentTC().curFrame);
    }

    public static CallSite multiBootstrap(MethodHandles.Lookup lookup, String name, MethodType type, Object... hlist) {
        System.out.println("have this: " + type.toString());
        DispatchCallSite cs = new DispatchCallSite(name, type, hlist);
        cs.setTarget(cs.fallback);
        return cs;
    }

    protected void createAdaptorMultiDispatch(ClassContext cc, ArrayList<Method> mlist) {
        // jnthn psch: The trick for multi-dispatch is to use the bootstrap method to install a call to something that will do the dispatch.
        //       [...]
        //       [the good] way is to code-gen something that uses invokedynamic
        //       And put the real interesting logic inside there
        //       Then we can start caching it and probably be way faster.
        //
        // so, what we want to do here is something like the following
        //  generate a new method which takes the arguments from p6land and knows all the multi candidates with the same shortname 
        //  this method: 
        //      checks argumenttypes from p6land against argumenttypes for the different multis
        //      dispatch to the one with matching argumenttypes
        //  return that method
        //
        // maybe we should dynamically call something that handles the dispatch statically..?
        //
        //  what do we need for that? 
        //      we have the Method as it exists in jvm-land, which means we can figure out the argtypes
        //      we get the args as they come from p6land, which means we can compare types
        //        is the one directly above actually true? we get an Object[] that contains SixModelObjects i think?
        //
        // jnthn psch: If just having the args as an array is helpful, I think there's an asVarArgsCollector or somehting in the 
        //             MethodHandles combinator set
        //
        // [!] actually, just read the asm example properly until you understand it...

        String name = "mmd+" + mlist.get(0).getName();
        String desc = "method/" + name + "/([java/lang/Object;)Ljava/lang/Object;";
        int arity = 1;
        // from here copied from startCallout
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
        emitInteger(mc, arity);
        // ... because we need -1 for the desired arity
        emitInteger(mc, -1);
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, TYPE_OPS.getInternalName(), "checkarity", Type.getMethodDescriptor(TYPE_CSD, TYPE_CF, TYPE_CSD, TYPE_AOBJ, Type.INT_TYPE, Type.INT_TYPE));
        mv.visitVarInsn(Opcodes.ASTORE, 3); // csd
        mv.visitVarInsn(Opcodes.ALOAD, 1); // tc
        mv.visitFieldInsn(Opcodes.GETFIELD, TYPE_TC.getInternalName(), "flatArgs", TYPE_AOBJ.getDescriptor());
        mv.visitVarInsn(Opcodes.ASTORE, 4); // args
        // end copy from startCallout

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

        mv.visitVarInsn(Opcodes.ALOAD, 1);
        mv.visitVarInsn(Opcodes.ALOAD, 5);
        mv.visitVarInsn(Opcodes.ALOAD, 3);
        mv.visitVarInsn(Opcodes.ALOAD, 4);
        mv.visitInvokeDynamicInsn(mlist.get(0).getName(), 
                "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;",
                disphandle, (Object[]) candhandles);

        // this is endCallout(), might not be neccessary to copy
        Label endTry = new Label();
        Label handler = new Label();
        Label notcontrol = new Label();

        mv.visitLabel(endTry);
        mv.visitVarInsn(Opcodes.ALOAD, 5); //cf
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, TYPE_CF.getInternalName(), "leave", "()V");
        mv.visitInsn(Opcodes.RETURN);

        mv.visitLabel(handler);
        mv.visitInsn(Opcodes.DUP);
        mv.visitTypeInsn(Opcodes.INSTANCEOF, "org/perl6/nqp/runtime/ControlException");
        mv.visitJumpInsn(Opcodes.IFEQ, notcontrol);
        mv.visitVarInsn(Opcodes.ALOAD, 5); //cf
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, TYPE_CF.getInternalName(), "leave", "()V");
        mv.visitInsn(Opcodes.ATHROW);

        mv.visitLabel(notcontrol);
        mv.visitVarInsn(Opcodes.ALOAD, 1); // tc
        mv.visitInsn(Opcodes.SWAP);
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "org/perl6/nqp/runtime/ExceptionHandling", "dieInternal",
                Type.getMethodDescriptor(Type.getType(RuntimeException.class), TYPE_TC, Type.getType(Throwable.class)));
        mv.visitInsn(Opcodes.ATHROW);

        mc.mv.visitTryCatchBlock(mc.tryStart, endTry, handler, null);
        mc.mv.visitMaxs(0,0);
        mc.mv.visitEnd();
    }

    @Override
    protected SixModelObject computeHOW(ThreadContext tc, String name) {
        RakOps.GlobalExt gcx = RakOps.key.getGC(tc);
        SixModelObject mo = gcx.JavaHOW.st.REPR.allocate(tc, gcx.JavaHOW.st);
        mo.bind_attribute_boxed(tc, gcx.JavaHOW, "$!name", STable.NO_HINT,
            RakOps.p6box_s(name, tc));

        return mo;
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
        createAdaptorSpecials(cc);
        compunitMethods(cc);

        finishClass(cc);
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
            if (ent.getValue() != null)
                hash.bind_key_boxed(tc, ent.getKey(), ent.getValue());
            else
                it.remove();
        }

        freshType.st.MethodCache = names;
        freshType.st.ModeFlags |= STable.METHOD_CACHE_AUTHORITATIVE;

        hash.bind_key_boxed(tc, "/TYPE/", freshType);

        return hash;
    }
}
