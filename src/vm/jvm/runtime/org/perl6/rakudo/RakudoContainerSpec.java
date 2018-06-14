package org.perl6.rakudo;

import java.lang.reflect.Field;
import sun.misc.Unsafe;

import org.perl6.nqp.runtime.*;
import org.perl6.nqp.sixmodel.*;

public class RakudoContainerSpec extends ContainerSpec {
    /* Container related hints. */
    public static final int HINT_descriptor = 0;
    public static final int HINT_value = 1;
    public static final int HINT_whence = 2;

    /* Callsite descriptor for WHENCEs. */
    private static final CallSiteDescriptor WHENCE = new CallSiteDescriptor(
        new byte[] { }, null);

    /* Fetches a value out of a container. Used for decontainerization. */
    public SixModelObject fetch(ThreadContext tc, SixModelObject cont) {
        return cont.get_attribute_boxed(tc, RakOps.key.getGC(tc).Scalar, "$!value", HINT_value);
    }
    public long fetch_i(ThreadContext tc, SixModelObject cont) {
        return fetch(tc, cont).get_int(tc);
    }
    public double fetch_n(ThreadContext tc, SixModelObject cont) {
        return fetch(tc, cont).get_num(tc);
    }
    public String fetch_s(ThreadContext tc, SixModelObject cont) {
        return fetch(tc, cont).get_str(tc);
    }

    /* Stores a value in a container. Used for assignment. */
    private static final CallSiteDescriptor storeThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_STR, CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private void checkStore(ThreadContext tc, SixModelObject cont, SixModelObject value) {
        RakOps.GlobalExt gcx = RakOps.key.getGC(tc);

        SixModelObject desc = cont.get_attribute_boxed(tc, gcx.Scalar,
            "$!descriptor", HINT_descriptor);
        if (desc == null)
            throw ExceptionHandling.dieInternal(tc,
                "Cannot assign to a readonly variable or a value");

        if (value.st.WHAT == gcx.Nil) {
            value = desc.get_attribute_boxed(tc,
                gcx.ContainerDescriptor, "$!default", RakOps.HINT_CD_DEFAULT);
        }
        SixModelObject of = desc.get_attribute_boxed(tc,
            gcx.ContainerDescriptor, "$!of", RakOps.HINT_CD_OF);
        long ok = of == gcx.Mu ? 1 : Ops.istype(value, of, tc);
        if (ok == 0) {
            desc.get_attribute_native(tc, gcx.ContainerDescriptor, "$!name", RakOps.HINT_CD_NAME);
            String name = tc.native_s;
            SixModelObject thrower = RakOps.getThrower(tc, "X::TypeCheck::Assignment");
            if (thrower == null)
                throw ExceptionHandling.dieInternal(tc,
                    "Type check failed in assignment to '" + name + "'");
            else
                Ops.invokeDirect(tc, thrower,
                    storeThrower, new Object[] { name, value, of });
        }
    }
    public void store(ThreadContext tc, SixModelObject cont, SixModelObject value) {
        checkStore(tc, cont, value);
        RakOps.GlobalExt gcx = RakOps.key.getGC(tc);
        SixModelObject whence = cont.get_attribute_boxed(tc, gcx.Scalar, "$!whence", HINT_whence);
        if (whence != null)
            Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });
        if (value.st.WHAT == gcx.Nil) {
            SixModelObject desc = cont.get_attribute_boxed(tc, gcx.Scalar,
                "$!descriptor", HINT_descriptor);
            value = desc.get_attribute_boxed(tc,
                gcx.ContainerDescriptor, "$!default", RakOps.HINT_CD_DEFAULT);
        }
        cont.bind_attribute_boxed(tc, gcx.Scalar, "$!value", HINT_value, value);
    }
    public void store_i(ThreadContext tc, SixModelObject cont, long value) {
        store(tc, cont, RakOps.p6box_i(value, tc));
    }
    public void store_n(ThreadContext tc, SixModelObject cont, double value) {
        store(tc, cont, RakOps.p6box_n(value, tc));
    }
    public void store_s(ThreadContext tc, SixModelObject cont, String value) {
        store(tc, cont, RakOps.p6box_s(value, tc));
    }

    /* Stores a value in a container, without any checking of it (this
     * assumes an optimizer or something else already did it). Used for
     * assignment. */
    public void storeUnchecked(ThreadContext tc, SixModelObject cont, SixModelObject obj) {
        SixModelObject Scalar = RakOps.key.getGC(tc).Scalar;
        SixModelObject whence = cont.get_attribute_boxed(tc, Scalar, "$!whence", HINT_whence);
        if (whence != null)
            Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });

        cont.bind_attribute_boxed(tc, Scalar, "$!value", HINT_value, obj);
    }

    /* Not all containers are rw (ContainerSpec.canStore() defaults to true). */
    public boolean canStore(ThreadContext tc, SixModelObject cont) {
        if (!(cont instanceof TypeObject)) {
            SixModelObject desc = cont.get_attribute_boxed(tc, cont.st.WHAT,
                "$!descriptor", HINT_descriptor);
            return desc != null;
        }
        return false;
    }

    /* Name of this container specification. */
    public String name() {
        return "rakudo_scalar";
    }

    /* Serializes the container data, if any. */
    public void serialize(ThreadContext tc, STable st, SerializationWriter writer) {
        /* No data to serialize. */
    }

    /* Deserializes the container data, if any. */
    public void deserialize(ThreadContext tc, STable st, SerializationReader reader) {
        /* No data to deserialize. */
    }

    /* Atomic operations. */

    private Unsafe unsafe;
    private long scalarValueOffset;

    @SuppressWarnings("restriction")
    private void ensureAtomicsReady(SixModelObject cont) {
        if (unsafe == null) {
            try {
                Field unsafeField = Unsafe.class.getDeclaredField("theUnsafe");
                unsafeField.setAccessible(true);
                unsafe = (Unsafe)unsafeField.get(null);
                scalarValueOffset = unsafe.objectFieldOffset(
                    cont.getClass().getDeclaredField("field_1"));
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    public SixModelObject cas(ThreadContext tc, SixModelObject cont,
                              SixModelObject expected, SixModelObject value) {
        ensureAtomicsReady(cont);
        checkStore(tc, cont, value);
        return unsafe.compareAndSwapObject(cont, scalarValueOffset, expected, value)
            ? expected
            : (SixModelObject)unsafe.getObjectVolatile(cont, scalarValueOffset);
    }

    public SixModelObject atomic_load(ThreadContext tc, SixModelObject cont) {
        ensureAtomicsReady(cont);
        return (SixModelObject)unsafe.getObjectVolatile(cont, scalarValueOffset);
    }

    public void atomic_store(ThreadContext tc, SixModelObject cont,
                             SixModelObject value) {
        ensureAtomicsReady(cont);
        checkStore(tc, cont, value);
        unsafe.putObjectVolatile(cont, scalarValueOffset, cont);
    }
}
