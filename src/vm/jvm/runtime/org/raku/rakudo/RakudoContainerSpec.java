package org.raku.rakudo;

import java.lang.reflect.Field;
import sun.misc.Unsafe;

import org.raku.nqp.runtime.*;
import org.raku.nqp.sixmodel.*;

public class RakudoContainerSpec extends ContainerSpec {
    /* Container related hints. */
    public static final int HINT_descriptor = 0;
    public static final int HINT_value = 1;

    /* Callsite descriptors. */
    private static final CallSiteDescriptor STORE = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    private static final CallSiteDescriptor CAS = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);

    /* Callbacks. */
    public SixModelObject store;
    public SixModelObject storeUnchecked;
    public SixModelObject cas;
    public SixModelObject atomicStore;

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
    public void store(ThreadContext tc, SixModelObject cont, SixModelObject value) {
        Ops.invokeDirect(tc, store, STORE, new Object[] { cont, value });
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
    public void storeUnchecked(ThreadContext tc, SixModelObject cont, SixModelObject value) {
        Ops.invokeDirect(tc, storeUnchecked, STORE, new Object[] { cont, value });
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
        return "value_desc_cont";
    }

    /* Serializes the container data, if any. */
    public void serialize(ThreadContext tc, STable st, SerializationWriter writer) {
        writer.writeRef(store);
        writer.writeRef(storeUnchecked);
        writer.writeRef(cas);
        writer.writeRef(atomicStore);
    }

    /* Deserializes the container data, if any. */
    public void deserialize(ThreadContext tc, STable st, SerializationReader reader) {
        store = reader.readRef();
        storeUnchecked = reader.readRef();
        cas = reader.readRef();
        atomicStore = reader.readRef();
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
        Ops.invokeDirect(tc, cas, CAS, new Object[] { cont, expected, value });
        return Ops.result_o(tc.curFrame);
    }

    public SixModelObject atomic_load(ThreadContext tc, SixModelObject cont) {
        ensureAtomicsReady(cont);
        return (SixModelObject)unsafe.getObjectVolatile(cont, scalarValueOffset);
    }

    public void atomic_store(ThreadContext tc, SixModelObject cont,
                             SixModelObject value) {
        Ops.invokeDirect(tc, atomicStore, STORE, new Object[] { cont, value });
    }
}
