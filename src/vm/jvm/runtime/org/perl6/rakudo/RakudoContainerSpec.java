package org.perl6.rakudo;

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
        return cont.get_attribute_boxed(tc, Ops.Scalar, "$!value", HINT_value);
    }
    
    /* Stores a value in a container. Used for assignment. */
    public void store(ThreadContext tc, SixModelObject cont, SixModelObject obj) {
        if (Ops.DEBUG_MODE)
            System.err.println("scalar store typecheck / rwcheck NYI");
        
        SixModelObject whence = cont.get_attribute_boxed(tc, Ops.Scalar, "$!whence", HINT_whence);
        if (whence != null)
            org.perl6.nqp.runtime.Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });
        
        cont.bind_attribute_boxed(tc, Ops.Scalar, "$!value", HINT_value, obj);
    }
    
    /* Stores a value in a container, without any checking of it (this
     * assumes an optimizer or something else already did it). Used for
     * assignment. */
    public void storeUnchecked(ThreadContext tc, SixModelObject cont, SixModelObject obj) {
        SixModelObject whence = cont.get_attribute_boxed(tc, Ops.Scalar, "$!whence", HINT_whence);
        if (whence != null)
            org.perl6.nqp.runtime.Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });
        
        cont.bind_attribute_boxed(tc, Ops.Scalar, "$!value", HINT_value, obj);
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
}
