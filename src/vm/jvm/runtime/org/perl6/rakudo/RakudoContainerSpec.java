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
        return cont.get_attribute_boxed(tc, Ops.key.getGC(tc).Scalar, "$!value", HINT_value);
    }
    
    /* Stores a value in a container. Used for assignment. */
    private static final CallSiteDescriptor storeThrower = new CallSiteDescriptor(
        new byte[] { CallSiteDescriptor.ARG_STR, CallSiteDescriptor.ARG_OBJ, CallSiteDescriptor.ARG_OBJ }, null);
    public void store(ThreadContext tc, SixModelObject cont, SixModelObject value) {
        Ops.GlobalExt gcx = Ops.key.getGC(tc);
        
        long rw = 0;
        SixModelObject desc = cont.get_attribute_boxed(tc, gcx.Scalar,
            "$!descriptor", HINT_descriptor);
        if (desc != null) {
            desc.get_attribute_native(tc, gcx.ContainerDescriptor, "$!rw", Ops.HINT_CD_RW);
            rw = tc.native_i;
        }
        if (rw == 0)
            throw ExceptionHandling.dieInternal(tc,
                "Cannot assign to a readonly variable or a value");
        
        SixModelObject of = desc.get_attribute_boxed(tc,
            gcx.ContainerDescriptor, "$!of", Ops.HINT_CD_OF);
        long ok = org.perl6.nqp.runtime.Ops.istype(value, of, tc);
        if (ok == 0) {
            if (value.st.WHAT == gcx.Nil) {
                value = desc.get_attribute_boxed(tc,
                    gcx.ContainerDescriptor, "$!default", Ops.HINT_CD_DEFAULT);
            }
            else {
                desc.get_attribute_native(tc, gcx.ContainerDescriptor, "$!name", Ops.HINT_CD_NAME);
                String name = tc.native_s;
                SixModelObject thrower = Ops.getThrower(tc, "X::TypeCheck::Assignment");
                if (thrower == null)
                    throw ExceptionHandling.dieInternal(tc,
                        "Type check failed in assignment to '" + name + "'");
                else
                    org.perl6.nqp.runtime.Ops.invokeDirect(tc, thrower,
                        storeThrower, new Object[] { name, value, of });
            }
        }
        
        SixModelObject whence = cont.get_attribute_boxed(tc, gcx.Scalar, "$!whence", HINT_whence);
        if (whence != null)
            org.perl6.nqp.runtime.Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });
        
        cont.bind_attribute_boxed(tc, gcx.Scalar, "$!value", HINT_value, value);
    }
    
    /* Stores a value in a container, without any checking of it (this
     * assumes an optimizer or something else already did it). Used for
     * assignment. */
    public void storeUnchecked(ThreadContext tc, SixModelObject cont, SixModelObject obj) {
        SixModelObject Scalar = Ops.key.getGC(tc).Scalar;
        SixModelObject whence = cont.get_attribute_boxed(tc, Scalar, "$!whence", HINT_whence);
        if (whence != null)
            org.perl6.nqp.runtime.Ops.invokeDirect(tc, whence,
                WHENCE, new Object[] { });
        
        cont.bind_attribute_boxed(tc, Scalar, "$!value", HINT_value, obj);
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
