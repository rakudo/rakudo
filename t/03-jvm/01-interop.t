use v6;
use Test;

{
    # still supported
    use java::lang::String:from<java>;

    ok 1, "alive after 'use java::lang::String:from<java>' (not deprecated yet)";
}

{
    use java::lang::String:from<Java>;

    ok 1, "alive after 'use java::lang::String:from<Java>'";

    is String."method/valueOf/(Z)Ljava/lang/String;"(True), "true", 
        "calling explicit static methods works";

    is String.valueOf(True), "true", "calling multi static methods works";

    my $jstr = String."constructor/new/(Ljava/lang/String;)V"("foo");
    is $jstr, "foo", "instantiation with explicit constructors works";

    $jstr = String.new("bar");
    is $jstr, "bar", "multi constructor works";
}

{
    use java::util::zip::CRC32:from<Java>;
    # check two of the .update candidates for CRC32
    {
        my $crc32 = CRC32.new;
        for 'Hello, Java'.encode('utf-8').list {
            $crc32.update($_);
        }
        is $crc32.getValue, 1072431491, "(I)V candidate for CRC32 is recognized correctly";
    }
    
    {
        my $crc32 = CRC32.new;
        for 'Hello, Java'.encode('utf-8') {
            $crc32.update($_);
        }
        is $crc32.getValue, 1072431491, "([B)V candidate for CRC32 is recognized correctly";
    }
}

{
    use java::lang::Long:from<Java>;
    {
        my $long = Long.new("42");
        is $long, 42, 'multi constructor and marshalling for Long works (1)';
        my $otherlong = Long.new(42);
        is $otherlong, 42, 'multi constructor and marshalling for Long works (2)';
    }
}

{
    use java::lang::Integer:from<Java>;
    {
        my $int = Integer.new("42");
        is $int, 42, 'multi constructor and marshalling for Integer works (1)';
        my $otherint = Integer.new(42);
        is $otherint, 42, 'multi constructor and marshalling for Integer works (2)';
    }
}

{
    use java::lang::Short:from<Java>;
    {
        my $short = Short.new("42");
        is $short, 42, 'multi constructor and marshalling for Short works (1)';
        my $othershort = Short.new(42);
        is $othershort, 42, 'multi constructor and marshalling for Short works (2)';
    }
}

{
    use java::lang::Byte:from<Java>;
    {
        my $Byte = Byte.new("42");
        is $Byte, 42, 'multi constructor and marshalling for Byte works (1)';
        my $otherByte = Byte.new(42);
        is $otherByte, 42, 'multi constructor and marshalling for Byte works (2)';
    }
}

{
    use java::lang::Float:from<Java>;
    {
        my $Float = Float.new("42.0");
        is $Float, 42.0, 'multi constructor and marshalling for Float works (1)';
        my $otherFloat = Float.new(42e0);
        is $otherFloat, 42e0, 'multi constructor and marshalling for Float works (1)';
    }
}

{
    use java::lang::Double:from<Java>;
    {
        my $Double = Double.new("42.0");
        is $Double, 42.0, 'multi constructor and marshalling for Double works (1)';
        my $otherDouble = Double.new(42e0);
        is $otherDouble, 42e0, 'multi constructor and marshalling for Double works (2)';
    }
}

{
    use java::lang::Boolean:from<Java>;
    {
        my $Boolean = Boolean.new("true"); # lower case t, because Java does the converting
        is $Boolean, True, 'multi constructor and marshalling for Boolean works (1)';
        my $otherBoolean = Boolean.new(True); # upper case T, because it's our own Bool::True
        is $otherBoolean, True, 'multi constructor and marshalling for Boolean works (2)';
    }
}

{
    use java::util::zip::CRC32:from<Java>;
    {
        CRC32.HOW.add_method(CRC32, "doubledValue", method ($self:) {
            return $self.getValue() * 2;
        });
        my $crc32 = CRC32.new;
        for 'Hello, Java'.encode('utf-8') {
            $crc32.update($_);
        }
        is $crc32.doubledValue(), 2144862982, "adding methods to a Java type object works";
    }
}

{
    use java::lang::StringBuilder:from<Java>;
    {
        my $sb = StringBuilder.new();
        $sb = $sb.append("foo"); # dies
        is $sb.toString(), "foo", 'calling through to a less visible parent method works';
    }
}

{
    use java::lang::Boolean:from<Java>;
    {
        my $Boolean = Boolean.new("true"); # lower case t, because Java does the converting
        is $Boolean, True, 'multi constructor and marshalling for Boolean works (1)';
        my $otherBoolean = Boolean.new(True); # upper case T, because it's our own Bool::True
        is $otherBoolean, True, 'multi constructor and marshalling for Boolean works (2)';
    }
}

done;
