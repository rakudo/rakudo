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
            # utf8 elems aren't int-y and not marshalled smart enough (yet?) either
            # to clarify, the coercion should probably be not neccessary 
            # and should be considered a bug
            $crc32.update($_.Int);
        }
        is $crc32.getValue, 1072431491, "(I)V candidate for CRC32 is recognized correctly";
    }
    
    {
        my $crc32 = CRC32.new;
        for 'Hello, Java'.encode('utf-8') {
            # because utf8 lists automatically know they're something int-y
            $crc32.update($_);
        }
        is $crc32.getValue, 1072431491, "([B)V candidate for CRC32 is recognized correctly";
    }
}


done;
