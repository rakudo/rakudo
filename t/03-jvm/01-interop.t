use v6;
use Test;

BEGIN unless $*VM ~~ "jvm" { plan 0; skip-rest "jvm only test"; done-testing; exit 0; };

plan 30;

{
    use java::lang::String:from<JavaRuntime>;

    ok 1, "alive after 'use java::lang::String:from<JavaRuntime>'";

    is String."method/valueOf/(Z)Ljava/lang/String;"(True), "true", 
        "calling explicit static methods works";

    is String.valueOf(True), "true", "calling multi static methods works";

    my $jstr = String."constructor/new/(Ljava/lang/String;)V"("foo");
    is $jstr, "foo", "instantiation with explicit constructors works";

    $jstr = String.new("bar");
    is $jstr, "bar", "multi constructor works";
}

{
    use java::util::zip::CRC32:from<JavaRuntime>;
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
        $crc32.update('Hello, Java'.encode('utf-8'));
        is $crc32.getValue, 1072431491, "([B)V candidate for CRC32 is recognized correctly";
    }

    {
        my $crc32 = CRC32.new;
        $crc32."method/update/([B)V"('Hello, Java'.encode('utf-8'));
        is $crc32.getValue, 1072431491, "([B)V candidate for CRC32 works when explicitly specified";
    }
}

{
    use java::lang::Long:from<JavaRuntime>;
    {
        my $long = Long.new("42");
        is $long, 42, 'multi constructor and marshalling for Long works (1)';
        my $otherlong = Long.new(42);
        is $otherlong, 42, 'multi constructor and marshalling for Long works (2)';
    }
}

{
    use java::lang::Integer:from<JavaRuntime>;
    {
        my $int = Integer.new("42");
        is $int, 42, 'multi constructor and marshalling for Integer works (1)';
        my $otherint = Integer.new(42);
        is $otherint, 42, 'multi constructor and marshalling for Integer works (2)';
    }
}

{
    use java::lang::Short:from<JavaRuntime>;
    {
        my $short = Short.new("42");
        is $short, 42, 'multi constructor and marshalling for Short works (1)';
        my $othershort = Short.new(42);
        is $othershort, 42, 'multi constructor and marshalling for Short works (2)';
    }
}

{
    use java::lang::Byte:from<JavaRuntime>;
    {
        my $Byte = Byte.new("42");
        is $Byte, 42, 'multi constructor and marshalling for Byte works (1)';
        my $otherByte = Byte.new(42);
        is $otherByte, 42, 'multi constructor and marshalling for Byte works (2)';
    }
}

{
    use java::lang::Float:from<JavaRuntime>;
    {
        my $Float = Float.new("42.0");
        is $Float, 42.0, 'multi constructor and marshalling for Float works (1)';
        my $otherFloat = Float.new(42e0);
        is $otherFloat, 42e0, 'multi constructor and marshalling for Float works (1)';
    }
}

{
    use java::lang::Double:from<JavaRuntime>;
    {
        my $Double = Double.new("42.0");
        is $Double, 42.0, 'multi constructor and marshalling for Double works (1)';
        my $otherDouble = Double.new(42e0);
        is $otherDouble, 42e0, 'multi constructor and marshalling for Double works (2)';
    }
}

{
    use java::lang::Boolean:from<JavaRuntime>;
    {
        my $Boolean = Boolean.new("true"); # lower case t, because Java does the converting
        is $Boolean, True, 'multi constructor and marshalling for Boolean works (1)';
        my $otherBoolean = Boolean.new(True); # upper case T, because it's our own Bool::True
        is $otherBoolean, True, 'multi constructor and marshalling for Boolean works (2)';
    }
}

{
    use java::util::zip::CRC32:from<JavaRuntime>;
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
    use java::lang::StringBuilder:from<JavaRuntime>;
    {
        my $sb = StringBuilder.new();
        $sb = $sb.append("foo");
        is $sb.toString(), "foo", 'calling through to a less visible parent method works';
    }
}

{
    use java::util::zip::CRC32:from<JavaRuntime>;
    {
        my $crc32 = CRC32.new;
        throws-like { $crc32.foo() }, X::Method::NotFound;
    }
}

{
    use java::lang::String:from<JavaRuntime>;
    {
        dies-ok { String.new([1..*]) }, 'we die on marshalling lazy lists';
    }
}

{
    my $r = run('javac', 't/03-jvm/Foo.java');
    if $r && "t/03-jvm/Foo.class".IO ~~ :e {
        my $out = shell("$*EXECUTABLE -e'use lib q[java#t/03-jvm/]; use Foo:from<Java>; say Foo.bar; say Foo.new.quux;'", :out);
        is $out.out.lines, "baz womble", "(compiling and) loading a .class file via 'use lib' works";
           $out = shell("$*EXECUTABLE -e'use lib q[java#t/03-jvm/]; use Foo:from<Java>; say Foo.trizzle([1, 2e0, <bar>])'", :out);
        is $out.out.lines, "12.0bar", "passing arrays with mixed types to Object[] works";
           $out = shell("$*EXECUTABLE -e'use lib q[java#t/03-jvm/]; use Foo:from<Java>; say Foo.suzzle([1, 2e0, <bar>])'", :out);
        is $out.out.lines, "12.0bar", "passing arrays with mixed types to List<Object> works";
           $out = shell("$*EXECUTABLE -e'use lib q[java#t/03-jvm/]; use Foo:from<Java>; say Foo.foozzle(%(a => 1e0, b => 2, c => \"foo\"))'", 
                    :out);
        is $out.out.lines, "a => 1.0, b => 2, c => foo, ", "passing Hash[Str] with mixed types to Map works";
    }
    else {
        skip 2;
    }
}

# vim: expandtab shiftwidth=4
