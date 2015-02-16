use v6;

use Test;

plan 92;

sub sanity($it) {
    ok FILETEST-e($it),   "does $it exist";
    nok FILETEST-l($it),  "is $it not a symlink";
    isa_ok FILETEST-s($it), Int,   "is the file size an Int";
    isa_ok FILETEST-z($it), Bool,  "is the emptyness a Bool";
    ok FILETEST-r($it),   "can we r $it";

    isa_ok FILETEST-INODE($it), Int, 'is the inode an Int';
    isa_ok FILETEST-DEVICE($it), Int, 'is the device id an Int';

    isa_ok FILETEST-MODIFIED($it), Instant, "is modified on $it an Instant";
    isa_ok FILETEST-ACCESSED($it), Instant, "is accessed on $it an Instant";
    isa_ok FILETEST-CHANGED($it),  Instant, "is changed on $it an Instant";
}

my $CWD = $*CWD;
{
    temp $*CWD = chdir('t/01-sanity');
    my $dir = $*CWD ~ 'filetests/';
    sanity($dir);
    ok FILETEST-d($dir),   "is $dir a directory";
    nok FILETEST-f($dir),  "is $dir not a file";
    ok FILETEST-w($dir),   "can we w $dir";
    ok FILETEST-rw($dir),  "can we rw $dir";
    ok FILETEST-x($dir),   "can we x $dir";
    ok FILETEST-rx($dir),  "can we rx $dir";
    ok FILETEST-wx($dir),  "can we wx $dir";
    ok FILETEST-rwx($dir), "can we rwx $dir";

    my $CWD = $*CWD;
    {
        temp $*CWD = chdir('filetests');
        {
            my $file = $*CWD ~ 'r--';
            chmod 0o400, $file;   # need to make sure file access is correct
            sanity($file);
            ok FILETEST-f($file),    "is $file a file";
            nok FILETEST-d($file),   "is $file not a directory";
            nok FILETEST-w($file),   "can we not w $file";
            nok FILETEST-rw($file),  "can we not rw $file";
            nok FILETEST-x($file),   "can we not x $file";
            nok FILETEST-rx($file),  "can we not rx $file";
            nok FILETEST-wx($file),  "can we not wx $file";
            nok FILETEST-rwx($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'r-x';
            chmod 0o500, $file;   # need to make sure file access is correct
            sanity($file);
            ok FILETEST-f($file),    "is $file a file";
            nok FILETEST-d($file),   "is $file not a directory";
            nok FILETEST-w($file),   "can we not w $file";
            nok FILETEST-rw($file),  "can we not rw $file";
            ok FILETEST-x($file),    "can we x $file";
            ok FILETEST-rx($file),   "can we rx $file";
            nok FILETEST-wx($file),  "can we not wx $file";
            nok FILETEST-rwx($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'rw-';
            sanity($file);
            ok FILETEST-f($file),    "is $file a file";
            nok FILETEST-d($file),   "is $file not a directory";
            ok FILETEST-w($file),    "can we w $file";
            ok FILETEST-rw($file),   "can we rw $file";
            nok FILETEST-x($file),   "can we not x $file";
            nok FILETEST-rx($file),  "can we not rx $file";
            nok FILETEST-wx($file),  "can we not wx $file";
            nok FILETEST-rwx($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'rwx';
            sanity($file);
            ok FILETEST-f($file),   "is $file a file";
            nok FILETEST-d($file),  "is $file not a directory";
            ok FILETEST-w($file),   "can we w $file";
            ok FILETEST-rw($file),  "can we rw $file";
            ok FILETEST-x($file),   "can we x $file";
            ok FILETEST-rx($file),  "can we rx $file";
            ok FILETEST-wx($file),  "can we wx $file";
            ok FILETEST-rwx($file), "can we rwx $file";
        }
    }

    is $*CWD, $CWD, 'did we restore current dir ok';
}

is $*CWD, $CWD, 'did we restore current dir ok';
