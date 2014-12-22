use v6;

use Test;

plan 50;

sub sanity($it) {
    ok FILETEST-E($it),   "does $it exist";
    nok FILETEST-L($it),  "is $it not a symlink";
    ok FILETEST-S($it),   "does $it have a size";   # not sure this makes sense
    nok FILETEST-Z($it),  "does $it have a size";   # not sure this makes sense
    ok FILETEST-R($it),   "can we r $it";

    ok FILETEST-MODIFIED($it) ~~ Instant, "is modified on $it an Instant";
    ok FILETEST-ACCESSED($it) ~~ Instant, "is accessed on $it an Instant";
    ok FILETEST-CHANGED($it)  ~~ Instant, "is changed on $it an Instant";
}

my $CWD = $*CWD;
{
    temp $*CWD = chdir('t/01-sanity');
    my $dir = $*CWD ~ 'filetests/';
    sanity($dir);
    ok FILETEST-D($dir),   "is $dir a directory";
    nok FILETEST-F($dir),  "is $dir not a file";
    ok FILETEST-W($dir),   "can we w $dir";
    ok FILETEST-RW($dir),  "can we rw $dir";
    ok FILETEST-X($dir),   "can we x $dir";
    ok FILETEST-RX($dir),  "can we rx $dir";
    ok FILETEST-WX($dir),  "can we wx $dir";
    ok FILETEST-RWX($dir), "can we rwx $dir";

    my $CWD = $*CWD;
    {
        temp $*CWD = chdir('filetests');
        {
            my $file = $*CWD ~ 'r--';
            ok FILETEST-F($file),    "is $file a file";
            nok FILETEST-D($file),   "is $file not a directory";
            nok FILETEST-W($file),   "can we not w $file";
            nok FILETEST-RW($file),  "can we not rw $file";
            nok FILETEST-X($file),   "can we not x $file";
            nok FILETEST-RX($file),  "can we not rx $file";
            nok FILETEST-WX($file),  "can we not wx $file";
            nok FILETEST-RWX($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'r-x';
            ok FILETEST-F($file),    "is $file a file";
            nok FILETEST-D($file),   "is $file not a directory";
            nok FILETEST-W($file),   "can we not w $file";
            nok FILETEST-RW($file),  "can we not rw $file";
            ok FILETEST-X($file),    "can we x $file";
            ok FILETEST-RX($file),   "can we rx $file";
            nok FILETEST-WX($file),  "can we not wx $file";
            nok FILETEST-RWX($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'rw-';
            ok FILETEST-F($file),    "is $file a file";
            nok FILETEST-D($file),   "is $file not a directory";
            ok FILETEST-W($file),    "can we w $file";
            ok FILETEST-RW($file),   "can we rw $file";
            nok FILETEST-X($file),   "can we not x $file";
            nok FILETEST-RX($file),  "can we not rx $file";
            nok FILETEST-WX($file),  "can we not wx $file";
            nok FILETEST-RWX($file), "can we not rwx $file";
        }

        {
            my $file = $*CWD ~ 'rwx';
            ok FILETEST-F($file),   "is $file a file";
            nok FILETEST-D($file),  "is $file not a directory";
            ok FILETEST-W($file),   "can we w $file";
            ok FILETEST-RW($file),  "can we rw $file";
            ok FILETEST-X($file),   "can we x $file";
            ok FILETEST-RX($file),  "can we rx $file";
            ok FILETEST-WX($file),  "can we wx $file";
            ok FILETEST-RWX($file), "can we rwx $file";
        }
    }

    is $*CWD, $CWD, 'did we restore current dir ok';
}

is $*CWD, $CWD, 'did we restore current dir ok';
