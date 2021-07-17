use nqp;

# This constant must specify current CORE revision
# Must precede class declarations to allow correct recording of their
# respective language version.
my constant CORE-SETTING-REV = 'e';

my constant DateTimeD = DateTime;
class DateTime is DateTimeD {
    multi method posix(DateTime:D: --> Real:D) {
        # algorithm from Claus Tøndering
        my int $a = (14 - $.month) div 12;
        my int $y = $.year + 4800 - $a;
        my int $m = $.month + 12 * $a - 3;
        my int $jd = $.day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400 - 32045;
        ($jd - 2440588) * 86400
          + $.hour      * 3600
          + $.minute    * 60
          - $.timezone
          + nqp::getattr(self,DateTimeD,'$!second')
    }
}

# vim: expandtab shiftwidth=4
