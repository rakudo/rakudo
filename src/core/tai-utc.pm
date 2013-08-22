# This file keeps track of the differences between TAI and UTC
# for internal use. The "BEGIN" and "END" comments are for
# tools/update-tai-utc.pl.

# Some handy tables:
# http://tf.nist.gov/pubs/bulletin/leapsecond.htm
# http://hpiers.obspm.fr/eop-pc/earthor/utc/TAI-UTC_tab.html

my module tai-utc {

    #our $initial-offset = 10;
    our sub initial-offset() { 10 }
      # TAI - UTC at the Unix epoch (1970-01-01T00:00:00Z).

    # our @leap-second-dates = <
    our sub leap-second-dates() {
        #BEGIN leap-second-dates
        <
        1972-06-30
        1972-12-31
        1973-12-31
        1974-12-31
        1975-12-31
        1976-12-31
        1977-12-31
        1978-12-31
        1979-12-31
        1981-06-30
        1982-06-30
        1983-06-30
        1985-06-30
        1987-12-31
        1989-12-31
        1990-12-31
        1992-06-30
        1993-06-30
        1994-06-30
        1995-12-31
        1997-06-30
        1998-12-31
        2005-12-31
        2008-12-31
        2012-06-30
        >
        #END leap-second-dates
    };

    # our %leap-seconds =
    #     @leap-second-dates Z=> $initial-offset + 1 .. *;

    # So for any date $d in @leap-second-dates, $d 23:59:00 UTC
    # is the leap second that made (or will make) UTC
    # %leap-seconds{$d} seconds behind TAI.

    # Ambiguous POSIX times.
    our sub leap-second-posix() {
        #BEGIN leap-second-posix
        <
          78796800
          94694400
         126230400
         157766400
         189302400
         220924800
         252460800
         283996800
         315532800
         362793600
         394329600
         425865600
         489024000
         567993600
         631152000
         662688000
         709948800
         741484800
         773020800
         820454400
         867715200
         915148800
        1136073600
        1230768000
        1341100800
        >
        #END leap-second-posix
    };

};
