use v6;

# This file keeps track of the differences between TAI and UTC
# for internal use.

# Some handy tables:
# http://tf.nist.gov/pubs/bulletin/leapsecond.htm
# http://hpiers.obspm.fr/eop-pc/earthor/utc/TAI-UTC_tab.html

module tai-utc {

    # our @leap-second-dates = <
    our sub leap-second-dates { <
        2008-12-31
        2005-12-31
        1998-12-31
        1997-06-30
        1995-12-31
        1994-06-30
        1993-06-30
        1992-06-30
        1990-12-31
        1989-12-31
        1987-12-31
        1985-06-30
        1983-06-30
        1982-06-30
        1981-06-30
        1979-12-31
        1978-12-31
        1977-12-31
        1976-12-31
        1975-12-31
        1974-12-31
        1973-12-31
        1972-12-31
        1972-06-30
        1971-12-31
    > };

    # our %leap-seconds = reverse(@leap-second-dates) Z=> 10 .. *;

    # So for any date $d in @leap-second-dates, $d 23:59:00 UTC
    # is the leap second that made (or will make) UTC
    # %leap-seconds{$d} seconds behind TAI.
    
};
