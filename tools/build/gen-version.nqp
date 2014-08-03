#! nqp

=begin pod 
=TITLE gen-version.nqp -- script to generate version information for HLL compilers
=end pod

# TODO make strftime available to nqp

sub is_leap($year) {
    return ($year % 4 == 0) && (($year % 100 == 0) || !($year % 400 == 0));
}

sub year_size($year) {
    return is_leap($year) ?? 366 !! 365;
}

sub month_size($year,$month) {
    my @days := (31,28,31,30,31,30,31,31,30,31,30,31);
    my $bit := is_leap($year) ?? 1 !! 0;
    return @days[$month] + $bit;
}


sub find_time() {
    my $now := nqp::time_i;
    my $secs_this_day := $now % 86400;
    my $days_since_epoch := ($now - $secs_this_day) / 86400;

    my $sec := $secs_this_day % 60;
    my $min := ($secs_this_day % 3600) / 60;
    my $hour := $secs_this_day / 3600;

    my $year := 1970;
    my $jday := $days_since_epoch;
    while $jday >= year_size($year) {
        $jday := $jday - year_size($year);
        $year++;
    }
    
    my $month := 0;
    my $day := $jday;
    while $day >= month_size($year, $month) {
        $day := $day - month_size($year,$month);
        $month++;
    }

    return ($year,$month + 1,$day + 1,$hour,$min,$sec);
}


my $VERSION := slurp('VERSION');
my @stuff := match($VERSION, /\S+/, :global);
my $version := @stuff[0];
my $release := @stuff[1];
my $codename := @stuff[2];

if nqp::stat(".git", nqp::const::STAT_ISDIR) {
    # TODO Find a better way to capture the output of a command 
    my $tmpfile := nqp::sha1(nqp::time_n);
    nqp::shell("git describe --match '2*' > $tmpfile", ".", {});
    my $v2 := slurp($tmpfile);
    $version := match($v2, /\S+/) if $v2 ne '';
    nqp::unlink($tmpfile);
}
my @time := find_time();
my $builddate := nqp::sprintf("%4d-%02d-%02dT%02d:%02d:%02dZ", @time);

say(qq«sub hll-config(\$config) \{
    \$config<name>           := 'rakudo';
    \$config<version>        := '$version';
    \$config<release-number> := '$release';
    \$config<codename>       := '$codename';
    \$config<build-date>     := '$builddate';
}»);
nqp::exit(1);

