use Test;

plan 4;

# The statement form of try runs its expression in a called code object,
# so a backtrace taken inside it shows that call as a frame, as the
# traditional grammar does. Log::Async's context feature shifts that
# frame off a filtered backtrace to reach its caller and died with
# "No such method 'file' for invocant of type 'Any'" when the frame
# was not there.

sub context() is hidden-from-backtrace {
    my $e = Exception.new;
    try $e.throw;
    $e.backtrace.grep({ !.is-hidden and !.is-setting }).list
}

my @frames = context();
my $line   = $?LINE - 1;

ok @frames.elems >= 2,
    'the filtered backtrace holds the try frame and its caller';
ok @frames[0].code.defined,
    'the try frame carries a code object';
is @frames[1].line, $line,
    'the frame after the try frame is the caller of the hidden sub';
is @frames[1].file.IO.basename, $?FILE.IO.basename,
    'that caller frame reports this file';
