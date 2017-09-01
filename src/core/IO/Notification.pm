# stub what we need now
my class Supplier { ... }

my enum FileChangeEvent (:FileChanged(1), :FileRenamed(2));

my class IO::Notification {
    my class FileWatchCancellation is repr('AsyncTask') { }

    class Change {
        has $.path;
        has $.event;
        multi method gist(Change:D:) {
            "$.path: $.event";
        }
        method IO { $!path.IO }
    }

    method watch-path(Str() $path, :$scheduler = $*SCHEDULER) {
        my $s = Supplier.new;
        nqp::watchfile(
            $scheduler.queue,
            -> \path, \rename, \err {
                if err {
                    $s.quit(err);
                }
                else {
                    my $event = rename ?? FileRenamed !! FileChanged;
                    $s.emit(Change.new(:path($*SPEC.catdir($path, path)), :$event));
                }
            },
            $path, FileWatchCancellation);
        $s.Supply
    }
}
