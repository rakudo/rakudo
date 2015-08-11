my enum FileChangeEvent (:FileChanged(1), :FileRenamed(2));

my class IO::Notification {
    my class FileWatchCancellation is repr('AsyncTask') { }

    class Change {
        has $.path;
        has $.event;
        multi method gist(Change:D:) {
            "$.path: $.event";
        }
    }

    method watch-path(Str() $path, :$scheduler = $*SCHEDULER) {
        my $s = Supply.new;
        nqp::watchfile(
            $scheduler.queue,
            -> \path, \rename, \err {
                if err {
                    $s.quit(err);
                }
                else {
                    my $event = rename ?? FileRenamed !! FileChanged;
                    $s.emit(Change.new(:path(path // $path), :$event));
                }
            },
            $path, FileWatchCancellation);
        $s
    }

    method watch_path(|c) {
        DEPRECATED('watch-path', |<2015.08 2015.09> );
        self.watch-path(|c);
    }
}
