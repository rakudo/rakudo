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

        multi method WHICH(Change:D: --> ValueObjAt:D) {
            nqp::box_s(
              nqp::join('|',nqp::list_s(self.^name,$!event.Str,$!path)),
              ValueObjAt
            )
        }
    }

    method watch-path(Str() $path, :$scheduler = $*SCHEDULER) {
        my $is-dir = $path.IO.d;
        my $s = Supplier.new;
        nqp::watchfile(
            $scheduler.queue(:hint-affinity),
            -> \path, \rename, \err {
                if err {
                    $s.quit(err);
                }
                else {
                    my $event = rename ?? FileRenamed !! FileChanged;
                    my $full-path = ( $is-dir and path ) ?? $*SPEC.catdir($path, path) !! $path;
                    $s.emit(Change.new(:path($full-path), :$event));
                }
            },
            $path, FileWatchCancellation);
        $s.Supply
    }
}

# vim: expandtab shiftwidth=4
