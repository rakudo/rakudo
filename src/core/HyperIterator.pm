# HyperIterator is done by things that know how to get a batch of values
# filled up, and maybe to process it.
my class HyperWorkBuffer { ... }
my class HyperConfiguration { ... }
my role HyperIterator {
    # Called in order to fill up a work buffer with items. For things that
    # can be part of a pipeline of operations, this simply defers to the
    # next thing in the pipeline, up until a source is reached. The source
    # should push items to the input of the work buffer. Only one thread
    # can ever be calling fill-batch on a given iterator chain at a time
    # (usually the co-ordinating thread), so you can safely consume items
    # from any usual iterable to fill the batch. Return IterationEnd if this
    # is the last buffer you can produce, and anything else otherwise.
    method fill-buffer(HyperWorkBuffer:D $work, int $items) { ... }

    # Process the provided work buffer. If you are a source, then return Mu.
    # If you are a processing stage, you should pass the work buffer down to
    # the next process-buffer in the chain. If it returns a HyperWorkBuffer,
    # then .swap() it so the previous stage's output is now your input, and
    # then process it, putting your results into the output buffer. This is
    # the code that can run on any thread; keep it side-effect free.
    method process-buffer(HyperWorkBuffer:D $work) { ... }

    # Gets HyperConfiguration information for this parallelized operation.
    # Processing stages should ask their source.
    method configuration() { ... }
}

# vim: ft=perl6 expandtab sw=4
