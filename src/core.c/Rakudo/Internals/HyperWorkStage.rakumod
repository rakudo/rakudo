# Work stages are individual steps in a hyper/race pipeline. They are chained
# in a linked list by the source attribute. Roles for different kinds of stages
# follow.
my role Rakudo::Internals::HyperWorkStage {
    has Rakudo::Internals::HyperWorkStage $.source;
}

# A HyperBatcher stage produces batches of work to do. It will typically be
# created with an Iterable of some kind, and divide up the work into batches
# of the appropriate size. Such a stage always lives at the start of a piece
# of parallel processing pipeline.
my role Rakudo::Internals::HyperBatcher does Rakudo::Internals::HyperWorkStage {
    method produce-batch(int $batch-size --> Rakudo::Internals::HyperWorkBatch) { ... }
}

# A HyperProcessor performs some operation in a work batch, updating it to
# reflect the results of the operation.
my role Rakudo::Internals::HyperProcessor does Rakudo::Internals::HyperWorkStage {
    method process-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) { ... }
}

# A HyperRebatcher is given batches, and may produce zero or more batches as a
# result. The produced batches will be passed on to the next pipeline stages.
# This is intended only for steps that need to look across multiple batches,
# but that work in a "streaming" way rather than being a full bottleneck in
# the pipeline. A HyperRebatcher should produce one output batch for each
# input batch it gets (though may produce no batches on one call, and two on
# the next, for example).
my role Rakudo::Internals::HyperRebatcher does Rakudo::Internals::HyperWorkStage {
    method rebatch(Rakudo::Internals::HyperWorkBatch $batch --> List) { ... }
}

# Comes at the end of a pipeline, or a stage in a multi-stage pipeline (that
# is, one with a step in it where all results are needed). The batch-used
# method should be called whenever a batch passed to consume-batch has been
# used. This allows for backpressure control: a sequential iterator at the
# end of a parallel pipeline can choose to call batch-used only at the point
# when the downstream iterator has actually eaten all the values in a batch.
my role Rakudo::Internals::HyperJoiner does Rakudo::Internals::HyperWorkStage {
    has $!batch-used-channel;
    method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) { ... }
    method consume-error(Exception \e) { ... }
    method batch-used(--> Nil) {
        $!batch-used-channel.send(True);
    }
    method SET-BATCH-USED-CHANNEL($!batch-used-channel) {}
}

# vim: expandtab shiftwidth=4
