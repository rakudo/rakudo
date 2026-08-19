# Implementations shared between HyperSeq and RaceSeq.
class Rakudo::Internals::HyperRaceSharedImpl is implementation-detail {
    my class Grepper does Rakudo::Internals::HyperProcessor {
        has $!matcher is built;

        method process-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
            my $items := $batch.items;
            my int $elems = nqp::elems($items);
            my &matcher := nqp::istype($!matcher, Callable)
              ?? $!matcher.clone
              !! $!matcher;
            my int $from = 0;
            my int $to   = -1;
            my int $stopped = 0;
            my $item;

            nqp::if(
              nqp::istype(&matcher,Callable)
                && nqp::not_i(nqp::istype(&matcher,Regex)),
              nqp::while(
                nqp::islt_i($from,$elems) && nqp::not_i($stopped),
                nqp::handle(
                  nqp::stmts(
                    nqp::if(
                      matcher(($item := nqp::atpos($items,$from))),
                      nqp::bindpos($items,++$to,$item)
                    ),
                    ($from = nqp::add_i($from,1))
                  ),
                  'LABELED', Any,
                  'NEXT', ($from = nqp::add_i($from,1)),
                  'REDO', nqp::null,
                  'LAST', ($stopped = 1)
                ),
                :nohandler
              ),
              nqp::while(
                nqp::islt_i($from,$elems) && nqp::not_i($stopped),
                nqp::handle(
                  nqp::stmts(
                    nqp::if(
                      &matcher.ACCEPTS(($item := nqp::atpos($items,$from))),
                      nqp::bindpos($items,++$to,$item)
                    ),
                    ($from = nqp::add_i($from,1))
                  ),
                  'LABELED', Any,
                  'NEXT', ($from = nqp::add_i($from,1)),
                  'REDO', nqp::null,
                  'LAST', ($stopped = 1)
                ),
                :nohandler
              )
            );
            nqp::setelems($items,nqp::add_i($to,1));
            $batch.mark-stopped if $stopped;
        }
    }
    multi method grep(\hyper, $source, \matcher, %options) {
        if %options || nqp::istype(matcher, Code) && matcher.count > 1 {
            # Fall back to sequential grep for cases we can't yet handle
            self.rehyper(hyper, hyper.Any::grep(matcher, |%options))
        }
        elsif nqp::istype(matcher,Block) && matcher.has-phasers {
            NYI('Phasers in hyper/race').throw;
        }
        else {
            hyper.bless:
                configuration => hyper.configuration,
                work-stage-head => Grepper.new(:$source, :matcher(matcher))
        }
    }

    my class Mapper does Rakudo::Internals::HyperProcessor {
        has &!mapper is built;
        has $!label  is built;

        # Push a control exception's payload, if any, following the same
        # Slip semantics as the serial map iterators.
        method !push-control-payload(Mu \result --> Nil) {
            my $payload := nqp::getpayload(nqp::exception);
            nqp::unless(
              nqp::isnull($payload),
              nqp::if(
                nqp::istype($payload,Slip),
                nqp::if(
                  $payload.DEFINITE,
                  nqp::unless(
                    nqp::eqaddr($payload,Empty),
                    $payload.iterator.push-all(result)
                  ),
                  nqp::push(result,$payload)
                ),
                nqp::push(result,$payload)
              )
            )
        }

        method process-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
            my $result := nqp::create(IterationBuffer);
            my $items := $batch.items;
            my int $n = $items.elems;
            my &mapper := &!mapper.clone;
            my $label := nqp::decont($!label);
            my int $i = 0;
            my int $stopped = 0;

            nqp::while(
              nqp::islt_i($i,$n) && nqp::not_i($stopped),
              nqp::handle(
                nqp::stmts(
                  nqp::if(
                    nqp::istype((my \val = mapper(nqp::atpos($items, $i))),Slip)
                      && nqp::not_i(nqp::iscont(val)),
                    val.iterator.push-all($result),
                    nqp::push($result,val)
                  ),
                  ($i = nqp::add_i($i,1))
                ),
                'LABELED', $label,
                'NEXT', nqp::stmts(
                  self!push-control-payload($result),
                  ($i = nqp::add_i($i,1))
                ),
                'REDO', nqp::null,
                'LAST', nqp::stmts(
                  self!push-control-payload($result),
                  ($stopped = 1)
                )
              ),
              :nohandler
            );
            $batch.replace-with($result);
            $batch.mark-stopped if $stopped;
        }
    }
    multi method map(\hyper, $source, &mapper, %options, :$label) {
        NYI('Phasers in hyper/race').throw
          if nqp::istype(&mapper,Block) && &mapper.has-phasers;

        if %options || &mapper.count > 1 {
            # Fall back to sequential map for cases we can't yet handle
            self.rehyper(hyper, hyper.Any::map(&mapper, |%options, :$label))
        }
        else {
            hyper.bless:
                configuration => hyper.configuration,
                work-stage-head => Mapper.new(:$source, :&mapper, :$label)
        }
    }
    multi method invert(\hyper, $source) {
        hyper.bless:
          configuration => hyper.configuration,
          work-stage-head => Mapper.new(:$source,:mapper(-> Pair:D $p {$p.antipair}))
    }

    my class Sinker does Rakudo::Internals::HyperJoiner {
        has Promise $.complete .= new;

        has int $!target = -1;
        has int $!next-unseen = 0;
        has int $!done = 0;
        has $!seen;

        submethod TWEAK() {
            $!seen := nqp::list_i;
        }

        # Completes once every batch up to the target sequence number has
        # been consumed. The target is the batch the batcher marked as last,
        # or the earliest batch a loop control stopped the iteration in;
        # batches beyond a stop may still arrive and are of no interest.
        method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
            self.batch-used();
            my int $sequence = $batch.sequence-number;
            if ($batch.last || $batch.stopped)
                && ($!target < 0 || $sequence < $!target) {
                $!target = $sequence;
            }
            nqp::bindpos_i($!seen,$sequence,1);
            nqp::while(
              nqp::atpos_i($!seen,$!next-unseen),
              ++$!next-unseen
            );
            if $!target >= 0 && $!next-unseen > $!target && !$!done {
                $!done = 1;
                $!complete.keep(True);
            }
        }

        method consume-error(Exception $e --> Nil) {
            $!complete.break($e);
        }
    }
    method sink(\hyper, $source --> Nil) {
        if hyper.DEFINITE {
            CATCH {
                unless nqp::istype($_, X::HyperRace::Died) {
                    ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                }
            }
            my $sink = Sinker.new(:$source);
            Rakudo::Internals::HyperPipeline.start($sink, hyper.configuration);
            $*AWAITER.await($sink.complete);
        }
    }

    proto method rehyper($, $) {*}
    multi method rehyper(HyperSeq \hyper, \seq) {
        my \conf = hyper.configuration;
        seq.hyper(:degree(conf.degree), :batch(conf.batch))
    }
    multi method rehyper(RaceSeq \hyper, \seq) {
        my \conf = hyper.configuration;
        seq.race(:degree(conf.degree), :batch(conf.batch))
    }
}

# vim: expandtab shiftwidth=4
