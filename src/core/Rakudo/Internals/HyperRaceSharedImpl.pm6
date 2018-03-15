# Implementations shared between HyperSeq and RaceSeq.
class Rakudo::Internals::HyperRaceSharedImpl {
    my class Grep does Rakudo::Internals::HyperProcessor {
        has $!matcher;

        submethod TWEAK(:$!matcher) {}

        method process-batch(Rakudo::Internals::HyperWorkBatch $batch) {
            nqp::stmts(
              (my $items := $batch.items),
              (my $elems := nqp::elems($items)),
              (my &matcher := nqp::if(
                nqp::istype($!matcher, Callable),
                $!matcher.clone,
                $!matcher
              )),
              (my int $from = -1),
              (my int $to   = -1),
              nqp::if(
                nqp::istype(&matcher,Callable)
                  && nqp::not_i(nqp::istype(&matcher,Regex)),
                nqp::while(
                  nqp::islt_i(($from = nqp::add_i($from,1)),$elems),
                  nqp::if(
                    matcher(nqp::atpos($items,$from)),
                    nqp::bindpos(
                      $items,
                      ($to = nqp::add_i($to,1)),
                      nqp::atpos($items,$from)
                    )
                  )
                ),
                nqp::while(
                  nqp::islt_i(($from = nqp::add_i($from,1)),$elems),
                  nqp::if(
                    matcher.ACCEPTS(nqp::atpos($items,$from)),
                    nqp::bindpos(
                      $items,
                      ($to = nqp::add_i($to,1)),
                      nqp::atpos($items,$from)
                    )
                  )
                )
              ),
              nqp::setelems($items,nqp::add_i($to,1))
            )
        }
    }
    multi method grep(\hyper, $source, \matcher, %options) {
        if %options || nqp::istype(matcher, Code) && matcher.count > 1 {
            # Fall back to sequential grep for cases we can't yet handle
            self.rehyper(hyper, hyper.Any::grep(matcher, |%options))
        }
        elsif nqp::istype(matcher,Block) && matcher.has-phasers {
            X::NYI.new(feature => 'Phasers in hyper/race').throw
        }
        else {
            hyper.bless:
                configuration => hyper.configuration,
                work-stage-head => Grep.new(:$source, :matcher(matcher))
        }
    }

    my class Map does Rakudo::Internals::HyperProcessor {
        has &!mapper;

        submethod TWEAK(:&!mapper) {}

        method process-batch(Rakudo::Internals::HyperWorkBatch $batch) {
            my $result := IterationBuffer.new;
            my $items := $batch.items;
            my int $n = $items.elems;
            my &mapper := &!mapper.clone;
            loop (my int $i = 0; $i < $n; ++$i) {
                my \mapped = mapper(nqp::atpos($items, $i));
                nqp::istype(mapped, Slip) && !nqp::iscont(mapped)
                    ?? mapped.iterator.push-all($result)
                    !! $result.push(mapped)
            }
            $batch.replace-with($result);
        }
    }
    multi method map(\hyper, $source, &mapper, %options) {
        X::NYI.new(feature => 'Phasers in hyper/race').throw
          if nqp::istype(&mapper,Block) && &mapper.has-phasers;

        if %options || &mapper.count > 1 {
            # Fall back to sequential map for cases we can't yet handle
            self.rehyper(hyper, hyper.Any::map(&mapper, |%options))
        }
        else {
            hyper.bless:
                configuration => hyper.configuration,
                work-stage-head => Map.new(:$source, :&mapper)
        }
    }

    my class Sink does Rakudo::Internals::HyperJoiner {
        has Promise $.complete .= new;

        has int $!last-target = -1;
        has int $!batches-seen = 0;
        method consume-batch(Rakudo::Internals::HyperWorkBatch $batch --> Nil) {
            ++$!batches-seen;
            self.batch-used();
            if $batch.last {
                $!last-target = $batch.sequence-number;
            }
            if $!last-target >= 0 && $!batches-seen == $!last-target + 1 {
                $!complete.keep(True);
            }
        }

        method consume-error(Exception $e --> Nil) {
            $!complete.break($e);
        }
    }
    method sink(\hyper, $source --> Nil) {
        if hyper.DEFINITE {
            my $sink = Sink.new(:$source);
            Rakudo::Internals::HyperPipeline.start($sink, hyper.configuration);
            $*AWAITER.await($sink.complete);
            CATCH {
                unless nqp::istype($_, X::HyperRace::Died) {
                    ($_ but X::HyperRace::Died(Backtrace.new(5))).rethrow
                }
            }
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

# vim: ft=perl6 expandtab sw=4
