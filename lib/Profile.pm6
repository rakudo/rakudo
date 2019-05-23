# An object oriented interface to the data structure as returned by
# nqp::mvmendprofile, when started with mvmstartprofile({:instrumented})

# We need NQP here, duh!
use nqp;

# Simple role that maps a set of given keys onto a hash, so that we need to
# do the minimal amount of work to convert the data structure to a full-blown
# object hierarchy.
role OnHash[@keys] {
    has %.hash;

    for @keys -> $key {
        $?CLASS.^add_method($key, { .hash.AT-KEY($key) } )
    }

    method new(%hash) { self.bless(:%hash) }

    method !mogrify-to-object(\the-class, \key --> Nil) {
        with %!hash{key} {
            $_ = the-class.new($_) unless $_ ~~ the-class;
        }
        default {
            $_ = the-class.new({});
        }
    }

    method !mogrify-to-list(\the-class, \key --> Nil) {
        with %!hash{key} {
            $_ = @$_.map( {
                 $_ ~~ the-class ?? $_ !! the-class.new($_)
            } ).list;
        }
        default {
            $_ = ()
        }
    }
}

class Profile::Allocation does OnHash[<
  count
  id
>] { }

class Profile::Callee does OnHash[<
  allocations
  entries
  exclusive_time
  file
  first_entry_time
  id
  inclusive_time
  line
  name
>] {

    method TWEAK(:@allocations --> Nil) {
        self!mogrify-to-list(Profile::Allocation, 'allocations');
    }
}

class Profile::CallGraph does OnHash[<
  allocations
  callees
  entries
  exclusive_time
  file
  first_entry_time
  id
  inclusive_time
  line
  name
>] {

    method TWEAK(:@allocations, :@callees --> Nil) {
        self!mogrify-to-list(Profile::Allocation, 'allocations');
        self!mogrify-to-list(Profile::Callee,     'callees'    );
    }
}

class Profile::Deallocation does OnHash[<
  id
  nursery_fresh
  nursery_seen
>] { }

class Profile::GC does OnHash[<
  cleared_bytes
  deallocations
  full
  gen2_roots
  promoted_bytes
  promoted_bytes_unmanaged
  responsible
  retained_bytes
  sequence
  start_time
  time
>] {

    method TWEAK(:@deallocations --> Nil) {
        self!mogrify-to-list(Profile::Deallocation, 'deallocations');
    }
}

class Profile::Thread does OnHash[<
  call_graph
  gcs
  id
  parent
  spesh_time
  start_time
  total_time
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-object(Profile::CallGraph, 'call_graph');
        self!mogrify-to-list(Profile::GC, 'gcs');
    }
}

class Profile {
    has @.global  is required;
    has @.threads is required;

    method new(@raw) {
        self.bless(
          global  => @raw[0],
          threads => @raw.skip.map( { Profile::Thread.new($_) } ).list,
        )
    }
}

# Raw subs, for cases where starting an extra scope would be troublesome
sub profile-start(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented})
}

sub profile-end(--> Profile:D) is export {
    Profile.new(nqp::mvmendprofile)
}

# HLL sub for profiling a piece of code and getting the info from that
sub profile(&code --> Profile:D) is export {
    nqp::mvmstartprofile({:instrumented});
    code();
    Profile.new(nqp::mvmendprofile)
}
