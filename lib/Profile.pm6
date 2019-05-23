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
        if %!hash{key} -> $hash {
            %!hash{key} = the-class.new($hash) unless $hash ~~ the-class;
        }
        else {
            %!hash{key} = the-class.new({});
        }
    }

    method !mogrify-to-list(\the-class, \key --> Nil) {
        if %!hash{key} -> @list {
            %!hash{key} = @list.map( {
                $_ ~~ the-class ?? $_ !! the-class.new($_)
            } ).list;
        }
        else {
            %!hash{key} = ()
        }
    }
}

class Profile::Allocation does OnHash[<
  count
  id
  jit
>] { }

class Profile::Callee does OnHash[<
  allocations
  callees
  entries
  exclusive_time
  file
  first_entry_time
  id
  inclusive_time
  inlined_entries
  jit_entries
  line
  name
>] {

    method TWEAK(:@allocations --> Nil) {
        self!mogrify-to-list(Profile::Allocation, 'allocations');
        self!mogrify-to-list(Profile::Callee,     'callees'    );
    }

    method all-callees() {
        |(|self.callees, |self.callees.map: *.all-callees)
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

    method all-callees() {
        |(|self.callees, |self.callees.map: *.all-callees)
    }
}

class Profile::Deallocation does OnHash[<
  id
  nursery_fresh
  nursery_seen
>] { }

class Profile::GC does OnHash[<
  cleared_bytes
  deallocs
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
        self!mogrify-to-list(Profile::Deallocation, 'deallocs');
    }
}

class Profile::Object does OnHash[<
  managed_size
  repr
  type
  has_unmanaged_data
>] {
    has $.id;
    has %.threads;  # set by Profile.new

    method new( ($id,%hash) ) { self.bless(:$id, :%hash) }
    method TWEAK() {
        $_ = (try .^name) || "(" ~ nqp::objectid($_) ~ ")" given %!hash<type>;
    }

    # thread given ID
    method thread($id) { %!threads{$id} }
}

class Profile::Thread does OnHash[<
  call_graph
  gcs
  parent
  spesh_time
  start_time
  total_time
>] {
    has $.id;
    has %.objects;  # set by Profile.new

    method TWEAK(--> Nil) {
        $!id := %!hash.DELETE-KEY("thread");
        self!mogrify-to-object(Profile::CallGraph, 'call_graph');
        self!mogrify-to-list(Profile::GC, 'gcs');
    }

    # object given ID
    method object($id) { %!objects{$id} }

    method all-callees() { self.call_graph.all-callees }
}

class Profile {
    has %.objects is required;
    has %.threads is required;

    method !SET-SELF(@raw) {
        %!objects = @raw[0].map: -> $object {
            .id => $_ given Profile::Object.new($object)
        }
        %!threads = @raw.skip.map: -> $thread {
            .id => $_ given Profile::Thread.new($thread)
        }

        # let objects know about threads and vice-versa
        nqp::bindattr(nqp::decont($_),Profile::Object,'%!threads',%!threads)
          for %!objects.values;
        nqp::bindattr(nqp::decont($_),Profile::Thread,'%!objects',%!objects)
          for %!threads.values;

        self
    }
    method new(@raw) { self.CREATE!SET-SELF(@raw) }

    # types of objects seen with their frequencies
    method types() { %!objects.values.map(*.type).Bag }

    # object/thread given an ID
    method object($id) { %!objects{$id} }
    method thread($id) { %!threads{$id} }
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
