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
  osr
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-list(Profile::Allocation, 'allocations');
        self!mogrify-to-list(Profile::Callee,     'callees'    );
    }

    method all_callees() {
        |(|self.callees, |self.callees.map: *.all_callees)
    }
    method all_allocations() {
        |(|self.allocations, |self.callees.map: *.all_allocations)
    }

    method nr_allocations(--> Int:D) {
        self.allocations.map(*.count).sum
          + self.callees.map(*.nr_allocations).sum
    }
    method nr_frames(--> Int:D) {
        (self.entries // 0) + self.callees.map(*.nr_frames).sum
    }
    method nr_inlined(--> Int:D) {
        (self.inlined_entries // 0) + self.callees.map(*.nr_inlined).sum
    }
    method nr_jitted(--> Int:D) {
        (self.jit_entries // 0) + self.callees.map(*.nr_jitted).sum
    }
    method nr_osred(--> Int:D) {
        (self.osr // 0) + self.callees.map(*.nr_osred).sum
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

    method TWEAK(--> Nil) {
        self!mogrify-to-list(Profile::Deallocation, 'deallocs');
    }
}

class Profile::Type does OnHash[<
  managed_size
  repr
  type
  has_unmanaged_data
>] {
    has Int $.id;
    has %.threads;  # set by Profile.new

    method new( ($id,%hash) ) { self.bless(:$id, :%hash) }
    method TWEAK() {
        $_ = (try .^name) || "(" ~ nqp::objectid($_) ~ ")" given %!hash<type>;
    }

    # thread given ID
    method thread($id) { %!threads{$id} }
}

class Profile::Thread does OnHash[<
  callee
  gcs
  parent
  spesh_time
  start_time
  total_time
>] {
    has Int $.id;
    has $.callee;
    has %.types;  # set by Profile.new

    method TWEAK(--> Nil) {
        $!id := %!hash.DELETE-KEY("thread");
        %!hash.ASSIGN-KEY('callee',%!hash.DELETE-KEY("call_graph"));

        self!mogrify-to-object(Profile::Callee, 'callee');
        self!mogrify-to-list(Profile::GC, 'gcs');
    }

    # type given ID
    method type($id) { %!types{$id} }

    method all_callees()     { self.callee.all_callees     }
    method all_allocations() { self.callee.all_allocations }

    method nr_allocations(--> Int:D) { self.callee.nr_allocations }
    method nr_frames(--> Int:D)      { self.callee.nr_frames      }
    method nr_inlined(--> Int:D)     { self.callee.nr_inlined     }
    method nr_jitted(--> Int:D)      { self.callee.nr_jitted      }
    method nr_osred(--> Int:D)       { self.callee.nr_osred       }
}

class Profile {
    has %.types   is required;
    has %.threads is required;

    method !SET-SELF(@raw) {
        %!types = @raw[0].map: -> $type {
            .id => $_ given Profile::Type.new($type)
        }
        %!threads = @raw.skip.map: -> $thread {
            .id => $_ given Profile::Thread.new($thread)
        }

        # let types know about threads and vice-versa
        nqp::bindattr(nqp::decont($_),Profile::Type,'%!threads',%!threads)
          for %!types.values;
        nqp::bindattr(nqp::decont($_),Profile::Thread,'%!types',%!types)
          for %!threads.values;

        self
    }
    method new(@raw) { self.CREATE!SET-SELF(@raw) }

    # type/thread given an ID
    method type($id)   { %!types{$id}   }
    method thread($id) { %!threads{$id} }

    method report(--> Str:D) {
        (
"  #   wallclock    objects     frames    inlined     jitted      OSRed",
"----+-----------+----------+----------+----------+----------+----------",
          |self.threads.grep(*.value.nr_frames).sort(*.key).map( {
              sprintf("%3d%12d%11d%11d%11d%11d%11d",
                .id,
                .total_time,
                .nr_allocations,
                .nr_frames,
                .nr_inlined,
                .nr_jitted,
                .nr_osred,
              ) given .value
          } ),
"----+-----------+----------+----------+----------+----------+----------",
        ).join("\n")
    }

    method sink(--> Nil) {
        note self.report;
    }
}

# Raw subs, for cases where starting an extra scope would be troublesome
sub profile_start(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented})
}

sub profile_end(--> Profile:D) is export {
    Profile.new(nqp::mvmendprofile)
}

# HLL sub for profiling a piece of code and getting the info from that
sub profile(&code --> Profile:D) is export {
    nqp::mvmstartprofile({:instrumented});
    code();
    Profile.new(nqp::mvmendprofile)
}
