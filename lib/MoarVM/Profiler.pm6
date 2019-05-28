# An object oriented interface to the data structure as returned by
# nqp::mvmendprofile, when started with mvmstartprofile({:instrumented})

# The nqp::mvmendprofile returns an nqp::list that needs to be nqp::hllize'd
# before you can iterate over it in Perl 6.

# The first element is an array of arrays with information about the types
# that have been allocated.  At the moment of writing, this array appears to
# have information about objects that were created, but for which there is no
# allocation information.  It has the following structure:

# 0                                  - array with type information
# ├ 0 = 140415871842064               - unique ID for this type
# └ 1                                 - hash with additional information
#   ├ repr => P6opaque                  - name of the REPR of this type
#   ├ type => Block                     - type object of type (aka, the .WHAT)
#   ├ managed_size => 72                - size in bytes of instance
#   └ has_unmanaged_data => 1           - is there additional data on heap?

# The second element of the list returned by nqp::mvmendprofile, is a list of
# hashes, one for each thread on which data has been collected.  It has the
# following structure (times are in microseconds, sizes are in bytes):
# 
# 0                                 - hash with info of thread
# ├ thread => 1                       - OS thread ID
# ├ parent => 0                       - OS thread ID of parent thread
# ├ spesh_time => 0                   - amount of time spent in spesh
# ├ start_time => 0                   - when thread was started
# └ total_time => 21004               - total time spent in thread
# ├ call_graph                        - hash with first Callee info
# │ ├ id => 140328666076608             - unique ID of this Callee
# │ ├ first_entry_time => 0             - when Callee was first called
# │ ├ inclusive_time => 2               - time spent here + all sub-Callees
# │ ├ exclusive_time => 2               - time spent in this Callee
# │ ├ entries => 97897                  - number of times Callee was called
# │ ├ inlined_entries => 56757          - times called when inlined
# │ ├ jit_entries => 6566               - times called when jitted
# │ ├ osr => 1                          - times Callee was OSR'd
# │ ├ name => foo                       - name of Callee (if available)
# │ ├ file => gen/moar/BOOTSTRAP.nqp    - filename of Callee
# │ ├ line => 2070                      - line of Callee in file
# │ ├ allocations => (2)                - array with Allocations
# │ │ ├ 0                                 - hash with Allocation info
# │ │ │ ├ count => 100                      - number of allocations
# │ │ │ ├ replaced => 1                     - scalar replacements stopping alloc
# │ │ │ └ id => 140329083232016             - type ID
# │ └ callees => (2)                    - array with Callees called here
# └ gcs                               - array with Garbage Collections
#   └ 0                                 - hash with GC info
#     ├ sequence => 0                     - ordinal number of GC
#     ├ start_time => 1964                - when GC was started
#     ├ time => 7222                      - time spent doing GC
#     ├ full => 0                         - whether or not a full GC
#     ├ responsible => 1                  - thread ID that triggered this GC
#     ├ promoted_bytes => 212960          - bytes promoted from the nuresery
#     ├ promoted_bytes_unmanaged => 54781 - additional bytes promoted
#     ├ retained_bytes => 76576           - bytes *not* promoted
#     ├ cleared_bytes => 3228716          - bytes cleared from the nursery
#     ├ gen2 => 18402                     -
#     ├ gen2_roots => 18402               - gen2 allocs rooted in nursery
#     ├ deallocs                          - array with Deallocations
#       ├ 0                                 - hash with deallocation info
#         ├ id => 140329080607960             - type ID being deallocated
#         ├ nursery_seen => 10                - seen before in a GC
#         └ nursery_fresh => 6                - *not* seen before in a GC

# To reduce any additional memory pressure, the objects created by this class
# are just shims around the arrays / hashes that have been returned.  This
# means that access to attributes is slightly more expensive CPU-wise then
# they could be.  On the other hand, no additional CPU was spent to create
# "proper" Perl 6 objects to begin with, so when inspecting only parts of a
# big profile, will only cause addtional overhead in accessing those parts,
# rather than using a lot of CPU and additional memory on the whole structure.

# We need NQP here, duh!
use nqp;

# stubs we need
class MoarVM::Profiler::Thread { ... }

# some helper ops
sub infix:<%%%>(\a,\b --> Str:D) { sprintf "%.2f%%", (100 * a) / b }
sub infix:<avg>(\a,\b --> Str:D) { sprintf "(avg. %.2f)", a / b }
sub prefix:<§>(\a --> Str:D) { a == 1 ?? "" !! "s" }

# Simple role that maps a set of given keys onto a hash, so that we need to
# do the minimal amount of work to convert the data structure to a full-blown
# object hierarchy.
role OnHash[@keys] {
    has %.hash;

    # this gets run at mixin time, before the class is composed
    for @keys -> $key {
        $?CLASS.^add_method($key, { .hash.AT-KEY($key) } )
          unless $?CLASS.^methods.grep(*.name eq $key);
    }

    # don't want to use named parameters for something this simple
    method new(%hash) { self.bless(:%hash) }

    # make sure we have an object, and not just a hash for a given key
    method !mogrify-to-object(\the-class, \key, \link --> Nil) {
        if %!hash{key} -> $hash {
            unless $hash ~~ the-class {
                my \object := the-class.new($hash);
                %!hash.BIND-KEY(key,object);
                object.hash.BIND-KEY(link,self);
            }
        }
        else {
            %!hash.BIND-KEY(key,the-class.new({}));
        }
    }

    # make sure we have a Slip of objects, and not just an array of hashes
    method !mogrify-to-slip(\the-class, \key, \link --> Nil) {
        if %!hash{key} -> @list {
            %!hash.BIND-KEY(
              key,
              @list.map({
                  if $_ ~~ the-class {
                      $_
                  }
                  else {
                      my \object := the-class.new($_);
                      object.hash.BIND-KEY(link,self);
                      object
                  }
              }).Slip
            );
        }
        else {
            %!hash.BIND-KEY(key,Empty);
        }
    }

    method Str(--> Str:D) { self.gist }
}

# Information about objects of a certain type being allocated in a Callee.
class MoarVM::Profiler::Allocation does OnHash[<
  callee
  count
  id
  jit
>] {

    # additional accessor logic
    method jit(--> Int:D) { %!hash<jit> // %!hash.BIND-KEY("jit",0) }

    # convenience methods
    method thread() { self.callee.thread }
    method name()   { self.thread.type_by_id($.id).name }
    method file()   { self.callee.file }
    method line()   { self.callee.line }

    method gist(--> Str:D) {
        my $gist = "Allocated $.count objects of $.name";
        $gist ~= " (JITted $.jit)" if $.jit;
        $gist ~ "\n  at $.file line $.line"
    }
}

# Information about a Callable that has been called at least once.
class MoarVM::Profiler::Callee does OnHash[<
  allocations
  callees
  caller
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
  nr_allocations
  nr_exclusive_allocations
  nr_frames
  nr_inlined
  nr_jitted
  nr_osred
  osr
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-slip(
          MoarVM::Profiler::Allocation, 'allocations', 'callee');
        self!mogrify-to-slip(
          MoarVM::Profiler::Callee, 'callees', 'caller');
    }

    # additional accessor logic
    method name(--> Str:D) {
        %!hash<name> // %!hash.BIND-KEY("name",'')
    }
    method entries(--> Int:D) {
        %!hash<entries> // %!hash.BIND-KEY("entries",0)
    }
    method inlined_entries(--> Int:D) {
        %!hash<inlined_entries> // %!hash.BIND-KEY("inlined_entries",0)
    }
    method jit_entries(--> Int:D) {
        %!hash<jit_entries> // %!hash.BIND-KEY("jit_entries",0)
    }
    method osr(--> Int:D) {
        %!hash<osr> // %!hash.BIND-KEY("osr",0)
    }
    method nr_callees(--> Int:D) {
        %!hash<nr_callees> // %!hash.BIND-KEY("nr_callees",self.callees.elems)
    }
    method nr_exclusive_allocations(--> Int:D) {
        %!hash<nr_exclusive_allocations> // %!hash.BIND-KEY(
          "nr_exclusive_callees",
          self.allocations.map(*.count).sum
        )
    }
    method nr_allocations(--> Int:D) {
        %!hash<nr_allocations> // %!hash.BIND-KEY(
          "nr_allocations",
          self.nr_exclusive_allocations + self.callees.map(*.nr_allocations).sum
        )
    }
    method nr_frames(--> Int:D) {
        %!hash<nr_frames> // %!hash.BIND-KEY(
          "nr_frames",
          self.entries + self.callees.map(*.nr_frames).sum
        )
    }
    method nr_inlined(--> Int:D) {
        %!hash<nr_inlined> // %!hash.BIND-KEY(
          "nr_inlined",
          self.inlined_entries + self.callees.map(*.nr_inlined).sum
        )
    }
    method nr_jitted(--> Int:D) {
        %!hash<nr_jitted> // %!hash.BIND-KEY(
          "nr_jitted",
          self.jit_entries + self.callees.map(*.nr_jitted).sum
        )
    }
    method nr_osred(--> Int:D) {
        %!hash<nr_osred> // %!hash.BIND-KEY(
          "nr_osred",
          self.osr + self.callees.map(*.nr_osred).sum
        )
    }

    method thread() {
        %!hash<thread> // %!hash.BIND-KEY(
          "thread",
          do {
              my $thread = self.caller;
              $thread = $thread.caller
                until $thread ~~ MoarVM::Profiler::Thread;
              $thread
          }
        )
    }

    method all_callees() {
        self.callees, |self.callees.map: |*.all_callees
    }
    method all_allocations() {
        self.allocations, |self.callees.map: |*.all_allocations
    }

    method average_inclusive_time(--> Str:D) {
        sprintf("%.2f",$.inclusive_time / $.nr_frames)
    }
    method average_exclusive_time(--> Str:D) {
        sprintf("%.2f",$.exclusive_time / $.nr_frames)
    }

    method gist(--> Str:D) {
        if $.entries -> $entries {
            my $gist = $.name ?? "'$.name'" !! 'Unnamed callee';
            $gist ~= " was called $_ time{§$_}\n" given $entries;
            $gist ~= "   at $.file line {$.line}\n";

            $gist ~= $_ ~~ MoarVM::Profiler::Thread
              ?? " from thread #" ~ .id ~ ".\n"
              !! " from " ~ .file ~ " line " ~ .line ~ ".\n"
              given self.caller;

            $gist ~= "$_ call{ $_ == 1 ?? " was" !! "s were"} inlined ({ $_ %%% $entries }).\n"
              if $_ given $.inlined_entries;
            $gist ~= "$_ call{ $_ == 1 ?? " was" !! "s were"} jitted ({ $_ %%% $entries }).\n"
              if $_ given $.jit_entries;

            if $entries > 1 {
                $gist ~= "First called at $.first_entry_time microsecs for $_ microsecs { $_ avg $entries }"
                  given $.inclusive_time;
            }
            else {
                $gist ~= "Called at $.first_entry_time microsecs for $.inclusive_time microsecs";
            }

            if $.callees {
                $gist ~= $entries
                  ?? "\n  of which $_ microsecs here { $_ avg $entries }.\n"
                  !! "\n  of which $_ microsecs here.\n"
                  given $.exclusive_time;
            }
            else {
                $gist ~= ".\n";
            }

            if $.nr_allocations -> $allocations {
                $gist ~= "Did $_ allocation{§$_}" given $allocations;
                $gist ~= $allocations == $_
                  ?? ".\n"
                  !! " (of which $_ ({ $_ %%% $allocations }) { $_ == 1 ?? "was" !! "were"} done here).\n"
                  given $.nr_exclusive_allocations;
            }
            $gist ~= "Had $_ On Stack Replacement{§$_}.\n"
              if $_ given $.osr;
            $gist
        }
        else {
            "Callee without information"
        }
    }
}

# Information about a de-allocation as part of a garbage collection.
class MoarVM::Profiler::Deallocation does OnHash[<
  id
  gc
  name
  nursery_fresh
  nursery_seen
>] {

    method name(--> Str:D) {
        %!hash<name>
          // %!hash.BIND-KEY("name",self.gc.thread.type_by_id($.id).name)
    }
    method gist(--> Str:D) {
        "De-allocation of $.name in garbage collection {$.gc.sequence}"
    }
}


# Information about a garbage collection.
class MoarVM::Profiler::GC does OnHash[<
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
  thread
  time
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-slip(MoarVM::Profiler::Deallocation, 'deallocs', 'gc');
    }

    method gist(--> Str:D) {
        "Garbage collection $.sequence cleared $.cleared_bytes bytes"
    }
}

# Information about a type that have at least one object instantiated.
class MoarVM::Profiler::Type does OnHash[<
  has_unmanaged_data
  id
  managed_size
  nr_allocations
  name
  profile
  repr
  type
>] {

    method new( ($id,%hash) ) {
        %hash.BIND-KEY("id",$id);
        self.bless(:%hash)
    }
    method TWEAK(--> Nil) {
        # link to originating profile
        %!hash.BIND-KEY("profile",$_) with $*PROFILE;
    }

    # additional accessor logic
    method name(--> Str:D) {
        %!hash<name> // %!hash.BIND-KEY(
          "name",
          ((try .^name) || "(" ~ nqp::objectid($_) ~ ")" given %!hash<type>)
        )
    }
    method has_unmanaged_data(--> Int:D) {
        %!hash<has_unmanaged_data> // %!hash.BIND-KEY("has_unmanaged_data",0)
    }

    method all_allocations() {
        with $.profile {
            my $id = $.id;
            |.threads_by_id.values.map: *.all_allocations.grep(*.id eq $id)
        }
        else {
            ()
        }
    }

    method nr_allocations() {
        %!hash<nr_allocations> // %!hash.BIND-KEY(
          "nr_allocations",
          do {
              with $.profile {
                  my $id = $.id;
                  .threads_by_id.values.map(
                    *.all_allocations.grep(*.id eq $id).elems
                  ).sum
              }
              else {
                  0
              }
          }
        )
    }

    method gist(--> Str:D) {
        "$.name of $.repr ($.managed_size bytes, $.nr_allocations allocations)"
    }
}

# Information about a thread.
class MoarVM::Profiler::Thread does OnHash[<
  callee
  gcs
  id
  names
  parent
  profile
  spesh_time
  start_time
  total_time
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-object(MoarVM::Profiler::Callee,'call_graph','caller');
        self!mogrify-to-slip(MoarVM::Profiler::GC,'gcs','thread');

        # link to originating profile
        %!hash.BIND-KEY("profile",$_) with $*PROFILE;
    }

    # additional accessor logic
    method id(--> Int:D) {
        %!hash<id>
          // %!hash.BIND-KEY("id",%!hash.DELETE-KEY("thread"))
    }
    method callee(--> MoarVM::Profiler::Callee:D) {
        %!hash<callee>
          // %!hash.BIND-KEY("callee",%!hash.DELETE-KEY("call_graph"))
    }
    method types_by_id()   { $.profile ?? $.profile.types_by_id   !! {} }
    method types_by_name() { $.profile ?? $.profile.types_by_name !! {} }

    # type given ID / name
    method type_by_id(Int:D $id --> MoarVM::Profiler::Type:D) {
        $.profile ?? $.profile.types_by_id{$id} !! Any
    }
    method type_by_name(Str:D $name --> MoarVM::Profiler::Type:D) {
        $.profile ?? $.profile.types_by_name{$name} !! Any
    }

    method all_callees()     { self.callee, |self.callee.all_callees }
    method all_allocations() { self.callee.all_allocations }

    method nr_allocations(--> Int:D) { self.callee.nr_allocations  }
    method nr_frames(--> Int:D)      { self.callee.nr_frames       }
    method nr_inlined(--> Int:D)     { self.callee.nr_inlined      }
    method nr_jitted(--> Int:D)      { self.callee.nr_jitted       }
    method nr_osred(--> Int:D)       { self.callee.nr_osred        }
    method nr_gcs(--> Int:D)         { self.gcs.elems              }
    method nr_full_gcs(--> Int:D)    { self.gcs.grep(*.full).elems }

    method callees_by_file(\matcher) {
        self.all_callees.grep({ matcher.ACCEPTS(.file) })
    }
    method allocations_by_file(\matcher) {
        self.callees_by_file(matcher).map: *.allocations
    }

    method gist(--> Str:D) {
        my $gist = "Thread #{$.id}{ " (from thread #$.parent)" if $.parent}:\n";
        $gist ~= $_ ?? "Started at $_ microseconds and r" !! "R"
          given $.start_time;
        $gist ~= "an for $.total_time microseconds";
        $gist ~= $_ ?? " (of which $_ in spesh).\n" !! ".\n"
          given $.spesh_time;
        if $.nr_gcs -> $gcs {
            $gist ~= "Did $gcs garbage collections";
            $gist ~= $_ ?? " (of which $_ full collections).\n" !! ".\n"
              given $.nr_full_gcs;
        }
        if $.nr_frames -> $frames {
            $gist ~= "Called $frames frames.\n";
            $gist ~= "$_ frames were inlined ({$_ %%% $frames}).\n"
              given $.nr_inlined;
            $gist ~= "$_ frames were jitted ({$_ %%% $frames}).\n"
              given $.nr_jitted;
            $gist ~= "$_ On Stack Replacement{§$_} were done.\n"
              given $.nr_osred;
        }
        else {
            $gist ~= "No profileable code was executed.";
        }

        $gist
    }
}

# Main object returned by profile() and friends.
class MoarVM::Profiler {
    has %.types_by_id;
    has %.types_by_name;
    has %.threads_by_id;
    has @.callees_by_id;
    has @.allocations_by_id;
    has @.deallocations_by_id;

    method !SET-SELF(@raw) {
        my $*PROFILE = self;
        %!types_by_id = @raw[0].map: -> $type {
            .id => $_ given MoarVM::Profiler::Type.new($type)
        }

        %!threads_by_id = @raw.skip.map: -> $thread {
            .id => $_ given MoarVM::Profiler::Thread.new($thread)
        }

        self
    }
    method new(@raw) { self.CREATE!SET-SELF(@raw) }

    method types_by_name() {
        %!types_by_name
          ?? %!types_by_name
          !! %!types_by_name = %!types_by_id.values.map: { .name => $_ }
    }

    # type/thread given an ID
    method type_by_id($id)     { %!types_by_id{$id}   }
    method type_by_name($name) { %.types_by_name{$name} }
    method thread_by_id($id)   { %!threads_by_id{$id} }

    method my_callees() {
        self.callees_by_file(callframe(1).file)
    }
    method callees_by_file(\matcher) {
        self.callees.grep({ matcher.ACCEPTS(.file) })
    }
    method callees_by_name(\matcher) {
        self.callees.grep({ matcher.ACCEPTS(.name) })
    }

    method my_allocations() {
        self.allocations_by_file(callframe(1).file)
    }
    method allocations_by_file(\matcher) {
        self.callees_by_file(matcher).map: *.allocations
    }

    method report(--> Str:D) {
        (
"  #   wallclock   objects    frames   inlined    jitted   OSR   GCs",
"----+-----------+---------+---------+---------+---------+-----+-----",
          |self.threads.grep(*.value.nr_frames).sort(*.key).map( {
              sprintf("%3d %11d %9d %9d %9d %9d %5d %5d",
                .id,
                .total_time,
                .nr_allocations,
                .nr_frames,
                .nr_inlined,
                .nr_jitted,
                .nr_osred,
                .nr_gcs,
              ) given .value
          } ),
"----+-----------+---------+---------+---------+---------+-----+-----",
        ).join("\n")
    }

    method gist(--> Str:D) {
        self.threads_by_id.sort(*.key).map(*.value.gist).join("-" x 80 ~ "\n")
    }
    method Str(--> Str:D) { self.gist }

    method sink(--> Nil) { self.note if %!threads_by_id }

    multi method profile(&code, :$times!) {
        my @profiles;
        for ^$times {
            nqp::mvmstartprofile({:instrumented});
            code();
            @profiles.push: MoarVM::Profiler.new(nqp::mvmendprofile);
        }
        @profiles
    }
    multi method profile(&code --> MoarVM::Profiler:D) {
        nqp::mvmstartprofile({:instrumented});
        code();
        MoarVM::Profiler.new(nqp::mvmendprofile)
    }

    method !average(@profiles --> MoarVM::Profiler:D) {
        return @profiles.head unless @profiles > 1;  # nothing to average

#        # logic for adding a type not yet seen (by name)
#        my $type_id = 0;
#        method !new-type($source  --> Nil) {
#            my $type := MoarVM::Profiler::Type.new(
#              (++$type_id, {
#                managed_size       => $source.managed_size,
#                repr               => $source.repr,
#                type               => $source.type,
#                has_unmanaged_data => $source.has_unmanaged_data,
#              })
#            );
#
#            nqp::bindattr($type,MoarVM::Profiler::Type,'$!name',$source.name);
#            nqp::bindattr($type,MoarVM::Profiler::Type,'%!threads',%!threads);
#            %!types.BIND-KEY($type_id,     $type);
#            %!names.BIND-KEY($source.name, $type);
#        }
#
#        # logic for adding to an existing type (by name)
#        method !add-to-type($source, $target --> Nil) {
#            my %hash := $target.hash;
#            %hash<managed_size>       += $source.managed_size;
#            %hash<has_unmanaged_data> += $source.has_unmanaged_data;
#        }
#
#        my %callees;
#        my %allocations;
#        for @profiles -> $profile {
#            for $profile.callees -> $callee {
#                if %callees{$callee.sha} -> $found {
#                }
#                else {
#                    my $new;
#                    @!callees.push($new);
#                }
#
#                for $callee.allocations -> $allocation {
#                }
#            }
#        }
#
        self
    }

    method average(*@profiles --> MoarVM::Profiler:D) {
        self.CREATE!average(@profiles)
    }

    method average_profile(&code, :$times = 5 --> MoarVM::Profiler:D) {
        self.average( self.profile(&code, :$times) )
    }
}

# Raw subs, for cases where starting an extra scope would be troublesome
sub profile_start(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented})
}

sub profile_end(--> MoarVM::Profiler:D) is export {
    MoarVM::Profiler.new(nqp::mvmendprofile)
}

# Profile the rest of the compilation unit
sub profile_rest(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented});
    my $end-profile = True;
    END MoarVM::Profiler.new(nqp::mvmendprofile) if $end-profile;
}

# HLL sub for profiling a piece of code and getting the info from that
sub profile(&code --> MoarVM::Profiler:D) is export {
    MoarVM::Profiler.profile(&code)
}

# vim: ft=perl6 expandtab sw=4
