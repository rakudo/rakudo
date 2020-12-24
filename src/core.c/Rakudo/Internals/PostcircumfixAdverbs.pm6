# Handle adverbs for array postcircumfix here late so that we can use
# all of the high-level language features, such as native arrays, that
# are not available (or needed) earlier in the settings build.

augment class Rakudo::Internals {

    # Constants to allow mapping of valid slice adverb combinations to
    # a value that can be used in lookup table to get a dispatch table
    # lookup value.
    my constant SLICE_NO_ADVERBS = 0x0000;
    my constant SLICE_DELETE     = 0x0001;  # :delete
    my constant SLICE_EXISTS     = 0x0002;  # :exists
    my constant SLICE_NOT_EXISTS = 0x0004;  # :!exists
    my constant SLICE_KV         = 0x0008;  # :k
    my constant SLICE_NOT_KV     = 0x0010;  # :!kv
    my constant SLICE_P          = 0x0020;  # :p
    my constant SLICE_NOT_P      = 0x0040;  # :!p
    my constant SLICE_K          = 0x0080;  # :k
    my constant SLICE_NOT_K      = 0x0100;  # :!k
    my constant SLICE_V          = 0x0200;  # :v

    # Array of above constants with their names, used for generating
    # error messages.
    my constant @pc-constant-name = (
      SLICE_DELETE,     'delete',
      SLICE_EXISTS,     'exists',
      SLICE_NOT_EXISTS, '!exists',
      SLICE_KV,         'kv',
      SLICE_NOT_KV,     '!kv',
      SLICE_P,          'p',
      SLICE_NOT_P,      '!p',
      SLICE_K,          'k',
      SLICE_NOT_K,      '!k',
      SLICE_V,          'v',
    );

    # Array that contains dispatch table value for postcircumfix
    # adverbs.  When mapping into this array results in 0, then it
    # was an unsupported combination of adverbs.  Any other value
    # indicates an index value into a dispatch table that should
    # be initialized in the order in which these combinations are
    # initialized here.
    my constant @pc-adverb-mapper = do {
        my int $i = -1;
        my uint16 @map;
        
        # add the simple access version, e.g. with :!delete or :!v
        @map[SLICE_NO_ADVERBS]                               = ++$i;

        # simple filtering adverbs
        @map[SLICE_KV]                                       = ++$i;
        @map[SLICE_NOT_KV]                                   = ++$i;
        @map[SLICE_P]                                        = ++$i;
        @map[SLICE_NOT_P]                                    = ++$i;
        @map[SLICE_K]                                        = ++$i;
        @map[SLICE_NOT_K]                                    = ++$i;
        @map[SLICE_V]                                        = ++$i;

        # adverbs that return whether exists / existed
        @map[SLICE_EXISTS]                                   = ++$i;
        @map[SLICE_EXISTS + SLICE_KV]                        = ++$i;
        @map[SLICE_EXISTS + SLICE_NOT_KV]                    = ++$i;
        @map[SLICE_EXISTS + SLICE_P]                         = ++$i;
        @map[SLICE_EXISTS + SLICE_NOT_P]                     = ++$i;
        @map[SLICE_EXISTS + SLICE_DELETE]                    = ++$i;
        @map[SLICE_EXISTS + SLICE_DELETE + SLICE_KV]         = ++$i;
        @map[SLICE_EXISTS + SLICE_DELETE + SLICE_NOT_KV]     = ++$i;
        @map[SLICE_EXISTS + SLICE_DELETE + SLICE_P]          = ++$i;
        @map[SLICE_EXISTS + SLICE_DELETE + SLICE_NOT_P]      = ++$i;

        # adverbs that return whether NOT exists / existed
        @map[SLICE_NOT_EXISTS]                               = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_KV]                    = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_NOT_KV]                = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_P]                     = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_NOT_P]                 = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_DELETE]                = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_DELETE + SLICE_KV]     = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_DELETE + SLICE_NOT_KV] = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_DELETE + SLICE_P]      = ++$i;
        @map[SLICE_NOT_EXISTS + SLICE_DELETE + SLICE_NOT_P]  = ++$i;

        # adverbs that just delete
        @map[SLICE_DELETE]                                   = ++$i;
        @map[SLICE_DELETE + SLICE_KV]                        = ++$i;
        @map[SLICE_DELETE + SLICE_NOT_KV]                    = ++$i;
        @map[SLICE_DELETE + SLICE_P]                         = ++$i;
        @map[SLICE_DELETE + SLICE_NOT_P]                     = ++$i;
        @map[SLICE_DELETE + SLICE_K]                         = ++$i;
        @map[SLICE_DELETE + SLICE_NOT_K]                     = ++$i;
        @map[SLICE_DELETE + SLICE_V]                         = ++$i;

        @map
    }

    # Take a set of adverbs in a hash, and return either a positive
    # dispatch table index value if it was a valid combination of
    # adverbs, or a X::Adverb exception object with the "unexpected"
    # and "nogo" fields set.
    method ADVERBS_TO_DISPATCH_INDEX(%adverbs) {
        my $nameds := nqp::getattr(%adverbs,Map,'$!storage');

        my int $bitmap;
        my $value;

        # Check all the valid adverbs
        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'exists')),
          nqp::stmts(
            ($bitmap = nqp::if($value,SLICE_EXISTS,SLICE_NOT_EXISTS)),
            nqp::deletekey($nameds,'exists')
          )
        );

        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'delete')),
          nqp::stmts(
            nqp::if(
              $value,
              ($bitmap = nqp::bitor_i($bitmap,SLICE_DELETE))
            ),
            nqp::deletekey($nameds,'delete')
          )
        );

        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'kv')),
          nqp::stmts(
            ($bitmap = nqp::bitor_i($bitmap,nqp::if(
              $value,
              SLICE_KV,
              SLICE_NOT_KV
            ))),
            nqp::deletekey($nameds,'kv')
          )
        );

        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'p')),
          nqp::stmts(
            ($bitmap = nqp::bitor_i($bitmap,nqp::if(
              $value,
              SLICE_P,
              SLICE_NOT_P
            ))),
            nqp::deletekey($nameds,'p')
          )
        );

        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'k')),
          nqp::stmts(
            ($bitmap = nqp::bitor_i($bitmap,nqp::if(
              $value,
              SLICE_K,
              SLICE_NOT_K
            ))),
            nqp::deletekey($nameds,'k')
          )
        );

        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'v')),
          nqp::stmts(
            nqp::if(
              $value,
              ($bitmap = nqp::bitor_i($bitmap,SLICE_V))
            ),
            nqp::deletekey($nameds,'v')
          )
        );

        # Perform the actual lookup
        my int $index = nqp::atpos_i(@pc-adverb-mapper,$bitmap);

        # Unexpected adverbs
        if nqp::elems($nameds) {
            X::Adverb.new(
              unexpected => %adverbs.keys.sort.list,
              nogo       => @pc-constant-name.map( -> \constant, \adverb {
                  adverb if nqp::bitand_i(constant,$bitmap);
              } ).list
            )
        }

        # All adverbes accounted for and we have a dispatch index
        elsif $index {
            $index
        }

        # Did not find a dispatch index, but had valid adverbs
        elsif $bitmap {
            X::Adverb.new(
              nogo => @pc-constant-name.map( -> \constant, \adverb {
                  adverb if nqp::bitand_i(constant,$bitmap)
              } ).list
            )
        }

        # Had valid adverbs, but no special handling required
        else {
            0
        }
    }

    my constant $access-dispatch = nqp::list(
      Array::Slice::Access::none,
      Array::Slice::Access::kv,
      Array::Slice::Access::not-kv,
      Array::Slice::Access::p,
      Array::Slice::Access::not-p,
      Array::Slice::Access::k,
      Array::Slice::Access::not-k,
      Array::Slice::Access::v,
      Array::Slice::Access::exists,
      Array::Slice::Access::exists-kv,
      Array::Slice::Access::exists-not-kv,
      Array::Slice::Access::exists-p,
      Array::Slice::Access::exists-not-p,
      Array::Slice::Access::exists-delete,
      Array::Slice::Access::exists-delete-kv,
      Array::Slice::Access::exists-delete-not-kv,
      Array::Slice::Access::exists-delete-p,
      Array::Slice::Access::exists-delete-not-p,
      Array::Slice::Access::not-exists,
      Array::Slice::Access::not-exists-kv,
      Array::Slice::Access::not-exists-not-kv,
      Array::Slice::Access::not-exists-p,
      Array::Slice::Access::not-exists-not-p,
      Array::Slice::Access::not-exists-delete,
      Array::Slice::Access::not-exists-delete-kv,
      Array::Slice::Access::not-exists-delete-not-kv,
      Array::Slice::Access::not-exists-delete-p,
      Array::Slice::Access::not-exists-delete-not-p,
      Array::Slice::Access::delete,
      Array::Slice::Access::delete-kv,
      Array::Slice::Access::delete-not-kv,
      Array::Slice::Access::delete-p,
      Array::Slice::Access::delete-not-p,
      Array::Slice::Access::delete-k,
      Array::Slice::Access::delete-not-k,
      Array::Slice::Access::delete-v
    );

    method ACCESS-DISPATCH-CLASS(int $index) {
        nqp::atpos($access-dispatch,$index)
    }

    my constant $lazy-access-dispatch = nqp::list(
      Array::Slice::Access::lazy-none,
      Array::Slice::Access::lazy-kv,
      Array::Slice::Access::lazy-kv,                   # same
      Array::Slice::Access::lazy-p,
      Array::Slice::Access::lazy-p,                    # same
      Array::Slice::Access::lazy-k,
      Array::Slice::Access::lazy-k,                    # same
      Array::Slice::Access::lazy-v,
      Array::Slice::Access::lazy-exists,
      Array::Slice::Access::lazy-exists-kv,
      Array::Slice::Access::lazy-exists-kv,            # same
      Array::Slice::Access::lazy-exists-p,
      Array::Slice::Access::lazy-exists-p,             # same
      Array::Slice::Access::lazy-exists-delete,
      Array::Slice::Access::lazy-exists-delete-kv,
      Array::Slice::Access::lazy-exists-delete-kv,     # same
      Array::Slice::Access::lazy-exists-delete-p,
      Array::Slice::Access::lazy-exists-delete-p,      # same
      Array::Slice::Access::lazy-not-exists,
      Array::Slice::Access::lazy-not-exists-kv,
      Array::Slice::Access::lazy-not-exists-kv,        # same
      Array::Slice::Access::lazy-not-exists-p,
      Array::Slice::Access::lazy-not-exists-p,         # same
      Array::Slice::Access::lazy-not-exists-delete,
      Array::Slice::Access::lazy-not-exists-delete-kv,
      Array::Slice::Access::lazy-not-exists-delete-kv, # same
      Array::Slice::Access::lazy-not-exists-delete-p,
      Array::Slice::Access::lazy-not-exists-delete-p,  # same
      Array::Slice::Access::lazy-delete,
      Array::Slice::Access::lazy-delete-kv,
      Array::Slice::Access::lazy-delete-kv,            # same
      Array::Slice::Access::lazy-delete-p,
      Array::Slice::Access::lazy-delete-p,             # same
      Array::Slice::Access::lazy-delete-k,
      Array::Slice::Access::lazy-delete-k,             # same
      Array::Slice::Access::lazy-delete-v
    );

    method LAZY-ACCESS-DISPATCH-CLASS(int $index) {
        nqp::atpos($lazy-access-dispatch,$index)
    }
}

# vim: expandtab shiftwidth=4
