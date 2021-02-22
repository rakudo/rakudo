# Handle adverbs for array postcircumfix here late so that we can use
# all of the high-level language features, such as native arrays, that
# are not available (or needed) earlier in the settings build.

augment class Rakudo::Internals {

    # Dispatch table for single element access in Int index
    my constant $access-element-dispatch = nqp::list(
      Array::Element::Access::none,
      Array::Element::Access::kv,
      Array::Element::Access::not-kv,
      Array::Element::Access::p,
      Array::Element::Access::not-p,
      Array::Element::Access::k,
      Array::Element::Access::not-k,
      Array::Element::Access::v,
      Array::Element::Access::exists,
      Array::Element::Access::exists-kv,
      Array::Element::Access::exists-not-kv,
      Array::Element::Access::exists-p,
      Array::Element::Access::exists-not-p,
      Array::Element::Access::exists-delete,
      Array::Element::Access::exists-delete-kv,
      Array::Element::Access::exists-delete-not-kv,
      Array::Element::Access::exists-delete-p,
      Array::Element::Access::exists-delete-not-p,
      Array::Element::Access::not-exists,
      Array::Element::Access::not-exists-kv,
      Array::Element::Access::not-exists-not-kv,
      Array::Element::Access::not-exists-p,
      Array::Element::Access::not-exists-not-p,
      Array::Element::Access::not-exists-delete,
      Array::Element::Access::not-exists-delete-kv,
      Array::Element::Access::not-exists-delete-not-kv,
      Array::Element::Access::not-exists-delete-p,
      Array::Element::Access::not-exists-delete-not-p,
      Array::Element::Access::delete,
      Array::Element::Access::delete-kv,
      Array::Element::Access::delete-not-kv,
      Array::Element::Access::delete-p,
      Array::Element::Access::delete-not-p,
      Array::Element::Access::delete-k,
      Array::Element::Access::delete-not-k,
      Array::Element::Access::delete-v
    );

    method ACCESS-ELEMENT-DISPATCH-CLASS(int $index) {
        nqp::atpos($access-element-dispatch,$index)
    }

    # Dispatch table for single element access in Any index
    my constant $access-element-dispatch-any = nqp::list(
      Array::Element::Access::none-any,
      Array::Element::Access::kv-any,
      Array::Element::Access::not-kv-any,
      Array::Element::Access::p-any,
      Array::Element::Access::not-p-any,
      Array::Element::Access::k-any,
      Array::Element::Access::not-k-any,
      Array::Element::Access::v-any,
      Array::Element::Access::exists-any,
      Array::Element::Access::exists-kv-any,
      Array::Element::Access::exists-not-kv-any,
      Array::Element::Access::exists-p-any,
      Array::Element::Access::exists-not-p-any,
      Array::Element::Access::exists-delete-any,
      Array::Element::Access::exists-delete-kv-any,
      Array::Element::Access::exists-delete-not-kv-any,
      Array::Element::Access::exists-delete-p-any,
      Array::Element::Access::exists-delete-not-p-any,
      Array::Element::Access::not-exists-any,
      Array::Element::Access::not-exists-kv-any,
      Array::Element::Access::not-exists-not-kv-any,
      Array::Element::Access::not-exists-p-any,
      Array::Element::Access::not-exists-not-p-any,
      Array::Element::Access::not-exists-delete-any,
      Array::Element::Access::not-exists-delete-kv-any,
      Array::Element::Access::not-exists-delete-not-kv-any,
      Array::Element::Access::not-exists-delete-p-any,
      Array::Element::Access::not-exists-delete-not-p-any,
      Array::Element::Access::delete-any,
      Array::Element::Access::delete-kv-any,
      Array::Element::Access::delete-not-kv-any,
      Array::Element::Access::delete-p-any,
      Array::Element::Access::delete-not-p-any,
      Array::Element::Access::delete-k-any,
      Array::Element::Access::delete-not-k-any,
      Array::Element::Access::delete-v-any
    );

    method ACCESS-ELEMENT-ANY-DISPATCH-CLASS(int $index) {
        nqp::atpos($access-element-dispatch-any,$index)
    }

    # Constants to allow mapping of valid slice adverb combinations to
    # a value that can be used in lookup table to get a dispatch table
    # lookup value.
    my constant SLICE_NO_ADVERBS = 0x0000;
    my constant SLICE_DELETE     = 0x0001;  # :delete
    my constant SLICE_EXISTS     = 0x0002;  # :exists
    my constant SLICE_NOT_EXISTS = 0x0004;  # :!exists
    my constant SLICE_KV         = 0x0008;  # :kv
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
#?if moar
        my uint16 @map;
#?endif
#?if !moar
        my @map;
#?endif
        
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

    # Take a set of adverbs in a hash, and a name and value of an
    # additional named parameter and return either a positive
    # dispatch table index value if it was a valid combination of
    # adverbs, or an X::Adverb exception object with the "unexpected"
    # and "nogo" fields set.
    method ADVERBS_AND_NAMED_TO_DISPATCH_INDEX(
      %adverbs,
      str $name,
      \extra
    ) {
        my $nameds := nqp::getattr(%adverbs,Map,'$!storage');

        my int $bitmap;
        my $value;

        # Initialize bitmap with additional named mapping, assuming it
        # is one of <exists delete kv p k v>.
        nqp::if(
          nqp::iseq_s($name,'exists'),
          ($bitmap = nqp::if(extra,SLICE_EXISTS,SLICE_NOT_EXISTS)),
          nqp::if(
            nqp::iseq_s($name,'delete'),
            nqp::if(extra,$bitmap = SLICE_DELETE),
            nqp::if(
              nqp::iseq_s($name,'kv'),
              ($bitmap = nqp::if(extra,SLICE_KV,SLICE_NOT_KV)),
              nqp::if(
                nqp::iseq_s($name,'p'),
                ($bitmap = nqp::if(extra,SLICE_P,SLICE_NOT_P)),
                nqp::if(
                  nqp::iseq_s($name,'k'),
                  ($bitmap = nqp::if(extra,SLICE_K,SLICE_NOT_K)),
                  nqp::if(extra,$bitmap = SLICE_V)
                )
              )
            )
          )
        );

        # Check all the other valid adverbs
        nqp::unless(
          nqp::isnull($value := nqp::atkey($nameds,'exists')),
          nqp::stmts(
            ($bitmap = nqp::bitor_i(
              $bitmap,
              nqp::if($value,SLICE_EXISTS,SLICE_NOT_EXISTS)
            )),
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

        # Perform the actual lookup and handling
#?if moar
        my int $index = nqp::atpos_i(@pc-adverb-mapper,$bitmap);
#?endif
#?if !moar
        my $index = @pc-adverb-mapper[$bitmap];
#?endif
        nqp::if(
          nqp::elems($nameds),
          X::Adverb.new(     # Unexpected adverbs
            unexpected => %adverbs.keys.sort.list,
            nogo       => @pc-constant-name.map( -> \constant, \adverb {
                adverb if nqp::bitand_i(constant,$bitmap);
            } ).list
          ),
          nqp::if(
            $index,
            $index,           # All adverbs accounted for have a dispatch index
            nqp::if(
              $bitmap,
              X::Adverb.new(  # Did not find a dispatch index, had valid adverbs
                nogo => @pc-constant-name.map( -> \constant, \adverb {
                    adverb if nqp::bitand_i(constant,$bitmap)
                } ).list
              ),
              0               # Had valid adverbs, no special handling required
            )
          )
        )
    }

    # Take a set of adverbs in a hash, and return either a positive
    # dispatch table index value if it was a valid combination of
    # adverbs, or an X::Adverb exception object with the "unexpected"
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

        # Perform the actual lookup and handling
#?if moar
        my int $index = nqp::atpos_i(@pc-adverb-mapper,$bitmap);
#?endif
#?if !moar
        my $index = @pc-adverb-mapper[$bitmap];
#?endif
        nqp::if(
          nqp::elems($nameds),
          X::Adverb.new(     # Unexpected adverbs
            unexpected => %adverbs.keys.sort.list,
            nogo       => @pc-constant-name.map( -> \constant, \adverb {
                adverb if nqp::bitand_i(constant,$bitmap);
            } ).list
          ),
          nqp::if(
            $index,
            $index,           # All adverbs accounted for have a dispatch index
            nqp::if(
              $bitmap,
              X::Adverb.new(  # Did not find a dispatch index, had valid adverbs
                nogo => @pc-constant-name.map( -> \constant, \adverb {
                    adverb if nqp::bitand_i(constant,$bitmap)
                } ).list
              ),
              0               # Had valid adverbs, no special handling required
            )
          )
        )
    }

    my constant $access-slice-dispatch = nqp::list(
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

    method ACCESS-SLICE-DISPATCH-CLASS(int $index) {
        nqp::atpos($access-slice-dispatch,$index)
    }
}

# vim: expandtab shiftwidth=4
