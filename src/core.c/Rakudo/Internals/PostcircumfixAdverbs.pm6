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
        unless nqp::isnull($value := nqp::atkey($nameds,'exists')) {
            $bitmap = $value ?? SLICE_EXISTS !! SLICE_NOT_EXISTS;
            nqp::deletekey($nameds,'exists');
        }

        unless nqp::isnull($value := nqp::atkey($nameds,'delete')) {
            $bitmap = nqp::bitor_i($bitmap,SLICE_DELETE) if $value;
            nqp::deletekey($nameds,'delete');
        }

        unless nqp::isnull($value := nqp::atkey($nameds,'kv')) {
            $bitmap = nqp::bitor_i($bitmap,$value ?? SLICE_KV !! SLICE_NOT_KV);
            nqp::deletekey($nameds,'kv');
        }

        unless nqp::isnull($value := nqp::atkey($nameds,'p')) {
            $bitmap = nqp::bitor_i($bitmap,$value ?? SLICE_P !! SLICE_NOT_P);
            nqp::deletekey($nameds,'p');
        }

        unless nqp::isnull($value := nqp::atkey($nameds,'k')) {
            $bitmap = nqp::bitor_i($bitmap,$value ?? SLICE_K !! SLICE_NOT_K);
            nqp::deletekey($nameds,'k');
        }

        unless nqp::isnull($value := nqp::atkey($nameds,'v')) {
            $bitmap = nqp::bitor_i($bitmap,SLICE_V) if $value;
            nqp::deletekey($nameds,'v');
        }

        # Perform the actual lookup
        my int $index = @pc-adverb-mapper[$bitmap];

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
}

# vim: expandtab shiftwidth=4
