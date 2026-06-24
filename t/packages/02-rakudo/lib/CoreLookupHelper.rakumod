unit module CoreLookupHelper;

# A role method that references a CORE:: symbol. The role body is compiled
# dynamically and serialized when this module is precompiled, so resolving
# the CORE pseudo-package must not capture the unserializable setting context.
role Asker is export {
    method ask($prompt) { &CORE::prompt($prompt) }
}
