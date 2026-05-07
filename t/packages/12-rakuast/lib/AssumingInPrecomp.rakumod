# Regression fixture for https://github.com/rakudo/rakudo/issues/6169
# .assuming uses RakuAST::Sub.new(...).EVAL internally. When this module
# was loaded as a dependency of a precompiling module, the synthetic Sub's
# static_code never got registered in any SC, so the outer precomp's
# serializer died with "missing static code ref for closure 'assumed.max'".
my &max42 = &max.assuming(42);
