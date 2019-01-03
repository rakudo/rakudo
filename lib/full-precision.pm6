# Provide full precision for all rational calculations by making all
# Rat FatRats.

my constant Rat is export = FatRat;
my constant &old-infix-slash = &infix:</>;
my sub infix:</>(\nu, \de) is export { old-infix-slash(nu,de).FatRat }
