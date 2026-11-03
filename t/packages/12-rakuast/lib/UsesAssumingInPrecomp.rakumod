# Regression fixture for https://github.com/rakudo/rakudo/issues/6169
# Loading this module triggers precomp of itself plus AssumingInPrecomp,
# which is the precise topology that surfaced the bug.
use AssumingInPrecomp;
