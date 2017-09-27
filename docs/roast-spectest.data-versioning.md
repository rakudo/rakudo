The files `t/spectest.data*` specify the list of files to use to test a
specific roast version. The version to test is obtained from the VERSION
file in roast checkout (`t/spec/VERSION`).

The default file is `t/spectest.data` and it's used is roast version could not
be obtained or if the version matches string `propo` (e.g. `6.d.proposal`).
Otherwise, the version is used as a suffix, separated with a dot:

    VERSION file contains "6.c" => tests read from t/spectest.data.6.c

The master roast branch would typically contain a proposal version (`6.d.proposal`).
Once that language version is released and a new branch with it is published, the
VERSION file will be changed (`6.d`) and a new spectest.data file will be created
(`t/spectest.data.6.d`).
