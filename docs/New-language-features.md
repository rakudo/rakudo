# New language versions

This is intended to be a guide or checklist for creating a
new language version, adding new features in an
already-established future language version, and deprecating
and removing obsolete features..

For general information on Rakudo language versioning
see [Language versions and proposals](./language_versions.md).

NOTE: This is a WIP (work in progress) and sections that contributors believe
need additional contributions by domain experts should
be marked with TBE (to be extended) or some other comment
to that effect.

## Adding the tooling structure to start a future version

    TO BE EXTENDED

## Deprecating features

    TO BE EXTENDED

## Adding new features for a future version

For an example we use the upcoming new version '6.e' for which the
tooling structure is in place. New entries will be in the following
directories and files:

*   ./src/core.e/
*   ./t/02-rakudo/03-corekeys.t
*   ./t/02-rakudo/03-corekeys-6e.t
*   ./t/02-rakudo/04-settingkeys-6e.t
*   ./t/02-rakudo/07-implementation-detail-6e.t

### Adding a new class

*   create the file for the new class in directory './src/core.e/'
*   ...MORE TO BE ADDED

### Adding a new routine to an existing class

*   copy the class file from its most recent version into directory ./src/core.e/ if need be
*   add the new names (symbols) to the corekeys and settingkeys files listed above
*   ...MORE TO BE ADDED

