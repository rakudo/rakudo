Binary release pipeline
=======================

The files in this directory comprise the binary release pipeline which is used
to generate the pre-compiled release archives and installers provided on
<rakudo.org>. The Star Bundle is handled separately though.

The pipeline is meant to be run on Azure Pipelines. It is triggered via the
`azure-pipelines.yml` file at the top level of this repo. The details of how to
do an actual build and how to put it on <rakudo.org> is documented in
`docs/release-guide-binary.md`.