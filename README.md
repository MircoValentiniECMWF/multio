ifs-bundle
==========

Build-system for IFS based on ecbuild, which bundles all ECMWF-dependencies
inside one large project, saving the extra work to make sure all dependencies
are in place.

Basic instructions
------------------

    # Clone this bundle
    git clone -b <THIS_BRANCH> ssh://git@git.ecmwf.int/IFS/ifs-bundle.git
    cd ifs-bundle

    # Download and create bundle
    ./ifs-bundle create

    # Configure and compile bundle
    ./ifs-bundle build

Partial matches are accepted, e.g.

    ./ifs-bundle bui
    ./ifs-bundle cr

Advanced configuration
----------------------

Several environment variables can impact the ecbundle-create step only.

    export IFS_BUNDLE_<PROJECT>_GIT=...
    export IFS_BUNDLE_<PROJECT>_VERSION=...
    export IFS_BUNDLE_<PROJECT>_CMAKE=...
    export IFS_BUNDLE_SKIP_<PROJECT>=1

where project is the uppercase name of each project, with '-' replaced with '_'.

For example, to override the version of ifs-source:

    export IFS_BUNDLE_IFS_SOURCE_VERSION=CY43R3

For example, to override the repository of ifs-source:

    export IFS_BUNDLE_IFS_SOURCE_GIT='${BITBUCKET}/~nawd/ifs-source'


Please do check the following commands for all available options

    ./ifs-bundle create --help
    ./ifs-bundle build  --help

