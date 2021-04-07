# Static libraries scripts

Static libraries are built using an fPIC GHC created with [these](https://gitlab.com/Concordium/devops/-/tree/master/fpic) scripts by [this](http://jenkins.internal.concordium.com/job/fpic-ghc_jenkinsfile/) job.

These scripts are consumed by the job created with `jenkinsfiles/static-libraries.Jenkinsfile` [here](http://jenkins.internal.concordium.com/job/static-libraries) (internal).

The main script is `build-and-push-static-libs.sh` which creates a docker container in which it builds the static libraries and then pushes them to S3 named after the commit of the repository from which the job was run.
The file `LATEST_STATIC_LIBRARIES` should be updated with this hash after the build is done.

This file is later consumed by `download-static-libs.sh` to correctly place the static libraries, enabling compilation of the node with `static` or `profiling` features.

:warning: WARNING:

This pipeline can only be run in non push-protected branches, so this excludes `master` and `develop`. If you want to use the pipeline you will have to create a new branch and run the script there.
