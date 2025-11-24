#!/bin/bash

set -e

export CURL_CA_BUNDLE="${CONDA_PREFIX}/ssl/cacert.pem"

# Install missing R pacakges
Rscript -e 'install.packages(c("dcurves", "halfmoon"), repos="https://cloud.r-project.org/")'