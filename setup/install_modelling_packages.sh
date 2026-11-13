#!/usr/bin/env bash
# Modelling packages: run once in a session when modelling work starts (~15 min,
# gllvm compiles from source). Not part of the environment's startup script.
#   bash setup/install_modelling_packages.sh
set -e
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq r-cran-ggrepel r-cran-lme4 r-cran-glmnet r-cran-brms r-cran-rstan \
  r-cran-jsonlite r-cran-gridextra r-cran-terra libboost-dev
Rscript -e 'p <- c("targets", "tarchetypes", "gllvm", "ranger", "statmod", "bayesplot")' \
  -e 'p <- p[!vapply(p, requireNamespace, TRUE, quietly = TRUE)]' \
  -e 'if (length(p)) install.packages(p, repos = "https://cloud.r-project.org", lib = "/usr/local/lib/R/site-library")'
if ! Rscript -e 'quit(status = !requireNamespace("MuMIn", quietly = TRUE))'; then
  curl -sSfL -o /tmp/MuMIn_1.48.4.tar.gz https://cloud.r-project.org/src/contrib/Archive/MuMIn/MuMIn_1.48.4.tar.gz
  R CMD INSTALL -l /usr/local/lib/R/site-library /tmp/MuMIn_1.48.4.tar.gz
fi
