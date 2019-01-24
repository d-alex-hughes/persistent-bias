# Adapted from the Jupyter Development Team
# Keeping their Copyright, though this is edited down 

# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.
ARG BASE_CONTAINER=jupyter/minimal-notebook
FROM $BASE_CONTAINER

LABEL maintainer="Jupyter Project <jupyter@googlegroups.com>"

USER root

# R pre-requisites
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    fonts-dejavu \
    tzdata \
    gfortran \
    gcc \
    r-base && \
    rm -rf /var/lib/apt/lists/*

USER $NB_UID

RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile

RUN chmod -R 777 /usr/local/lib/R/site-library

RUN R -e "install.packages('devtools')"
# RUN R -e "devtools::install_version('irkernel', version = '0.8')"

# RUN R -e "install.packages('irkernel')"

# RUN R -e "devtools::install_version('data.table', version = '1.11')"
# RUN R -e "devtools::install_version('sandwich', version = '2.4')"
# RUN R -e "devtools::install_version('matrix', version = '1.2')"
# RUN R -e "devtools::install_version('lmtest', version = '0.9')"
# RUN R -e "devtools::install_version('magrittr', version = '1.5')"
# RUN R -e "devtools::install_version('survival', version = '2.42')"
# RUN R -e "devtools::install_version('lubridate', version = '1.7')"
# RUN R -e "devtools::install_version('repr', version = '0.15')"
# RUN R -e "devtools::install_version('rmarkdown', version = '1.11')"
# RUN R -e "devtools::install_version('lfe', version = '2.5')"

# R packages
# RUN conda install --quiet --yes \
#     'r-base=3.5.1' \
#     'r-irkernel=0.8*' \
#     'r-data.table=1.11*' \
#     'r-sandwich=2.4*' \
#     'r-matrix=1.2*' \
#     'r-lmtest=0.9*' \
#     'r-magrittr=1.5*' \
#     'r-survival=2.42*' \
#     'r-lubridate=1.7*' \
#     'r-repr=0.15*' \
#     'r-rmarkdown=1.11*' \
#     # 'r-lfe=2.5* ' \
#     && \    
#     conda clean -tipsy && \
#     fix-permissions $CONDA_DIR

# RUN R -e "install.packages('lfe', repos = 'http://cran.us.r-project.org')"