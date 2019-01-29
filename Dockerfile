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
    curl         \
    fonts-dejavu \
    gcc          \    
    gfortran     \
    openssl      \
    r-base       \ 
    tzdata       \
    zlib1g-dev   \
     &&          \
    rm -rf /var/lib/apt/lists/*

USER $NB_UID

# R packages
RUN conda install --quiet --yes \
    'r-base=3.5.1' \
    'r-irkernel=0.8*' \
    'r-data.table=1.11*' \
    'r-sandwich=2.4*' \
    'r-matrix=1.2*' \
    'r-lmtest=0.9*' \
    'r-magrittr=1.5*' \
    'r-survival=2.42*' \
    'r-lubridate=1.7*' \
    'r-rmarkdown=1.11*' \
    && \    
    conda clean -tipsy && \
    fix-permissions $CONDA_DIR

RUN R -e "install.packages('lfe', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('stargazer', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('interflex', repos = 'http://cran.us.r-project.org')" 

USER root

COPY . .

RUN chmod 777 -R ./

USER $NB_UID