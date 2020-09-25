FROM rocker/tidyverse:4.0.2

# Install packages and dependencies 
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    imagemagick \
    libmagick++-dev \
    tcl8.6-dev \
    tk8.6-dev \
    libicu-dev \
    pandoc-citeproc \
    pandoc \
    make \
    libv8-dev \
    libglpk-dev \
    libgmp3-dev 

# Install R packages

RUN R -e "devtools::install_version('pavo', version = '2.4.0', repos='https://packagemanager.rstudio.com/all/latest', clean = T, Ncpus = 6, quick = T)" 

RUN R -e "devtools::install_version('brms', version = '2.13.0', repos='https://packagemanager.rstudio.com/all/latest', clean = T, Ncpus = 6, quick = T)"

RUN R -e "devtools::install_version('tidybayes', version = '2.1.1', repos='https://packagemanager.rstudio.com/all/latest', clean = T, Ncpus = 6, quick = T)"