FROM aluro2/rstudio-bayes:4.0.2

# Install packages and dependencies for pavo
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    imagemagick \
    libmagick++-dev \
    tcl8.6-dev \
    tk8.6-dev

# Install R packages
RUN R -e "install.packages('devtools', clean = T, Ncpus = 6)"
RUN R -e "devtools::install_version('pavo', version = '2.4.0', repos='http://cran.us.r-project.org', clean = T, Ncpus = 6, quick = T)" 
