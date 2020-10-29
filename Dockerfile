FROM aluro2/rstudio-visual-modeling:4.0.2

# Install packages and dependencies 
# Install packages and dependencies for pavo, raster and letsR packages
RUN apt-get update && apt-get install -y \
    pandoc-citeproc \
    pandoc \
    # Extra dependencies for igraph R package
    libxml2-dev \
    libv8-dev \
    libglpk-dev \
    libgmp3-dev 

# Install R packages

RUN install2.r -s --error -r "https://packagemanager.rstudio.com/all/latest" --ncpus 8 \
 rstan \
 rstanarm \
 brms \
 bayestestR \
 tidybayes