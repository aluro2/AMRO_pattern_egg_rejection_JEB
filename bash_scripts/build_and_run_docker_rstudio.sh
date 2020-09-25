#!/usr/bin/env bash

echo "Starting docker image build (visual_modeling_rstudio)"

bash bash_scripts/docker_build.sh

echo "Run RStudio docker container"

bash bash_scripts/docker_run_rstudio.sh
