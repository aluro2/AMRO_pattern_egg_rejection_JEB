#!/usr/bin/env bash

echo "Building docker image"

# Build docker image using docker buildkit
DOCKER_BUILDKIT=1 docker build -t amro_pattern_egg_rejection_rstudio .

echo "amro_pattern_egg_rejection_rstudio Rstudio image ready!"

