#!/usr/bin/env bash

DOCKER_NAME=hatmatrix/kaggle:latest

docker build --tag $DOCKER_NAME --build-arg REBUILD=$(date +%s) .
