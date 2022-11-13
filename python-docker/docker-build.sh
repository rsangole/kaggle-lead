#!/usr/bin/env bash

DOCKER_NAME=hatmatrix/kaggle-python:latest

docker build --tag $DOCKER_NAME --build-arg REBUILD=$(date +%s) .

docker push $DOCKER_NAME
