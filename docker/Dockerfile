FROM rocker/tidyverse:4.2.0

LABEL maintainer="Rahul Sangole <rsangole@apple.com>"

# WORKDIR /renv
# COPY renv.lock renv.lock
# RUN Rscript -e 'Sys.setenv("ARROW_WITH_SNAPPY"="ON"); renv::restore()'
# WORKDIR /

ENV RENV_VERSION 0.15.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN apt-get update && apt-get install -y --no-install-recommends \
	libglpk40 \
	htop \
	jq \
	libxtst6 \
	libxt6 \
	python3-pip && \
	pip install -U radian && \
	ln -s /usr/local/lib/R/lib/libR.so /lib/x86_64-linux-gnu/libR.so

RUN mkdir -p /renv/cache
