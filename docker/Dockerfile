FROM rocker/tidyverse:4.2.0

LABEL maintainer="Rahul Sangole <rsangole@apple.com>"

ENV RENV_VERSION 0.15.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN apt-get update && apt-get install -y --no-install-recommends \
	libglpk40 \
	htop \
	jq \
	libxtst6 \
	libxt6 \
	xdg-utils \
	python3-pip && \
	pip install -U radian && \
	ln -s /usr/local/lib/R/lib/libR.so /lib/x86_64-linux-gnu/libR.so

RUN mkdir -p /renv/cache

RUN Rscript -e "devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.1/catboost-R-Linux-1.1.tgz', INSTALL_opts = c('--no-multiarch', '--no-test-load'))"
RUN Rscript -e "devtools::install_github('mlr-org/mlr3extralearners')"
