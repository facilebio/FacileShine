# Previous base image:
# FROM bioconductor/bioconductor_docker:RELEASE_3_22
FROM facilebio/facilebio:latest

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    g++ \
    gfortran \
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libnode-dev \
    libpng-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    libxt-dev \
    make \
    pandoc \
    python3-pip \
    python3-venv \
    zlib1g-dev \
    && python3 -m venv /opt/radian \
    && /opt/radian/bin/pip install --no-cache-dir radian \
    && ln -sf /opt/radian/bin/radian /usr/local/bin/radian \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Previous global pip install, which now fails in externally managed Python environments:
#     && pip3 install --no-cache-dir radian \

# USER rstudio
