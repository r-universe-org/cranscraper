FROM ubuntu:noble

ENV DEBIAN_FRONTEND=noninteractive

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN \
	apt-get update && \
	apt-get -y dist-upgrade && \
	apt-get install -y r-base-core language-pack-en-base git gcc libcurl4-openssl-dev libssl-dev libgit2-dev

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN \
	R -e 'install.packages("remotes"); remotes::install_local("/pkg")'

# For debugging CI errors:
RUN \
	R -e 'install.packages("rlang")'  && \
	echo 'options(error=rlang::entrace)' >> "/etc/R/Rprofile.site"

ENTRYPOINT ["/entrypoint.sh"]

