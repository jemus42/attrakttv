# attrakttv Shiny app container.
# See vault: personal/projects/attrakttv/specs/2026-05-30-deployment-design.md
FROM rocker/r-ver:4.6.0

# System libs needed for the native code in our R deps.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install pak (fast dependency resolver) from r-lib's binary repo.
RUN R -q -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform\$pkgType, R.Version()\$os, R.Version()\$arch))"

WORKDIR /pkg

# Layer 1: deps from DESCRIPTION only. This layer stays cached across
# rebuilds as long as DESCRIPTION (and Remotes) don't change.
COPY DESCRIPTION ./
RUN R -q -e "pak::local_install_deps(ask = FALSE, dependencies = TRUE, upgrade = FALSE)"

# Layer 2: the package source itself.
COPY . /pkg
RUN R -q -e "pak::local_install('.', ask = FALSE, upgrade = FALSE)"

# Non-root runtime user. /data is the bind-mount target for the SQLite cache.
# UID/GID 1000 is the Debian default and matches the typical host operator
# UID on horst. Override via --build-arg if the host UID differs:
#   docker compose build --build-arg APP_UID=$(id -u) --build-arg APP_GID=$(id -g)
ARG APP_UID=1000
ARG APP_GID=1000
RUN groupadd --gid ${APP_GID} attrakttv \
    && useradd --uid ${APP_UID} --gid ${APP_GID} --create-home --shell /bin/bash attrakttv \
    && mkdir -p /data \
    && chown attrakttv:attrakttv /data
USER attrakttv
WORKDIR /home/attrakttv

ENV PORT=3838 \
    HOST=0.0.0.0 \
    trakt_db_path=/data

EXPOSE 3838

CMD ["Rscript", "-e", "attrakttv::attrakttv_app(host = Sys.getenv('HOST', unset = '0.0.0.0'), port = as.integer(Sys.getenv('PORT', unset = '3838')), launch.browser = FALSE)"]
