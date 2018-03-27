FROM rocker/tidyverse:3.4.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  software-properties-common xclip xsel \
  libudunits2-dev libproj-dev libgeos-dev libgdal-dev \
  && rm -rf /var/cache/apt/archives/* /var/lib/apt/lists/*

RUN install2.r \
  clipr \
  clisymbols \
  glue \
  rlang \
  sf \
  usethis \
  withr

RUN install2.r \
  DT \
  spelling

RUN installGithub.r \
  'r-lib/devtools' \
  'tidyverse/reprex' \
  'r-lib/lobstr' \
  'uribo/sealr'
