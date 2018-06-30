FROM rocker/tidyverse:3.5.0

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    qpdf \
    xclip \
    xsel && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error \
    clipr \
    clisymbols \
    glue \
    rlang \
    usethis \
    withr && \
  : "Internal use"&& \
  install2.r --error \
    DT \
    spelling && \
  installGithub.r \
    'r-lib/devtools' \
    'r-lib/revdepcheck' \
    'tidyverse/reprex' \
    'r-lib/lobstr'
