FROM rocker/tidyverse:3.4.3
RUN apt-get update

RUN apt-get install -y \
  xclip xsel

RUN install2.r \
  clipr \
  clisymbols \
  rlang \
  usethis \
  withr

RUN install2.r \
  DT

RUN installGithub.r \
  'r-lib/devtools' \
  'tidyverse/reprex' \
  'r-lib/lobstr'
