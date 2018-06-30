FROM rocker/tidyverse:3.5.0
RUN apt-get update

RUN apt-get install -y \
  xclip xsel

RUN install2.r \
  clipr \
  clisymbols \
  glue \
  rlang \
  usethis \
  withr

RUN install2.r \
  DT \
  spelling

RUN installGithub.r \
  'r-lib/devtools' \
  'tidyverse/reprex' \
  'r-lib/lobstr'
