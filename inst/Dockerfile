FROM rocker/tidyverse:3.4.3
RUN apt-get update

RUN apt-get install -y \
  xclip xsel

RUN install2.r \
  cli \
  clipr \
  rlang \
  usethis \
  withr

#RUN installGithub.r \
#  'r-lib/usethis'
