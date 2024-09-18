FROM rocker/shiny:4.3.2 as base

RUN R -e "install.packages('remotes')"
COPY install.R requirements.txt ./
RUN Rscript install.R

FROM base

RUN rm -rf /srv/shiny-server/*
COPY app.R /srv/shiny-server/farm/
COPY src/ /srv/shiny-server/farm/src/
