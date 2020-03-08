FROM rocker/shiny-verse:latest

ADD . /attrakttv

RUN R -e "devtools::install('/attrakttv')"
RUN rm -r /srv/shiny-server
RUN ln -s /usr/local/lib/R/site-library/attrakttv/app /srv/shiny-server

EXPOSE 3838
