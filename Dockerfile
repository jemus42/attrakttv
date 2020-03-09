FROM jemus42/attrakttv-base:init
#FROM rocker/shiny-verse:latest

# Add the package directory to the container
ADD . /attrakttv

# Install the app locally
RUN R -e "devtools::install('/attrakttv')"

# Clunkily make the shiny server use the packaged app in the package install dir
RUN rm -r /srv/shiny-server
RUN ln -s /usr/local/lib/R/site-library/attrakttv/app /srv/shiny-server

# Copy secrets to server directory
COPY --chown=shiny:shiny attrakttv.env /srv/shiny-server/.Renviron
COPY --chown=shiny:shiny .httr-oauth /srv/shiny-server/.httr-oauth

# RUN chown -R shiny:shiny /home/shiny/

# Expose the shiny server port
EXPOSE 3838
