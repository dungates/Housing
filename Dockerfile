# get shiny serves plus tidyverse packages image
FROM rocker/shiny:4.0.5
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
# install R packages required 
# (change it depending on the packages you need)
RUN R -e "install.packages('bslib', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('dungates/DGThemes')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggmap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpmisc', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gt', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('scales', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('thematic', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
# copy the app to the image
COPY app.R /srv/shiny-server/
COPY post.html /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY www /srv/shiny-server/www
COPY Data /srv/shiny-server/data
# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
# run app
CMD ["/usr/bin/shiny-server.sh"]