FROM openanalytics/r-ver:4.1.3

LABEL maintainer="Juan Venegas <juan@dataidea.cl>"

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libglpk-dev \
    libxml2 \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -q -e 'install.packages(c(\
              "shiny", \
              "bs4Dash", \
              "igraph", \
              "highcharter", \
              "dplyr", \
              "viridisLite", \
              "moments", \
              "shinycssloaders", \
              "shiny.i18n", \
              "scales" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2023-01-13"\
          )'

#RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# install dependencies of the euler app
#RUN R -q -e "install.packages('gmp')"
#RUN R -q -e "install.packages('Rmpfr')"

# copy the app to the image
RUN mkdir /root/app_dash_hbv
COPY app_dash_hbv /root/app_dash_hbv

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/root/app_dash_hbv')"]

