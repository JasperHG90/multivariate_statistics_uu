# Dockerfile for the multivariate statistics practical
FROM rocker/rstudio

MAINTAINER Jasper Ginn "jasperginn@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libgit2-dev \
    libpq-dev \
    libxml2-dev
    
# install R packages
RUN R -e "install.packages('Rcpp', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('plotly', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('caret', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('e1071', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('nnet', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('knitr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rmarkdown', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('caTools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('bitops', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rprojroot', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rattle.data', repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('JasperHG90/mnlr')"

# Copy application
RUN mkdir /home/rstudio/practical
COPY multinomial_regression_practical.Rmd /home/rstudio/practical
COPY img /home/rstudio/practical/img

# Permissions
RUN chown -R rstudio /home/rstudio/practical