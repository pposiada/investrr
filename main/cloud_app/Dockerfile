FROM rocker/r-ver:4.3.2

# Install system dependencies required for tidyquant and googleCloudStorageR
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'shinycssloaders', 'quantmod', 'dplyr', 'tidyquant', 'tidyr', 'googleCloudStorageR'), repos='https://cloud.r-project.org/')"

# Copy app code
COPY app.R /app/app.R

WORKDIR /app

# Expose port required by Cloud Run
EXPOSE 8080

# Run the Shiny App on the port specified by Cloud Run (or default to 8080)
CMD R -e "shiny::runApp('/app/app.R', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))"
