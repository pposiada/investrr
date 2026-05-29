FROM rocker/r-ver:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libglpk-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN packages
RUN R -e "install.packages(c( \
    'tidyverse', \
    'tidyquant', \
    'PerformanceAnalytics', \
    'PortfolioAnalytics', \
    'remotes', \
    'ROI', \
    'ROI.plugin.glpk', \
    'ROI.plugin.quadprog', \
    'DEoptim', \
    'pso', \
    'shiny', \
    'shinydashboard', \
    'DT', \
    'shinycssloaders', \
    'googleCloudStorageR' \
), repos='https://cloud.r-project.org/')"

# Install TradeAnalytics packages from GitHub
RUN R -e "remotes::install_github('braverock/FinancialInstrument')"
RUN R -e "remotes::install_github('braverock/blotter')"
RUN R -e "remotes::install_github('braverock/quantstrat')"

WORKDIR /app

EXPOSE 8080
