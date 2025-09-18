FROM rocker/shiny:4.2.2

# Ensure packages are installed system-wide
ENV R_LIBS_SITE=/usr/local/lib/R/site-library

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    nano \
    && rm -rf /var/lib/apt/lists/*

# Copy renv.lock into the container
COPY renv.lock /srv/shiny-server/dashboard/renv.lock

# Install renv and restore exact package versions system-wide
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/'); \
          renv::restore(lockfile='/srv/shiny-server/dashboard/renv.lock', \
                        prompt=FALSE, \
                        repos='https://cloud.r-project.org/')"

# Copy the app into the container
COPY dashboard/ /srv/shiny-server/dashboard/

# Make sure the shiny user owns the app folder
RUN chown -R shiny:shiny /srv/shiny-server/dashboard

# Expose Shiny Server default port
EXPOSE 3838
