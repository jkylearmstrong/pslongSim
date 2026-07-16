# Multi-stage Dockerfile optimized for R packages with tidymodels ecosystem
# Follows R + Docker best practices: caching, minimal runtime, binary packages

FROM rocker/r-ver:4.4.0 AS builder

WORKDIR /build

# Install compile-time system dependencies (needed for source packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    gfortran \
    libgsl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy only metadata files first (cache breaking strategy)
COPY DESCRIPTION NAMESPACE .

# Pre-install tidymodels ecosystem and core dependencies from RSPM binaries
# This drastically speeds up builds and avoids compilation on every rebuild
RUN R --vanilla --quiet -e "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/cran/__linux__/jammy/latest')); \
    pkgs <- c('recipes', 'parsnip', 'workflows', 'rsample', 'tune', 'yardstick', 'dials', 'geepack', 'survival'); \
    install.packages(pkgs, dependencies = FALSE, Ncpus = 4L)"

# Copy renv files for reproducibility
COPY renv.lock renv.lock
COPY renv/ renv/

# Restore remaining dependencies from renv.lock (ensures exact versions)
RUN R --vanilla --quiet -e "renv::restore(prompt = FALSE)"

# Copy complete source code
COPY . .

# Install local package
RUN R CMD INSTALL --no-test-load .

# Runtime stage: minimal footprint without build tools
FROM rocker/r-ver:4.4.0

WORKDIR /workspace

# Install only runtime libraries (no dev tools)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2 \
    libssl3 \
    libcurl4 \
    libgsl27 \
    && rm -rf /var/lib/apt/lists/*

# Copy installed packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy package source and data
COPY --from=builder /build /workspace

# Default: R interactive session for manual testing
CMD ["R", "--vanilla"]
