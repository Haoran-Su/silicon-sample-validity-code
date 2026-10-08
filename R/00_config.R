# Paths used by the archived analysis scripts.
# Set the environment variables before running, or place the files in the default locations.
DG_COMBINED_META <- Sys.getenv("DG_COMBINED_META", file.path(getwd(), "data", "combined_meta.RData"))
DG_OUTPUT_DIR <- Sys.getenv("DG_OUTPUT_DIR", file.path(getwd(), "outputs"))
DG_MCMC_OUTPUT_DIR <- Sys.getenv("DG_MCMC_OUTPUT_DIR", file.path(getwd(), "data", "mcmc_simplified_output"))
dir.create(DG_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
