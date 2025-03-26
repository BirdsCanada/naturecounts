# Steps/Commands to run before a package release -----------------------------

# Get started --------------------------------------------------

# - Change API in data-raw/data_creation.R to non-sandbox
# - Run tests and check
devtools::test()

# - Update internal data files
source("data-raw/data_creation.R")

# - Update metadata stored in inst/extdata (Check URLS in data-raw/data_creation.R)
# - Utm codes take time to update
nc_metadata_internal(force = TRUE, utm = FALSE)
#nc_metadata_internal(force = TRUE, utm = TRUE)

# Documentation -------------------------------------------------

# - Update NEWS
# - Check spelling
dict <- hunspell::dictionary('en_CA')
devtools::spell_check()
spelling::update_wordlist()

# - Update README.Rmd
devtools::build_readme()

# - Precompile Vignettes - MUST BUILD/INSTALL PACKAGE FIRST!
devtools::install(quick = TRUE, build = TRUE, upgrade = "never") # Build/install
unlink("vignettes/articles/figures/", recursive = TRUE) # Clean up old figs
source("vignettes/_PRECOMPILE.R")

# Finalize package version --------------------------------------

# - Edit DESCRIPTION and NEWS.md as needed

# Final checks --------------------------------------------------

# - Checks
devtools::check(run_dont_test = TRUE)   # Local, run long-running examples

# - Run in console
system("cd ..; R CMD build naturecounts")
system("cd ..; R CMD check naturecounts_0.1.0.tar.gz --as-cran")

# - Check GH Actions on GitHub


# Release! ------------------------------------------------------

# - Merge dev into master/main
# - PULL updates to master/main
# - Actually release it! Create signed release on GitHub
usethis::use_github_release()

# Get ready for next cycle --------------------------------------

# - Create sandbox/dev branch
# - Change API in data-raw/data_creation.R to sandbox
# - Add dev components to version in DESCRIPTION and NEWS.md


# Testing ----------------------

# Test the pkgdown site locally as needed
#
# **BUILD PACKAGE FIRST**  (Ctrl-Shift-B)

pkgdown::build_site(lazy = TRUE) # lazy=TRUE means only rebuild articles/vignettes which have changed
pkgdown::build_home()
pkgdown::build_reference()
pkgdown::build_news()
pkgdown::build_articles(lazy = TRUE)

# Build a specific article
pkgdown::build_article("articles/test")
pkgdown::build_article("species-codes")
pkgdown::build_article("format-zero-fill")
pkgdown::build_article("selecting-fields")
pkgdown::build_article("filtering-data")
pkgdown::build_article("data-access")
pkgdown::build_article("region-codes")
pkgdown::build_article("region-areas")
pkgdown::build_article("articles/region-spatial")
pkgdown::build_article("articles/2.0-TableOfContents")
pkgdown::build_article("articles/2.1-SpatialDataExploration")
pkgdown::build_article("articles/2.2-SpatialSubsets")
pkgdown::build_article("articles/2.3-ClimateData")
pkgdown::build_article("articles/2.4-ElevationData")
pkgdown::build_article("articles/2.5-LandcoverData")
pkgdown::build_article("articles/2.6-SatelliteImagery")
pkgdown::build_article("articles/2.7-SummaryTools")
pkgdown::build_article("articles/3.1-ZeroFilling")
pkgdown::build_article("articles/3.2-AuxiliaryTables")

# Don't push resources
unlink("vignettes/articles/BCR_Terrestrial/", recursive = TRUE)
unlink("vignettes/articles/bcr_terrestrial_shape.zip")
