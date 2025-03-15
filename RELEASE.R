# Steps/Commands to run before a package release -----------------------------

# Get started --------------------------------------------------

# - Change API in data-raw/data_creation.R to non-sandbox
# - Run tests and check
devtools::test()

# - Update internal data files
source("data-raw/data_internal.R")
source("data-raw/data_example.R")
source("data-raw/data_test.R")

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
# Can build site locally if needed
## **BUILD PACKAGE FIRST**
pkgdown::build_site(lazy = TRUE)
pkgdown::build_home()
pkgdown::build_reference()
pkgdown::build_news()
pkgdown::build_articles(lazy = TRUE)

pkgdown::build_article("species-codes")
pkgdown::build_article("format-zero-fill")
pkgdown::build_article("selecting-fields")
pkgdown::build_article("filtering-data")
pkgdown::build_article("data-access")
pkgdown::build_article("region-codes")
pkgdown::build_article("region-areas")
pkgdown::build_article("articles/region-spatial")

unlink("vignettes/articles/BCR_Terrestrial/", recursive = TRUE)
unlink("vignettes/articles/bcr_terrestrial_shape.zip")
