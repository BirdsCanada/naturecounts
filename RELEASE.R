# Steps/Commands to run before a package release -----------------------------

# Get started --------------------------------------------------

## Change API in data-raw/data_createion.R to non-sandbox

## Run tests and check
devtools::test()

## Update internal data files
source("data-raw/data_creation.R")

## Update metadata stored in inst/extdata (UPDATE URL!)
# - Utm codes take a VERY long time to update!
nc_metadata_internal(force = TRUE, utm = FALSE)

# Documentation -------------------------------------------------
## Update NEWS

## Check spelling
dict <- hunspell::dictionary('en_CA')
devtools::spell_check()
spelling::update_wordlist()

## Update README.Rmd
devtools::build_readme()

# Finalize package version --------------------------------------
## Edit DESCRIPTION and NEWS.md as needed

# Final checks --------------------------------------------------
## Checks
devtools::check(run_dont_test = TRUE)   # Local, run long-running examples

## Windows checks (particularly if submitting to CRAN)
devtools::check_win_release() # Win builder
devtools::check_win_devel()
devtools::check_win_oldrelease()

## Run in console
system("cd ..; R CMD build naturecounts")
system("cd ..; R CMD check naturecounts_0.1.0.tar.gz --as-cran")

## Check GH Actions on GitHub


# Release! ------------------------------------------------------
## Actually release it! Create signed release on GitHub

# Get ready for next cycle --------------------------------------

# - Create sandbox branch
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


