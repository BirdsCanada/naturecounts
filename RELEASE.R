# Steps/Commands to run before a package release -----------------------------

## Update internal data files
source("data-raw/data_creation.R")

## Update metadata stored in inst/extdata
# - Utm codes take a VERY long time to update!
nc_metadata_internal(force = TRUE, utm = FALSE)

## Documentation
# Update NEWS

# Check spelling
dict <- hunspell::dictionary('en_CA')
devtools::spell_check()
spelling::update_wordlist()


## Finalize package version

## Checks
devtools::check(run_dont_test = TRUE)   # Local, run long-running examples

## Windows checks (particularly if submitting to CRAN)
devtools::check_win_release() # Win builder
devtools::check_win_devel()
devtools::check_win_oldrelease()

## Run in console
system("cd ..; R CMD build naturecounts")
system("cd ..; R CMD check naturecounts_0.1.0.tar.gz --as-cran")

## Push to github
## Check travis / appveyor

## Check Reverse Dependencies (are there any?)
#tools::dependsOnPkgs("naturecounts")
#devtools::revdep()

## Build site (so website uses newest version)
pkgdown::build_site(lazy = TRUE)
## Push to github

## Actually release it, create signed release on github
system("git tag -s v0.1.0 -m 'v0.1.0'")
system("git push --tags")
