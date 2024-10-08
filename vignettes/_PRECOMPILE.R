# Pre-compile vignettes which run for a while
library(knitr)
library(readr)
library(stringr)

# Make sure to put figures in local dir in knitr chunk options
v <- list.files("vignettes", ".orig$", full.names = TRUE, recursive = TRUE)

for(i in v) {
  new <- stringr::str_remove(i, ".orig$")
  knit(i, new)
  
  read_lines(new) %>%
    #str_extract("vignettes(/articles)*/")
    str_replace_all("vignettes(/articles)*/", "") %>%
    write_lines(new)
}

cache <- list.files("./vignettes", "cache", include.dirs = TRUE,
                    recursive = TRUE, full.names = TRUE)
unlink(cache, recursive = TRUE)

#build vignette
#devtools::build_vignettes()
#unlink("./doc/", recursive = TRUE)
#unlink("./Meta/", recursive = TRUE)