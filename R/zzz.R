# Dealing with CRAN Notes due to Non-standard evaluation
.onLoad <- function(libname = find.package("rNatureCounts"),
                    pkgname = "rNatureCounts"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid CRAN warnings
      c(".")  # piping requires '.' at times
    )

  invisible()
}