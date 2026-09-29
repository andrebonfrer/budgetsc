#' budgetsc: reproducible pipeline for the household budgeting study
#'
#' Layers (see the design sketch): spec (what to run) -> data -> sample -> sc
#' -> post -> summarise -> analyses, plus a jobs layer for several machines on
#' one filesystem. Phase 0 provides timeline, spec, registry, locks/jobs and
#' the synthetic panel; model stages follow.
#'
#' @section Options:
#' \describe{
#'   \item{`budgetsc.root`}{Project root holding `Processed/` and `Runs/`.}
#'   \item{`budgetsc.timeline`}{Path to a timeline YAML overriding the installed one.}
#' }
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table := .N .SD data.table setorder setnames fifelse frollapply frollmean %between% uniqueN
NULL
