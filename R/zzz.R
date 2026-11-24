# R/zzz.R

warn_deprecated_data <- function(
  old,
  new,
  when,
  user_env = rlang::caller_env()
) {
  lifecycle::deprecate_warn(
    when = when,
    what = I(paste0("The `", old, "` dataset")),
    with = I(paste0("`", new, "`")),
    env = rlang::current_env(),
    user_env = user_env
  )
}

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  ld <- .getNamespaceInfo(ns, "lazydata")

  delayedAssign(
    "engagement",
    {
      warn_deprecated_data("engagement", "port_activity", when = "10.1.0")
      readRDS(system.file("extdata", "engagement.rds", package = "ecodata"))
    },
    eval.env = ns,
    assign.env = ld
  )
}
