.onAttach <- function(libname, pkgname) {
  if ( ! require(ggformula))
      packageStartupMessage("Install ggformula from github: `projectMosaic/ggformula`")
}
