.onLoad <- function(libname, pkgname) {
    ## silly hack to get around a catch-22 in the namespace code
    ##
    ## loading the methods package with library(methods) (on R 2.15)
    ## produces the NOTE:
    ##
    ## Package startup functions should not change the search path
    ##
    ## removing the loading of methods causes a different error:
    ##    Error: .onLoad failed in loadNamespace() for 'fishplot', details:
    ##    call: initRdClass()
    ##    error: could not find function "setClass"
    ##    Execution halted
    ##
    ## so we disguise the call (based on code in r-bioc-iranges)
    ## there is probably a way to change the namespace to fix this in
    ## a more "correct" way.

    sillyname <- library
    sillyname(methods)
    initFishClass()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Using fishPlot version 0.5")
}
