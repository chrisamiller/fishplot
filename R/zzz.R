.onLoad <- function(libname, pkgname) {
    ## silly hack to get around a catch-22 in the namespace code
    ##
    ## loading the methods package with library(methods) (on R 2.15)
    ## produces the NOTE:
    ##
    ## Package startup functions should not change the search path
    ##
    ## removing the loading of methods causes a different error:
    ##    Error: .onLoad failed in loadNamespace() for 'copyCat', details:
    ##    call: initRdClass()
    ##    error: could not find function "setClass"
    ##    Execution halted
    ##
    ## so we disguise the call (based on code in r-bioc-iranges)

    sillyname <- library
    sillyname(methods)
    initFishClass()
    packageStartupMessage("Using fishPlot version 0.1")
}

