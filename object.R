##-------------------------------------------------
## Set up an object to hold the data
##
initFishClass <- function(){
  setClass("fishObject", representation(ytop="list", ybtm="list", xpos="list",
                                        colors="character",
                                        labels="character", timepoints="numeric",
                                        frac.table="matrix", parents="numeric",
                                        nest.level="numeric", free.space="list"))
}
