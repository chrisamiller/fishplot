function <- test.fixDisappearingClones(){
  timepoints=c(0,34,69)
  parents = c(0,1,1)
  nest.level=c(0,1,1)
  
  frac.table = matrix(
    c(99, 30,   20,
      5,   0,   0,
      80,  0,   10),
    ncol=length(timepoints))

  expected.result = matrix(
    c(99, 30,   20,
      5,   0,   0,
      80,  0.01,   10),
    ncol=length(timepoints))

  frac.table=fixDisappearingClones(frac.table,nest.level)
  if(!identica(frac.table,expected)){
    stop("ERROR: fixDisappearingClones not working as expected")
  }
}
