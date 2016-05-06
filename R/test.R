library(fishplot)

testNew <- function(){

  timepoints=c(0,30,75,150)
  
  frac.table = matrix(
    c(100, 45, 00, 00,
       02, 00, 00, 00,
       02, 00, 02, 01,
       98, 00, 95, 40),
    ncol=length(timepoints))

  parents = c(0,1,1,3)
  
  
  fish = createFishObject(frac.table,parents,timepoints=timepoints)
  fish = layoutClust(fish)

  sample.times = c(0,150)

  drawPlot(fish,shape="spline",title.btm="633734",
           vlines=sample.times, vlab=sample.times, cex.title=0.5)

}

################################################

print("test new")
pdf("testnew.pdf",width=8,height=4)
testNew()
dev.off()
