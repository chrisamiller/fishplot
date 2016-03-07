

##test data
## parents = c(0,1,1,3,0)
## names(parents) = seq(1:5)

## nestlevel = c(0,1,1,2,0)

## frac.table=matrix(c(98,50,48,6,2, 95,0,75,40,5, 95,0,95,40,5),ncol=3)
## rownames(frac.table) = seq(1:5)
## colnames(frac.table)= seq(1:3)

##------------------------------------------
##test1
test1 <- function(){

  parents = c(0,1,1,3,0,4)
  names(parents) = seq(1:6)
  
  nestlevel = c(0,1,1,2,0,3) #add function to calc this
  
  frac.table=matrix(c(98,40,48,6,2,0, 95,0,75,40,5,5, 95,0,95,40,5,10),ncol=3)
  rownames(frac.table) = seq(1:6)
  colnames(frac.table)= seq(1:3)

  
  fish = new("fishObject", ytop=list(), ybtm=list(), colors=c("NULL"),
    labels=c("NULL"), timepoints=c(1:ncol(frac.table)), frac.table=frac.table,
    parents=parents, nest.level=nestlevel, inner.space=list(), outer.space=c(0))
  
  
  fish = layoutClust(fish)
  fish@colors=c("grey50","darkgreen","darkred","orange","purple","yellow","cyan")
  
  fish@labels=c("t1","t2","t3")
  
  #drawPlot(fish,shape="polygon",vlines=fish@timepoints)
                                        #drawPlot(fish,shape="bezier")
  drawPlot(fish,shape="spline",vlines=fish@timepoints)
}


##------------------------------------------
##test2 - AML31 original
test2 <- function(){

  parents = c(0,1,1,1,3,4,0)
  names(parents) = seq(1:7)

  nestlevel = c(0,1,1,1,2,2,0) #add function to calc this

  frac.table = matrix( c(99, 60, 2,   30, 0,   2, 1,
    30, 10, 0.1, 15, 0,   1, 1,
    1,  0,  0.1, 0,  0,   0, 1,
    3,  0,  2.5, 0,  0,   0, 1,
    1,  0,  0.9, 0,  0,   0, 10,
    3,  0,  0.9, 0,  0.1, 0, 20,
    80, 0,  76,  0,  60,  0, 15),
    ncol=7)

  rownames(frac.table) = seq(1:7)
  colnames(frac.table)= c(0,14,34,63,187,334,505)

  fish = new("fishObject", ytop=list(), ybtm=list(), colors=c("NULL"),
    labels=c("NULL"), timepoints=as.numeric(colnames(frac.table)), frac.table=frac.table,
    parents=parents, nest.level=nestlevel, inner.space=list(),outer.space=c(0))


  fish = layoutClust(fish)

  fish@colors=c("grey50","darkgreen","darkred","orange","purple","yellow","cyan")
  fish@labels=c("0","14","34","69","187","334","505")

  ##par(mfrow=c(2,1))
  ##drawPlot(fish,shape="polygon",vlines=fish@timepoints)
  ##drawPlot(fish,shape="bezier")#,pad.left=100)
  drawPlot(fish,shape="spline",vlines=fish@timepoints,vlab=c("0","14","34","69","187","334","505"))

}

##------------------------------------------
##test3 - AML31 with post-allo
test3 <- function(){

  frac.table = matrix( c(99,  60, 2,     30, 0,     2, 1,  0,
    30,  10, 0.1,   15, 0,     1, 1,  0,
    1,   0,  0.1,   0,  0,     0, 1,  0,
    3,   0,  2.5,   0,  0,     0, 1,  0,
    1,   0,  0.9,   0,  0,     0, 10, 0,
    3,   0,  0.9,   0,  0.1,   0, 20, 0,
    80,  0,  76,    0,  60,    0, 15, 0,
    0.1, 0,  0.005, 0,  0.001, 0, 0,  0,
    0.1, 0,  0.005, 0,  0.001, 0, 0,  0.005,
    99,  0,  98,    0,  0,     0, 0,  95), 
    ncol=10)

  parents = c(0,1,1,1,3,4,0,3)
  names(parents) = seq(1:8)
  nestlevel = c(0,1,1,1,2,2,0,2) #add function to calc this

  rownames(frac.table) = seq(1:8)
  colnames(frac.table)= c(0,14,34,63,187,334,505,530,1000,1200)


  fish = new("fishObject", ytop=list(), ybtm=list(), colors=c("NULL"),
    labels=c("NULL"), timepoints=as.numeric(colnames(frac.table)), frac.table=frac.table,
    parents=parents, nest.level=nestlevel, inner.space=list(),outer.space=c(0))

  fish = layoutClust(fish)

  fish@colors=c("grey50","darkgreen","darkred","darkorange","purple","yellow","cyan","slateblue4")
  fish@labels=c("0","14","34","69","187","334","505","post-allo")

  ## par(mfrow=c(2,1))
  ## drawPlot(fish,shape="polygon",vlines=fish@timepoints[c(1:8,10)],
  ##          vlab=fish@timepoints[c(1:8,10)])
  ## drawPlot(fish,shape="bezier",vlines=fish@timepoints[c(1:8,10)])
  drawPlot(fish,shape="spline",vlines=fish@timepoints[c(1:8,10)],
           vlab=fish@timepoints[c(1:8,10)])


  #also test while separating independent clone
  fish = layoutClust(fish, separateIndependentClones=TRUE)
  
  drawPlot(fish,shape="spline",vlines=fish@timepoints[c(1:8,10)],
           vlab=fish@timepoints[c(1:8,10)])
}





source("zzz.R")
source("object.R")
source("layout.R")
.onLoad()

################################################
print("test 1")
pdf("test.pdf",width=5,height=4)
test1()
dev.off()

print("test 2")
pdf("aml31.relapse1.pdf",width=10,height=5)
test2()
dev.off()

print("test 3")
pdf("aml31.full.pdf",width=15,height=5)
test3()
dev.off()

system("pdftk test.pdf aml31.relapse1.pdf aml31.full.pdf cat output all.pdf")
