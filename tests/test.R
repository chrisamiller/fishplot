source("~/fishplot/R/draw.R")
source("~/fishplot/R/object.R")
initFishClass()

#simple example
timepoints=c(0,30,75,150)

frac.table = matrix(
  c(100, 45, 00, 00,
    02, 00, 00, 00,
    02, 00, 02, 01,
    98, 00, 95, 40),
  ncol=length(timepoints))

parents = c(0,1,1,3)

fish = createFishObject(frac.table,parents,
                        timepoints=timepoints, 
                        clone.labels=c("Founding", "Subclone 1","Subclone 2","Subclone 3"))
fish = layoutClones(fish)

sample.times = c(0,150)

pdf("test.out.pdf",width=8,height=4)
fishPlot(fish,shape="spline",title.btm="633734",
         vlines=sample.times, vlab=sample.times, cex.title=0.5, bg.type="solid")
drawLegend(fish)
dev <- dev.off()

#recreate figure one from the manuscript

pdf("figure1.pdf",height=6,width=8)

layout(mat=t(matrix(c(1,2,3,3),nrow=2)), widths=c(1,1), heights=c(1,1.5))

##-------------------------------------------
##panel A
timepoints=c(0,30,200,423)
parents = c(0,1,1,3)
frac.table = matrix(
  c(100, 38, 24, 00,
    002, 00, 01, 00,
    002, 00, 01, 01,
    100, 00, 98, 30),
  ncol=length(timepoints))

fish = createFishObject(frac.table,parents,timepoints=timepoints)
fish = layoutClones(fish)
fishPlot(fish, shape="spline", vlines=c(0,423), vlab=c(0,423), title="Sample 150288", cex.title=0.9, cex.vlab=0.8)
##panel label
text(-100,130,"A",xpd=T,font=2)

##-------------------------------------------
##panel B
parents = c(0,1,2,1)
timepoints=c(0,120)
frac.table = matrix(
  c(100, 98, 46, 01,
    100, 44, 01, 55),
  ncol=length(timepoints))

fish = createFishObject(frac.table,parents,timepoints=timepoints)
fish = layoutClones(fish)
fish = setCol(fish,col=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

fishPlot(fish, shape="polygon", vlab=c("Primary","Post-AI"), vlines=c(0,120), title.btm="BRC32", cex.title=0.7, cex.vlab=0.8,ramp.angle=1, pad.left=0.3)
##panel label
text(-50,130,"B",xpd=T,font=2)

##-------------------------------------------
##panel C

timepoints=c(0,34,69,187,334,505,530)
parents = c(0,1,1,1,3,4,0)
frac.table = matrix(
  c(99, 30,     2, 60, 0,     2, 1,
    1,   0,   0.1, 00, 0,     0, 1,
    3,   0,   2.5, 00, 0,     0, 1,
    1,   0,   0.9, 00, 0,     0, 10,
    3,   0,   0.9, 00, 0.1,   0, 20,
    80,  0,    76, 00, 60,    0, 15,
    0.1, 0, 0.005, 00, 0.001, 0, 0),
  ncol=7)

fish = createFishObject(frac.table,parents,timepoints=timepoints)
fish = layoutClones(fish, separate.independent.clones=T)
fish = setCol(fish,c("#888888", "#EF0000", "#8FFF40", "#FF6000", "#50FFAF", "#FFCF00", "#0070FF"))
vlines=c(0,34,69,187,334,505,530,650,750)

fishPlot(fish, shape="spline", vlines=vlines, vlab=vlines, title.btm="AML31",cex.vlab=0.9)

##panel label
text(-125,130,"C",xpd=T,font=2)

dev=dev.off()

# testing driver mutation annotations
pdf("figure2.pdf", width=5, height=8)

layout(mat=t(matrix(c(1,2,3), nrow=1,ncol=3)))

##-------------------------------------------
##panel A
timepoints=c(0,30,75,150)
frac.table = matrix(
  c(100, 45, 00, 00,
    02, 00, 00, 00,
    02, 00, 02, 01,
    98, 00, 95, 40),
  ncol=length(timepoints))
parents = c(0,1,1,3)

fish = createFishObject(frac.table,parents,
                        timepoints=timepoints, 
                        clone.labels=c("Founding", "Subclone 1","Subclone 2","Subclone 3"),
                        clone.annots=c("TP53,MET", "NF1", "", "8q+,6p-"))
fish = layoutClones(fish)

sample.times = c(0,150)

fishPlot(fish,shape="spline",title.btm="633734",
         vlines=sample.times, vlab=sample.times, cex.title=0.5, bg.type="solid")

##-------------------------------------------
##panel B

timepoints=c(0,30,200,423)
parents = c(0,1,1,3)
frac.table = matrix(
  c(100, 38, 24, 00,
    002, 00, 01, 00,
    002, 00, 01, 01,
    100, 00, 98, 30),
  ncol=length(timepoints))

fish = createFishObject(frac.table,
                        parents,
                        timepoints=timepoints,
                        clone.annots=c("DNMT3A,FLT3", "NPM1", "MET", "ETV6,WNK1-WAC,\nMYO18B"))
fish = layoutClones(fish)
fishPlot(fish, shape="spline", vlines=c(0,423), vlab=c(0,423), title="Sample 150288", cex.title=0.9, cex.vlab=0.8)
##panel label
text(-100,130,"A",xpd=T,font=2)

drawLegend(fish)

##-------------------------------------------
##panel C
parents = c(0,1,2,1)
timepoints=c(0,120)
frac.table = matrix(
  c(100, 98, 46, 01,
    100, 44, 01, 55),
  ncol=length(timepoints))

fish = createFishObject(frac.table,parents,timepoints=timepoints)
fish = layoutClones(fish)
fish = setCol(fish,col=c("#1B9E77","#D95F02","#7570B3","#E7298A"))
fish@clone.annots = c("TP53", "", "BRCA", "")
fishPlot(fish, shape="polygon", vlab=c("Primary","Post-AI"),
         vlines=c(0,120), title.btm="BRC32",
         cex.title=0.7, cex.vlab=0.8,ramp.angle=1, pad.left=0.3)

dev <- dev.off()