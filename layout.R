testing <<- 0;

library(plotrix)

##--------------------------------------------------------------
##
##
printerr <- function(var){
  if(testing){
    cat(" -",deparse(substitute(var)),":  ")
    cat(var,sep=",")
    cat("\n")
  }
}


##---------------------------------------------------------------
## draw a single cluster
##
drawClustPolygon <- function(xpos, ytop, ybtm, color, nest.level, label, pad.left=0){
  ##start with polygons

  printerr(ytop)
  printerr(ybtm)
  printerr(xpos)

  xst = xpos[1] - pad.left*(0.6^nest.level)
  yst = (ytop[1]+ybtm[1])/2
  #xst=xpos[1]
  #yst=ytop[1]
  x = c(xst, xpos, rev(xpos))
  y = c(yst, ybtm, rev(ytop))
  printerr(x)
  printerr(y)
  polygon(x=x, y=y, col=color, border=0)
}


drawClustBezier <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0){

  ##the flank value is used to add extra control points
  ##to the L and R of each real point, which helps to anchor the
  ##curves more firmly to the actual numbers
  range=max(xpos)-min(xpos)
  flank=range*0.01

  xst = xpos[1] - pad.left*(0.6^nest.level)
  yst = (ytop[1]+ybtm[1])/2
  #xst=xpos[1]
  #yst=ytop[1]

  ## xst = c(xst,xst+0.1)
  ## yst = c(yst,yst)

  xpos = c(rbind(xpos-flank*2,xpos-flank,xpos,xpos+flank,xpos+flank*2))
  ybtm = c(rbind(ybtm,ybtm,ybtm,ybtm,ybtm))
  ytop = c(rbind(ytop,ytop,ytop,ytop,ytop))


  printerr(ytop)
  printerr(ybtm)
  printerr(xpos)


  library(Hmisc)
  #top line
  top = bezier(c(xst,xpos),c(yst,ytop),evaluation=100)
  btm = bezier(c(xst,xpos),c(yst,ybtm),evaluation=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color,
          border=0)

  #view control points for testing
  #points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


drawClustSpline <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0){



  ##xst=xpos[1]
  ##yst=ytop[1]


  ##the flank value is used to add extra control points
  ##to the L and R of each real point, which helps to anchor the
  ##curves more firmly to the actual numbers
  range=max(xpos)-min(xpos)
  flank=range*0.001

  xpos = c(rbind(xpos-flank*2,xpos-flank,xpos,xpos+flank,xpos+flank*2))
  ybtm = c(rbind(ybtm,ybtm,ybtm,ybtm,ybtm))
  ytop = c(rbind(ytop,ytop,ytop,ytop,ytop))

  xst = xpos[1] - pad.left*(0.6^nest.level)
  yst = (ytop[1]+ybtm[1])/2
  xst = c(xst-flank*2,xst,xst+flank*2)
  yst = c(yst,yst,yst)


  printerr(ytop)
  printerr(ybtm)
  printerr(xpos)

  #top line
  top = spline(c(xst,xpos),c(yst,ytop),n=100)
  btm = spline(c(xst,xpos),c(yst,ybtm),n=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color,
          border=0)

  ## #view control points for testing
  ## points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


createBackgroundImage <- function(){
  ##create background image with smooth gradient
  library(png)
  png("/tmp/bck.png",width=80,height=80)  ##TODO - make this work with system temp dir (or current dir?)

  par(mar=c(0,0,0,0))
  plot(-100,-100,col="white",ylim=c(0,100), xlim=c(0,100),
       yaxt="n", xaxt="n",xlab="",ylab="",bty="n")

  ##background color
  gradient.rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
                col=smoothColors("bisque",100,"darkgoldenrod1",50,
                  "darkorange3",alpha=200),
                border=NA)
  dev.off()
  ##par back to default
  par(mar=c(5.1,4.1,4.1,2.1))
  return("/tmp/bck.png")
}


##---------------------------------------------------------------
## draw the plot
##
drawPlot <- function(fish,shape="polygon", vlines=NULL, vlineCol="#FFFFFF99", vlab=NULL){

  pad = max(fish@timepoints)*0.2;

  ##create raster background image for smooth gradient
  bckImage = readPNG(createBackgroundImage())

  #set up the plot
  plot(-100,-100,col="white",
       ylim=c(0,100),
       xlim=c(0-pad, max(fish@timepoints)),
       yaxt="n", xaxt="n",
       bty="n", xlab="", ylab="")

  lim=par()
  rasterImage(bckImage, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])



  ##draw the clusters one at a time, being sure that parents go before children
  for(parent in sort(unique(fish@parents))){
    for(i in as.numeric(names(fish@parents[fish@parents==parent]))){

      pad.left=pad
      if(parent>0){
        pad.left=pad*0.5
      }

      printerr("----draw")
      printerr(i)

      if(shape=="bezier"){
        drawClustBezier(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                        fish@colors[i], fish@nest.level[i],
                        pad.left=pad.left)
      } else {
        if(shape=="spline"){
          drawClustSpline(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                          fish@colors[i], fish@nest.level[i],
                          pad.left=pad.left)
        } else {
          if(!shape=="polygon"){
            print(paste("unknown shape \"",shape,"\". Using polygon representation"))
          }
          drawClustPolygon(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                           fish@colors[i], fish@nest.level[i],
                           pad.left=pad.left)
        }
      }
    }
  }
  #draw timepoint labels/lines
  if(!is.null(vlines)){
    abline(v=vlines,col=vlineCol)

    if(!is.null(vlab)){
      par(xpd=NA)
      text(vlines,103,vlab,pos=3,cex=0.7,col="grey20")
    }
  }
}


##---------------------------------------------------------------
## get the key points for the cluster layout
##
layoutClust <- function(fish){

  fish@inner.space=lapply(rownames(fish@frac.table),getInnerSpace,fish)
  fish@outer.space=getOuterSpace(fish)

  ytop.vec = c()
  ybtm.vec = c()
  xpos.vec = c()
  printerr(fish@timepoints)

  ##for each timepoint
  for(timepos in 1:length(fish@timepoints)){
    timepoint=fish@timepoints[timepos]

    printerr("------")
    printerr(timepoint)

    ytop = rep(NA,length(fish@parents))
    ybtm = rep(NA,length(fish@parents))
    xpos = rep(timepoint, length(fish@parents))

    ##starting with those with no parents, then moving through each existing parent
    for(parent in sort(unique(fish@parents))){

      numChildren = length(fish@parents[fish@parents==parent])
      spacing = 0
      ##start at the bottom plus half the outer space
      y = fish@outer.space[timepos]/2;
            
      ##consider inner spacing if this is a subclone
      if(parent > 0){
        y = ybtm[parent]
        spacing = fish@inner.space[[parent]][timepos]/(numChildren(fish,parent,timepos)+1)
      }

      ##for each cluster that has this parent, get coords
      for(cluster in as.numeric(names(fish@parents[fish@parents==parent]))){
        printerr(cluster)
        printerr(fish@frac.table[cluster,])
        printerr(fish@frac.table[cluster,timepos])

        if(fish@frac.table[cluster,timepos] == 0){ #cluster absent, don't need to add positions
          xpos[cluster] = NA
          ##smooth ending to dying clusters
          if(timepos > 1){
            if(fish@frac.table[cluster,timepos-1] > 0){
              printerr("DEAD")
              ybtm[cluster] = y+spacing/2
              ytop[cluster] = y+spacing/2
              xpos[cluster] = timepoint-0.25
            }
          }
        } else {
        printerr(ybtm)
        printerr(spacing)
        printerr(y)
          ybtm[cluster] = y+spacing
          y = y + fish@frac.table[cluster,timepos]
          ytop[cluster] = y+spacing
          y = y+spacing
        }


        printerr(cluster)
        printerr(spacing)
        printerr(ybtm)
        printerr(ytop)
        printerr(xpos)
      }
    }
    ybtm.vec = c(ybtm.vec,ybtm)
    ytop.vec = c(ytop.vec,ytop)
    xpos.vec = c(xpos.vec,xpos)
  }

  ##turn coords into a matrix so that we go by cluster instead of by timepoint
  ybtm = matrix(ybtm.vec,ncol=ncol(fish@frac.table))
  ytop = matrix(ytop.vec,ncol=ncol(fish@frac.table))
  xpos = matrix(xpos.vec,ncol=ncol(fish@frac.table))

  ybtm.list = list()
  ytop.list = list()
  xpos.list = list()

  ##now, split into lists per cluster
  for(i in 1:nrow(fish@frac.table)){
    ybtm.list[[i]] = ybtm[i,!is.na(ybtm[i,])]
    ytop.list[[i]] = ytop[i,!is.na(ytop[i,])]
    xpos.list[[i]] = xpos[i,!is.na(xpos[i,])]
  }

  fish@ybtm = ybtm.list
  fish@ytop = ytop.list
  fish@xpos = xpos.list

  return(fish)
}

##---------------------------------------------------------------
## get the number of non-zero children at this timepoint
##
numChildren <- function(fish,cluster,timepoint){
  if(cluster==0){
    return(0)
  }

  return(length(which(fish@frac.table[as.numeric(names(fish@parents[fish@parents==cluster])), timepoint]>0)))
}



##---------------------------------------------------------------
## Get the amount of this cluster that is only this cluster
## (not sub-clusters)
##
getInnerSpace <- function(clust,fish){
  total = fish@frac.table[as.numeric(clust),]
  for(i in as.numeric(names(fish@parents[fish@parents==clust]))){
    total = total - fish@frac.table[i,]
  }
  return(total)
}

##---------------------------------------------------------------
## Get the amount of non-tumor space outside all of clusters
##
getOuterSpace <- function(fish){
  #return the sums of all clusters with parents of 0 at each timepoint
  return(100-colSums(fish@frac.table[names(fish@parents[fish@parents==0]),]))
}



## getNestLevel <- function(parents){
##   levels = rep(NA,length(parents))
##   for(i in as.numeric(names(parents))){
##     parents
##   }
## }

###########################################################################

##need functions for reading in data. and sanity checks on input
## - each timepoint can't sum to over 100%,
## - no cluster can go to zero then back to some measurable amount
##   must have some residual signal (can be set to 0.000001 or something)
##

