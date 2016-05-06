testing <<- 1;

library(plotrix)
library(png)

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
drawClustPolygon <- function(xpos, ytop, ybtm, color, nest.level, label, pad.left=0,
                             border=1,borderCol=NULL){
  ##start with polygons

  ## printerr(ytop)
  ## printerr(ybtm)
  ## printerr(xpos)

  xst = xpos[1] - pad.left*(0.6^nest.level)
  yst = (ytop[1]+ybtm[1])/2
  #xst=xpos[1]
  #yst=ytop[1]
  x = c(xst, xpos, rev(xpos))
  y = c(yst, ybtm, rev(ytop))
  ## printerr(x)
  ## printerr(y)
  polygon(x=x, y=y, col=color, border=borderCol, lwd=border)
}


drawClustBezier <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                            border=1, borderCol=NULL){

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


  ## printerr(ytop)
  ## printerr(ybtm)
  ## printerr(xpos)


  library(Hmisc)
  #top line
  top = bezier(c(xst,xpos),c(yst,ytop),evaluation=100)
  btm = bezier(c(xst,xpos),c(yst,ybtm),evaluation=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color, border=borderCol, lwd=border)

  #view control points for testing
  #points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


drawClustSpline <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                            border=1, borderCol=NULL){



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


  ## printerr(ytop)
  ## printerr(ybtm)
  ## printerr(xpos)

  #top line
  top = spline(c(xst,xpos),c(yst,ytop),n=100)
  btm = spline(c(xst,xpos),c(yst,ybtm),n=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color, border=borderCol, lwd=border)

  ## #view control points for testing
  ## points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


createBackgroundImage <- function(){
  ##create background image with smooth gradient
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
drawPlot <- function(fish,shape="polygon", vlines=NULL, vlineCol="#FFFFFF99", vlab=NULL,
                     border=1, borderCol="#777777", left.pad=0.2,
                     title=NULL, title.btm=NULL, cex.title=NULL){

  pad = max(fish@timepoints)*left.pad;

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
        pad.left=pad*0.4
      }

      ## ## printerr("----draw")
      ## ## printerr(i)

      if(shape=="bezier"){
        drawClustBezier(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                        fish@colors[i], fish@nest.level[i],
                        pad.left=pad.left, border=border ,borderCol=borderCol)
      } else {
        if(shape=="spline"){
          drawClustSpline(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                          fish@colors[i], fish@nest.level[i],
                          pad.left=pad.left, border=border, borderCol=borderCol)
        } else {
          if(!shape=="polygon"){
            print(paste("unknown shape \"",shape,"\". Using polygon representation"))
          }
          drawClustPolygon(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                           fish@colors[i], fish@nest.level[i],
                           pad.left=pad.left, border=border, borderCol=borderCol)
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

  if(!is.null(title)){
    #get the center
    xmax = tail(fish@timepoints,n=1)
    cent = (xmax/2)-(pad/2)
    text(cent,112,title,pos=3,cex.title)
  }


  if(!is.null(title.btm)){
    text(0-(pad*1.2),2,title.btm,pos=4,cex.title)
  }

}


##---------------------------------------------------------------
## get the key points for the cluster layout
##
layoutClust <- function(fish,separateIndependentClones=FALSE){

  fish@inner.space=lapply(rownames(fish@frac.table),getInnerSpace,fish)
  fish@outer.space=getOuterSpace(fish)

  ytop.vec = c()
  ybtm.vec = c()
  xpos.vec = c()
  ## printerr(fish@timepoints)

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

      ## (unless we are separating indpendent clones, in which case
      ## we divide outer spacing info inbetween and around
      if(separateIndependentClones){
        y=0
        if(parent == 0){
          numZeros = length(which(fish@parents==0))
          if(numZeros > 1 & fish@outer.space[timepos] > 0){
            spacing=fish@outer.space[timepos]/(numZeros+1)
          }
        }
      } 
      if(parent!=0){##consider inner spacing if this is a subclone
        y = ybtm[parent]
        spacing = fish@inner.space[[parent]][timepos]/(numChildren(fish,parent,timepos)+1)
      }

      ##for each cluster that has this parent, get coords
      for(cluster in as.numeric(names(fish@parents[fish@parents==parent]))){
        printerr(cluster)
        printerr(fish@frac.table[cluster,])
        printerr(fish@frac.table[cluster,timepos])

        ##cluster absent, don't need to add positions
        if(fish@frac.table[cluster,timepos] == 0){
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
        } else { #cluster is still here, deal with it
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
  ##return the sums of all clusters with parents of 0 at each timepoint
  z = fish@frac.table[names(fish@parents[fish@parents==0]),]
  if(is.vector(z)){ #only one row, just return it
    return(100-z)
  }
  return(100-colSums(z))
}

##---------------------------------------------------------------
## Given the a list representing the parents of each clone, and the
## number specifying which clone to test, returns how deeply it is
## nested
##
getNestLevel <- function(parents,x){
  #sanity checks
  if(x > length(parents)){
    stop(paste("cannot have a parent that does not exist in list. parent =",x,", length(parents) =",length(parents)))
  }
  if(x < 0){
    stop("cannot have a value in parents of less than zero")
  }

  if(parents[x] == 0){
    return(0)
  } else {
    return(getNestLevel(parents,parents[x])+1)
  }
}


###########################################################################

##need functions for reading in data. and sanity checks on input
## - each timepoint can't sum to over 100%,
## - no cluster can go to zero then back to some measurable amount
##   must have some residual signal (can be set to 0.000001 or something)
##

