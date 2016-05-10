#' Draw a single cluster using polygons
#'
#' @param xpos A vector of x values for control points
#' @param ytop A vector of y values for control points on the top
#' @param ybtm A vector of y values for control points on the bottom
#' @param color A color value for this polygon
#' @param nest.level integer describing how deeply this is nested
#' @param pad.left extra padding to add to the left side of the shape
#' @param border width of the border line around this polygon
#' @param borderCol color of the border line
#' 
#' @return No return value, outputs on graphics device
#' @examples 
#' drawClustPolygon(xpos=c(0,30,75,150), ytop=c(100,51,51,99), ybtm=c(0,49,49,1), color="red", nest.level=1) 
#'
drawClustPolygon <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                             border=1,borderCol=NULL){
    
    xst = xpos[1] - pad.left*(0.6^nest.level)
    yst = (ytop[1]+ybtm[1])/2
    
    x = c(xst, xpos, rev(xpos))
    y = c(yst, ybtm, rev(ytop))
    
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

  xpos = c(rbind(xpos-flank*2,xpos-flank,xpos,xpos+flank,xpos+flank*2))
  ybtm = c(rbind(ybtm,ybtm,ybtm,ybtm,ybtm))
  ytop = c(rbind(ytop,ytop,ytop,ytop,ytop))

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

##-----------------------------------------------------------------
##check that the colors match up with the number of clones
##
checkCol <- function(fish){
  nclones = nrow(fish@frac.table)
  if(length(fish@col) != nclones){
    stop(paste("ERROR: number of colors must be equal to the number of clones (",nclones,"). Use the setCol() function to set an appropriate color scheme.",sep=""))
  }
}


##---------------------------------------------------------------
## draw the plot
##
drawPlot <- function(fish,shape="polygon", vlines=NULL, vlineCol="#FFFFFF99", vlab=NULL,
                     border=1, borderCol="#777777", left.pad=0.2,
                     title=NULL, title.btm=NULL, cex.title=NULL){

  #make sure we have the right number of colors
  checkCol(fish)
  
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
    for(i in which(fish@parents==parent)){

      pad.left=pad
      if(parent>0){
        pad.left=pad*0.4
      }

      if(shape=="bezier"){
        drawClustBezier(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                        fish@col[i], fish@nest.level[i],
                        pad.left=pad.left, border=border ,borderCol=borderCol)
      } else {
        if(shape=="spline"){
          drawClustSpline(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                          fish@col[i], fish@nest.level[i],
                          pad.left=pad.left, border=border, borderCol=borderCol)
        } else {
          if(!shape=="polygon"){
            print(paste("unknown shape \"",shape,"\". Using polygon representation"))
          }
          drawClustPolygon(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                           fish@col[i], fish@nest.level[i],
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

