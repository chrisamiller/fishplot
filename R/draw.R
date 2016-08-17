#' Draw a single cluster using polygons
#'
#' @param xpos A vector of x values for control points
#' @param ytop A vector of y values for control points on the top
#' @param ybtm A vector of y values for control points on the bottom
#' @param color A color value for this polygon
#' @param nest.level An integer describing how deeply this is nested
#' @param pad.left A numeric amount of extra padding to add to the left side of the shape
#' @param ramp.angle A numeric value between 0 and 1 that indicates how steeply the polygon should expand from it's origin to the first measured point
#' @param border A numeric width for the border line around this polygon
#' @param col.border A color for the border line
#' 
#' @return No return value, outputs on graphics device
#' @examples 
#' \dontrun{
#' drawClustPolygon(xpos=c(0,30,75,150), ytop=c(100,51,51,99), ybtm=c(0,49,49,1), color="red", nest.level=1) 
#' }
#' 
drawClustPolygon <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                             border=1,col.border=NULL, ramp.angle=0.5){
    
    xst = xpos[1] - pad.left*(0.6^nest.level)
    yst = (ytop[1]+ybtm[1])/2

    ##create an angled rampup - halfway between xst and xpos[1] with y increase proportional to the ramp angle
    xangle = (xst+xpos[1])/2
    yangle.top = yst + abs(yst-ytop[1])*ramp.angle
    yangle.btm = yst - abs(yst-ybtm[1])*ramp.angle

    x = c(xst, xangle, xpos, rev(xpos), xangle, xst)
    y = c(yst, yangle.btm, ybtm, rev(ytop), yangle.top, yst)

    polygon(x=x, y=y, col=color, border=col.border, lwd=border)
}

#' Draw a single cluster using bezier curves
#'
#' @param xpos A vector of x values for control points
#' @param ytop A vector of y values for control points on the top
#' @param ybtm A vector of y values for control points on the bottom
#' @param color A color value for this shape
#' @param nest.level An integer describing how deeply this is nested
#' @param pad.left A numeric amount of extra padding to add to the left side of the shape
#' @param border A numeric width for the border line around this polygon
#' @param col.border A color for the border line
#' 
#' @return No return value, outputs on graphics device
#' @examples
#' \dontrun{
#' drawClustBezier(xpos=c(0,30,75,150), ytop=c(100,51,51,99), ybtm=c(0,49,49,1), color="red", nest.level=1) 
#' }
#' 
drawClustBezier <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                            border=1, col.border=NULL){

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
  top = Hmisc::bezier(c(xst,xpos),c(yst,ytop),evaluation=100)
  btm = Hmisc::bezier(c(xst,xpos),c(yst,ybtm),evaluation=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color, border=col.border, lwd=border)

  #view control points for testing
  #points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


#' Draw a single cluster using splined curves
#'
#' @param xpos A vector of x values for control points
#' @param ytop A vector of y values for control points on the top
#' @param ybtm A vector of y values for control points on the bottom
#' @param color A color value for this shape
#' @param nest.level An integer describing how deeply this is nested
#' @param pad.left A numeric amount of extra padding to add to the left side of the shape
#' @param border A numeric width of the border line around this polygon
#' @param col.border A color for the border line
#' 
#' @return No return value, outputs on graphics device
#' @examples
#' \dontrun{
#' drawClustSpline(xpos=c(0,30,75,150), ytop=c(100,51,51,99), ybtm=c(0,49,49,1), color="red", nest.level=1) 
#' }
drawClustSpline <- function(xpos, ytop, ybtm, color, nest.level, pad.left=0,
                            border=1, col.border=NULL){

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
    print(paste0("yst: ",yst))
  ##this is not a complete fix for starts inappropriately hanging outside of parent, but helps
  if(yst > 85 | yst < 15){
      xst = (xst+xpos[1])/2
  }

  xst = c(xst-flank*2,xst,xst+flank*2)
  yst = c(yst,yst,yst)

    
  #top line
  top = spline(c(xst,xpos),c(yst,ytop),n=100)
  btm = spline(c(xst,xpos),c(yst,ybtm),n=100)
  polygon(x = c(top$x,rev(btm$x)),
          y = c(top$y,rev(btm$y)),
          col=color, border=col.border, lwd=border)

  ## #view control points for testing
  ## points(c(xst,xpos,xpos), c(yst,ytop,ybtm), pch=18,cex=0.5)
}


#' Create the gradient background image for the plot
#'
#' @param col A vector of three colors to use for the gradient
#' 
#' @return returns the location of the temporary png file that will get embedded into the eventual output
#'
createBackgroundImage <- function(col=c("bisque","darkgoldenrod1","darkorange3")){
  ##create background image with smooth gradient
    tmpfile=tempfile()
    png(tmpfile,width=80,height=80)  ##TODO - make this work with system temp dir (or current dir?)

  op <- par(mar=c(0,0,0,0))
  plot(-100,-100,col="white",ylim=c(0,100), xlim=c(0,100),
       yaxt="n", xaxt="n",xlab="",ylab="",bty="n")

  ##background color
  plotrix::gradient.rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
                         col=plotrix::smoothColors(col[1],50,col[2],25,col[3],alpha=200),
                         border=NA)
  dev.off()
  ##par back to default
  par(op)
  return(tmpfile)
}


#' Check that the number of colors provided matches  up with the number of clones
#'
#' @param fish A fish object
#'
#' @return no return value - stops execution if the numbers don't match up
#'
checkCol <- function(fish){
  nclones = nrow(fish@frac.table)
  if(length(fish@col) != nclones){
    stop(paste("ERROR: number of colors must be equal to the number of clones (",nclones,"). Use the setCol() function to set an appropriate color scheme.",sep=""))
  }
}


#' Given a fish object containing layout information, draw the fish plot
#'
#' @param fish A fish object that contains layout information
#' @param shape The type of shape to construct the plot out of. The "spline" and "polygon" methods work well. "bezier" is more hit or miss
#' @param vlines A vector of x positions at which to draw vertical lines
#' @param col.vline A color value for the vertical lines
#' @param vlab A character vector containing labels for each of the vertical lines
#' @param border A numeric width for the border line around this polygon
#' @param col.border A color for the border line
#' @param pad.left The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width. Default 0.2
#' @param title A string for the title above the plot
#' @param title.btm A string for the title at the bottom left, internal to the plot
#' @param cex.title A numeric value for scaling the title size
#' @param cex.vlab A numeric value for scaling the top label size default is 0.7
#' @param ramp.angle A numeric value between 0 and 1 that indicates how steeply the shape should expand from it's leftmost origin to the first measured point. Only used when shape="polygon".
#' 
#' @return No return value, outputs on graphics device
#' @examples 
#' \dontrun{
#' fishPlot(fish,shape="polygon",title.btm="633734",
#'            vlines=c(0,150), vlab=c("day 0","day 150"), cex.title=0.5)
#' }
#' @export
#' 
fishPlot <- function(fish,shape="polygon", vlines=NULL, col.vline="#FFFFFF99", vlab=NULL,
                     border=0.5, col.border="#777777", pad.left=0.2, ramp.angle=0.5,
                     title=NULL, title.btm=NULL, cex.title=NULL, cex.vlab=0.7){

  #make sure we have the right number of colors
  checkCol(fish)
  
  pad = (max(fish@timepoints)-min(fish@timepoints))*pad.left;
  ##create raster background image for smooth gradient
  bckImage = png::readPNG(createBackgroundImage())

  #set up the plot
  plot(-100,-100,col="white",
       ylim=c(0,100),
       xlim=c(min(fish@timepoints)-pad, max(fish@timepoints)),
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
                        pad.left=pad.left, border=border ,col.border=col.border)
      } else {
        if(shape=="spline"){
          drawClustSpline(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                          fish@col[i], fish@nest.level[i],
                          pad.left=pad.left, border=border, col.border=col.border)
        } else {
          if(!shape=="polygon"){
            print(paste("unknown shape \"",shape,"\". Using polygon representation"))
          }
          drawClustPolygon(fish@xpos[[i]], fish@ytop[[i]], fish@ybtm[[i]],
                           fish@col[i], fish@nest.level[i], ramp.angle=ramp.angle,
                           pad.left=pad.left, border=border, col.border=col.border)
        }
      }
    }
  }
  #draw timepoint labels/lines
  if(!is.null(vlines)){
    abline(v=vlines,col=col.vline,xpd=F)

    if(!is.null(vlab)){
      text(vlines,103,vlab,pos=3,cex=cex.vlab,col="grey20",xpd=NA)
    }
  }

  if(!is.null(title)){
    #get the center
    xmax = tail(fish@timepoints,n=1)
    cent = (xmax/2)-(pad/2)
    text(cent,112,title,pos=3,cex=cex.title,xpd=T)
  }


  if(!is.null(title.btm)){
    text(min(fish@timepoints)-(pad*1.2),2,title.btm,pos=4,cex=cex.title)
  }

}

#' Draw a legend beneath the plot
#'
#' @param fish A fish object
#' @param xpos The x coordinate at which to draw the left side of the legend (default 0)
#' @param ypos The y coordinate at which to draw the top of the legend (default -5)
#' @param nrow An integer number of rows which should be used for the legend
#' @param cex A numerical value giving the amount by which the legend should be magnified relative to the default.
#'
#' @return No return value, outputs on graphics device
#' @examples
#' \dontrun{
#' drawLegend(fish)
#' drawLegend(fish, 20, -20, 3)
#' }
#' @export
#'
drawLegend <- function(fish, xpos=0, ypos=-5, nrow=NULL, cex=1){
  if(is.null(fish@clone.labels)){
    fish@labels=1:dim(fish@frac.table)[1]
  }

  #do something sensible by default - can fit about 8 per row on a typically sized plot
  if(is.null(nrow)){
    nrow = ceiling(length(fish@clone.labels)/8)
  }
  
  ##reorder for multi-row layout
  ncol = ceiling(length(fish@clone.labels)/nrow)
  lab = as.vector(suppressWarnings(t(matrix(fish@clone.labels,nrow=ncol))))[1:length(fish@clone.labels)]
  col = as.vector(suppressWarnings(t(matrix(fish@col,nrow=ncol))))[1:length(fish@col)]

  legend(xpos,ypos,fill=col, legend=lab, bty="n", ncol=ncol, xpd=T, col="grey30", border="grey30", cex=cex*0.8)
}
