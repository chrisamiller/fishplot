#' Generate key points of the layout that will be used for plotting
#'
#' @param fish A fish object with appropriate slots filled (frac.table, parents, nest.level)
#' @param separate.independent.clones Boolean - Should independently-arising clones (with parent 0) be separated by blank space in the plot?
#'
#' @return A fish object with layout slots filled in
#' @export
#' @examples
#' \dontrun{
#' layoutClones(fish.object)
#'
#' layoutClones(fish.object, separate.independent.clones=TRUE)
#' }
#'
layoutClones <- function(fish,separate.independent.clones=FALSE){

  fish@inner.space=lapply(rownames(fish@frac.table),getInnerSpace,fish)
  fish@outer.space=getOuterSpace(fish)

  ytop.vec = c()
  ybtm.vec = c()
  xpos.vec = c()

  ##for each timepoint
  for(timepos in 1:length(fish@timepoints)){
    timepoint=fish@timepoints[timepos]

    ytop = rep(NA,length(fish@parents))
    ybtm = rep(NA,length(fish@parents))
    xpos = rep(timepoint, length(fish@parents))
	##starting with those with no parents, then moving through each existing parent
    parentsList = 0
    while(length(parentsList) > 0) 
    {
      parent = parentsList[[1]]
      childrens = which(fish@parents == parent)
      parentsList = parentsList[-1]
      parentsList = c(parentsList, childrens)
      numChildren = length(childrens)
      spacing = 0
      ##start at the bottom plus half the outer space
      y = fish@outer.space[timepos]/2;

      ## (unless we are separating indpendent clones, in which case
      ## we divide outer spacing info inbetween and around
      if(separate.independent.clones){
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

      ##for each clone that has this parent, get coords
      for(clone in childrens)
	{

        ##clone absent, don't need to add positions
        if(fish@frac.table[clone,timepos] == 0){
          xpos[clone] = NA
          ##smooth ending to dying clones
          if(timepos > 1){
            if(fish@frac.table[clone,timepos-1] > 0){
              ybtm[clone] = y+spacing/2
              ytop[clone] = y+spacing/2
              xpos[clone] = timepoint-0.25
            }
          }
        } else { #clone is still here, deal with it
          ybtm[clone] = y+spacing
          y = y + fish@frac.table[clone,timepos]
          ytop[clone] = y+spacing
          y = y+spacing
        }
      }
    }
    ybtm.vec = c(ybtm.vec,ybtm)
    ytop.vec = c(ytop.vec,ytop)
    xpos.vec = c(xpos.vec,xpos)
  }

  ##turn coords into a matrix so that we go by clone instead of by timepoint
  ybtm = matrix(ybtm.vec,ncol=ncol(fish@frac.table))
  ytop = matrix(ytop.vec,ncol=ncol(fish@frac.table))
  xpos = matrix(xpos.vec,ncol=ncol(fish@frac.table))

  ybtm.list = list()
  ytop.list = list()
  xpos.list = list()

  ##now, split into lists per clone
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

#' get the number of non-zero children at a particular timepoint
#'
#' @param fish A fish object
#' @param clone An integer representing the clone number to check
#' @param timepoint The timepoint at which to check
#'
#' @return the number of children with non-zero fractions
#' 
numChildren <- function(fish,clone,timepoint){
  if(clone==0){
    return(0)
  }
  return(length(which(fish@frac.table[which(fish@parents==clone), timepoint]>0)))
}

#' Get the percentage of a clone that is only that clone and not occupied by subclones
#' 
#' @param clone The number of the clone to check
#' @param fish A fish object
#'
#' @return A number giving the percentage of this clone that is uniquely this clone
getInnerSpace <- function(clone,fish){
  total = fish@frac.table[as.numeric(clone),]
  for(i in which(fish@parents==clone)){
    total = total - fish@frac.table[i,]
  }
  return(total)
}

#' Get the amount of non-tumor space outside of all clones
#' 
#' @param fish A fish object
#'
#' @return A numeric vector representing non-tumor space at each timepoint
getOuterSpace <- function(fish){
  ##return the sums of all clones with parents of 0 at each timepoint
  z = fish@frac.table[which(fish@parents==0),]
  if(is.vector(z)){ #only one row, just return it
    return(100-z)
  }
  return(100-colSums(z))
}

