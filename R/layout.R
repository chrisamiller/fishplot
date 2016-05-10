##---------------------------------------------------------------
## get the key points for the cluster layout
##
layoutClust <- function(fish,separateIndependentClones=FALSE){

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
      for(cluster in which(fish@parents==parent)){

        ##cluster absent, don't need to add positions
        if(fish@frac.table[cluster,timepos] == 0){
          xpos[cluster] = NA
          ##smooth ending to dying clusters
          if(timepos > 1){
            if(fish@frac.table[cluster,timepos-1] > 0){
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

  return(length(which(fish@frac.table[which(fish@parents==cluster), timepoint]>0)))
}



##---------------------------------------------------------------
## Get the amount of this cluster that is only this cluster
## (not sub-clusters)
##
getInnerSpace <- function(clust,fish){
  total = fish@frac.table[as.numeric(clust),]
  for(i in which(fish@parents==clust)){
    total = total - fish@frac.table[i,]
  }
  return(total)
}

##---------------------------------------------------------------
## Get the amount of non-tumor space outside all of clusters
##
getOuterSpace <- function(fish){
  ##return the sums of all clusters with parents of 0 at each timepoint
  z = fish@frac.table[which(fish@parents==0),]
  if(is.vector(z)){ #only one row, just return it
    return(100-z)
  }
  return(100-colSums(z))
}

#todo - import code that takes clonevol input
