#' Initialize the fish class
#'
#' @return no return value
#'
initFishClass <- function(){
  methods::setClass("fishObject", representation(ytop="list", ybtm="list", xpos="list",
                                        col="character", timepoints="numeric",
                                        frac.table="matrix", parents="numeric",
                                        nest.level="numeric", inner.space="list",
                                        outer.space="numeric", clone.labels="character",
                                        clone.annots="character", 
                                        clone.annots.angle="numeric", clone.annots.col="character",
                                        clone.annots.pos="numeric", clone.annots.cex="numeric", clone.annots.offset="numeric"
                                        ))
}


##------------------------------------------------------------------------
#' Validate some key assumptions about the fish object's data
#'
#' @param frac.table A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between clones
#' @param nest.level An integer vector specifying how deeply a given clone is nested in the overall hierarchy
#' @param clone.labels An integer vector specifying how deeply a given clone is nested in the overall hierarchy
#' @param clone.annots A character vector of annotations (mutation) to label to each clone in the plot
#' @return No return value - stops execution with an error if invalid inputs are detected
#'
validateInputs <- function(frac.table, parents, nest.level, clone.labels, clone.annots){
  clones =  1:dim(frac.table)[1]
  timepts = 1:dim(frac.table)[2]

  ##no cluster can go from present to absent and then back
  for(clone in clones){
    started=FALSE
    ended=FALSE
    for(timept in timepts){
      if(frac.table[clone,timept] > 0){
        if(started & ended){
          stop(paste("Clone",clone,"goes from present to absent (fraction=0) and then back to present. Values for which the cluster was present must be non-zero. Either fix the inputs or set fix.missing.clones=TRUE to change offending timepoints to very small values."))
          
        }
        started=TRUE
      } else {
        if(started){
          ended=TRUE
        }
      }
    }
  }

  ##clusters of entirely zero get a warning
  if(length(which(rowSums(frac.table) == 0)) > 0){
    print("WARNING: at least one cluster has fraction zero at all timepoints. It will not be displayed")
  }
  
  ##make sure that each timepoint doesn't sum to more than the parental value at a given nest level (or 100% for level 0)
  for(timept in timepts){
    for(i in unique(nest.level)){
      neighbors = which(nest.level==i)
      if(sum(frac.table[neighbors,timept]) > 100){
        stop(paste("clones with same nest level cannot have values that sum to more than 100%: Problem is in clusters ",
                   paste(neighbors,collapse=",")))
      }
    }

    for(i in unique(parents)){
      if(i > 0){
        neighbors = which(parents==i)
        if(sum(frac.table[neighbors,timept]) > frac.table[parents[neighbors[1]],timept]){
          stop(paste("clones with same parent cannot have values that sum to more than the percentage of the parent: Problem is in clusters ",paste(neighbors,collapse=","),"at timepoint",timept))
        }
      }
    }
  }

  ##ensure that the number of clone labels is equal to the number of clones
  if(length(clone.labels) != nrow(frac.table)){
    stop(paste("number of clone.labels provided must be equal to the number of clones"))
  }
  
  if(length(clone.annots) != nrow(frac.table)){
    stop(paste("number of clone.annots provided must be equal to the number of clones"))
  }

}


##------------------------------------------------------------------------
#' Given the a list representing the parents of each clone, and the number specifying which clone to test, returns how deeply it is nested
#'
#' @param parents An integer vector specifying parental relationships between clones
#' @param x The integer specifying which subclone to calculate nest level for
#'
#' @return An integer representing how deeply this subclone is nested
#' @seealso getAllNestLevels
#'
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


##------------------------------------------------------------------------
#' Given the a list representing the parents of each clone, return a vector specifying how deeply each clone is nested
#'
#' @param parents An integer vector specifying parental relationships between clones
#'
#' @return An integer vector representing how deeply each subclone is nested
#' @seealso getNestLevel
#'
getAllNestLevels <- function(parents){
  nest.level=c()
  for(i in 1:length(parents)){
    nest.level=c(nest.level, getNestLevel(parents,i))
  }
  return(nest.level)
}

##------------------------------------------------------------------------
#' Create a fish object after doing some input validation and data munging
#'
#' @param frac.table A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between clones
#' @param timepoints An numeric vector specifying the timepoints for each column of the matrix
#' @param col A vector of colors to use when plotting each clone
#' @param clone.labels A character vector of names to assign to each clone when plotting a legend
#' @param clone.annots A character vector of annotations (mutation) to label to each clone in the plot
#' @param clone.annots.angle A numeric angle in degrees (0-360) giving the angle at which to plot the annotations
#' @param clone.annots.col A string giving the color with which to draw the clone annotations
#' @param clone.annots.pos An integer describing the position for the clone annotations (1=below, 2=left, 3=above, 4=right)
#' @param clone.annots.cex A numeric specifying the clone annotation text size ("character expansion factor")
#' @param clone.annots.offset A numeric specifying distance ("offset") of the annotation from the clone start point
#' @param fix.missing.clones A boolean value, telling whether to "correct" clones that have zero values at timepoints between non-zero values. (the clone must still have been present if it came back). Default FALSE.
#'
#' @return A fish object with the relevant slots filled
#' @export
#'
#' @examples
#' timepoints=c(0,30,75,150)
#' frac.table = matrix(
#'     c(100, 45, 00, 00,
#'        02, 00, 00, 00,
#'        02, 00, 02, 01,
#'        98, 00, 95, 40),
#'     ncol=length(timepoints))
#' parents = c(0,1,1,3)
#' fish = createFishObject(frac.table,parents,timepoints=timepoints)
#'
createFishObject <- function(frac.table,parents,timepoints=NULL,col=NULL,clone.labels=NULL,clone.annots=NULL,
                             clone.annots.angle=0,clone.annots.col="black",clone.annots.pos=2,clone.annots.cex=0.7,clone.annots.offset=0.2,
                             fix.missing.clones=FALSE){

  nest.level = getAllNestLevels(parents)

  rownames(frac.table)=seq(1:dim(frac.table)[1])

  #default timepoints are just 1,2,...,numColumns
  if(is.null(timepoints)){
    colnames(frac.table)=c(1:ncol(frac.table))
  } else {
    if(!(is.numeric(timepoints) & is.vector(timepoints))){
      stop("ERROR: timepoints must be a numeric vector")
    }
    colnames(frac.table)=timepoints
  }

  #default clone labels are just 1:numClones
  if(is.null(clone.labels)){
    clone.labels=as.character(1:nrow(frac.table))
  }
  
  #default clone annotations are just empty strings
  if(is.null(clone.annots)){
    clone.annots=rep("",nrow(frac.table))
  }

  if(fix.missing.clones){
    frac.table = fixDisappearingClones(frac.table,nest.level)
  }
  
  
  #sanity checks on input data
  validateInputs(frac.table, parents, nest.level, clone.labels, clone.annots)

  #create the object
  fish = new("fishObject", ytop=list(), ybtm=list(), col=c("NULL"),
    timepoints=as.numeric(colnames(frac.table)), frac.table=frac.table,
    parents=parents, nest.level=nest.level, inner.space=list(), outer.space=c(0),
    clone.labels=clone.labels, clone.annots=clone.annots, clone.annots.angle=clone.annots.angle, clone.annots.col=clone.annots.col,
    clone.annots.pos=clone.annots.pos, clone.annots.cex=clone.annots.cex, clone.annots.offset=clone.annots.offset
    )

  #set default colors to start
  fish = setCol(fish,col)

  return(fish)
}

#' Attach the colors for plotting to the fish object, ensuring that they are valid. If no color vector is provided, a default color scheme is used.
#'
#' @param fish A fish object with the frac.table slot filled in
#' @param col A vector of colors with the same length as the number of clones (number of rows in the frac table)
#'
#' @return The fish object with the colors stored in the appropriate slot
#' @export
#' @examples
#' \dontrun{
#' setCol(fish)
#'
#' fish = setCol(fish, c("red","yellow","blue","green"))
#' }
#'
setCol <- function(fish,col=NULL){
  nclones = nrow(fish@frac.table)
  if(!(exists("nclones"))){
      print("WARNING: Could not set colors, as the number of rows in the frac.table slot of the fish object could not be calculated")
      return(fish)
  }

  if(is.null(col)){
    ##print("Using default color scheme. Use the setCol() function to change this.")
    ##use default color scheme
    col = rev(c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#888888"))

    ##check length
    if(length(col) < nclones){
      print(paste0("WARNING: default color scheme only includes 10 colors, but ",nclones," are needed to color each clone uniquely. Use the setCol() function to add a color scheme"))
    } else {
      fish@col=col[1:nclones]
    }
    return(fish)
  }

  ##else colors provided, check them for sanity
  if(length(col) != nrow(fish@frac.table)){
    stop(paste("ERROR: number of colors provided must be equal to the number of clones (",nclones,")",sep=""))
  }
  fish@col=col
  return(fish)
}


#' A clone that has a nonzero value at one time point cannot completely disappear in a second, and then reappear in a third.
#' That clone must have really been there all along.  This function will "fix" any such instances by replacing the in-between
#' zero values with a very small value.
#'
#' @param frac.table A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param nest.level An integer vector specifying how deeply a given clone is nested in the overall hierarchy
#'
#' @return The matrix with appropriate zeros converted to appropriate small values
#' @examples
#' \dontrun{
#'
#' frac.table = fixDisappearingClones(frac.table, nest.level)
#' }
#'
fixDisappearingClones <- function(frac.table,nest.level){
  clones =  1:dim(frac.table)[1]
  timepts = 1:dim(frac.table)[2]

  for(clone in clones){
    ## get the first and last non-zero timepoint
    minNonZeroPos = 0
    maxNonZeroPos = 0
    for(i in timepts){
      if(frac.table[clone,i] > 0){
        if(minNonZeroPos == 0){
          minNonZeroPos = i
        } else {
          maxNonZeroPos = i
        }
      }
    }
    ## then go back and change any previous zeros to non-zero values.  We can't just set this arbitrarily, because it
    ## the matrix still has to pass the other checks (i.e. subclones can't sum to more than their shared parents)
    if(minNonZeroPos > 0 & maxNonZeroPos > 0){
      for(i in minNonZeroPos:maxNonZeroPos){
        if(frac.table[clone,i] == 0){
          frac.table[clone,i] = 0.01^nest.level[clone] ##this may cause problems if we have more than 100 subclones,
        }                                              ##(in which case the plot is going to look shitty anyway)
      }
    }
  }
  return(frac.table)
}
