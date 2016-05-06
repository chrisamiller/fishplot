An R package for visualizing changes in the subclonal architecture of tumors

## Installation instructions:

Both the 'sciClone' package and it's 'bmm' dependency can be installed by doing the following:

    #install dependencies
    install.packages(c("Hmisc","plotrix","png"))
    #install devtools if you don't have it already for easy installation
    install.packages("devtools")
    library(devtools)
    install_github("chrisamiller/fishplot")

If you prefer to build the package by hand, follow these steps:

- Make sure that you have the dependencies from CRAN ("Hmisc","plotrix","png")

- Download and build from source:


        git clone git@github.com:chrisamiller/fishplot.git
        R CMD build fishplot
        R CMD INSTALL fishplot_0.1.tar.gz

## Usage

    library(fishplot)

    #provide a list of timepoints to plot
    #This may require some manual interpolation
    #of data to provide the desired effect
    timepoints=c(0,30,75,150)      

    #provide a matrix with the fraction of each population
    #present at each timepoint
    frac.table = matrix(
      c(100, 45, 00, 00,
         02, 00, 00, 00,
         02, 00, 02, 01,
         98, 00, 95, 40),
      ncol=length(timepoints))

    #provide a vector listing each clone's parent
    #(0 indicates no parent)
    parents = c(0,1,1,3)

    #create a fish object
    fish = createFishObject(frac.table,parents,timepoints=timepoints)

    #calculate the layout of the drawing
    fish = layoutClust(fish)

    #draw the plot, using the splining method (recommended)
    #and providing both timepoints to label and a plot title
    drawPlot(fish,shape="spline",title.btm="Sample1",
             cex.title=0.5, vlines=c(0,150), 
             vlab=c("day 0","day 150"))


    v1 = read.table("data/vafs.tumor1.dat",header=T);
    v2 = read.table("data/vafs.tumor2.dat",header=T);
    v3 = read.table("data/vafs.tumor3.dat",header=T);

    #read in regions to exclude (commonly LOH)
    #format is 3-col bed
    regions = read.table("data/exclude.loh")

    #read in segmented copy number data
    #4 columns - chr, start, stop, segment_mean   
    cn1 = read.table("data/copy_number_tum1")
    cn2 = read.table("data/copy_number_tum2")
    cn3 = read.table("data/copy_number_tum3")

    #set sample names
    names = c("Sample1","Sample2","Sample3")


    #Examples:
    #------------------------------------
    #1d clustering on just one sample
    sc = sciClone(vafs=v1,
             copyNumberCalls=cn1,
             sampleNames=names[1],
             regionsToExclude=reg1)
    #create output
    writeClusterTable(sc, "results/clusters1")
    sc.plot1d(sc,"results/clusters1.1d.pdf")

    #------------------------------------
    #2d clustering using two samples:
    sc = sciClone(vafs=list(v1,v2),
                  copyNumberCalls=list(cn1,cn2),
                  sampleNames=names[1:2],
                   regionsToExclude=regions)
    #create output
    writeClusterTable(sc, "results/clusters2")
    sc.plot1d(sc,"results/clusters2.1d.pdf")
    sc.plot2d(sc,"results/clusters2.2d.pdf")


    #------------------------------------
    #3d clustering using three samples:
    sc = sciClone(vafs=list(v1,v2,v3),
                  copyNumberCalls=list(cn1,cn2,cn3),
                  sampleNames=names[1:3],
                   regionsToExclude=regions)
    #create output
    writeClusterTable(sc, "results/clusters2")
    sc.plot1d(sc,"results/clusters2.1d.pdf")
    sc.plot2d(sc,"results/clusters2.2d.pdf")
    sc.plot3d(sc, sc@sampleNames, size=700, outputFile="results/clusters3.3d.gif")

    #This pattern generalizes up to N samples, except for plotting, which caps out at 3d for obvious reasons.


  sample.times = c(0,150)


## Outputs
(see the vignettes (to be completed soon) for more examples and code

### Simple plot with multiple subclones
![fishplot1](http://i.imgur.com/OECRqcD.png)

### More complex clonal structure
![fishplot1](http://i.imgur.com/gYO23si.png)

### Multiple independent clones, many timepoints
![fishplot1](http://i.imgur.com/uFPgudQ.png)


## Notes
Manuscript in preparation.