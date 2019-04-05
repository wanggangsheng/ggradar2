#' ggradar2
#' to plot Radar Charts with ggplot2
#' @author Gangsheng Wang (wang.gangsheng@gmail.com)
#' https://github.com/wanggangsheng/ggradar2.git'
#' March 17, 2019
#' Improvement over ggradar: 
#' (1) add variable 'grid.n' = number of grids/circles; 
#' (2) allow to generate multiple (>3) girds/circles (ggradar only generate 3 circles: min/mid/max)
#' (3) allow to assign 'expression' to 'axis.labels', e.g., superscript, subscript, bolditalic
#' (4) delete "font.radar" to avoid errors in "font" when plot is saved to a file

#' based on ggradar developed by Ricardo Bion
#' https://github.com/ricardo-bion/ggradar.git
#' @export ggradar
#'
#' @export
# most of the code is from http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html

ggradar3 <- function(plot.data,
                         base.size=15, 
                         grid.n=6,
                         grid.values=NULL,
                         values.radar = c("0%", "60%", "120%"),                  
                         axis.labels=colnames(plot.data)[-1],                             
                         grid.min=0,  #10,
                         grid.max=NULL,  #100,
                         centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                         plot.extent.x.sf=1,
                         plot.extent.y.sf=1.2,
                         x.centre.range=0.02*(grid.max-centre.y),
                         label.centre.y=FALSE,
                         grid.line.width=0.5,
                         grid.line.linetype="longdash",
                         grid.line.colour=rep(c("gray","#007A87"),len=10),
                         grid.label.size=6,
                         gridline.label.offset=-0.1*(grid.max-centre.y),
                         axis.label.offset=1.15,
                         axis.label.size=5,
                         axis.line.colour="grey",
                         group.line.width=1.5,
                         group.point.size=6,
                         group.colours=NULL,
                         background.circle.colour="#D7D6D1",
                         background.circle.transparency=0.2,
                         plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                         legend.title="",
                         plot.title="",
                         legend.text.size=14,
                         legend.position = "left") {

  library(ggplot2)

  plot.data <- as.data.frame(plot.data)

## wgs: begin
  if (is.null(grid.max)){
    ncol <- ncol(plot.data)
    grid.max <- max(plot.data[,2:ncol])
  }

  centre.y=grid.min - ((1/9)*(grid.max-grid.min))

  if (!is.null(grid.values)){
    grid.n <-  length(grid.values)
    } else {
      grid.values <- vector("double",length=grid.n)
      ## Value for each grid/circle
      for(i in 1:grid.n){
        grid.values[i] <- grid.min + (i-1)*(grid.max - grid.min)/(grid.n-1)
      }
    ## Value-Labels for the radar graph
    values.radar <- paste0(grid.values*100,'%')
    }

  
## wgs:end

  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"

  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n

  #calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf

  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")

#Declare required internal functions
##---------------------------------------------------------------------
CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

  path <- df[,1]

  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]

  for(i in levels(path)){
    pathData = subset(df, df[,1]==i)
    for(j in c(2:ncol(df))){
      #pathData[,j]= pathData[,j]


      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData #data frame returned by function
} ## CalculateGroupPath
##---------------------------------------------------------------------
CaclulateAxisPath = function(var.names,min,max) {
  #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
  #Args:
  #var.names - list of variables to be plotted on radar plot
  #min - MININUM value required for the plotted axes (same value will be applied to all axes)
  #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
  #var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  #Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i,min.x[i],min.y[i])
    b <- c(i,max.x[i],max.y[i])
    axisData <- rbind(axisData,a,b)
  }
  #Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no","x","y")
  rownames(axisData) <- seq(1:nrow(axisData))
  #Return calculated axis paths
  as.data.frame(axisData)
} ## CaclulateAxisPath
##---------------------------------------------------------------------
funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
} ## funcCircleCoords
##---------------------------------------------------------------------

### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=var.names,
    x=NA,
    y=NA )
  ## wgs: begin
  axis.labels1 <- axis.labels
  ## wgs: end
  # print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  # print(n.vars)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  ## wgs: begin
  gridline <- NULL
  for(i in 1:grid.n){
    gridline$path[[i]] <- funcCircleCoords(c(0,0),grid.values[i]+abs(centre.y),npoints = 360)  
    gridline$label[[i]] <- data.frame(x=gridline.label.offset,y=grid.values[i]+abs(centre.y),
                                   text=as.character(values.radar[i]))
  }
  ## wgs: end

### Start building up the radar plot

# Declare 'theme_clear', with or without a plot legend as required by user
#[default = no legend if only 1 group [path] being plotted]
theme_clear <- theme_bw(base_size=base.size) + 
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.key=element_rect(linetype="blank"))

if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")

#Base-layer = axis labels + plot extent
# [need to declare plot extent as well, since the axis labels don't always
# fit within the plot area automatically calculated by ggplot, even if all
# included in first plot; and in any case the strategy followed here is to first
# plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
# then centred labels for axis labels almost immediately above/below x= 0 
# [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
# This building up the plot in layers doesn't allow ggplot to correctly 
# identify plot extent when plotting first (base) layer]

#base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
## wgs: begin
axis_id <- which(axis$label$x < (-x.centre.range))
axis.labels2 <- axis.labels1[axis_id]
# print(axis.labels2)
base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
  geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
            aes(x=x,y=y),label=axis.labels2,size=axis.label.size,hjust=1) +
  scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) + 
  scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))

# ... + circular grid-lines at 'grid.n' y-axis values
# print(values.radar)
for(i in 1:grid.n){
  base <- base + geom_path(data=gridline$path[[i]],aes(x=x,y=y),
                          lty=grid.line.linetype,colour=grid.line.colour[i],size=grid.line.width) 
  base <- base + geom_text(aes(x=x,y=y,label=text,fontface='bold'),data=gridline$label[[i]],size=grid.label.size*0.8, hjust=1) 

  }

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  axis_id <- which(abs(axis$label$x)<=x.centre.range)
  axis.labels2 <- axis.labels1[axis_id]
  # print(axis.labels2)
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y),label=axis.labels2,size=axis.label.size,hjust=0.5)
  # + axis labels for any vertical axes [x>x.centre.range]
  axis_id <- which(axis$label$x>x.centre.range)
  axis.labels2 <- axis.labels1[axis_id]
  # print(axis.labels2)
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y),label=axis.labels2,size=axis.label.size,hjust=0)
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$path[[grid.n]],aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  ## wgs: end

  # + radial axes
  # print(axis$path)
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)

  # ... + group (cluster) 'paths'
  # print(group$path)
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)

  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)


  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)

  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5) }

  if (!is.null(group.colours)){
    colour_values=rep(group.colours,100)
  } else {
    colour_values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", 
                       "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)
  }
  
  base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
  theme(legend.key.height=unit(2,"line")) +
  scale_colour_manual(values=colour_values) +
  # theme(text=element_text(family=font.radar)) + 
  theme(legend.title=element_blank())

  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  return(base)
  }