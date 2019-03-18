#' Example code to call 'ggradar2' to build Radar Charts
#' @author Gangsheng Wang (wang.gangsheng@gmail.com)
#' https://github.com/wanggangsheng/ggradar2.git'
#' March 17, 2019
#' Improvement over ggradar: 
#' (1) add variable 'grid.n' = number of grids/circles; 
#' (2) allow to generate multiple (>3) girds/circles (ggradar only generate 3 circles: min/mid/max)
#' (3) allow to assign 'expression' to 'axis.labels', e.g., superscript, subscript, bolditalic
#' based on ggradar developed by Ricardo Bion

# setwd("/Users/wgs/ownCloud/Rcode/ggradar2")
# source("ggradar2_test.R")

#Clean up Environment
rm(list = ls())
library(ggplot2)
source("ggradar2.R")

path <- getwd()
dir_io <- "input_data"

grid.line.color <-rep(c("gray","#007A87"),len=10)
group.colors <- c("#37AFA9","blue","#F6893D","red")

dir_fig <- paste0("output_figure")

file_par0 <- "ggradar2_input_data.txt"

##------------------------------------------------
path_io <- paste0(path, "/", dir_io)
file_par <- paste0(path_io,"/", file_par0)
ifelse(!dir.exists(file.path(path, dir_fig)), 
	dir.create(file.path(path, dir_fig)), FALSE)
path_out <- paste0(path, "/",dir_fig)

par <- read.table(file_par, sep="\t", header=TRUE)
ncol <- ncol(par)
par_name <- colnames(par)[2:ncol]
npar <- ncol - 1

## Labels for parameters shown in the radar graph
par_label <- vector("expression", length=npar)
par_label[1] <- expression(bolditalic(r[E]))
par_label[2] <- expression(bolditalic(p[EP]))
par_label[3] <- expression(bolditalic(fp[EM]))
par_label[4] <- expression(bolditalic(f[D]))
par_label[5] <- expression(bolditalic(g[D]))
par_label[6] <- expression(bolditalic(V[g]))
par_label[7] <- expression(bolditalic(alpha))
par_label[8] <- expression(bolditalic(K[D]))
par_label[9] <- expression(bolditalic(Y[g]))
par_label[10] <- expression(bolditalic(k[Yg]))
par_label[11] <- expression(bolditalic(Q[10]))

grid.n <- 7

grid.min <- 0.0
grid.max <- max(par[,2:ncol])
grid.values <- vector("double",length=grid.n)
## Value for each grid/circle
for(i in 1:grid.n){
    grid.values[i] <- grid.min + (i-1)*(grid.max - grid.min)/(grid.n-1)
}
## Value-Labels for the radar graph
values.radar <- paste0(grid.values*100,'%')

# legend.groups <- c("Control: w/ Mic","Control: w/o Mic","Warming: w/ Mic","Warming: w/o Mic")

title1 <- "Parameter Uncertainty: Coefficient of Variation"
size_title <- 18
size_axis <- 12

fn <- paste0(path_out,"/ggradar2_output.png")
# png(fn, width=8, height=8, units='in', res=600)

sp <- ggradar2(plot.data=par, 
	axis.labels = par_label, axis.label.size=6,
	grid.min=grid.min, grid.max=grid.max, grid.n=grid.n,
	grid.values=grid.values, values.radar=values.radar,
	grid.line.colour=grid.line.color,
	group.colours=group.colors) +
	ggtitle(title1) +
	guides(color=guide_legend(ncol=2)) +
	theme(
		plot.title=element_text(hjust=0.5, size=size_title,face="bold"),
		legend.text=element_text(size=size_axis),
		legend.title=element_blank(),
		legend.position=c(0.5,0.98),
		legend.background=element_rect(fill="transparent"),
		legend.key.height=unit(1,'line'),
		legend.spacing.x=unit(2,'line')
		)

print(sp)
# dev.off()




