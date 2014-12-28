library( ggplot2 );
library( ggthemes );
library( Nozzle.R1 );

### ggplot2 Theme for Nature Methods PoV
theme_pov <- function(base_size=12, base_family="", ...) {
  return( theme_bw(base_size=base_size, base_family=base_family) +
    theme(
      line = element_line(colour = "gray"),
      rect = element_rect(fill =  NA, colour = NA),
      text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(colour = "black"),      
      legend.key = element_rect(colour = NA),
      ## Examples do not use grid lines
      panel.border = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_rect(fill="white", colour=NA),
      ...
    ));
}

palette_pov <- function(n, random_order = FALSE) {
  # default (colorblind and printer-friendly) colors
  cols <- c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#999999", "#E69F00")
  
  if (length(cols) < n)
    cols <- rep(cols, length.out = n)
  
  return( cols[1:n] );
}

scale_color_pov <- function(...) {
  discrete_scale('colour', 'pov', palette_pov, ...);
}

scale_colour_pov <- scale_color_pov;

scale_fill_pov <- function(...) {
  discrete_scale('fill', 'pov', palette_pov, ...);  
}

### parsing and cleanup
data <- read.table( "data/influenza_usa_totalAvsB.tsv", sep="\t" );

header <- apply( data[1,], 1, strsplit, " week: " );
header <- matrix( nrow=2, unlist(header) )
header <- cbind(c("year", "week"), header)

data <- data[-1,]

data <- rbind( header, data )
rownames(data) <- data[,1]
data <- data[,-1]

data <- as.data.frame( apply( as.matrix( t(data) ), 2, as.numeric ) )

### plots
start = 2010;

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), color=as.factor(year), geom="line", facets=year ~ . ) + theme_pov() + scale_color_pov() )
print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), fill=as.factor(year), color=as.factor(year), geom="bar", stat="identity", facets=year ~ . ) + theme_pov() + scale_color_pov() + scale_fill_pov() )

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), color=as.factor(year), geom="line" ) + coord_polar() + theme_pov() + theme( axis.line=element_blank(), panel.grid = element_line(color = "black")) + scale_color_pov() )

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], color=as.factor(year), geom="line" ) + theme_pov() + scale_color_pov() )

