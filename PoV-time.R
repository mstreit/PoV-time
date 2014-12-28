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
      #panel.margin = rep(element_blank(),4),
      #plot.margin = rep(unit(0,"null"),4),      
      ...
    ));
}

palette_pov <- function(n, random_order = FALSE) {
  # default (colorblind and printer-friendly) colors
  cols <- c("#CC79A7", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#56B4E9", "#F0E442", "#999999")
  
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

data <- cbind( data, matrix( ncol=1, nrow=dim(data)[1], seq( 1:dim(data )[1] ) ) )
colnames( data )[dim(data)[2]] <- "index"

week53 <- data[which(data$week==1),];
week53$week <- 53
week53$year <- week53$year - 1;

data <- rbind( data, week53 )

### plots
start = 2010;
index_delta = data$index[which(data$year == start & data$week == 1)];

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), color=as.factor(year), geom="line", facets=year ~ . ) + theme_pov() + scale_color_pov() )
print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), fill=as.factor(year), color=as.factor(year), geom="bar", stat="identity", facets=year ~ . ) + theme_pov() + scale_color_pov() + scale_fill_pov() )

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], group=as.factor(year), color=as.factor(year), geom="line" ) + coord_polar() + theme_pov() + theme( axis.line=element_blank(), panel.grid = element_line(color = "black")) + scale_color_pov() )

print( qplot(week, influenza_ab/specimens, data=data[which(data$year>=start),], color=as.factor(year), geom="line" ) + theme_pov(legend.position = "none") + scale_color_pov() + scale_x_continuous(limits = c(1, 52), breaks = c(1,10,20,30,40,50,52), expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) )

print( qplot(index, influenza_ab/specimens, data=data[which(data$year>=start),], color=as.factor(year), geom="line" ) + theme_pov(legend.position = "none") + scale_color_pov() + scale_x_continuous(expand=c(0,0), breaks=((rep( c(0:5)*52, each=6) + rep( c(1,10,20,30,40,50), 6) )+index_delta-1), labels=(rep(c(1,10,20,30,40,50),6))) + scale_y_continuous(expand=c(0,0)) )
