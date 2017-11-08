#
# Key driver analysis #
#

#' Performs true driver analysis.
#' 
#' This is a thin wrapper around relaimp(), returning a list with results and key indicators.
#' 
#' By default, uses the pratt method, which is fast but can yield negative coefficients. The pmvd method (proportionate marginal variance decomposition) is better but expoentially slower depending on number of covariates. 
#' 
#' @param model A model formula
#' @param data A dataframe
#' @param impLabels Character vector with labels for importance elements. Defaults to 
#' @param type Passed to \code{\link[relaimpo]{calc.relimp}}
#' @param meansFunction Vector of length 2, containing functions to use for calculating midpoints of importance and satisfaction. Defaults to median and mean.
#' @return A list:
#' \item{vinflation}{Variance inflation factor, see \code{\link[car]{vif}}}
#' \item{impsats}{A data frame with columns for labels, imp (importance) and sats (satisfaction)}
#' \item{means}{A list with two elements: \code{means} are the midpoints of imp and sats, while \code{meansFunction} contain the functions used to calculate the midpoint (typically \code{median} and \code{mean})}
#' @importFrom car vif
#' @export 
#' @examples 
#' data(diamonds, package="ggplot2")
#' x <- truedriver(price~cut+color+carat+clarity, data=diamonds, type="pmvd")
#' plot(x)
#' 
#' x <- truedriver(price~., data=diamonds[, c("price", "cut", "color", "clarity", "carat")])
#' plot(x)
truedriver <- function(model, data, impLabels=labels(terms(model, data=data)), 
    type=c("pratt", "pmvd"), meansFunction=c("median", "mean")){
	
  stopifnot(is.formula(model))
  
  data <- model.frame(model, data=data)
#	require(relaimpo)
  type <- match.arg(type)
	#require(car)
	net_score <- NULL#; rm(net_score)  ### Trick to fool R CMD check
	tdata <- data
  # Calculate net satisfaction score
  sats <- data.frame(
            sats=sapply(tdata[, -1], netScore),
            row.names=names(tdata[, -1])
          )

  tdata <- as.data.frame(llply(tdata, as.numeric))
  #str(tdata)

  fit <- lm (model, data=tdata)
  #summary(fit)

  #vif(fit) # variance inflation factors (to evaluate collinearity)
  vinflation <- sqrt(car::vif(fit)) # problem with collinearity?

  # Calculates relative importance using Pratt's importance measure

  imp <- switch(type,
      pmvd = relaimpo::calc.relimp(fit, type="pmvd", rela=TRUE)$pmvd,
      pratt = relaimpo::calc.relimp(fit, type="pratt", rela=TRUE)$pratt,
      stop("type not found")
  )
	impsats <- data.frame(
      labels=impLabels,
     imp=imp,
     sats
  )

  mean_imp  <- match.fun(meansFunction[1])(impsats$imp)
  mean_sats <- match.fun(meansFunction[2])(impsats$sats)
  
  means <- list(
      values=c(mean_imp, mean_sats),
      functions=meansFunction
  )
              
#  row.names(impsats) <- impLabels

	ret <- list(
      vinflation = vinflation,
			impsats    = impsats,
      means      = means
  )
  class(ret) <- "truedriver"
  ret
}


#' Combines graph parameters into a list.
#' 
#' Combines graph parameters into a list.
#' @param xBreaks vector with breaks for x axis
#' @param yBreaks vector with breaks for y axis
#' @param xLim vector with limits for coord_cartesian x-axis
#' @param yLim vector with limits for coord_cartesian x-axis
#' @param xLabel x axis label
#' @param yLabel y axis label
#' @param xlogScale If TRUE will plot x-axis on log scale
#' @export 
as.GraphParameter <- function( 
    xBreaks = c(0, 1),
    yBreaks = c(-1, 1),
    xLim = c(min(c(xBreaks, -0.1)), max(xBreaks)),
    yLim = c(min(yBreaks), max(yBreaks)),
    xLabel = "Relative importance",
    yLabel = "Net satisfaction",
    xlogScale = FALSE
){
  list(
    xBreaks = xBreaks,
    yBreaks = yBreaks,
    xLim = xLim,
    yLim = yLim,
    xLabel = xLabel,
    yLabel = yLabel,
    xlogScale = xlogScale
  )
}


	
#------------------------------------------------------------------------------	
	
#' Plots true driver graph.
#' 
#' Plots true driver graph.  
#' 
#' @param x A truedriver object 
#' @param GraphParameters A list with graphics parameters.  See also \code{\link{as.GraphParameter}}
#' @param meansFunction Vector of length 2, containing functions to use for calculating midpoints of importance and satisfaction. Defaults to median and mean.
#' @param means Numeric vector of length 2. First element is mean of importance, second element is mean of satisfaction. If null, defaults to calculated means of data 
#' @param ... passed to ggplot opts
#' @method plot truedriver
#' @return A ggplot object
#' @seealso \code{\link{truedriver}}
#' @export 
plot.truedriver <- function(x, GraphParameters=as.GraphParameter(), meansFunction=c("median", "mean"), means=NULL, ...){
  stopifnot(require(ggplot2))
  stopifnot(class(x)=="truedriver")
  impsats <- x$impsats
  
  if(missing(means) || is.null(means)){
    mean_imp  <- match.fun(meansFunction[1])(impsats$imp)
    #median_imp  <- median(impsats$imp)
    mean_sats <- match.fun(meansFunction[2])(impsats$sats)
    #median_sats <- median(impsats$sats)
  } else {
    mean_imp <- means[1]
    mean_sats <- means[2]
  }
  
	xBreaks <- c(GraphParameters$xBreaks[1],
			0,
#			mean(impsats$imp),
#			median(impsats$imp),
      mean_imp,
			GraphParameters$xBreaks[2])
	
	yBreaks <- c(GraphParameters$yBreaks[1],
			0,
#			mean(impsats$sats),
      mean_sats,
			GraphParameters$yBreaks[2])
  
  xLim <- GraphParameters$xLim
  yLim <- GraphParameters$yLim
  
	hvline <- data.frame(vline=mean(impsats$imp),
			hline=mean(impsats$sats))
	
  sats_label <- data.frame(x=max(xBreaks), y=mean_sats)
  imp_label  <- data.frame(x=mean_imp, y=min(yBreaks))
    
	p <- ggplot(impsats, aes_string(x="imp", y="sats"), colour="blue") +
			geom_point(colour="blue", fill="blue") +
			geom_text(aes_string(label="labels"), vjust=1, hjust=-0.1, size=3) +
			xlab(GraphParameters$xLabel) +
			ylab(GraphParameters$yLabel) +
			
			geom_hline(yintercept=mean_sats, colour="blue") +
			geom_text(aes_string(x="x", y="y"), label="Average satisfaction", data=sats_label,  
					size=3, hjust=1.1, vjust=1.1) +
      
			geom_vline(xintercept=mean_imp, colour="blue") +
			geom_text(aes_string(x="x", y="y"), label="Median importance", data=imp_label,
					size=3, vjust=1.1, hjust=-0.1, angle=90) +
			
      coord_cartesian(
          xlim=c(min(xLim), max(xLim)),
          ylim=c(min(yLim), max(yLim))
      ) +
      scale_x_continuous(
					breaks=xBreaks,
					minor_breaks=NA,
					formatter="percent"
			) +
			scale_y_continuous(
					breaks=yBreaks,
      minor_breaks=NA,
      formatter="percent"
      ) +
      opts(...)
  
  #if(GraphParameters$xlogScale) p <- p + scale_x_log()
	p
}

#------------------------------------------------------------------------------ 

#' Plots segmental true driver graph.
#' 
#' This is a thin wrapper around relaimp(), returning a list with results and key indicators
#'
#' @inheritParams truedriver 
#' @param impsats A dataframe
#' @param GraphParameters Result of \code{\link{as.GraphParameter}}
#' @param means Numeric vector of length 2. First element is mean of importance, second element is mean of satisfaction. If null, defaults to calculated means of data
#' @param facet Logical. If TRUE, creates plot with a facet for each segment, otherwise creates single facet with lines connecting observations. 
#' @param ... Passed to ggplot opts
#' @return A list
#' @export 
plot_truedriver_segmental <- function(impsats, GraphParameters, meansFunction=c("median", "mean"), 
    means=NULL, facet=FALSE, ...){
  require(reshape2)
  
  if(is.null(means) || missing(means)){
    mean_imp  <- match.fun(meansFunction[1])(impsats$imp)
    mean_sats <- match.fun(meansFunction[2])(impsats$sats)
  } else {
    mean_imp  <- means[1]
    mean_sats <- means[2]
  }
  
  xBreaks <- c(GraphParameters$xBreaks[1],
			0,
      mean_imp,
			GraphParameters$xBreaks[2])
	
	yBreaks <- c(GraphParameters$yBreaks[1],
			0,
      mean_sats,
			GraphParameters$yBreaks[2])
	hvline <- data.frame(vline=mean_imp, hline=mean_sats)
	
	
	linedata <- cast(melt(impsats, id.vars=c("segment", "labels")), labels~variable+segment)
	names(linedata) <- c("labels", "x1", "x2", "y1", "y2")
	
  if(!facet){
  	p <- ggplot() +
  			geom_segment(data=linedata, aes_string(x="x1", y="y1", xend="x2", yend="y2"), colour="grey50") +
  			geom_point(data=impsats, aes_string(x="imp", y="sats", colour="segment", fill="segment"), size=3) +
  			geom_text(data=linedata, aes_string(x="x1", y="y1", label="labels"), vjust=1.1, hjust=-0.1, size=3) +
  			xlab(GraphParameters$xLabel) +
  			ylab(GraphParameters$yLabel) +
  			
  			geom_hline(yintercept=mean_sats, colour="blue") +
  			geom_text(x=max(xBreaks), y=mean_sats, label="Average satisfaction", 
  					size=3, hjust=1.1, vjust=1.1, colour="blue") +
  			
  			geom_vline(xintercept=mean_imp, colour="blue") +
  			geom_text(x=mean_imp, y=min(yBreaks), label="Average importance", 
  					size=3, vjust=-0.1, hjust=-0.1, colour="blue", angle=90) +
  			
  			scale_x_continuous(
  					breaks=xBreaks,
  					minor_breaks=NA,
  					formatter="percent"
  			) +
  			scale_y_continuous(
  					breaks=yBreaks,
  					minor_breaks=NA,
  					formatter="percent"
  			) +
  			coord_cartesian(
  					xlim=c(min(xBreaks), max(xBreaks)),
  					ylim=c(min(yBreaks), max(yBreaks))
  			) +
        opts(...)
    }
    if(facet){
      #browser()
      p <- ggplot(impsats, aes(x=imp, y=sats)) + 
          geom_point(aes(colour=segment)) +  
          geom_text(aes(label=labels), vjust=1.1, hjust=-0.1, size=3) +
          xlab(GraphParameters$xLabel) +
          ylab(GraphParameters$yLabel) + 
          facet_grid(~segment) +
          
          geom_hline(yintercept=mean_sats, colour="blue") +
#          geom_text(x=max(xBreaks), y=mean_sats, label="Average satisfaction", 
#              size=3, hjust=1.1, vjust=1.1, colour="blue") +
          
          geom_vline(xintercept=mean_imp, colour="blue") +
#          geom_text(x=mean_imp, y=min(yBreaks), label="Average importance", 
#              size=3, vjust=-0.1, hjust=-0.1, colour="blue", angle=90) +
          
          scale_x_continuous(
              breaks=xBreaks,
              minor_breaks=NA,
              formatter="percent"
          ) +
          scale_y_continuous(
              breaks=yBreaks,
              minor_breaks=NA,
              formatter="percent"
          ) +
          coord_cartesian(
              xlim=c(min(xBreaks), max(xBreaks)),
              ylim=c(min(yBreaks), max(yBreaks))
          ) +
          opts(...) +
          opts(legend.position="none")
    }
    p
}

#------------------------------------------------------------------------------ 

#' Combines result from two true driver analyses.
#' 
#' This combines the result of two separate true drivera analyses into 
#' a single dataframe, to plot using \link{plot_truedriver_segmental}
#' 
#' @param td1 A dataframe
#' @param td2 A data frame
#' @param segments Segments
#' @return A list
#' @export 
combine_truedriver <- function(td1, td2, segments){
	td1 <- data.frame(td1, segment=rep(segments[1], nrow(td1)))
	td2 <- data.frame(td2, segment=rep(segments[2], nrow(td2)))
	rbind(td1, td2)
}

#------------------------------------------------------------------------------ 

#' Calculates a net score.
#'
#' This assumes that x is an ordered factor, from low to high
#' It then calculates a net percentage score
#' 
#' @param x An ordered factor
#' @export 
netScore <- function(x){
  
  x <- x[!is.na(x)]
  
  xu <- unclass(x)
  m <- ceiling(length(levels(x))/2)
  below <- length(xu[xu < m])
  above <- length(xu[xu > m])
  net <- above - below
  netScore <- net / length(x)
  netScore
}



