#' Segment a data frame based on a numeric vector
#'
#' Returns a data frame 
#'
#' @param x A matrix or data frame with numeric time series in each column
#' @param y A numeric vector used to segment columns of \code{x}. Must be
#'          the same length as the columns of \code{x}
#' @param labels A dataframe with factor label(s). Labels will be 
#'               be added to each observation in the output dataframe.
#' @param func A function to apply to each segment of each column. 
#'
#' @return df A dataframe with each corresponds to the supplied function
#'            applied to the segmented data along with the label
#' @export
#' @importFrom ecomplex palarm
#' @importFrom dplyr "%>%"
segment <- function(x, y, labels = NULL, func = mean){
  if(!is.data.frame(x)) x <- data.frame(x)
  if(!is.data.frame(labels)) labels <- data.frame(labels)
  # labels <- unlist(lapply(labels, factor)) %>% data.frame
  seg <- palarm(y)
  len <- dim(x)[1]
  change_pts <- add_end_pts(seg$kout, len)
  ranges <- segment_ranges(change_pts)
  if(length(unlist(ranges))!= len){
    stop("Length of segments does not equal original length")
  }
  ret <- segment_df(x, ranges, labels, func) 
  ret 
}

# Apply function 'func' to segmented columns.
# Return a data frame with weights and labels appended
segment_df <- function(df, ranges, labels = NULL, func){
  # Add weight column 
  weights <- unlist(lapply(ranges, length))/sum(unlist(lapply(ranges, length)))
  weights <- data.frame(weights) 
  # Repeat metadata rows 
  labeldf  <- data.frame(labels[rep(1, length(ranges)), ])
  names(labeldf) <- names(labels)
  metadata <- cbind(labeldf, weights)  
  ret <- lapply(ranges, function(x) apply(df[x, ], 2, func)) 
  ret <- do.call(rbind, ret)  
  ret <- data.frame(ret, metadata)
  row.names(ret) <- NULL
  ret  
}

# Return list of ranges based on change_pts
segment_ranges <- function(change_pts){
  starts <- change_pts[-length(change_pts)] 
  ends <- change_pts[-1]-1
  ends[length(ends)] <- change_pts[length(change_pts)]
  lapply(1:length(starts), function(k) starts[k]:ends[k])
}

# Add first and last point to change points
add_end_pts <- function(x, len){
  if(!(1 %in% x)) { x <- append(x, 1)}
  if(!(len %in% x)) { x <- append(x, len)}
  sort(x)
}
