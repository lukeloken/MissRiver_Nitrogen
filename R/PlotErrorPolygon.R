# Plot error polygon

error.polygon <- function(x, y, upper.y, 
                      lower.y=upper.y,...){
  if(length(x) != length(y) | length(y) !=length(lower.y) |
     length(lower.y) != length(upper.y))
  {stop("vectors must be same length")}
  polygon(c(x, rev(x)),c(y+upper.y, rev(y-lower.y)),...)
}
