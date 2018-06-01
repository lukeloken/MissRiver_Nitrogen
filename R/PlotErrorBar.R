# #############################################
# Error Bar Function
# This plots error bars on baseplots
# Luke Loken from Evan Childress
# #############################################

error.bar <- function(x, y, upper.y, 
                      lower.y=upper.y,upper.x,lower.x=upper.x,
                      length=0.03,x.bar=F,y.bar=T,...){
  if(length(x) != length(y) | length(y) !=length(lower.y) |
     length(lower.y) != length(upper.y))
  {stop("vectors must be same length")}
  
  if(y.bar==T){
    arrows(x,y+upper.y, x, y-lower.y, angle=90, 
           code=3, length=length, ...)}
  if(x.bar==T){
    arrows(x+upper.x,y,x-lower.x,y,angle=90,code=3,length=length,...)
  }}

