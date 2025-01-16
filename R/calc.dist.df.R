#this function takes the output of the increase.stream.points function and returns a data frame with the distance from point 1 to 2, 2 to 3, 3, to 4, etc
calc.dist.df <- function(coords){
  coords <- as.data.frame(coords)
  coords$p2 <- c(1:nrow(coords))
  distances <- vector("list",(nrow(coords)-1))

  for(i in 2:nrow(coords)){
    dist.i <- distm(coords[(i-1),1:2],coords[i,1:2])
    dist.i <- cbind(dist.i,coords[(i-1),3],coords[i,3])
    distances[[(i-1)]] <- dist.i}#closes for i
  distances <- do.call(rbind.data.frame,distances)
  colnames(distances) <- c("distance","p1","p2")
  distances}#closes function
