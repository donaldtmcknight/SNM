#' @keywords internal
#' line.b.dist (data.frame) Output for a single segment from the \code{\link{dist.per.segment}} function
#' line.b (data.frame) Output from \code{\link{increase.stream.points}} subset to a single stream segment
#' p1.b (numeric vector) Longitude and latitude of first point (within segment specified by line.b.dist and line.b)
#' p2.b (numeric vector) Longitude and latitude of second point (within segment specified by line.b.dist and line.b)
#' Return (numeric) Distance (in meters) between the two points
c.stream.dist <- function(line.b.dist,line.b,p1.b,p2.b){
  p1.b.d<- distm(p1.b,line.b[,1:2])#distances from p1.b to line.b points
  p1.b.dlp <- which(p1.b.d==min(p1.b.d)) #extracts the ID of the closest point
  p2.b.d<- distm(p2.b,line.b[,1:2])#distances from p2.b to line.b points
  p2.b.dlp <- which(p2.b.d==min(p2.b.d)) #extracts the ID of the closest point


  for(i in 1:length(p1.b.dlp)){
    for(a in 1:length(p2.b.dlp)){
      if(p1.b.dlp[i]==p2.b.dlp[a]){res.a <- 0} #if both points are closest to the same point, the distance is 0

      if(p1.b.dlp[i]<p2.b.dlp[a]){ #if point1 is smaller than point 2, then this corresponds to the matching row# for p1.b and the matching column3 positoon for p2.b
        res.a <- sum(line.b.dist[p1.b.dlp[i]:which(line.b.dist[,3]==p2.b.dlp[a]),1])}

      if(p1.b.dlp[i]>p2.b.dlp[a]){ #if things go the opposite direction, it reverses the pattern
        res.a <- sum(line.b.dist[p2.b.dlp[i]:which(line.b.dist[,3]==p1.b.dlp[a]),1])}

      res.f.a <- if(a==1){res.a}else{c(res.f.a,res.a)}
      remove(res.a)
    }#closes for a
    res.i <- if(i==1){res.f.a}else{c(res.i,res.f.a)}
  }#closes for(i)
  res <- mean(res.i)

  res}#closes function
