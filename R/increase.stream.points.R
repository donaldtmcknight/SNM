#' Add points to a line
#'
#' Takes a polyline shapefile and adds points along the line between the existing points. Thus, a long straight section with only one point on either end will have points added along its length. Increasing the number of points will improve the accuracy of distance calculations but will greatly reducing the speed of all functions.
#' @param l (shapefile) A projected polyline loaded with st_read(). Should include an id variable specifying different segments of the stream (this should be included even if there is only a single segment)
#' @param freq (numeric) Default = 1. The distance (in meters) to add points.
#' @return A data frame of points consisting of three columns: lon (longitude), lat (latitude), id = stream segment identities (from the input shapefile)
#' @details
#' If you encounter a "LINESTRING EMPTY" warning, it means that your input shapefile had an ID for a segment that contained no data, and that segment was removed.
#'
#' @keywords internal
increase.stream.points <- function(l,freq=1){
  colnames(l)[1] <- "id"
  ids <- unique(l$id)
  res <- vector("list",length(ids))
  for(a in 1:length(ids)){
    id.a <- ids[a]

    #subset to a segment and reformat
    coords <- l[which(l$id==id.a),]
    coords <- sf::st_coordinates(coords)[,1:2]
    #check for and remove missing data
    if(nrow(coords)==0){warning(paste("LINESTRING EMPTY for segment",id.a,"removing that segment because there are no coordinates in it"))}else{

      new.line <- vector("list", nrow(coords))
      new.line[[1]] <- coords[1,]

      for(i in 2:nrow(coords)){ #make new points between each vertex
        bearing.i <- geosphere::bearing(coords[(i-1),],coords[i,])# bearing between points
        dist.i <- geosphere::distm(coords[(i-1),],coords[i,]) #distance between points
        new.line[[i]] <- if(dist.i > freq){  #sequence of distances to space new points
        rbind(geosphere::destPoint(coords[(i-1),],bearing.i,seq(freq,dist.i,freq)),coords[i,])}else{coords[i,]}} #if the distance between the two sets of corrdinates is smaller than the distances being added, it just keeps the originals
      new.line <- do.call("rbind.data.frame",new.line) #combine results
      new.line$id <- id.a
      res[[a]] <- new.line}
  }#end else{}

  res <- do.call("rbind.data.frame",res)
  res <- res[duplicated(res)==F,] #removes duplicates
  res}
