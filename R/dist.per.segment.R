#' Calculate distances of stream segments
#'
#' For each segment of a stream, it calculates the distance between each pair of consecutive points and the total distance of that segment (in meters)
#' @param network (data.frame) A data frame with the the following columns (in this order): "lon", "lat", "id" (the output of \code{\link{increase.stream.points}})
#' @param nodes (data.frame) A data frame of coordinates for the nodes where stream segments meet. Coordinates should be in the same projected system as the input to \code{\link{increase.stream.points}}. Should contain columns for latitude, longitude, and identity of each node (\strong{identities must be integers and go from 1 to number of nodes}, but they do not need to correspond with anything in the input to \code{\link{increase.stream.points}})
#' @param lon.name (character) Name of the column of longitudes (in quotes). Default = "lon"
#' @param lat.name (character) Name of the column of latitudes (in quotes). Default = "lat"
#' @param node.name (character) Name of the column of segment identities (in quotes). Default = "id"
#' @return A list containing two objects:
#'
#' \code{segments} = the first object: a data frame with the id of each segment, nodes at either end of the segment, and the total distance (in meters) of that segment.
#'
#' \code{all.distances} = the second object: a list, with one data frame per segment containing the distances between each consecutive pair of points along the line.
#' @keywords internal
dist.per.segment <- function(network,nodes,lon.name="lon",lat.name="lat",node.name="id"){

  ids <- unique(network$id) #extract unique segment ids

  segment.distances <- vector("list",length(ids)) #make list to store all pairwise distances per segment
  names(segment.distances) <- ids #name the list based on the ids

  res <- vector("list",length(ids)) #list to fill with summary outputs

  for(a in 1:length(ids)){#loop per network segment

    seg.a <- network[which(network$id==ids[a]),]
    dist.a <- calc.dist.df(seg.a[,1:2]) #calculate the distances between consecutive points in a segment
    segment.distances[[a]] <- dist.a #add to list

    dist.total.a <- sum(dist.a$distance) #get total distance of segment
    dist.node1.a <- geosphere::distm(nodes[,c(lon.name,lat.name)],seg.a[1,1:2]) #distances from start of segment to nodes
    dist.node2.a <- geosphere::distm(nodes[,c(lon.name,lat.name)],seg.a[nrow(seg.a),1:2]) #distances from end of segment to nodes

    res[[a]] <- cbind.data.frame(ids[a],
                                 nodes[which(dist.node1.a==min(dist.node1.a)),node.name],
                                 nodes[which(dist.node2.a==min(dist.node2.a)),node.name],
                                 dist.total.a)}
  res <- do.call("rbind.data.frame",res)
  colnames(res) <- c("segment","node1","node2","distance")
  res2 <- list(res,segment.distances)
  names(res2) <- c("segments","all.distances")
  res2
}
