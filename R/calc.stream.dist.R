#' Calculate distance along a stream
#'
#' Calculate the shortest distance along a stream network between a pair of points
#' @param p1 (numeric vector) Longitude and latitude (in that order) of first point
#' @param p2 (numeric vector) Longitude and latitude (in that order) of second point
#' @param data (list) Output from \code{\link{prep.data}}.
#' @return (numeric) Distance (in meters) between the two points
#' @details
#' This is the primary function for calculating the shortest distance between two points along the stream network.
#'
#' @export
calc.stream.dist <- function(p1,p2,data){

  #extracts necessary objects from data list
    line.dist<-data$dps.distances
    line<-data$increased.line
    node.dists<-data$node.distances
    seg.dist<-data$dps.segments
    nodes<-data$nodes

  p1.d<- geosphere::distm(p1,line[,1:2])#distances from p1 to line points
  p1.dlp <- line[which(p1.d==min(p1.d))[1],] #extracts the ID of the closest point
  p2.d<- geosphere::distm(p2,line[,1:2])#distances from p2 to line points
  p2.dlp <- line[which(p2.d==min(p2.d))[1],] #extracts the ID of the closest point

  if(nrow(p1.dlp)>1){p1.dlp <- cbind.data.frame(lon=mean(p1.dlp[,1]),lat=mean(p1.dlp[,2]),id=p1.dlp[1,3])}
  if(nrow(p2.dlp)>1){p2.dlp <- cbind.data.frame(lon=mean(p2.dlp[,1]),lat=mean(p2.dlp[,2]),id=p2.dlp[1,3])}

  #note that the ID here is segments, not nodes

  res <-
    if(p1.dlp[1,1]==p2.dlp[1,1] & p1.dlp[1,2]==p2.dlp[1,2]){0}else{ #if both points are closest to the same point, the distance is 0

      if(p1.dlp[1,3]==p2.dlp[1,3]){ #if both points are from the same segment, does a simple measurement within that segment
        c_stream_dist(line.b.dist = line.dist[[which(names(line.dist)==p1.dlp[1,3])]],line.b = line[which(line$id==p1.dlp[1,3]),], p1.b = p1, p2.b=p2)}else{

          seg.nodes <- seg.dist[which(seg.dist$segment %in% c(p1.dlp[1,3],p2.dlp[1,3])),] #gives the nodes and data for the end segments in questions (need to determine the outermost node for each segment, relative to the other segment)

          #distances between all nodes involved. The max distance will give you the two nodes with the furthest separation, thus telling you the outside nodes
          node.distances <- node.dists[which(node.dists$node1 %in% c(seg.nodes$node1,seg.nodes$node2) & node.dists$node2 %in% c(seg.nodes$node1,seg.nodes$node2)),]
          outer.node.distances <- node.distances[which(node.distances$distance==max(node.distances$distance)),]

          if(nrow(node.distances)==3){ #if there are only 3 rows, then the points are in adjacent segments and need to be treated differently
            #identify the inner node
            inner.node <- setdiff(c(node.distances$node1,node.distances$node2),c(outer.node.distances$node1,outer.node.distances$node2))
            #get distance from each point to the inner node
            p1.dist <- c_stream_dist(line.b.dist = line.dist[[which(names(line.dist)==p1.dlp[1,3])]],line.b = line[which(line$id==p1.dlp[1,3]),], p1.b = p1, p2.b=nodes[which(nodes[,"id"]== inner.node),c("lon","lat")])
            p2.dist <- c_stream_dist(line.b.dist = line.dist[[which(names(line.dist)==p2.dlp[1,3])]],line.b = line[which(line$id==p2.dlp[1,3]),], p1.b = p2, p2.b=nodes[which(nodes[,"id"]== inner.node),c("lon","lat")])
            sum(p1.dist,p2.dist) #sum distances
          }else{#end if (nrow(node.distances)==3)
            #distance between the inner nodes of the segments in question (i.e., the distance covered by all segments between the two where the poitns fall)
            inner.node.distances <- node.distances[-which(node.distances$node1 %in% c(outer.node.distances[,1],outer.node.distances[,2]) | node.distances$node2 %in% c(outer.node.distances[,1],outer.node.distances[,2])),]

            #subsets to node and segment info for segment where first point is found
            p1.inner.node <- seg.nodes[which(seg.nodes$segment==p1.dlp[1,3]),]
            #identifies the inner node
            p1.inner.node <- intersect(c(p1.inner.node$node1,p1.inner.node$node2),c(inner.node.distances$node1,inner.node.distances$node2))
            #calculate the distance between the first point and the inner node of its segment
            p1.dist <- c_stream_dist(line.b.dist = line.dist[[which(names(line.dist)==p1.dlp[1,3])]],line.b = line[which(line$id==p1.dlp[1,3]),], p1.b = p1, p2.b=nodes[which(nodes[,"id"]== p1.inner.node),c("lon","lat")])

            #subsets to node and segment info for segment where second point is found
            p2.inner.node <- seg.nodes[which(seg.nodes$segment==p2.dlp[1,3]),]
            #identifies the inner node
            p2.inner.node <- intersect(c(p2.inner.node$node1,p2.inner.node$node2),c(inner.node.distances$node1,inner.node.distances$node2))
            #calculate the distance between the second point and the inner node of its segment
            p2.dist <- c_stream_dist(line.b.dist = line.dist[[which(names(line.dist)==p2.dlp[1,3])]],line.b = line[which(line$id==p2.dlp[1,3]),], p1.b = p2, p2.b=nodes[which(nodes[,"id"]== p2.inner.node),c("lon","lat")])

            #calculates total distance by summing the distances within each segment and the total distances of the segments between those segments
            sum(p1.dist,p2.dist, inner.node.distances[,3])} #end else{} from more than 3 rows

        }#ends else if points are on different segments

    }#ends else from seeing if points are from the same segment

  res}#closes function
