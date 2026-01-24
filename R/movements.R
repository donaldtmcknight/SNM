#' Calculate movements
#'
#' Calculate movements in a stream network for multiple individuals
#'
#' @param data (list) Output from \code{\link{prep.data}}.
#' @param space.use (logical) If \code{TRUE} (default), total space use (range) will be calculated up to and including each point
#' @param from.previous (logical) If \code{TRUE} (default), the distance from the previous point will be calculated.
#' @param cumulative (logical) If \code{TRUE} (default), the cumulative distance moved up to and including each point will be calculated.
#' @param downstream.node (integer) The ID (number) of the node representing the furthest point downstream in your network (the root). If included, the distance from that point and the direction of movement (upstream or downstream) will be calculated per point.
#' @param coords (data.frame) Data frame containing coordinates for animal locations. At a minimum, it must include a column of animal IDs and columns of latitude and longitude (in decimal degrees projected to the same system as the rest of the data). It can optionally include a date.time column (highly recommended) containing the date and time of each point (in POSIX format). If the date.time column is present, the data will be sorted by that. Otherwise, ensure the data are sorted from oldest to newest. Other columns of metadata can be included and will be returned.
#' @param lon.name (character) For the \code{coords} object, the name of the column of longitudes (in quotes). Default = "lon"
#' @param lat.name (character) For the \code{coords} object, the name of the column of latitudes (in quotes). Default = "lat"
#' @param id.name (character) For the \code{coords} object, the name of the column of animal identities (in quotes). Default = "id"
#' @param date.time.name (character: optional) For the \code{coords} object, the name of the column of dates and times (in a POSIX format). If included, the data will be sorted from oldest to newest (otherwise sort before running), and the time difference between consecutive points will be returned. Default = \code{NULL}
#' @return Data frame including all original columns, plus a columns of summary data. Column order may have changed. All measurements are in linear meters.
#'
#' Column \code{space.use} = result of \code{space.use} = \code{TRUE}
#'
#' Column \code{dist.from.prev} = result of \code{from.previous} = \code{TRUE}
#'
#' Column \code{cumulative.dist} = result of \code{cumulative} = \code{TRUE}
#'
#' Column \code{dist.from.downstream} = result of \code{downstream.node}
#'
#' Column \code{direction} = result of \code{downstream.node} (direction)
#'
#' Column \code{time.diff} = returned if a date.time column is included. Shows the elapsed time (in hours) between consecutive points
#'
#' @details
#' It takes a data frame of coordinates and calculates the specified summary statistics for each individual. All results are in meters. Input data should be sorted from oldest point to newest, unless a date.time column is included, in which case the sorting will be done internally.
#'
#' \code{space.use} If \code{TRUE}, for each point, it will calculate the total space use up to and including the given point. This includes all branches of the network covered by the points, but no portion is measured more than once. It is simply the total area of the stream (expressed as linear meters) visited by the animal. Note that there may occasionally be a very slight difference between this metric and the other metrics when they describe the same space. This is caused by a slight misalignment between a node and the end of a segment, but usually will be trivial.
#'
#' \code{from.previous} If \code{TRUE}, for each point, it calculates the distance between that point and the previous point (following the stream network). This calculation is required for the \code{cummualtive} calculations and outputs will be returned anytime \code{cummualtive == T}, even if \code{from.previous == F}.
#'
#' \code{cumulative} If \code{TRUE}, for each point, it calculates the total distance moved up to and including that point. This differs from \code{space.use} because stretches of river get included each time they are visited, so the cumulative distance increases anytime the animal moves.
#'
#' \code{downstream.node} If this is included, for each point, it calculates the current distance from the point furthest downstream. This can be a useful way to visualize movements over time by referencing a fixed point (the furthest downstream). It also returns a column showing the direction of movement. Keep in mind that on branched rivers, these results may be somewhat misleading. If, for example, a stream forks 10 m upstream, and an animal moves from 5 m up the right fork then backtracks on goes 6 m up the left fork, the distances from the furthest downstream point will be 15 and 16 (respectively) even though the animal moved 5 m downstream the left fork before moving 6 m up the other. Likewise, the direction would be scored as "upstream" even though it first moved downstream. The direction is based simply on whether the shortest distance to the furthest downstream point increased or decreased.
#'
#' \strong{Note}: This function does not incorporate the amount of time between points. Bear that in mind when interpreting data collected over uneven time intervals.
#'
#' \strong{Example}: If an individual starts 100 meters upstream from the furthest downstream point in your network and moves 10 m upstream from day 1 to 2, another 5 m upstream from day 2 to 3, then 4 m downstream from day 3 to 4, then doesn't move between days 4 and 5, it will return the following:
#'
#' \tabular{ccccl}{
#' \strong{space.use} \tab \strong{dist.from.prev} \tab \strong{cumulative.dist} \tab \strong{dist.from.downstream} \tab \strong{direction} \cr
#' NA \tab NA \tab NA \tab 100 \tab NA        \cr
#' 10 \tab 10 \tab 10 \tab 110 \tab upstream  \cr
#' 15 \tab  5 \tab 15 \tab 115 \tab upstream  \cr
#' 15 \tab  4 \tab 19 \tab 111 \tab downstream\cr
#' 15 \tab  0 \tab 19 \tab 111 \tab no.change
#' }
#'
#' The first row is NA in most cases because there are no previous points to make comparisons. The \code{dist.from.prev} column is simply the distances described in the example. The \code{space.use} column shows increasing total space use over time. Notice that it does not change on days 4 and 5 because the animal visits areas where it had already been recorded. In contrast, the \code{cumulative.dist} increased on day 4 because it is simply the sum of all distances. Most values remained the same on the last day because there was no movement (as reflected by the \code{dist.from.prev} and \code{direction} columns).
#' @export
movements <- function(data=NULL,space.use=T,from.previous=T,cumulative=T, downstream.node=NULL,coords,lon.name="lon",lat.name="lat",id.name="id",date.time.name=NULL){


  line.dist<-data$dps.distances
  line<-data$increased.line
  node.dists<-data$node.distances
  seg.dist<-data$dps.segments
  seg.dist$distance <- seg.dist$distance+.00000001 #adds a very small pseudo count to prevent real distances from perfectly matching segment distances. This is necessary to avoid ties being falsely rejected in the space.use function
  nodes<-data$nodes

  colnames(coords)[which(colnames(coords)==lon.name)] <- "lon"
  colnames(coords)[which(colnames(coords)==lat.name)] <- "lat"
  colnames(coords)[which(colnames(coords)==id.name)] <- "id"
  if(is.null  (date.time.name)==F){colnames(coords)[which(colnames(coords)==date.time.name)] <- "date.time"
  coords <- coords[with(coords,order(id,date.time)),]}

  #remove critical NAs
  if(length(c(which(is.na(coords$lon)==T),which(is.na(coords$lat)==T),which(is.na(coords$id)==T))) > 0){
    coords <- coords[-c(which(is.na(coords$lon)==T),which(is.na(coords$lat)==T),which(is.na(coords$id)==T)),]}

  #set aside extra columns to keep
  if(ncol(coords)>3){
    keep <- as.data.frame(coords[,setdiff(colnames(coords),c("lon","lat"))])
    colnames(keep) <- setdiff(colnames(coords),c("lon","lat"))}else{keep <- NULL}

  coords <- coords[,c("lon","lat","id")]

  coords.id <- unique(coords$id)

  nodes <- as.data.frame(nodes)
  #if the downstream option is being used, extracts the coordinate for the furthest point downstream
  down.node <- if(is.null(downstream.node)==F){nodes[which(nodes$id==downstream.node),c("lon","lat")]}

  #loop over each individual
  space.use.a <- vector("list",length(coords.id)) #total space use
  from.prev.a <- vector("list",length(coords.id)) #distance from previous point
  from.downstream.a <- vector("list",length(coords.id)) #distance from furthest point downstream
  direction.a <- vector("list",length(coords.id))#direction (upstream or downstream)
  cumm.dist.a <- vector("list",length(coords.id))#cumulative distance moved
  if(is.null(date.time.name)==F){time.diffs.a <- vector("list",length(coords.id))}

  #subset per individual
  for(a in 1:length(coords.id)){

    id.a <- coords.id[a]

    print(paste("Analyzing individal",id.a))

    coords.a <- coords[which(coords$id==id.a),]

    if(is.null(date.time.name)==F){times.a <- keep[which(keep$id==id.a),"date.time"]}

    #seg.dist is the original, will not be altered
    seg.dist.ref <- seg.dist #a reference frame that will be built upon as the script runs
    seg.dist.map <- seg.dist #the frame that will actually be used for mapping
    colnames(seg.dist)[4] <- "weight"
    colnames(seg.dist.ref)[4] <- "weight"
    colnames(seg.dist.map)[4] <- "weight"

    max.node <- max(nodes$id)

    #makes vectors to fill per coordinate
    space.use.i <- vector("list",nrow(coords.a)) #total space use
    from.prev.i <- vector("list",nrow(coords.a)) #distance from previous point
    from.downstream.i <- vector("list",nrow(coords.a)) #direction (upstream or downstream)
    direction.i <- vector("list",nrow(coords.a)) #distance from furthest point downstream
    if(is.null(date.time.name)==F){time.diffs.i <- vector("list",nrow(coords.a))}

    #subset per coordinate
    for(i in 1:nrow(coords.a)){

      p.i <- coords.a[i,1:2]

      ################
      #time difference
      ################
      if(is.null(date.time.name)==F){
        time.diffs.i[[i]] <- if(i==1){NA}else{as.numeric(difftime(times.a[i],times.a[(i-1)],units="hours"))}
      }
      ################


      ################
      #space use
      ################

      if(space.use==T){ #do space use calculations
        p.id.i <- geosphere::distm(p.i,line[,1:2])#distances from p.i to line points
        seg.id.i <- line[which(p.id.i==min(p.id.i))[1],3] #extracts the segment ID of the closest point

        seg.i <- seg.dist[which(seg.dist$seg==seg.id.i),] #extract end nodes for relevant segment

        ref.node.i <- seg.i$node1 #reference node against which all points on this vertex will be measured

        #calculate distance from point to first node in segment
        dist1.i <- calc.stream.dist(p1 = unlist(p.i),
                                    p2 = nodes[which(nodes$id == ref.node.i),c("lon","lat")],
                                    data=data)

        seg.dist.ref.i <- seg.dist.ref[which(seg.dist.ref$segment==seg.id.i),] #extract rows from seg.dist.ref (this is all nodes currently involved in that segment)

        new.node.id.i <- max.node+i

        #make temporary seg entry first new node
        new.node1.i <- cbind.data.frame(segment=seg.i$segment[1],
                                        node1 = ref.node.i,
                                        node2 = new.node.id.i,
                                        weight=dist1.i)
        seg.dist.ref.i <- rbind.data.frame(seg.dist.ref.i,new.node1.i) #combines temporary segment data


        seg.dist.ref.i <- seg.dist.ref.i[order(seg.dist.ref.i$weight,decreasing=T),] #sorts by distance

        seg.dist.ref.i <- seg.dist.ref.i[which(seg.dist.ref.i $node1 == ref.node.i),] #subset to reference node

        seg.dist.ref.i$rank <- rank(seg.dist.ref.i$weight)

        #if the new node is the smallest distance from the reference node (rank = 1) then it needs to be added as the distance to the ref node, and the distance from node2 one place above it, to it, which is found by the distance above it minus its distance
        #if the new node is not the smallest, then its distances and nodes will be based on the nodes on either side of its rank
        #the current new node will get added to the seg.dist.ref object (so that there is a continuous record of nodes to the reference node), but the new nodes created via the two steps above go into the seg.dist.map object, while removing previous node records

        rank.i <- seg.dist.ref.i[which(seg.dist.ref.i$node2==new.node.id.i),"rank"] #rank of new node

        #checks whether there is a tied rank (indicating an exact point that has been previously used) and only proceeds if it is not a tied rank. If it is tied, it pulls the same total area as last time
        if( (floor(rank.i)-rank.i) != 0){space.use.i[[i]] <- space.use.i[[(i-1)]]}else{

          seg.dist.ref <- rbind.data.frame(seg.dist.ref,new.node1.i)#add to permanent record


          if(rank.i==1){ #if the new point is the closest one to the first node (the node against which its distance was calculated) then calculate the distance between it and the next point
            new.node2.i <- cbind.data.frame(segment=seg.i$segment[1],
                                            node1 = new.node.id.i,
                                            node2 = seg.dist.ref.i[which(seg.dist.ref.i$rank==2),"node2"],
                                            weight=seg.dist.ref.i[which(seg.dist.ref.i$rank==2),"weight"]-dist1.i)

            seg.dist.map.i <- rbind.data.frame(seg.dist.ref.i[-which(seg.dist.ref.i$rank>=2),1:4], #new node1 (remove old connections)
                                               seg.dist.map[which(seg.dist.map$node1 != ref.node.i & seg.dist.map$segment == seg.id.i),], #other nodes (remove old connections)
                                               new.node2.i) #new node 2
            seg.dist.map <- rbind.data.frame(seg.dist.map[-which(seg.dist.map$segment==seg.id.i),],seg.dist.map.i)}else{

              #this is the else for if rank.i == 1
              new.node1.i <- cbind.data.frame(segment=seg.i$segment[1],
                                              node1 = seg.dist.ref.i[which(seg.dist.ref.i$rank==(rank.i-1)),"node2"], #Id of node one position closer to reference
                                              node2 = new.node.id.i,
                                              weight=dist1.i-seg.dist.ref.i[which(seg.dist.ref.i$rank==(rank.i-1)),"weight"]) #distance to reference minus the distance to reference of the node one position closer

              new.node2.i <- cbind.data.frame(segment=seg.i$segment[1],
                                              node1 = new.node.id.i,
                                              node2 = seg.dist.ref.i[which(seg.dist.ref.i$rank==(rank.i+1)),"node2"], #Id of node one position further from reference
                                              weight=seg.dist.ref.i[which(seg.dist.ref.i$rank==(rank.i+1)),"weight"]-dist1.i) #distance to reference of the node one position further from new node, minus distance to reference for new node

              seg.dist.map.i <- rbind.data.frame(seg.dist.ref.i[-which(seg.dist.ref.i$rank>=2),1:4], #new node1 (remove old connections)
                                                 seg.dist.map[which(seg.dist.map$node1 != ref.node.i & seg.dist.map$node1 != new.node1.i$node1 & seg.dist.map$segment == seg.id.i),], #other nodes (remove old connections)
                                                 new.node1.i,
                                                 new.node2.i) #new node 2

              seg.dist.map <- rbind.data.frame(seg.dist.map[-which(seg.dist.map$segment==seg.id.i),],seg.dist.map.i)
            }#ends the else


          space.use.i[[i]]  <- if(i == 1){NA}else{
            net.i <- igraph::graph_from_data_frame(d = seg.dist.map[,2:4], directed = FALSE)
            igraph::E(net.i)$weight <- seg.dist.map$weight  # Assign weights to edges

            #remove any segments that are between original nodes (i.e., they do not contain animal points)
            originals.i <- which(seg.dist.map$node1 <= max.node & seg.dist.map$node2 <= max.node) #have to do this in two steps because it runs into issues
            seg.dist.sub.map.i <- if(length(originals.i) > 0){seg.dist.map[-originals.i,]}else{seg.dist.map}


            #extract paths based on those points (includes segments animals moved through but did not stop in)
            short.i <- igraph::shortest_paths(net.i, to= as.character(unique(c(seg.dist.sub.map.i$node1,seg.dist.sub.map.i$node2))),from = as.character(unique(c(seg.dist.sub.map.i$node1,seg.dist.sub.map.i$node2))))$vpath
            short.i <- unique(do.call("c",short.i))

            #create a subgraph based on a subset of nodes
            subgraph <- igraph::induced_subgraph(net.i, vids = short.i)

            #identify  and remove terminal nodes (these are leftovers that need trimmed)
            terminal.i <- igraph::degree(subgraph)
            terminal.i <- igraph::V(subgraph)[which(terminal.i ==1)]
            subgraph <- igraph::delete_vertices(subgraph,terminal.i)

            sum(igraph::E(subgraph)$weight)}#close else from if(i == 1)

        }#closes else from checking for tied ranks (integer)
      }#close space use section

      ###############
      #Movement from previous
      ###############
      if(from.previous==T | cumulative == T){

        from.prev.i[[i]] <- if(i==1){NA}else{
          calc.stream.dist(p1 = unlist(p.i),
                           p2 = unlist(coords.a[(i-1),1:2]),
                           data=data)}
      }#close from.previous

      ###############
      #Downstream data
      ###############

      if(is.null(downstream.node)==F){

        down.dist.i <- calc.stream.dist(p1 = unlist(p.i),
                                        p2 = unlist(down.node),
                                        data=data)

        from.downstream.i[[i]] <- down.dist.i
        direction.i[[i]] <- if(i == 1){NA}else{
          if(down.dist.i > from.downstream.i[[(i-1)]]){"upstream"}else{
            if(down.dist.i < from.downstream.i[[(i-1)]]){"downstream"}else{
              if(down.dist.i == from.downstream.i[[(i-1)]]){"no.change"}
            }
          }
        }
      } #closes downstream

    }   #closes the for loop over data points

    if(space.use==T){space.use.a[[a]] <- do.call("c",space.use.i)}
    if(from.previous==T | cumulative == T){
      from.prev.i <- do.call("c",from.prev.i)
      from.prev.a[[a]] <- from.prev.i}
    if(is.null(downstream.node)==F){
      from.downstream.a[[a]] <- do.call("c",from.downstream.i)
      direction.a[[a]] <- do.call("c",direction.i)}
    if(cumulative == T){cumm.dist.a[[a]] <- c(NA,cumsum(from.prev.i[-1]))}
    if(is.null(date.time.name)==F){time.diffs.a[[a]] <- do.call("c",time.diffs.i)}

  } #closes loop over individuals

  space.use.a <- if(space.use==T){do.call("c",space.use.a)}else{NULL}
  from.prev.a <- if(from.previous==T | cumulative == T){do.call("c",from.prev.a)}else{NULL}
  cumm.dist.a <- if(cumulative == T){do.call("c",cumm.dist.a)}else{NULL}
  from.downstream.a <- if(is.null(downstream.node)==F){do.call("c",from.downstream.a)}else{NULL}
  direction.a <- if(is.null(downstream.node)==F){do.call("c",direction.a)}else{NULL}
  time.diffs.a <- if(is.null(date.time.name)==F){do.call("c",time.diffs.a)}else{NULL}


  #make list so I can detect the nulls
  if(is.null(keep)==F){keep <- keep[,-which(colnames(keep)=="id")]}

  temp <- list(keep = keep,space.use = space.use.a,dist.from.prev=from.prev.a,cumulative.dist =  cumm.dist.a, dist.from.downstream=from.downstream.a,direction=direction.a,time.diff=time.diffs.a )

  final.res <- cbind.data.frame(do.call("cbind.data.frame",temp[which(sapply(temp,is.null)==F)]),coords)

  if(is.null(keep)==F){
    colnames(final.res)[1:ncol(keep)] <- colnames(keep)}

  final.res
}



