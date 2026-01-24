#' Prepare data
#'
#' Wrapper function to prepare stream data for analyses
#' @param l (shapefile) A projected polyline loaded with st_read(). Should include an id variable  specifying different segments of the stream (this should be included even if there is only a single segment)
#' @param freq (numeric) The distance (in meters) to add points. Default = 1
#' @param nodes (data.frame) A data frame of coordinates for the nodes where stream segments meet or end. Should contain columns for latitude, longitude, and the identity of each node (\strong{identities must be integers and go from 1 to number of nodes)}
#' @param lon.name (character) Name of the column of longitudes (in quotes). Default = "lon"
#' @param lat.name (character) Name of the column of latitudes (in quotes). Default = "lat"
#' @param node.name (character) Name of the column of segment identities (in quotes). Default = "id"
#' @details
#' Takes a shapefile (polyline) of the stream (including all segments (i.e., edges), with each segment having a different ID) as well as a data frame of node locations. It then adds points at the specified interval (to increase accuracy) and does a series of calculations to build reference tables of distances along segments and among nodes.
#'
#' Segments (edges) are defined as any section between two nodes with no nodes between them. Nodes occur at each start, end, or intersection of segments.
#'
#' This is the only function needed to prepare data for analyses such as \code{\link{calc.stream.dist}}, \code{\link{movements}}, and \code{\link{dist.over.time}}
#'
#' \strong{Note:} All data MUST be in the same projected system. These functions are intended for data formatted in decimal degrees.
#'
#' \strong{Do not use braided streams}. If a stream is braided (i.e., channels split then reconnect forming islands) the calculations will be incorrect. If you are working in a braided system, but all of your known movements do not include the braids, simply use a shapefile that does not include the braids (e.g., a study that only tracks in the main channel and largest side channels, without tracking in braided channels)
#'
#' @return A list containing five objects:
#'
#' \code{nodes} = (data.frame) Coordinates for the nodes where stream segments meet or end.
#'
#' \code{increased.line} = (data.frame) Coordinates describing the center line of the stream with an increased frequency of points
#'
#' \code{dps.segments} = (data.frame) Total distances of each segment and the nodes defining that segment
#'
#' \code{dps.distances} = (list) List of data frames (one data frame per segment) with the distances between each consecutive pair of points
#'
#' \code{node.distances} = (data.frame) Minimum distance between each pair of nodes (not simply nodes on a given segment)
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph degree
#' @importFrom igraph V
#' @importFrom igraph delete_vertices
#' @importFrom igraph E
#' @importFrom igraph distances
#' @importFrom igraph mst
#' @importFrom sf st_coordinates
#' @importFrom geosphere bearing
#' @importFrom geosphere distm
#' @importFrom geosphere destPoint
#' @export
prep.data <- function(l, freq = 1,nodes,lon.name="lon",lat.name="lat",node.name="id"){

  #step 1: rename the columns in the node object
  colnames(nodes)[which(colnames(nodes)==lon.name)] <- "lon"
  colnames(nodes)[which(colnames(nodes)==lat.name)] <- "lat"
  colnames(nodes)[which(colnames(nodes)==node.name)] <- "id"


  #step 2: add points to the center line
  increased.line <- increase.stream.points(l= l,freq=freq)

  #step 3: calculate distances per segment
  dps <- dist.per.segment(network = increased.line,nodes=nodes,lon.name="lon",lat.name="lat",node.name="id")
  dps.segments <- dps[[1]]
  dps.distances <- dps[[2]]

  #step 4: get distances between each pair of nodes
  n.dist <- node.dist(dps.segments)

  #step 5: combine into a list

  list(nodes=nodes,increased.line = increased.line, dps.segments = dps.segments, dps.distances = dps.distances ,node.distances = n.dist)

}
