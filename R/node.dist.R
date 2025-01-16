#' Calculate minimum distance between all pairs of nodes
#'
#' For a given network of nodes and distances, it will use Dijkstra’s algorithm via \code{\link[igraph:distances]{igraph}} to calculate the minimum distance between each pair of nodes following the river network.
#' @param x (data.frame) The \code{segement} object created using \code{\link{dist.per.segment}}
#' @return A data frame containing the name of the first node, name of the second node, and distance between them (in meters). The diagonal of the distance matrix is not returned, and there are no duplicate pairs.
#' @keywords internal
node.dist <- function(x){
  colnames(x)[4] <- "weight"
  net <- igraph::graph_from_data_frame(d = x[,2:4], directed = FALSE)
  dists <- igraph::distances(net, mode = "all", weights = E(net)$weight)

  ind <- which(upper.tri(dists, diag = F), arr.ind = TRUE)
  nn <- dimnames(dists)
  res <- data.frame(node1 = nn[[1]][ind[, 1]],
                    node2 = nn[[2]][ind[, 2]],
                    distance = dists[ind])
  res}
