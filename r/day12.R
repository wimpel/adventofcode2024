library(igraph)
library(sf)

# input <- readLines("./input/day12_sample1.txt")
# input <- readLines("./input/day12_sample2.txt")
# input <- readLines("./input/day12_sample3.txt")
# input <- readLines("./input/day12_sample4.txt")
input <- readLines("./input/day12.txt")
m <- strsplit(input, "") |>
  unlist() |> 
  matrix(ncol = nchar(input[[1]]), byrow = TRUE)

# part 1
labels <- as.vector(m)
g <- graph.lattice(dim(m))
# Remove edges between elements of different types
edgelist <- as_edgelist(g)
retain <- labels[edgelist[,1]] == labels[edgelist[,2]]
g <- delete_edges(g, E(g)[!retain])
# Take a look at what we have
#plot(g, vertex.label = labels, layout = lyt)
final <- data.table(label = labels,
           group = components(g)$membership,
           conn = 4 - degree(g)
           )
final[, col := rep(1:ncol(m), each = ncol(m))][]
final[, row := rep(1:nrow(m), nrow(m))][]
sum(final[, .(.N * sum(conn)), by = .(group)]$V1)

## part 2
final.sf <- st_as_sf(final, coords = c("row", "col"))
mygrid <- sf::st_make_grid(final.sf, n = ncol(m), cellsize = 1) |> 
  sf::st_as_sf() |>
  cbind(final)
# split by group
L <- split(mygrid, mygrid$group)
edges <- sapply(L, function(x) {
  points <- x |> st_union() |> st_simplify() |> st_cast("LINESTRING")
  sapply(split(points, f = 1:length(points)), function(x) st_cast(x, "POINT") |> length() - 1) |> sum()
})
(sapply(L, nrow) * edges) |> sum()

