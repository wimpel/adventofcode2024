library(igraph)
input <- readLines("./input/day12_sample1.txt")
# input <- readLines("./input/day12_sample2.txt")
# input <- readLines("./input/day12_sample3.txt")
# input <- readLines("./input/day12.txt")

m <- strsplit(input, "") |>
  unlist() |> 
  matrix(ncol = nchar(input[[1]]), byrow = TRUE)

# Build initial matrix and lattice graph

labels <- as.vector(m)
g <- graph.lattice(dim(m))

lyt <- layout_nicely(g) #####

# Remove edges between elements of different types
edgelist <- as_edgelist(g)
retain <- labels[edgelist[,1]] == labels[edgelist[,2]]
g <- delete_edges(g, E(g)[!retain])

# Take a look at what we have
plot(g, vertex.label = labels, layout = lyt)

final <- data.table(label = labels,
           group = components(g)$membership,
           conn = 4 - degree(g)
           )
final[, col := rep(1:ncol(m), each = ncol(m))][]
final[, row := rep(1:nrow(m), nrow(m))][]

#setkey(final, group, col, row)
final

ans <- final[, .(n = .N, perm = sum(conn)), by = .(group)]
sum(ans$n * ans$perm)

final
m
library(sf)
final.sf <- st_as_sf(final, coords = c("row", "col"))
plot(final.sf)

plot(st_make_grid(final.sf,n = 5))

plot(final.sf[1])

mygrid <- sf::st_make_grid(final.sf,n = 5, cellsize = 1) |> 
  sf::st_as_sf()

mycents <- st_centroid(mygrid) |> st_as_sf() |> cbind(final)

mygrid2 <- st_as_sf(mygrid)


m
final
