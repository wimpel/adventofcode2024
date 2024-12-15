library(data.table)
library(stringr)
library(ggplot2)

input <- readLines("./input/day14_sample.txt")
input <- readLines("./input/day14.txt")

L <- lapply(strsplit(input, " "), stringr::str_extract_all, pattern = "[-0-9]+")
m <- unlist(L) |> as.numeric() |> matrix(ncol = 4, byrow = TRUE)

boardwide <- 101
boardheigt <- 103
middlerow <- ceiling(boardheigt/2)
middlecol <- ceiling(boardwide/2)

position <- function(input, time) {
  x <- input[1]
  y <- input[2]
  vx <- input[3]
  vy <- input[4]
  posx <- x + 1 + vx * time
  posx2 <- posx - (ceiling(posx / boardwide) - 1) * boardwide
  posy <- boardheigt - y + -1 * vy * time
  posy2 <- posy - (ceiling(posy / boardheigt) - 1) * boardheigt
  return(c(posx2,posy2))
}


ans <- as.data.table(t(apply(m, 1, position, time = 100)))[, .(.N), by = .(x = V1, y = V2)]

ans[x < middlecol & y < middlerow, quadrant := 1 ]
ans[x > middlecol & y < middlerow, quadrant := 2 ]
ans[x < middlecol & y > middlerow, quadrant := 3 ]
ans[x > middlecol & y > middlerow, quadrant := 4 ]

ans[complete.cases(ans), sum(N), by = .(quadrant)]$V1 |> prod()

## part 2

test <- sapply(1:10000, function(i) {
  ans<- as.data.table(t(apply(m, 1, position, time = i)))[, .(.N), by = .(x = V1, y = V2)]
  sd(ans$x)
})

#mintest == 8006

final <- as.data.table(t(apply(m, 1, position, time = 8006)))
