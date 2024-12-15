library(stringr)
library(lpSolve)
library(bit64)

options(scipen=999)

input <- readLines("./input/day13_sample.txt")
input <- readLines("./input/day13.txt")

input <- readLines("./input/day13_sample2.txt")

buta <- sapply(stringr::str_extract_all(input[grepl("Button A", input)], "[+-][0-9]+"), as.integer) 
butb <- sapply(stringr::str_extract_all(input[grepl("Button B", input)], "[+-][0-9]+"), as.integer) 
goal <- sapply(stringr::str_extract_all(input[grepl("Prize", input)], "(?<=\\=)[\\-0-9]+"), as.character) 

stringr::str_extract_all(input[grepl("Prize", input)], "(?<=\\=)[\\-0-9]+")

goaltemp <- as.integer64(goal) |> matrix(nrow = 2)
goal


sapply(seq.int(ncol(buta)), function(i) {
  f.obj <- c(3, 1)
  f.con <- matrix(c(buta[1,i], butb[1,i], buta[2,i], butb[2,i],
                    1,0,0,1), nrow = 4, byrow = TRUE)
  f.dir <- c("=", "=", "<=", "<=")
  f.rhs <- c(goal[,i], 100000000000, 10000000000)
  sum(lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, all.int = TRUE)$solution * c(3,1))
  #lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:4, all.int = TRUE)$solution
}) |> sum()
