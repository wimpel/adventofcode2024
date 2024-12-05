library(stringr)
library(data.table)
#input <- readLines("./input/day04_sample.txt")
input <- readLines("./input/day04.txt")

input <- readLines("./input/aoc-2024-day-04-challenge-1.txt")
# part 1
m <- strsplit(input, "") |>
  unlist() |> 
  matrix(ncol = nchar(input[[1]]), byrow = TRUE)
# create strings to look for christmas
hor <- apply(m, 1, paste0, collapse = "")
hor.rev <- apply(m, 1, function(x) paste0(rev(x), collapse = ""))
ver <- apply(m, 2, paste0, collapse = "")
ver.rev <- apply(m, 2, function(x) paste0(rev(x), collapse = ""))
# diagonals
d <- row(m) - col(m)
d2 <- row(m) + col(m)
diag1 <- sapply(split(m, d), paste0, collapse = "") #\
diag1.rev <- sapply(split(m, d),function(x) paste0(rev(x), collapse = "")) #\
diag2 <- sapply(split(m,d2), paste0, collapse = "") #/
diag2.rev <- sapply(split(m, d2),function(x) paste0(rev(x), collapse = "")) #/
# put in a list
L <- list(hor, hor.rev, ver, ver.rev, diag1, diag1.rev, diag2, diag2.rev)
# find XMAS
sapply(L, stringr::str_count, pattern = "XMAS") |> sapply(sum) |> sum()

# part 2
m <- strsplit(input, "") |>
  unlist() |> 
  matrix(ncol = nchar(input[[1]]), byrow = TRUE)
rownames(m) <- 1:nrow(m)
colnames(m) <- 1:ncol(m)
# find location of all A's
DT <- which(m == "A", arr.ind = TRUE) |> as.data.table()
# remove A's from outside square
DT <- DT[!(row == 1 | row == nrow(m) | col == 1 | col == ncol(m)), ]
# create helpers
lookup <- melt(data.table(m, keep.rownames = TRUE), id.vars = c("rn"), variable.name = "col", value.name = "letter", variable.factor = FALSE)
lookup[, `:=`(rn = as.numeric(rn), col = as.numeric(col))]
DT[, r1 := row - 1]
DT[, c1 := col - 1]
DT[, r2 := row - 1]
DT[, c2 := col + 1]
DT[, r3 := row + 1]
DT[, c3 := col - 1]
DT[, r4 := row + 1]
DT[, c4 := col + 1]
# checks
DT[lookup, t1 := i.letter, on = .(r1 = rn, c1 = col)][]
DT[lookup, t2 := i.letter, on = .(r2 = rn, c2 = col)][]
DT[lookup, t3 := i.letter, on = .(r3 = rn, c3 = col)][]
DT[lookup, t4 := i.letter, on = .(r4 = rn, c4 = col)][]
DT[paste0(t1, t4) %in% c("MS", "SM") & paste0(t2, t3) %in% c("MS", "SM"), test := "pass"]
DT[test == "pass", ] |> nrow()
