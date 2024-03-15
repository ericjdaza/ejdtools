# Mode function. Source: https://www.statology.org/mode-in-r/
findMode <- function(x) {
  u <- unique(x)[!is.na(unique(x))]
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}