# Compute statistics for the scottish.capital bigraphs for comparison with
# the statistical summaries in "The Anatomy of Scottish Capital"

library(igraph)
load('data/scottish.capital.rda')

# Numbers of companies and of multiple directors
# Companies: 108, 114, 111, 109, 120
# Directors: 135, 118, 132, 118, 101
dat <- data.frame(
    t(sapply(scottish.capital, function(g) c(
        no.companies = length(which(V(g)$type)),
        no.directors = length(which(!V(g)$type))
    )))
)
print(dat)

# Numbers of "surviving" directors from previous cross-section
# 37, 21, 25, 29
surv <- sapply(2:length(scottish.capital), function(i) {
    length(intersect(
        V(scottish.capital[[i - 1]])$name[!V(scottish.capital[[i - 1]])$type],
        V(scottish.capital[[i]])$name[!V(scottish.capital[[i]])$type]
    ))
})
print(surv)

# Greatest numbers of directors in any director's neighborhood
met <- lapply(scottish.capital, function(g) {
    h <- bipartite.projection(g)[[1]]
    d <- sort(degree(h), decreasing = TRUE)
    d[1:12]
})
print(met)

# Largest connected component(s) of company graphs by multiple common directors
for(g in scottish.capital) {
    h <- bipartite.projection(g)[[2]]
    h <- delete.edges(h, which(E(h)$weight < 2))
    c <- clusters(h)
    h <- delete.vertices(h, which(c$membership %in% which(c$csize <= 5)))
    V(h)$label.cex <- .5
    V(h)$label <- NA
    plot(h, vertex.size = 1)
}
