library(igraph)
if(!grepl('scottish.capital$', getwd())) stop('Wrong directory')

# End year of each interval
years <- c(1905, 1921, 1938, 1956, 1974)
# For each interval...
scottish.capital <- lapply(years, function(year) {
    # Identify the file
    file = paste0('data/Scotland',
                  as.numeric(year) - 1, '-', substr(year, 4, 4),
                  '.txt')
    # Identify the lines that demarcate the sections, and save their names
    lines <- readLines(file)
    secs <- grep('^Section', lines)
    Section <- gsub('^Section [A-Z]: | Companies$', '', lines[secs])
    # Read table for each section
    tabs <- lapply(1:length(secs), function(i) {
        read.table(
            file, header = TRUE, sep = '|', quote = '',
            nrows = if(i == length(secs)) -1 else secs[i + 1] - secs[i] - 2,
            skip = secs[i]
        )
    })
    # Save company name and capital columns
    Company <- do.call(c, lapply(tabs, function(tab) as.character(tab$Company)))
    Capital <- do.call(c, lapply(tabs, function(tab) {
        as.numeric(tab[, grep('1000', names(tab))])
    }))
    # Make list of director vectors
    datDirectors <- do.call(c, lapply(tabs, function(tab) {
        directors <- gsub(' \\(([A-Z]| |\\&|,)+\\)', '', tab$Multiple.Directors)
        strsplit(directors, split = ', ')
    }))
    Directors <- unique(unlist(datDirectors))
    # Named actor and event vectors
    actors <- 1:length(Directors)
    names(actors) <- Directors
    events <- 1:length(Company) + length(actors)
    names(events) <- Company
    # Edge list
    edgelist <- unname(cbind(
        actors[unlist(datDirectors)],
        rep(events, sapply(datDirectors, length))
    ))
    # Affiliation network! (actors first)
    an <- graph.edgelist(edgelist, directed = FALSE)
    V(an)$name <- c(names(actors), names(events))
    V(an)$type <- c(rep(FALSE, length(actors)), rep(TRUE, length(events)))
    V(an)$sector[V(an)$type] <-
        rep(Section, c(secs[-1], length(lines) + 1) - secs - 2)
    V(an)$capital[V(an)$type] <- Capital
    an
})

save(list = 'scottish.capital', file = 'data/scottish.capital.rda')

rm(list = ls())
