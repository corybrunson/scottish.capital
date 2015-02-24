library(igraph)
load('data/scottish.capital.rda')

# COMPANY NAMES

# Vector of company names
companies <- unlist(sapply(scottish.capital,
                           function(g) V(g)$name[V(g)$type]))
length(unique(companies))

# Convert " and " to " & "
companies <- gsub(' and ', ' & ', companies)
length(unique(companies))

# Convert all "William" company names to "Wm"
companies <- gsub('William', 'Wm', companies)
length(unique(companies))

# Changed names
wh.rep <- grep('\\(formerly', companies)
name.dat <- unique(data.frame(
    old = I(gsub('^.* \\(formerly (.*)\\)$', '\\1', companies[wh.rep])),
    new = I(gsub('^(.*) \\(formerly .*\\)$', '\\1', companies[wh.rep]))
))
# Where else do the original names appear?
wh.old <- lapply(1:nrow(name.dat), function(i) {
    setdiff(grep(name.dat$old[i], companies), wh.rep)
})
# Where else do the changed names appear?
wh.new <- lapply(1:nrow(name.dat), function(i) {
    grep(name.dat$new[i], companies)
})
# Replace all instances with changed names
for(i in 1:nrow(name.dat)) {
    companies[union(wh.old[[i]], wh.new[[i]])] <- name.dat$new[i]
}
length(unique(companies))

# Combine companies that begin the same way
uniq.companies <- unique(companies)
wh.prefix <- lapply(1:length(uniq.companies), function(i) {
    grep(paste0('^', uniq.companies[i]), uniq.companies)
})
wh.prefix <- wh.prefix[which(sapply(wh.prefix, length) > 1)]
prefix.companies <- lapply(wh.prefix, function(vec) uniq.companies[vec])
prefix.tab <- lapply(prefix.companies, function(vec) {
    table(companies[which(companies %in% vec)])
})
for(i in 1:length(prefix.tab)) {
    name <- names(prefix.tab[[i]])[which(prefix.tab[[i]] ==
                                             max(prefix.tab[[i]]))[1]]
    companies[which(companies %in% names(prefix.tab[[i]]))] <- name
}
length(unique(companies))

# Rename event nodes
i <- 1
for(j in 1:length(scottish.capital)) {
    wh.event <- which(V(scottish.capital[[j]])$type)
    V(scottish.capital[[j]])$name[wh.event] <-
        companies[i:(i + length(wh.event) - 1)]
    i <- i + length(wh.event)
}

# DIRECTOR NAMES

# Vector of director names
directors <- unlist(sapply(scottish.capital,
                           function(g) V(g)$name[!V(g)$type]))
length(unique(directors))

# Convert " & " to " and "
directors <- gsub(' & ', ' and ', directors)
length(unique(directors))

# Combine directors that begin the same way (BEWARE OF REG EXPR CHARACTERS)
uniq.directors <- unique(directors)
wh.prefix <- lapply(1:length(uniq.directors), function(i) {
    grep(paste0('^', uniq.directors[i]), uniq.directors)
})
wh.prefix <- wh.prefix[which(sapply(wh.prefix, length) > 1)]
prefix.directors <- lapply(wh.prefix, function(vec) uniq.directors[vec])
prefix.tab <- lapply(prefix.directors, function(vec) {
    table(directors[which(directors %in% vec)])
})
for(i in 1:length(prefix.tab)) {
    name <- names(prefix.tab[[i]])[which(prefix.tab[[i]] ==
                                             max(prefix.tab[[i]]))[1]]
    directors[which(directors %in% names(prefix.tab[[i]]))] <- name
}
length(unique(directors))

# Presence of an honorific
hons <- c('Sir', 'Maj\\. Gen\\.', 'Col\\.', 'Hon\\.')
for(hon in hons) {
    # Determine which names come both with and without the honorific
    w.hon <- grep(paste0('^', hon, ' '), directors)
    wo.hon <- sub(paste0('^', hon, ' (.*)$'), '\\1', directors[w.hon])
    wwo.hon <- wo.hon[which(wo.hon %in% directors)]
    # Provided at most one interval is skipped over between appearances,
    # replace all instances with the one with the honorific
    for(wwo in wwo.hon) {
        wh.graph <- which(sapply(scottish.capital, function(g) {
            any(grepl(paste0('^(', hon, ' ){0,1}', wwo, '$'),
                      V(g)$name))
        }))
        if(length(wh.graph) > 1) {
            if(max(diff(wh.graph)) > 2 | max(wh.graph) - min(wh.graph) > 3) next
        }
        wh.dirs <- which(grepl(paste0('^(', hon, ' ){0,1}', wwo, '$'),
                               directors))
        directors[wh.dirs] <- paste0(hon, ' ', wwo)
    }
}

# Manual name equivalences across intervals
equivs <- c(
    '^Sir R\\.(W\\.){0,1} Anstruther$',
    '^Sir A\\. {0,1}S(\\.|teven) Bilsland$',
    '^Sir A\\.(C\\.){0,1} Blair$',
    '^(Sir ){0,1}J(as|ames){0,1}. {0,1}(C|I). Campbell$',
    '^Sir J(\\.|ohn )T. Cargill$',
    '^J(\\.|ohn) Cowan$',
    '^H\\.(U\\.){0,1} Cunningham$',
    '^Sir Maurice (E\\. ){0,1}Denny$',
    '^J\\.(A\\.){0,1} Dewar$',
    '^Lord (George|Nigel) Douglas-Hamilton$',
    '^R(\\.|alph )(W\\. ){0,1}Dundas$',
    '^Sir R(\\.|obert) Erskine-Hill$',
    '^R\\.(E\\.){0,1} Findlay$',
    '^(R|T)\\.D\\. Findlay$',
    '^(Sir ){0,1}H(\\.|ugh) Fraser$',
    '^A\\.(B\\.){0,1} Gilroy$',
    '^Sir L\\.(G|J)\\. Grant$',
    '^Sir G\\.(C\\.){0,1} Harvie-Watt$',
    '^(Capt\\. ){0,1}J\\.(F\\.){0,1}H\\. Houldsworth$',
    '^B\\.(G|J)\\. Ivory$',
    '^H(\\.|enry) Lithgow$',
    '^M\\. Ma{0,1}cDougall$',
    '^Sir A\\.(F\\.){0,1} McDonald$',
    '^Sir J(\\.|ohn) Muir$',
    '^Sir H(\\.|ugh) Rose$',
    '^R\\.(H|M)\\. Sinclair$',
    '^D\\.(J\\.){0,1} Smith$',
    '^Sir D(\\.|ouglas) Thomson$',
    '^(J|T)\\.W\\. Tod$',
    '^Sir Ernest (M\\. ){0,1}Wedderburn$',
    '^Sir G(\\.|eorge) Williamson$',
    '(B|H)\\.C\\. Wilson$',
    '^C\\.F\\.(J\\.){0,1} Younger$',
    '^(Sir ){0,1}W(\\.|m )McE\\. Younger$'
)
for(equiv in equivs) {
    # Provided no intervals are skipped over between appearances...
    wh.graphs <- which(sapply(scottish.capital, function(g) {
        any(grep(equiv, V(g)$name))
    }))
    if(length(wh.graphs) > 1)
        if(any(diff(wh.graphs) > 1)) {
            print(equiv)
            next
        }
    # ...replace each instance with the most recent version to appear twice,
    # or else the most recent
    wh <- grep(equiv, directors)
    tab <- table(directors[wh])
    tw <- names(tab)[which(tab > 1)]
    ok <- directors[wh[which(directors[wh] %in% tw)]]
    use <- if(length(ok) > 0) ok[length(ok)] else directors[wh[length(wh)]]
    directors[wh] <- use
}
length(unique(directors))

# Rename nodes according to renamed directors
i <- 0
sc <- 1
while(i < length(directors)) {
    wh <- which(!V(scottish.capital[[sc]])$type)
    j <- length(wh)
    V(scottish.capital[[sc]])$name[wh] <- directors[(i + 1):(i + j)]
    i <- i + j
    sc <- sc + 1
}

# Combine nodes with same name
for(i in 1:length(scottish.capital)) {
    scottish.capital[[i]] <- contract.vertices(
        scottish.capital[[i]],
        factor(V(scottish.capital[[i]])$name),
        'first'
    )
}

# Remove directors that only appear once
for(i in 1:length(scottish.capital)) {
    wh <- which(
        (!V(scottish.capital[[i]])$type) & (degree(scottish.capital[[i]]) == 1)
    )
    if(length(wh) == 0) next
    print('Deleted directors (appear only once)')
    print(c(i, V(scottish.capital[[i]])$name[wh]))
    scottish.capital[[i]] <- delete.vertices(scottish.capital[[i]], wh)
}

save(list = 'scottish.capital', file = 'data/scottish.capital.rda')

rm(list = ls())
