
out.dir <- "ldavis/"
dir.create(out.dir)


dnames <- c("pythonquestions", "rquestions", "stacksample", "statsquestions")
minSc <- 3
seed <- 102

out.index <- file(out.dir, "index.html")

done.copy <- FALSE
for(dname in dnames){
  for(K in 1:4*10){
    head <- paste(dname, "K", K, sep="_")
    fname.html <- paste0(head, ".html")
    fname.json <- paste0(head, ".json")
    in.dir <- paste0(head, "_seed_", seed, "/")
    html <- readLines(paste0(in.dir, "index.html"))
    html <- gsub("lda.json", fname.json, html)
    file.rename(paste0(in.dir, "lda.json"), paste0(out.dir, fname.json))
    out <- file(paste0(out.dir, fname.html), open="w")
    writeLines(html, out)
    close(out)
    if(!done.copy){
      for(fname in c("d3.v3.js", "lda.css", "ldavis.js")){
        file.rename(paste0(in.dir, fname), paste0(out.dir, fname))
      }
      done.copy <- TRUE
    }
    unlink(in.dir, recursive=T)
  }   
}
