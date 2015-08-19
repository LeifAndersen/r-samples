library(fsp);

f <- function(x) {
    x[1] <- 42;
}

Rprof(filename="output.out", marks.profiling=TRUE);
for(i in 1:3000000) {
    f(c(1,2,3,4,5));
}
Rprof(NULL);
feature.profile(filename="output.out");
