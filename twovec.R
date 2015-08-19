library(fsp)

x <- 1:1000
y <- 1:1000

attr(x,"source") <- "object x";
attr(y,"source") <- "object y";

poor <- function(x) {
    z <- x
    z[1] <- 42
    x
}

Rprof(filename="output.out",marks.profiling=TRUE);
# Rprof(filename="output2.out");
for(i in 1:10000000) {
    poor(x);
}

for(i in 1:500000) {
    poor(y);
}
Rprof(NULL);
# summaryRprof(filename="output2.out")
feature.profile(filename="output.out");
