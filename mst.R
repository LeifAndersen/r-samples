# Implementation of a graph library and Minimum Spanning Tree in R

library(methods)

# ==============================================================================

# Data definitions for graph object

setClass("Node",
         slots = list(data="character",
                      edges="list"))

setClass("Edge",
         slots = list(weight="numeric",
                      left = "character",
                      right = "character"))

setClass("Graph",
         slots = list(nodes = "list",
                      edges = "list"))

# ==============================================================================

# Operations used for manipulating graph objects

# Extend == to operate over Nodes
setMethod("==",
          c(e1="Node",e2="Node"),
          function(e1, e2) {
              e1@data == e2@data
          })

# Extend %in% to operate over lists of nodes
setGeneric("%in%")
setMethod("%in%",
          c(x="Node",table="list"),
          function(x,table) {
              Reduce(function(acc,test) acc || (test==x),
                     table,
                     FALSE)
          })

# For adding a node to a graph.
setGeneric("add.node",function(g,data) standardGeneric("add.node"))
setMethod("add.node",
          c("Graph","character"),
          function(g,data) {
              g@nodes <- c(g@nodes, setNames(as.list(c(new("Node",
                                                           data=data,
                                                           edges=list()))),
                                             c(data)))
              return(g)
          })

# For adding an edge to a graph. (Updates nodes accordingly)
setGeneric("add.edge",function(g,weight,left,right) standardGeneric("add.edge"))
setMethod("add.edge",
          c("Graph","numeric","character","character"),
          function(g,weight,left,right) {
              tag <- paste(left,right);
              g@edges <- c(g@edges, setNames(as.list(c(new("Edge",
                                                           weight=weight,
                                                           left=left,
                                                           right=right))),
                                             c(tag)))
              g@nodes[[left]]@edges  <- c(g@nodes[[left]]@edges, list(tag))
              g@nodes[[right]]@edges <- c(g@nodes[[right]]@edges, list(tag))
              return(g)
          })

# Given a graph, node and edge tag, ruturn the other node the edge points to
setGeneric("step", function(g,n,t) standardGeneric("step"))
setMethod("step",
          c("Graph","Node","character"),
          function(g,n,t) {
              edge <- g@edges[[t]]
              if(edge@left == n@data) return(g@nodes[[edge@right]])
              else                    return(g@nodes[[edge@left]])
          })

# Return a list of all the nodes that are connected in the graph to the input node
setGeneric("find.set",function(g,n) standardGeneric("find.set"))
setMethod("find.set",
          c("Graph","Node"),
          function(g,n) {
              nodes <- c(n)
              q <- Queue()
              q$push(n)
              while(!q$empty()) {
                  node <- q$pop()
                  next.nodes <- lapply(node@edges, function(t) step(g,node,t))
                  for(next.node in next.nodes) {
                      if(!(next.node %in% nodes)) {
                          q$push(next.node)
                          nodes <- c(nodes, next.node)
                      }
                  }
              }
              return(nodes)
          })

# Return an identical graph, with all edges removed
setGeneric("drop.edges",function(g) standardGeneric("drop.edges"))
setMethod("drop.edges",
          c("Graph"),
          function(g) {
              gr <- new("Graph")
              gr@nodes <- mapply(function(n) new("Node", data=n@data, edges=list()),
                                 g@nodes,
                                 SIMPLIFY = FALSE)
              return(gr)
          })

# Determin if the two lists are equivalent when interpreted as sets
setGeneric("identical.set",function(e1,e2) standardGeneric("identical.set"))
setMethod("identical.set",
          c("list","list"),
          function(e1,e2) {
              (Reduce(function(acc,test) acc && (test %in% e2),
                     e1, TRUE)
               &&
               Reduce(function(acc,test) acc && (test %in% e1),
                     e2, TRUE))
          })

# Find the minimum spanning tree via Kruskal's algorithm.
setGeneric("kruskal", function(g) standardGeneric("kruskal"))
setMethod("kruskal",
          c("Graph"),
          function(g) {
              A <- drop.edges(g)
              pq <- PriorityQueue()
              pq$push(g@edges)
              while(!(pq$empty())) {
                  edge <- pq$pop()
                  if(!(identical.set(find.set(A,A@nodes[[edge@left]]),
                                     find.set(A,A@nodes[[edge@right]])))) {
                      A <- add.edge(A,edge@weight,edge@left,edge@right)
                  }
              }
              return(A)
          })

# ==============================================================================

# Queue Data structures (for walking graphs)

# Find the weight of an object
setGeneric("weight",function(x) standardGeneric("weight"));
setMethod("weight",
          c("Edge"),
          function(x) x@weight)

PriorityQueue <- function() {
    items <- NULL
    push <- function(item) {
        ord <- order(mapply(weight, c(items,item)))
        items <<- as.list(c(items, item))[ord]
    }
    pop <- function() {
        head <- items[[1]]
        items <<- items[-1]
        return(head)
    }
    empty <- function() length(items) == 0
    list(push=push,pop=pop,empty=empty)
}

Queue <- function() {
    items <- NULL
    push <- function(item) {
        items <<- c(items, list(item))
    }
    pop <- function() {
        head <- items[[1]]
        items <<- items[-1]
        return(head)
    }
    empty <- function() length(items) == 0
    list(push=push,pop=pop,empty=empty)
}

# ==============================================================================

# Tests

g <- new("Graph")
#print("Empty Graph")
#print(g)
g <- add.node(g,"foo")
g <- add.node(g,"bar")
g <- add.node(g,"baz")
g <- add.node(g,"qux")
g <- add.node(g,"quux")
#print("Adding Nodes")
#print(g)
g <- add.edge(g,4,"foo","bar")
g <- add.edge(g,8,"foo","baz")
g <- add.edge(g,2,"bar","baz")
g <- add.edge(g,12,"baz","qux")
#print("Adding Edge")
#print(g)
for(i in 1:1000)
    kruskal(g)
