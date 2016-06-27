#Unit 7 Homework - Visualizing Network Data


# 1
edges = read.csv("edges.csv", stringsAsFactors = F)
users = read.csv("users.csv", stringsAsFactors = F)

str(edges)
nrow(users)
146*2/59

table(users$gender, users$school)

# 2
install.packages("igraph")
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users) 
g

plot(g, vertex.size=5, vertex.label=NA)

degree(g)
table(degree(g) >= 10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

#3
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

# 4
?igraph.plotting
