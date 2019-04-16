### Syntax for Analyzing Social Networks with R: 
### Description, Visualization, and Modeling

## 5 Number Summary
# Setup
library(statnet)
library(UserNetR)
data(Moreno)

# Simple plot
gender <- Moreno %v% "gender"
plot(Moreno,vertex.col=gender+2,vertex.cex=1.2)

# Size
network.size(Moreno)
summary(Moreno,print.adj=FALSE)

# Density
den_hand <- 2*46/(33*32)
den_hand
gden(Moreno)

# Components
components(Moreno)
lgc <- component.largest(Moreno,result="graph")

# Diameter
gd <- geodist(lgc)
max(gd$gdist)

# Transitivity
gtrans(Moreno,mode="graph")

## Network graphic layouts

# Setup
library(statnet)
library(UserNetR)
library(RColorBrewer)
options(width=50)

# Different layout example 1
data(Moreno)
op <- par(mar = rep(0, 4),mfrow=c(1,2))
plot(Moreno,mode="circle",vertex.cex=1.5)
plot(Moreno,mode="fruchtermanreingold",vertex.cex=1.5)
par(op)

# Energy algorithm example
data(Bali)
strt_coord <- gplot(Bali,mode='circle')
op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
set.seed(100)
pardum <- list(niter=0,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,coord=strt_coord,main='Iteration = 0')
pardum <- list(niter=1,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,main='Iteration = 1')
pardum <- list(niter=5,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,main='Iteration = 5')
pardum <- list(niter=10,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,main='Iteration = 10')
pardum <- list(niter=25,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,main='Iteration = 25')
pardum <- list(niter=50,seed.coord=strt_coord)
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,
      layout.par=pardum,main='Iteration = 50')
par(op)

# Various layout options
op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='circle',main="circle")
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='eigen',main="eigen")
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='random',main="random")
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='spring',main="spring")
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='fruchtermanreingold',
      main='fruchtermanreingold')
gplot(Bali,gmode="graph",edge.col="grey75",
      vertex.cex=1.5,mode='kamadakawai',
      main='kamadakawai')
par(op)

# Can save and manipulate coordinates
mycoords1 <- gplot(Bali,gmode="graph",
                   vertex.cex=1.5)
mycoords2 <- mycoords1
mycoords2[,2] <- mycoords1[,2]*1.5
mycoords1
mycoords2
op <- par(mar=c(4,3,4,3),mfrow=c(1,2))
gplot(Bali,gmode="graph",coord=mycoords1,
      vertex.cex=1.5,suppress.axes = FALSE,
      ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
      main="Original coordinates")
gplot(Bali,gmode="graph",coord=mycoords2,
      vertex.cex=1.5,suppress.axes = FALSE,
      ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
      main="Modified coordinates")
par(op)

# igraph examples
detach(package:statnet)
library(igraph)
library(intergraph)
iBali <- asIgraph(Bali)
op <- par(mar=c(0,0,3,0),mfrow=c(1,3))
plot(iBali,layout=layout_in_circle,
     main="Circle")
plot(iBali,layout=layout_randomly,
     main="Random")
plot(iBali,layout=layout_with_kk,
     main="Kamada-Kawai")
par(op)

# Clean up
detach("package:igraph", unload=TRUE)

## Network graphics fundamentals

# Set-up
library(statnet)
library(xtable)
library(UserNetR)
library(RColorBrewer)
options(width=50)

# Node color
data(Bali)
gplot(Bali,vertex.col="slateblue2",gmode="graph")

# Other ways to specify color
col2rgb('slateblue2')
gplot(Bali,vertex.col=rgb(122,103,238,
                          maxColorValue=255),gmode="graph")
gplot(Bali,vertex.col="#7A67EE",gmode="graph")

# Can use alpha channel to set transparency
ndum <- rgraph(300,tprob=0.025,mode="graph")
op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Fully opaque")
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,alpha=80,
                     maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Partly transparent")
par(op)

# Using vertex attributes to set node colors
rolelab <- get.vertex.attribute(Bali,"role")
op <- par(mar=c(0,0,0,0))
plot(Bali,usearrows=FALSE,vertex.cex=1.5,label=rolelab,
     displaylabels=T,vertex.col="role")
par(op)

# Using palettes for finer color control
palette()
library(RColorBrewer)
display.brewer.pal(5, "Dark2")
my_pal <- brewer.pal(5,"Dark2")

rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
plot(Bali,vertex.cex=1.5,label=rolelab,
     displaylabels=T,vertex.col=my_pal[rolecat])
op <- par(mar=c(0,0,0,0))

# Node sizes
op <- par(mar = c(0,0,2,0),mfrow=c(1,3))
plot(Bali,vertex.cex=0.5,main="Too small")
plot(Bali,vertex.cex=2,main="Just right")
plot(Bali,vertex.cex=6,main="Too large")
par(op)

# Using network info to set node size
deg <- degree(Bali,gmode="graph")
deg
cls <- closeness(Bali,gmode="graph")
cls
bet <- betweenness(Bali,gmode="graph")
bet

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=deg,main="Raw")
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),
     main="Adjusted")
par(op)

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=cls,main="Raw")
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,
     main="Adjusted")
par(op)

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=bet,main="Raw")
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),
     main="Adjusted")
par(op)

# Simple function to rescale node sizes
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

# Example of function
plot(Bali,vertex.cex=rescale(deg,1,6),
     main="Adjusted node sizes with rescale function.")
get.vertex.attribute(Bali,"vertex.names")
op <- par(mar = c(0,0,0,0))

# Final, publication ready graphic
my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
set.seed(654)
plot(Bali,vertex.cex=rescale(deg,1,5),vertex.col=my_pal[rolecat])
legend("bottomleft",legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n",
       title="Terrorist Role")

# Clean up
detach("package:statnet", unload=TRUE)

## Actor Prominence

# Setup
library(statnet)
library(xtable)
library(UserNetR)
library(RColorBrewer)
options(width=50)

# Initial example
load(file="prom_example_net.RData")
net_mat <- as.sociomatrix(net)
net_mat
gplot(net,displaylabels=TRUE,usearrows=FALSE,coord=coords,
      jitter=FALSE)
net <- network(net_mat)
net %v% 'vertex.names'

# Three centrality measures
degree(net, gmode="graph")
closeness(net, gmode="graph")
betweenness(net, gmode="graph")

# More interesting example
data(DHHS)
df.prom <- data.frame(
  deg = degree(DHHS),
  cls = closeness(DHHS),
  btw = betweenness(DHHS),
  evc = evcent(DHHS),
  inf = infocent(DHHS),
  flb = flowbet(DHHS)
)
cor(df.prom)

# Centralization
dum1 <- rbind(c(1,2),c(1,3),c(1,4),c(1,5))
star_net <- network(dum1,directed=FALSE)  
dum2 <- rbind(c(1,2),c(2,3),c(3,4),c(4,5),c(5,1))
circle_net <- network(dum2,directed=FALSE) 

my_pal <- brewer.pal(5,"Set2")
gplot(star_net,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=2,
      vertex.col=my_pal[1],
      edge.lwd=0,edge.col="grey50",xlab="Star Graph")
gplot(circle_net,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=2,
      vertex.col=my_pal[3],
      edge.lwd=0,edge.col="grey50",xlab="Circle Graph")
closeness(circle_net)
centralization(circle_net,closeness)
closeness(star_net)
centralization(star_net,closeness)

# Bali example
data(Bali)
str(degree(Bali))
summary(degree(Bali))

my_pal <- brewer.pal(5,"Set2")
rolecat <- Bali %v% "role"
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0,edge.col="grey25")
legend("topright",legend=c("BM","CT","OA","SB",
                           "TL"),col=my_pal,pch=19,pt.cex=2)


df.prom2 <- data.frame(
  degree = degree(Bali),
  closeness = closeness(Bali),
  betweenness = betweenness(Bali)
)

row.names(df.prom2) <- Bali %v% "vertex.names"
df.promsort <- df.prom2[order(-df.prom2$degree),]
df.promsort

centralization(Bali,degree)
centralization(Bali,closeness)
centralization(Bali,betweenness)

# Nice plot, nodes sized by relative degree
deg <- degree(Bali,rescale=TRUE)
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.cex=deg*12,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0.5,edge.col="grey75")
legend("topright",legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,pch=19,pt.cex=2)

# Cutpoints
cpnet <- cutpoints(net,mode="graph",
                   return.indicator=TRUE)
gplot(net,gmode="graph",vertex.col=cpnet+2,coord=coords,
      jitter=FALSE,displaylabels=TRUE)

# Components increase if cutpoint removed
net2 <- net
components(net2)
delete.vertices(net2,7)
components(net2)
gplot(net2,gmode="graph",vertex.col=2,
      coord=coords[-7,],jitter=FALSE,displaylabels=TRUE)

# Bridges
# No bridge function in R, can create one here
bridges <- function(dat,mode="graph",
                    connected=c("strong", "weak")) {
  e_cnt <- network.edgecount(dat)
  if (mode == "graph") {
    cmp_cnt <- components(dat)
    b_vec <- rep(FALSE,e_cnt) 
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2) != cmp_cnt)
    }
  }
  else {
    cmp_cnt <- components(dat,connected=connected)
    b_vec <- rep(FALSE,e_cnt)
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2,connected=connected) 
                   != cmp_cnt)
    }
  }
  return(b_vec)
}

# Demonstrate bridges
bridges(net)
brnet <- bridges(net)
gplot(net,gmode="graph",vertex.col="red",
      edge.col=brnet+2,coord=coords,
      jitter=FALSE,displaylabels=TRUE)

# Clean up
detach("package:statnet", unload=TRUE)

## Subgroups

# Setup
library(UserNetR)
options(width=50)
library(statnet)

# First example
data(Moreno)
gender <- Moreno %v% "gender"

op <- par(mar = rep(0, 4))
plot(Moreno,vertex.col=2+gender,vertex.cex=1.2)
par(op)

# More complicated example
data(DHHS)
agency <- DHHS %v% "agency"
op <- par(mar = rep(0, 4))
plot(DHHS,vertex.col=agency+2,vertex.cex=1.4,
     edge.col="gray60",displaylabels=TRUE)
par(op)

# Cliques
detach("package:statnet", unload=TRUE)
library(igraph)
clqexmp <- graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
plot(clqexmp,vertex.color="SkyBlue2")

clique.number(clqexmp)
cliques(clqexmp, min=3)
maximal.cliques(clqexmp,min=3)
largest.cliques(clqexmp)


# Cliques not that common in larger networks
g25 <- erdos.renyi.game(25, 75, type="gnm")
g50 <- erdos.renyi.game(50, 150, type="gnm")
g100 <- erdos.renyi.game(100, 300, type="gnm")
g500 <- erdos.renyi.game(500, 1500, type="gnm")
nodes <- c(25,50,100,500)
lrgclq <- c(clique.number(g25),clique.number(g50),
            clique.number(g100),clique.number(g500))
numclq <- c(length(cliques(g25,min=3)),
            length(cliques(g50,min=3)),
            length(cliques(g100,min=3)),
            length(cliques(g500,min=3)))
clqinfo <- data.frame(Nodes=nodes,Largest=lrgclq,
                      Number=numclq)
clqinfo

# k-cores
data(DHHS)
library(intergraph)
iDHHS <- asIgraph(DHHS)
graph.density(iDHHS)
iDHHS <- subgraph.edges(iDHHS,E(iDHHS)[collab > 2])
graph.density(iDHHS)
plot(iDHHS)
coreness <- graph.coreness(iDHHS)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness

# Visualizing cores
Vname <- get.vertex.attribute(iDHHS,name='vertex.names',
                              index=V(iDHHS))
V(iDHHS)$name <- Vname
V(iDHHS)$color <- coreness + 1
op <- par(mar = rep(0, 4))
plot(iDHHS,vertex.label.cex=0.8)

par(op)
colors <- rainbow(maxCoreness)
op <- par(mar = rep(0, 4))
plot(iDHHS,vertex.label=coreness,
     vertex.color=colors[coreness])
par(op)

# 'Peeling away' cores to reveal central cores
V(iDHHS)$name <- coreness
V(iDHHS)$color <- colors[coreness]
iDHHS1_6 <- iDHHS
iDHHS2_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 1))
iDHHS3_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 2))
iDHHS4_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 3))
iDHHS5_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 4))
iDHHS6_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 5))
lay <- layout.fruchterman.reingold(iDHHS)
op <- par(mfrow=c(3,2),mar = c(3,0,2,0))
plot(iDHHS1_6,layout=lay,main="All k-cores")
plot(iDHHS2_6,layout=lay[which(coreness > 1),],
     main="k-cores 2-6")
plot(iDHHS3_6,layout=lay[which(coreness > 2),],
     main="k-cores 3-6")
plot(iDHHS4_6,layout=lay[which(coreness > 3),],
     main="k-cores 4-6")
plot(iDHHS5_6,layout=lay[which(coreness > 4),],
     main="k-cores 5-6")
plot(iDHHS6_6,layout=lay[which(coreness > 5),],
     main="k-cores 6-6")
par(op)

# Modularity
g1 <- graph.formula(A-B-C-A,D-E-F-D,G-H-I-G,A-D-G-A)
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3)
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)

op <- par(mfrow=c(1,2))
plot(g1,vertex.color=(V(g1)$grp_good),
     vertex.size=20,
     main="Good Grouping")
plot(g1,vertex.color=(V(g1)$grp_bad),
     vertex.size=20,
     main="Bad Grouping")
par(op)

modularity(g1,V(g1)$grp_good)
modularity(g1,V(g1)$grp_bad)

# Community detection
library(intergraph)
data(DHHS)
iDHHS <- asIgraph(DHHS)
table(V(iDHHS)$agency)
V(iDHHS)[1:10]$agency
modularity(iDHHS,(V(iDHHS)$agency+1))

data(Moreno)
iMoreno <- asIgraph(Moreno)
table(V(iMoreno)$gender)
modularity(iMoreno,V(iMoreno)$gender)

data(Facebook)
levels(factor(V(Facebook)$group))
grp_num <- as.numeric(factor(V(Facebook)$group))
modularity(Facebook,grp_num)


# Modularity calculation and presentation
cw <- cluster_walktrap(iMoreno)
membership(cw)
modularity(cw)
plot(cw, iMoreno)

cw <- cluster_walktrap(iDHHS)
modularity(cw)
membership(cw)
plot(cw,iDHHS)
table(V(iDHHS)$agency,membership(cw))

# More detailed example with Bali
data(Bali)
iBali <- asIgraph(Bali)
cw <- cluster_walktrap(iBali)
modularity(cw)
membership(cw)
ceb <- cluster_edge_betweenness(iBali)
modularity(ceb)
membership(ceb)
cs <- cluster_spinglass(iBali)
modularity(cs)
membership(cs)
cfg <- cluster_fast_greedy(iBali)
modularity(cfg)
membership(cfg)
clp <- cluster_label_prop(iBali)
modularity(clp)
membership(clp)
cle <- cluster_leading_eigen(iBali)
modularity(cle)
membership(cle)
cl <- cluster_louvain(iBali)
modularity(cl)
membership(cl)
co <- cluster_optimal(iBali)
modularity(co)
membership(co)

table(V(iBali)$role,membership(cw))
compare(as.numeric(factor(V(iBali)$role)),cw,
        method="adjusted.rand")
compare(cw,ceb,method="adjusted.rand")
compare(cw,cs,method="adjusted.rand")
compare(cw,cfg,method="adjusted.rand")

op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
plot(ceb, iBali,vertex.label=V(iBali)$role,
     main="Edge Betweenness")
plot(cfg, iBali,vertex.label=V(iBali)$role,
     main="Fastgreedy")
plot(clp, iBali,vertex.label=V(iBali)$role,
     main="Label Propagation")
plot(cle, iBali,vertex.label=V(iBali)$role,
     main="Leading Eigenvector")
plot(cs, iBali,vertex.label=V(iBali)$role,
     main="Spinglass")
plot(cw, iBali,vertex.label=V(iBali)$role,
     main="Walktrap")
par(op)

# Clean up
detach("package:igraph", unload=TRUE)

