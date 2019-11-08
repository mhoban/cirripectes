library(tidyverse)
library(ggtree)
library(ggplot2)
library(treeio)
library(phytools)

# clear the workspace
rm(list=ls())

# load the tree data
tree <- ggtree::read.mrbayes("../data/treebuild2.nex.con.tre")

# Reroot the tree on the outgroup (Exallias brevis), giving it the same branch length that figtree would
tree@phylo <- reroot(tree@phylo,60,position = tree@phylo$edge.length[which(tree@phylo$edge[,2]==60)]/2)

# update tree data by cutting posterior probability into distinct bins
# update prob to 1 at basal split between in/outgroups and make sure it's numeric data 
# (this data isn't left in the tree when rerooted, but FigTree puts a 1 there, so I am too)
td <- as_tibble(tree) %>%
  mutate(
    prob = case_when(
      node == 149 ~ 1,
      TRUE ~ as.numeric(prob)
    ),
    posterior_bins = cut(
      prob,
      c(-Inf,0.60,0.75,0.95,Inf),
      labels=c("<0.60","0.60-0.75","0.75-0.95","0.95+")
    )
  ) 

# cast back to treedata object
tr <- as.treedata(td)

# layout data for tree annotations
offs <- 0
offs2 <- -0.03
bars <- 2
# color map for posterior probabilities
clr_map <- c("<0.60"="white","0.60-0.75"="gray75","0.75-0.95"="gray40","0.95+"="black")

# optionally pop up a quartz window
quartz()

font.size <- 5

# ---- plot the tree ----
ggtree(tr,ladderize = T) + 
  # show a root edge
  geom_rootedge(rootedge = 0.005) +
  
  # show circles at each node colored according to posterior prob
  geom_nodepoint(aes(fill=posterior_bins,subset=(node != 102)),shape=21,alpha=1,size=3) +
  scale_fill_manual(na.translate=F,name="Posterior node support",values=clr_map) +
  
  # only show tip label for outgroup
  geom_tiplab(aes(subset=(node==60))) + 
  
  # here I manually label each species as a clade. I tried several ways to
  # automate this and couldn't get them to format properly so it's just done
  # manually. the lone geom_strip call is because that species doesn't have a
  # "clade" since it's just one tip so it has to be a "strip" label instead of a clade label
  geom_cladelabel(node=122,'atop(bolditalic(C.~vanderbilti)~"              ","(Hawai‘i & Johnston Atoll)")',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=120,'atop(bolditalic(C.~matatakaro)~bold( sp.~nov.)~"           ","(Lineage B: South/Central Pacific)")',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=114,'bolditalic(C.~filamentosis)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_strip(63,63,'bolditalic(C.~chelomatus)',align=T,offset=offs,barsize=bars,parse=T,extend = 0.7, fontsize = font.size) +
  geom_cladelabel(node=112,'bolditalic(C.~auritus)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=125,'bolditalic(C.~fuscoguttatus)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=127,'bolditalic(C.~polyzona)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=106,'atop(bolditalic(C.~cf.~randalli)~"           ","(Western Indian Ocean)")',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=129,'bolditalic(C.~stigmaticus)',align=T,offset=-0.095,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=135,'atop(bolditalic(C.~variolosus)~"                     ","(Incl. Lineage A: Pacific Plate)")',align=T,offset=-0.095,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=130,'bolditalic(C.~castaneus)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  # geom_cladelabel(node=141,'bolditalic(C.~`"`~lineopunctatus~`"`)~"(Hawai‘i)"',align=F,offset=offs+0.002,barsize=bars,parse=T) +
  geom_cladelabel(node=141,'bolditalic(C.~"\\"lineopunctatus\\"")~"(Hawai‘i)"',align=F,offset=offs+0.002,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=139,'bolditalic(C.~quagga)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=146,'bolditalic(C.~obscurus)',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=145,'bolditalic(C.~alboapicalis)',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +
  # geom_cladelabel(node=147,'bolditalic(C.~`"`~patuki~`"`)~"(Rapa Nui)"',align=T,offset=offs2,barsize=bars,parse=T) +
  geom_cladelabel(node=147,'bolditalic(C.~"\\"patuki\\"")~"(Rapa Nui)"',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=148,'bolditalic(C.~jenningsi)',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +

  # show the branch length scale
  geom_treescale(x=0.15,y=-5) +
  # extend the viewable area to avoid text cutoff
  xlim(-0.005,0.275) +
  # position and style the posterior prob color legend
  theme(legend.position = c(0.1,0.9)) +
  guides(fill = guide_legend(override.aes = list(size = 5)))
  #geom_label2(aes(label=node, subset=!isTip,x=branch), size=5, color="darkred", alpha=0.5,nudge_x=-0.01) +

 
# ---- nodes of note ----
# c. vanderbilti: 122
# c. matatakaro sp. nov.: 120
# c. filamentosis: 114
# c. chelomatus: 63
# c. auritus: 112
# c. fuscoguttatus: 126
# c. polyzona: 127
# c. randalli: 106
# c. stigmaticus: 129
# c. variolosus: 135
# c. castaneus: 130
# c. "lineopunctatus": 141
# c. quagga: 139
# c. obscurus: 146
# c. alboapicalis: 145
# c. patiuki: 147
# c. jenningsi: 148


