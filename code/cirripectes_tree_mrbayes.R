library(tidyverse)
library(ggtree)
library(ggplot2)
library(treeio)
library(phytools)
library(here)


# load the tree data
tree <- ggtree::read.mrbayes(here("data","cirripectes-684-reduced.nex.con.tre"))

# Reroot the tree on the outgroup (Plagiotremus tapeinosoma), giving it the same branch length that figtree would
tree@phylo <- reroot(tree@phylo,1,position = tree@phylo$edge.length[which(tree@phylo$edge[,2]==1)]/2)

# update tree data by cutting posterior probability into distinct bins
# update prob to 1 at basal split between in/outgroups and make sure it's numeric data 
# (this data isn't left in the tree when rerooted, but FigTree puts a 1 there, so I am too)
# make sure outgroup label displays properly
td <- as_tibble(tree) %>%
  mutate(
    prob = case_when(
      node == 250 ~ 1,
      TRUE ~ as.numeric(prob)
    ),
    posterior_bins = cut(
      prob,
      c(-Inf,0.60,0.75,0.95,Inf),
      labels=c("<0.60","0.60-0.75","0.75-0.95","0.95+")
    ),
    label = ifelse(label=="Pta_BLN0831",'"Pta_BLN0831 ("~italic(Plagiotremus~tapeinosoma)~")"',label)
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

# don't plot node symbols for the following internal nodes
node_ignore <- c(seq(207,211),seq(213,220),seq(195,196),seq(199,204),seq(230,240),seq(243,249),seq(180,184),227,170)

# ---- plot the tree ----
ggtree(tr,ladderize = T) + 
  # show a root edge
  geom_rootedge(rootedge = 0.005) +
  
  #geom_label2(aes(label=node,subset=!isTip, x=branch), alpha=0.7,size=5, color="darkred") +
  
  # show circles at each node colored according to posterior prob
  geom_nodepoint(aes(fill=posterior_bins,subset=!(node %in% node_ignore)),shape=21,alpha=1,size=3) +
  #geom_nodepoint(aes(fill=posterior_bins,subset=(node != 170)),shape=21,alpha=1,size=3) +
  scale_fill_manual(na.translate=F,name="Posterior node support",values=clr_map) +
  
  # only show tip label for outgroup
  geom_tiplab(aes(subset=(node==1)),parse=T) + 
  
  # here I manually label each species as a clade. I tried several ways to
  # automate this and couldn't get them to format properly so it's just done
  # manually. the lone geom_strip call is because that species doesn't have a
  # "clade" since it's just one tip so it has to be a "strip" label instead of a clade label
  geom_cladelabel(node=212,'atop(bolditalic(C.~cf.~randalli)~"           ","(Western Indian Ocean)")',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=206,'bolditalic(C.~fuscoguttatus)',align=F,offset=0.0026,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=198,'bolditalic(C.~filamentosus)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_strip(80,80,'bolditalic(C.~chelomatus)',align=T,offset=offs,barsize=bars,parse=T,extend = 0.7, fontsize = font.size) +
  
  geom_cladelabel(node=194,'bolditalic(C.~auritus)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=221,'bolditalic(C.~polyzona)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=225,'atop(bolditalic(C.~vanderbilti)~"              ","(Hawai‘i & Johnston Atoll)")',align=F,offset=0.00335,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=223,'atop(bolditalic(C.~matatakaro)~bold( sp.~nov.),"(South/Central Pacific)      ")',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=226,'bolditalic(C.~stigmaticus)',align=T,offset=-0.085,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=228,'bolditalic(C.~castaneus)',align=F,offset=0.001,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=241,'atop(bolditalic(C.~variolosus),"(Pacific Plate)    ")',align=T,offset=-0.08,barsize=bars,parse=T, fontsize = font.size) +
  
  # geom_cladelabel(node=141,'bolditalic(C.~`"`~lineopunctatus~`"`)~"(Hawai‘i)"',align=F,offset=offs+0.002,barsize=bars,parse=T) +
  geom_cladelabel(node=179,'bolditalic(C.~quagga)',align=F,offset=offs,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=185,'bolditalic(C.~"\\"lineopunctatus\\"")~"(Hawai‘i)"',align=F,offset=offs+0.0028,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=186,'bolditalic(C.~jenningsi)',align=T,offset=offs-0.0131,barsize=bars,parse=T, fontsize = font.size) +
  
  geom_cladelabel(node=174,'bolditalic(C.~alboapicalis)',align=T,offset=offs-0.008,barsize=bars,parse=T, fontsize = font.size) +
  geom_cladelabel(node=175,'bolditalic(C.~obscurus)',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +
  # geom_cladelabel(node=147,'bolditalic(C.~`"`~patuki~`"`)~"(Rapa Nui)"',align=T,offset=offs2,barsize=bars,parse=T) +
  geom_cladelabel(node=176,'bolditalic(C.~"\\"patuki\\"")~"(Rapa Nui)"',align=T,offset=offs2,barsize=bars,parse=T, fontsize = font.size) +
  
  
  # show the branch length scale
  geom_treescale(x=0.15,y=-5) +
  # extend the viewable area to avoid text cutoff
  xlim(-0.005,0.275) +
  # position and style the posterior prob color legend
  theme(legend.position = c(0.1,0.9)) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# ---- nodes numbers of note ----
# 212:  castaneus/randalli
# 206:  fuscoguttatus
# 198:  filamentosus
# 80:   chelomatus
# 194:  auritus/heemstraorum
# 221:  polyzona
# 225:  vanderbilti
# 223:  matatakaro
# 226:  stigmaticus
# 228:  castaneus
# 241:  variolosus
# 178:  quagga/lineopunctatus
# 179:  quagga
# 185:  lineopunctatus
# 186:  jenningsi
# 174:  alboapicalis
# 175:  obscurus
# 176:  patuki
# 1:    outgroup
# 170:  outgroup split?


