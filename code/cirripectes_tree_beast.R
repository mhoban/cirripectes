library(tidyverse)
library(ggtree)
library(ggplot2)
library(deeptime)
library(treeio)
rm(list=ls)

tree <- ggtree::read.beast("../data/blennies-combined-consensus-renamed.tre")

# duplicate tip taxa to drop from the final displayed tree
tip_drop <- c("ANGBF9231-12_Cirripectes_auritus_0",
              "AUSTR154-13_Cirripectes_fuscoguttatus_0",
              "AUSTR398-13_Cirripectes_alboapicalis_0",
              #"AUSTR544-13_Cirripectes_obscurus_0",
              "AUSTR546-13_Cirripectes_jenningsi_0",
              "COLOR408-15_Cirripectes_cf_randalli_0",
              "Crv-004_Cirripectes_matatakaro_sp_nov_0",
              "Crv-026_Cirripectes_variolosus_0",
              "Cvd-043_Cirripectes_vanderbilti_0",
              "DRRA1725-12_Cirripectes_patuki_0",
              "FLHI377-09_Cirripectes_lineopunctatus_0",
              #"FLHI849-14_Cirripectes_obscurus_0",
              "FPFLB317-12_Cirripectes_quagga_0",
              "GBMIN95957-17_Cirripectes_filamentosus_0",
              "KU4315_Cirripectes_polyzona_0",
              "LIFS794-08_Cirripectes_castaneus_0",
              "UKFBI528-08_Cirripectes_stigmaticus_0")


# filter out redundant taxa and cut posterior probability into discrete bins
# also reformat various tip labels
td <- as_tibble(treeio::drop.tip(tree,tip_drop)) %>%
  mutate(
    posterior_bins = cut(
      posterior,
      c(-Inf,0.60,0.75,0.95,Inf),
      labels=c("<0.60","0.60-0.75","0.75-0.95","0.95+")
    ),
    # here we cut out the sample IDs and tip dates from tip labels
    label=map_chr(label,function(x) {
      if (!is.na(x)) {
        f <- strsplit(x,"_")
        return(paste(f[[1]][2:(length(f[[1]])-1)],collapse=" "))  
      } else {
        return(NA)
      }
    }),
    # these are various tip label cleanup operations
    label=str_replace(label,"Parablennius prokofievi","Parablennius prokofievi†"), # dagger denotes extinct taxa
    label=str_replace(label,"Paranarrhichas damesi","Paranarrhichas damesi†"), # dagger denotes extinct taxa
    label=str_replace(label,"sp nov","sp. nov."),
    label=str_replace(label,"cf randalli","cf. randalli"),
    label=str_replace(label,"patuki",'"patuki"'),
    label=str_replace(label,"lineopunctatus",'"lineopunctatus"')
  )

# map node support labels to fill colors
clr_map <- c("<0.60"="white","0.60-0.75"="gray75","0.75-0.95"="gray40","0.95+"="black")
# cast tree data back to treedata
tr <- as.treedata(td)
p <- ggtree(tr,ladderize = T) + 
  # show root edge
  geom_rootedge(rootedge = 1) + 
  # fill node symbols by posterior probability
  scale_fill_manual(na.translate=F,name="Posterior node support",values=clr_map) +
  # label tree tips
  geom_tiplab() +
  # show node bars for 95% HPD divergence time estimate
  geom_range(range="height_0.95_HPD",color="red",alpha=0.3,size=2,branch.length = "height") +
  # show node symbols (call this after geom_range so the node points are on top)
  geom_nodepoint(aes(fill=posterior_bins),shape=21,alpha=1,size=3) +
  # show time before present axis
  theme_tree2() + 
  # show legend for node support colors
  theme(legend.position = c(0.1,0.9)) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

quartz()
# reverse axis and extend the right side so tip labels are fully visible
p <- revts(p) + xlim(-45,5) + scale_x_continuous(labels = abs, breaks=pretty(-tr@data$height,n=10)) 


# add geoscale with geological epochs
p <- gggeo_scale(p, dat = "international epochs", abbrv = F, size = 3, neg = TRUE, rot=0)
p

