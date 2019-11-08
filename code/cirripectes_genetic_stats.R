library(tidyverse)
library(poppr)
library(pegas)
rm(list=ls())

alignment <- read.dna("sequence/vanderbilti-redhead-coi.fasta",format="fasta")
samples <- read_tsv("samples.tab") %>%
  filter(species == "Cirripectes vanderbilti") %>%
  mutate(island=str_replace(island,"Laysan","Laysan-Lisi-FFS"),
         island=str_replace(island,"Lisianski","Laysan-Lisi-FFS"),
         island=str_replace(island,"French Frigate Shoals","Laysan-Lisi-FFS")) %>%
  mutate_if(is.character,as.factor)

vanderbilti <- alignment[labels(alignment) %in% samples$id,]
samples <- samples[match(labels(vanderbilti),samples$id),] %>%
  droplevels()

v.pops <- DNAbin2genind(vanderbilti,pop=samples$island)
strata(v.pops) <- data.frame(pop=samples$island)

ff <- poppr.amova(v.pops,~pop)


d <- dist.dna(vanderbilti)
fa <- amova(d ~ island,data=samples)

haps.vanderbilti <- haplotype(vanderbilti)
hap.div(haps.vanderbilti,variance = T)
nuc.div(vanderbilti)

alignment <- read.dna("sequence/vanderbilti-redhead-coi.fasta",format="fasta")
new.sp <- alignment[!grepl('Cvd',labels(alignment)),]
hap.div(new.sp,variance = T)
nuc.div(new.sp)


