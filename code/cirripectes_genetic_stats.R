library(tidyverse)
library(poppr)
library(pegas)
library(here)



all_samples <- read_tsv(here("data","samples.tab"))

# variolosus stuff
alignment <- read.dna(here("sequence","variolosus-redhead.fasta"),format="fasta")

variolosus_samples <- all_samples %>%
  filter(species == "Cirripectes variolosus") %>%
  filter(region %in% c("Line Islands","Marquesas")) %>%
  mutate_if(is.character,as.factor)

variolosus <- alignment[labels(alignment) %in% variolosus_samples$id,]
variolosus_samples <- variolosus_samples[match(labels(variolosus),variolosus_samples$id),] %>%
  droplevels()

v.pops <- DNAbin2genind(variolosus,pop=variolosus_samples$region)
strata(v.pops) <- data.frame(pop=variolosus_samples$region)

d <- dist.dna(variolosus)
p.amova <- poppr.amova(v.pops,~pop)
b.amova <- pegas::amova(d ~ region,data=variolosus_samples)

p.amova$statphi

sig2 <- setNames(b.amova$varcomp$sigma2, rownames(b.amova$varcomp))
getPhi(sig2)



# vanderbilti stuff
alignment <- read.dna(here("sequence","vanderbilti-redhead-coi.fasta"),format="fasta")
vanderbilti_samples <- all_samples %>%
  filter(species == "Cirripectes vanderbilti") %>%
  mutate(island=str_replace(island,"Laysan","Laysan-Lisi-FFS"),
         island=str_replace(island,"Lisianski","Laysan-Lisi-FFS"),
         island=str_replace(island,"French Frigate Shoals","Laysan-Lisi-FFS")) %>%
  mutate_if(is.character,as.factor)


vanderbilti <- alignment[labels(alignment) %in% vanderbilti_samples$id,]
vanderbilti_samples <- vanderbilti_samples[match(labels(vanderbilti),vanderbilti_samples$id),] %>%
  droplevels()

v.pops <- DNAbin2genind(vanderbilti,pop=vanderbilti_samples$island)
strata(v.pops) <- data.frame(pop=vanderbilti_samples$island)

ff <- poppr.amova(v.pops,~pop)
randtest(ff,nrepet = 1000)


d <- dist.dna(vanderbilti)
fa <- amova(d ~ island,data=vanderbilti_samples)

haps.vanderbilti <- haplotype(vanderbilti)
hap.div(haps.vanderbilti,variance = T)
nuc.div(vanderbilti)

alignment <- read.dna(here("sequence","vanderbilti-redhead-coi.fasta"),format="fasta")
new.sp <- alignment[!grepl('Cvd',labels(alignment)),]
hap.div(new.sp,variance = T)
nuc.div(new.sp)


