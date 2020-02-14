[![DOI](https://zenodo.org/badge/220354091.svg)](https://zenodo.org/badge/latestdoi/220354091)

# *Cirripectes matatakaro* sp. nov. species description
This repository contains data, code, and supplementary information for the paper:

Hoban, ML, Williams, JT, 2019. *Cirripectes matatakaro*, a new species of combtooth blenny from the Central Pacific, illuminates the origins of the Hawaiian fish fauna (in review)

## Contents
- cirripectes.Rproj -- RStudio project for code and analyses

### code
- cirripectes_genetic_stats.R -- calculate genetic summary statistics
- cirripectes_map.R -- create distribution map (requires google API key / billing)
- cirripectes_pca.R -- calculate and plot principal components analysis (PCA)
- cirripectes_tree_beast.R -- format and display divergence timetree
  - this is left over from a previous version of the paper
- cirripectes_tree_mrbayes.R -- format and display COI phylogeny

### data
- cirripectes_specimens.xlsx -- morphological specimen data
- new_sp_occurrence.csv -- geographical occurrence data for C. matatakaro returned from google geocoding algorithm
  - 'lon': longitude
  - 'lat': latitude
  - 'type': location type
  - 'loctype': basis of geocode calculation
  - 'address': text description of location
  - 'north': northern bounding latitude
  - 'south': southern bounding latitude
  - 'east': eastern bounding longitude
  - 'west': western bounding longitude
- samples.tab -- sequencee/specimen data for haplotype network construction
  - 'id': sequence identifier
  - 'species': species
  - 'island','country','region': locality information
- variolosus-vanderbilti-geocoded.csv -- specimen occurrence data from GBIF with geocoded locations (where coordinates where lacking)
  - see GBIF.org for file format
- PartitionFinder_output.txt -- best partitioned substitution model scenario (AICc) output from PartitionFinder
- jModelTest2_output.txt -- best nonpartitioned substitution model scenario (AICc) output from jModelTest2

### tree
- cirripectes-684-reduced.nex -- MrBayes input file / NEXUS alignment
  - partitioned substitution model (see below)
- cirripectes-684-reduced.nex.con.tre -- COI phylogeny tree file (current, used)
  - outgroup: *Plagiotremus tapeinosoma*
  - MrBayes MCMC run: 2e8 generations, 20% burn-in
  - partitioned substitution model
    - codon 1: GTR+G
    - codon 2: F81+G
    - codon 3: GTR+I+G
- cirripectes-HKY-nonpartitioned.tre -- COI phylogeny tree file (secondary)
  -this is a secondary analysis using a nonpartitioned substitution model
  - outgroup: *Plagiotremus tapeinosoma*
  - MrBayes MCMC run: 2e7 generations, 20% burn-in
  - substitution mode: HKY+I+G
- treebuild2.nex.con.tre -- COI phylogeny tree file 
  - this is retained from a previous version of the manuscript
  - outgroup: *Exallias brevis*
  - MrBayes MCMC run: 3e7 generations, 20% burn-in
  - substitution model: GTR+I+G

### sequence
- vanderbilti-redhead-coi.fasta -- COI sequence data for C. vanderbilti and C. matatakaro
- variolosus-redhead.fasta -- COI sequence data for C. variolosus and C. matatakaro




