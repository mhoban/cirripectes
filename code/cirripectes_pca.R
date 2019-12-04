library(tidyverse)
library(readxl)
library(FactoMineR)
library(ggbiplot)
library(missMDA)
library(naniar)
library(plotly)
library(viridis)
library(beyonce)
library(here)


fish <- read_excel(here("data","cirripectes_specimens.xlsx"),sheet=1) %>%
  #filter((nominal_sp %in% c("quagga","lineopunctatus"))) %>%
  filter((nominal_sp %in% c("vanderbilti","redhead","variolosus"))) %>%
  mutate_if(is.character,as.factor) %>%
  dplyr::select(-starts_with('canines'),-maxillary,-dentary,
                -ends_with('color'),-lower_lip,-bony_interorbit,-notes) %>%
  mutate(head_body = hl/sl) %>%
  mutate(depth_body = depth_anus/sl) %>%
  #######
  # mutate(nuchal_ratio = nuchal_cirri/sl) %>%
  # mutate(nasal_ratio = (nasal_l+nasal_r)/sl) %>%
  # mutate(supraorbital_ratio = (supraorbital_l+supraorbital_r)/sl) %>%
  # mutate(separation_ratio = (nuchal_separation_l+nuchal_separation_d+nuchal_separation_r)/sl) %>%
  #######
  mutate(postorbit_head = postorbit_midnuchal/hl) %>%
  mutate(lowernuchal_head = lowernuchal_operculum/hl) %>%
  mutate(dorsal_ratio = dorsalI/(dorsalII+dorsalIII)) %>%
  mutate(dorsal_ratio2 = dorsal_last/dorsalI) %>%
  mutate(tube_ratio = ll_tubes/sl) %>%
  mutate(tube_under_ratio = last_tube_under/sl) %>%
  mutate(notch_ratio = notch_height/first_ray) %>%
  mutate(nasal_cirri = nasal_l + nasal_r) %>%
  mutate(supraorbital_cirri = supraorbital_l + supraorbital_r) %>%
  mutate(supraorbital_ratio = supraorbital_length / hl) %>%
  mutate(lateral_seps=(nuchal_separation_l+nuchal_separation_r)/2)
  # select(-hl,-sl,-postorbit_midnuchal,-lowernuchal_operculum,-starts_with('dorsalI'),-dorsal_last,
  #        -notch_height,-first_ray,-dorsal_attached,-depth_anus,-pterygiophores_lastvert,-anal_split,
  #        -nuchal_shape,-genitalia_type,-postnuchal_tube_type,-anal_s,-pectoral_s)
# fish <- fish %>%
#   mutate(
#     nominal_sp = case_when(
#       (locality=="Johnston Atoll" & nominal_sp=="quagga") ~ "lineopunctatus",
#       TRUE ~ as.character(nominal_sp)
#     )
#   )
# fish <- fish %>%
#   mutate(
#     nominal_sp = case_when(
#       (nominal_sp=="variolosus" & postnuchal_tube_type=="redhead") ~ "redhead",
#       TRUE ~ as.character(nominal_sp)
#     )
#   )
levels(fish$nominal_sp) <- c("matatakaro sp. nov.","vanderbilti","variolosus")
  
fish.data <- fish %>% 
  select(
    dorsal_s,dorsal_r,anal_s,anal_r,pectoral_s,pectoral_r,segmented_caudal_d,segmented_caudal_v,
    branched_caudal_d,branched_caudal_v,pelvic_s,pelvic_r,nuchal_bases,nuchal_cirri,nasal_cirri,
    supraorbital_cirri,tube_ratio,tube_under_ratio,dorsal_ratio,dorsal_ratio2,#,supraorbital_ratio
    head_body,postorbit_head,lowernuchal_head,notch_ratio,depth_body,
    nuchal_separation_d,last_pleural,procurrent_d,procurrent_v,vertebrae_precaudal,
    vertebrae_caudal,epipleural,
    # nuchal_separation_l,nuchal_separation_r,
    lateral_seps
  ) %>%
  select_if(~var(.,na.rm=T) != 0) # remove columns with zero variance (PCA missing values imputation can't handle that)
  

# scale data 
fish.data <- scale(fish.data)

# impute missing values for PCA
fish.data.imp <- imputePCA(as.data.frame(fish.data))
# do the PCA with the imputed values
fish.pca <- prcomp(fish.data.imp$completeObs)#PCA(fish.data.imp$completeObs)
percent.explained <- (fish.pca$sdev^2)/sum(fish.pca$sdev^2)
pc1.explained <- round(percent.explained[1]*100,1)
pc2.explained <- round(percent.explained[2]*100,1)


p <- ggbiplot(fish.pca,groups=fish$nominal_sp,var.axes = T,ellipse = T,loading.vars = 10) + 
  geom_point(aes(color=fish$nominal_sp,text=paste("lot no:",fish$lot_no,"<br>","locality:",fish$locality)),size=3) +
  # scale_color_viridis(name="Species",discrete = T) +
  # scale_color_brewer(name="Species",palette = "Dark2") +
  # scale_color_manual(name="Species",values = wesanderson::wes_palette("GrandBudapest1",type="discrete",n=3)) +
  scale_color_manual(name="Species",values=beyonce_palette(78,3), guide=F) +#labels=c(expression(italic("matatakaro sp. nov.")),expression(italic("vanderbilti")),expression(italic("variolosus")))) + 
  theme_bw() +
  xlim(-3,3) +
  xlab(paste0("PC1 (",pc1.explained,"% explained variance)")) +
  ylab(paste0("PC2 (",pc2.explained,"% explained variance)")) +
  theme(legend.position = "bottom")
# ggplotly(p)
quartz()
p
