#DATA ANALYSIS TFM#

#script written in R version 4.2.3

rm(list=ls())
#import libraries
library(readxl) #ver 1.4.0
library(ggplot2) #ver 3.4.2
library(forcats) #ver 1.0.0
library(dplyr) #ver 1.0.9
library(stringr) #ver 1.5.0
library(devtools) #ver 2.4.4
library(vegan) #ver 2.6.2
#devtools::install_github("yanlinlin82/ggvenn")    <-- use to install the package ggvenn
library(ggvenn) #ver 0.1.10
library(pollimetry) #ver 1.0.1
library(abdiv) #ver 0.2.0

#import datasets:

pap<- #pap_2010_bd_total.xlsx, sheet 1
gar<-#gar_2010_bd_total.xlsx, sheet 2
col_ref<- #bd_coleccio_creaf.xlsx, sheet 1
meteo_pap<- #climograma_tfm.xlsx, sheet 1
meteo_gar<- #climograma_tfm.xlsx, sheet 2


###################################################################################################################################


#CLIMOGRAPH
months<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
meteo_pap <- transform(meteo_pap, month = factor(month, levels = months))
meteo_gar <- transform(meteo_gar, month = factor(month, levels = months))

#climograph Papiol
ggplot(data = meteo_pap, mapping = aes(x = month, y = mean_prec/2, group = 1)) + 
  geom_bar(stat = "identity", color="#75b2fc", fill="#75b2fc", width = 0.5, alpha = 0.8)+ 
  geom_line(mapping = aes(y = mean_temp), color="#F8766D", size=1.5)+
  scale_y_continuous("Temperature (ºC)", 
                     sec.axis = sec_axis(~ .*2, name = "Precipitation (mm)", breaks = c(0, 20, 40, 60, 80, 100)), limits = c(0,50))+ 
  xlab("Month")+ theme_classic() + theme(text = element_text(size = 15), axis.text.x = element_text(size = 13.5, angle = 45, hjust = 1))+
  theme(axis.title.y.right = element_text(angle = 90))

#climograph Garraf
ggplot(data = meteo_gar, mapping = aes(x = month, y = mean_prec/2, group = 1)) + 
  geom_bar(stat = "identity", color="#75b2fc", fill="#75b2fc", width = 0.5, alpha = 0.8)+ 
  geom_line(mapping = aes(y = mean_temp), color="#F8766D", size=1.5)+
  scale_y_continuous("Temperature (ºC)", 
                     sec.axis = sec_axis(~ .*2, name = "Precipitation (mm)", breaks = c(0, 20, 40, 60, 80, 100)))+ 
  xlab("Month")+ theme_classic() + theme(text = element_text(size = 15), axis.text.x = element_text(size = 13.5, angle = 45, hjust = 1))+
  theme(axis.title.y.right = element_text(angle = 90))


###################################################################################################################################


#CREATION OF CHECKLISTS AND DATA PREPARATION:

#garraf community:
gar_2<-gar |>
  filter(sp_def != "sp.", sp_def != "-")
garraf_comunitat<-merge(col_ref, gar_2, by.x = c("name"), by.y = c("gen_sp_def")) |>
  mutate(zona = "Garraf")
pre_llistat_garraf<-data.frame(unique(garraf_comunitat$name))
llistat_garraf<-merge(col_ref, pre_llistat_garraf, by.x = c("name"), by.y = c("unique.garraf_comunitat.name.")) |>
  mutate(zona = "Garraf")  
#papiol community:
pap_2<-pap |>
  filter(sp_def != "sp.", sp_def !="?", sp_def !="-", h_setmana !="")
papiol_comunitat<-merge(col_ref, pap_2, by.x = c("name"), by.y = c("gen_sp_def")) |>
  mutate(zona = "Papiol")
pre_llistat_papiol<-data.frame(unique(papiol_comunitat$name))
llistat_papiol<-merge(col_ref,pre_llistat_papiol, by.x = c("name"), by.y = c("unique.papiol_comunitat.name.")) |>
  mutate(zona = "Papiol")

#total/pooled community:
llistat_total<-rbind(llistat_garraf, llistat_papiol)
llistat_total$zona<-factor(llistat_total$zona)
llistat_total$zona<-relevel(llistat_total$zona, "Papiol")
llistat_total$its<-as.numeric(llistat_total$its)
llistat_total$prob<-as.numeric(llistat_total$prob)
llistat_total$sociality<-factor(llistat_total$sociality)

###################################################################################################################################


#DATA PREPARATION FOR ABUNDANCE ANALYSES:

#Papiol:
pre_pap_abund<-pap_2 |>
  filter(!is.na(pap_2$abund_setmana))
#split between max and min:
pre_pap_abund[c("min", "max")]<-str_split_fixed(pre_pap_abund$abund_setmana, "-", 2)
pre_pap_abund_2<-pre_pap_abund |>
  filter(!is.na(pre_pap_abund$min)) |>
  filter(!is.na(pre_pap_abund$max))
pre_pap_abund_2$gen_sp_def<-factor(pre_pap_abund_2$gen_sp_def)
pre_pap_abund_2$min<-as.numeric(pre_pap_abund_2$min)
#add min data to those cases without maxs
#calculate the mean (max+min/2):
pap_abund<-pre_pap_abund_2 |>
  mutate(maxims = ifelse(max %in% "", min, max)) |>
  select(!max) |>
  rename(max = "maxims") |>
  mutate(mean = round(((as.numeric(min)+as.numeric(max))/2)))
summary(pre_pap_abund_2$min)
#daily summary:  
pap_abund_resum<-group_by(pap_abund, gen_sp_def) %>% 
  summarise(sum_min = sum(as.numeric(min)), sum_max = sum(as.numeric(max)), sum_mean = sum(as.numeric(mean))) |>
  mutate(zona = "Papiol")|>
  filter(gen_sp_def != "Apis mellifera")|>
  mutate(prop_abund = (sum_mean/sum(sum_mean))*100) 
pap_abund_resum <- pap_abund_resum[order(-pap_abund_resum$sum_mean),] %>%
  mutate(rank = 1:nrow(pap_abund_resum))

#Garraf:
pre_gar_abund<-gar_2 |>
  filter(!is.na(gar_2$abund_setmana)) |>
  filter(abund_grup!="?")
#split between max and min:
pre_gar_abund[c("min", "max")]<-str_split_fixed(pre_gar_abund$abund_setmana, "-", 2)
pre_gar_abund$min<-as.numeric(pre_gar_abund$min)
pre_gar_abund$gen_sp_def<-factor(pre_gar_abund$gen_sp_def)
#add min data to those cases without maxs
#calculate the mean (max+min/2):
gar_abund<-pre_gar_abund |>
  mutate(maxims = ifelse(max %in% "", min, max)) |>
  select(!max) |>
  rename(max = "maxims") |>
  mutate(mean = round(((as.numeric(min)+as.numeric(max))/2)))
#daily summary: 
gar_abund_resum<-group_by(gar_abund, gen_sp_def) %>% 
  summarise(sum_min = sum(as.numeric(min)), sum_max = sum(as.numeric(max)), sum_mean = sum(as.numeric(mean))) |>
  mutate(zona = "Garraf") |>
  filter(gen_sp_def != "Apis mellifera")|>
  mutate(prop_abund = (sum_mean/sum(sum_mean))*100)
gar_abund_resum <- gar_abund_resum[order(-gar_abund_resum$sum_mean),] %>%
  mutate(rank = 1:nrow(gar_abund_resum))

#add together both communities:
pap_gar_resum<-rbind(pap_abund_resum, gar_abund_resum)
pap_gar_resum[c("gen", "sp")]<-str_split_fixed(pap_gar_resum$gen_sp_def, " ", 2)
pap_gar_resum$zona<-factor(pap_gar_resum$zona)
pap_gar_resum$zona<-relevel(pap_gar_resum$zona, "Papiol")
pap_gar_resum$gen<-factor(pap_gar_resum$gen)
pap_gar_resum$sp<-factor(pap_gar_resum$sp)
pap_gar_resum$sum_min<-as.numeric(pap_gar_resum$sum_min)
pap_gar_resum$sum_max<-as.numeric(pap_gar_resum$sum_max)
pap_gar_resum$sum_mean<-as.numeric(pap_gar_resum$sum_mean)

#data preparation for the comparative graph, only for shared species:
rank_compartit<-pap_abund_resum |>
  filter(gen_sp_def %in% gar_abund_resum$gen_sp_def) |>
  select(gen_sp_def, rank) 
rank_abund_compartit<-merge(rank_compartit, gar_abund_resum, by.x = c("gen_sp_def"), by.y=c("gen_sp_def")) %>%
  select(gen_sp_def, sum_min, sum_max, sum_mean, zona, rank.x, prop_abund) |>
  rename(rank= `rank.x`)
comp_all_papiol<-rbind(pap_abund_resum, rank_abund_compartit)
comp_all_papiol$zona<-factor(comp_all_papiol$zona)
comp_all_papiol$zona<-relevel(comp_all_papiol$zona, "Papiol")

rank_compartit_2<-pap_abund_resum |>
  filter(gen_sp_def %in% gar_abund_resum$gen_sp_def) |>
  select(-rank)
rank_compartit_2<-rank_compartit_2[order(-rank_compartit_2$sum_mean),] %>%
  mutate(rank = 1:nrow(rank_compartit_2))
rank_abund_compartit_2<-merge(rank_compartit_2, gar_abund_resum, by.x = c("gen_sp_def"), by.y=c("gen_sp_def")) %>%
  select(gen_sp_def, sum_min.y, sum_max.y, sum_mean.y, zona.y, prop_abund.y, rank.x) |>
  rename(rank= `rank.x`, sum_min = "sum_min.y", sum_max = "sum_max.y", sum_mean = "sum_mean.y", prop_abund = "prop_abund.y",zona = "zona.y", rank = "rank.x")
comp_only_shared<-rbind(rank_compartit_2, rank_abund_compartit_2) %>%
  select(-prop_abund)
comp_only_shared$zona<-factor(comp_only_shared$zona)
comp_only_shared$zona<-relevel(comp_only_shared$zona, "Papiol")

#data preparation for the comparative graph, for all of the species:
#papiol:
pap_abund_resum_2<-pap_abund_resum |>
  mutate(filling = "Papiol")
sp_exclusives_gar_info<-gar_abund_resum |>
  filter(!(gen_sp_def %in% pap_abund_resum$gen_sp_def))
rank_pap<-nrow(pap_abund_resum_2)
sp_exclusives_gar_info<- sp_exclusives_gar_info %>% 
  mutate(rank = (rank_pap+1):((rank_pap)+nrow(sp_exclusives_gar_info)), filling = "Garraf")

sp_exclusives_gar_info_2<-sp_exclusives_gar_info %>%
  mutate(sum_mean = 0, sum_min = 0, sum_max = 0, prop_abund = 0, zona = "Papiol", filling = "Absent")
total_pap_comp<-rbind(pap_abund_resum_2, sp_exclusives_gar_info_2)

#garraf:
rank_abund_compartit_3 <-rank_abund_compartit |>
  mutate(filling = "Garraf")
sp_exclusives_pap_info<-pap_abund_resum |>
  filter(!(gen_sp_def %in% gar_abund_resum$gen_sp_def))%>% 
  mutate(sum_mean = 0, sum_min = 0, sum_max = 0, prop_abund = 0, zona = "Garraf", filling = "Absent")
total_gar_comp<-rbind(sp_exclusives_pap_info, sp_exclusives_gar_info, rank_abund_compartit_3)

total_comp<-rbind(total_pap_comp, total_gar_comp) %>%
  mutate(log_abund = log(sum_mean+1))
total_comp$zona<-factor(total_comp$zona)
total_comp$zona<-relevel(total_comp$zona, "Papiol")


###################################################################################################################################

#ACCUMULATION CURVES

#Papiol:
pap_sp_acc_curve<-pap_abund[rep(seq_len(nrow(pap_abund)), times = pap_abund$mean), ]
pap_table<-table(pap_sp_acc_curve$num_setmana, pap_sp_acc_curve$gen_sp_def)
pap_matrix<-as.data.frame.matrix(pap_table)

sac.pap <- specaccum(pap_matrix, method = "random", permutations = 500)

### Number of unseen species 
specpool(pap_matrix)

#Garraf:
gar_sp_acc_curve<-gar_abund[rep(seq_len(nrow(gar_abund)), times = gar_abund$mean), ]
gar_table<-table(gar_sp_acc_curve$num_setmana, gar_sp_acc_curve$gen_sp_def)
gar_matrix<-as.data.frame.matrix(gar_table)
#Calculs de SAC
sac.gar <- specaccum(gar_matrix, method = "random", permutations = 500)

### Number of unseen species
specpool(gar_matrix)

#data preparation to plot it using ggplot:
tidy_specaccum <- function(x) {
  data.frame(
    site = x$sites,
    richness = x$richness,
    sd = x$sd)
}

tidy_pap <- tidy_specaccum(sac.pap) %>%
  mutate(lloc = "Papiol")
tidy_gar <- tidy_specaccum(sac.gar) %>%
  mutate(lloc = "Garraf")
tidy_total<-rbind(tidy_pap, tidy_gar)
tidy_total$lloc<-factor(tidy_total$lloc)
tidy_total$lloc<-relevel(tidy_total$lloc, "Papiol")

#plot:
ggplot(tidy_total) + geom_line(aes(site, richness, colour = lloc), linewidth=1.5) +
  theme_classic()+
  geom_linerange(aes(x = site, ymin = richness - 2*sd, ymax = richness + 2*sd, colour = lloc), linewidth=0.5, alpha = 0.4)+ 
  ylim(0, 250)+xlab("Weeks")+ylab("Accumulated richness")+labs(col="Site")+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))


###################################################################################################################################

#BASIC QUESTIONS:

#How many species are there in each family?
especies_familia<-data.frame("Garraf" = table(llistat_garraf$fam), "Papiol" = table(llistat_papiol$fam))

#How many genera are there in each community?
generes_com<-data.frame("Garraf" = length(unique(llistat_garraf$gen)), "Papiol" = length(unique(llistat_papiol$gen)))

#Which genera are exclusive to papiol?
gen_unics<-unique(llistat_papiol$gen)
gen_exclusius<-data.frame()
for (i in gen_unics) {
  ifelse(i %in% unique(llistat_garraf$gen),"",gen_exclusius<-rbind(gen_exclusius, i))
}
gen_exclusius_2<-gen_exclusius
gen_exclusius<-data.frame()

#How many species have been found in each community?
especies_com<-data.frame("Garraf" = length(unique(llistat_garraf$name)), "Papiol" = length(unique(llistat_papiol$name)))

#How many species does each genus have?
llistat_total$zona<-factor(llistat_total$zona)
ordre<-c("Andrena", "Lasioglossum", "Hylaeus", "Nomada", "Osmia",
         "Anthophora", "Megachile", "Sphecodes", "Ceratina",
         "Colletes", "Eucera", "Hoplitis", "Anthidium", "Amegilla", "Halictus", 
         "Seladonia", "Coelioxys", "Epeolus", "Heriades", "Panurgus", 
         "Anthidiellum", "Bombus", "Chelostoma", "Nomiapis", "Protosmia", "Rhodanthidium",
         "Stelis", "Tetralonia", "Thyreus","Xylocopa", "Aglaoapis", "Ammobates",
         "Apis", "Dioxys", "Lithurgus", "Melecta",
         "Nomioides", "Panurginus", "Pasites","Pseudoanthidium", "Trachusa")
llistat_total <- transform(llistat_total, gen = factor(gen, levels = ordre))
llistat_total$zona<-relevel(llistat_total$zona, "Papiol")
unique(llistat_total$gen)
ggplot(llistat_total, aes(gen, fill = zona))+geom_bar(position = position_dodge(preserve = "single"))+
  scale_y_continuous(breaks = seq(0, 35, by = 5))+
  theme_classic() + 
  labs(fill = "Site") +
  xlab("Genus")+
  ylab("Number of species")+theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                                  axis.title = element_text(size = 20), 
                                  axis.text.y = element_text(size = 15),
                                  legend.title = element_text(size = 15),
                                  legend.text = element_text(size = 15))


#chi-squared test to compare taxonomic composition:
table(llistat_total$gen, llistat_total$zona)
test_gen<-matrix(c(31,20,4,1,22,17,10,6,12,3,19,10,26,14,16,10,16,10,9,5,3,1,9,5,9,1,12,12,11,9,15,8,15,7,7,1), ncol = 18)
table_test_gen<-as.table(test_gen)
gen_chi<-chisq.test(table_test_gen)
gen_chi$expected #to ensure <=20% of expected counts are <5


#How many shared species between both communities?
sp_compartides<-data.frame()
for (i in unique(llistat_garraf$name)) {
  ifelse(i %in% unique(llistat_papiol$name), sp_compartides<-rbind(sp_compartides, i), "")
}
sp_compartides_2<-sp_compartides
sp_compartides<-data.frame()
print(sp_compartides_2)

#How many exclusive species of each community?
#exclusive of papiol:
sp_exclusives_pap<-data.frame()
for (i in unique(llistat_papiol$name)) {
  ifelse(i %in% unique(llistat_garraf$name),"",sp_exclusives_pap<-rbind(sp_exclusives_pap, i))
}
sp_exclusives_pap_2<-sp_exclusives_pap
sp_exclusives_pap<-data.frame()
print(sp_exclusives_pap_2)

#exclusive of garraf:
sp_exclusives_gar<-data.frame()
for (i in unique(llistat_garraf$name)) {
  ifelse(i %in% unique(llistat_papiol$name),"",sp_exclusives_gar<-rbind(sp_exclusives_gar, i))
}
sp_exclusives_gar_2<-sp_exclusives_gar
sp_exclusives_gar<-data.frame()
print(sp_exclusives_gar_2)

#Venn diagram:
venn_list <- list(
  Papiol = llistat_papiol$name, 
  Garraf = llistat_garraf$name
)
ggvenn(
  venn_list, 
  fill_color = c("#F8766D", "#00BFC4"),
  stroke_size = 0.5, set_name_size = 8, text_size = 10, fill_alpha = 0.4,
)
#this diagram was then done manually in order to change the size of Garraf's circle


###################################################################################################################################


#FUNCTIONAL TRAITS

#Body size (ITS) distirbution:
#preparation for analysis:
its_grafic<-llistat_total |>
  filter(!is.na(llistat_total$its))
its_grafic$its<-as.numeric(its_grafic$its)
its_grafic$zona<-factor(its_grafic$zona)

#division into categories:
its_categ<-its_grafic %>%
  mutate(its_cat = ifelse(its <= 1, "0 - 1 mm",
                          ifelse(its <= 2, "1 - 2 mm",
                                 ifelse(its <= 3, "2 - 3 mm",
                                        ifelse(its <= 4, "3 - 4 mm", "> 4 mm")))))

count_its_papiol<-data.frame(table(its_categ$its_cat[llistat_total$zona=="Papiol"]))
prop_its_papiol<-count_its_papiol |>
  mutate(prop = (Freq/sum(count_its_papiol$Freq))*100) |>
  mutate(zona = "Papiol")

count_its_garraf<-data.frame(table(its_categ$its_cat[llistat_total$zona=="Garraf"]))
prop_its_garraf<-count_its_garraf |>
  mutate(prop = (Freq/sum(count_its_garraf$Freq))*100) |>
  mutate(zona = "Garraf")

ordre_its<-c("0 - 1 mm","1 - 2 mm","2 - 3 mm", "3 - 4 mm", "> 4 mm")
prop_its_total<-rbind(prop_its_papiol, prop_its_garraf)
prop_its_total$zona<-factor(prop_its_total$zona)
prop_its_total$zona<-relevel(prop_its_total$zona, "Papiol")
prop_its_total <- transform(prop_its_total, Var1 = factor(Var1, levels = ordre_its))
ggplot(prop_its_total, aes(x=Var1, y = prop, fill = zona))+geom_bar(stat="identity", position = "dodge")+
  theme_classic() +
  xlab("Category")+ylab("Proportion (%)")+ labs(fill = "Site") +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))


#chi-squared test for ITS categories:
test_its<-matrix(c(12, 10, 101, 55, 68, 37, 31, 23, 7, 6), ncol = 5)
table_test_its<-as.table(test_its)
its_chi<-chisq.test(table_test_its)
its_chi$expected #to ensure <=20% of expected counts are <5

#-------------------------------------------------------------------------------------------------------------------------------#


#Proboscis length distribution:
#pollimetry calculations:
missing_tongue<-llistat_total %>%
  filter(is.na(llistat_total$prob) & !is.na(llistat_total$its)) #dataframe of missing proboscis measurements, with available ITS measurements

mt_pollimetry<-llistat_total %>%
  filter(is.na(llistat_total$prob) & !is.na(llistat_total$its)) %>%
  mutate(IT = its, Family = fam) %>%
  select(IT, Family) #dataframe ready for pollimetry calculations. Two columns: IT and Family

measurements_tongue<-data.frame(tonguelength(mt_pollimetry, mouthpart = "all")) %>%
  select(Proboscis) #pollimetry calculations

missing_tongue_2<-cbind(missing_tongue, measurements_tongue) %>%
  mutate(prob = Proboscis) %>%
  select(-Proboscis) #pollimetry calculations + dataframe of missing measurements that contains species info

pre_prob_length<-rbind(llistat_total, missing_tongue_2) #dataframe of new measurements + the rest of the species

#preparation for analysis:
proboscis_length<-pre_prob_length %>%
  filter(!is.na(pre_prob_length$prob)) #filtering out NA values

proboscis_length$prob<-as.numeric(proboscis_length$prob) 
proboscis_length$zona<-factor(proboscis_length$zona)


#division into categories:
prob_categ <- proboscis_length %>%
  mutate(prob_cat = ifelse(prob <= 1, "0 - 1 mm",
                           ifelse(prob <= 2, "1 - 2 mm",
                                  ifelse(prob <= 3, "2 - 3 mm",
                                         ifelse(prob <= 4, "3 - 4 mm",
                                                ifelse(prob <= 5,  "4 - 5 mm",
                                                       ifelse(prob <= 6,  "5 - 6 mm",
                                                              ifelse(prob <= 7, "6 - 7 mm",
                                                                     ifelse(prob <= 8, "7 - 8 mm",
                                                                            ifelse(prob <= 9, "8 - 9 mm",
                                                                                   ifelse(prob <= 10, "9 - 10 mm",
                                                                                          ifelse(prob <= 11, "10 - 11 mm",
                                                                                                 ifelse(prob <= 12, "11 - 12 mm", "> 12 mm")))))))))))))


count_prob_papiol<-data.frame(table(prob_categ$prob_cat[prob_categ$zona=="Papiol"]))
prop_prob_papiol<-count_prob_papiol |>
  mutate(prop = (Freq/sum(count_prob_papiol$Freq))*100) |>
  mutate(zona = "Papiol")

count_prob_garraf<-data.frame(table(prob_categ$prob_cat[prob_categ$zona=="Garraf"]))
prop_prob_garraf<-count_prob_garraf |>
  mutate(prop = (Freq/sum(count_prob_garraf$Freq))*100) |>
  mutate(zona = "Garraf")

ordre_prob<-c("0 - 1 mm","1 - 2 mm","2 - 3 mm","3 - 4 mm", "4 - 5 mm", "5 - 6 mm",
              "6 - 7 mm", "7 - 8 mm", "8 - 9 mm", "9 - 10 mm", "10 - 11 mm",
              "11 - 12 mm", "> 12 mm")
prop_prob_total<-rbind(prop_prob_papiol, prop_prob_garraf)
prop_prob_total$zona<-factor(prop_prob_total$zona)
prop_prob_total$zona<-relevel(prop_prob_total$zona, "Papiol")
prop_prob_total <- transform(prop_prob_total, Var1 = factor(Var1, levels = ordre_prob))

#plot:
ggplot(prop_prob_total, aes(x=Var1, y = prop, fill = zona))+geom_bar(stat="identity", position = "dodge")+
  theme_classic() +
  xlab("Category")+ylab("Proportion (%)")+labs(fill = "Site")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))


#proboscis test:
test_prob<-matrix(c(10,6, 54, 29, 48, 26, 26, 16, 23, 13, 13, 6, 16, 13, 12, 10, 5, 2, 2, 2, 11, 7), ncol = 11)
table_test_prob<-as.table(test_prob)
prob_chi<-chisq.test(table_test_prob)
prob_chi$expected #to ensure <=20% of expected counts are <5


#-------------------------------------------------------------------------------------------------------------------------------#


#Proportion of each sociality category:
count_sociality_papiol<-data.frame(table(llistat_total$sociality[llistat_total$zona=="Papiol"]))
count_sociality_papiol<-count_sociality_papiol |>
  filter(Var1 != "unknown")
freq_sociality_papiol<-count_sociality_papiol |>
  mutate(prop = (Freq/sum(count_sociality_papiol$Freq))*100) |>
  mutate(zona = "Papiol")

count_sociality_garraf<-data.frame(table(llistat_total$sociality[llistat_total$zona=="Garraf"]))
count_sociality_garraf<-count_sociality_garraf |>
  filter(Var1 != "unknown")
freq_sociality_garraf<-count_sociality_garraf |>
  mutate(prop = (Freq/sum(count_sociality_garraf$Freq))*100) |>
  mutate(zona = "Garraf")

freq_sociality_total<-rbind(freq_sociality_garraf, freq_sociality_papiol)
freq_sociality_total$zona<-factor(freq_sociality_total$zona)
freq_sociality_total$zona<-relevel(freq_sociality_total$zona, "Papiol")
freq_sociality_total <- transform(freq_sociality_total, Var1 = factor(Var1, levels = c("Solitary", "Parasitic", "Social")))
#plot:
ggplot(freq_sociality_total, aes(x=Var1, y = prop, fill = zona))+geom_bar(stat="identity", position = "dodge")+
  theme_classic() +
  labs(fill = "Site") +  xlab("Sociality category")+ylab("Proportion (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#sociality test:
test_social<-matrix(c(160, 100, 19, 12, 42, 15), ncol = 3)
table_test_social<-as.table(test_social)
social_chi<-chisq.test(table_test_social)
social_chi$expected #to ensure <=20% of expected counts are <5


#-------------------------------------------------------------------------------------------------------------------------------#


#Proportion of each nesting category:
summary(llistat_total$nesting)
grafic_nesting<-llistat_total |>
  filter(!is.na(nesting)) |>
  filter(nesting != "CLE") |>
  filter(nesting != "unknown")
count_nest_papiol<-data.frame(table(grafic_nesting$nesting[grafic_nesting$zona=="Papiol"]))
freq_nest_papiol<-count_nest_papiol |>
  mutate(prop = (Freq/sum(count_nest_papiol$Freq))*100) |>
  mutate(zona = "Papiol") 
count_nest_garraf<-data.frame(table(grafic_nesting$nesting[grafic_nesting$zona=="Garraf"]))
freq_nest_garraf<-count_nest_garraf |>
  mutate(prop = (Freq/sum(count_nest_garraf$Freq))*100) |>
  mutate(zona = "Garraf")

freq_nest_total<-rbind(freq_nest_papiol, freq_nest_garraf)
freq_nest_total$zona<-factor(freq_nest_total$zona)
freq_nest_total$zona<-relevel(freq_nest_total$zona, "Papiol")
freq_nest_total <- transform(freq_nest_total, Var1 = factor(Var1, levels = c("S", "SC", "SPS", "O","SN", "LC","W")))
#plot:
ggplot(freq_nest_total, aes(x=Var1, y = prop, fill = zona))+geom_bar(stat="identity", position = "dodge")+
  theme_classic() +
  labs(fill = "Site") + xlab("Nesting category")+ylab("Proportion (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#nesting test:
test_nest<-matrix(c(108,65,46,27,10,5,5,6,13,11), ncol = 5)
table_test_nest<-as.table(test_nest)
nest_chi<-chisq.test(table_test_nest)
nest_chi$expected #to ensure <=20% of expected counts are <5


#-------------------------------------------------------------------------------------------------------------------------------#


#Proportion of each lecty category:
lecty<-llistat_total %>%
  filter(!is.na(lecty)) %>%
  filter(lecty!="NA", lecty!="?") %>%
  mutate(lecty_sum = 1)
proportion_lecty<-group_by(lecty, zona, lecty) %>% 
  summarise(count_lecty = sum(lecty_sum)) %>%
  mutate(prop_lecty = (count_lecty/sum(count_lecty))*100)

host_graph<-lecty %>%
  filter(!is.na(host_plant))
proportion_host<-group_by(host_graph, zona, host_plant) %>% 
  summarise(count_host = sum(lecty_sum)) %>%
  mutate(prop_host = (count_host/sum(count_host))*100)

proportion_lecty <- transform(proportion_lecty, lecty = factor(lecty, levels = c("Polylectic", "Oligolectic", "Monolectic")))
#test:
ggplot(proportion_lecty, aes(x=lecty, y = prop_lecty, fill = zona))+geom_bar(stat="identity", position = "dodge")+
  theme_classic() +
  labs(fill = "Site") + xlab("Lecty category")+ylab("Proportion (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#lecty test:
test_lecty<-matrix(c(122,78,51,27,6,5), ncol = 3)
table_test_lecty<-as.table(test_lecty)
lecty_chi<-chisq.test(table_test_lecty)
lecty_chi$expected #to ensure <=20% of expected counts are <5

#host plants:
proportion_host <- transform(proportion_host, host_plant = factor(host_plant, levels = c("Asteraceae", "Brassicaceae", "Boraginaceae", "Cistaceae", "Fabaceae", "Caprifoliaceae", "Resedaceae", "Convolvulaceae", "Malvaceae", "Ranunculaceae")))
#plot:
ggplot(proportion_host, aes(x=host_plant, y = prop_host, fill = zona))+geom_bar(stat="identity", position = position_dodge(preserve = "single"))+
  theme_classic() +
  labs(fill = "Site") + xlab("Floral host")+ylab("Proportion (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#host plants test:
test_host<-matrix(c(31, 16, 3, 9, 8, 2, 15, 5), ncol = 4) #Asteraceae, Fabaceae, Brassicaceae, Boraginaceae, Others pooled
table_test_host<-as.table(test_host)
hostplant_chi<-chisq.test(table_test_host)
hostplant_chi$expected #to ensure <=20% of expected counts are <5


###################################################################################################################################



#RANK-ABUNDANCE PLOT

pap_ranking<-pap_abund_resum %>%
  mutate(site = ifelse(gen_sp_def %in% sp_compartides_2$X.Amegilla.fasciata., "Papiol and Garraf", "Papiol only"))

gar_ranking<-gar_abund_resum %>%
  mutate(site = ifelse(gen_sp_def %in% sp_compartides_2$X.Amegilla.fasciata., "Papiol and Garraf", "Garraf only"))

total_ranking<-rbind(pap_ranking, gar_ranking) %>%
  mutate(log_abund = log(sum_mean), filling = site)

total_ranking$zona<-factor(total_ranking$zona)
total_ranking$site<-factor(total_ranking$site)
total_ranking$filling<-factor(total_ranking$filling)
total_ranking$zona<-relevel(total_ranking$zona, "Papiol")
total_ranking$filling<-relevel(total_ranking$filling, "Papiol only")

#plot:
ggplot(total_ranking, aes(x = rank, y = log_abund, col = filling)) +
  theme_bw()+
  geom_line(aes(colour = filling, group = zona)) + 
  geom_point(aes(),size = 2) + xlab("Rank") + ylab("ln (abundance)") + 
  scale_colour_manual(values=c("#F8766D", "#00BFC4", "grey")) +
  facet_wrap(~zona, ncol = 1,scales = 'free_y') + labs(col = "Category")+
  scale_x_continuous(breaks = c(0,50,100,150,200,250)) +
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=15))


###################################################################################################################################


#Spearman correlation, for all of the species
corr_abund_2<-merge(total_pap_comp, total_gar_comp, by.x=c("gen_sp_def"), by.y=c("gen_sp_def")) %>%
  mutate(abund_pap = sum_mean.x, abund_gar = sum_mean.y) %>%
  mutate(prop_abund_pap = prop_abund.x, prop_abund_gar = prop_abund.y) %>%
  select(gen_sp_def, abund_pap, abund_gar,prop_abund_pap,prop_abund_gar) %>%
  mutate(log_pap = log(abund_pap+1), log_gar = log(abund_gar+1),log_prop_pap = log(prop_abund_pap+1), log_prop_gar = log(prop_abund_gar+1))

shapiro.test(corr_abund_2$log_gar) #non-normal
shapiro.test(corr_abund_2$log_pap) #non-normal

cor.test(corr_abund_2$log_pap, corr_abund_2$log_gar, method = "spearman")

#excluding Bombus terrestris (most abundant species):
corr_abund_3<-corr_abund_2[c(corr_abund_2$gen_sp_def!="Bombus terrestris"),]

cor.test(corr_abund_3$log_pap, corr_abund_3$log_gar, method = "spearman")


###################################################################################################################################


#data preparation to compare abundances between shared and exclusive species:
sp_pap_anova<-pap_abund_resum |>
  filter(gen_sp_def %in% sp_exclusives_pap_2$X.Aglaoapis.tridentata.) %>%
  mutate(zona = "Papiol only")
anova_shared<-comp_only_shared %>%
  mutate(zona = "Papiol and Garraf") %>%
  select(gen_sp_def, sum_mean, zona)
anova_gar<-sp_exclusives_gar_info %>%
  mutate(zona = "Garraf only")%>%
  select(gen_sp_def, sum_mean, zona)
anova_pap<-sp_pap_anova %>%
  select(gen_sp_def, sum_mean, zona)
anova_total<-rbind(anova_shared, anova_gar, anova_pap) %>%
  mutate(log = log(sum_mean+1))

t_shared<-comp_only_shared %>%
  mutate(graph = ifelse(zona == "Papiol", "Papiol shared", "Garraf shared")) %>%
  select(gen_sp_def, sum_mean, zona, graph)
t_only_pap<-anova_pap %>%
  mutate(zona = "Papiol",graph = "Papiol only")
t_only_gar<-anova_gar %>%
  mutate(zona = "Garraf",graph = "Garraf only")
t_total<-rbind(t_shared, t_only_pap, t_only_gar) %>%
  mutate(log = log(sum_mean))
t_total$zona<-factor(t_total$zona)
t_total$graph<-factor(t_total$graph)
ggplot(t_total, aes(x = graph, y = log, fill = zona)) + geom_boxplot(alpha = 0.4) +
  xlab("Site")+ylab("ln (abundance)")+labs(fill="Site")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

shapiro.test(t_total$log) #non-normal

wilcox.test(log~graph, data = t_total[c(t_total$zona=="Papiol"),])
wilcox.test(log~graph, data = t_total[c(t_total$zona=="Garraf"),])

t_total_bomb<-t_total %>%
  filter(gen_sp_def != "Bombus terrestris") #without Bombus terrestris

wilcox.test(log~graph, data = t_total_bomb[c(t_total_bomb$zona=="Papiol"),])
wilcox.test(log~graph, data = t_total_bomb[c(t_total_bomb$zona=="Garraf"),])

#plot:
ggplot(t_total[c(t_total$zona=="Papiol"),], aes(x = graph, y = log, fill = zona)) + geom_boxplot(alpha = 0.4) + #change Papiol for Garraf in order to obtain the other plot
  xlab("Site")+ylab("ln (abundance)")+labs(fill="Site")+
  theme_classic()+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

#Garraf:
wilcox.test(log~graph, data = t_total_bomb[c(t_total_bomb$zona=="Garraf"),])
#plot:
ggplot(t_total[c(t_total$zona=="Garraf"),], aes(x = graph, y = log, fill = zona)) + geom_boxplot(alpha = 0.4) +
  xlab("Site")+ylab("ln (abundance)")+labs(fill="Site")+
  theme_classic()+
  scale_fill_manual(values = c("#00BFC4", "#00BFC4"))+
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))


###################################################################################################################################


#data preparation to plot phenology:
pap_abund_categ<-pap_abund %>%
  mutate(abund_categ = ifelse(mean == 1, "1",
                              ifelse(mean <5, "2",
                                     ifelse(mean<11, "3",
                                            ifelse(mean<51, "4",
                                                   ifelse(mean<101, "5", "6"))))))
pap_abund_categ$gen_def<-factor(pap_abund_categ$gen_def)

gar_abund_categ<-gar_abund %>%
  mutate(abund_categ = ifelse(mean == 1, "1",
                              ifelse(mean <5, "2",
                                     ifelse(mean<11, "3",
                                            ifelse(mean<51, "4",
                                                   ifelse(mean<101, "5", "6"))))))
gar_abund_categ$gen_def<-factor(gar_abund_categ$gen_def)

total_abund_categ<-rbind(pap_abund_categ, gar_abund_categ)
total_abund_categ_2<-total_abund_categ %>%
  group_by(num_setmana, gen_sp_def) %>% 
  summarise(abund_total = sum(mean))
total_abund_categ_2[c("gen", "sp")]<-str_split_fixed(total_abund_categ_2$gen_sp_def, " ", 2)
total_abund_categ_2<-total_abund_categ_2 |>
  mutate(num = 1)
total_abund_categ_3<-total_abund_categ_2 %>%
  group_by(num_setmana, gen) %>% 
  summarise(abund = sum(abund_total), num_sp = sum(num))
phenology_order<-c("Pasites", "Nomioides", "Lithurgus", "Aglaoapis", "Stelis", "Trachusa",
                   "Coelioxys", "Anthidiellum", "Amegilla", "Pseudoanthidium", "Thyreus",
                   "Tetralonia", "Nomiapis", "Heriades", "Ammobates", "Epeolus", "Dioxys",
                   "Panurgus", "Colletes", "Anthidium", "Panurginus", "Megachile", "Hoplitis",
                   "Seladonia", "Protosmia", "Hylaeus", "Halictus", "Osmia", "Chelostoma",
                   "Sphecodes", "Rhodanthidium", "Ceratina", "Melecta", "Xylocopa", 
                   "Lasioglossum", "Eucera", "Nomada", "Andrena", "Bombus", "Anthophora", "Apis")
total_abund_categ_3 <- transform(total_abund_categ_3, gen = factor(gen, levels = phenology_order))

andrenidae<-c("Panurgus","Panurginus","Andrena")
apidae<-c("Pasites","Amegilla","Thyreus", "Tetralonia","Ammobates", "Epeolus","Ceratina", "Melecta", "Xylocopa",
          "Eucera", "Nomada","Bombus","Anthophora", "Apis")
colletidae<-c("Colletes","Hylaeus")
halictidae<-c("Nomiapis","Seladonia","Halictus","Sphecodes","Lasioglossum", "Nomioides")
megachilidae<-c("Lithurgus", "Aglaoapis", "Stelis", "Trachusa", "Coelioxys", "Anthidiellum","Pseudoanthidium",
                "Heriades","Dioxys","Anthidium","Megachile", "Hoplitis","Protosmia","Osmia", "Chelostoma",
                "Rhodanthidium")

total_abund_categ_3<-total_abund_categ_3 %>%
  mutate(fam = ifelse(gen %in% andrenidae, "Andrenidae",
                      ifelse(gen %in% apidae, "Apidae",
                             ifelse(gen %in% colletidae, "Colletidae",
                                    ifelse(gen %in% halictidae, "Halictidae",
                                           ifelse(gen %in% megachilidae, "Megachilidae", "ERROR"))))))

ggplot(total_abund_categ_3, aes(x=num_setmana, y=gen, col = fam)) + 
  theme_classic()+scale_colour_manual(values = c("#fde725","#5ec962","#21918c","#3b528b","#440154"))+
  xlab("Week") + ylab("Genus") +
  geom_point(aes(size = num_sp))+ geom_line() + 
  labs(size = "Number of species", col = "Family") +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))



###################################################################################################################################


#correlation: first day of activity for shared species between the two communities:
pap_first_week<-group_by(pap_abund_categ, pap_abund_categ$gen_sp_def) %>%
  summarise(first_week_pap = min(num_setmana), duration_pap = max(num_setmana)-min(num_setmana)) %>%
  filter(`pap_abund_categ$gen_sp_def` %in% sp_compartides_2$X.Amegilla.fasciata.)

gar_first_week<-group_by(gar_abund_categ, gar_abund_categ$gen_sp_def) %>%
  summarise(first_week_gar = min(num_setmana), duration_gar = max(num_setmana)-min(num_setmana)) %>%
  filter(`gar_abund_categ$gen_sp_def` %in% sp_compartides_2$X.Amegilla.fasciata.) 

all_first_week<-cbind(pap_first_week, gar_first_week) %>%
  filter(duration_pap > 3, duration_gar > 3)

cor.test(all_first_week$first_week_pap, all_first_week$first_week_gar, method = "pearson")


###################################################################################################################################


#phenology differences: do species start their activity during the same week in the two communities?
pap_first_week_2<-data.frame(matrix(c(pap_first_week$`pap_abund_categ$gen_sp_def`, pap_first_week$first_week_pap), ncol = 2))
pap_first_week_3<-pap_first_week_2 %>%
  mutate(zona = "papiol", sp = 1:100)
gar_first_week_2<-data.frame(matrix(c(gar_first_week$`gar_abund_categ$gen_sp_def`, gar_first_week$first_week_gar), ncol = 2))
gar_first_week_3<-gar_first_week_2 %>%
  mutate(zona = "garraf", sp = 1:100)
all_first_week_2<-rbind(pap_first_week_3, gar_first_week_3)
ggplot(all_first_week_2, aes(x = zona, y = X2))+geom_boxplot()

t.test(X2~zona, data = all_first_week_2)


###################################################################################################################################


#MONTHLY PATTERNS:
#abundance, excluding Apis mellifera:
total_abund_categ[c("any", "mes", "dia")]<-str_split_fixed(total_abund_categ$fecha, "-", 3)
temporal_abund<-total_abund_categ %>%
  filter(gen_def != "Apis")
temporal_abund_2<-temporal_abund %>%
  group_by(lugar, mes) %>%
  summarise(abundance = sum(mean)) %>%
  mutate(site = ifelse(lugar == "GARRAF", "Garraf", "Papiol")) %>%
  select(-lugar)
temporal_abund_2$site<-relevel(factor(temporal_abund_2$site), "Papiol")
ggplot(temporal_abund_2, aes(x = mes, y = abundance, fill = site)) +
  geom_bar(stat = "identity", position = "dodge")+ labs(fill = "Site") +
  xlab("Month") + ylab("Abundance") + theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

test_abund<-matrix(c(129,202,162,241,126,361,273,649,459,624,289,645,302,486,341,661,191,564,52,269,66,164,286,245), ncol = 12)
table_test_abund<-as.table(test_abund)
chisq.test(table_test_abund, correct = F)

#richness:
temporal_richness<-total_abund_categ %>%
  mutate(rich = 1) %>%
  group_by(lugar, mes, gen_sp_def) %>% 
  summarise(richness = sum(rich)) %>%
  mutate(rich = 1) %>%
  group_by(lugar, mes) %>%
  summarise(richness = sum(rich)) %>%
  mutate(site = ifelse(lugar == "GARRAF", "Garraf", "Papiol"))%>%
  select(-lugar)
temporal_richness$site<-relevel(factor(temporal_richness$site), "Papiol")
ggplot(temporal_richness, aes(x = mes, y = richness, fill = site)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  xlab("Month") + ylab("Richness") + labs(fill = "Site") + theme_classic() +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

test_rich<-matrix(c(7,3,16,5,48,15,90,44,107,75,115,57,84,45,72,46,66,30,40,16,19,5,10,2), ncol = 12)
table_test_rich<-as.table(test_rich)
rich_chi<-chisq.test(table_test_rich, correct = F)
rich_chi$observed


###################################################################################################################################


#morisita horn index
morisita_pap<-corr_abund_2$abund_pap
morisita_gar<-corr_abund_2$abund_gar
1-horn_morisita(morisita_pap, morisita_gar)

