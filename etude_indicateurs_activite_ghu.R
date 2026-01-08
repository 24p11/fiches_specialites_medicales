#Variables
# - Niveau : cf
# - age2 : ge_18/lt_18
# - type_hosp : HC, HP, NA (tot) = une ligne pour chaque type d'hospitalisation
# - lib_spe_uma : spécialité médicale
# - tot : nombre de séjours vs nb pour le niveau indicateur
# - nb : nombre de séjours (quand pertient : NA lorsque l'on est au niveau régional)
# - p : pourcentage régional (quand pertient : NA lorsque l'on est au niveau régional)
# - indicateur : nom de l'indicateur patient si pertinent
#Critères :
# - Niveau régional
#   * niveau = Total régional = Nombre total de séjours dans la région par age2 : 
#   * niveau = Total Spéclialité régional : Nombre total de séjours dans la région par spécialité et age2
# - Niveau catégorie d'établissements
#   * niveau = Total categ  : Nombre de séjours par type d'établissement et age2
#   * niveau = Spéclialité - Categ : Nombre de séjours par type d'établissement et age2 --> une ligne par indicateur
#   * niveau = Indicateurs - Categ : Nombre de séjours par type d'établissement, specialité et age2  (une ligne par indicateur) --> une ligne par indicateur
# - Niveau GHU
#   * niveau = Total GHU : Nombre de séjours par GHU et age2
#   * niveau = Spéclialité - GHU : Nombre de séjours par GHU et age2  --> une ligne par indicateur
#   * niveau = Indicateurs - GHU : Nombre de séjours par GHU,specialité et age2  --> une ligne par indicateur

`%+%` <- function(x,y){paste0(x,y)}

path_data = "~/data/pdh/fiches_specialites_medicales/"
path_pg <- getwd() %+% "/"
source(path_pg %+% "utils.R")
library(plotly)

#Tableau cartographie globale activité GHU
# - Niveau
#    * Total Spéclialité régional : pour les données régionales
#.   * Spéclialité - Categ : pour les données APHP
#    * Spéclialité - GHU : pour les données GHU


df |> filter_df(list(age2 = age_ref,niveau=c("Spéclialité - GHU"), ghu=ghu_ref)) |> 
  dplyr::distinct(age2,lib_spe_uma,type_hosp,tot,annee) |> 
  dplyr::mutate(type_hosp = ifelse(is.na(type_hosp),"tot",type_hosp)) |> 
  dplyr::rename(nb= tot) |> 
  #Ajout du total APHP par spécialité
  dplyr::left_join( df |> filter_df(list(age2 = age_ref,niveau=c("Spéclialité - Categ"), categ="APHP")) |> 
                    dplyr::distinct(age2,lib_spe_uma,type_hosp,tot,annee) |> 
                    dplyr::mutate(type_hosp = ifelse(is.na(type_hosp),"tot",type_hosp)) |> 
                    dplyr::rename(tot_aphp= tot) ) |> 
  #Ajout du total Régional par spécialité
  dplyr::left_join(df |> filter_df(list(age2 = age_ref,niveau=c("Total Spéclialité régional"), categ=NA)) |> 
                     dplyr::mutate(type_hosp = ifelse(is.na(type_hosp),"tot",type_hosp)) |> 
                     dplyr::select(age2,lib_spe_uma,type_hosp,tot,annee)) |> 
  #Ajout du total GHU
  dplyr::left_join(df |> filter_df(list(age2 = age_ref,niveau=c("Total GHU"), ghu=ghu_ref)) |> 
                  dplyr::select(age2,type_hosp,annee,tot) |> 
                  dplyr::mutate(type_hosp = ifelse(is.na(type_hosp),"tot",type_hosp)) |> 
                  dplyr::rename(tot_ghu = tot) ) |>
  dplyr::filter(! (age2=="lt_18" & lib_spe_uma %in% specialite_ped_exclues )) |> 
  dplyr::mutate(p_spe_ghu = round(nb*100/tot_ghu,1),
                pm_reg = round(nb*100/tot,1),
                pm_aphp = round(nb*100/tot_aphp,1)) -> df_prep
#Total, prop GHU, pm APHP la dernière année
df_prep |> dplyr::filter(type_hosp=="tot",annee  == max(periode_annee)) |> 
  dplyr::select(age2,lib_spe_uma,p_spe_ghu,pm_aphp) -> df_res_p_spe_ghu

#PM régionale et évolution
df_prep |> dplyr::filter(type_hosp=="tot",annee %in% c(min(periode_annee),max(periode_annee)) ) |> 
    dplyr::select(age2,annee,lib_spe_uma,pm_reg) |> 
    tidyr::pivot_wider(names_from = annee,values_from = pm_reg) |> 
    dplyr::mutate(diff_pm_aphp = !!sym(max(periode_annee)) - !!sym(min(periode_annee))) |> 
    dplyr::select(- !!sym(min(periode_annee))) |> 
    dplyr::rename(pm_reg = !!sym(max(periode_annee)) ) -> df_res_pm_reg
#Nb HC+HP et évolution période
df_prep |> dplyr::filter(type_hosp=="tot",annee %in% c(min(periode_annee),max(periode_annee)) ) |> 
    dplyr::select(age2,annee,lib_spe_uma,nb) |> 
    tidyr::pivot_wider(names_from = annee,values_from = nb) |> 
    dplyr::mutate(diff_tot = !!sym(max(periode_annee)) - !!sym(min(periode_annee)),
                  p_diff_tot = round(diff_tot*100/!!sym(min(periode_annee)),1)) |> 
    dplyr::select(- !!sym(min(periode_annee))) |> 
    dplyr::rename(tot = !!sym(max(periode_annee)) ) |> 
    dplyr::filter(!is.na(tot))-> df_res_tot
  
#Nb HC et évolution période
df_prep |> dplyr::filter(type_hosp=="HC",annee %in% c(min(periode_annee),max(periode_annee)) ) |> 
  dplyr::select(age2,annee,lib_spe_uma,nb) |> 
  tidyr::pivot_wider(names_from = annee,values_from = nb) |> 
  dplyr::mutate(diff_hc = !!sym(max(periode_annee)) - !!sym(min(periode_annee)),
                p_diff_hc = round(diff*100/!!sym(min(periode_annee)),1)) -> df_res_hc



#Nb HC et évolution période
df_prep |> dplyr::filter(type_hosp=="HC",annee %in% c(min(periode_annee),max(periode_annee)) ) |> 
  dplyr::select(age2,annee,lib_spe_uma,nb) |> 
  tidyr::pivot_wider(names_from = annee,values_from = nb) |> 
  dplyr::mutate(diff_hc = !!sym(max(periode_annee)) - !!sym(min(periode_annee)),
                p_diff_hc = round(diff_hc*100/!!sym(min(periode_annee)),1)) |> 
  dplyr::select(- !!sym(min(periode_annee))) |> 
  dplyr::rename(hc = !!sym(max(periode_annee)) ) -> df_res_hc

#Nb HP et évolution période
df_prep |> dplyr::filter(type_hosp=="HP",annee %in% c(min(periode_annee),max(periode_annee)) ) |> 
  dplyr::select(age2,annee,lib_spe_uma,nb) |> 
  tidyr::pivot_wider(names_from = annee,values_from = nb) |> 
  dplyr::mutate(diff_hp = !!sym(max(periode_annee)) - !!sym(min(periode_annee)),
                p_diff_hp = round(diff_hp*100/!!sym(min(periode_annee)),1)) |> 
  dplyr::select(- !!sym(min(periode_annee))) |> 
  dplyr::rename(hp = !!sym(max(periode_annee)) ) -> df_res_hp

df_res_tot |> 
  dplyr::left_join(df_res_tx) |> 
  dplyr::left_join(df_res_hc) |> 
  dplyr::left_join(df_res_hp) -> df_res



date_extraction = "20260107"

periode_an = c("18","21","22","23","24")
periode_annee = "20"%+% c("18","21","22","23","24")

df<-NULL
for(an in periode_an){
  
  file ="indicateurs_ghu_"%+% an %+%"_"%+% date_extraction %+% ".csv"
  df <- df |> dplyr::bind_rows(readr::read_csv2(path_data %+% file))
  
}

specialite_exclues = c("DIALYSE ADULTE","CANCERO ADULTE","DIALYSE PEDIATRIQUE","RADIOTHERAPIE","I.V.G.")
specialite_ped_exclues <- c("GYNECOLOGIE")
df<- df |> dplyr::filter(! lib_spe_uma %in% specialite_exclues)

tab_hop_aphp<-readr::read_delim(path_data %+% "tab_hop_aphp.tsv",delim = "\t")
names(tab_hop_aphp)<-tolower(names(tab_hop_aphp))
tab_hop_aphp |> 
                 dplyr::mutate(ghu_ref = dplyr::case_when(ghu =="APHP.Nord-Université de Paris"  ~ "GHU.Nord",
                                                          ghu=="APHP.Université Paris Saclay"~ "GHU.Saclay",
                                                          ghu=="APHP.Sorbonne Université"~ "GHU.SUN",
                                                          ghu=="APHP.Hôpitaux Universitaires Henri-Mondor"~ "GHU.HMN",
                                                          ghu=="APHP.Hôpitaux Universitaires Paris-Seine-Saint-Denis"~ "GHU.PSSD",
                                                          ghu=="APHP.Centre-Université de Paris"~ "GHU.Centre",
                                                      TRUE~ "Autre hopitaux" )) -> tab_hop_aphp

df_ghu<-tab_hop_aphp |> dplyr::filter(ghu_ref!="Autre hopitaux") |> dplyr::distinct(ghu,ghu_ref)            

df_detail_spe_ga_aphp <- readxl::read_excel(path_pg %+%"detail_spe_ga_aphp.xlsx", sheet = "Feuille1")

df_detail_spe_ga_aphp <- df_detail_spe_ga_aphp |> dplyr::mutate(tot=sum(nb),.by=lib_spe_uma) |> 
  dplyr::mutate(p = round(nb*100/tot),
                p_print = p %+% "%")

for(i in 1:nrow(df_ghu)){
  
  ghu = df_ghu$ghu[i]
  ghu_ref = df_ghu$ghu_ref[i]
  
  
  
  rmarkdown::render(path_pg %+% "etude_indicateurs_activite_ghu.Rmd",
                    output_file = path_data %+% "Etudes_indicateurs_patients_"%+% ghu %+% "_v1.html",
                    envir = .GlobalEnv)
 
  print(ghu %+% " -  done") 
}



df<-NULL
for(an in c("18","21","22","23","24")){
  
  file ="indicateurs_ghu_tot_"%+% an %+%"_"%+% date_extraction %+% ".csv"
  df <- df |> dplyr::bind_rows(readr::read_csv2(path_data %+% file))
  
}

for(i in 1:nrow(df_ghu)){
  
  ghu = df_ghu$ghu[i]
  ghu_ref = df_ghu$ghu_ref[i]
  
  rmarkdown::render("pgm-r/etude_indicateurs_activite_chirurgicale.Rmd",
                    #output_file = "C:/data/wd/Etudes_specialite_medicales_v4.docx",
                    output_file = path_data %+% "Etudes_indicateurs_patients_specialites_chirurgicales_v2.html",
                    envir = .GlobalEnv)
  
  print(ghu %+% " -  done") 
}











