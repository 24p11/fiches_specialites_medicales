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
#path_data = "C:/data/pdh/fiches_specialites_medicales/"
path_pg <- getwd() %+% "/"
source(path_pg %+% "utils.R")
library(plotly)



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
                 dplyr::mutate(ghu_ref = dplyr::case_when(ghu=="APHP.Nord-Université de Paris"  ~ "GHU.Nord",
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

for(i in 1:5){
  
  ghu = df_ghu$ghu[i]
  ghu_ref = df_ghu$ghu_ref[i]
  
  rmarkdown::render(path_pg %+% "etude_indicateurs_activite_ghu.Rmd",
                    #output_file = "C:/data/wd/Etudes_specialite_medicales_v4.docx",
                    output_file = path_data %+% "Etudes_indicateurs_patients_specialites_chirurgicales_v2.html",
                    envir = .GlobalEnv)
  
  print(ghu %+% " -  done") 
}











