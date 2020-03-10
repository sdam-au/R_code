
####  EPIGRAPHIC DATABASE HEIDELBERG: INSCRIPTIONS
####
####  NETWORK TIES: MEASURES OF SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY


##
library("devtools")
#source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/get.edh.r")
source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/simil.r")
###


#### load the data
load(file="epalln.Rdata")
####



# CREATE EPIGRAPHIC NETWORK DATA LIST WITH VARIABLES MEASURES OF 
# SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY (E.G.

epinetl <- lapply(epalln, function (x) x[c("ID", "type_of_monument", "language", "material", 
                  "country", "findspot_ancient", "not_after", "not_before")] )



# EXECUTE A FUNCTION CALL ON THE LIST TO CONVERT LIST OF DATA INTO A DATA FRAME

epinet <- as.data.frame(do.call(rbind, epinetl))



# TAKE A LOOK AT IT
head(epinet, 8)
#      ID type_of_monument language                                material country findspot_ancient not_after not_before
#1 000001           tabula    Latin                Marmor, geädert / farbig   Italy       Cumae, bei      0130       0071
#2 000002           tabula    Latin       marble: rocks - metamorphic rocks   Italy             Roma      0200       0051
#3 000003      statue base    Latin       marble: rocks - metamorphic rocks   Spain             NULL      0170       0131
#4 000004            altar    Latin    limestone: rocks - clastic sediments   Spain    Ipolcobulcula      0200       0151
#5 000005            stele    Latin                                    NULL   Italy             Roma      0200       0001
#6 000006            stele    Latin    limestone: rocks - clastic sediments   Spain      Sabora, bei      0150       0071
#7 000007           tabula    Latin travertine: rocks - chemische Sedimente   Italy             Roma     -0051      -0100
#8 000008           tabula    Latin       marble: rocks - metamorphic rocks   Italy            Roma?      0200       0101



# REMOVE QUESTION MARKS
epinet2 <- as.data.frame(do.call(rbind, lapply(epinetl, function (x)  as.list(gsub('\\?', '', x)) )) )
colnames(epinet2) <- names(epinetl[[1]])



# TAKE A LOOK AT IT again
head(epinet2, 8)
#      ID type_of_monument language                                material country findspot_ancient not_after not_before
#1 000001           tabula    Latin                Marmor, geädert / farbig   Italy       Cumae, bei      0130       0071
#2 000002           tabula    Latin       marble: rocks - metamorphic rocks   Italy             Roma      0200       0051
#3 000003      statue base    Latin       marble: rocks - metamorphic rocks   Spain             NULL      0170       0131
#4 000004            altar    Latin    limestone: rocks - clastic sediments   Spain    Ipolcobulcula      0200       0151
#5 000005            stele    Latin                                    NULL   Italy             Roma      0200       0001
#6 000006            stele    Latin    limestone: rocks - clastic sediments   Spain      Sabora, bei      0150       0071
#7 000007           tabula    Latin travertine: rocks - chemische Sedimente   Italy             Roma     -0051      -0100
#8 000008           tabula    Latin       marble: rocks - metamorphic rocks   Italy             Roma      0200       0101




## COUNTRIES

unique(unlist(epinet2$country))
# [1] "Italy"                  "Spain"                  "United Kingdom"         "Portugal"              
# [5] "France"                 "Libyan Arab Jamahiriya" "Germany"                "Hungary"               
# [9] "Austria"                "Bulgaria"               "Bosnia and Herzegovina" "Montenegro"            
#[13] "Netherlands"            "Tunisia"                "Romania"                "Algeria"               
#[17] "Jordan"                 "NULL"                   "Croatia"                "Switzerland"           
#[21] "Belgium"                "Albania"                "Serbia"                 "Egypt"                 
#[25] "Syrian Arab Republic"   "Morocco"                "Turkey"                 "Lebanon"               
#[29] "Kosovo"                 "Macedonia"              "Slovakia"               "Greece"                
#[33] "Slovenia"               "Iraq"                   "Israel"                 "unknown"               
#[37] "Vatican City State"     "Ukraine"                "Cyprus"                 "Yemen"                 
#[41] "Sudan"                  "Luxembourg"             "Czech Republic"         "Malta"                 
#[45] "Poland"                 "Armenia"                "Monaco"                 "Azerbaijan"            
#[49] "Sweden"                 "Denmark"                "Moldova"                "Saudi Arabia"          
#[53] "Uzbekistan"             "Liechtenstein"          "Georgia"




## e.g. EPIGRAPHIC MATERIAL IN "GREEK-LATIN" FROM EGYPT

subset(subset(epinet2, country=="Egypt"), language=="Greek-Latin")
#          ID     type_of_monument    language    material country findspot_ancient not_after not_before
#2003  002003 architectural member Greek-Latin        NULL   Egypt           Philae      NULL      -0116
#23091 023091              diptych Greek-Latin Holz, Wachs   Egypt             NULL      NULL       0145
#23138 023138                 NULL Greek-Latin        NULL   Egypt            Syene      NULL      -0029
#29256 023091              diptych Greek-Latin Holz, Wachs   Egypt             NULL      NULL       0145
#29302 023138                 NULL Greek-Latin        NULL   Egypt            Syene      NULL      -0029
#36306 030147                 NULL Greek-Latin        NULL   Egypt       Alexandria      0011       0010
#38242 032079                 NULL Greek-Latin        NULL   Egypt          Schedia      NULL       NULL
#55004 048625                cliff Greek-Latin        NULL   Egypt         Berenice      NULL       0006
#58000 051485                stele Greek-Latin        NULL   Egypt       Alexandria      0225       0155
#62124 055974                stele Greek-Latin        NULL   Egypt         Berenice      0200       0001
#73916 067781                 NULL Greek-Latin        NULL   Egypt      Leontopolis     -0030      -0037




## COMPUTE SIMILARITY AMONG EGYPTIAN EPIGRAPHS WITH FUNCTION simil()
## CASE: 'type_of_monument', 'material', AND 'findspot_ancient'

epEgs <- simil(subset(epinet2, country=="Egypt"), c(2,4,6))


## HOW MANY INSCRIPTIONS?

dim(epEgs)
#[1] 170 170




## PLOT GRAPH OF THESE SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES

library("multigraph")

scp <- list(directed=FALSE, valued=TRUE, ecol=8, pos=0, cex=2.5, vcol="")

multigraph(rm.isol(epEgs), layout="force", maxiter=70, scope=scp, main="Similarity of Egyptian epigraphs")

pdf("C:/R/AUArts/Epigraphic/epEgs-RANDOM321.pdf")
multigraph(rm.isol(epEgs), layout="force", seed=321, maxiter=3, scope=scp, main="Similarity of Egyptian epigraphs")
dev.off()
