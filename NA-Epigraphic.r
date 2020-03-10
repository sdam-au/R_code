
####  EPIGRAPHIC DATABASE HEIDELBERG: INSCRIPTIONS
####
####  NETWORK TIES: MEASURES OF SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY


##
library("devtools")
#source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/get.edh.r")
# magic code that detects similarity in column variables among observations
source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/simil.r")
###


#### load the data
load(file="data/epalln.Rdata")
####

ls()
head(epalln)
# CREATE EPIGRAPHIC NETWORK DATA LIST WITH VARIABLES MEASURES OF 
# SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY (E.G.

# with this function you select those artefact attributes you are interested in 
#comparing and create a dataframe \netl\ from them 
epinetl <- lapply(epalln, function (x) x[c("ID", "type_of_monument", "language", "material", 
                  "country", "findspot_ancient", "type_of_inscription", "not_after", "not_before")] )



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

str(epinet$type_of_inscription)

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

# INSCRIPTION TYPES
unique(unlist(epinet2$type_of_inscription))

# FREQUENCY
sort(table(unlist(epinet2$type_of_inscription)))

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
## CASES: 'type_of_monument', 'material', AND 'findspot_ancient'

epEgs <- simil(subset(epinet2, country=="Egypt"), c(2,4,6))
epEgs267 <- simil(subset(epinet2, country=="Egypt"), c(2,6,7))
epEgs2 <- simil(subset(epinet2, country=="Egypt"), c(2))
epEgs6 <- simil(subset(epinet2, country=="Egypt"), c(6))
epEgs7 <- simil(subset(epinet2, country=="Egypt"), c(7))

## HOW MANY INSCRIPTIONS?

dim(epEgs)
#[1] 170 170

dim(epEgs267)


<<<<<<< HEAD
## CALCULATE FREQUENCY OF DIFFERENT KINDS OF INSCRIPTIONS
data.frame(sort(table(unlist(epinet2$type_of_inscription)), 
                  decreasing = TRUE))

## PLOT GRAPH OF THESE SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES
=======
## PLOT GRAPH OF EGYPTIAN EPIGRAPHS SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES
>>>>>>> 2f3bf58f633bc8bae485814b33e1bcec9ae0f4dd

install.packages("multigraph")
library("multigraph")

<<<<<<< HEAD
scp <- list(directed=FALSE, valued=TRUE, ecol=8, pos=0, cex=2.5, vcol="red")

multigraph(rm.isol(epEgs7), layout="force", maxiter=14, scope=scp,
           main="Similarity of Egyptian epigraphs by inscription type", values = TRUE)
=======
scp <- list(directed=FALSE, valued=TRUE, ecol=8, pos=0, cex=2.5, vcol="yellow")

multigraph(rm.isol(epEgs), layout="force", maxiter=5, seed=1, scope=scp, main="Graph similarity of Egyptian epigraphs")



## LOOK SIMILARITY OF EGYPTIAN EPIGRAPHS AT EACH CASE SEPARATELY

# type_of_monument
epEgs2 <- simil(subset(epinet2, country=="Egypt"), 2)

# material
epEgs4 <- simil(subset(epinet2, country=="Egypt"), 4)

#findspot_ancient
epEgs6 <- simil(subset(epinet2, country=="Egypt"), 6)



## BIND EGYPTIAN EPIGRAPHS SIMILARITIES INTO A 3D ARRAY (and remove isolates)

epEGs <- rm.isol(zbind(epEgs2, epEgs4, epEgs6))


## PLOT MULTIGRAPH OF EGYPTIAN EPIGRAPHS SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES

multigraph(epEGs, layout="force", maxiter=5, seed=1, scope=scp, main="Multigraph similarities of Egyptian epigraphs")




## LOOK AT THE DIFFERENT COMPONENTS IN EGYPTIAN EPIGRAPHS AS A MULTIPLEX NETWORK

epEGscmp <- comps(epEGs)
#$com
#$com[[1]]
# [1] "000741" "012932" "045448" "046449" "047182" "047235" "047236" "048625" "048626" "048627" "050446" "050447" "012935" "051485" "052269"
#[16] "053039" "054883" "055974" "061438" "061439" "078079" "012938" "017883" "017922" "018266" "019751" "002003" "019754" "020881" "021680"
#[31] "021792" "023091" "003126" "023280" "023372" "025080" "025435" "025438" "006782" "025441" "025444" "025447" "025450" "025477" "026572"
#[46] "026575" "026578" "026581" "028809" "006785" "029676" "030147" "030150" "030411" "032199" "036549" "037146" "037166" "037167" "037168"
#[61] "008142" "037177" "037178" "037179" "037180" "037181" "037182" "037183" "037185" "037186" "037187" "037194" "037195" "037196" "037197"
#[76] "037198" "037199" "037200" "039393" "044135" "044137" "044138" "044139" "044140" "044141" "012929" "044142" "044154" "044155" "044156"
#[91] "044157" "044158" "044159" "045225" "045226" "045227"
#
#$com[[2]]
#[1] "018468" "018471" "011887"
#
#$com[[3]]
#[1] "019274" "023094"
#
#$com[[4]]
#[1] "019928" "019931" "023126" "023129" "023132" "023135" "023138" "011661"
#
#$com[[5]]
#[1] "021705" "021708"
#
#$com[[6]]
#[1] "037188" "037189" "037190" "037191" "037192" "037193"
#
#
#$isol
#character(0)





## SELECT THE FIRST AND FOURTH COMPONENT OF EGYPTIAN EPIGRAPHS

epEGscmp1 <- rel.sys(epEGs, type="toarray", sel=epEGscmp$com[[1]])

epEGscmp4 <- rel.sys(epEGs, type="toarray", sel=epEGscmp$com[[4]])
#, , 1
#
#       011661 019928 019931 023126 023129 023132 023135 023138
#011661      0      0      0      0      0      0      0      0
#019928      0      0      0      0      0      0      0      0
#019931      0      0      0      0      0      0      0      0
#023126      0      0      0      0      0      0      0      0
#023129      0      0      0      0      0      0      0      0
#023132      0      0      0      0      0      0      0      0
#023135      0      0      0      0      0      0      0      0
#023138      0      0      0      0      0      0      0      0
#
#, , 2
#
#       011661 019928 019931 023126 023129 023132 023135 023138
#011661      0      0      0      0      0      0      0      0
#019928      0      0      0      0      0      0      0      0
#019931      0      0      0      0      0      0      0      0
#023126      0      0      0      0      0      0      0      0
#023129      0      0      0      0      0      0      0      0
#023132      0      0      0      0      0      0      0      0
#023135      0      0      0      0      0      0      0      0
#023138      0      0      0      0      0      0      0      0
#
#, , 3
#
#       011661 019928 019931 023126 023129 023132 023135 023138
#011661      0      1      1      1      1      1      1      1
#019928      1      0      1      1      1      1      1      1
#019931      1      1      0      1      1      1      1      1
#023126      1      1      1      0      1      1      1      1
#023129      1      1      1      1      0      1      1      1
#023132      1      1      1      1      1      0      1      1
#023135      1      1      1      1      1      1      0      1
#023138      1      1      1      1      1      1      1      0




## NOW PLOT THESS SINGLE COMPONENTS OF EGYPTIAN EPIGRAPHS

multigraph(epEGscmp1, layout="force", maxiter=50, seed=1, scope=scp, main="Multigraph similarities of Egyptian epigraphs 1st component")

multigraph(epEGscmp4, layout="force", maxiter=50, seed=1, scope=scp, main="Multigraph similarities of Egyptian epigraphs 4th component")

>>>>>>> 2f3bf58f633bc8bae485814b33e1bcec9ae0f4dd

# Can we color the nodes according to inscription type?