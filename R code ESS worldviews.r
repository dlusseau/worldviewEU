#####ESS
##########
setwd(here::here())

#ESS questions:

values<-c(
"pplfair",
"pplhlp",
"ppltrst",
"cptppol",
"cptppola",
"dclenv",
"ecohenv",
"gincdif",
"ginveco",
"lawobey",
"polcmpl",
"psppipl",
"psppsgv",
"psppsgva",
"scnsenv",
"trstun",
"trstsci",
"lrnobed",
"impenv",
"impenva",
"impfree",
"impfreea",
"imptrad",
"imptrada",
"ipbhprp",
"ipfrule",
"ipfrulea",
"iphlppl",
"iphlppla",
"ipstrgv",
"ipstrgva",
"ccrdprs",
"ccgdbd",
"elgnuc",
"eneffap",
"gvsrdcc",
"inctxff",
"ownrdcc",
"sbsrnen",
"likrisk",
"actcomp")


#ESS<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv",header=T)

ESS7<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS7e02_3.csv",header=T)
ESS8<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS8e02_3.csv",header=T)
ESS9<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS9e03_3.csv",header=T)
ESS10<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS10e03_3.csv",header=T)
ESS11<-read.csv("C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/ESS11e04_1.csv",header=T)

#these are the right editions
#let's keep them separate to lighten the memory load

sum(values%in%colnames(ESS11))
sum(values%in%colnames(ESS10))
sum(values%in%colnames(ESS9))
sum(values%in%colnames(ESS8))
sum(values%in%colnames(ESS7))


##tedious but safe

select<-colnames(ESS11)%in%values
select[1:13]<-TRUE
ESS11_worldviews<-ESS11[,select]

select<-colnames(ESS10)%in%values
select[1:13]<-TRUE
ESS10_worldviews<-ESS10[,select]

select<-colnames(ESS9)%in%values
select[1:13]<-TRUE
ESS9_worldviews<-ESS9[,select]

select<-colnames(ESS8)%in%values
select[1:13]<-TRUE
ESS8_worldviews<-ESS8[,select]

select<-colnames(ESS7)%in%values
select[1:13]<-TRUE
ESS7_worldviews<-ESS7[,select]


common_questions <- Reduce(intersect, 
                           list(values,
                                colnames(ESS7_worldviews),
                                colnames(ESS8_worldviews),
                                colnames(ESS9_worldviews),
                                colnames(ESS10_worldviews),
                                colnames(ESS11_worldviews)))

common_questions <- Reduce(intersect, 
                           list(values,
                                colnames(ESS10_worldviews),
                                colnames(ESS11_worldviews)))

ESS_worldviews<-list(ESS11_worldviews,ESS10_worldviews,ESS9_worldviews,ESS8_worldviews,ESS7_worldviews)

"C:/Users/davlu/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/data/ESS/"
saveRDS(ESS_worldviews,file="ESS_worldviews_finaleditions.rds")

#all ordinal?
#yes but values >10 or >5  be turned to NA!! for now - after all refusal to answer is a clue to worldview for some questions but we can't handle it in an ordinal manner....

values[values%in%colnames(ESS11_worldviews)]

###let's circle back here - is it poLCA issue?
## yes categorical convenience add 1. all values have to be positive integer 
#positive integers. For poLCA to run, please recode categorical
#outcome variables to increment from 1 to the maximum number of
#outcome categories for each variable. 

for (i in 1:length(ESS_worldviews)) {
thezeros<-names(which(apply(ESS_worldviews[[i]][values[values%in%colnames(ESS_worldviews[[i]])]],2,function(x) min(x,na.rm=T))==0))
ESS_worldviews[[i]][thezeros]<-ESS_worldviews[[i]][thezeros]+1
}


#only 49 individuals have all NAs, we keep them in
# we remove them
V11<-values[values%in%colnames(ESS11_worldviews)]
whosna<-apply(ESS11_worldviews[V11],1,function (x) sum(is.na(x)))
allnas<-which(whosna==length(V11))
length(allnas)
formulae11<-as.formula(paste("cbind(", paste(V11, collapse = ", "), ") ~ 1")) #  column names

V10<-values[values%in%colnames(ESS10_worldviews)]
whosna<-apply(ESS10_worldviews[V10],1,function (x) sum(is.na(x)))
sum(whosna)
allnas<-which(whosna==length(V10))
length(allnas)
formulae10<-as.formula(paste("cbind(", paste(V10, collapse = ", "), ") ~ 1")) #  column names

V9<-values[values%in%colnames(ESS9_worldviews)]
whosna<-apply(ESS9_worldviews[V9],1,function (x) sum(is.na(x)))
sum(whosna)
allnas<-which(whosna==length(V9))
length(allnas)
formulae9<-as.formula(paste("cbind(", paste(V9, collapse = ", "), ") ~ 1")) #  column names

V8<-values[values%in%colnames(ESS8_worldviews)]
whosna<-apply(ESS8_worldviews[V8],1,function (x) sum(is.na(x)))
sum(whosna)
allnas<-which(whosna==length(V8))
length(allnas)
formulae8<-as.formula(paste("cbind(", paste(V8, collapse = ", "), ") ~ 1")) #  column names

V7<-values[values%in%colnames(ESS7_worldviews)]
whosna<-apply(ESS7_worldviews[V7],1,function (x) sum(is.na(x)))
sum(whosna)
allnas<-which(whosna==length(V7))
length(allnas)
formulae7<-as.formula(paste("cbind(", paste(V7, collapse = ", "), ") ~ 1")) #  column names


formulaes<-list(formulae11,formulae10,formulae9,formulae8,formulae7)

saveRDS(formulaes,"formulaes.rds")

#install.packages("poLCA")
library(poLCA)
library(poLCAParallel)
library(parallel)

rm(V7,V8,V9,V10,V11,whosna,nass,ESS7,ESS8,ESS9,ESS10,ESS11)

gc()

n_threads <- max(1L, parallel::detectCores() - 1L)

# try<-poLCA(formulae, data = ESS_worldviews[values], nclass = 2, maxiter = 1000, 
                           # nrep = 5, na.rm = FALSE)


#one analysis per edition

alleditions<-c("ESS11","ESS10","ESS9","ESS8","ESS7")

lca_editions<-vector("list",length=length(ESS_worldviews))

names(lca_editions)<-c("ESS11","ESS10","ESS9","ESS8","ESS7")

for (j in 1:length(ESS_worldviews)) {


max_clusters <- sum(values%in%colnames(ESS_worldviews[[j]]))
lca_models <- list()
bic_values <- numeric(max_clusters)
chisq <- numeric(max_clusters)

 for (k in 1:max_clusters) {
   
   lca_models[[k]] <- poLCAParallel::poLCA(
     formula    = formulaes[[j]],
     data       = ESS_worldviews[[j]],
     nclass     = k,
     maxiter    = 1000,
     nrep       = 5,             # or 10 if you can afford it
     n.thread   = n_threads,
     na.rm      = FALSE,
     calc.se    = FALSE,
     calc.chisq = FALSE,         # skip expensive goodness-of-fit in search stage
     tol        = 1e-6,
     verbose    = FALSE,
     graphs     = FALSE
   )
   # na.rm = FALSE for FIML lighten all this for cluster number definition
	# will refit with increased tolerance, se calculated and nrep >3
   bic_values[k] <- lca_models[[k]]$bic
   chisq[k]<-lca_models[[k]]$Chisq
   print(k)
   flush.console()
}

lca_editions[[j]]<-list(lca_models,bic_values,chisq)
print(j)
flush.console()
}


save(lca_editions,file="lca_per_round_per_cluster.rds")

## model selection is not working as usual,let's turn to modularity coefficient


###let's get the mod calculator

# clustermod<-function(AI,clusass){
# #estimate modularity of each topological modules in a network
# #clusass is a vector of size n, with cluster membership for each node (1 to k clusters)
# #AI is a symmetric matrix n x n
# n<-dim(AI)[1]
# k<-max(clusass)
# #calculates modularity matrix
# strength<-rowSums(AI)
# rAI<-matrix(0,n,n)
# rAI<-(strength%*%t(strength))/(sum(strength))
# Qq<-AI-rAI  #modularity matrix
# diag(Qq)<-0

# Q<-sum(Qq)/sum(strength)
# #declare mod
# mod<-array(0,k)

# for (i in 1:k) {
	# mod[i]<-sum(Qq[which(clusass==i),which(clusass==i)])/sum(strength[which(clusass==i)])
# }
	# return(list(mod=mod,Q=Q))
# }



#no we are going for a measure based on the probability to belong to a cluster

#https://arxiv.org/pdf/1411.4257 has an interesting idea to integrate it to BIC to combine cluster number selection and the separation measure it offers


cluster.select.df<-data.frame(editions=as.character(rep(alleditions,valuesfreq)),
								clusters=unlist(apply(as.data.frame(valuesfreq),1,function (x) seq(1,x,1))),
								bic=NA,entropy=NA,entropy.std=NA,max.prob=NA,icl=NA)
i=10

for (i in 1:nrow(cluster.select.df)) {
cluster.select.df$bic[i]<-lca_editions[[cluster.select.df$editions[i]]][[2]][cluster.select.df$clusters[i]]

mattemp<-lca_editions[[cluster.select.df$editions[i]]][[1]][[cluster.select.df$clusters[i]]]["posterior"]$posterior
lmattemp<-log(mattemp)
lmattemp[lmattemp==-Inf]<-0
cluster.select.df$entropy[i]<-sum(mattemp*lmattemp)
cluster.select.df$entropy.std[i]<-1-(-sum(mattemp*lmattemp)/(dim(mattemp)[1]*log(dim(mattemp)[2]))) #maximise: 1-H
#I am going for a measure of separation

cluster.select.df$max.prob[i]<- mean(apply(mattemp,1,max))
}


cluster.select.df$icl<-cluster.select.df$bic-cluster.select.df$entropy

library(ggplot2)

ggplot(cluster.select.df,aes(x=clusters,y=entropy.std,colour=editions))+
geom_line()+
theme_minimal()+ylim(.7,.8)


best_clus<-data.frame(editions=as.character(alleditions),clus=c(3,3,3,3,3,3,4,4,3,3))



lca_editions_best<-vector("list",length=length(alleditions))
names(lca_editions_best)<-alleditions

for (j in 1:length(alleditions)) {

valueinedition<-question_in_edition$values[question_in_edition$edition==alleditions[j]&question_in_edition$present==TRUE]
ESS<-subset(ESS_worldviews,edition==alleditions[j])

formulae<-as.formula(paste("cbind(", paste(valueinedition, collapse = ", "), ") ~ 1")) # Replace with your column names

k <- best_clus$clus[j]



#lca_models[[best_k]] <- poLCAParallel::poLCAParallel.goodnessfit(lca_models[[best_k]])
#best_chisq <- lca_models[[best_k]]$Chisq


lca_editions_best[[j]] <- poLCAParallel::poLCA(formulae, data = ESS[valueinedition], nclass = k, maxiter = 1000, 
                            nrep = 3, na.rm = FALSE,calc.se=TRUE,tol = 1e-10)  # na.rm = FALSE for FIML lighten all this for cluster number definition
   print(j)
   flush.console()
}


save(lca_editions_best,question_in_edition,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/best_models_each_edition.Rdata")
##we need to use ln not log_2 for entropy to have on the same scale as BIC


modprobs<-lca_editions_best[[1]]["probs"]
v1<-modprobs[[1]][[2]]

library(reshape2)

v1m<-melt(v1)

ggplot(v1m,aes(x=Var2,y=value,fill=Var1))+
geom_bar(stat="identity",position="dodge")+
coord_polar()+
theme_minimal()

#weighted average (by probability) response - median response expected
apply(v1,1,cumsum)

model_interpret<-question_in_edition[question_in_edition$present==TRUE,]
model_interpret$class1<-NA
model_interpret$class2<-NA
model_interpret$class3<-NA
model_interpret$class4<-NA
model_interpret$scale<-NA

model_interpret$edition<-as.character(model_interpret$edition)
editions<-unique(model_interpret$edition)


for (i in 1:nrow(model_interpret)) {
ed<-model_interpret$edition[i]
val<-model_interpret$values[i]

modprobs<-lca_editions_best[[ed]]["probs"]
v1<-modprobs[[1]][val]
model_interpret$scale[i]<-ncol(v1[[val]])

valcum<-apply(v1[[val]],1,cumsum)


if (nrow(v1[[val]])==4) {
model_interpret[i,c("class1","class2","class3","class4")]<-as.numeric(apply(valcum,2,function(x) which.min(abs(x-0.5))))

} else {

model_interpret[i,c("class1","class2","class3")]<-as.numeric(apply(valcum,2,function(x) which.min(abs(x-0.5))))

}
}

library(tidyverse)
model_interpret<-model_interpret%>%
					arrange(desc(edition),desc(values)) 


library(ggpubr)


#rose plots

for (i in 1:length(editions)) {

edition1.class1<-ggplot(subset(model_interpret,edition==editions[i]),aes(x=values,y=class1/scale))+
geom_bar(stat="identity",fill="dark green")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class2<-ggplot(subset(model_interpret,edition==editions[i]),aes(x=values,y=class2/scale))+
geom_bar(stat="identity",fill="dark red")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class3<-ggplot(subset(model_interpret,edition==editions[i]),aes(x=values,y=class3/scale))+
geom_bar(stat="identity",fill="dark blue")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class4<-ggplot(subset(model_interpret,edition==editions[i]),aes(x=values,y=class4/scale))+
geom_bar(stat="identity",fill="dark orange")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))


if (is.na(model_interpret$class4[model_interpret$edition==editions[i]][1])) {
graph1<-ggarrange(edition1.class1,edition1.class2,edition1.class3,ncol=4,nrow=1)
fig1<-annotate_figure(graph1,
						fig.lab = paste0("ESS edition ",editions[i]),
						fig.lab.pos = "top.left",
						fig.lab.size=12,
						fig.lab.face="bold")
  
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/rose_plot_clusters_interpretations_edition_",editions[i],".png"),units="cm",res=200,width=56,height=14)
print(fig1)
dev.off()

} else {

graph1<-ggarrange(edition1.class1,edition1.class2,edition1.class3,edition1.class4,ncol=4,nrow=1)
fig1<-annotate_figure(graph1,
						fig.lab = paste0("ESS edition ",editions[i]),
						fig.lab.pos = "top.left",
						fig.lab.size=12,
						fig.lab.face="bold")
						
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/rose_plot_clusters_interpretations_edition_",editions[i],".png"),units="cm",res=200,width=56,height=14)
print(fig1)
dev.off()

}


}


#+
#scale_fill_brewer(palette="Greens")

ESS_worldviews$cluster<-NA

for (i in 1:length(editions)) {

ESS_worldviews$cluster[ESS_worldviews$edition==editions[i]] <-  lca_editions_best[[editions[i]]]["predclass"][[1]]


}

write.csv(ESS_worldviews,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS_worldviews_subset_classfied.csv")

table(ESS_worldviews$cntry,ESS_worldviews$cluster,ESS_worldviews$edition)
table(ESS_worldviews$proddate,ESS_worldviews$edition)

########################################



library(scatterpie)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)

for (i in 1:length(editions)) {

if(max(ESS_worldviews$cluster[ESS_worldviews$edition==editions[i]])==3) {
country.tab<-(table(ESS_worldviews$cntry[ESS_worldviews$edition==editions[i]],ESS_worldviews$cluster[ESS_worldviews$edition==editions[i]]))

country_df <- data.frame(
  iso =rownames(country.tab),
  class1 = country.tab[,1],
  class2 = country.tab[,2],
  class3 = country.tab[,3]
)
country_df$size<-apply(country.tab[,1:3],1,sum)
country_df$size.std<-country_df$size/max(country_df$size)

country_df$country<-countrycode(country_df$iso, "iso2c", "country.name")

# Get European country shapes as sf object
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(returnclass = "sf", scale = "medium")

cropbox<-  c(xmin = -27.5, ymin = 30.3, xmax = 41.3, ymax = 71.3)
localsf<-st_crop(world, st_bbox(cropbox))


centroids <- st_centroid(localsf$geometry)
centroids <- st_sf(country = localsf$admin, geometry = centroids)
centroids <- cbind(centroids, st_coordinates(centroids$geometry))

country_df <- merge(country_df, as.data.frame(centroids)[, c("country", "X", "Y")], 
                 by.x = "country", by.y = "country", all.x = TRUE)

# Plot the map with pie charts
mappy<-ggplot() +
  # Plot the base map 
  geom_sf(data = localsf, fill = "lightgrey", color = "white") +
  # Add pie charts at centroids
  geom_scatterpie(data = country_df, aes(x = X, y = Y,r = 2*sqrt(size.std)),
                  cols = c("class1", "class2", "class3"),
                    
                  color = "black", alpha = 0.8) +
  # Set coordinate system and limits for Europe
  coord_sf(xlim = c(-27.5, 41.3), ylim = c(30.3, 71.3), expand = FALSE) +
  # Customize theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA)) +  # Optional: ocean color
  labs(title = paste0("cluster prevalence by country ESS edition ",editions[i]),
       x = NULL, y = NULL) +
  # Optional: customize pie colors
  scale_fill_manual(values = c("class1" = "dark green", "class2" = "dark red", "class3" = "dark blue"),
                    name = "Proportions")



 
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/map_prevalence_edition_",editions[i],".png"),units="cm",res=200,width=25,height=20)
print(mappy)
dev.off()

} else {

country.tab<-(table(ESS_worldviews$cntry[ESS_worldviews$edition==editions[i]],ESS_worldviews$cluster[ESS_worldviews$edition==editions[i]]))

country_df <- data.frame(
  iso =rownames(country.tab),
  class1 = country.tab[,1],
  class2 = country.tab[,2],
  class3 = country.tab[,3],
  class4 = country.tab[,4]
)
country_df$size<-apply(country.tab[,1:4],1,sum)
country_df$size.std<-country_df$size/max(country_df$size)

country_df$country<-countrycode(country_df$iso, "iso2c", "country.name")

# Get European country shapes as sf object
world <- ne_countries(returnclass = "sf", scale = "medium")

cropbox<-  c(xmin = -27.5, ymin = 30.3, xmax = 41.3, ymax = 71.3)
localsf<-st_crop(world, st_bbox(cropbox))


centroids <- st_centroid(localsf$geometry)
centroids <- st_sf(country = localsf$admin, geometry = centroids)
centroids <- cbind(centroids, st_coordinates(centroids$geometry))

country_df <- merge(country_df, as.data.frame(centroids)[, c("country", "X", "Y")], 
                 by.x = "country", by.y = "country", all.x = TRUE)

# Plot the map with pie charts
mappy<-ggplot() +
  # Plot the base map 
  geom_sf(data = localsf, fill = "lightgrey", color = "white") +
  # Add pie charts at centroids
  geom_scatterpie(data = country_df, aes(x = X, y = Y,r = 2*sqrt(size.std)),
                  cols = c("class1", "class2", "class3","class4"),
                    
                  color = "black", alpha = 0.8) +
  # Set coordinate system and limits for Europe
  coord_sf(xlim = c(-27.5, 41.3), ylim = c(30.3, 71.3), expand = FALSE) +
  # Customize theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA)) +  # Optional: ocean color
  labs(title = paste0("cluster prevalence by country ESS edition ",editions[i]),
       x = NULL, y = NULL) +
  # Optional: customize pie colors
  scale_fill_manual(values = c("class1" = "dark green", "class2" = "dark red", "class3" = "dark blue","class4"="dark orange"),
                    name = "Proportions")



 
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/map_prevalence_edition_",editions[i],".png"),units="cm",res=200,width=25,height=20)
print(mappy)
dev.off()

}


}




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#### round level instead

ESS11<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS11.csv",header=T)
ESS10<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS10.csv",header=T)
ESS08<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS8e02_3.csv",header=T)

sum(values%in%colnames(ESS11))
values%in%colnames(ESS10)
values%in%colnames(ESS08)


values%in%colnames(ESS)
select<-colnames(ESS11)%in%values
select[1:13]<-TRUE
ESS11_worldviews<-ESS11[,select]

select<-colnames(ESS10)%in%values
select[1:13]<-TRUE
ESS10_worldviews<-ESS10[,select]

select<-colnames(ESS08)%in%values
select[1:13]<-TRUE
ESS08_worldviews<-ESS08[,select]

#all ordinal?
#yes but values >10 or >5 should be turned to NA!! for now - after all refusal to answer is a clue to worldview for some questions but we can't handle it in an ordinal manner....

fivers<-c(
"cptppola",
"dclenv",
"ecohenv",
"gincdif",
"ginveco",
"lawobey",
"polcmpl",
"psppsgva",
"scnsenv",
"lrnobed",
"elgnuc",
"inctxff",
"sbsrnen")

fivers11<-fivers[fivers%in%colnames(ESS11_worldviews)]
fivers10<-fivers[fivers%in%colnames(ESS10_worldviews)]
fivers08<-fivers[fivers%in%colnames(ESS08_worldviews)]

ESS11_worldviews[fivers11][ESS11_worldviews[fivers11]>5&!is.na(ESS11_worldviews[fivers11])]<-NA
ESS10_worldviews[fivers10][ESS10_worldviews[fivers10]>5&!is.na(ESS10_worldviews[fivers10])]<-NA
ESS08_worldviews[fivers08][ESS08_worldviews[fivers08]>5&!is.na(ESS08_worldviews[fivers08])]<-NA

sixers<-c(
"impenv",
"impenva",
"impfree",
"impfreea",
"imptrad",
"imptrada",
"ipbhprp",
"ipfrule",
"ipfrulea",
"iphlppl",
"iphlppla",
"ipstrgv",
"ipstrgva",
"likrisk",
"actcomp")
sixers11<-sixers[sixers%in%colnames(ESS11_worldviews)]
sixers10<-sixers[sixers%in%colnames(ESS10_worldviews)]
sixers08<-sixers[sixers%in%colnames(ESS08_worldviews)]


ESS11_worldviews[sixers11][ESS11_worldviews[sixers11]>6&!is.na(ESS11_worldviews[sixers11])]<-NA
ESS10_worldviews[sixers10][ESS10_worldviews[sixers10]>6&!is.na(ESS10_worldviews[sixers10])]<-NA
ESS08_worldviews[sixers08][ESS08_worldviews[sixers08]>6&!is.na(ESS08_worldviews[sixers08])]<-NA

values11<-values[values%in%colnames(ESS11_worldviews)]
values10<-values[values%in%colnames(ESS10_worldviews)]
values08<-values[values%in%colnames(ESS08_worldviews)]

ESS11_worldviews[values11][ESS11_worldviews[values11]>10&!is.na(ESS11_worldviews[values11])]<-NA
ESS10_worldviews[values10][ESS10_worldviews[values10]>10&!is.na(ESS10_worldviews[values10])]<-NA
ESS08_worldviews[values08][ESS08_worldviews[values08]>10&!is.na(ESS08_worldviews[values08])]<-NA



thezeros11<-names(which(apply(ESS11_worldviews[values11],2,function(x) min(x,na.rm=T))==0))
thezeros10<-names(which(apply(ESS10_worldviews[values10],2,function(x) min(x,na.rm=T))==0))
thezeros08<-names(which(apply(ESS08_worldviews[values08],2,function(x) min(x,na.rm=T))==0))


ESS11_worldviews[thezeros11]<-ESS11_worldviews[thezeros11]+1
ESS10_worldviews[thezeros10]<-ESS10_worldviews[thezeros10]+1
ESS08_worldviews[thezeros08]<-ESS08_worldviews[thezeros08]+1

#only 49 individuals have all NAs, we keep them in
# we remove them
whosna11<-apply(ESS11_worldviews[,14:30],1,function (x) sum(is.na(x)))
allnas11<-which(whosna11>16)
whosna11<-apply(ESS11_worldviews[,14:30],1,function (x) sum(is.na(x)))
allnas11<-which(whosna11>16)
whosna08<-apply(ESS08_worldviews[,14:35],1,function (x) sum(is.na(x)))
allnas08<-which(whosna11>21)


#rest is ten
formulae11<-as.formula(paste("cbind(", paste(values11, collapse = ", "), ") ~ 1")) # Replace with your column names
formulae10<-as.formula(paste("cbind(", paste(values10, collapse = ", "), ") ~ 1")) # Replace with your column names
formulae08<-as.formula(paste("cbind(", paste(values08, collapse = ", "), ") ~ 1")) # Replace with your column names

formulae.ls<-list(formulae11,formulae10,formulae08)
ESS.ls<-list(ESS11_worldviews,ESS10_worldviews,ESS08_worldviews)
values.ls<-list(values11,values10,values08)
#install.packages("poLCA")
library(poLCA)

# try<-poLCA(formulae, data = ESS_worldviews[values], nclass = 2, maxiter = 1000, 
                           # nrep = 5, na.rm = FALSE)

lca_rounds<-vector("list",length=3)
names(lca_rounds)<-c("ESS11","ESS10","ESS8")

for (j in 1:length(lca_rounds)) {

ESS<-ESS.ls[[j]]
valueinedition<-values.ls[[j]]
formulae<-formulae.ls[[j]]

max_clusters <- length(valueinedition)
lca_models <- list()
bic_values <- numeric(max_clusters)
entropy.std<-numeric(max_clusters)

 for (k in 1:max_clusters) {
   lca_models[[k]] <- poLCA(formulae, data = ESS[valueinedition], nclass = k, maxiter = 1000, 
                            nrep = 1, na.rm = FALSE,calc.se=FALSE,tol = 1e-6)  # na.rm = FALSE for FIML lighten all this for cluster number definition
																				# will refit with increased tolerance, se calculated and nrep >3
	bic_values[k] <- lca_models[[k]]$bic
	mattemp<-lca_models[[k]]["posterior"]$posterior
	lmattemp<-log(mattemp)
	lmattemp[lmattemp==-Inf]<-0
	entropy.std[k]<-1-(-sum(mattemp*lmattemp)/(dim(mattemp)[1]*log(dim(mattemp)[2]))) #maximise: 1-H

   print(k)
   flush.console()
}

lca_rounds[[j]]<-list(models=lca_models,bic=bic_values,entropy.std=entropy.std)
print(j)
flush.console()
}

maxclus<-unlist(lapply(values.ls,length))

modelselection_df<-data.frame(round=c(rep("ESS11",maxclus[1]),rep("ESS10",maxclus[2]),rep("ESS08",maxclus[3])),
								clusters=c(c(1:maxclus[1]),c(1:maxclus[2]),c(1:maxclus[3])),
								bic=c(lca_rounds[[1]]["bic"]$bic,lca_rounds[[2]]["bic"]$bic,lca_rounds[[3]]["bic"]$bic),
								entropy=c(lca_rounds[[1]]["entropy.std"]$entropy.std,lca_rounds[[2]]["entropy.std"]$entropy.std,lca_rounds[[3]]["entropy.std"]$entropy.std))

modelselection_df$entropy[is.nan(modelselection_df$entropy)==TRUE]<-0

ggplot(modelselection_df,aes(x=clusters,y=bic,colour=round))+
geom_line()+
#ylim(0.7,0.8)+
theme_minimal()

modelselection_df$dbic<-0
modelselection_df$dbic[modelselection_df$round=="ESS11"]<-(modelselection_df$bic[modelselection_df$round=="ESS11"]-lag(modelselection_df$bic[modelselection_df$round=="ESS11"],1))/modelselection_df$bic[modelselection_df$round=="ESS11"][1]
modelselection_df$dbic[modelselection_df$round=="ESS10"]<-(modelselection_df$bic[modelselection_df$round=="ESS10"]-lag(modelselection_df$bic[modelselection_df$round=="ESS10"],1))/modelselection_df$bic[modelselection_df$round=="ESS10"][1]
modelselection_df$dbic[modelselection_df$round=="ESS08"]<-(modelselection_df$bic[modelselection_df$round=="ESS08"]-lag(modelselection_df$bic[modelselection_df$round=="ESS08"],1))/modelselection_df$bic[modelselection_df$round=="ESS08"][1]

ggplot(modelselection_df,aes(x=clusters,y=dbic,colour=round))+
geom_line()+
#ylim(0.7,0.8)+
theme_minimal()

#ESS11: 4 clusters

#ESS10: 5 clusters

#ESS08: 9 clusters

best_clus_round<-data.frame(round=c("ESS11","ESS10","ESS08"),clus=c(4,5,6)) #first maxima ala modularity coefficient



lca_round_best<-vector("list",length=3)
names(lca_round_best)<-c("ESS11","ESS10","ESS8")


for (j in 1:length(lca_round_best)) {

ESS<-ESS.ls[[j]]
valueinedition<-values.ls[[j]]
formulae<-formulae.ls[[j]]

k <- best_clus_round$clus[j]

lca_round_best[[j]] <- poLCA(formulae, data = ESS[valueinedition], nclass = k, maxiter = 1000, 
                            nrep = 3, na.rm = FALSE,calc.se=TRUE,tol = 1e-10)  # na.rm = FALSE for FIML lighten all this for cluster number definition
   print(j)
   flush.console()
}


save(lca_round_best,values.ls,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/best_models_last_3_rounds.Rdata")
##we need to use ln not log_2 for entropy to have on the same scale as BIC


modprobs<-lca_round_best[[1]]["probs"]
v1<-modprobs[[1]][[2]]

library(reshape2)

v1m<-melt(v1)

ggplot(v1m,aes(x=Var2,y=value,fill=Var1))+
geom_bar(stat="identity",position="dodge")+
coord_polar()+
theme_minimal()

#weighted average (by probability) response - median response expected
apply(v1,1,cumsum)

model_interpret_round<-data.frame(round=c(rep("ESS11",length(values.ls[[1]])),rep("ESS10",length(values.ls[[2]])),rep("ESS8",length(values.ls[[3]]))),values=c(values.ls[[1]],values.ls[[2]],values.ls[[3]]))
model_interpret_round$class1<-NA
model_interpret_round$class2<-NA
model_interpret_round$class3<-NA
model_interpret_round$class4<-NA
model_interpret_round$class5<-NA
model_interpret_round$class6<-NA


for (i in 1:nrow(model_interpret_round)) {
ed<-model_interpret_round$round[i]
val<-model_interpret_round$values[i]

modprobs<-lca_round_best[[ed]]["probs"]
v1<-modprobs[[1]][val]
model_interpret_round$scale[i]<-ncol(v1[[val]])

valcum<-apply(v1[[val]],1,cumsum)


if (model_interpret_round$round[i]=="ESS11") {
model_interpret_round[i,c("class1","class2","class3","class4")]<-as.numeric(apply(valcum,2,function(x) which.min(abs(x-0.5))))

} else {

if (model_interpret_round$round[i]=="ESS10") {
model_interpret_round[i,c("class1","class2","class3","class4","class5")]<-as.numeric(apply(valcum,2,function(x) which.min(abs(x-0.5))))

} else {
model_interpret_round[i,c("class1","class2","class3","class4","class5","class6")]<-as.numeric(apply(valcum,2,function(x) which.min(abs(x-0.5))))

}
}
}


library(tidyverse)
model_interpret_round<-model_interpret_round%>%
					arrange(desc(round),desc(values)) 


library(ggpubr)


#rose plots
rounds<-c("ESS11","ESS10","ESS8")
for (i in 1:3) {

edition1.class1<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class1/scale))+
geom_bar(stat="identity",fill="dark green")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class2<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class2/scale))+
geom_bar(stat="identity",fill="dark red")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class3<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class3/scale))+
geom_bar(stat="identity",fill="dark blue")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class4<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class4/scale))+
geom_bar(stat="identity",fill="dark orange")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class5<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class5/scale))+
geom_bar(stat="identity",fill="darksalmon")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))

edition1.class6<-ggplot(subset(model_interpret_round,round==rounds[i]),aes(x=values,y=class6/scale))+
geom_bar(stat="identity",fill="darkturquoise")+
coord_polar()+
theme_minimal()+
xlab("")+ylab("")+
ylim(0,1)+
theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
theme(axis.text.x = element_text(size = 10))


if (rounds[i]=="ESS11") {
graph1<-ggarrange(edition1.class1,edition1.class2,edition1.class3,edition1.class4,ncol=4,nrow=1)
fig1<-annotate_figure(graph1,
						fig.lab = paste0("ESS round ",rounds[i]),
						fig.lab.pos = "top.left",
						fig.lab.size=12,
						fig.lab.face="bold")
  
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/rose_plot_clusters_interpretations_round_",rounds[i],".png"),units="cm",res=200,width=56,height=14)
print(fig1)
dev.off()

} else {

if (rounds[i]=="ESS10") {
graph1<-ggarrange(edition1.class1,edition1.class2,edition1.class3,edition1.class4,edition1.class5,ncol=3,nrow=2)
fig1<-annotate_figure(graph1,
						fig.lab = paste0("ESS round ",rounds[i]),
						fig.lab.pos = "top.left",
						fig.lab.size=12,
						fig.lab.face="bold")
						
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/rose_plot_clusters_interpretations_round_",rounds[i],".png"),units="cm",res=200,width=42,height=28)
print(fig1)
dev.off()

} else {

graph1<-ggarrange(edition1.class1,edition1.class2,edition1.class3,edition1.class4,edition1.class5,edition1.class6,ncol=3,nrow=2)
fig1<-annotate_figure(graph1,
						fig.lab = paste0("ESS round ",rounds[i]),
						fig.lab.pos = "top.left",
						fig.lab.size=12,
						fig.lab.face="bold")
						
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/rose_plot_clusters_interpretations_round_",rounds[i],".png"),units="cm",res=200,width=42,height=28)
print(fig1)
dev.off()

}

}

}


#+
#scale_fill_brewer(palette="Greens")

ESS11$cluster<-lca_round_best[[rounds[1]]]["predclass"][[1]]
ESS10$cluster<-lca_round_best[[rounds[2]]]["predclass"][[1]]
ESS08$cluster<-lca_round_best[[rounds[3]]]["predclass"][[1]]


write.csv(ESS11,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS_complete_with_clusters_classified_ESS11.csv")
write.csv(ESS10,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS_complete_with_clusters_classified_ESS10.csv")
write.csv(ESS08,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/ESS_complete_with_clusters_classified_ESS08.csv")


table(ESS_worldviews$cntry,ESS_worldviews$cluster,ESS_worldviews$edition)
table(ESS_worldviews$proddate,ESS_worldviews$edition)

########################################



library(scatterpie)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)

#ESS11
country.tab<-(table(ESS11$cntry,ESS11$cluster))

country_df <- data.frame(
  iso =rownames(country.tab),
  class1 = country.tab[,1],
  class2 = country.tab[,2],
  class3 = country.tab[,3],
  class4 = country.tab[,4]
)
country_df$size<-apply(country.tab[,1:4],1,sum)
country_df$size.std<-country_df$size/max(country_df$size)

country_df$country<-countrycode(country_df$iso, "iso2c", "country.name")

# Get European country shapes as sf object
world <- ne_countries(returnclass = "sf", scale = "medium")

cropbox<-  c(xmin = -27.5, ymin = 30.3, xmax = 41.3, ymax = 71.3)
localsf<-st_crop(world, st_bbox(cropbox))


centroids <- st_centroid(localsf$geometry)
centroids <- st_sf(country = localsf$admin, geometry = centroids)
centroids <- cbind(centroids, st_coordinates(centroids$geometry))

country_df <- merge(country_df, as.data.frame(centroids)[, c("country", "X", "Y")], 
                 by.x = "country", by.y = "country", all.x = TRUE)

# Plot the map with pie charts
mappy<-ggplot() +
  # Plot the base map 
  geom_sf(data = localsf, fill = "lightgrey", color = "white") +
  # Add pie charts at centroids
  geom_scatterpie(data = country_df, aes(x = X, y = Y,r = 2*sqrt(size.std)),
                  cols = c("class1", "class2", "class3","class4"),
                    
                  color = "black", alpha = 0.8) +
  # Set coordinate system and limits for Europe
  coord_sf(xlim = c(-27.5, 41.3), ylim = c(30.3, 71.3), expand = FALSE) +
  # Customize theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA)) +  # Optional: ocean color
  labs(title = paste0("cluster prevalence by country ESS round 11"),
       x = NULL, y = NULL) +
  # Optional: customize pie colors
  scale_fill_manual(values = c("class1" = "dark green", "class2" = "dark red", "class3" = "dark blue","class4"="dark orange"),
                    name = "Proportions")



 
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/map_prevalence_round_ESS11.png"),units="cm",res=200,width=25,height=20)
print(mappy)
dev.off()


##############################
#ESS10
country.tab<-(table(ESS10$cntry,ESS10$cluster))

country_df <- data.frame(
  iso =rownames(country.tab),
  class1 = country.tab[,1],
  class2 = country.tab[,2],
  class3 = country.tab[,3],
  class4 = country.tab[,4],
  class5 = country.tab[,5]
)
country_df$size<-apply(country.tab[,1:5],1,sum)
country_df$size.std<-country_df$size/max(country_df$size)

country_df$country<-countrycode(country_df$iso, "iso2c", "country.name")

# Get European country shapes as sf object
# world <- ne_countries(returnclass = "sf", scale = "medium")

# cropbox<-  c(xmin = -27.5, ymin = 30.3, xmax = 41.3, ymax = 71.3)
# localsf<-st_crop(world, st_bbox(cropbox))


# centroids <- st_centroid(localsf$geometry)
# centroids <- st_sf(country = localsf$admin, geometry = centroids)
# centroids <- cbind(centroids, st_coordinates(centroids$geometry))

country_df <- merge(country_df, as.data.frame(centroids)[, c("country", "X", "Y")], 
                 by.x = "country", by.y = "country", all.x = TRUE)

# Plot the map with pie charts
mappy<-ggplot() +
  # Plot the base map 
  geom_sf(data = localsf, fill = "lightgrey", color = "white") +
  # Add pie charts at centroids
  geom_scatterpie(data = country_df, aes(x = X, y = Y,r = 2*sqrt(size.std)),
                  cols = c("class1", "class2", "class3","class4","class5"),
                    
                  color = "black", alpha = 0.8) +
  # Set coordinate system and limits for Europe
  coord_sf(xlim = c(-27.5, 41.3), ylim = c(30.3, 71.3), expand = FALSE) +
  # Customize theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA)) +  # Optional: ocean color
  labs(title = paste0("cluster prevalence by country ESS round 10"),
       x = NULL, y = NULL) +
  # Optional: customize pie colors
  scale_fill_manual(values = c("class1" = "dark green", "class2" = "dark red", "class3" = "dark blue","class4"="dark orange","class5"="darksalmon"), #"darkturquoise"
                    name = "Proportions")



 
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/map_prevalence_round_ESS10.png"),units="cm",res=200,width=25,height=20)
print(mappy)
dev.off()

###########################################
#ESS08
country.tab<-(table(ESS08$cntry,ESS08$cluster))

country_df <- data.frame(
  iso =rownames(country.tab),
  class1 = country.tab[,1],
  class2 = country.tab[,2],
  class3 = country.tab[,3],
  class4 = country.tab[,4],
  class5 = country.tab[,5],
  class6 = country.tab[,6]
)
country_df$size<-apply(country.tab[,1:6],1,sum)
country_df$size.std<-country_df$size/max(country_df$size)

country_df$country<-countrycode(country_df$iso, "iso2c", "country.name")

# Get European country shapes as sf object
# world <- ne_countries(returnclass = "sf", scale = "medium")

# cropbox<-  c(xmin = -27.5, ymin = 30.3, xmax = 41.3, ymax = 71.3)
# localsf<-st_crop(world, st_bbox(cropbox))


# centroids <- st_centroid(localsf$geometry)
# centroids <- st_sf(country = localsf$admin, geometry = centroids)
# centroids <- cbind(centroids, st_coordinates(centroids$geometry))

country_df <- merge(country_df, as.data.frame(centroids)[, c("country", "X", "Y")], 
                 by.x = "country", by.y = "country", all.x = TRUE)

# Plot the map with pie charts
mappy<-ggplot() +
  # Plot the base map 
  geom_sf(data = localsf, fill = "lightgrey", color = "white") +
  # Add pie charts at centroids
  geom_scatterpie(data = country_df, aes(x = X, y = Y,r = 2*sqrt(size.std)),
                  cols = c("class1", "class2", "class3","class4","class5","class6"),
                    
                  color = "black", alpha = 0.8) +
  # Set coordinate system and limits for Europe
  coord_sf(xlim = c(-27.5, 41.3), ylim = c(30.3, 71.3), expand = FALSE) +
  # Customize theme
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA)) +  # Optional: ocean color
  labs(title = paste0("cluster prevalence by country ESS round 08"),
       x = NULL, y = NULL) +
  # Optional: customize pie colors
  scale_fill_manual(values = c("class1" = "dark green", "class2" = "dark red", "class3" = "dark blue","class4"="dark orange","class5"="darksalmon","class6"="darkturquoise"), #"darkturquoise"
                    name = "Proportions")



 
png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/map_prevalence_round_ESS08.png"),units="cm",res=200,width=25,height=20)
print(mappy)
dev.off()

## how worried about climate change are the clusters

#wrclmch 1-5

#### climate change is natural
#ccnthum 1-5 55: climate change is not happening, 1-5: climate change is happening


ESS11$climatechangedeny<-NA
ESS11$climatechangedeny[ESS11$ccnthum==55]<-TRUE
ESS11$climatechangedeny[ESS11$ccnthum<6]<-FALSE
ESS11$wrclmch.std<-ESS11$wrclmch
ESS11$wrclmch.std[ESS11$wrclmch.std>5]<-NA
ESS11$ccnthum.std<-ESS11$ccnthum
ESS11$ccnthum.std[ESS11$ccnthum.std>5]<-NA

ESS10$climatechangedeny<-NA
ESS10$climatechangedeny[ESS10$ccnthum==55]<-TRUE
ESS10$climatechangedeny[ESS10$ccnthum<6]<-FALSE
ESS10$wrclmch.std<-ESS10$wrclmch
ESS10$wrclmch.std[ESS10$wrclmch.std>5]<-NA
ESS10$ccnthum.std<-ESS10$ccnthum
ESS10$ccnthum.std[ESS10$ccnthum.std>5]<-NA

ESS08$climatechangedeny<-NA
ESS08$climatechangedeny[ESS08$ccnthum==55]<-TRUE
ESS08$climatechangedeny[ESS08$ccnthum<6]<-FALSE
ESS08$wrclmch.std<-ESS08$wrclmch
ESS08$wrclmch.std[ESS08$wrclmch.std>5]<-NA
ESS08$ccnthum.std<-ESS08$ccnthum
ESS08$ccnthum.std[ESS08$ccnthum.std>5]<-NA

cd_df11<-table(ESS11$climatechangedeny,ESS11$cluster)
cc_df11<-data.frame(class=c("1","2","3","4"),CCdeny=cd_df11[2,]/colSums(cd_df11))

cd_df10<-table(ESS10$climatechangedeny,ESS10$cluster)
cc_df10<-data.frame(class=c("1","2","3","4","5"),CCdeny=cd_df10[2,]/colSums(cd_df10))

cd_df08<-table(ESS08$climatechangedeny,ESS08$cluster)
cc_df08<-data.frame(class=c("1","2","3","4","5","6"),CCdeny=cd_df08[2,]/colSums(cd_df08))

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatedenying_ESS11.png"),units="cm",res=200,width=30,height=15)

ggplot(cc_df11,aes(x=factor(class),y=CCdeny))+
geom_bar(stat="identity")+
theme_minimal()+
ylab("proportion of respondents denying climate change")+
xlab("cluster")+
labs(title="ESS round 11")+
ylim(0,0.035)

dev.off()

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatedenying_ESS10.png"),units="cm",res=200,width=30,height=15)

ggplot(cc_df10,aes(x=factor(class),y=CCdeny))+
geom_bar(stat="identity")+
theme_minimal()+
ylab("proportion of respondents denying climate change")+
xlab("cluster")+
labs(title="ESS round 10")+
ylim(0,0.035)

dev.off()

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatedenying_ESS08.png"),units="cm",res=200,width=30,height=15)

ggplot(cc_df08,aes(x=factor(class),y=CCdeny))+
geom_bar(stat="identity")+
theme_minimal()+
ylab("proportion of respondents denying climate change")+
xlab("cluster")+
labs(title="ESS round 8")+
ylim(0,0.035)

dev.off()

################################
##human cause of CC
cdh_df11<-table(ESS11$ccnthum.std,ESS11$cluster)
cdh_df11<-apply(cdh_df11, 2, function(x) x / sum(x))
cdh_df11.m<-melt(cdh_df11)
names(cdh_df11.m)<-c("response","cluster","probability")

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeisHuman_ESS11.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df11.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("climate change is caused by natural processes ↔ human activities")+
labs(title="ESS round 11",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon"), #"darkturquoise"
                    name = "cluster")

dev.off()


cdh_df10<-table(ESS10$ccnthum.std,ESS10$cluster)
cdh_df10<-apply(cdh_df10, 2, function(x) x / sum(x))

cdh_df10.m<-melt(cdh_df10)
names(cdh_df10.m)<-c("response","cluster","probability")

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeisHuman_ESS10.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df10.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("climate change is caused by natural processes ↔ human activities")+
labs(title="ESS round 10",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon","6"="darkturquoise"), #
                    name = "cluster")

dev.off()


cdh_df08<-table(ESS08$ccnthum.std,ESS08$cluster)
cdh_df08<-apply(cdh_df08, 2, function(x) x / sum(x))
cdh_df08.m<-melt(cdh_df08)
names(cdh_df08.m)<-c("response","cluster","probability")



png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeisHuman_ESS08.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df08.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("climate change is caused by natural processes ↔ human activities")+
labs(title="ESS round 8",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon","6"="darkturquoise"), #
                    name = "cluster")

dev.off()


########################################################################
#### worried 
#wrclmch.std


cdh_df11<-table(ESS11$wrclmch.std,ESS11$cluster)
cdh_df11<-apply(cdh_df11, 2, function(x) x / sum(x))
cdh_df11.m<-melt(cdh_df11)
names(cdh_df11.m)<-c("response","cluster","probability")

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeWorried_ESS11.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df11.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("How worried about climate change not at all ↔ extemely")+
labs(title="ESS round 11",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon"), #"darkturquoise"
                    name = "cluster")

dev.off()


cdh_df10<-table(ESS10$wrclmch.std,ESS10$cluster)
cdh_df10<-apply(cdh_df10, 2, function(x) x / sum(x))

cdh_df10.m<-melt(cdh_df10)
names(cdh_df10.m)<-c("response","cluster","probability")

png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeWorried_ESS10.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df10.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("How worried about climate change not at all ↔ extemely")+
labs(title="ESS round 10",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon","6"="darkturquoise"), #
                    name = "cluster")

dev.off()


cdh_df08<-table(ESS08$wrclmch.std,ESS08$cluster)
cdh_df08<-apply(cdh_df08, 2, function(x) x / sum(x))
cdh_df08.m<-melt(cdh_df08)
names(cdh_df08.m)<-c("response","cluster","probability")



png(paste("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/SABRES/5pt1 ms/climatechangeWorried_ESS08.png"),units="cm",res=200,width=30,height=15)

ggplot(cdh_df08.m,aes(x=(response),y=probability,fill=factor(cluster)))+
geom_bar(stat="identity",position="dodge")+
theme_minimal()+
ylab("probability of response")+
xlab("How worried about climate change not at all ↔ extemely")+
labs(title="ESS round 8",fill="cluster")+
scale_fill_manual(values = c("1" = "dark green", "2" = "dark red", "3" = "dark blue","4"="dark orange","5"="darksalmon","6"="darkturquoise"), #
                    name = "cluster")

dev.off()


##############################
#ordinal logistic regression
library(MASS)

ESS11$wrclmch.std <- ordered(ESS11$wrclmch.std, levels = 1:5)
ESS11$cluster <- factor(ESS11$cluster)

model11 <- polr(wrclmch.std ~ cluster, data = ESS11, Hess = TRUE)
summary(model11)

library(car)
Anova(model11)
pred0<-ggpredict(model11)
plot(pred0)


ESS11$ccnthum.std <- ordered(ESS11$ccnthum.std, levels = 1:5)
ESS11$cluster <- factor(ESS11$cluster)

model11.h <- polr(ccnthum.std ~ cluster, data = ESS11, Hess = TRUE)
summary(model11.h)

library(car)
Anova(model11.h)
pred0h<-ggpredict(model11.h)
plot(pred0h)
