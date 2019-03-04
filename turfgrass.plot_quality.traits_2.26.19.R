## Demo script for quantifying some interesting traits in perennial ryegrass turf
## This script will demonstrate:
#1) measuring the proportion of a plot with healthy turf (density) 
#2) measuring the proportion of a plot consisting of stems (stemminess)
#3) measuring the proportion of a plot made up of crabgrass
## Date: 2.26.19
## Author: Garett Heineck


## Required packages - may need installation first
## PLEASE UPDATE R (if you have not done so recently)
library(tidyverse)
library(readxl)
library(dplyr)
library(jpeg)
library(EBImage) #needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)***
library(randomForest)
library(stringr)
library(ggplot2)
library(cowplot)
##############
##############
##############


##############
##############
##############
## Required file path
#parent directory folder nomed "grass.leaf_rust.detection_2.26.19"***
#To run this you need a folder named "grass.leaf_rust.detection_2.26.19"
img_dir_turf.demo.2.26.19<- "/Users/heine237/Documents/GitHub/turfgrass.plot_quality.traits_2.26.19" #NOTE: change to your own file path***
#************************#
## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("training data_2.26.19",
             "results", 
             "original_img", 
             "S1_green.grass.foreground_classify", 
             "S2_green.grass.foreground_EBImage", 
             "S3_stem.foreground_classify", 
             "S4_stem.foreground_EBImage", 
             "S5_crabgrass.foreground_classify", 
             "S6_crabgrass.foreground_EBImage",
             "S7_crabgrass.cluster_EBImage") #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_dir_turf.demo.2.26.19,folders[i], sep="/")) 
}
#NOTE: you may see Warning messages, that is ok***
#make sure the "original_img" folder has images in it***
##############
##############
##############


##############
##############
##############
#************************#
## training palettes
palette_directory_plant<- paste(img_dir_turf.demo.2.26.19, "training data_2.26.19",sep = "/") # file path where mixes are saved***
#************************#
mixes_names<- list.files(path=palette_directory_plant,pattern="*.csv",full.names = FALSE) # name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_plant, pattern="*.csv", full.names = TRUE) # path directory for what is in the palette folder***
training.palette_plant<- data.frame(mix=factor(), image=factor(), x=numeric(), y=numeric(), red=numeric(), green=numeric(), blue=numeric()) # an object will build the training palette mixes***
## A for() loop that will systematically re arrange and condense each mix file in the training palette folder ###
## The reason I am going about this in this particular way is to make a system that automatically update itself upon adding additional mixes ###
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band)
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[1]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean)
  training.palette_plant<- rbind(training.palette_plant, temp3) 
}
summary(training.palette_plant) #summarizing the training palette***
count(training.palette_plant, mix) #counting mixes training palette*** 
#View(count(training.palette_plant, mix))
#************************#
## Palette selection for healthy green grass
palette_selection_green.leaf<- training.palette_plant
palette_selection_green.leaf$classification<- c(rep(0, len=200),rep(1, len=300),rep(0, len=400)) 
palette_selection_green.leaf %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_green.leaf_plant<- randomForest(classification~(red+green+blue),data=palette_selection_green.leaf, ntree=80,mtry = 1,importance=TRUE)
print(rfm_green.leaf_plant)
plot(rfm_green.leaf_plant) # this could use up to 200 trees, but 100 looks ok
importance(rfm_green.leaf_plant)
#************************#
## Palette selection for stemmy and dead turf
palette_selection_stem<- training.palette_plant
palette_selection_stem$classification<- c(rep(0, len=700),rep(1, len=200)) 
palette_selection_stem %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_stem_plant<- randomForest(classification~(red+green+blue),data=palette_selection_stem, ntree=100,mtry = 1,importance=TRUE)
print(rfm_stem_plant)
plot(rfm_stem_plant)
importance(rfm_stem_plant)
#************************#
## Palette selection for crabgrass
palette_selection_crabgrass<- training.palette_plant
palette_selection_crabgrass$classification<- c(rep(1, len=200),rep(0, len=700)) 
palette_selection_crabgrass %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_crabgrass_plant<- randomForest(classification~(red+green+blue),data=palette_selection_crabgrass, ntree=100,mtry = 1,importance=TRUE)
print(rfm_crabgrass_plant)
plot(rfm_crabgrass_plant)
importance(rfm_crabgrass_plant)
##############
##############
##############


##############
##############
##############
## area image analysis on leaf plates
folder_original.img<-  (paste(img_dir_turf.demo.2.26.19,"original_img",sep = "/"))
folder_classify.leaf<-  (paste(img_dir_turf.demo.2.26.19,"S1_green.grass.foreground_classify",sep = "/"))
folder_EBImage.leaf<-  (paste(img_dir_turf.demo.2.26.19,"S2_green.grass.foreground_EBImage",sep = "/"))
folder_classify.stem<-  (paste(img_dir_turf.demo.2.26.19,"S3_stem.foreground_classify",sep = "/"))
folder_EBImage.stem<-  (paste(img_dir_turf.demo.2.26.19,"S4_stem.foreground_EBImage",sep = "/"))
folder_classify.crabgrass<-  (paste(img_dir_turf.demo.2.26.19,"S5_crabgrass.foreground_classify",sep = "/"))
folder_EBImage.crabgrass<-  (paste(img_dir_turf.demo.2.26.19,"S6_crabgrass.foreground_EBImage",sep = "/"))
folder_EBImage.cluster<-  (paste(img_dir_turf.demo.2.26.19,"S7_crabgrass.cluster_EBImage",sep = "/"))
#************************#
paths_original.img<- list.files(path=folder_original.img,full.names = TRUE)
names_original.img<- list.files(path=folder_original.img,full.names = FALSE) 

img.stats_turf.demo.2.26.19<- data.frame() #creating an empty data frame to record resutls***


for (i in 1:length(paths_original.img)){
  img.01<- readJPEG(paths_original.img[i]) #starting chunk for green leaves in turfgrass plots***
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(rfm_green.leaf_plant, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>.80, 1,0) #set threshold to 80%***
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.02, paste(folder_classify.leaf, "/", names_original.img[i], sep = ""), quality = 1)
  paths_classify_leaf<- list.files(path=folder_classify.leaf,full.names = TRUE)
  morph_op.01<- readImage(paths_classify_leaf[i])
  overlay.01<-  readImage(paths_original.img[i])
  kernal.01<- makeBrush(7, shape="Gaussian")
  image_dilate.01<- thresh(dilate(morph_op.01, kernal.01), w=100, h=100, offset= 0.001)
  dim.1<- computeFeatures.moment(image_dilate.01, overlay.01)
  img.03 = stackObjects(image_dilate.01, overlay.01, combine = T, bg.col='black', ext = dim.1[1])
  writeImage(img.03, paste(folder_EBImage.leaf, "/", names_original.img[i] ,sep=""), quality = 100)
  leaf.featr.img.03<- bwlabel(image_dilate.01)
  leaf.featr<- data.frame(computeFeatures.shape(leaf.featr.img.03))
  #************************#
  img.dat.02<- cbind(coor, red, green, blue) #starting chunk for perennial ryegrass stems in turfgrass plots***
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$classify<- predict(rfm_stem_plant, img.dat.02)
  img.dat.02$thresh<- ifelse(img.dat.02$classify>.40, 1,0) #set threshold to 40%***
  img.04<- matrix(img.dat.02$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.04, paste(folder_classify.stem, "/", names_original.img[i], sep = ""), quality = 1)
  paths_classify_stem<- list.files(path=folder_classify.stem,full.names = TRUE)
  morph_op.02<- readImage(paths_classify_stem[i])
  overlay.02<-  readImage(paths_original.img[i])
  kernal.02<- makeBrush(3, shape="Gaussian")
  image_dilate.02<- thresh(dilate(morph_op.02, kernal.02), w=100, h=100, offset= 0.001)
  dim.2<- computeFeatures.moment(image_dilate.02, overlay.02)
  img.05 = stackObjects(image_dilate.02, overlay.02, combine = T, bg.col='black', ext = dim.2[1])
  writeImage(img.05, paste(folder_EBImage.stem, "/", names_original.img[i] ,sep=""), quality = 100)
  stem.featr.img.05<- bwlabel(image_dilate.02)
  stem.featr<- data.frame(computeFeatures.shape(stem.featr.img.05))
  #************************#
  img.dat.03<- cbind(coor, red, green, blue) #starting chunk for crabgrass detection in turfgrass plots***
  colnames(img.dat.03)<- c("y","x","red","green","blue")
  img.dat.03$classify<- predict(rfm_crabgrass_plant, img.dat.03)
  img.dat.03$thresh<- ifelse(img.dat.03$classify>.70, 1,0) #set threshold to 70%***
  img.06<- matrix(img.dat.03$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.06, paste(folder_classify.crabgrass, "/", names_original.img[i], sep = ""), quality = 1)
  paths_classify_crabgrass<- list.files(path=folder_classify.crabgrass,full.names = TRUE)
  morph_op.03<- readImage(paths_classify_crabgrass[i])
  overlay.03<-  readImage(paths_original.img[i])
  image_morph.03<- fillHull(morph_op.03)
  image_morph.04<- opening(image_morph.03, makeBrush(5, shape="disc"))
  image_morph.05<- dilate(image_morph.04, makeBrush(7, shape="gaussian"))
  dim.3<- computeFeatures.moment(image_morph.05, overlay.03)
  img.07<- stackObjects(image_morph.05, overlay.03, combine = T, bg.col='black', ext = dim.3[1])
  writeImage(img.07, paste(folder_EBImage.crabgrass, "/", names_original.img[i] ,sep=""), quality = 100)
  crabgrass.featr.img.07<- bwlabel(image_morph.05)
  crabgrass.featr<- data.frame(computeFeatures.shape(crabgrass.featr.img.07)) #dataframe saving crabgrass related blobs***
  #************************#
  #image_wtr.shd<- watershed(image_morph.05, ext = 30)  #starting chunk for crabgrass CLUSTER detection in turfgrass plots***
  #crabgrass.cluster<- data.frame(computeFeatures.shape(image_wtr.shd))
  #crabgrass.cluster<- crabgrass.cluster %>% filter(s.area > quantile(s.area, 0.95))  %>% filter(s.area > 1000) #this is a VERY rough estimation of cluster number*** 
  #display(colorLabels(image_wtr.shd))
  #writeImage(colorLabels(image_wtr.shd), paste(folder_EBImage.cluster, "/", names_original.img[i] ,sep=""), quality = 100)
  
  write.stats<- data.frame(sum.pixel= (dim(img.01)[1]*dim(img.01)[2]),
                           sum.leaf=  sum(leaf.featr$s.area),
                           prop.leaf= sum(leaf.featr$s.area)/(dim(img.01)[1]*dim(img.01)[2]),
                           sum.stem=  sum(stem.featr$s.area),
                           prop.stem= sum(stem.featr$s.area)/(dim(img.01)[1]*dim(img.01)[2]),
                           sum.crab=  sum(crabgrass.featr$s.area),
                           prop.crab= sum(crabgrass.featr$s.area)/(dim(img.01)[1]*dim(img.01)[2])
                           #,crab.cluster.n= length(crabgrass.cluster$s.area), #counting the number of clusters of crabgrass***
                           #crab.cluster.avg= mean(crabgrass.cluster$s.area) #average pixel number of each clusters of crabgrass***
  )
  img.stats_turf.demo.2.26.19<-rbind(img.stats_turf.demo.2.26.19, write.stats)
}

##############
##############
##############


##############
##############
##############
## Saving computer output
## Binding metadata and computer output
write.csv(img.stats_turf.demo.2.26.19, paste(img_dir_turf.demo.2.26.19, "results","img.stats_plant.demo_2.25.19.csv", sep = "/")) #saving computer output***
img.stats_turf.demo.2.26.19<- read.csv(paste(img_dir_turf.demo.2.26.19, "results", "img.stats_plant.demo_2.25.19.csv", sep = "/")) #reading in computer output from the results folder***
turfplot.demo_field.data_2.26.19<- read_excel(paste(img_dir_turf.demo.2.26.19, "results", "turfplot.demo_field.data_2.26.19.xlsx", sep = "/"), skip = 6, na = ".") #reading in turf plot field data***

turf.demo.2.26.19.output<- cbind(turfplot.demo_field.data_2.26.19,img.stats_turf.demo.2.26.19) %>%
  mutate(prcnt.leaf= prop.leaf*100) %>%
  mutate(prcnt.stem= prop.stem*100) %>%
  mutate(prcnt.crab= prop.crab*100)
summary(turf.demo.2.26.19.output)
#************************#
## Plotting data

#plotting healthy turf***
prcnt.leaf<- ggplot(turf.demo.2.26.19.output, aes(x=plot_num, y=prcnt.leaf))+
  labs(x = "turfgrass plot", y = "% healthy leaves", title = "Healthy Leaves")+
  geom_col(fill = c("#0b3d4c", "#00b2e2", "#9bdae9"), color = "black")+
  geom_text(aes(label = round(prcnt.leaf, 1), y = prcnt.leaf+5), size = 6)
#plotting stem tissue***
prcnt.stem<- ggplot(turf.demo.2.26.19.output, aes(x=plot_num, y=prcnt.stem))+
  labs(x = "turfgrass plot", y = "% ryegrass stems", title = "Stemminess")+
  geom_col(fill = c("#0b3d4c", "#00b2e2", "#9bdae9"), color = "black")+
  geom_text(aes(label = round(prcnt.stem, 1), y = prcnt.stem+5), size = 6)
#plotting crabgrass infestation***
prcnt.crab<- ggplot(turf.demo.2.26.19.output, aes(x=plot_num, y=prcnt.crab))+
  labs(x = "turfgrass plot", y = "% ryegrass stems", title = "Crabgrass")+
  geom_col(fill = c("#0b3d4c", "#00b2e2", "#9bdae9"), color = "black")+
  geom_text(aes(label = round(prcnt.crab, 1), y = prcnt.crab+.5), size = 6)
#plotting visual turfgrass quality***
turf.quality<- ggplot(turf.demo.2.26.19.output, aes(x=plot_num, y=turf.quality))+
  labs(x = "turfgrass plot", y = "turf quality (VISUAL)", title = "Visual Quality Rating")+
  geom_col(fill = c("#0b3d4c", "#00b2e2", "#9bdae9"), color = "black")+
  geom_text(aes(label = round(turf.quality, 1), y = turf.quality+.5), size = 6)

#binding plots togather***
plot_grid(prcnt.leaf, prcnt.stem, prcnt.crab, labels = c("A","B","C","D"), turf.quality, ncol = 2)


