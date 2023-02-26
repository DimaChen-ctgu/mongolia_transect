######################### All pacages
setwd("D:/Stab")
library(ggplot2)
library(readxl)
library(grid)
library(multcomp)
library(lme4)
library(nlme)
library(readxl)
library(dplyr)
library(lavaan) 
library(piecewiseSEM) 
library(semPlot)
library(lavaanPlot)
library(Hmisc)
library(tidyr)

######################### Input dataset 
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

##################################################################Figure_1
B1<-ggplot(AB,aes(x=log1p(Plant_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant alpha-diversity (ln)")+ylab("Ecosystem stability (ln)")   
B2<-ggplot(AB,aes(x=log1p(Plant_alpha),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_alpha),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant alpha-diversity (ln)")+ylab("Mean NDVI (ln)")   
B3<-ggplot(AB,aes(x=log1p(Plant_alpha),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_alpha),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant alpha-diversity (ln)")+ylab("SD of NDVI (ln)")  
B4<-ggplot(AB,aes(x=log1p(Plant_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant beta-diversity (ln)")+ylab("Ecosystem stability (ln)")   
B5<-ggplot(AB,aes(x=log1p(Plant_beta),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_beta),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant beta-diversity (ln)")+ylab("Mean NDVI (ln)")    
B6<-ggplot(AB,aes(x=log1p(Plant_beta),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_beta),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant beta-diversity (ln)")+ylab("SD of NDVI (ln)")    

pdf("Fig_1_Plant_Div_Stab.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(2, 1))
print(B5, vp = vplayout(2, 2))
print(B6, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_1
M1<-lm(log1p(Plant_alpha)~log1p(Stability),AB)
summary(M1)
M2<-lm(log1p(Plant_alpha)~log1p(NDVI),AB)
summary(M2)
M3<-lm(log1p(Plant_alpha)~log1p(SD),AB)
summary(M3)
M4<-lm(log1p(Plant_beta)~log1p(Stability),AB)
summary(M4)
M5<-lm(log1p(Plant_beta)~log1p(NDVI),AB)
summary(M5)
M6<-lm(log1p(Plant_beta)~log1p(SD),AB)
summary(M6)
##################################################################



##################################################################Figure_2
B1<-ggplot(AB,aes(x=SB_alpha,y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_alpha,y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota alpha-diversity (z-score)")+ylab("Ecosystem stability (ln)")   
B2<-ggplot(AB,aes(x=SB_alpha,y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_alpha,y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota alpha-diversity (z-score)")+ylab("Mean NDVI (ln)")   
B3<-ggplot(AB,aes(x=SB_alpha,y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_alpha,y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota alpha-diversity (z-score)")+ylab("SD of NDVI (ln)")  
B4<-ggplot(AB,aes(x=SB_beta,y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_beta,y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota beta-diversity (z-score)")+ylab("Ecosystem stability (ln)")   
B5<-ggplot(AB,aes(x=SB_beta,y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_beta,y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota beta-diversity (z-score)")+ylab("Mean NDVI (ln)")    
B6<-ggplot(AB,aes(x=SB_beta,y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_beta,y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota beta-diversity (z-score)")+ylab("SD of NDVI (ln)")    

pdf("Fig_2_Soil_Biota_Div_Stab.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(2, 1))
print(B5, vp = vplayout(2, 2))
print(B6, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_2
M1<-lm(SB_alpha~log1p(Stability),AB)
summary(M1)
M2<-lm(SB_alpha~log1p(NDVI),AB)
summary(M2)
M3<-lm(SB_alpha~log1p(SD),AB)
summary(M3)
M4<-lm(SB_beta~log1p(Stability),AB)
summary(M4)
M5<-lm(SB_beta~log1p(NDVI),AB)
summary(M5)
M6<-lm(SB_beta~log1p(SD),AB)
summary(M6)
##################################################################



##################################################################Figure_3
B1<-ggplot(AB,aes(x=log1p(Ba_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial alpha-diversity (ln)")+ylab("Ecosystem stability (ln)")   
B2<-ggplot(AB,aes(x=log1p(Fu_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal alpha-diversity (ln)")+ylab("Mean NDVI (ln)")   
B3<-ggplot(AB,aes(x=log1p(Nema_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode alpha-diversity (ln)")+ylab("SD of NDVI (ln)")  
B4<-ggplot(AB,aes(x=log1p(Ba_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial beta-diversity (ln)")+ylab("Ecosystem stability (ln)")   
B5<-ggplot(AB,aes(x=log1p(Fu_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal beta-diversity (ln)")+ylab("Mean NDVI (ln)")    
B6<-ggplot(AB,aes(x=log1p(Nema_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode beta-diversity (ln)")+ylab("SD of NDVI (ln)")    

pdf("Fig_3_Soil_Biota_FG_Div_Stab.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(2, 1))
print(B5, vp = vplayout(2, 2))
print(B6, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_3
M1<-lm(log1p(Ba_alpha)~log1p(Stability),AB)
summary(M1)
M2<-lm(log1p(Fu_alpha)~log1p(Stability),AB)
summary(M2)
M3<-lm(log1p(Nema_alpha)~log1p(Stability),AB)
summary(M3)
M4<-lm(log1p(Ba_beta)~log1p(Stability),AB)
summary(M4)
M5<-lm(log1p(Fu_beta)~log1p(Stability),AB)
summary(M5)
M6<-lm(log1p(Nema_beta)~log1p(Stability),AB)
summary(M6)


##################################################################Fig.4a
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)
AB_log <- log1p(AB[,c(39:62)])   #Subset dataset of diversity of individual functional groups
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
AD<- as.data.frame(lapply(AB_log, minMax))  #normalise data using custom function

B1<-ggplot()+
geom_smooth(aes(x=AD$AB_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0000ff",linetype="solid")+
geom_smooth(aes(x=AD$PB_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0020ff",linetype="solid")+
geom_smooth(aes(x=AD$PF_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0040ff",linetype="solid")+
geom_smooth(aes(x=AD$PR_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0060ff",linetype="solid")+
geom_smooth(aes(x=AD$SS_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0080ff",linetype="solid")+
geom_smooth(aes(x=AD$BF_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c00000",linetype="solid")+
geom_smooth(aes(x=AD$FF_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c02000",linetype="solid")+
geom_smooth(aes(x=AD$RF_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c04000",linetype="solid")+
geom_smooth(aes(x=AD$OC_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c06000",linetype="solid")+
geom_smooth(aes(x=AD$ECM_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c000ff",linetype="dashed")+
geom_smooth(aes(x=AD$Path_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c040ff",linetype="dashed")+
geom_smooth(aes(x=AD$Sap_alpha,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c060ff",linetype="solid")+
theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Standardized alpha-diversity")+ylab("Ecosystem stability (ln)")   

B2<-ggplot()+
geom_smooth(aes(x=AD$AB_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0000ff",linetype="dashed")+
geom_smooth(aes(x=AD$PB_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0020ff",linetype="solid")+
geom_smooth(aes(x=AD$PF_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0040ff",linetype="dashed")+
geom_smooth(aes(x=AD$PR_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0060ff",linetype="dashed")+
geom_smooth(aes(x=AD$SS_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#0080ff",linetype="dashed")+
geom_smooth(aes(x=AD$BF_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c00000",linetype="solid")+
geom_smooth(aes(x=AD$FF_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c02000",linetype="dashed")+
geom_smooth(aes(x=AD$RF_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c04000",linetype="solid")+
geom_smooth(aes(x=AD$OC_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c06000",linetype="solid")+
geom_smooth(aes(x=AD$ECM_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c000ff",linetype="dashed")+
geom_smooth(aes(x=AD$Path_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c040ff",linetype="solid")+
geom_smooth(aes(x=AD$Sap_beta,y=log1p(AB$Stability)),method="lm",span=2,size=0.6,se=F,colour="#c060ff",linetype="dashed")+
theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Standardized beta-diversity")+ylab("Ecosystem stability (ln)")   

pdf("Fig_4a_GR_IndA_Stab.pdf", width =9, height =8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(2, 1))

dev.off()
##################################################################


##################################################################Fig_4b
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

df_AB <- data.frame(MAP=log1p(AB$MAP),MAT=log1p(AB$MAT),Soil_pH=log1p(AB$Soil_pH),SST=log(5+AB$SST),AB_A=log1p(AB$AB_alpha),PB_A=log1p(AB$PB_alpha),PF_A=log1p(AB$PF_alpha),PR_A=log1p(AB$PR_alpha),SS_A=log1p(AB$SS_alpha),BF_A=log1p(AB$BF_alpha),FF_A=log1p(AB$FF_alpha),RF_A=log1p(AB$RF_alpha),OC_A=log1p(AB$OC_alpha),ECM_A=log1p(AB$ECM_alpha),Path_A=log1p(AB$Path_alpha),Sap_A=log1p(AB$Sap_alpha),AB_B=log1p(AB$AB_beta),PB_B=log1p(AB$PB_beta),PF_B=log1p(AB$PF_beta),PR_B=log1p(AB$PR_beta),SS_B=log1p(AB$SS_beta),BF_B=log1p(AB$BF_beta),FF_B=log1p(AB$FF_beta),RF_B=log1p(AB$RF_beta),OC_B=log1p(AB$OC_beta),ECM_B=log1p(AB$ECM_beta),Path_B=log1p(AB$Path_beta),Sap_B=log1p(AB$Sap_beta)) #Subset dataset of diversity individual functional groups and environmental variables 

type <- c(c('AB_A','PB_A','PF_A','PR_A','SS_A','BF_A','FF_A','RF_A','OC_A','ECM_A','Path_A','Sap_A','AB_B','PB_B','PF_B','PR_B','SS_B','BF_B','FF_B','RF_B','OC_B','ECM_B','Path_B','Sap_B'))
attach(df_AB)

#MAP-residuals
result1 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,1])
  colnames(estimate1) <- type[i]
  result1 <- cbind(result1,estimate1)  
}
alpha <- data.frame(MAP=df_AB$MAP,result1[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

MAP_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
MAP_AB <- data.frame(Var1=row.names(MAP_AB),Var2=c('MAP'),MAP_AB)

#MAT-residuals
result2 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,2])
  colnames(estimate1) <- type[i]
  result2 <- cbind(result2,estimate1)  
}
alpha <- data.frame(MAT=df_AB$MAT,result2[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

MAT_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
MAT_AB <- data.frame(Var1=row.names(MAT_AB),Var2=c('MAT'),MAT_AB)

#Soil_pH-residuals
result3 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,3])
  colnames(estimate1) <- type[i]
  result3 <- cbind(result3,estimate1)  
}
alpha <- data.frame(PH=df_AB$Soil_pH,result3[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

Soil_pH_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
Soil_pH_AB <- data.frame(Var1=row.names(Soil_pH_AB),Var2=c('Soil_pH'),Soil_pH_AB)

#SST-residuals
result4 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,4])
  colnames(estimate1) <- type[i]
  result4 <- cbind(result4,estimate1)  
}
alpha <- data.frame(SST=df_AB$SST,result4[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

SST_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
SST_AB <- data.frame(Var1=row.names(SST_AB),Var2=c('SST'),SST_AB)

#combine data of MAP-MAT-PH-STT cor
cor <- data.frame(rbind(MAP_AB,MAT_AB,Soil_pH_AB,SST_AB))
cor$label <- ifelse(cor$p>0.05,NA,ifelse(cor$p<= 0.05&cor$p >= 0.01,"*",ifelse(cor$p <= 0.01&cor$p >= 0.001,"**","***")))

#Heatmap plot for partial correlation
cor$Var1 <- factor(cor$Var1,levels=c("Sap_B","Path_B","ECM_B","OC_B","RF_B","FF_B","BF_B","SS_B","PR_B","PF_B","PB_B","AB_B","Sap_A","Path_A","ECM_A","OC_A","RF_A","FF_A","BF_A","SS_A","PR_A","PF_A","PB_A","AB_A"),labels=c("Sap_B","Path_B","ECM_B","OC_B","RF_B","FF_B","BF_B","SS_B","PR_B","PF_B","PB_B","AB_B","Sap_A","Path_A","ECM_A","OC_A","RF_A","FF_A","BF_A","SS_A","PR_A","PF_A","PB_A","AB_A"))
cor$Var2 <- factor(cor$Var2,levels=c("MAP","MAT","Soil_pH","SST"),labels=c("MAP","MAT","Soil_pH","Soil_fertility"))

H1<-ggplot(cor, aes(Var2,Var1,  fill = r))+
  geom_tile(color = "grey")+
  scale_fill_gradient2(low = "#77C034", high = "#C388FE", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+ 
  geom_text(aes(label = label), color = "black", size = 4)+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())  

pdf("Fig_4b_Soil_biota_Ind_Heatmap.pdf", width =5, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(H1, vp = vplayout(1, 1))
dev.off()
##################################################################



##################################################################Fig_5a
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)
   
psem1 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lme(Plant_alpha ~ log1p(MAT),random = ~1|Site/Plot,AB),
  lm(Plant_beta ~ log1p(MAP)+log1p(MAT)+SST,AB),
  lme(SB_alpha ~ log1p(MAT)+log1p(MAP)+log1p(Soil_pH),random = ~1|Site/Plot,AB),
  lm(SB_beta ~  SST+log1p(MAP)+Plant_alpha,AB),
  lm(Stability ~ Plant_alpha+Plant_beta+log1p(MAP)++log1p(MAT)+SB_alpha+SB_beta,AB),
  log1p(MAT) %~~% log1p(MAP),
  SB_alpha %~~% SB_beta
)                     

summary(psem1,.progressBar =F)
##################################################################



##################################################################Fig_5b
psem2 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lm(Ba_alpha ~ log1p(MAT)+log1p(Soil_pH),AB),
  lme(Fu_alpha ~ SST,random = ~1|Site/Plot,AB),
  lme(Nema_alpha ~ log1p(MAP)+log1p(MAT)+log1p(Soil_pH)+Ba_alpha,random = ~1|Site/Plot,AB),
  log1p(MAT) %~~% log1p(MAP)
)

summary(psem2,.progressBar =F)
##################################################################



##################################################################Fig_5c
psem3 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lm(Ba_beta ~ log1p(MAT)+log1p(MAP)+log1p(Soil_pH),AB),
  lm(Fu_beta ~ log1p(MAP)+log1p(MAT)+SST,AB),
  lm(Nema_beta ~ +log1p(MAP)+Fu_beta,AB),
  log1p(MAT) %~~% log1p(MAP)
)

summary(psem3,.progressBar =F)
##################################################################



##################################################################Fig_5d
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

df_AB <- data.frame(MAP=log1p(AB$MAP),MAT=log1p(AB$MAT),Soil_pH=log1p(AB$Soil_pH),SST=log(5+AB$SST),
                 Plants_A=log1p(AB$Plant_alpha),Soil_biota_A=AB$SB_alpha,Bacteria_A=log1p(AB$Ba_alpha),Fungi_A=log1p(AB$Fu_alpha),Nematodes_A=log1p(AB$Nema_alpha),
                 Plants_B=log1p(AB$Plant_beta),Soil_biota_B=AB$SB_beta,Bacteria_B=log1p(AB$Ba_beta),Fungi_B=log1p(AB$Fu_beta),Nematodes_B=log1p(AB$Nema_beta),
                 Stability=log1p(AB$Stability),NDVI=log1p(AB$NDVI),SD=log1p(AB$SD))
type <- c(c('Plants_A','Soil_biota_A','Bacteria_A','Fungi_A','Nematodes_A','Plants_B','Soil_biota_B','Bacteria_B','Fungi_B','Nematodes_B','Stability','NDVI','SD'))

attach(df_AB)

#MAP-residuals
result1 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,1])
  colnames(estimate1) <- type[i]
  result1 <- cbind(result1,estimate1)  
}
alpha <- data.frame(MAP=df_AB$MAP,result1[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

MAP_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
MAP_AB <- data.frame(Var1=row.names(MAP_AB),Var2=c('MAP'),MAP_AB)

#MAT-residuals
result2 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,2])
  colnames(estimate1) <- type[i]
  result2 <- cbind(result2,estimate1)  
}
alpha <- data.frame(MAT=df_AB$MAT,result2[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

MAT_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
MAT_AB <- data.frame(Var1=row.names(MAT_AB),Var2=c('MAT'),MAT_AB)

#Soil_pH-residuals
result3 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,3])
  colnames(estimate1) <- type[i]
  result3 <- cbind(result3,estimate1)  
}
alpha <- data.frame(PH=df_AB$Soil_pH,result3[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

Soil_pH_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
Soil_pH_AB <- data.frame(Var1=row.names(Soil_pH_AB),Var2=c('Soil_pH'),Soil_pH_AB)

#SST-residuals
result4 <- c(1:132)
for(i in 1:length(type)){
  A <- df_AB[,type[i]]
  M1=lm(A~MAP+MAT+Soil_pH+SST)  
  M2<-residuals(M1, type = c("partial"))
  estimate1 <- data.frame(M2[,4])
  colnames(estimate1) <- type[i]
  result4 <- cbind(result4,estimate1)  
}
alpha <- data.frame(SST=df_AB$SST,result4[,-1])
cor <- Hmisc::rcorr(as.matrix(alpha))

SST_AB <- data.frame(r=cor$r[-1,1],p=cor$P[-1,1])
SST_AB <- data.frame(Var1=row.names(SST_AB),Var2=c('SST'),SST_AB)


#combine data 
cor <- data.frame(rbind(MAP_AB,MAT_AB,Soil_pH_AB,SST_AB))
cor$label <- ifelse(cor$p>0.05,NA,ifelse(cor$p<= 0.05&cor$p >= 0.01,"*",ifelse(cor$p <= 0.01&cor$p >= 0.001,"**","***")))

#Heatmap plot-partial correlation
cor$Var1 <- factor(cor$Var1,levels=c("Plants_A","Soil_biota_A","Bacteria_A","Fungi_A","Nematodes_A","Plants_B","Soil_biota_B","Bacteria_B","Fungi_B","Nematodes_B","Stability","NDVI","SD"),
                   labels=c("Plants_A","Soil_biota_A","Bacteria_A","Fungi_A","Nematodes_A","Plants_B","Soil_biota_B","Bacteria_B","Fungi_B","Nematodes_B","Stability","NDVI","SD"))
cor$Var2 <- factor(cor$Var2,levels=c("SST","Soil_pH","MAT","MAP"),labels=c("Soil_fertility","Soil_pH","MAT","MAP"))


H1<-ggplot(cor, aes(Var1, Var2, fill = r))+
  geom_tile(color = "grey")+
  scale_fill_gradient2(low = "#77C034", high = "#C388FE", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  geom_text(aes(label = label), color = "black", size = 4)+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())

pdf("Fig_5d_Plant_Biota_Heatmap.pdf", width =6, height =3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(H1, vp = vplayout(1, 1))
dev.off()
##################################################################



##################################################################Fig_S1
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

AC<-AB %>% distinct(Site, .keep_all = TRUE)

#MAP
MAP1<-data.frame(AC$Site,AC$MAP,AC$MAP_2010,AC$MAP_2011)
MAP2<-MAP1 %>% gather(Yr, MAP_value, AC.MAP,AC.MAP_2010,AC.MAP_2011)
D1<-ggplot(MAP2,aes(x=Yr, y=MAP_value,color=Yr,fill=Yr))+ geom_boxplot(notch=TRUE,position=position_dodge(0.6),size=0.5,width=0.5)+geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+scale_color_manual(values=c("red","red","red"))+scale_fill_manual(values=c("#8DC2F7","#8DC2F7","#8DC2F7"))+theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))
#MAT
MAT1<-data.frame(AC$Site,AC$MAT,AC$MAT_2010,AC$MAT_2011)
MAT2<-MAT1 %>% gather(Yr, MAT_value, AC.MAT,AC.MAT_2010,AC.MAT_2011)
D2<-ggplot(MAT2,aes(x=Yr, y=MAT_value,color=Yr,fill=Yr))+ geom_boxplot(notch=TRUE,position=position_dodge(0.6),size=0.5,width=0.5)+geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+scale_color_manual(values=c("red","red","red"))+scale_fill_manual(values=c("#8DC2F7","#8DC2F7","#8DC2F7"))+theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))
#NDVI
NDVI1<-data.frame(AC$Site,AC$NDVI,AC$NDVI_2010,AC$NDVI_2011)
NDVI2<-NDVI1 %>% gather(Yr, NDVI_value,AC.NDVI,AC.NDVI_2010,AC.NDVI_2011)
D3<-ggplot(NDVI2,aes(x=Yr, y=NDVI_value,color=Yr,fill=Yr))+ geom_boxplot(notch=TRUE,position=position_dodge(0.6),size=0.5,width=0.5)+geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+scale_color_manual(values=c("red","red","red"))+scale_fill_manual(values=c("#8DC2F7","#8DC2F7","#8DC2F7"))+theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))

pdf("Fig_S1_MAP_MAT_NDVI.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(D1, vp = vplayout(1, 1))
print(D2, vp = vplayout(1, 2))
print(D3, vp = vplayout(1, 3))
dev.off()

####Statistics of comparation for Figure_S1 
#MAP
amod<-lmer(MAP_value~Yr+(1|AC.Site),MAP2)
tuk <-glht(amod,linfct=mcp(Yr="Tukey"))
tuk.cld <-cld(tuk)
old.par <-par(mai=c(0.5,0.5,1.5,0.5), no.readonly=TRUE)
plot(tuk.cld)
par(old.par)

#MAT
amod<-lmer(MAT_value~Yr+(1|AC.Site),MAT2)
tuk <-glht(amod,linfct=mcp(Yr="Tukey"))
tuk.cld <-cld(tuk)
old.par <-par(mai=c(0.5,0.5,1.5,0.5), no.readonly=TRUE)
plot(tuk.cld)
par(old.par)

#NDVI
amod<-lmer(NDVI_value~Yr+(1|AC.Site),NDVI2)
tuk <-glht(amod,linfct=mcp(Yr="Tukey"))
tuk.cld <-cld(tuk)
old.par <-par(mai=c(0.5,0.5,1.5,0.5), no.readonly=TRUE)
plot(tuk.cld)
par(old.par)
##################################################################



##################################################################Figure_S2
B1<-ggplot(AB,aes(x=log1p(Plant_alpha),y=log1p(Plant_gama)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Plant_alpha),y=log1p(Plant_gama)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant alpha-diversity (ln)")+ylab("Plant gama-diversity (ln)")  
B2<-ggplot(AB,aes(x=SB_alpha,y=SB_gama))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=SB_alpha,y=SB_gama),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil biota alpha-diversity (z-score)")+ylab("Soil biota gama-diversity (z-score)")  
B3<-ggplot(AB,aes(x=log1p(Ba_alpha),y=log1p(Ba_gama)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_alpha),y=log1p(Ba_gama)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial alpha-diversity (ln)")+ylab("Bacterial gama-diversity (ln)")  
B4<-ggplot(AB,aes(x=log1p(Fu_alpha),y=log1p(Fu_gama)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_alpha),y=log1p(Fu_gama)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal alpha-diversity (ln)")+ylab("Fungal gama-diversity (ln)")
B5<-ggplot(AB,aes(x=log1p(Nema_alpha),y=log1p(Nema_gama)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_alpha),y=log1p(Nema_gama)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal alpha-diversity (ln)")+ylab("Fungal gama-diversity (ln)")  

pdf("Fig_S2_Relationships_Alpha_Gama.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(2, 1))
print(B4, vp = vplayout(2, 2))
print(B5, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_S2
M1<-lm(log1p(Plant_alpha)~log1p(Plant_gama),AB)
summary(M1)
M2<-lm(SB_alpha~SB_gama,AB)
summary(M2)
M3<-lm(log1p(Ba_alpha)~log1p(Ba_gama),AB)
summary(M3)
M4<-lm(log1p(Fu_alpha)~log1p(Fu_gama),AB)
summary(M4)
M5<-lm(log1p(Nema_alpha)~log1p(Nema_gama),AB)
summary(M5)
##################################################################



##################################################################Figure_S3 
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

B1<-ggplot(AB,aes(x=NDVI_2010_2011,y=ANPP_2010_2011))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=NDVI_2010_2011,y=ANPP_2010_2011),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Mean NDVI")+ylab("ANPP (g m-2)")    

pdf("Fig_S3_Relationship_NDVI_ANPP.pdf", width =18, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
dev.off()

###Statistics (r and P values) for Figure_S3
M1<-lm(NDVI_2010_2011~ANPP_2010_2011,AB)
summary(M1)
##################################################################



##################################################################Figure_S4
B1<-ggplot(AB,aes(x=log1p(AB_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(AB_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Annuals and biennials (ln)")+ylab("Ecosystem stability (ln)")  
B2<-ggplot(AB,aes(x=log1p(PB_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PB_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial bunchgrasses (ln)")+ylab("Ecosystem stability (ln)") 
B3<-ggplot(AB,aes(x=log1p(PF_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PF_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial forbs (ln)")+ylab("Ecosystem stability (ln)") 
B4<-ggplot(AB,aes(x=log1p(PR_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PR_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial rhizome grasses (ln)")+ylab("Ecosystem stability (ln)") 
B5<-ggplot(AB,aes(x=log1p(SS_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(SS_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Shrubs and sub-shrubs (ln)")+ylab("Ecosystem stability (ln)") 
B6<-ggplot(AB,aes(x=log1p(ECM_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(ECM_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Mycorrhizal fungi (ln)")+ylab("Ecosystem stability (ln)") 
B7<-ggplot(AB,aes(x=log1p(Path_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Path_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant-pathogenic fungi (ln)")+ylab("Ecosystem stability (ln)") 
B8<-ggplot(AB,aes(x=log1p(Sap_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Sap_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil saprobic fungi (ln)")+ylab("Ecosystem stability (ln)") 
B9<-ggplot(AB,aes(x=log1p(BF_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(BF_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B10<-ggplot(AB,aes(x=log1p(FF_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(FF_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B11<-ggplot(AB,aes(x=log1p(RF_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(RF_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B12<-ggplot(AB,aes(x=log1p(OC_alpha),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(OC_alpha),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Omnivores + carnivores (ln)")+ylab("Ecosystem stability (ln)") 

pdf("Fig_S4_Soil_Biota_Ind_alpha_Stab.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(1, 4))
print(B5, vp = vplayout(2, 1))
print(B6, vp = vplayout(2, 2))
print(B7, vp = vplayout(2, 3))
print(B8, vp = vplayout(2, 4))
print(B9, vp = vplayout(3, 1))
print(B10, vp = vplayout(3, 2))
print(B11, vp = vplayout(3, 3))
print(B12, vp = vplayout(3, 4))
dev.off()

###Statistics (r and P values) for Figure_S4
M1<-lm(log1p(AB_alpha)~log1p(Stability),AB)
summary(M1)
M2<-lm(log1p(PB_alpha)~log1p(Stability),AB)
summary(M2)
M3<-lm(log1p(PF_alpha)~log1p(Stability),AB)
summary(M3)
M4<-lm(log1p(PR_alpha)~log1p(Stability),AB)
summary(M4)
M5<-lm(log1p(SS_alpha)~log1p(Stability),AB)
summary(M5)
M6<-lm(log1p(ECM_alpha)~log1p(Stability),AB)
summary(M6)
M7<-lm(log1p(Path_alpha)~log1p(Stability),AB)
summary(M7)
M8<-lm(log1p(Sap_alpha)~log1p(Stability),AB)
summary(M8)
M9<-lm(log1p(BF_alpha)~log1p(Stability),AB)
summary(M9)
M10<-lm(log1p(FF_alpha)~log1p(Stability),AB)
summary(M10)
M11<-lm(log1p(RF_alpha)~log1p(Stability),AB)
summary(M11)
M12<-lm(log1p(OC_alpha)~log1p(Stability),AB)
summary(M12)
##################################################################



##################################################################Figure_S5
B1<-ggplot(AB,aes(x=log1p(AB_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(AB_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Annuals and biennials (ln)")+ylab("Ecosystem stability (ln)")  
B2<-ggplot(AB,aes(x=log1p(PB_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PB_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial bunchgrasses (ln)")+ylab("Ecosystem stability (ln)") 
B3<-ggplot(AB,aes(x=log1p(PF_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PF_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial forbs (ln)")+ylab("Ecosystem stability (ln)") 
B4<-ggplot(AB,aes(x=log1p(PR_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(PR_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Perennial rhizome grasses (ln)")+ylab("Ecosystem stability (ln)") 
B5<-ggplot(AB,aes(x=log1p(SS_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(SS_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Shrubs and sub-shrubs (ln)")+ylab("Ecosystem stability (ln)") 
B6<-ggplot(AB,aes(x=log1p(ECM_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(ECM_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Mycorrhizal fungi (ln)")+ylab("Ecosystem stability (ln)") 
B7<-ggplot(AB,aes(x=log1p(Path_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Path_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant-pathogenic fungi (ln)")+ylab("Ecosystem stability (ln)") 
B8<-ggplot(AB,aes(x=log1p(Sap_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Sap_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Soil saprobic fungi (ln)")+ylab("Ecosystem stability (ln)") 
B9<-ggplot(AB,aes(x=log1p(BF_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(BF_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B10<-ggplot(AB,aes(x=log1p(FF_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(FF_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B11<-ggplot(AB,aes(x=log1p(RF_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(RF_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Plant-feeders (ln)")+ylab("Ecosystem stability (ln)") 
B12<-ggplot(AB,aes(x=log1p(OC_beta),y=log1p(Stability)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(OC_beta),y=log1p(Stability)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Omnivores + carnivores (ln)")+ylab("Ecosystem stability (ln)") 

pdf("Fig_S5_Soil_Biota_Ind_beta_Stab.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(1, 4))
print(B5, vp = vplayout(2, 1))
print(B6, vp = vplayout(2, 2))
print(B7, vp = vplayout(2, 3))
print(B8, vp = vplayout(2, 4))
print(B9, vp = vplayout(3, 1))
print(B10, vp = vplayout(3, 2))
print(B11, vp = vplayout(3, 3))
print(B12, vp = vplayout(3, 4))
dev.off()

####Statistics (r and P values) for Figure_S5
M1<-lm(log1p(AB_beta)~log1p(Stability),AB)
summary(M1)
M2<-lm(log1p(PB_beta)~log1p(Stability),AB)
summary(M2)
M3<-lm(log1p(PF_beta)~log1p(Stability),AB)
summary(M3)
M4<-lm(log1p(PR_beta)~log1p(Stability),AB)
summary(M4)
M5<-lm(log1p(SS_beta)~log1p(Stability),AB)
summary(M5)
M6<-lm(log1p(ECM_beta)~log1p(Stability),AB)
summary(M6)
M7<-lm(log1p(Path_beta)~log1p(Stability),AB)
summary(M7)
M8<-lm(log1p(Sap_beta)~log1p(Stability),AB)
summary(M8)
M9<-lm(log1p(BF_beta)~log1p(Stability),AB)
summary(M9)
M10<-lm(log1p(FF_beta)~log1p(Stability),AB)
summary(M10)
M11<-lm(log1p(RF_beta)~log1p(Stability),AB)
summary(M11)
M12<-lm(log1p(OC_beta)~log1p(Stability),AB)
summary(M12)
##################################################################



##################################################################Figure_S6
B1<-ggplot(AB,aes(x=log1p(Ba_alpha),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_alpha),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial alpha-diversity (ln)")+ylab("Mean NDVI (ln)")   
B2<-ggplot(AB,aes(x=log1p(Fu_alpha),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_alpha),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal alpha-diversity (ln)")+ylab("Mean NDVI (ln)")   
B3<-ggplot(AB,aes(x=log1p(Nema_alpha),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_alpha),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode alpha-diversity (ln)")+ylab("Mean NDVI (ln)")  
B4<-ggplot(AB,aes(x=log1p(Ba_alpha),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_alpha),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial alpha-diversity (ln)")+ylab("SD of NDVI (ln)")   
B5<-ggplot(AB,aes(x=log1p(Fu_alpha),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_alpha),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal alpha-diversity (ln)")+ylab("SD of NDVI (ln)")    
B6<-ggplot(AB,aes(x=log1p(Nema_alpha),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_alpha),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode alpha-diversity (ln)")+ylab("SD of NDVI (ln)")    

pdf("Fig_S6_Soil_Biota_FG_alpha_NDVI.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(2, 1))
print(B5, vp = vplayout(2, 2))
print(B6, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_S6
M1<-lm(log1p(Ba_alpha)~log1p(NDVI),AB)
summary(M1)
M2<-lm(log1p(Fu_alpha)~log1p(NDVI),AB)
summary(M2)
M3<-lm(log1p(Nema_alpha)~log1p(NDVI),AB)
summary(M3)
M4<-lm(log1p(Ba_alpha)~log1p(SD),AB)
summary(M4)
M5<-lm(log1p(Fu_alpha)~log1p(SD),AB)
summary(M5)
M6<-lm(log1p(Nema_alpha)~log1p(SD),AB)
summary(M6)
##################################################################


 
##################################################################Figure_S7
B1<-ggplot(AB,aes(x=log1p(Ba_beta),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_beta),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial beta-diversity (ln)")+ylab("Mean NDVI (ln)")   
B2<-ggplot(AB,aes(x=log1p(Fu_beta),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_beta),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal beta-diversity (ln)")+ylab("Mean NDVI (ln)")   
B3<-ggplot(AB,aes(x=log1p(Nema_beta),y=log1p(NDVI)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_beta),y=log1p(NDVI)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode beta-diversity (ln)")+ylab("Mean NDVI (ln)")  
B4<-ggplot(AB,aes(x=log1p(Ba_beta),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Ba_beta),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Bacterial beta-diversity (ln)")+ylab("SD of NDVI (ln)")   
B5<-ggplot(AB,aes(x=log1p(Fu_beta),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Fu_beta),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Fungal beta-diversity (ln)")+ylab("SD of NDVI (ln)")    
B6<-ggplot(AB,aes(x=log1p(Nema_beta),y=log1p(SD)))+geom_point(size=0.8,alpha=1,color="#8DC2F7")+geom_smooth(aes(x=log1p(Nema_beta),y=log1p(SD)),method="lm",size=0.6,alpha=0.6,color="#8DC2F7",fill="#8DC2F7")+scale_color_manual(values=c("#8DC2F7"))+scale_fill_manual(values=c("#8DC2F7"))+theme_set(theme_bw())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+ theme(panel.border = element_rect(fill=NA,color="black", size=0.1, linetype="solid"))+theme(axis.text.y=element_text(angle =90,vjust =0.5,hjust=0.5))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.y=element_line(color="black",size=0.1,lineend = 1))+theme(axis.ticks.x=element_line(color="black",size=0.1,lineend = 1))+xlab("Nematode beta-diversity (ln)")+ylab("SD of NDVI (ln)")    

pdf("Fig_S7_Soil_Biota_FG_beta_NDVI.pdf", width =13, height =10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 6)))
vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)
print(B1, vp = vplayout(1, 1))
print(B2, vp = vplayout(1, 2))
print(B3, vp = vplayout(1, 3))
print(B4, vp = vplayout(2, 1))
print(B5, vp = vplayout(2, 2))
print(B6, vp = vplayout(2, 3))
dev.off()

###Statistics (r and P values) for Figure_S7
M1<-lm(log1p(Ba_beta)~log1p(NDVI),AB)
summary(M1)
M2<-lm(log1p(Fu_beta)~log1p(NDVI),AB)
summary(M2)
M3<-lm(log1p(Nema_beta)~log1p(NDVI),AB)
summary(M3)
M4<-lm(log1p(Ba_beta)~log1p(SD),AB)
summary(M4)
M5<-lm(log1p(Fu_beta)~log1p(SD),AB)
summary(M5)
M6<-lm(log1p(Nema_beta)~log1p(SD),AB)
summary(M6)
##################################################################



##################################################################Fig_S8a
AB<-read.table("Wu_et_al_Dataset_2023.txt",header=T)
attach(AB)
names(AB)
head(AB)

psem4 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lme(Plant_alpha ~ log1p(MAT),random = ~1|Site/Plot,AB),
  lme(Fu_alpha ~ SST+Plant_alpha,random = ~1|Site/Plot,AB),
  lme(Nema_alpha ~ log1p(MAP)+log1p(MAT)+log1p(Soil_pH)+Ba_alpha,random = ~1|Site/Plot,AB),
  lm(Stability ~ log1p(MAT)+log1p(MAP)+Plant_alpha+Fu_alpha,AB),
  log1p(MAT) %~~% log1p(MAP)
)

summary(psem4,.progressBar =F)
##################################################################



##################################################################Fig_S8b
psem5 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lm(Plant_beta ~ log1p(MAP)+log1p(MAT)+SST,AB),
  lm(Ba_beta ~ log1p(MAT)+log1p(Soil_pH)+Plant_beta,AB),
  lm(Fu_beta ~ log1p(MAP)+log1p(MAT)+SST,AB),
  lm(Nema_beta ~ log1p(MAP)+Fu_beta+Plant_beta+SST,AB),
  lm(Stability ~ log1p(MAT)+log1p(MAP)+Nema_beta,AB),
  log1p(MAT) %~~% log1p(MAP)
)

summary(psem5,.progressBar =F)
##################################################################



##################################################################Fig_S9 
psem6 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lm(Stability ~ log1p(MAT)+log1p(MAP),AB),  
  lme(Plant_alpha ~ log1p(MAT),random = ~1|Site/Plot,AB),
  lm(Plant_beta ~ log1p(MAP)+SST+Stability,AB),
  lme(SB_alpha ~ log1p(MAP)+log1p(MAT)+log1p(Soil_pH),random = ~1|Site/Plot,AB),
  lm(SB_beta ~ log1p(MAP)+Plant_alpha+SST,AB),
  log1p(MAT) %~~% log1p(MAP),
  SB_alpha %~~% SB_beta
)

summary(psem6,.progressBar =F)
##################################################################



##################################################################Table S1, Chi-square difference test between Fig_5a and Fig_S9 
anova(psem1,psem6) 

##################################################################Fig_S10
psem7 <- psem(
  lme(log1p(Soil_pH) ~ log1p(MAT),random = ~ 1|Site/Plot,AB), 
  lme(SST ~ log1p(MAT)+log1p(MAP),random = ~1|Site/Plot,AB),
  lme(Plant_alpha ~ log1p(MAT),random = ~1|Site/Plot,AB),
  lm(Plant_beta ~ log1p(MAT)+log1p(MAP)+SST,AB),
  lme(SB_alpha ~ log1p(MAT)+log1p(MAP)+log1p(Soil_pH),random = ~1|Site/Plot,AB),
  lm(SB_beta ~ log1p(MAP)+Plant_alpha+SST,AB),
  lm(NDVI ~ log1p(MAT)+log1p(MAP)+SST+log1p(Soil_pH)+Plant_alpha+Plant_beta,AB),
  lm(SD ~ log1p(MAP)+SST+SB_alpha+SB_beta,AB),
  log1p(MAT) %~~% log1p(MAP),
  SB_alpha %~~% SB_beta,
  NDVI %~~% SD
)

summary(psem7,.progressBar =F)
#################################################################




