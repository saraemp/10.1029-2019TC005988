AHe=read.table("/Users/smoron/Documents/1.Projects/Thermochron/AHe_analyses_Pilbara_March_April2018.csv", header=T)
ZHe=read.table("/Users/smoron/Documents/1.Projects/Thermochron/ZrHe_data_Pilbara_project_Feb2019.csv", sep=",",header=T)
SystemPeriod=read.csv("/Users/smoron/Documents/Data/System_period_boundaries.csv"); attach(SystemPeriod)
UPb_loss=read.csv("/Users/smoron/Documents/1.Projects/Thermochron/GA_data_trial_UPb_loss.csv")
#Comment 
#2=no re-extract
#3=small re-extract then to zero gas
#4=normal
#Formating 
tma=.03
tmi=.01

#####
#AHe#
#####
gm=levels(AHe$Grain_morphology)
samples=levels(AHe$Sample_No)
xlim= c(min(AHe$eU_ppm), max(AHe$eU_ppm))
ylim=c(min(AHe$Corrected_Age_Ma), max(AHe$Corrected_Age_Ma) +50)

pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs/AHevseU.pdf", width=15/2.54, height=6/2.54)
par(mfrow=c(1, 2))
par(mar=c(2,2,0,0))
plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (i in 1:length(gm)){
  arrows(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
}

symbols=seq(21, 23,1)
legend("topright",legend=c("0T", "1T", "2T"), pch=symbols )

par(mar=c(2,1,0,0))
plot(AHe$Rs_um[AHe$Grain_morphology==gm[1]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", ylim=ylim, xlab="Rs (um)", ylab="", yaxt="n", mgp=c(.8, 0.1, 0), tck=tma)
axis(2, seq(0,1e3,200), labels=F,tck=tma,mgp=c(.8, 0.1, 0), cex.lab=0.6)
for (i in 1:length(gm)){
  arrows(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
}
symbols=seq(21, 23,1)
legend("topright",legend=c("0T", "1T", "2T"), pch=symbols )

dev.off()


#####
#ZHe#
#####
sn=levels(ZHe$SampleID)
xlimZHe= c(min(ZHe$eU_ppm), max(ZHe$eU_ppm))
xl=min(ZHe$eU_ppm)
xu=max(ZHe$eU_ppm)
ylimZHe=c(min(ZHe$Age_Ma),max(ZHe$Age_Ma)+30)
pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables/ZHevseU.pdf", width=9.5/2.54, height=9.5/2.54)
par(mfrow=c(1, 1))
par(mar=c(2,2,0,0))
plot(ZHe$eU_ppm[ZHe$SampleID==sn[2]], ZHe$Age_Ma[ZHe$SampleID==sn[2]], type="n",  xlim=xlimZHe, ylim=ylimZHe, xlab="eU (ppm)", ylab="ZHe Age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (i in 1:length(sn)){
  arrows(ZHe$eU_ppm[ZHe$SampleID==sn[i]], ZHe$Age_Ma[ZHe$SampleID==sn[i]]-ZHe$Age_Ma_1s[ZHe$SampleID==sn[i]],ZHe$eU_ppm[ZHe$SampleID==sn[i]],  ZHe$Age_Ma[ZHe$SampleID==sn[i]]+ZHe$Age_Ma_1s[ZHe$SampleID==sn[i]],length=0.02, angle=90, code=3)
  points(ZHe$eU_ppm[ZHe$SampleID==sn[i]], ZHe$Age_Ma[ZHe$SampleID==sn[i]], pch=20+i, bg="white")
}
symbols=seq(21, 24,1)
legend("topright",legend=sn, pch=symbols )
dev.off()



#Different figure
#Time table
#Plot dimensions
a=1150#time
b=a/2*-1# lateral extent
c=100#lateral extent of age box
g=10#origin
#Age ticks
d=5#controls tick width 
e=-10#tick frequency 10Ma
f=-50#label text age 
h=50#period labels x axis
j=15#series width
k=10#series label x axis
l=28
m=21.5
n=30
o=33
p=-.5#width little ticks
start= 1000
end_t= 45
es=c(700,450,310,305,300,148)
ee=c(500,400,290,285,270,135.4)

pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs/AHe&ZHe_events.pdf", width=19/2.54, height=23/2.54)
layout(matrix(c(1,2,2,3,3), nrow=1, ncol=5, byrow = TRUE))
par(mar=c(2,2,0,0))
plot(seq(0,119,1),seq(end_t,a/7,1),  type="n", xlim=c(0,100),ylim=ylim_c,yaxt='n', xaxt='n',ann=F, axes=F,frame.plot=F)#Frame
segments(10, seq(a,end_t,e),d,  seq(a,end_t,e), lwd=.5)#Add minor ticks if needded
segments(10, seq(a,end_t,e),d,  seq(a,end_t,e), lwd=.5)#ticks @ 2Ma
text(0, seq(a,end_t,f), c(paste (seq(a,end_t,f))), cex=0.5)#age tick labels 10Ma
#Age divisions
#System_period boxes
for(i in which(System_period_name=="Paleogene"):which(System_period_name=="Stenian")){
  rect(g,System_period_value[i], c, System_period_value[i+1],col=rgb(rsp[i], gsp[i], bsp[i],max=z))
}
#System_period labels
for(i in which(System_period_name=="Paleogene"):which(System_period_name=="Stenian")){
  text(h,((System_period_value [i+1]+System_period_value [i])/2),System_period_name[i],  cex=.5)
}
text(h,((System_period_value [2]+System_period_value [1])/2)*-1,System_period_name[1], cex=.5)

ylim_c=c(min(ZHe$Age_Ma), max(AHe$Corrected_Age_Ma)+50)

par(mar=c(2,2,0,0))
plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim_c, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (i in 1:length(gm)){
  arrows(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
}
for (i in 1:length(es)){
  rect( min(AHe$eU_ppm), es[i], max(AHe$eU_ppm),ee[i], col=rgb(0, i/10, 0,alpha=0.5), border=rgb(0, 0, 0,alpha=0.1))
}

abline(h=201.3)
text(150, (700+500)/2, "Gondwana Assembly (Cawood et al., 2013)", cex=.5)
text(150, (450+400)/2, "ASO I (Glorie et al., 2017)", cex=.5)
text(150, (310+290)/2, "ASO II (Glorie et al., 2017)", cex=.5)
text(150, (305+285)/2, "Glaciation (Mory et al., 2008)", cex=.5)
text(150, (300+270)/2, "Cimmerian rift (Yeh&Shellnutt, 2016)", cex=.5)
text(150, 201, "Fitzroy Movement Al-Hinaai&Redfern, 2015", cex=.5)
text(150, (148+135.4 )/2, "Gondwana break-up (Paumard et al, 2018)", cex=.5)
plot(ZHe$eU_ppm[ZHe$SampleID==sn[2]], ZHe$Age_Ma[ZHe$SampleID==sn[2]], type="n",  xlim=xlimZHe, ylim=ylim_c, xlab="eU (ppm)", ylab="ZHe Age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (i in 1:length(sn)){
  arrows(ZHe$eU_ppm[ZHe$SampleID==sn[i]], ZHe$Age_Ma[ZHe$SampleID==sn[i]]-ZHe$Age_Ma_1s[ZHe$SampleID==sn[i]],ZHe$eU_ppm[ZHe$SampleID==sn[i]],  ZHe$Age_Ma[ZHe$SampleID==sn[i]]+ZHe$Age_Ma_1s[ZHe$SampleID==sn[i]],length=0.02, angle=90, code=3)
  points(ZHe$eU_ppm[ZHe$SampleID==sn[i]], ZHe$Age_Ma[ZHe$SampleID==sn[i]], pch=20+i, bg="white")
}


for (i in 1:length(es)){
  rect( xl, es[i], xu,ee[i], col=rgb(0, i/10, 0,alpha=0.5), border=rgb(0, 0, 0,alpha=0.1))
}

abline(h=201.3)
text(1000, (700+500)/2, "Gondwana Assembly (Cawood et al., 2013)", cex=.5)
text(1000, (450+400)/2, "ASO I (Glorie et al., 2017)", cex=.5)
text(1000, (310+290)/2, "ASO II (Glorie et al., 2017)", cex=.5)
text(1000, (305+285)/2, "Glaciation (Mory et al., 2008)", cex=.5)
text(1000, (300+270)/2, "Cimmerian rift (Yeh&Shellnutt, 2016)", cex=.5)
text(1000, 201, "Fitzroy Movement Al-Hinaai&Redfern, 2015", cex=.5)
text(1000, (148+135.4 )/2, "Gondwana break-up (Paumard et al, 2018)", cex=.5)
dev.off()


#Previous versions
#abline(h=c(700,500))
#abline(h=c(450,400, 310, 290))
#abline(h=c(305,285))
#abline(h=c(300,270))
#abline(h=c(148, 135.4 ))




#U-Pb plots for U-Pb loss
#207Pb/235U vs 206Pb/238U
par(mfrow=c(1, 1))
par(mar=c(3,3,0,0))
plot( UPb_loss$X204cor.207Pb..235U, UPb_loss$X204cor.206Pb..238U)
plot(UPb_loss$Total.238U..206Pb, UPb_loss$Total.207Pb..206Pb, mgp=c(.8, 0.1, 0), tck=tma, xlab="238U/206Pb", ylab="207Pb/206Pb")




###

ZHe_Mungaroo=read.table("/Users/smoron/Documents/1.Projects/Thermochron/ZrHe_SaraMoron_Mungaroo_January2019.csv", sep=",",header=T)
ZHe_Mungaroo


sn=levels(ZHe_Mungaroo$SampleID);sn
xlimZHe= c(min(ZHe_Mungaroo$eU_ppm), max(ZHe_Mungaroo$eU_ppm))
xl=min(ZHe_Mungaroo$eU_ppm)
xu=max(ZHe_Mungaroo$eU_ppm)
ylimZHe=c(min(ZHe_Mungaroo$Age_Ma),max(ZHe_Mungaroo$Age_Ma)+20)
plot(ZHe_Mungaroo$eU_ppm[ZHe_Mungaroo$SampleID==sn[2]], ZHe_Mungaroo$Age_Ma[ZHe_Mungaroo$SampleID==sn[2]], type="n",  xlim=xlimZHe, ylim=ylimZHe, xlab="eU (ppm)", ylab="ZHe Age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
