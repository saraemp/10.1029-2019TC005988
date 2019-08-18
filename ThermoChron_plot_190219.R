cooling_ages_range=read.table("/Users/smoron/Documents/1.Projects/Thermochron/Pilbara_project_cooling_ages_rangeSM_202019.txt", header=T);names(cooling_ages_range)
SystemPeriod=read.csv("/Users/smoron/Documents/Data/System_period_boundaries.csv"); attach(SystemPeriod)

a=1150#time
a=System_period_value[System_period_name=="Cambrian"]
b=a/2*-1# lateral extent
tma=.03
tmi=.01
start= 1000
end_t= 45
es=c(700,450,310,305,300,148)
ee=c(500,400,290,285,270,135.4)
ylim_c=c(0,10)
xlim_p=c(System_period_value[System_period_name=="Cambrian"]*-1,System_period_value[System_period_name=="Permian"]*-1)
z=255
pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables/Pilbara project_cooling_ages_rangeSM_190219.pdf", width=19/2.54, height=10/2.54)
layout(matrix(c(1,2,2,3,3,3,3), nrow=7, ncol=1, byrow = TRUE))
#par(mfrow=c(2,1)) 
par(mar=c(0,2,0,0))
#plot(seq(-119,0,1),seq(end_t,a/7,1),  type="n", xlim=xlim_p,ylim=ylim_c,yaxt='n', xaxt='n',ann=F, axes=F,frame.plot=F)#Frame
plot(seq(0,100,1),seq(0,100,1),  type="n", xlim=xlim_p,ylim=ylim_c,yaxt='n', xaxt='n',ann=F, axes=F,frame.plot=F)#Frame, the values for x and y don't matter, xlim and y ylim is what matters  

for(i in which(System_period_name=="Permian"):which(System_period_name=="Ordovician")){
  rect(System_period_value[i]*-1, 0, System_period_value[i+1]*-1,10,col=rgb(rsp[i], gsp[i], bsp[i],max=z))
}
#System_period labels
for(i in which(System_period_name=="Permian"):which(System_period_name=="Ordovician")){
  text(((System_period_value [i+1]+System_period_value [i])/2)*-1,5,System_period_name[i],  cex=1)
}
text(((System_period_value [2]+System_period_value [1])/2)*-1,5, System_period_name[1], cex=1)
###Events###
y=seq(0,5)
c(0,5)
par(mar=c(0,2,0,0))
plot(seq(0,100,1),seq(0,100,1),  type="n", xlim=xlim_p,ylim=c(0,5),yaxt='n', xaxt='n',ann=F, axes=F,frame.plot=F)#Frame
es=c(700,450,310,305,300,148)
ee=c(500,400,290,285,270,135.4)

for (i in 1:length(es)){
  rect( es[i]*-1,  y[i],ee[i]*-1,y[i+1], col=rgb(0, 0, 0,alpha=0.5), border=rgb(0, 0, 0,alpha=0.1))
}
srt=0
text( (700+500)/-2,0.5,  "Gondwana Assembly (Cawood et al., 2013)", cex=1, srt=0)
text( (450+400)/-2, 1.5, "ASO I (Glorie et al., 2017)", cex=1,  srt=srt)
text( (310+290)/-2, 2.5, "ASO II (Glorie et al., 2017)", cex=1,  srt=srt)
text( (305+285)/-2, 3.5, "Glaciation (Mory et al., 2008)", cex=1,  srt=srt)
text( (300+270)/-2, 4.5, "Cimmerian rift (Yeh&Shellnutt, 2016)", cex=1,  srt=srt)
#text( 201, 5, "Fitzroy Movement Al-Hinaai&Redfern, 2015", cex=.5 , srt=srt)
#text( (148+135.4 )/2, 6, "Gondwana break-up (Paumard et al, 2018)", cex=.5,  srt=srt)
###Onset model ages
par(mar=c(2,2,0,0))
plot(cooling_ages_range$Ma_max*-1, cooling_ages_range$Lat, type="n", xlim=xlim_p, ylim=c(-24,-20), xlab="Onset modelled colling age  (Ma)", ylab="Latitude", mgp=c(.8, 0.1, 0), tck=tma)
axis(1,seq(-490, -250, 10), labels=F, tck=tmi)
segments(cooling_ages_range$Ma_max*-1,cooling_ages_range$Lat, (cooling_ages_range$Ma_max*-1)-(cooling_ages_range$Ma_max_err*-1),cooling_ages_range$Lat, col='grey',lty = 2)
points(cooling_ages_range$Ma_max*-1, cooling_ages_range$Lat, pch=16, cex=2)
text((cooling_ages_range$Ma_max*-1)-5,cooling_ages_range$Lat, cooling_ages_range$SampleID, cex =.9, adj=1 )
dev.off()


####Pooled ages
par(mar=c(2,2,0,0))
plot(cooling_ages_range$Pooled_age,cooling_ages_range$Lat,xlim=c(0,600), pch=16,  xlab="Pooled ages (Ma)", ylab="Latitude", mgp=c(.8, 0.1, 0), tck=tma)
arrows(cooling_ages_range$Pooled_age-cooling_ages_range$Pooled_age_1s,cooling_ages_range$Lat, cooling_ages_range$Pooled_age+cooling_ages_range$Pooled_age_1s,cooling_ages_range$Lat,length=0.02, angle=90, code=3)
abline(v=c(300,400), col="grey")