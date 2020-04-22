library(plotrix)
AHe=read.table("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables_sub/Table_3.csv", header=T, sep=",")
names(AHe)
head(AHe)
tma=.03
tmi=.01

#####
#AHe#
#####
gm=levels(AHe$Grain_morphology);gm
samples=levels(AHe$Sample_No);samples
xlim= c(min(AHe$eU_ppm), max(AHe$eU_ppm))
ylim=c(min(AHe$Corrected_Age_Ma), max(AHe$Corrected_Age_Ma) +50)

pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables/AHeagevseU&Rs.pdf", width=15/2.54, height=6/2.54)

par(mfrow=c(1, 2))
par(mar=c(2,2,0,0))
bg_col=c(2,4,7) 
gap.plot(AHe$eU_ppm, AHe$Corrected_Age_Ma, type="n", gap=c(60,300),gap.axis = "x", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)",  xtics=F,ytics=F)

for (i in 1:length(gm)){
  arrows(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=22+i, bg='white')
}

points(AHe$eU_ppm[AHe$Sample_No=='0705-19'], AHe$Corrected_Age_Ma[AHe$Sample_No=='0705-19'],pch=23, bg=bg_col[1])
points(AHe$eU_ppm[AHe$Sample_No=='7315-41'], AHe$Corrected_Age_Ma[AHe$Sample_No=='7315-41'],pch=23, bg=bg_col[2])
points(AHe$eU_ppm[AHe$Sample_No=='UW98-28'], AHe$Corrected_Age_Ma[AHe$Sample_No=='UW98-28'],pch=23, bg=bg_col[3])

abline(v=60, col="white", lwd=9)

axis.break(axis=1,breakpos=60,style="slash")
axis.break(axis=1,breakpos=300,style="slash")
axis.break(axis=3,breakpos=60,style="slash")
axis(1, seq(0,50,10),  seq(0,50,10), mgp=c(.8, -0.2, 0), tck=tma, cex.axis=0.6)
axis(1, seq(70,90,10),  seq(310,330,10), mgp=c(.8, -0.2, 0), tck=tma, cex.axis=0.6)
axis(2, seq(200,600,100),  seq(200,600,100), mgp=c(.8, 0, 0), tck=tma, cex.axis=0.6)
mtext("eU(ppm)", side=1, line=1, cex.lab=.8)
mtext("AHe age (Ma)", side=2, line=1, cex.lab=.8)

par(new=T)
plot(max(AHe$eU_ppm), AHe$Corrected_Age_Ma[which(AHe$eU_ppm==max(AHe$eU_ppm))],type="n",xlim=xlim, ylim=ylim,axes=F, pch=21, bg='white')
arrows(max(AHe$eU_ppm), 
       AHe$Corrected_Age_Ma[which(AHe$eU_ppm==max(AHe$eU_ppm))]-AHe$Corrected_Age_Ma_1s[which(AHe$eU_ppm==max(AHe$eU_ppm))], 
       max(AHe$eU_ppm),
       AHe$Corrected_Age_Ma[which(AHe$eU_ppm==max(AHe$eU_ppm))]+AHe$Corrected_Age_Ma_1s[which(AHe$eU_ppm==max(AHe$eU_ppm))],
       length=0.02, angle=90, code=3)
points(max(AHe$eU_ppm), AHe$Corrected_Age_Ma[which(AHe$eU_ppm==max(AHe$eU_ppm))], pch=23, bg='white')
symbols=seq(23, 25,1)
legend("topright",legend=c("0705-19 R2=0.71", "7315-41 R2=0.39", "UW98-28 R2=0.25"), pch=23, pt.bg=bg_col,cex=0.5 )


######

par(mar=c(2,0.3,0,0))
plot(AHe$Rs_um[AHe$Grain_morphology==gm[1]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n",xlim=c(40,120), ylim=ylim, xlab="Rs (um)", ylab="", xaxt="n", yaxt="n")

axis(1, seq(30,120,10),  seq(30,120,10), mgp=c(.8, -0.2, 0), tck=tma, cex.axis=0.6)
axis(2, seq(0,1e3,200), labels=F,tck=tma,mgp=c(.8, 0.1, 0))
mtext("Rs(um)", side=1, line=1, cex.lab=.8)

for (i in 1:length(gm)){
  arrows(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Corrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Corrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=22+i, bg='white')
}
symbols=seq(23, 25,1)
points(AHe$Rs_um[AHe$Sample_No=='UW98-28'], AHe$Corrected_Age_Ma[AHe$Sample_No=='UW98-28'], pch=23, bg='pink')

legend("bottomright",legend=c("0T", "1T", "2T"), pch=symbols, pt.bg='white', cex=0.5 )
legend("topright",legend=c('UW98-28, R2=0.22'), pch=23, pt.bg='pink',cex=0.5 )

dev.off()