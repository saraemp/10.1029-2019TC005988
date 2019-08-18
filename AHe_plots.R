library(grDevices)
AHe=read.table("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables_0819/Table_3_1.csv", sep=',', header=T)
names(AHe)
gm=levels(AHe$Grain_morphology)
samples=levels(AHe$Sample_No)
#Formatting
tma=.03
tmi=.01

#Awfull way to extract the indices where each sample has a given grain morphology, but it works!
s11=which(AHe$Sample_No==samples[1]&AHe$Grain_morphology==gm[1])
s12=which(AHe$Sample_No==samples[1]&AHe$Grain_morphology==gm[2])
s13=which(AHe$Sample_No==samples[1]&AHe$Grain_morphology==gm[3])

s21=which(AHe$Sample_No==samples[2]&AHe$Grain_morphology==gm[1])
s22=which(AHe$Sample_No==samples[2]&AHe$Grain_morphology==gm[2])
s23=which(AHe$Sample_No==samples[2]&AHe$Grain_morphology==gm[3])

s31=which(AHe$Sample_No==samples[3]&AHe$Grain_morphology==gm[1])
s32=which(AHe$Sample_No==samples[3]&AHe$Grain_morphology==gm[2])
s33=which(AHe$Sample_No==samples[3]&AHe$Grain_morphology==gm[3])

s41=which(AHe$Sample_No==samples[4]&AHe$Grain_morphology==gm[1])
s42=which(AHe$Sample_No==samples[4]&AHe$Grain_morphology==gm[2])
s43=which(AHe$Sample_No==samples[4]&AHe$Grain_morphology==gm[3])

s51=which(AHe$Sample_No==samples[5]&AHe$Grain_morphology==gm[1])
s52=which(AHe$Sample_No==samples[5]&AHe$Grain_morphology==gm[2])
s53=which(AHe$Sample_No==samples[5]&AHe$Grain_morphology==gm[3])

s61=which(AHe$Sample_No==samples[6]&AHe$Grain_morphology==gm[1])
s62=which(AHe$Sample_No==samples[6]&AHe$Grain_morphology==gm[2])
s63=which(AHe$Sample_No==samples[6]&AHe$Grain_morphology==gm[3])

s71=which(AHe$Sample_No==samples[7]&AHe$Grain_morphology==gm[1])
s72=which(AHe$Sample_No==samples[7]&AHe$Grain_morphology==gm[2])
s73=which(AHe$Sample_No==samples[7]&AHe$Grain_morphology==gm[3])

s81=which(AHe$Sample_No==samples[8]&AHe$Grain_morphology==gm[1])
s82=which(AHe$Sample_No==samples[8]&AHe$Grain_morphology==gm[2])
s83=which(AHe$Sample_No==samples[8]&AHe$Grain_morphology==gm[3])

s91=which(AHe$Sample_No==samples[9]&AHe$Grain_morphology==gm[1])
s92=which(AHe$Sample_No==samples[9]&AHe$Grain_morphology==gm[2])
s93=which(AHe$Sample_No==samples[9]&AHe$Grain_morphology==gm[3])

s101=which(AHe$Sample_No==samples[10]&AHe$Grain_morphology==gm[1])
s102=which(AHe$Sample_No==samples[10]&AHe$Grain_morphology==gm[2])
s103=which(AHe$Sample_No==samples[10]&AHe$Grain_morphology==gm[3])

s111=which(AHe$Sample_No==samples[11]&AHe$Grain_morphology==gm[1])
s112=which(AHe$Sample_No==samples[11]&AHe$Grain_morphology==gm[2])
s113=which(AHe$Sample_No==samples[11]&AHe$Grain_morphology==gm[3])

s121=which(AHe$Sample_No==samples[12]&AHe$Grain_morphology==gm[1])
s122=which(AHe$Sample_No==samples[12]&AHe$Grain_morphology==gm[2])
s123=which(AHe$Sample_No==samples[12]&AHe$Grain_morphology==gm[3])

#Adding colors and also need to add error bars
colours=rainbow(12, alpha = 0.7)#

###########
#AHe vs eU#
###########
pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables_0819/AHevseU_uncorrected_colorcoded_0819.pdf", width=15/2.54, height=15/2.54)
par(mfrow=c(1, 2))
par(mar=c(2,2,0,0))

xlim= c(min(AHe$eU_ppm), max(AHe$eU_ppm))
ylim=c(min(AHe$Uncorrected_Age_Ma)-20, max(AHe$Uncorrected_Age_Ma)+50)

plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (s in 1:12){
  arrows(AHe$eU_ppm[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[s]], AHe$eU_ppm[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[s]],length=0.02, angle=90, code=3, col=colours[s])
}
#arrows(AHe$eU_ppm, AHe$Uncorrected_Age_Ma-AHe$Uncorrected_Age_Ma_1s, AHe$eU_ppm, AHe$Uncorrected_Age_Ma+AHe$Uncorrected_Age_Ma_1s,length=0.02, angle=90, code=3)

points(AHe$eU_ppm[s11], AHe$Uncorrected_Age_Ma[s11], pch=16, col=colours[1])
points(AHe$eU_ppm[s12], AHe$Uncorrected_Age_Ma[s12], pch=15, col=colours[1])
points(AHe$eU_ppm[s13], AHe$Uncorrected_Age_Ma[s13], pch=18, col=colours[1])



arrows(AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[1]], AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[1]],length=0.02, angle=90, code=3, col=colours[1])
arrows(AHe$eU_ppm[AHe$Sample_No==samples[2]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[2]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[2]], AHe$eU_ppm[AHe$Sample_No==samples[2]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[2]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[2]],length=0.02, angle=90, code=3, col=colours[2])
arrows(AHe$eU_ppm[AHe$Sample_No==samples[12]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[12]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[12]], AHe$eU_ppm[AHe$Sample_No==samples[12]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[12]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[12]],length=0.02, angle=90, code=3, col=colours[12])


points(AHe$eU_ppm[s21], AHe$Uncorrected_Age_Ma[s21], pch=16, col=colours[2])
points(AHe$eU_ppm[s22], AHe$Uncorrected_Age_Ma[s22], pch=15, col=colours[2])
points(AHe$eU_ppm[s23], AHe$Uncorrected_Age_Ma[s23], pch=18, col=colours[2])

points(AHe$eU_ppm[s31], AHe$Uncorrected_Age_Ma[s31], pch=16, col=colours[3])
points(AHe$eU_ppm[s32], AHe$Uncorrected_Age_Ma[s32], pch=15, col=colours[3])
points(AHe$eU_ppm[s33], AHe$Uncorrected_Age_Ma[s33], pch=18, col=colours[3])

points(AHe$eU_ppm[s41], AHe$Uncorrected_Age_Ma[s41], pch=16, col=colours[4])
points(AHe$eU_ppm[s42], AHe$Uncorrected_Age_Ma[s42], pch=15, col=colours[4])
points(AHe$eU_ppm[s43], AHe$Uncorrected_Age_Ma[s43], pch=18, col=colours[4])

points(AHe$eU_ppm[s51], AHe$Uncorrected_Age_Ma[s51], pch=16, col=colours[5])
points(AHe$eU_ppm[s52], AHe$Uncorrected_Age_Ma[s52], pch=15, col=colours[5])
points(AHe$eU_ppm[s53], AHe$Uncorrected_Age_Ma[s53], pch=18, col=colours[5])

points(AHe$eU_ppm[s61], AHe$Uncorrected_Age_Ma[s61], pch=16, col=colours[6])
points(AHe$eU_ppm[s62], AHe$Uncorrected_Age_Ma[s62], pch=15, col=colours[6])
points(AHe$eU_ppm[s63], AHe$Uncorrected_Age_Ma[s63], pch=18, col=colours[6])

points(AHe$eU_ppm[s71], AHe$Uncorrected_Age_Ma[s71], pch=16, col=colours[7])
points(AHe$eU_ppm[s72], AHe$Uncorrected_Age_Ma[s72], pch=15, col=colours[7])
points(AHe$eU_ppm[s73], AHe$Uncorrected_Age_Ma[s73], pch=18, col=colours[7])

points(AHe$eU_ppm[s81], AHe$Uncorrected_Age_Ma[s81], pch=16, col=colours[8])
points(AHe$eU_ppm[s82], AHe$Uncorrected_Age_Ma[s82], pch=15, col=colours[8])
points(AHe$eU_ppm[s83], AHe$Uncorrected_Age_Ma[s83], pch=18, col=colours[8])

points(AHe$eU_ppm[s91], AHe$Uncorrected_Age_Ma[s91], pch=16, col=colours[9])
points(AHe$eU_ppm[s92], AHe$Uncorrected_Age_Ma[s92], pch=15, col=colours[9])
points(AHe$eU_ppm[s93], AHe$Uncorrected_Age_Ma[s93], pch=18, col=colours[9])

points(AHe$eU_ppm[s101], AHe$Uncorrected_Age_Ma[s101], pch=16, col=colours[10])
points(AHe$eU_ppm[s102], AHe$Uncorrected_Age_Ma[s102], pch=15, col=colours[10])
points(AHe$eU_ppm[s103], AHe$Uncorrected_Age_Ma[s103], pch=18, col=colours[10])

points(AHe$eU_ppm[s111], AHe$Uncorrected_Age_Ma[s111], pch=16, col=colours[11])
points(AHe$eU_ppm[s112], AHe$Uncorrected_Age_Ma[s112], pch=15, col=colours[11])
points(AHe$eU_ppm[s113], AHe$Uncorrected_Age_Ma[s113], pch=18, col=colours[11])

points(AHe$eU_ppm[s121], AHe$Uncorrected_Age_Ma[s121], pch=16, col=colours[12])
points(AHe$eU_ppm[s122], AHe$Uncorrected_Age_Ma[s122], pch=15, col=colours[12])
points(AHe$eU_ppm[s123], AHe$Uncorrected_Age_Ma[s123], pch=18, col=colours[12])

symbols=c(16, 15,18)
legend("top",legend=c("0T", "1T", "2T"), pch=symbols )
legend("topright",legend=samples, pch=16 , col=colours )


###########
#AHe vs Rs#
###########
par(mar=c(2,2,0,0))
xlim= c(min(AHe$Rs_um), max(AHe$Rs_um))
ylim=c(min(AHe$Uncorrected_Age_Ma)-20, max(AHe$Uncorrected_Age_Ma)+50)

plot(AHe$Rs_um[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (s in 1:12){
  arrows(AHe$Rs_um[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[s]], AHe$Rs_um[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[s]],length=0.02, angle=90, code=3, col=colours[s])
}
#arrows(AHe$Rs_um, AHe$Uncorrected_Age_Ma-AHe$Uncorrected_Age_Ma_1s, AHe$Rs_um, AHe$Uncorrected_Age_Ma+AHe$Uncorrected_Age_Ma_1s,length=0.02, angle=90, code=3)

points(AHe$Rs_um[s11], AHe$Uncorrected_Age_Ma[s11], pch=16, col=colours[1])
points(AHe$Rs_um[s12], AHe$Uncorrected_Age_Ma[s12], pch=15, col=colours[1])
points(AHe$Rs_um[s13], AHe$Uncorrected_Age_Ma[s13], pch=18, col=colours[1])



arrows(AHe$Rs_um[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[1]], AHe$Rs_um[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[1]],length=0.02, angle=90, code=3, col=colours[1])
arrows(AHe$Rs_um[AHe$Sample_No==samples[2]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[2]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[2]], AHe$Rs_um[AHe$Sample_No==samples[2]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[2]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[2]],length=0.02, angle=90, code=3, col=colours[2])
arrows(AHe$Rs_um[AHe$Sample_No==samples[12]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[12]]-AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[12]], AHe$Rs_um[AHe$Sample_No==samples[12]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[12]]+AHe$Uncorrected_Age_Ma_1s[AHe$Sample_No==samples[12]],length=0.02, angle=90, code=3, col=colours[12])


points(AHe$Rs_um[s21], AHe$Uncorrected_Age_Ma[s21], pch=16, col=colours[2])
points(AHe$Rs_um[s22], AHe$Uncorrected_Age_Ma[s22], pch=15, col=colours[2])
points(AHe$Rs_um[s23], AHe$Uncorrected_Age_Ma[s23], pch=18, col=colours[2])

points(AHe$Rs_um[s31], AHe$Uncorrected_Age_Ma[s31], pch=16, col=colours[3])
points(AHe$Rs_um[s32], AHe$Uncorrected_Age_Ma[s32], pch=15, col=colours[3])
points(AHe$Rs_um[s33], AHe$Uncorrected_Age_Ma[s33], pch=18, col=colours[3])

points(AHe$Rs_um[s41], AHe$Uncorrected_Age_Ma[s41], pch=16, col=colours[4])
points(AHe$Rs_um[s42], AHe$Uncorrected_Age_Ma[s42], pch=15, col=colours[4])
points(AHe$Rs_um[s43], AHe$Uncorrected_Age_Ma[s43], pch=18, col=colours[4])

points(AHe$Rs_um[s51], AHe$Uncorrected_Age_Ma[s51], pch=16, col=colours[5])
points(AHe$Rs_um[s52], AHe$Uncorrected_Age_Ma[s52], pch=15, col=colours[5])
points(AHe$Rs_um[s53], AHe$Uncorrected_Age_Ma[s53], pch=18, col=colours[5])

points(AHe$Rs_um[s61], AHe$Uncorrected_Age_Ma[s61], pch=16, col=colours[6])
points(AHe$Rs_um[s62], AHe$Uncorrected_Age_Ma[s62], pch=15, col=colours[6])
points(AHe$Rs_um[s63], AHe$Uncorrected_Age_Ma[s63], pch=18, col=colours[6])

points(AHe$Rs_um[s71], AHe$Uncorrected_Age_Ma[s71], pch=16, col=colours[7])
points(AHe$Rs_um[s72], AHe$Uncorrected_Age_Ma[s72], pch=15, col=colours[7])
points(AHe$Rs_um[s73], AHe$Uncorrected_Age_Ma[s73], pch=18, col=colours[7])

points(AHe$Rs_um[s81], AHe$Uncorrected_Age_Ma[s81], pch=16, col=colours[8])
points(AHe$Rs_um[s82], AHe$Uncorrected_Age_Ma[s82], pch=15, col=colours[8])
points(AHe$Rs_um[s83], AHe$Uncorrected_Age_Ma[s83], pch=18, col=colours[8])

points(AHe$Rs_um[s91], AHe$Uncorrected_Age_Ma[s91], pch=16, col=colours[9])
points(AHe$Rs_um[s92], AHe$Uncorrected_Age_Ma[s92], pch=15, col=colours[9])
points(AHe$Rs_um[s93], AHe$Uncorrected_Age_Ma[s93], pch=18, col=colours[9])

points(AHe$Rs_um[s101], AHe$Uncorrected_Age_Ma[s101], pch=16, col=colours[10])
points(AHe$Rs_um[s102], AHe$Uncorrected_Age_Ma[s102], pch=15, col=colours[10])
points(AHe$Rs_um[s103], AHe$Uncorrected_Age_Ma[s103], pch=18, col=colours[10])

points(AHe$Rs_um[s111], AHe$Uncorrected_Age_Ma[s111], pch=16, col=colours[11])
points(AHe$Rs_um[s112], AHe$Uncorrected_Age_Ma[s112], pch=15, col=colours[11])
points(AHe$Rs_um[s113], AHe$Uncorrected_Age_Ma[s113], pch=18, col=colours[11])

points(AHe$Rs_um[s121], AHe$Uncorrected_Age_Ma[s121], pch=16, col=colours[12])
points(AHe$Rs_um[s122], AHe$Uncorrected_Age_Ma[s122], pch=15, col=colours[12])
points(AHe$Rs_um[s123], AHe$Uncorrected_Age_Ma[s123], pch=18, col=colours[12])

symbols=c(16, 15,18)
legend("top",legend=c("0T", "1T", "2T"), pch=symbols )
legend("topright",legend=samples, pch=16 , col=colours )
dev.off()














# I created a loop to extract the indices for a given grain morphology and sample but I couln't store it properly because the
#number was never consistent e.g. in some samples all the grains are T0. So I had to do it manually
# out <- vector("list", 3)
# for (i in seq (1:3)){
#   out[i]=which(AHe$Sample_No==samples[1]&AHe$Grain_morphology==gm[i])
# }
# out
# str(out)
# str(unlist(out))

#####
#Created a name sequence with the hope that I could use it in the loop below so that I could create a loop for the plot but it didn't work!!
a=as.character(rep('s',12*3));a
b=sort(rep(seq(1,12),3));b
c=rep(seq(1,3),12);c
for (i in 1:(12*3)){
  c[i]=paste0(a[i],b[i], c[i])
};c

#There are two problems, one that the sYX is pasted as a straing "s11" and the other one that I'm subsetting the subset
#AHe$eU_ppm["s11"]
# for (i in 1:3){#  s11  s12  s13
#   points(AHe$eU_ppm[c1[i]], AHe$Uncorrected_Age_Ma[c[i]], pch=16+i, bg=colours[1])
# }
# for (i in 4:6){# s21  s22  s23
#   points(AHe$eU_ppm[c1[i]], AHe$Uncorrected_Age_Ma[c[i]], pch=16+i, bg=colours[2])
# }











####color pallette#####
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404", alpha=.1)

#jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

colfunc_cT3ii<- colorRampPalette(YlOrBr, interpolate="spline")
colores_cT3ii=c(colfunc_cT3ii(length(samples)))

####################
plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (s in 1:length(samples)){
  points(AHe$eU_ppm[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]], pch=16, bg=colores_cT3ii[s])
}
####

plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (s in 1:length(samples)){
  points(AHe$eU_ppm[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]], pch=16, bg=colours[s])
}




#pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs/AHevseU_uncorrected_colorcoded_0819.pdf", width=15/2.54, height=6/2.54)
par(mfrow=c(1, 2))
par(mar=c(2,2,0,0))
plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)

for (i in 1:length(gm)){
  arrows(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  #points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg=samples[s])

}

colours=seq(1:length(samples))

for (i in 1:length(gm)){
  for (s in 1:length(samples)){
  arrows(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  #points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg=colours[s])
  }
}


# Let's try again
plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", xlim=xlim, ylim=ylim, xlab="eU (ppm)", ylab="AHe age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
for (s in 1:length(samples)){
  points(AHe$eU_ppm[AHe$Sample_No==samples[s]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[s]], pch=16, bg=colores_cT3ii[s])
}

points(AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]], pch=20, bg=colours[1])
points(AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]], pch=20, bg=colours[2])
points(AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]], pch=16, bg='red')



plot(AHe$eU_ppm[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], pch=20, bg=colours[s])
plot(AHe$eU_ppm[AHe$Sample_No==samples[1]], AHe$Uncorrected_Age_Ma[AHe$Sample_No==samples[1]], pch=20, bg=colours[s])




if (AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]]&){
  points(AHe$eU_ppm[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg=colours[s])
}



symbols=seq(21, 23,1)
legend("topright",legend=c("0T", "1T", "2T"), pch=symbols )

par(mar=c(2,1,0,0))
plot(AHe$Rs_um[AHe$Grain_morphology==gm[1]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[1]], type="n", ylim=ylim, xlab="Rs (um)", ylab="", yaxt="n", mgp=c(.8, 0.1, 0), tck=tma)
axis(2, seq(0,1e3,200), labels=F,tck=tma,mgp=c(.8, 0.1, 0), cex.lab=0.6)
for (i in 1:length(gm)){
  arrows(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]-AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]], AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]]+AHe$Uncorrected_Age_Ma_1s[AHe$Grain_morphology==gm[i]],length=0.02, angle=90, code=3)
  points(AHe$Rs_um[AHe$Grain_morphology==gm[i]], AHe$Uncorrected_Age_Ma[AHe$Grain_morphology==gm[i]], pch=20+i, bg="white")
}
symbols=seq(21, 23,1)
legend("topright",legend=c("0T", "1T", "2T"), pch=symbols )

dev.off()
