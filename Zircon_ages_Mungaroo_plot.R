#ZHe_Mungaroo=read.table("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables_0819/
ZHe_Mungaroo=read.table("/Users/smoron/Documents/1.Projects/Thermochron/ZrHe_SaraMoron_Mungaroo_January2019.csv", sep=",",header=T)
names(ZHe_Mungaroo)



d <- density(ZHe_Mungaroo$Age_Ma)
mode=d$x[which.max(d$y)]

hist(ZHe_Mungaroo$Age_Ma,breaks=10 )
abline(v=median(ZHe_Mungaroo$Age_Ma))
length(ZHe_Mungaroo$Age_Ma)

pdf("/Users/smoron/Documents/1.Projects/Thermochron/Thermochron_figs&tables_0819/Mungaroo_Ages_hist.pdf", width=9.5/2.54, height=9.5/2.54)

par(mar=c(2,2,0,0))
plot(density(ZHe_Mungaroo$Age_Ma), main="", ylab="Probability density", xlab="Age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
abline(v=mode)
text(200,0.0038,round(mode,2) )
dev.off()

sn=levels(ZHe_Mungaroo$SampleID);sn
xlimZHe= c(min(ZHe_Mungaroo$eU_ppm), max(ZHe_Mungaroo$eU_ppm))
xl=min(ZHe_Mungaroo$eU_ppm)
xu=max(ZHe_Mungaroo$eU_ppm)
ylimZHe=c(min(ZHe_Mungaroo$Age_Ma),max(ZHe_Mungaroo$Age_Ma)+20)
plot(ZHe_Mungaroo$eU_ppm[ZHe_Mungaroo$SampleID==sn[2]], ZHe_Mungaroo$Age_Ma[ZHe_Mungaroo$SampleID==sn[2]], type="n",  xlim=xlimZHe, ylim=ylimZHe, xlab="eU (ppm)", ylab="ZHe Age (Ma)", mgp=c(.8, 0.1, 0), tck=tma)
