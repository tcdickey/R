dat<-read.delim(file="/Users/tcd8/Documents/Med School/3rd year/MPH/Thesis/systematic review/figures/hospitalmeta.txt",header=TRUE)
#dat2<-na.pass(dat)
res<-rma(yi=yi,sei=sei,data=dat)
#res<-rma(yi=yi,vi=vi,mods=cbind(educ,coach,monitor),data=dat)
{forest(res, slab = paste(dat$Author, dat$Year,sep=" "), 
       xlim = c(-16, 6), at = log(c(0.05, 0.25, 1, 4)), 
       atransf = exp, ilab = cbind(dat$ti, dat$tn, dat$ci, dat$cn),
       ilab.xpos = c(-10.5, -8.5, -6, -4),cex=0.8)
}
text(c(-10.5, -8.5, -6, -4), 3.5, c("Visits", "Participants", "Visits", "Participants"),cex=0.8)
text(c(-9.5, -5), 4, c("Intervention", "Control"),cex=0.8)
text(-16, 3.5, "Author(s) and Year", pos = 4,cex=0.8)
text(6, 3.5, "Relative Risk [95% CI]", pos = 2,cex=0.8)
#text(-15.5, 1.75, "Community Health Worker", pos =4,cex=0.8)
#text(-15.5, 1, "Case Manager", pos =4,cex=0.8)
#at 900x651 pixels