# Constrained clustering for regeneration patches by using understory-plant spatial structure
## Reference: Borcard D, Gillet F, Legendre P (2011) Numerical ecology with R. Springer, New York

#### But Constrained clustering, require  R 2.15.3 (or older versions). My trials in R 3.3 failed...
#### Can load final results from github, if not want re-run
#==============================
library("lattice",lib.loc="C:/Program Files/R/R-2.15.3/library") ## specify your older R lib.loc
library("Matrix",lib.loc="C:/Program Files/R/R-2.15.3/library")
library(const.clust) ## download frome https://github.com/philippec/fonctions_R_git
library(spdep)

## load dataset, NOTE: Note that the https://URL may not work except on Windows? if not, just download the files
## download.file(URL,"rdata"); load("rdata")
load(url("https://github.com/cywhale/spat_struct_regen/raw/master/dataset/tmp_biofac_us.RData"))

## if not want to rerun
## load(url("https://github.com/cywhale/spat_struct_regen/raw/master/dataset/tmp_clust_patch.RData"))

u.site <- as.data.frame(u.site)
coords <- coordinates(u.site[,c("x","y")])
nbg <- graph2nb(relativeneigh(coords,nnmult=4),sym=T)
plot(nbg,coords)

ttw <- nb2listw(nbg, style="B")
links.mat.dat <- listw2mat(ttw)

## Plot the connections
#
par(mfrow=c(1,1),mar=c(0,0,1,0))
plot(ttw,coords=coords,pch=19, cex=0.1, col="blue")

# 2. Constrained clustering using default Ward method
ux <- scale(cbind(ukm.ax[,1]))
colnames(ux) <- c("uRDA1")

D.uenv <- dist(ux) 
clus.uenv <- constrained.clust(D.uenv, links.mat.dat) ## Note: take several hours...

plot(clus.uenv, hang=-1)

cross.clus <- cross(ux, clus.uenv,k1=3,k2=15)
summary(cross.clus)

# 4. Plot maps of constrained clustering steps from 2 to 8 groups
# map.hclust(clus.uenv, as.data.frame(u.site[,c("x","y")]), k1=4,k2=7,cex=0.9,pos=4)

out1 <- data.frame(u.site[,c("transect")],cross.clus$out)
head(out1)

# write.table(out1,
#             "D:/R/01paper_ssn/simu_out/const_clust_grp01.csv",
#             row.names=F, col.names=TRUE, na="", quote=FALSE, sep=",")

out2 <- data.frame(rownames(cross.clus$AIC),cross.clus$AIC)
colnames(out2)[1] <- "group"
## for validation
# write.table(out2,
#             "D:/R/01paper_ssn/simu_out/cross_clust_AIC01.csv",
#             row.names=F, col.names=TRUE, na="", quote=FALSE, sep=",")

# for not re-run time-consumint steps
# save(clus.uenv, cross.clus, file="D:/R/01paper_ssn/simu_out/tmp_clust_patch.RData")

# check old version for consistent output
oldx <- read.table(url('https://raw.githubusercontent.com/cywhale/spat_struct_regen/master/dataset/const_clust_grp01.csv'),
                   header=T,na.string="",sep=",")
all.equal(as.integer(oldx$Gr.5), as.integer(out1$Gr.5))
# TRUE
