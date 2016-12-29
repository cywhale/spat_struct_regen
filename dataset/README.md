tmp_ssn_topography.RData
save intermediate results of topographic factors in this paper (utopo: topograpic factors of understory grids used in variation partitioning; trx: some additional information shared between tree subplots and understory sampling sites; 10 m x 10m projected on 2 m x 2m). Results from 01_terrainPlot.Rmd

tmp_PCNM_us.RData
save intermeidate, time-consuming results of PCNM (MEMs) and forward-selection in 01S_Tab1_spatialPCNM.Rmd

tmp_biofac_us.RData
sav intermediate (neighborhood) bio-factors in understory, from 01S_Tab1_spatialPCNM.rmd. (u.site: site name in transects and x,y-coordinates of understory sites, uscov: us cover data, tnc: tree BA and density, sx.env.red: significant topographic factors to variations in seedling assemblages; ukm.red, understory-plant spatial structure, ukm.ax, ukm.red projected on first RDA axis, partialling out topographic effects. xres.h, xsdl.h, sdl.new, ush.h: compositions of understory or seedlings, .h means hellinger transformed)

tmp_codisp_out.RData.gz.001
tmp_codisp_out.RData.gz.002
save intermeidate, time-consuming results of codispersion analysis in 03_Fig3_codisp_permutation.Rmd (R object dxq, sigt)

tmp_patch_permutation.RData
save intermeidate, time-consuming results to compare differences of community patterns among patches in 04_Fig4_community_diff_patch.Rmd (R object dxp)

**Constrained clustering results**

> Because the R code cluster_constrain_patch01.R needs old version of R 2.15.3, so, if not want to re-run, jus loaded from here

1. tmp_cluster_patch.RData: intermediate results of R objects from const.clust package. clus.uenv: clustering output, cross.clus: cross validation of clustering results

2. const_clust_grp01.csv: site grouping according to clustering results. We'll use "grp.5" (5 groups for our understory sites)

3. const_clust_AIC01.csv: cross validation results using AIC and CVRE (cross-validation residual error, help by ?cross in const.clust).
