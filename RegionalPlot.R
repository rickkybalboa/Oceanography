#####
# Plot stuff
#####

xlimits = c(34.8,35.2)
ylimits = c(-1,4)

# generates plot of avg of all ctd profiles in region
AvgPlot(ctd.avg,geodiaH.sal,geodiaH.temp, geodiaP.sal, geodiaP.temp, xlimits, ylimits)

# generates plots for each station in /data/region/Images
MultiPlot(files,bodc.coords)  





# all regional data on same figure



#xlimits = c(min(geodia[,grep("SAL",names(geodia),ignore.case = TRUE)]), 35.2)
#ylimits = c(min(geodia[,grep("TEMP",names(geodia),ignore.case = TRUE)]), 4)

 # png(file=paste0("~/Oceanography/Sponge_Project/Data/",region,"/RegionalPlot_",region,".png"),
 #     width=5, height=4, units="in", res=1200, pointsize=8) # L and O 1-column width.

for (f in files) {
  ctd.i <- ctd[[f]]
  t <- ctd.i[,grep("TEMP",colnames(ctd.i))]
  s <- ctd.i[,grep("SAL",colnames(ctd.i))]
  
  if (f == files[1]) { 
    plot(s,t
         , "l"
         , xlim = xlimits
         , ylim = ylimits
         , xlab = "Salinity (psu)"
         , ylab = "Temperature ("~degree~"C)"
    )
    title(paste0(region," Region TSPlot"))
  } else { 
    par(new=T)
    plot(s,t
         , xlab = ''
         , ylab = ''
         , "l"
         , axes = FALSE
         , xlim = xlimits
         , ylim = ylimits
    )
    
  }
}
par(new=TRUE)
plot(geodiaH.sal,geodiaH.temp
     , pch=21 
     , xlim = xlimits
     , ylim = ylimits
     , xlab = ''
     , ylab = '' 
     , col = "red", bg = "red"
     , cex = 1.2
)
par(new=TRUE)
plot(geodiaP.sal,geodiaP.temp
     , pch=21
     , xlim = xlimits
     , ylim = ylimits
     , xlab = ''
     , ylab = ''
     , col = "blue", bg = "blue"
     , cex = 1
)
legend( 'topright'
        , c("G.Hentscheli","G.Phlegraei/G.Parva")
        , pch = 21
        , col = c("red","blue")
        , pt.bg = c("red","blue")
        , bty ='n'
        , cex = .75
)
drawIsopycnals()
dev.off()
