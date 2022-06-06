dev.off()
for (f in files) {
  ctd.i <- ctd[[f]]
  t <- ctd.i[,grep("POTMC",colnames(ctd.i))]
  s <- ctd.i[,grep("SAL",colnames(ctd.i))]
  
  if (f == files[1]) { 
    plot(s,t
         , "l"
         , xlim = c(34.3,35.4)
         , ylim = c(min(geodia[,4]), 10)
         , xlab = "Salinity (psu)"
         , ylab = "Temperature ("~degree~"C)"
    )
    title(paste0(region,"Region TSPlot"))
  } else { 
    par(new=T)
    plot(s,t
         , xlab = ''
         , ylab = ''
         , "l"
         , axes = FALSE
         , xlim = c(34.3,35.4)
         , ylim = c(min(geodia[,4]), 10)
    )
    
  }
}
par(new=TRUE)
plot(geodia.sal,geodia.temp
     , pch=21 
     , xlim = c(34.3,35.4) 
     , ylim = c(min(geodia[,4]),10)
     , xlab = ''
     , ylab = '' 
     , col = "red", bg = "red"
     , cex = 1.25
)
par(new=TRUE)
plot(geodiaP.sal,geodiaP.temp
     , pch = 21
     , xlim = c(34.3, 35.4)
     , ylim = c(min(geodia[,4]),10)
     , xlab = ''
     , ylab = ''
     , col = "blue", bg = "blue"
     
)
text(35, 5
     , labels="Label"
     , adj = c(1.05, -0.5)
     , cex=0.9
     , font=2
     , col="black"
)
text(35.075, 0.8
     , labels="Label"
     , adj = c(1.05, -0.5)
     , cex=0.9
     , font=2
     , col="black"
)
lines(c(35.01,35.12), c(5.3,5.3)
      , lty="12"
      , lwd=3
      , col="steelblue2"
)
lines(c(35,34.9), c(0.8,0)
      , lty="12"
      , lwd=3
      , col="steelblue2"
)
legend( 'topright'
        , c("Geodia occurrences")
        , pch = 21
        , col = c("red","blue")
        , pt.bg = c("red","blue")
        , bty ='n'
        , cex = .75
)
drawIsopycnals()
