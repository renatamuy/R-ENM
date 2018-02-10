for(i in sp){
  tif.sp <- grep(i, tif, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
  tss <- do.call("rbind", eva.sp)$TSS
  id.tss <- which(tss > .5)
  tss.05 <- tss[tss > .5]
  
  if(length(id.tss) == 0){
    
    print(paste0("Ops! The ensemble for ", i, " don't have models with TSS above 0.5!"))
    
  } else{
    
    print(paste0("The ensemble for ", i, " started, relax, take a coffee, it may take a while...."))
    da <- rbind(da, stack(tif.sp[id.tss])[], use.names = F)
    da.r <- data.table(decostand(da, "range", na.rm = T)) 
    
    ens[] <- apply(da.r, 1, function (x) sum(x * tss.05) / sum(tss.05))
    
    setwd("ensemble_wei")
    writeRaster(ens, paste0("ens_wei_ave_", i, ".tif"), format = "GTiff")
    setwd("..")
    
    print(paste0("Nice! The ensemble for ", i, " it's done!"))
    
    da <- data.table()
    ens[] <- NA}
  
  print("Yeh! It's over!!!")
  }
