#--------------------------------------------#
#------ Kdec Coefficient for Sugarcane ------#
#--------------------------------------------#

#--- The Kdec coefficient is computed to mimick the sugarcane yield decrease with sequential cuts (Ratooning).
#--- In experiental areas with low soil compactation, hand-cut canes and better managed crop field the yield decrease (kdec)
#--- with sequential cuts is low. However, the scalability of a satisfatory management (roots health, P&D, compactation and  
#--- harvest) in Brazilian commercial areas impose a huge challenge nowadays. On these areas there is a considerable yield decrease 
#--- with sequencial sugarcane cuts (ratooning). Process-based crop models used to simulate sugarcane yield, such as DSSAT/CAENGRO, 
#--- APSIM-Sugar and FAO-MZA, were not developed to account for these effects on sugarcane yield (roots health, P&D, compactation and harvest)
#--- therefore, a coeffcient (Kdec) is proposed to reduce the simulated yield. 
#--- i.e. Kdec is the management efficiency of the grower.
#--- Refer to: http://dx.doi.org/10.1016/j.fcr.2017.07.022

#--- Goal of this code:
#--- Read growers yield historical series and compute Kdec

#--- Murilo Vianna (Jul-2018)
#--------------------------------------------#
wd            ="D:/Murilo/FE/kdec_sc_soil"
mast_xfile    = "KDEC0001_master.SCX"
soil_db_fln   = "BR.sol"
new_soil_fln  = "BM.sol"
ds_v          = 47                  # DSSAT version
crop          = "Sugarcane"         # Crop used
plantgro_fh   = "PlantGro_Head.csv" # A csv file with original PlantGro output names and converted names for R (This is necessary because R do not accept some special characters as vector label, such as # and %)
url_pat       = "https://stormy.granduke.net/weather/dssat/download?startDate=<init_date>&endDate=<end_date>&token=108c266955198058416680abc015dadcd9c0656b&lat=<latitude>&lon=<longitude>&insi=<WTH_ID>"
sim_start_bf  = 6 * 30
y_csvfile     = "y_data.csv"
opt_per_cv    = F # optmize per cultivar
check_soil    = T
slope         = 5.1
slro_fln      = "SLRO.csv"
delete_xfiles = T #--- delete all created xfiles in the end?
#--------------------------------------------#

setwd(wd)
source("kdec_f.R")
source("dssat_sol_f.R")

#--- reading yield data provided by growers
y_data_db  = read.csv(y_csvfile)

y_data_p = y_data_db[y_data_db$cut==0,]

slro        = read.csv(slro_fln)
soil_db     = readLines(soil_db_fln)

soil_ID     = rep("",length(soil_db))
soil_dtype  = rep("",length(soil_db))

for(i in 1:length(soil_db)){
  if(substr(soil_db[i],1,1) == "*"){
    soil_ID[i] = substr(soil_db[i],2,11)
  }else{
    soil_ID[i] = soil_ID[i-1]
  }
  
  if(substr(soil_db[i],1,1) == "*"){
    soil_dtype[i] = "ID"
  }else if(substr(soil_db[i],1,5) == "@SITE"){
    soil_dtype[i] = "INFO"
  }else if(substr(soil_db[i],1,6) == "@ SCOM"){
    soil_dtype[i] = "PROFDAT"
  }else if(substr(soil_db[i],1,12) =="@  SLB  SLMH"){
    soil_dtype[i] = "LAYER_DT1"
  }else if(substr(soil_db[i],1,12) =="@  SLB  SLPX"){
    soil_dtype[i] = "LAYER_DT2"
  }else{
    soil_dtype[i] = soil_dtype[i-1]
  }
  
  if(soil_ID[i] == ""){
    soil_dtype[i] = ""
    soil_ID[i]    = ""
  }
}

l_soil_db = unique(soil_ID[soil_ID != "SOILS: DSS"])


for(s in l_soil_db){
  
  #--- Soil ID
  soil_id = s
  soil_id = paste0(substr(new_soil_fln,1,2),substr(soil_id,3,10))
  
  #--- Soil profile Info
  soil_block = soil_db[soil_dtype == "INFO" & soil_ID == s]
  
  info_data = data.frame(site       = trimws(substr(soil_block[2],2,12)),
                         country    = trimws(substr(soil_block[2],14,24)),
                         lat        = as.numeric(substr(soil_block[2],27,34)),
                         lon        = as.numeric(substr(soil_block[2],36,42)),
                         soil_class = trimws(substr(soil_block[2],43,100)))
  
  #--- Soil Prof Data
  soil_block = soil_db[soil_dtype == "PROFDAT" & soil_ID == s]
  
  ps_data = read.table(text = soil_block[2:length(soil_block)],
                       col.names = c("scom","salb","slu1","sldr","slro","slnf","slpf","smhb","smpx","smke"))
  
  #--- compute SLRO based on slope and SLDR
  ps_data$slro = slro$SLRO[slro$SLDR_min<=ps_data$sldr & slro$SLDR_max>ps_data$sldr & 
            slro$slope_min<=slope & slro$slope_max>slope]
  
  #--- Soil layered data
  soil_block = soil_db[soil_dtype == "LAYER_DT1" & soil_ID == s]

  ly_data = read.table(text = soil_block[2:length(soil_block)],
            col.names = c("slb","slmh","slll","sdul","ssat","srgf","ssks","sbdm","sloc","slcl","slsi","slcf","slni","slhw","slhb","scec","sadc"))
  
  #--- check if the first layer is lower than 5 cm
  if(ly_data$slb[1]>5){
    ly_data = rbind(ly_data[1,],ly_data)
    ly_data$slb[1] = 3# set the first layer to 3 cm
  }
  
  ly_data$slmh = as.character(ly_data$slmh)
  
  #--- compute root factor based on soil depth
  #--- refer to: https://farmersedgedev.atlassian.net/wiki/spaces/ARD/pages/287113416/DSSAT-CANEGRO+Soil+Sensitivity+Analysis+for+Brazil
  a = 67.863  #Rainfed
  b = 0.990   #Rainfed
  
  slt = ly_data$slb
  if(length(slt)== 1){
    slt = 0
  }else{
    slt[1] = 0
    slt[2:length(slt)] = ly_data$slb[1:(length(slt)-1)]
  }
  
  ly_data$srgf = 1/(1+(((ly_data$slb+slt)/2)/a)^b)
  
  max_dp = max(ly_data$slb)
  
  #--- check soil data validity
  while(any(ly_data$slll > ly_data$sdul) | any(ly_data$sdul > ly_data$ssat)){
    
    if(any(ly_data$slll > ly_data$sdul)){
      #--- swap values
      swap_slll = ly_data$slll
      swap_slll[ly_data$slll > ly_data$sdul] = ly_data$sdul[ly_data$slll > ly_data$sdul]
    
      swap_sdul = ly_data$sdul
      swap_sdul[ly_data$slll > ly_data$sdul] = ly_data$slll[ly_data$slll > ly_data$sdul]
    
      message(paste0("Witing point is higher than Field Capacity for soil: ",soil_id))
      message("Values were swapped.")
    
      ly_data$slll = swap_slll
      ly_data$sdul = swap_sdul
    
    }else if(any(ly_data$sdul > ly_data$ssat)){
      #--- swap values
      swap_sdul = ly_data$sdul
      swap_sdul[ly_data$sdul > ly_data$ssat] = ly_data$ssat[ly_data$sdul > ly_data$ssat]
    
      swap_ssat = ly_data$ssat
      swap_ssat[ly_data$sdul > ly_data$ssat] = ly_data$sdul[ly_data$sdul > ly_data$ssat]
    
      message(paste0("Field capacity point is higher than saturation point for soil: ",soil_id))
      message("Values were swapped.")
    
      ly_data$sdul = swap_sdul
      ly_data$ssat = swap_ssat
    
    }
  }
  
  if(any(ly_data$slll > 0.4)){message(paste0("Warning: Wilting point value is too high in soil: ",soil_id))}
  if(any(ly_data$sdul > 0.6)){message(paste0("Warning: Field capacity point value is too high in soil: ",soil_id))}
  if(any(ly_data$ssat > 0.8)){message(paste0("Warning: Saturation point value is too high in soil: ",soil_id))}
  
  if(any(ly_data$slll < 0.0)){stop(paste0("Mandatory wilting point value (SLLL) not defined in soil: ",s))}
  if(any(ly_data$sdul < 0.0)){stop(paste0("Mandatory field capacity point value (SLLL) not defined in soil: ",s))}
  if(any(ly_data$ssat < 0.0)){stop(paste0("Mandatory saturation point value (SLLL) not defined in soil: ",s))}
  
  if(ps_data$salb < 0.05){
    ps_data$salb = 0.05
    message(paste0("Albedo value is too low for soil: ",s))
    message(paste0("Albedo value set to minimum of 0.05 in soil: ",s))
  }
  
  if(ps_data$salb > 0.95){
    ps_data$salb = 0.95
    message(paste0("Albedo value is too high for soil: ",s))
    message(paste0("Albedo value set to maximun of 0.95 in soil: ",s))
  }
  
  
  soil_dssat = ds_sol(soil_id,
                      max_dp,
                      as.character(info_data$site),
                      as.character(info_data$country),
                      info_data$lat,
                      info_data$lon,
                      as.character(info_data$soil_class),
                      ps_data$salb,
                      ps_data$slu1,
                      ps_data$sldr,
                      ps_data$slro,
                      ps_data$slnf,
                      ps_data$slpf,
                      ly_data$slb,
                      ly_data$slmh,
                      ly_data$slll,
                      ly_data$sdul,
                      ly_data$ssat,
                      ly_data$srgf,
                      ly_data$ssks,
                      ly_data$sbdm,
                      ly_data$sloc,
                      ly_data$slcl,
                      ly_data$slsi,
                      ly_data$slcf,
                      ly_data$slni,
                      ly_data$slhw,
                      ly_data$slhb,
                      ly_data$scec,
                      ly_data$sadc,
                      f_head = F)
  
  if(s == l_soil_db[1]){
    file_sol_dssat = c("*SOILS:  DSSAT BRAZIL FOR SUGARCANE","",soil_dssat)  
  }else{
    file_sol_dssat = c(file_sol_dssat,"",soil_dssat)  
  }
}

write(file_sol_dssat,file = paste0("C:/DSSAT", ds_v, "/Soil/",new_soil_fln))


#----------------------------------------------------------------------------------------

y_data = y_data_p

#--- Separate data by Grower
l_grower = unique(y_data$site)
for(grower in l_grower){
  
  #--- separate by weather
  l_wth = unique(y_data$WTH_code[y_data$site==grower])
  for(wth in l_wth){
    
    message(paste("Preparing to download",wth,"WTH file from Stormy for customer",grower))
    
    #--- Read initial and end dates for the WTH file
    planting_dates_wth = as.Date(paste0(y_data$year_planting[y_data$site==grower & y_data$WTH_code==wth],"-01-01")) + y_data$doy_planting[y_data$site==grower & y_data$WTH_code==wth] - 1
    init_date_wth = planting_dates_wth - sim_start_bf
    init_date_wth = as.Date(paste0(format(min(init_date_wth),"%Y"),"-01-01"))
    harvesting_dates_wth = as.Date(paste0(y_data$year_harvesting[y_data$site==grower & y_data$WTH_code==wth],"-01-01")) + y_data$doy_harvesting[y_data$site==grower & y_data$WTH_code==wth] - 1
    end_date_wth = as.Date(paste0(format(max(harvesting_dates_wth),"%Y"),"-12-31"))
    wth_code     = unique(y_data$WTH_code[y_data$site==grower & y_data$WTH_code==wth])
    
    #--- formating WTH file name
    nyear     = sprintf("%02.0f", (as.numeric(format(end_date_wth,"%Y")) - as.numeric(format(init_date_wth,"%Y")) + 1))
    init_year = substr(format(init_date_wth,"%Y"),3,4)
    wth_nm    = paste0(wth_code,init_year,nyear)
    
    #--- mean lat/lon (because some fields can use the same WTH by being close together)
    lat = mean(y_data$lat[y_data$site==grower & y_data$WTH_code==wth])
    lon = mean(y_data$lon[y_data$site==grower & y_data$WTH_code==wth])
    
    #--- download WTH from Stormy
    d_link = gsub("<init_date>" ,init_date_wth,url_pat)
    d_link = gsub("<end_date>"  ,end_date_wth,d_link)
    d_link = gsub("<latitude>"  ,lat,d_link)
    d_link = gsub("<longitude>" ,lon,d_link)
    d_link = gsub("<WTH_ID>"    ,wth,d_link)
    
    #--- download directly on Weather directory
    download.file(url = d_link, destfile = paste0("C:/DSSAT",ds_v,"/",crop,"/",wth_nm,".WTH"))
    message(paste("Download of",wth_nm,"WTH file is completed."))
    
    #--- separate by type of soil
    l_soil = unique(y_data$DSSAT_Soil[y_data$site==grower & y_data$WTH_code==wth])
    for(soil in l_soil){
      
      #--- separate by cultivar
      l_cultivar = unique(y_data$cultivar_name_dssat[y_data$site==grower & y_data$DSSAT_Soil==soil & y_data$WTH_code==wth])
      for(cultivar in l_cultivar){
        
        fy_data = y_data[y_data$site==grower &
                           y_data$DSSAT_Soil==soil &
                           y_data$cultivar_name_dssat==cultivar &
                           y_data$WTH_code==wth,]
        
        #--- create a single xfile for each observation
        for(obs in 1:length(fy_data$cut)){
          
          l_soil_db_c = paste0(substr(new_soil_fln,1,2),substr(l_soil_db,3,10))
          
          it = 1
          for(s in l_soil_db_c){
            
            setwd(wd)
            message(paste("Preparing Xfile for",grower,"grower, unique ID:",as.character(fy_data$unique_run_id[obs]),"soil:",s))
          
            #--- reading the initial date os start of simulations
            pl_date = as.Date(paste0(fy_data$year_planting[obs],"-01-01")) + fy_data$doy_planting[obs] - 1
            init_date = pl_date - sim_start_bf
            ssdate = YYDOY(init_date)
          
            #--- read the xfile
            xfile = readLines(mast_xfile)
            crop_ID       = substr(mast_xfile,nchar(mast_xfile)-2,nchar(mast_xfile)-1)
          
            #--- format data for xfile
            t_id = paste(substr(cultivar,1,8),substr(s,3,10),format(pl_date,"%Y_%m"),sep="_")
            if(nchar(t_id)>25){
              for(i in 1:(25 - nchar(t_id))){t_id = paste0(t_id," ")} # treatment ID is 25 characters length
            }
            rowspacing = fy_data$rowspacing[obs]
            if(rowspacing<100){rowspacing = paste0(" ",rowspacing)}
          
            #--- modify master xfile 
            xfile = gsub("<tname_id>"   ,t_id,xfile)
            xfile = gsub("<ingeno>"     ,fy_data$ingeno_dssat[obs],xfile)
            xfile = gsub("<cname>"      ,fy_data$cultivar_name_dssat[obs],xfile)
            xfile = gsub("<station>"    ,wth,xfile)
            xfile = gsub("<soil_id>"    ,s,xfile) # here is the iterator
            xfile = gsub("<pyr>"        ,substr(fy_data$year_planting[obs],3,4),xfile)
            xfile = gsub("<pdoy>"       ,sprintf("%003.0f", fy_data$doy_planting[obs]),xfile)
            xfile = gsub("<plme>"       ,fy_data$plme[obs],xfile)
            xfile = gsub("<rowspacing>" ,rowspacing,xfile)
            xfile = gsub("<hyr>"        ,substr(fy_data$year_harvesting[obs],3,4),xfile)
            xfile = gsub("<hdoy>"       ,sprintf("%003.0f", fy_data$doy_harvesting[obs]),xfile)
            xfile = gsub("<nyr>"        ," 1",xfile) # a single run per observation
            xfile = gsub("<sspyr>"      ,substr(ssdate,1,2),xfile)
            xfile = gsub("<sspdoy>"     ,substr(ssdate,3,5),xfile)
          
            #--- write xfile
            write(xfile,paste0("C:/DSSAT", ds_v, "/", crop, "/", paste0(wth,sprintf("%0004.0f", it)),".",crop_ID,"X"))
          
            message(paste("The Xfile",paste0(wth,sprintf("%0004.0f", obs)),"for",grower,"grower, unique ID:",as.character(fy_data$unique_run_id[obs]),"was successfully created."))
          
            if(it == 1){
              l_xfile = data.frame(xfile = paste0(fy_data$WTH_code[obs],sprintf("%0004.0f", it)),
                                 run_id= as.character(fy_data$unique_run_id[obs]))
            }else{
              l_xfile = rbind(l_xfile,data.frame(xfile = paste0(fy_data$WTH_code[obs],sprintf("%0004.0f", it)),
                                               run_id= as.character(fy_data$unique_run_id[obs])))
            }
            
            it = it +1
          }
          
          #--- prepare batch call
          bfile = readLines(paste(wd, "/DSSBatch_Master.v47", sep = ""))
          bfile_head  = bfile[1:3]
          bfile_rep   = bfile[4]
          
          bfile_call = bfile_head
          for(xfile_nm in l_xfile$xfile){
            
            bfile_call = c(bfile_call,gsub("<calib_xfile>", paste0(xfile_nm,".",crop_ID,"X"), bfile_rep))
          }
          #--- write in Crop folder
          write(bfile_call,file = paste("C:/DSSAT", ds_v, "/", crop, "/", "DSSBatch.v", ds_v, sep = ""))
          
          #--- set wd to run
          setwd(paste("C:/DSSAT", ds_v, "/", crop, "/", sep = ""))
          
          #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
          system(paste0("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste0("DSSBatch.v", ds_v)))
          
          message("-------------------------------------------------------")
          message(paste("DSSAT Completed Simulations for",grower,"grower"))
          message(paste("WTH:     ",wth))
          message(paste("All Soils"))
          message(paste("Cultivar:",cultivar))
          message("-------------------------------------------------------")
          
          message(paste("Reading PlantGro.OUT"))
          
          #--- Read simulated data
          plant_lines = readLines("PlantGro.OUT")
          
          setwd(wd)
          
          #--- PlantGro Head
          pgro_head = read.csv(plantgro_fh)
          
          #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
          write.table(
            plant_lines[substr(plant_lines, 2, 3) == "19" |
                          substr(plant_lines, 2, 3) == "20"],
            file = "PlantGro_numeric.OUT",
            row.names = F,
            col.names = F,
            quote = F
          )
          plant = read.table(file = "PlantGro_numeric.OUT")                   #Read numeric lines as data.frame
          file.remove("PlantGro_numeric.OUT")
          #--- Columns name accordingly to DSSAT output name
          colnames(plant) = pgro_head$R_head
          
          message(paste("Plantgro indexing Unique_RUN_ID"))
          
          #--- Read Runs (last year series)
          run = trimws(substr(plant_lines[substr(plant_lines, 2, 4) == "TRE"], 18, 43))
          
          #--- Index outputs with treatments
          plant$run = ""
          j = 0
          for (i in 1:length(plant$dap)) {
            if (plant$dap[i] == 0) {
              j = j + 1
            }
            plant$run[i] = run[j]
          }
          
          message("Retrieving corresponding simulated data from PlantGro.OUT")
          
          l_uid = unique(plant$run)
          for(uid in l_uid){
            
            #--- check if there is more than one output to be retrieved
            l_out = unique(fy_data$dssat_output)
            
            for(out in l_out){
              out_pg = as.character(pgro_head$R_head[pgro_head$PlantGro_head==out])
              if(uid == l_uid[1]){
                fy_out = data.frame(uid = uid,
                                    out = out,
                                    dssat_out = plant[plant$run==uid & plant$dap==max(plant$dap[plant$run==uid]),out_pg])
              }else{
                fy_out = rbind(fy_out,data.frame(uid = uid,
                                                 out = out,
                                                 dssat_out = plant[plant$run==uid & plant$dap==max(plant$dap[plant$run==uid]),out_pg]))
              }
            }
          }
          
          simula = fy_out
          
          observ = fy_data$measured_data[obs]
          
          if(length(observ)==1){
            #--- compute absolute deviation for this observation against simulations using all soils in DB
            perf = data.frame(id   = simula$uid,
                              soil = l_soil_db_c,
                              adev = abs(simula$dssat_out - rep(observ,length(simula$dssat_out))))
          }else{
            #--- use mperf here for a more general code
          }
          
          #--- sort soils by best performance
          perf = perf[order(perf$adev),]
          
          #--- gather performance for soils
          if(obs ==1){
            rank_soil = perf[1:25,c("soil","adev")]
          }else{
            rank_soil = cbind(rank_soil,perf[1:25,c("soil","adev")])
          }
          
          colnames(rank_soil)[length(rank_soil)-1]  = paste(grower,substr(cultivar,1,8),format(pl_date,"%Y_%m"),"soilID",sep="_")
          colnames(rank_soil)[length(rank_soil)]    = paste(grower,substr(cultivar,1,8),format(pl_date,"%Y_%m"),"adev",sep="_")
        }
        
        #--- gather data by cultivar
        message("Gathering data by Cultivar")
        if(cultivar == l_cultivar[1]){
          fy_out_cv = fy_out
          rank_soil_cv = rank_soil
        }else{
          fy_out_cv = rbind(fy_out_cv,fy_out)
          rank_soil_cv = cbind(rank_soil_cv,rank_soil)
        }
      }
      #--- gather data by soil
      message("Gathering data by Soil")
      if(soil == l_soil[1]){
        fy_out_cv_sl = fy_out_cv
        rank_soil_cvs= rank_soil_cv
      }else{
        fy_out_cv_sl = rbind(fy_out_cv_sl,fy_out_cv)
        rank_soil_cvs= cbind(rank_soil_cvs,rank_soil_cv)
      }
    }
    
    #--- gather data by WTH
    message("Gathering data by Weather")
    if(wth == l_wth[1]){
      fy_out_cv_sl_wth = fy_out_cv_sl
      rank_soil_cvswth = rank_soil_cvs
    }else{
      fy_out_cv_sl_wth = rbind(fy_out_cv_sl_wth,fy_out_cv_sl)
      rank_soil_cvswth = cbind(rank_soil_cvswth,rank_soil_cvs)
    }
  }
  
  #--- gather data by grower
  message("Gathering data by grower")
  if(grower == l_grower[1]){
    fy_out_grower = fy_out_cv_sl_wth
    rank_soil_all = rank_soil_cvswth
  }else{
    fy_out_grower = rbind(fy_out_grower,fy_out_cv_sl_wth)
    rank_soil_all = cbind(rank_soil_all,rank_soil_cvswth)
  }
}

if(delete_xfiles){
  file.remove(paste0("C:/DSSAT", ds_v, "/", crop, "/", l_xfile$xfile,".",crop_ID,"X"))
}

write.csv(rank_soil_all,file = paste0(wd,"/soil_rank.csv"),row.names = F)

#----------------------------------------------------------------------------------------






















#--- Separate data by Grower
l_grower = unique(y_data$site)
for(grower in l_grower){
  
  #--- separate by weather
  l_wth = unique(y_data$WTH_code[y_data$site==grower])
  for(wth in l_wth){
    
    message(paste("Preparing to download",wth,"WTH file from Stormy for customer",grower))
    
    #--- Read initial and end dates for the WTH file
    planting_dates_wth = as.Date(paste0(y_data$year_planting[y_data$site==grower & y_data$WTH_code==wth],"-01-01")) + y_data$doy_planting[y_data$site==grower & y_data$WTH_code==wth] - 1
    init_date_wth = planting_dates_wth - sim_start_bf
    init_date_wth = as.Date(paste0(format(min(init_date_wth),"%Y"),"-01-01"))
    harvesting_dates_wth = as.Date(paste0(y_data$year_harvesting[y_data$site==grower & y_data$WTH_code==wth],"-01-01")) + y_data$doy_harvesting[y_data$site==grower & y_data$WTH_code==wth] - 1
    end_date_wth = as.Date(paste0(format(max(harvesting_dates_wth),"%Y"),"-12-31"))
    wth_code     = unique(y_data$WTH_code[y_data$site==grower & y_data$WTH_code==wth])
    
    #--- formating WTH file name
    nyear     = sprintf("%02.0f", (as.numeric(format(end_date_wth,"%Y")) - as.numeric(format(init_date_wth,"%Y")) + 1))
    init_year = substr(format(init_date_wth,"%Y"),3,4)
    wth_nm    = paste0(wth_code,init_year,nyear)
    
    #--- mean lat/lon (because some fields can use the same WTH by being close together)
    lat = mean(y_data$lat[y_data$site==grower & y_data$WTH_code==wth])
    lon = mean(y_data$lon[y_data$site==grower & y_data$WTH_code==wth])
    
    #--- download WTH from Stormy
    d_link = gsub("<init_date>" ,init_date_wth,url_pat)
    d_link = gsub("<end_date>"  ,end_date_wth,d_link)
    d_link = gsub("<latitude>"  ,lat,d_link)
    d_link = gsub("<longitude>" ,lon,d_link)
    d_link = gsub("<WTH_ID>"    ,wth,d_link)
    
    #--- download directly on Weather directory
    download.file(url = d_link, destfile = paste0("C:/DSSAT",ds_v,"/Weather/",wth_nm,".WTH"))
    message(paste("Download of",wth_nm,"WTH file is completed."))
    
    #--- separate by type of soil
    l_soil = unique(y_data$DSSAT_Soil[y_data$site==grower & y_data$WTH_code==wth])
    for(soil in l_soil){
    
      #--- separate by cultivar
      l_cultivar = unique(y_data$cultivar_name_dssat[y_data$site==grower & y_data$DSSAT_Soil==soil & y_data$WTH_code==wth])
      for(cultivar in l_cultivar){
      
        fy_data = y_data[y_data$site==grower &
                         y_data$DSSAT_Soil==soil &
                         y_data$cultivar_name_dssat==cultivar &
                         y_data$WTH_code==wth,]
      
        #--- create a single xfile for each observation
        for(obs in 1:length(fy_data$cut)){
          
          setwd(wd)
          message(paste("Preparing Xfile for",grower,"grower, unique ID:",as.character(fy_data$unique_run_id[obs])))
          
          #--- reading the initial date os start of simulations
          pl_date = as.Date(paste0(fy_data$year_planting[obs],"-01-01")) + fy_data$doy_planting[obs] - 1
          init_date = pl_date - sim_start_bf
          ssdate = YYDOY(init_date)
        
          #--- read the xfile
          xfile = readLines(mast_xfile)
          crop_ID       = substr(mast_xfile,nchar(mast_xfile)-2,nchar(mast_xfile)-1)
        
          #--- format data for xfile
          t_id = as.character(fy_data$unique_run_id[obs])
          for(i in 1:(25 - nchar(t_id))){t_id = paste0(t_id," ")} # treatment ID is 25 characters length
          rowspacing = fy_data$rowspacing[obs]
          if(rowspacing<100){rowspacing = paste0(" ",rowspacing)}
        
          #--- modify master xfile 
          xfile = gsub("<tname_id>"   ,t_id,xfile)
          xfile = gsub("<ingeno>"     ,fy_data$ingeno_dssat[obs],xfile)
          xfile = gsub("<cname>"      ,fy_data$cultivar_name_dssat[obs],xfile)
          xfile = gsub("<station>"    ,wth,xfile)
          xfile = gsub("<soil_id>"    ,fy_data$DSSAT_Soil[obs],xfile)
          xfile = gsub("<pyr>"        ,substr(fy_data$year_planting[obs],3,4),xfile)
          xfile = gsub("<pdoy>"       ,sprintf("%003.0f", fy_data$doy_planting[obs]),xfile)
          xfile = gsub("<plme>"       ,fy_data$plme[obs],xfile)
          xfile = gsub("<rowspacing>" ,rowspacing,xfile)
          xfile = gsub("<hyr>"        ,substr(fy_data$year_harvesting[obs],3,4),xfile)
          xfile = gsub("<hdoy>"       ,sprintf("%003.0f", fy_data$doy_harvesting[obs]),xfile)
          xfile = gsub("<nyr>"        ," 1",xfile) # a single run per observation
          xfile = gsub("<sspyr>"      ,substr(ssdate,1,2),xfile)
          xfile = gsub("<sspdoy>"     ,substr(ssdate,3,5),xfile)
        
          #--- write xfile
          write(xfile,paste0("C:/DSSAT", ds_v, "/", crop, "/", paste0(wth,sprintf("%0004.0f", obs)),".",crop_ID,"X"))
          
          message(paste("The Xfile",paste0(wth,sprintf("%0004.0f", obs)),"for",grower,"grower, unique ID:",as.character(fy_data$unique_run_id[obs]),"was successfully created."))
        
          if(obs == 1){
            l_xfile = data.frame(xfile = paste0(fy_data$WTH_code[obs],sprintf("%0004.0f", obs)),
                                 run_id= as.character(fy_data$unique_run_id[obs]))
          }else{
            l_xfile = rbind(l_xfile,data.frame(xfile = paste0(fy_data$WTH_code[obs],sprintf("%0004.0f", obs)),
                                               run_id= as.character(fy_data$unique_run_id[obs])))
          }
        }
        
        #--- prepare batch call
        bfile = readLines(paste(wd, "/DSSBatch_Master.v47", sep = ""))
        bfile_head  = bfile[1:3]
        bfile_rep   = bfile[4]
        
        bfile_call = bfile_head
        for(xfile_nm in l_xfile$xfile){
          
          bfile_call = c(bfile_call,gsub("<calib_xfile>", paste0(xfile_nm,".",crop_ID,"X"), bfile_rep))
        }
        #--- write in Crop folder
        write(bfile_call,file = paste("C:/DSSAT", ds_v, "/", crop, "/", "DSSBatch.v", ds_v, sep = ""))
        
        #--- set wd to run
        setwd(paste("C:/DSSAT", ds_v, "/", crop, "/", sep = ""))
        
        #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
        system(paste0("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste0("DSSBatch.v", ds_v)))
        
        message("-------------------------------------------------------")
        message(paste("DSSAT Completed Simulations for",grower,"grower"))
        message(paste("WTH:     ",wth))
        message(paste("Soil:    ",soil))
        message(paste("Cultivar:",cultivar))
        message("-------------------------------------------------------")
        
        message(paste("Reading PlantGro.OUT"))
        
        #--- Read simulated data
        plant_lines = readLines("PlantGro.OUT")
        
        setwd(wd)
        
        #--- PlantGro Head
        pgro_head = read.csv(plantgro_fh)
        
        #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
        write.table(
          plant_lines[substr(plant_lines, 2, 3) == "19" |
                        substr(plant_lines, 2, 3) == "20"],
          file = "PlantGro_numeric.OUT",
          row.names = F,
          col.names = F,
          quote = F
        )
        plant = read.table(file = "PlantGro_numeric.OUT")                   #Read numeric lines as data.frame
        file.remove("PlantGro_numeric.OUT")
        #--- Columns name accordingly to DSSAT output name
        colnames(plant) = pgro_head$R_head
        
        message(paste("Plantgro indexing Unique_RUN_ID"))
        
        #--- Read Runs (last year series)
        run = trimws(substr(plant_lines[substr(plant_lines, 2, 4) == "TRE"], 18, 43))
        
        #--- Index outputs with treatments
        plant$run = ""
        j = 0
        for (i in 1:length(plant$dap)) {
          if (plant$dap[i] == 0) {
            j = j + 1
          }
          plant$run[i] = run[j]
        }
        
       message("Retrieving corresponding simulated data from PlantGro.OUT")
       
       l_uid = unique(plant$run)
       for(uid in l_uid){
         
         #--- check if there is more than one output to be retrieved
         l_out = unique(fy_data$dssat_output)
         
            for(out in l_out){
              out_pg = as.character(pgro_head$R_head[pgro_head$PlantGro_head==out])
              if(uid == l_uid[1]){
                fy_out = data.frame(uid = uid,
                                out = out,
                                dssat_out = plant[plant$run==uid & plant$dap==max(plant$dap[plant$run==uid]),out_pg])
              }else{
                fy_out = rbind(fy_out,data.frame(uid = uid,
                                out = out,
                                dssat_out = plant[plant$run==uid & plant$dap==max(plant$dap[plant$run==uid]),out_pg]))
              }
            }
          }
          #--- gather data by cultivar
          message("Gathering data by Cultivar")
          if(cultivar == l_cultivar[1]){
            fy_out_cv = fy_out
          }else{
            fy_out_cv = rbind(fy_out_cv,fy_out)
          }
        }
        #--- gather data by soil
      message("Gathering data by Soil")
        if(soil == l_soil[1]){
          fy_out_cv_sl = fy_out_cv
        }else{
          fy_out_cv_sl = rbind(fy_out_cv_sl,fy_out_cv)
        }
      }
    
    #--- gather data by WTH
    message("Gathering data by Weather")
    if(wth == l_wth[1]){
      fy_out_cv_sl_wth = fy_out_cv_sl
    }else{
      fy_out_cv_sl_wth = rbind(fy_out_cv_sl_wth,fy_out_cv_sl)
    }
  }
  
  #--- gather data by grower
  message("Gathering data by grower")
  if(grower == l_grower[1]){
    fy_out_grower = fy_out_cv_sl_wth
  }else{
    fy_out_grower = rbind(fy_out_grower,fy_out_cv_sl_wth)
  }
}

message("Joining with observed data")
#--- merge with observed data
colnames(fy_out_grower) = c("unique_run_id","dssat_output","dssat_canegro_output")
y_data_out = merge(y_data,fy_out_grower, by = c("unique_run_id","dssat_output"))

write.csv(y_data_out,file = "Yield_data_sccan_out.csv")
message("Outputs are saved on file Yield_data_sccan_out.csv")

y_data_out$dec = y_data_out$measured_data / y_data_out$dssat_canegro_output

if(length(y_data_out$dec[y_data_out$dec>1&y_data_out$cut>0])>0){
  
  message(paste0(length(y_data_out$dec[y_data_out$dec>1&y_data_out$cut>0]),
                 " of ",length(y_data_out$dec[y_data_out$cut>0])," reported yields are higher than simulated"))
  message("Unique IDs:")
  for(i in y_data_out$unique_run_id[y_data_out$dec>1&y_data_out$cut>0]){
    message(paste0(i))
  }
  message("These data above will be neglected for the kdec calibration")
}

#--- dec values
dec_data_all = y_data_out[y_data_out$dec<1&y_data_out$cut>0,c("site","cultivar","cut","measured_data","dssat_canegro_output","dec")]

#--- optimize kdec
l_grower_kdec = unique(dec_data_all$site)
for(grower in l_grower_kdec){
  
  if(opt_per_cv){
    l_cultivar_kdec = unique(dec_data_all$cultivar[dec_data_all$site==grower])
    for(cultivar in l_cultivar_kdec){
      dec_data = dec_data_all[dec_data_all$site==grower & dec_data_all$cultivar==cultivar,c("site","cultivar","cut","measured_data","dssat_canegro_output","dec")]
      if(length(dec_data$dec)==0){next}
    
      kdec_opt = optim(0.25,calib_kdec, method = "Brent", lower = 0, upper = 1)$par
    
      if(cultivar == l_cultivar_kdec[1]){
        kdec_opt_df_cv = data.frame(grower = grower,
                             cultivar = cultivar,
                             kdec_opt = kdec_opt)
      }else{
        kdec_opt_df_cv = rbind(kdec_opt_df_cv,data.frame(grower = grower,
                                                       cultivar = cultivar,
                                                       kdec_opt = kdec_opt))
      }
    }
  }else{
    dec_data = dec_data_all[dec_data_all$site==grower,c("site","cultivar","cut","measured_data","dssat_canegro_output","dec")]
    if(length(dec_data$dec)==0){
      message("None observed data is valid, kdec was not optmized.")
      next
    }
    kdec_opt = optim(0.25,calib_kdec, method = "Brent", lower = 0, upper = 1)$par
    kdec_opt_df_cv = data.frame(grower = grower,
                                cultivar = cultivar,
                                kdec_opt = kdec_opt)
  }
  if(grower == l_grower_kdec[1]){
    kdec_opt_df = kdec_opt_df_cv
  }else{
    kdec_opt_df = rbind(kdec_opt_df,kdec_opt_df)
  }
}
if(!opt_per_cv){kdec_opt_df$cultivar= NULL}

plot(f_kdec(seq(1,5),kdec_opt_df$kdec_opt)~seq(1,5),type = "l",ylim = c(0,1))
points(dec_data_all$dec~dec_data_all$cut)

