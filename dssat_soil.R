

#--- Check soil

soil_db_fln   = "BR.sol"
mast_xfile    = "KDEC0001_master.SCX"
ds_v          = 47                  # DSSAT version
crop          = "Sugarcane"         # Crop used
sim_start_bf  = 6 * 30
y_csvfile     = "y_data.csv"
slro_fln      = "SLRO.csv"
msol_fn       = "temp.sol"
smsol_fn      = "temp_sol.csv"             


wd            ="C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/kdec/kdec_sc_soil"

xfile         = readLines(paste0(wd,"/",mast_xfile))
crop_ID       = substr(mast_xfile,nchar(mast_xfile)-2,nchar(mast_xfile)-1)

slope         = 5.52 #%
slro_db = read.csv(paste0(wd,"/",slro_fln))

latlon_s = 7
latlon_p = 3
max_dp_s = 3



#--------------------------
#--- DEBUG FUNCTION


#--- Input soil parameters
soil_id   	= "BRWFRBR001" 
max_dp    	= 180
site      	= "Campinas"
country   	= "Brazil"
lat       	= -21.1651651651
lon       	= -47.6515651651
soil_class	= "Latossolo"
salb      	= 0.1235
slu1      	= 6.40
sldr      	= 0.50
slro      	= 79.0
slnf      	= 1.00
slpf      	= 1.00

slb       	= c(10,20,30)
slmh      	= c("A","B","C")
slll      	= c(0.200,0.200,0.200)
sdul      	= c(0.300,0.300,0.300)
ssat      	= c(0.400,0.400,0.400)
srgf      	= c(1.00,0.50,0.01)
ssks      	= c(20,20,20)
sbdm      	= c(1,1,1)
sloc      	= c(1,1,1)
slcl      	= c(10,10,10)
slsi      	= c(5,5,5)
slcf      	= c(-99,-99,-99)
slni      	= c(-99,-99,-99)
slhw      	= c(-99,-99,-99)
slhb      	= c(-99,-99,-99)
scec      	= c(-99,-99,-99)
sadc      	= c(-99,-99,-99)
f_head = T
#----------------------


#--- Create the soil profile
ds_sol(soil_id,max_dp,site,country,lat,lon,soil_class,
       salb,slu1,sldr,slro,slnf,slpf,slb,slmh,slll,sdul,
       ssat,srgf,ssks,sbdm,sloc,slcl,slsi,slcf,slni,slhw,
       slhb,scec,sadc,f_head = F)


ds_sol = function(soil_id,max_dp,site,country,lat,lon,soil_class,
                  salb,slu1,sldr,slro,slnf,slpf,slb,slmh,slll,sdul,
                  ssat,srgf,ssks,sbdm,sloc,slcl,slsi,slcf,slni,slhw,
                  slhb,scec,sadc,wd,f_head){
  
  #-------------------------------------------
  #------ Soil profile function (.sol) -------
  #-------------------------------------------
  
  #--- Arguments of this function:
  #-----	soil_id   	(single value)
  #-----	max_dp    	(single value)
  #-----	site      	(single value)
  #-----	country   	(single value)
  #-----	lat       	(single value)
  #-----	lon       	(single value)
  #-----	soil_class	(single value)
  #-----	salb      	(single value)
  #-----	slu1      	(single value)
  #-----	sldr      	(single value)
  #-----	slro      	(single value)
  #-----	slnf      	(single value)
  #-----	slpf      	(single value)
  #-----	slb       	(vector with same size)
  #-----	slmh      	(vector with same size)
  #-----	slll      	(vector with same size)
  #-----	sdul      	(vector with same size)
  #-----	ssat      	(vector with same size)
  #-----	srgf      	(vector with same size)
  #-----	ssks      	(vector with same size)
  #-----	sbdm      	(vector with same size)
  #-----	sloc      	(vector with same size)
  #-----	slcl      	(vector with same size)
  #-----	slsi      	(vector with same size)
  #-----	slcf      	(vector with same size)
  #-----	slni      	(vector with same size)
  #-----	slhw      	(vector with same size)
  #-----	slhb      	(vector with same size)
  #-----	scec      	(vector with same size)
  #-----	sadc      	(vector with same size)
  #-----  wd          Working directory (optional)
  #-----  f_head      Logical if the file head need to be outputed
  
  #--- Constants:
  #-----  soil template file temp.sol
  #-----  soil build-up setup file temp_sol.csv
  
  #----- Contact: murilodsv@gmail.com
  #--------------------------------------------
  
  #--- Check input arguments 
  defined   = ls()
  optional  = c("wd","f_head")
  mandatory = defined[!(defined %in% optional)]
  passed    = names(as.list(match.call())[-1])
  
  if(any(!mandatory %in% passed)){
    stop(paste("Missing values for", paste(setdiff(defined, passed), collapse=", "),", please inform missing data as -99.0."))
  }
  
  #--- Check optional arguments
  if(missing(wd)){wd = getwd()}
  if(missing(f_head)){f_head = F}
  
  #--- Check constants
  if(!file.exists(paste0(wd,"/temp.sol"))){
    stop(paste("Master soil file temp.sol not found in:",wd))
  }
  if(!file.exists(paste0(wd,"/temp_sol.csv"))){
    stop(paste("Master soil file temp_sol.csv not found in:",wd))
  }
  
  #--- master file
  msol      = c(
  "*Soils: Murilo Vianna (MV) Soil Template for DSSAT",
  "",
  "*<soil_id>  TEMP        C       <max_dp> TEMP DATABASE",
  "@SITE        COUNTRY          LAT     LONG SCS FAMILY",
  " <site> <country> <lat>  <lon> <soil_class>",
  "  @ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE",
  "      BN <salb> <slu1> <sldr> <slro> <slnf> <slpf> SA001 SA001 SA001",
  "@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC",
  " <slb> <slmh> <slll> <sdul> <ssat> <srgf> <ssks> <sbdm> <sloc> <slcl> <slsi> <slcf> <slni> <slhw> <slhb> <scec> <sadc>"
  )
  
  
  #--- Separate soil sub-blocks
  msol_head = msol[1:2]
  msol_id   = msol[3]
  msol_inf  = msol[4:5]
  msol_pdat = msol[6:7]
  msol_lhead= msol[8]
  msol_ldat = msol[9]
  
  #--- Read .sol setup
  smsol     = data.frame(sol_var = c("<soil_id>","<max_dp>","<site>","<country>","<lat>","<lon>","<soil_class>","<salb>","<slu1>","<sldr>","<slro>","<slnf>","<slpf>",
                                     "<slb>","<slmh>","<slll>","<sdul>","<ssat>","<srgf>","<ssks>","<sbdm>","<sloc>","<slcl>","<slsi>","<slcf>","<slni>","<slhw>","<slhb>","<scec>","<sadc>"),
                         var_s   = c(10,3,11,12,7,7,25,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5),
                         var_p   = c(0,0,0,0,3,3,0,2,2,2,2,2,2,0,0,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2),
                         var_t   = c("C","I","C","C","R","R","C","R","R","R","R","R","R","I","C","R","R","R","R","R","R","R","R","R","R","R","R","R","R","R"))
  
  if(!any(colnames(smsol) %in% "sol_var")){
    stop(paste0("Collumn  'sol_var' is missing in file: '",wd,"/temp_sol.csv'. Please check the file."))
  }
  
  #--- Pass single variable to dataframe for checking format
  soil_data_single  = 
    data.frame(sol_var = 
                 c("<soil_id>","<max_dp>","<site>","<country>","<lat>","<lon>","<soil_class>","<salb>","<slu1>","<sldr>","<slro>","<slnf>","<slpf>"),
               sol_val = 
                 as.character(c(soil_id,max_dp,site,country,lat,lon,soil_class,salb,slu1,sldr,slro,slnf,slpf)))
  
  soil_data_single$sol_val = as.character(soil_data_single$sol_val)
  
  #--- single data values master
  sol_sdat = c(msol_id,msol_inf,msol_pdat)
  
  #--- merge with temp_sol.csv
  fmt_dt = merge(smsol,soil_data_single,by = "sol_var")
  
  #--- Check format
  for(d in fmt_dt$sol_var){
    
    val_d     = fmt_dt$sol_val[fmt_dt$sol_var==d]
    size_fmt  = fmt_dt$var_s[fmt_dt$sol_var==d]
    type_d    = as.character(fmt_dt$var_t[fmt_dt$sol_var==d])
    prec_d    = fmt_dt$var_p[fmt_dt$sol_var==d]
    size_d    = nchar(val_d)
  
    if(type_d == "R"){
      #--- Float data
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",prec_d,"f"), as.numeric(val_d))
        
      if(nchar(val_d_fmt) > size_fmt){
        if(grepl(".", val_d_fmt)){
          warning(paste0("Input parameter: ",d,":",val_d," is too high for DSSAT fmt."))
        }else{
          stop(paste0("Input parameter: ",d,":",val_d," has precision = 0 and is too high for DSSAT fmt. Please review value and precision in file temp_sol.csv"))
        }
      }
      
      if(!grepl(".", val_d_fmt)){
        warning(paste0("Input parameter: ",d,":",val_d," has precision = 0 on the temp_sol.csv file. One point precision was added to meet DSSAT fmt."))
        val_d_fmt =
          sprintf(paste0("%",size_fmt,".",1,"f"), as.numeric(val_d))
      }
      
    }else if(type_d == "I"){
      #--- Integer data
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",0,"f"), as.numeric(val_d))
      
      if(nchar(val_d_fmt) > size_fmt){
          warning(paste0("Input parameter: ",d,":",val_d," is too high. Data is reduced. Please review value and unit (cm) of inputed data"))
          val_d_fmt = substr(val_d_fmt,1,size_fmt)    
      }
      
    }else if(type_d == "C"){
      #--- Character data
      val_d_fmt = substr(val_d, 1,size_fmt)
      if(nchar(val_d)>size_fmt){
        warning(paste0("Input parameter: ",d,":",val_d," was trimmed to meet DSSAT fmt. New value: '",val_d_fmt,"'"))
      }
      
      if(nchar(val_d_fmt) < size_fmt){
        for(i in 1:(size_fmt - nchar(val_d_fmt))){val_d_fmt = paste0(val_d_fmt," ")}
      }
      
    }else{
      #--- Unkown type
      stop(paste0("Unknown type of data for parameter: ",d,". Make sure file temp_sol.csv has only var_t == ['C','I','R']."))
    }
    
    sol_sdat = gsub(d,val_d_fmt,sol_sdat)
  }
  
  
  #--- layered data values master head
  sol_ldat = msol_lhead
  
  #--- Check format
  for(sl in 1:length(slb)){
  
    #--- Pass layered variable to dataframe for checking format
    soil_data_layered  = 
      data.frame(sol_var = 
                   c("<slb>","<slmh>","<slll>","<sdul>","<ssat>","<srgf>","<ssks>","<sbdm>","<sloc>","<slcl>","<slsi>","<slcf>","<slni>","<slhw>","<slhb>","<scec>","<sadc>"),
                 sol_val = 
                   as.character(c(slb[sl],slmh[sl],slll[sl],sdul[sl],ssat[sl],srgf[sl],ssks[sl],sbdm[sl],sloc[sl],slcl[sl],slsi[sl],slcf[sl],slni[sl],slhw[sl],slhb[sl],scec[sl],sadc[sl])))
    
    soil_data_layered$sol_val = as.character(soil_data_layered$sol_val)
    
    #--- merge with temp_sol.csv
    fmt_dt = merge(smsol,soil_data_layered,by = "sol_var")
    
    msol_ldat_sl = msol_ldat
    
    for(d in fmt_dt$sol_var){    
      val_d     = fmt_dt$sol_val[fmt_dt$sol_var==d]
      size_fmt  = fmt_dt$var_s[fmt_dt$sol_var==d]
      type_d    = as.character(fmt_dt$var_t[fmt_dt$sol_var==d])
      prec_d    = fmt_dt$var_p[fmt_dt$sol_var==d]
      size_d    = nchar(val_d)
      
    if(type_d == "R"){
      #--- Float data
      
      if(as.numeric(val_d) == -99.0){
        #--- use precision equal to zero
        prec_d = 1
      }
      
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",prec_d,"f"), as.numeric(val_d))
      
      if(nchar(val_d_fmt) > size_fmt){
        if(grepl(".", val_d_fmt)){
          warning(paste0("Input parameter: ",d,":",val_d," is too high for DSSAT fmt."))
        }else{
          stop(paste0("Input parameter: ",d,":",val_d," has precision = 0 and is too high for DSSAT fmt. Please review value and precision in file temp_sol.csv"))
        }
      }
      
      if(!grepl(".", val_d_fmt)){
        warning(paste0("Input parameter: ",d,":",val_d," has precision = 0 on the temp_sol.csv file. One point precision was added to meet DSSAT fmt."))
        val_d_fmt =
          sprintf(paste0("%",size_fmt,".",1,"f"), as.numeric(val_d))
      }
      
    }else if(type_d == "I"){
      #--- Integer data
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",0,"f"), as.numeric(val_d))
      
      if(nchar(val_d_fmt) > size_fmt){
        warning(paste0("Input parameter: ",d,":",val_d," is too high. Data is reduced. Please review value and unit (cm) of inputed data"))
        val_d_fmt = substr(val_d_fmt,1,size_fmt)    
      }
      
    }else if(type_d == "C"){
      #--- Character data
      val_d_fmt = substr(val_d, 1,size_fmt)
      if(nchar(val_d)>size_fmt){
        warning(paste0("Input parameter: ",d,":",val_d," was trimmed to meet DSSAT fmt. New value: '",val_d_fmt,"'"))
      }
      
      if(nchar(val_d_fmt) < size_fmt){
        for(i in 1:(size_fmt - nchar(val_d_fmt))){val_d_fmt = paste0(" ",val_d_fmt)}
      }
      
    }else{
      #--- Unkown type
      stop(paste0("Unknown type of data for parameter: ",d,". Make sure file temp_sol.csv has only var_t == ['C','I','R']."))
    }
      
      #--- merge
      msol_ldat_sl = gsub(d,val_d_fmt,msol_ldat_sl)
    
    }
    
    sol_ldat = c(sol_ldat,msol_ldat_sl)
    
  }
  
  soil_p = c(sol_sdat,sol_ldat)
  
  if(f_head){soil_p = c(msol_head,soil_p)}
  
  return(soil_p)
}




write(c(msol_head,msol_id,msol_inf,msol_pdat,msol_lhead,msol_ldat),file = paste0(wd,"/","t.t"))


soil_db = readLines(paste0(wd,"/",soil_db_fln))
soil_ids = substr(soil_db[substr(soil_db,1,1)=="*"],2,11)

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
