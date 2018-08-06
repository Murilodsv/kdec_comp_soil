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
  "@   SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE",
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
          if(as.numeric(val_d_fmt)!=-99){message(paste0("Input parameter: ",d,":",val_d," is too high for DSSAT fmt."))}
        }else{
          stop(paste0("Input parameter: ",d,":",val_d," has precision = 0 and is too high for DSSAT fmt. Please review value and precision in file temp_sol.csv"))
        }
      }
      
      if(!grepl(".", val_d_fmt)){
        message(paste0("Input parameter: ",d,":",val_d," has precision = 0 on the temp_sol.csv file. One point precision was added to meet DSSAT fmt."))
        val_d_fmt =
          sprintf(paste0("%",size_fmt,".",1,"f"), as.numeric(val_d))
      }
      
    }else if(type_d == "I"){
      #--- Integer data
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",0,"f"), as.numeric(val_d))
      
      if(nchar(val_d_fmt) > size_fmt){
        message(paste0("Input parameter: ",d,":",val_d," is too high. Data is reduced. Please review value and unit (cm) of inputed data"))
          val_d_fmt = substr(val_d_fmt,1,size_fmt)    
      }
      
    }else if(type_d == "C"){
      #--- Character data
      val_d_fmt = substr(val_d, 1,size_fmt)
      if(nchar(val_d)>size_fmt){
        message(paste0("Input parameter: ",d,":",val_d," was trimmed to meet DSSAT fmt. New value: '",val_d_fmt,"'"))
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
          message(paste0("Input parameter: ",d,":",val_d," is too high for DSSAT fmt."))
        }else{
          stop(paste0("Input parameter: ",d,":",val_d," has precision = 0 and is too high for DSSAT fmt. Please review value and precision in file temp_sol.csv"))
        }
      }
      
      if(!grepl(".", val_d_fmt)){
        message(paste0("Input parameter: ",d,":",val_d," has precision = 0 on the temp_sol.csv file. One point precision was added to meet DSSAT fmt."))
        val_d_fmt =
          sprintf(paste0("%",size_fmt,".",1,"f"), as.numeric(val_d))
      }
      
    }else if(type_d == "I"){
      #--- Integer data
      val_d_fmt =
        sprintf(paste0("%",size_fmt,".",0,"f"), as.numeric(val_d))
      
      if(nchar(val_d_fmt) > size_fmt){
        message(paste0("Input parameter: ",d,":",val_d," is too high. Data is reduced. Please review value and unit (cm) of inputed data"))
        val_d_fmt = substr(val_d_fmt,1,size_fmt)    
      }
      
    }else if(type_d == "C"){
      #--- Character data
      val_d_fmt = substr(val_d, 1,size_fmt)
      if(nchar(val_d)>size_fmt){
        message(paste0("Input parameter: ",d,":",val_d," was trimmed to meet DSSAT fmt. New value: '",val_d_fmt,"'"))
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
