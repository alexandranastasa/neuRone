ghk_resting_pot<-function(units, `T`, perm_Na, conc_Na_in, conc_Na_out, perm_K, conc_K_in, conc_K_out, 
                          perm_Cl, conc_Cl_in, conc_Cl_out, resting_pot){
  if(missing(units)){
    R=8.31446261815324
    `F`=96485.33212
  }
  if(missing(`T`)){
    `T`=310.15
  }
  if(missing(resting_pot)){
    resting_pot = (((R*`T`)/`F`)*log((perm_Na*conc_Na_out+perm_K*conc_K_out+perm_Cl*conc_Cl_in)/
                                       (perm_Na*conc_Na_in+perm_K*conc_K_in+perm_Cl*conc_Cl_out)))
    system<-c("T (K)"=`T`,"Permeability Na+"=perm_Na, "[Na+]in"=conc_Na_in, "[Na+]out"=conc_Na_out,
              "Permeability K+"=perm_K, "[K+]in"=conc_K_in, "[K+]out"=conc_K_out,
              "Permeability Cl-"=perm_Cl, "[Cl-]in"=conc_Cl_in, "[Cl-]out"=conc_Cl_out,
              "Resting Potential (V)"=resting_pot)
    
  }
  
  return(system)
  
  
}