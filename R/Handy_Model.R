#' Handy Initialization
#'
#' A function that defines the parameter setting of the handy model, and run simulations for a desired set of years
#'
#' @param  
#' @author Alan Fortuny Sicart <msalanfortuny@@gmail.com>
#' @return A table with the simulation results
#'

Handy_Model <- function(beta_c =0.03,
                        xc_0=100,
                        beta_e = 0.03,
                        xe_0=c(0,1,25),
                        gamma=0.01,
                        y_0=100,
                        lambda=100,
                        s=0.0005,
                        kappa=c(1,10,100),
                        rho=0.005,
                        alphaM=0.07,
                        alpham=0.01,
                        inequaity_scenario=c("Egalitarian"),
                        consumption_scenario= C("optimal"),
                        time_rage = 1000){
 
  
  beta_c =0.03
  xc_0=100
  beta_e = 0.03
  xe_0=c(0,1,25)
  gamma=0.01
  y_0=100
  lambda=100
  s=0.0005
  kappa=c(1,10,100)
  rho=0.005
  alphaM=0.07
  alpham=0.01
  inequaity_scenario=c("Egalitarian")
  consumption_scenario= c("optimal")
  
  time = 0:1000
  
#####################################################
############# Create Similation table

simulation <-as.data.frame(matrix(ncol=6,nrow=length(time)))    
colnames(simulation)<-c("time","commoners","elites","wealth","natural_capital","carrying_capacity")  
simulation$time<-time  
    
  
#####################################################
###Define the equations #############################
#####################################################
  
  
  
  
#detah rate commoner

alphac <- formula(alpham+max(0,1-Cc/(s*xc))*(alphaM-alpham))

#detah rate elite

alphae <- alpham+max(0,1-Ce/(s*xe))*(alphaM-alpham)


#common population change per unit of time  
xc_dot <-   beta_c*xc-alphac*xc
#Elite popultion change
xe_dot <-   beta_e*xe-alphae*xe
# natural capital change  
y_dot <- gamma*y*(lambda-y)-delta*xc*y
#wealth change
w_dot <-delta*xc*y-Cc-Ce

#consumption commoners
Cc <- min(1,w/wth)*s*xc
#comsumption elites
Ce <- min(1,w/wth)*s*kappa*xe

#minimum income to avoid famine
wth <- rho*xc+kappa*xe*rho



#dimensionless parameter required for an equilibrium to exists

nu <- (alphaM-beta_c)/(alphaM-alpham)


#########################################################
################# Define the equilibrium values
#########################################################


#################### Egaliarian society equilibrium Xe=0


Xc_e <- (gamma/delta)*(lambda-nu*(s/delta))

y_e <- nu*(s/delta)

w_e <- nu*rho*Xc_e

#carrying capacity

X <- (gamma/delta)*(lambda-nu*(s/delta))
#arrying capacity is maximized when 

delta_opt <- (2*nu*s)/lambda

Xm <- (gamma/delta_opt)*(lambda/2)


#####################################################################
################# Egalitaria Society: Soft Landing to Equilibrium
#####################################################################


delta <-delta_opt


for(t in time){
  
  if(t==0){
    
    simulation[t,c("commoners")] <- xc <- xc_0
    simulation[t,c("elites")]    <- xe <- xe_0
    simulation[t,c("wealth")]    <- w  <- 0  
    simulation[t,c("natural_capital")] <- y <- y_0
    simulation[,c("carrying_capacity")] <- Xm
    
    
    
    
    
  }else{
      
    simulation[t,c("commoners")] <- xc+xc_dot 
    simulation[t,c("elites")] <-  xe+xe_dot
    simulation[t,c("wealth")] <-  w+w_dot
    simulation[t,c("natural_capital")] <- y+y_dot
   
    xc <-  simulation[t,c("commoners")]
    xe <-  simulation[t,c("elites")]
    w  <- simulation[t,c("wealth")]
    y <- simulation[t,c("natural_capital")]
    }
  
}
 

  
  
  
}
