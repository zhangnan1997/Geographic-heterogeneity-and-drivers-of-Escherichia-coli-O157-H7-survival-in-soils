library(plspm)
#inputdata
dat=read.csv("inputdata.csv",header = T, row.names = 1)
dat=scale(dat,center = T,scale=T)

#Specify latent variables, storing the relationships between variables and latent variables as a list in R
dat_blocks <- list(
  survivaltime = c('survivaltime'), 
  clamite_facotrs= c("MAT","MAP"), 
  AP = c('AP'),
  microbial_community = c('PCoA1',"PCoA2"),
  key_spe =c('Aerococcus.viridans',"Enterococcus.cecorum","Enterococcus.faecium","Enterococcus.durans"),
  diversity =c("Richness","Shannon","evenness"),
  Pgenes=c("Pgene"))
dat_blocks

#Describe the relationships between latent variables using a 0-1 matrix, where 0 indicates no relationship between variables and 1 indicates a relationship
clamite_facotrs <- c(0, 0,0,0,0,0,0)
AP <- c(1, 0, 0,0,0,0,0)
microbial_community <- c(1, 1, 0,0,0,0,0)
key_spe <- c(1, 1, 1,0,0,0,0)
diversity <- c(1, 1, 1,0,0,0,0)
Pgenes <- c(1,1, 0,0,1,0,0)
survivaltime<- c(1, 1, 1,1,1,1,0)
dat_path <- rbind( clamite_facotrs, AP,microbial_community,key_spe,diversity,Pgenes,survivaltime)
colnames(dat_path) <- rownames(dat_path)
dat_path

#Specify causal relationships, with options A (indicating the column is the cause of the row) or B (indicating the row is the cause of the column)
dat_modes <- rep('A', 7)
dat_modes

##PLS-PM
dat_pls <- plspm(dat, dat_path, dat_blocks, modes = dat_modes,boot.val = T, tol = 1e-06)
dat_pls
summary(dat_pls)
dat_pls$gof          #model gof
a=dat_pls$path_coefs   #direct effect
dat_pls$effects      #direct, indirect and total effect
dat_pls$inner_model  #p


