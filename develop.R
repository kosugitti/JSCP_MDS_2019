
# matrix file maker -------------------------------------------------------


image.set <- read.csv("travel_image.csv",header = F)
image.set %>% dplyr::select(-V46) %>% 
  mutate(ID=rownames(.)) %>% 
  tidyr::gather(key,val,-ID,factor_key=TRUE) %>% 
  group_by(key) %>% 
  summarise(VA=var(val)) %>% 
  arrange(desc(VA))

image.set %>% mutate(ID=rownames(.)) %>% 
  filter(ID %in% c(9,14,16,19,35)) -> M

for(i in 1:5){
  tmp <- matrix(0,nrow=10,ncol=10)
  pos <- 0
  for(j in 1:9){
    for(k in (j+1):10){
      pos <- pos + 1
      tmp[j,k] <- M[i,pos]
    }
  }
  diag(tmp) <- 0
  tmp <- t(tmp)+(tmp)
  colnames(tmp) <-  c("Sapporo","Hida","Maizuru","Sasebo","Shima","Akiyoshidai",
                      "Nozawa","Dougo","Yufuin","Miyakojima")
  write.csv(tmp,file = paste0("matrix",i,".csv"),row.names = F)
}