library(purrr)
library(tidyverse)
library(imager)

#questiin to be adressed: how much information is in an image of Mars? 
rochette=load.image("https://mars.nasa.gov/system/resources/detail_files/26227_PIA24839_Main_stretched-web.jpg")
#blobs
layout(t(1:1))
rbinom(100*100,1,.001) %>% as.cimg(x=100,y=100)
blobs <- isoblur(points,5)
nblobs <- blobs+.0001*imnoise(dim=dim(blobs))
df <- nblobs %>% as.data.frame()
ggplot(df,aes(x,y))+geom_raster(aes(fill=value)) + scale_fill_gradientn(colours=c("peru","tan4"))
Hdet <- with(imhessian(blobs),(xx*yy - xy^2)) 
hdet_df<-Hdet %>% as.data.frame()
ggplot(hdet_df,aes(x,y))+geom_raster(aes(fill=value)) + scale_fill_gradientn(colours=c("peru","tan4"))
test=threshold(Hdet,"99%") %>%as_data_frame()
ggplot(test,aes(x,y))+geom_tile(aes(fill=cc)) + scale_fill_gradientn(colours=c("peru","tan4"))

lab <- threshold(Hdet,"99%") %>% label

data <- as.data.frame(lab) %>% subset(value>0)
head(df,3)
ggplot(data,aes(x,y))+geom_tile(aes(fill=value)) + scale_fill_gradientn(colours=c("peru","tan4"))
centers <- dplyr::group_by(data,value) %>% dplyr::summarise(mx=mean(x),my=mean(y))

ggplot(centers,aes(mx,my))+geom_point(aes(fill=value)) + scale_fill_gradientn(colours=c("peru","tan4")) 

#overlay:
ggplot()  + geom_raster(data=df, aes(x=x, y=y, fill=value)) + scale_fill_gradientn(colours=c("peru","tan4")) + geom_point(data=centers, aes(x=mx, y=my)) +  geom_line(data=centers, aes(x=mx, y=my),alpha=0.2)



#the real deal:
get.centers <- function(im,thr="99%")
{
  dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
}

rochette.denoised <- isoblur(rochette,5)
plot(rochette.denoised)
c=get.centers(rochette.denoised,"99%") 
ggplot()  + geom_raster(data=exp, aes(x=x, y=y, fill=value))  + geom_point(data=c, aes(x=mx, y=my)) +  geom_line(data=c, aes(x=mx, y=my),alpha=0.2)

exp <- as.data.frame(rochette.denoised,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))


#overlay:
ggplot()  + geom_raster(data=exp, aes(x=x, y=y, fill=rgb.val)) +scale_fill_identity()+scale_y_reverse() + geom_point(data=c, aes(x=mx, y=my,alpha=value)) 
#general rock distribution 
ggplot()  + geom_raster(data=exp, aes(x=x, y=y, fill=rgb.val),alpha=0.8) +scale_fill_identity()+scale_y_reverse() + geom_point(data=c, aes(x=mx, y=my,alpha=value)) + geom_smooth(data=c, aes(x=mx, y=my,alpha=value)) + theme(legend.position="none")