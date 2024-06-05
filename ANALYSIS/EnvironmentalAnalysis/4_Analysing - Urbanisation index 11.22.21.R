################################################################################
#####################    Principal Component Analysis    #######################
################################################################################
## In this script we take all site characteristics and we aim to create an 
## an urbanization index. I use the model computed for a local scale (30m)
## for later analysis (saved as "PCAmodel.R"). 

#### Loading data ####
library("FactoMineR")
library("factoextra")
library("corrplot")
library("viridis")
library("ggplot2")
library('hrbrthemes')

load('./data/sitecharac_45')

#### PCA at local scale ####
#Local scale
site$L_BA[which(is.na(site$L_BA))] <- 0
site$L_BH[which(is.na(site$L_BH))] <- 0
sitePCA <- 
  site[,c('L_Grass',"L_Tree","L_Vege","L_Building","L_Urban", 'L_Residential',"L_BA", "L_BH")]
colnames(sitePCA) <- 
  c('Grass',"Trees","Veget.","Build.","Urban", 'Resid.',"B.Area", "B.Height")
Local <- prcomp(sitePCA,
                retx = TRUE,
                center = TRUE,
                scale = TRUE)
get_eigenvalue(Local)
var <- get_pca_var(Local)

corrplot(var$cos2, is.corr = FALSE)
corrplot(var$contrib, is.corr = FALSE)
fviz_pca_biplot(
  Local,
  label = "var",
  habillage = site$Habitat,
  addEllipses = TRUE,
  ellipse.level = 0.95)
+ggtheme = theme_minimal()

PCAViz <- fviz_pca_var(Local, axes = c(1, 2),
                       repel = TRUE # Évite le chevauchement de texte
                       )
                       
## Plot which goes in the paper
PCAViz +   
  ggtitle("")+
  geom_segment(aes(x = 0, y = 0, 
                   xend = var$coord[, 1], yend = var$coord[, 2],
                   col=var$contrib[,1]),
               size = 1, 
               arrow = arrow(length = unit(0.2, "cm")))+
  theme(axis.line = element_line(colour = "grey35", linetype = "solid"))+
  theme_ipsum(grid = "XY") +
  theme(plot.margin = unit(c(0.5, .5, 0.5, 0.5), "cm"), #top, right, bottom, left margin is the white zone which borders th whole figure
        panel.spacing = unit(c(-.1,0.2,-1,-.1), "cm"),
        panel.border = element_blank()) +
  theme(legend.position = "bottom",
        legend.box.margin = margin(-10, -10, -10, -10))+
  scale_color_gradient2(
    low = plasma(7)[2], 
    mid = plasma(7)[4],
    high = plasma(7)[6],
    midpoint=10,
    name = "Contribution to Dim1 (in %)",
    labels = as.character(seq(0, 20, 5)),
    breaks = seq(0,20,5),
    limits = c(0, 20),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,draw.llim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5))


site$L_Urbanisation <- get_pca_ind(Local)$coord[, 1] #green vs gray
site$L_Height <- get_pca_ind(Local)$coord[, 2] #Residential vs large buildings

save(site,file='./data/sitecharac_45' )

#### PCA at area scale ####

df_bird$A_BA[which(is.na(df_bird$A_BA))] <- 0  
df_bird$A_BH[which(is.na(df_bird$A_BH))] <- 0  

Area <- 
  PCA(df_bird[,c('A_Green',"A_Building","A_Tree", "A_Gray", 'A_Residential',"A_BA", "A_BH")], 
      scale.unit = TRUE, ncp = 5, graph = TRUE)
get_eigenvalue(Area)
var <- get_pca_var(Area)

corrplot(var$cos2, is.corr = FALSE)
corrplot(var$contrib, is.corr = FALSE)
fviz_pca_var(Local, col.var = "contrib", axes = c(1, 2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

df_bird$A_UImpervious <- get_pca_ind(Local)$coord[, 2] #residential vs big buildings
df_bird$A_USoc <- get_pca_ind(Local)$coord[, 1] #Residential + Green vs Big buildings



#### plotting at 2 scales ####
temp <- data.frame(variables = rep(c('impervious', 'economy'), each = nrow(df_bird)),
                   values = c(df_bird$A_UImpervious, df_bird$A_USoc))
ggplot(data = temp, aes(x = values)) +
  geom_density(aes(color = variables))+
  xlab("scaled value") + ylab('Frequency')+
  theme_ipsum(grid = "XY")+
  scale_color_ipsum()+
  #theme(axis.text.x= element_text(hjust = c(0, 0.5, 0.5, 0.5, 1)))+
  theme( axis.line = element_line(colour = "grey35", linetype = "solid"))
