
# Load packages -----------------------------------------------------------

library(tidyverse)


# Make patterns -----------------------------------------------------------

## Blue color
bkg_cols <- "#2DA9EF"

## Yellow color
targ_col <- "#F6EC70"

size <-c(25, 31, 41, 51, 61, 71, 101, 151, 201, 301, 401)

dist_mm <- (6/sqrt(size))*10
  
mx <- matrix(rep(c(bkg_cols, targ_col), 1), nrow=401, ncol=401)
ggplot(reshape2::melt(mx), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.1496mm_401 <- 
ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=401, ncol=401)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.1993mm_301 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=301, ncol=301)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.2985mm_201 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=201, ncol=201)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.3974mm_151 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=151, ncol=151)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.5941mm_101 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=101, ncol=101)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.8451mm_71 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=71, ncol=71)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_0.9836mm_61 <- 
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=61, ncol=61)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_1.1765mm_51 <-
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=51, ncol=51)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_1.4634mm_41 <-
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=41, ncol=41)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()

blue_yellow_1.9355mm_31 <-
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=31, ncol=31)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()


blue_yellow_2.400mm_25 <-
  ggplot(reshape2::melt(matrix(rep(c(bkg_cols, targ_col), 1), nrow=25, ncol=25)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_void()


# SAVE IMAGES -------------------------------------------------------------
  
  ## SET DPI = 300 for prints; dpi = 600 for AcuityView processing

{
  ggsave("pattern_images/blue_yellow_0.1496mm_401.tiff", plot = blue_yellow_0.1496mm_401,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )

 
  ggsave("pattern_images/blue_yellow_0.1993mm_301.tiff", plot = blue_yellow_0.1993mm_301,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  ggsave("pattern_images/blue_yellow_0.2985mm_201.tiff", plot = blue_yellow_0.2985mm_201,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  
  ggsave("pattern_images/blue_yellow_0.5941mm_101.tiff", plot = blue_yellow_0.5941mm_101,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  


  ggsave("pattern_images/blue_yellow_0.3974mm_151.tiff", plot = blue_yellow_0.3974mm_151,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  
  ggsave("pattern_images/blue_yellow_0.8451mm_71.tiff", plot = blue_yellow_0.8451mm_71,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  
  ggsave("pattern_images/blue_yellow_0.9836mm_61.tiff", plot = blue_yellow_0.9836mm_61,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  ggsave("pattern_images/blue_yellow_1.1765mm_51.tiff", plot = blue_yellow_1.1765mm_51,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  
  
  ggsave("pattern_images/blue_yellow_1.4634mm_41.tiff", plot = blue_yellow_1.4634mm_41,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  
  ggsave("pattern_images/blue_yellow_1.9355mm_31.tiff", plot = blue_yellow_1.9355mm_31,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  
  ggsave("pattern_images/blue_yellow_2.400mm_25.tiff", plot = blue_yellow_2.400mm_25,
         width = 6,
         height = 6,
         units = "cm",
         dpi = 300,
         scale = 1
  )
  

  
  }
  