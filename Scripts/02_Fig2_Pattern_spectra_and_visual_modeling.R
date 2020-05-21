
# Load packages -----------------------------------------------------------
library(pavo)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(flextable)

# Import reflectance spectra ----------------------------------------------

## Test blue-yellow pattern spectra
pattern_spectra <-
  getspec("spectra/pattern_spectra", ext = "jaz", lim = c(300, 700))

## Get pattern spectra names for subsetting
pattern_names <-
  do.call(rbind, strsplit(names(pattern_spectra), "\\_"))[, 1][2:length(pattern_spectra)]

## Average replicate measurements for each pattern spectrum
pattern_spectra <-
  pattern_spectra %>%
  aggspec(., by = pattern_names) %>%
  procspec(
    opt = "smooth",
    span = 0.2
  )
## Plot pattern spectra
plot(pattern_spectra, col = spec2rgb(pattern_spectra), lwd = 4, main = "Pattern Spectra")

## Import robin egg spectra
robin.eggs <-
  as.rspec(read.csv("spectra/other_spectra/AMRO_egg_spectra_CrostonPNE_2015.csv"), lim = c(300, 700)) %>%
  procspec(., opt = "smooth", span = 0.25, fixneg = "addmin")

## Get individual robin egg IDs
robin.eggs.ID <- 
  do.call(rbind, strsplit(names(robin.eggs), "\\_"))[, 1][2:length(robin.eggs)]

## Average robin egg spectra by egg ID
robin.eggs <- 
  robin.eggs %>% 
  aggspec(by = robin.eggs.ID) %>%
  setNames(c("wl", paste("robinegg", seq(1,22,1), sep = "_")))

## Plot robin egg spectra
plot(robin.eggs, col = spec2rgb(robin.eggs), lwd = 5, main = "American robin egg spectra from Croston & Hauber (2015)")

## Combine pattern spectra with robin egg spectra
all_specs <-
  as.rspec(left_join(pattern_spectra, robin.eggs))

## Plot all spectra together double-check
plot(all_specs, col = spec2rgb(all_specs), lwd = 3, main = "Pattern and robin egg spectra combined")

# Plot Fig. 2A natural egg range vs experimental egg color spectra (3.6in = 1col, 7.5in = 2col width  --------

## Subset pattern spectra used for experiments
experiment_specs <-
  pattern_spectra %>% 
  select(wl, blueyellow00, blue, yellow) %>% 
  as.rspec(lim = c(300, 700))

## Import robin egg reflectance spectra from English & Mongomerie (2011), combine with experimental pattern spectra
example_specs <-
  read_excel("spectra/other_spectra/english_mongomerie_2011_behecosocio.xlsx", sheet = 1) %>% 
  as.rspec(lim = c(300,700)) %>% 
  left_join(., as.rspec(read_excel("spectra/other_spectra/english_mongomerie_2011_behecosocio.xlsx", sheet = 2))) %>% 
  left_join(experiment_specs) %>%
  as.rspec()  

## Plot Fig 2A
example_specs %>% 
  pivot_longer(., cols = AMRO_HI:yellow, names_to = "sample", values_to = "reflectance") %>% 
  group_by(sample) %>% 
  ggplot(., aes(
    x = wl,
    y = reflectance,
    color = sample,
    group = sample
  )) +
  geom_line(size = 2)  +
  scale_color_manual(values = c(spec2rgb(example_specs)),
                     limits = c("blue",
                                "yellow",
                                "blueyellow00",
                                "AMRO_HI",
                                "AMRO_LO"),
                     labels = c("Blue square",
                                "Yellow square",
                                "Blended blue-yellow",
                                "Natural egg bright",
                                "Natural egg dark"
                                ),
                     name = sample) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20)) +
  scale_x_continuous(limits = c(300, 700),
                     breaks = seq(300, 700, 100)) +
  labs(
    x = "Wavelength (nm)",
    y = "Reflectance (%)",
    color = "Measured Sample"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10),
    # legend.title = element_text("Experimental egg type"),
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.text.align = 0,
    legend.title = element_blank(),
    #legend.position = "none",
    #legend.position = "right",
    #legend.position = c(0.22, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.5, "cm")
  )

ggsave("Figures/pattern_naturalegg_comparison.png",
       width = 3.54,
       height = 4,
       units = "in",
       dpi = 800)

# Visual modeling; JND between pattern colors and robin eggs -------------------------------

## Blackbird photoreceptor sensitivities and densities. See Hart et al. (2000) J Comp Phys A, 186:75–387, doi:10.1007/s003590050437
blackbirdsens <-
  sensmodel(
    peaksens = c(373, 454, 504, 557),
    lambdacut = c(330, 414, 514, 570),
    oiltype = c("T", "C", "Y", "R"),
    beta = T,
    om = "blackbird",
    integrate = T
  )

bb.dc <-
  sensmodel(
    557,
    lambdacut = 439,
    oiltype = "P",
    beta = T,
    om = F
  )

bb.photodens <- 
  c(1, 1.78, 2.21, 1.9)

## Just-noticeable differences between test pattern spectra and robin eggs
JNDs <-
  pattern_spectra %>%
  ## combine pattern egg spectra with real robin egg spectra
  left_join(., robin.eggs) %>%
  ## visual model cone catch--blackbird vismod
  vismodel(.,
           visual = blackbirdsens,
           illum = "D65",
           bkg = as.rspec(read_csv("spectra/other_spectra/aidala_robin_nest_spectra.csv")),
           qcatch = "Qi",
           # trans = "blackbird",
           achromatic = bb.dc,
           vonkries = T,
           relative = F
  ) %>%
  ## JND btw 3D eggs and real robin eggs
  coldist(.,
          noise = "neural",
          achro = T,
          weber = 0.1,
          weber.ref = "longest",
          subset = "robinegg"
  ) %>%
  filter(
    .,
    !grepl("robinegg", patch1)
  ) %>%
  select(patch1, dS, dL) %>%
  group_by(patch1) %>%
  summarise(.,
            mean_dS = mean(dS),
            median_dS = median(dS),
            min_dS = min(dS),
            max_dS = max(dS),
            mean_dL = mean(dL),
            median_dL = median(dL),
            min_dL = min(dL),
            max_dL = max(dL))

JNDs

## blueyellow00 is closest match by JNDs, plot it with natural robin egg spectra
as.rspec(left_join(subset(pattern_spectra, "blueyellow00"), robin.eggs)) %>% 
  plot(.,
       col = spec2rgb(.),
       lwd = 5,
       ylim=c(0, 100))

# JND table ---------------------------------------------------------------

JNDs %>% 
  select(patch1, median_dS, min_dS, max_dS, median_dL, min_dL, max_dL) %>% 
  filter(patch1 %in% c("blueyellow00", "blue", "yellow")) %>% 
  mutate(patch1 = recode(patch1,
                         blueyellow00 = "Blended blue-yellow",
                         blue = "Blue square",
                         yellow = "Yellow square"
                         )) %>% 
  rename(Stimulus = patch1) %>%
  mutate_if(is.numeric, round, 1) %>% 
  unite("Chromatic JND range", median_dS:max_dS, sep = ",") %>%
  unite("Achromatic JND range", median_dL:max_dL, sep = ",") %>% 
  qflextable() %>%
  theme_alafoli() %>%
  bold(., part = "header") %>%
  fontsize(
    part = "header",
    size = 12
  ) %>%
  fontsize(
    part = "body",
    size = 12
  ) %>%
  align(
    align = "left",
    part = "all"
  ) %>%
  italic(., j = "Stimulus") %>%
  autofit() %>%
  print(.)
  ## Only works if MS Powerpoint is available
  #print(., preview = "pptx")

# Plot Fig. 2B JND xyz plot ------------------------------------------------------------

png("Figures/jndplot.png",
    width = 3.54,
    height = 4,
    units = "in",
    res = 800 )

jndplot_specs <-
  pattern_spectra %>%
  select(wl, blueyellow00, blue, yellow) %>% 
  ## combine 3D egg spectra with real robin egg spectra
  left_join(., robin.eggs) 

jndplot_specs %>% 
  ## visual model cone catch--blackbird vismod
  vismodel(.,
           visual = blackbirdsens,
           illum = "D65",
           bkg = as.rspec(read_csv("spectra/other_spectra/aidala_robin_nest_spectra.csv")),
           qcatch = "Qi",
           #trans = "blackbird",
           achromatic = bb.dc,
           vonkries = T,
           relative = F
  ) %>%
  ## JND btw 3D eggs and real robin eggs
  coldist(.,
          noise = "neural",
          achro = T,
          weber = 0.1,
          weber.ref = "longest"
  ) %>%
  jnd2xyz(.,
          center = T,
          rotate = T,
          rotcenter = "mean",
          ref1 = "lmax557",
          ref2 = "lmax373",
          axis1 = c(1, 1, 0),
          axis2 = c(0, 0, 1)) %>% 
  jndplot(col = "black",
          bg = spec2rgb(jndplot_specs),
          arrow = "relative",
          pch = c(24, 24, 24, rep(21,30)),
          cex = 1.5,
          labels.cex = 1,
          arrow.col = "gray22",
          lwd = 0.5,
          cex.axis = 0.6,
          arrow.p = 2.25,
          margin = c(0, 1.5, 0, 0.01),
          angle = 20)

dev.off()


# EXTRA: Avian tetrahedral colorspace plot ---------------------------------------------------------

png("Figures/tetraplot.png",
    width = 3.54,
    height = 4,
    units = "in",
    res = 800 )


jndplot_specs <-
  pattern_spectra %>%
  select(wl, blueyellow00, blue, yellow) %>% 
  ## combine 3D egg spectra with real robin egg spectra
  left_join(., robin.eggs) 

jndplot_specs %>% 
  ## visual model cone catch--blackbird vismod
  vismodel(.,
           visual = blackbirdsens,
           illum = "D65",
           bkg = as.rspec(read_csv("spectra/other_spectra/aidala_robin_nest_spectra.csv")),
           qcatch = "Qi",
           # trans = "blackbird",
           achromatic = bb.dc,
           vonkries = T,
           relative = F
  ) %>%
  tcspace() %>% 
  tetraplot(
    col = "black",
    bg = spec2rgb(all_specs, alpha = 1),
    achro.col = "darkgray",
    achro.line = TRUE,
    achro.size = 0.5,
    theta = 10,
    phi = 10,
    cex = 0.8,
    labels = T,
    vert.cex = 0.6,
    perspective = T,
    pch = c(24, 24, 24, rep(21,30)),
    margin = c(0, 1.5, 0, 0.01)
  )

dev.off()
