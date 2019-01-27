library(vein)
library(ggplot2)

data(fe2015)
co1 <-  ef_cetesb(p = "CO", veh = "PC_G", full = T)
lef <- ef_ldv_scaled(dfcol = co1$CO, v = "PC", t = "4S", cc = "<=1400", f = "G",
                     eu = co1$EqEuro_PC, p = "CO")

length(lef)
co <- ef_cetesb(p = "CO", veh = "PC_G")
head(cet)
co[c(1,6,13)]

co1[c(1,6,13), ]

EF_2015_V <- lef[[1]](1:150)
EF_2010_III <- lef[[6]](1:150)
EF_2004_II <- lef[[13]](1:150)

df <- data.frame(EF = c(EF_2015_V,
                        EF_2010_III, 
                        EF_2004_II),
                 Original = as.numeric(c(
                   rep(co[1], length(EF_2015_V)),
                   rep(co[6], length(EF_2010_III)),
                   rep(co[13], length(EF_2004_II)))),
                 Standard = c(
                   rep("EF_2015_V", length(EF_2015_V)),
                 rep("EF_2010_III", length(EF_2010_III)),
                 rep("EF_2004_II", length(EF_2004_II)))
                 )
df$Speed <- 1:150

png(filename = "~/MEGA/PostDocChina/China20190121/figuras/ef_scaled.png", width = 600, height = 400, units ="px")
ggplot(df, aes(x = Speed, y = EF, colour = Standard)) +
  geom_point() +
  geom_text(data = df[df$Speed ==34,],
            aes(x = Speed + 15, y = Original, label = round(Original, 3)),colour = "black", size = 7) +
  geom_point(data = df[df$Speed ==34,],
             aes(x = Speed, y = Original), colour = "black") +
  labs(x = "Speed [km/h]", y = "CO [g/km]")+
scale_y_log10() +
  theme_bw() +
  theme(text = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 16, colour = "black"),
        legend.key.height=unit(2,"line"),
        legend.key.width=unit(1.5,"line"),
        legend.justification = "center",
        legend.text = element_text(size = 16))
dev.off()



png(filename = "~/MEGA/PostDocChina/China20190121/figuras/cetesb.png", width = 700, height = 500, units ="px")
ggplot(co1, aes(x = Age, y = as.numeric(CO))) +
  geom_point() +
  geom_text(data = co1[c(1, 6, 13), ],
            aes(x = Age, y = as.numeric(CO)+0.5, label = round(as.numeric(CO), 3)),colour = "black", size = 7)  +
  geom_point(data = co1[c(1, 6, 13), ],
            aes(x = Age, y = as.numeric(CO)),colour = "red", size = 3)  +
  scale_y_log10() +
  labs(x = "Age", y = "CO [g/km]")+
  theme_bw() +
  theme(text = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 16, colour = "black"),
        legend.key.height=unit(2,"line"),
        legend.key.width=unit(1.5,"line"),
        legend.justification = "center",
        legend.text = element_text(size = 16))
dev.off()

