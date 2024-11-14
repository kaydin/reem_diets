library(ggplot2)
library(paletteer)

this.model <- "NBS"
this.pred <- "Pacific_cod"

dat <- diet_cons_domain_combined %>%
  filter(species_name==this.pred & model==this.model) %>%
  filter(!is.na(pacific_cod_ebs) & !(pacific_cod_ebs %in% c("MISSING","Octopus","Tanner crab")))


my.col <- paletteer::paletteer_d("LaCroixColoR::Pamplemousse")
ord <- c("Walleye pollock","Other fish", "Snow crab","Benthos","Plankton")
yord <- c("2010","","2017","2019","2021","2022","2023")

#X11(width=8,height=5)
png("cod.png", width=3600, height=2000, res=600) 
ggplot(dat, aes(fill=factor(pacific_cod_ebs,levels=ord), y=preycons_sci_vonb_tons, x=factor(year,levels=yord))) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=my.col,
    name="Prey Type",
    labels=c("Walleye pollock","Other fish", "Snow crab","Bottom invertebrates","Plankton")) + 
  scale_x_discrete(drop=F) +
  xlab("") + 
  ylab("Percent in diet") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Pacific cod diets in the Northern Bering Sea") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 0.5,size=10,face="bold"),
        axis.title.y = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text.y = element_text(hjust = 0.5,size=10),
        legend.text = element_text(size=12),
        legend.title=  element_text(size=12,face="bold"))
dev.off()

###############################################################################

this.model <- "NBS"
this.pred <- "Walleye_pollock"

dat <- diet_cons_domain_combined %>%
  filter(species_name==this.pred & model==this.model) %>%
  filter(!is.na(walleye_pollock_ebs) & !(walleye_pollock_ebs %in% c("MISSING"))) %>%
  mutate(walleye_pollock_ebs = ifelse(walleye_pollock_ebs %in% c("Shrimp","Copepods","Other plankton"),
                                      "Other plankton", walleye_pollock_ebs))


my.col <- paletteer::paletteer_d("LaCroixColoR::Pamplemousse")
ord <- c("Walleye pollock","Other fish", "Amphipods", "Benthos","Euphausiids","Other Plankton")
yord <- c("2010","","2017","2019","2021","2022","2023")

png("pol.png", width=3600, height=2000, res=600) 
  ggplot(dat, aes(fill=factor(walleye_pollock_ebs,levels=ord), y=preycons_sci_vonb_tons, x=factor(year,levels=yord))) +
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=my.col,
                    name="Prey Type",
                    labels=c("Walleye pollock","Other fish", "Arctic plankton", "Bottom invertebrates","Krill","Other plankton")) + 
  scale_x_discrete(drop=F) +
  xlab("") + 
  ylab("Percent in diet") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Walleye pollock diets in the Northern Bering Sea") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 0.5,size=10,face="bold"),
        axis.title.y = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text.y = element_text(hjust = 0.5,size=10),
        legend.text = element_text(size=12),
        legend.title=  element_text(size=12,face="bold"))
dev.off()




