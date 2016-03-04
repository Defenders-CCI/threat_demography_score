# Analysis and plotting of Whitney's Recovery Actions dataset.
# Copyright (C) 2015 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

library(extrafont)
library(ggplot2)
library(ggthemes)
library(stringr)

library(AICcmodavg)
library(FactoMineR)
library(ggplot2)
library(lubridate)
library(MASS)
library(nnet)
source("~/Defenders_JWM/R/Whitney_proj/multiplot.R")

##############################################################################
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
##############################################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

##############################################################################
# The following lines were run on the 22Mar2015 file, saved, and after a couple
# of manual edits (fixing scores) the file was read back in for analysis
#
# Load the data, check vars
###############################################################################
# fil <- "~/Google Drive/Defenders/other_projects/RecoveryActions/RecoveryActs_22Mar2015.tab"
# dat <- read.table(fil,
#                   sep="\t",
#                   header=TRUE,
#                   na.string="NA",
#                   stringsAsFactors=FALSE)

# summary(dat)

# to_factor <- c(1, 2, 14)
# for (i in to_factor) {
#     dat[,i] <- as.factor(dat[,i])
# }

# to_numeric <- c(3:13)
# for (i in to_numeric) {
#     dat[,i] <- as.numeric(dat[,i])
# }

# to_date <- c(15, 16, 17)
# for (i in to_date) {
#     dat[,i] <- as.Date(dat[,i], format="%m/%d/%y")
# }

##############################################################################
# The following lines were run on the 22Mar2015 file, saved, and after a couple
# of manual edits (fixing scores) the file was read back
#
# fix_date <- gsub("20", "19", as.character(dat$listing_date))
# fix_date <- gsub("1901", "2001", fix_date)
# fix_date <- as.Date(fix_date)
# dat$listing_date <- as.Date(fix_date)

# tmp_act_rec <- ifelse(as.character(dat$act_recov_pl) == "2019-05-06",
#                       "1998-05-06",
#                       as.character(dat$act_recov_pl))
# tmp_act_rec <- as.Date(tmp_act_rec, format="%Y-%m-%d")
# dat$act_recov_pl <- tmp_act_rec

# tmp_act_rec <- ifelse(as.character(dat$Species) == "Modoc Sucker",
#                       "1984-05-08",
#                       as.character(dat$act_recov_pl))
# tmp_act_rec <- as.Date(tmp_act_rec, format="%Y-%m-%d")
# dat$act_recov_pl <- tmp_act_rec

# cur <- as.Date("2015-05-15", format="%Y-%m-%d")
# dat$time_listed <- as.numeric(cur - dat$listing_date)
# dat$time_plans <- as.numeric(cur - dat$act_recov_pl)
# dat$time_review <- as.numeric(cur - dat$five.yr_revw)

# dat$status_cat <- ifelse(dat$Listing_chng == "E" |
#                          dat$Listing_chng == "T",
#                          "No Change",
#                          ifelse(dat$Listing_chng == "E-->Ext." |
#                                 dat$Listing_chng == "T-->E",
#                                 "Degrade",
#                                 "Improve"))
# dat$status_cat <- as.factor(dat$status_cat)

# dat$combined_components <- dat$Threat_allev + dat$Improve_demo

# out <- "~/Google Drive/Defenders/other_projects/RecoveryActions/RecoveryActs_near-final.RData"
# save(dat, file=out)

# out <- "~/Google Drive/Defenders/other_projects/RecoveryActions/RecoveryActs_near-final.tab"
# write.table(dat, 
#             file=out,
#             sep="\t",
#             quote=FALSE,
#             row.names=FALSE)

##############################################################################
# Here's the real data loading...
base <- "/Users/jacobmalcom/Google Drive/Defenders/other_projects/RecoveryActions/"
fil <- paste(base, "RecoveryActs_near-final.tab", sep="")

dat <- read.table(fil,
                  sep="\t",
                  header=TRUE,
                  stringsAsFactors=FALSE)

dat$priority_conflict <- ifelse(dat$priority_conflict == "",
                                "NC",
                                dat$priority_conflict)

to_factor <- c(1, 2, 14, 25, 27)
for (i in to_factor) {
    dat[,i] <- as.factor(dat[,i])
}

to_numeric <- c(3:13)
for (i in to_numeric) {
    dat[,i] <- as.numeric(dat[,i])
}

to_date <- c(15, 16, 17)
for (i in to_date) {
    dat[,i] <- as.Date(dat[,i], format="%m/%d/%y")
}

dat <- dat[,c(1:25, 27)]
dat$combined_components <- dat$Threat_allev + dat$Improve_demo

###############################################################################
# Some quick analyses
###############################################################################

###########
# Threats and demography by status change
for_threats <- dat[dat$Priority == 1,]
dim(for_threats)

amod <- lm(for_threats$Threat_allev ~ for_threats$status_cat + 0)
summary(amod)

bmod <- lm(for_threats$Improve_demo ~ for_threats$status_cat + 0)
summary(bmod)

cmod <- lm(for_threats$combined_components ~ for_threats$status_cat + 0)
summary(cmod)

for_threats$status_cat <- relevel(for_threats$status_cat, ref="No Change")
combo_multinom <- multinom(for_threats$status_cat ~ 0 + for_threats$combined_components)
combo_zscore <- summary(combo_multinom)$coefficients / summary(combo_multinom)$standard.errors
combo_multinom_p <- (1 - pnorm(abs(combo_zscore), 0, 1)) * 2
combo_multinom_p
AICc(combo_multinom)

both_ind_multinom <- multinom(for_threats$status_cat ~ 0 + for_threats$Threat_allev + for_threats$Improve_demo)
both_ind_zscore <- summary(both_ind_multinom)$coefficients / summary(both_ind_multinom)$standard.errors
both_ind_multinom_p <- (1 - pnorm(abs(both_ind_zscore), 0, 1)) * 2
both_ind_multinom_p
AICc(both_ind_multinom)

threat_multinom <- multinom(for_threats$status_cat ~ 0 + for_threats$Threat_allev)
threat_multinom_zscore <- summary(threat_multinom)$coefficients / summary(threat_multinom)$standard.errors
threat_multinom_p <- (1 - pnorm(abs(threat_multinom_zscore), 0, 1)) * 2
threat_multinom_p
AICc(threat_multinom)

demog_multinom <- multinom(for_threats$status_cat ~ 0 + for_threats$Improve_demo)
demog_multinom_zscore <- summary(demog_multinom)$coefficients / summary(demog_multinom)$standard.errors
demog_multinom_p <- (1 - pnorm(abs(demog_multinom_zscore), 0, 1)) * 2
demog_multinom_p
AICc(demog_multinom)

###############################################################################
# And the boxplots for scores by status recommendations
for_threats$tmp_stat_cat <- ifelse(for_threats$status_cat == "Degrade",
                                   "Uplist",
                                   ifelse(for_threats$status_cat == "Improve",
                                          "Down- or de-list",
                                          "No change"))
for_threats$tmp_stat_cat <- as.factor(for_threats$tmp_stat_cat)
for_threats$tmp_stat_cat <- relevel(for_threats$tmp_stat_cat, ref="No change")

aplot <- ggplot(for_threats, aes(tmp_stat_cat, Threat_allev)) +
         geom_boxplot(outlier.shape=NA, alpha=0.5) +
         geom_jitter(alpha=0.3, size=4,
                     position=position_jitter(height=0.05, width=0.2)) +
         labs(x="\nFWS status change recommendation",
              y="",
              title="Threats\n") + 
         theme_minimal(base_size=14) +
         theme(axis.text.x=element_text(vjust=1.5),
               axis.ticks=element_blank(),
               text=element_text(size=14, family="Open Sans"),
               legend.position="none")

bplot <- ggplot(for_threats, aes(tmp_stat_cat, Improve_demo)) +
         geom_boxplot(outlier.shape=NA, alpha=0.5) +
         geom_jitter(alpha=0.3, size=4,
                     position=position_jitter(height=0.05, width=0.2)) +
         labs(x="\n",
              y="",
              title="Demography\n") + 
         theme_minimal(base_size=14) +
         theme(axis.text.x=element_text(vjust=1.5),
               axis.ticks=element_blank(),
               text=element_text(size=14, family="Open Sans"),
               legend.position="none")

cplot <- ggplot(for_threats, aes(tmp_stat_cat, combined_components)) +
         geom_boxplot(outlier.shape=NA, alpha=0.5) +
         geom_jitter(alpha=0.3, size=4,
                     position=position_jitter(height=0.05, width=0.2)) +
         labs(x="\n",
              y="Score",
              title="Threats + Demography\n") + 
         theme_minimal(base_size=14) +
         theme(axis.text.x=element_text(vjust=1.5),
               axis.ticks=element_blank(),
               text=element_text(size=14, family="Open Sans"),
               legend.position="none")

multiplot(cplot, aplot, bplot, cols=3)

outf <- "/Users/jacobmalcom/Google Drive/Defenders/other_projects/RecoveryActions/figs/FWS_rec_boxplot_minimal.pdf"
pdf(file=outf, height=6, width=18, family="Open Sans")
multiplot(cplot, aplot, bplot, cols=3)
dev.off()

outf <- "/Users/jacobmalcom/Google Drive/Defenders/other_projects/RecoveryActions/figs/FWS_rec_boxplot_minimal.png"
png(file=outf, height=6, width=18, units="in", res=300, family="Open Sans")
multiplot(cplot, aplot, bplot, cols=3)
dev.off()

newout <- "/Users/jacobmalcom/Google Drive/Defenders/other_projects/RecoveryActions/figs/FWS_rec_boxplot_minimal_embed.pdf"
embed_fonts(outf, outfile=newout)

##############################################################################
# Need to make a dataframe of one entry per species; this includes some additional
# variables for the recovery actions analyses that are now moved elsewhere
scores <- tapply(dat$combined_components, INDEX=dat$Species, FUN=mean)
threat <- tapply(dat$Threat_allev, INDEX=dat$Species, FUN=mean)
demogr <- tapply(dat$Improve_demo, INDEX=dat$Species, FUN=mean)
list_days <- tapply(dat$time_listed, INDEX=dat$Species, FUN=mean) 
recov_days <- tapply(dat$time_plans, INDEX=dat$Species, FUN=mean) 

d3 <- unique(data.frame(species=dat$Species, category=dat$status_cat))
d3 <- d3[order(d3$species), ]

P1_rec <- dat[as.character(dat$Priority) == "1", ]
P2_rec <- dat[as.character(dat$Priority) == "2", ]
P3_rec <- dat[as.character(dat$Priority) == "3", ]
P1_rec <- P1_rec[order(P1_rec$Species), ]
P2_rec <- P2_rec[order(P2_rec$Species), ]
P3_rec <- P3_rec[order(P3_rec$Species), ]

P1_rec$N_compartong <- P1_rec$N_compl + P1_rec$N_partc + P1_rec$N_oncur
P2_rec$N_compartong <- P2_rec$N_compl + P2_rec$N_partc + P2_rec$N_oncur
P3_rec$N_compartong <- P3_rec$N_compl + P3_rec$N_partc + P3_rec$N_oncur

P1_rec$pct_compartong <- ifelse(P1_rec$N_act > 0,
                                P1_rec$N_compartong / P1_rec$N_act,
                                NA)
P2_rec$pct_compartong <- ifelse(P2_rec$N_act > 0,
                                P2_rec$N_compartong / P2_rec$N_act,
                                NA)
P3_rec$pct_compartong <- ifelse(P2_rec$N_act > 0,
                                P3_rec$N_compartong / P3_rec$N_act,
                                NA)

dat2 <- data.frame(species=names(scores), 
                   status_cat=d3$category,
                   threats=as.vector(threat),
                   demography=as.vector(demogr),
                   combined_components=as.vector(scores),
                   list_days=as.vector(list_days),
                   recov_days=as.vector(recov_days),
                   P1_N_act=P1_rec$N_act,
                   P1_comp=P1_rec$N_compl,
                   P1_N=P1_rec$N_compartong,
                   P1_pct=P1_rec$pct_compartong,
                   P2_N_act=P2_rec$N_act,
                   P2_comp=P2_rec$N_compl,
                   P2_N=P2_rec$N_compartong,
                   P2_pct=P2_rec$pct_compartong,
                   P3_N_act=P3_rec$N_act,
                   P3_comp=P3_rec$N_compl,
                   P3_N=P3_rec$N_compartong,
                   P3_pct=P3_rec$pct_compartong)

##############################################################################
# Now for the discriminant function analysis
dfa_score <- lda(status_cat ~ combined_components,
                 data = dat2,
                 na.action = "na.omit",
                 CV=TRUE)
ct_score <- table(dat2$status_cat, dfa_score$class)
prop.table(ct_score, 1)
diag(prop.table(ct_score, 1))
sum(diag(prop.table(ct_score)))

dfa_score_alt <- lda(status_cat ~ threats + demography,
                     data = dat2,
                     na.action = "na.omit",
                     CV=TRUE)
ct_score_alt <- table(dat2$status_cat, dfa_score_alt$class)
prop.table(ct_score_alt, 1)
diag(prop.table(ct_score_alt, 1))
sum(diag(prop.table(ct_score_alt)))

dfa_threat <- lda(status_cat ~ threats,
                 data = dat2,
                 na.action = "na.omit",
                 CV=TRUE)
ct_threat <- table(dat2$status_cat, dfa_threat$class)
prop.table(ct_threat, 1)
diag(prop.table(ct_threat, 1))
sum(diag(prop.table(ct_threat)))

dfa_demog <- lda(status_cat ~ demography,
                 data = dat2,
                 na.action = "na.omit",
                 CV=TRUE)
ct_demog <- table(dat2$status_cat, dfa_demog$class)
prop.table(ct_demog, 1)
diag(prop.table(ct_demog, 1))
sum(diag(prop.table(ct_demog)))

t(prop.table(ct_score, 1))
t(prop.table(ct_score_alt, 1))
t(prop.table(ct_threat, 1))
t(prop.table(ct_demog, 1))

t(table(dat2$status_cat, dfa_score$class))
t(table(dat2$status_cat, dfa_score_alt$class))
t(table(dat2$status_cat, dfa_threat$class))
t(table(dat2$status_cat, dfa_demog$class))

#############################################################################
# Now am going to try to plot the classification results
infile <- paste(base, "manuscript/classification_data.tab", sep="")
cl_dat <- read.table(infile,
                     sep="\t",
                     header=TRUE,
                     stringsAsFactors=FALSE)
cl_dat$Model <- paste("Model ", cl_dat$Model, sep="")
cl_dat$Pct_cross <- paste(cl_dat$Pct_cross, "%", sep="")

LDA_plot <- ggplot(data=cl_dat, aes(x=classify, y=Pct_cross, fill=factor(col))) +
            geom_bar(stat="identity") +
            scale_fill_manual(values=c('#0A4783', 'darkolivegreen3', '#f49831'),
                              labels=c("overprotect", "consistent", "underprotect"),
                              name="") +
            facet_grid(Model ~ FWS_rec) +
            ylim(0, 100) +
            labs(x = "\nLDA classification",
                 y = "% species classified\nFWS x LDA classification\n",
                 title = "FWS status change recommendation") +
            theme_bw() +
            theme(text=element_text(size=12, family="Open Sans"),
                  panel.background=element_rect(fill="white"))

outf <- paste(base, "figs/LDA_cross-classify.pdf", sep="")
pdf(outf, height=10, width=14, family="Open Sans")
LDA_plot
dev.off()

newout <- paste(base, "figs/LDA_cross-classify.pdf", sep="")
embed_fonts(outf, outfile=newout)

LDA_plot2 <- ggplot(data=cl_dat, aes(x=classify, y=N_cross, fill=factor(col))) +
             geom_bar(stat="identity") +
             geom_text(y=26, aes(label=Pct_cross), size=3, colour="gray40") +
             scale_fill_manual(values=c('#0A4783', 'darkolivegreen4', '#f49831'),
                               labels=c("overprotect", "consistent", "underprotect"),
                               name="") +
             facet_grid(Model ~ FWS_rec) +
             ylim(0, 27) +
             labs(x = "\nLDA classification",
                  y = "# species classified\nper FWS x LDA category\n",
                  title = "FWS status change recommendation") +
             theme_bw() +
             theme(text=element_text(size=12, family="Open Sans"),
                   panel.background=element_rect(fill="white"))
LDA_plot2

outf <- paste(base, "figs/LDA_cross-classify_N.pdf", sep="")
pdf(outf, height=10, width=14, family="Open Sans")
LDA_plot2
dev.off()

newout <- paste(base, "figs/LDA_cross-classify_N_embed.pdf", sep="")
embed_fonts(outf, outfile=newout)

##############################################################################
# I want to take a closer look at the Recovery Priority Numbers
afig <- ggplot(for_threats, aes(x=combined_components, 
                                y=priority_num,
                                colour=factor(priority_conflict))) +
        geom_jitter(alpha=0.5, 
                    size=4,
                    position=position_jitter(width=0.05, height=0)) +
        ylim(c(0,15)) +
        stat_smooth(method="lm", alpha=0.2) +
        scale_colour_manual(values=c('#0A4783', "#f49831"),
                          labels=c("Conflict", "No conflict"),
                          name="") +
        labs(x="\nThreats + Demography Score",
             y="Recovery Priority Number\n") +
        theme_minimal(base_size=14) +
        theme(axis.text.x=element_text(vjust=1.5),
              axis.ticks=element_blank(),
              text=element_text(size=14, family="Open Sans"),
              legend.position="none")
afig

bfig <- ggplot(for_threats, aes(x=Threat_allev,
                                y=priority_num,
                                colour=factor(priority_conflict))) +
        geom_jitter(alpha=0.4, 
                    size=4,
                    position=position_jitter(width=0.05, height=0)) +
        ylim(c(0,15)) +
        stat_smooth(method="lm", alpha=0.2) +
        scale_colour_manual(values=c('#0A4783', "#f49831"),
                          labels=c("Conflict", "No conflict"),
                          name="") +
        labs(x="\nThreats Score",
             y="") +
        theme_minimal(base_size=14) +
        theme(axis.text.x=element_text(vjust=1.5),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              text=element_text(size=14, family="Open Sans"),
              legend.position="none")
bfig

cfig <- ggplot(for_threats, aes(x=Improve_demo,
                                y=priority_num,
                                colour=factor(priority_conflict))) +
        geom_jitter(alpha=0.4, 
                    size=4,
                    position=position_jitter(width=0.05, height=0)) +
        ylim(c(0,15)) +
        stat_smooth(method="lm", alpha=0.2) +
        scale_colour_manual(values=c('#0A4783', "#f49831"),
                          labels=c("Conflict", "No conflict"),
                          name="") +
        labs(x="\nDemography Score",
             y="") +
        theme_minimal(base_size=14) +
        theme(axis.text.x=element_text(vjust=1.5),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              text=element_text(size=14, family="Open Sans"),
              legend.position="none")
cfig

multiplot(afig, bfig, cfig, cols=3)

base <- "/Users/jacobmalcom/Google Drive/Defenders/manuscripts/threat_demog_scoring/figures/drafts/"
outf <- paste(base, "RPN_vs_scores.pdf", sep="")
pdf(outf, height=6, width=18, family="Open Sans")
multiplot(afig, bfig, cfig, cols=3)
dev.off()

newout <- paste(base, "RPN_vs_scores_embed.pdf", sep="")
embed_fonts(outf, outfile=newout)

qplot(for_threats$priority_num)
hist(for_threats$priority_num)

###############################################################################
# OK, there's something there, so time for some tests...
cor.test(for_threats$Threat_allev, for_threats$priority_num)
cor.test(for_threats$Improve_demo , for_threats$priority_num)
cor.test(for_threats$combined_components, for_threats$priority_num)

RPN <- cbind(for_threats$priority_num, for_threats$priority_conflict)

rpn_mmod1 <- lm(RPN ~ combined_components, data=for_threats)
sum_mmod1 <- summary(rpn_mmod1)
rpn_mmod1_manova <- manova(rpn_mmod1)
summary(rpn_mmod1_manova)

rpn_mmod2 <- lm(RPN ~ Threat_allev + Improve_demo, data=for_threats)
sum_mmod2 <- summary(rpn_mmod2)
rpn_mmod2_manova <- manova(rpn_mmod2)
summary(rpn_mmod2_manova)

rpn_mmod3 <- lm(RPN ~ Threat_allev, data=for_threats)
sum_mmod3 <- summary(rpn_mmod3)
rpn_mmod3_manova <- manova(rpn_mmod3)
summary(rpn_mmod3_manova)

rpn_mmod4 <- lm(RPN ~ Improve_demo, data=for_threats)
sum_mmod4 <- summary(rpn_mmod4)
rpn_mmod4_manova <- manova(rpn_mmod4)
summary(rpn_mmod4_manova)

# Removing conflict from response in MANOVA because nothing appears to be
# significantly (not even close...) predictive...and I can use AICc
rpn_mod1 <- lm(priority_num ~ combined_components, data=for_threats)
summary(rpn_mod1)
aov(rpn_mod1)
AICc(rpn_mod1)
hist(resid(rpn_mod1), breaks=12)

rpn_mod2 <- glm.nb(priority_num ~ combined_components, 
                   link="log",
                   data=for_threats)
summary(rpn_mod2)
aov(rpn_mod2)
AICc(rpn_mod2)
1 - rpn_mod2$deviance / rpn_mod2$null.deviance
hist(resid(rpn_mod2), breaks=12)

rpn_mod3 <- lm(priority_num ~ Threat_allev + Improve_demo, data=for_threats)
summary(rpn_mod3)
aov(rpn_mod3)
AICc(rpn_mod3)
hist(resid(rpn_mod3), breaks=12)

rpn_mod4 <- glm.nb(priority_num ~ Threat_allev + Improve_demo, 
                   link="log",
                   data=for_threats)
summary(rpn_mod4)
aov(rpn_mod4)
AICc(rpn_mod4)
1 - rpn_mod4$deviance / rpn_mod4$null.deviance
hist(resid(rpn_mod4), breaks=12)
plot(rpn_mod4$residuals ~ rpn_mod4$fitted.values)

rpn_mod4b <- glm.nb(priority_num ~ Threat_allev * Improve_demo, 
                   link="log",
                   data=for_threats)
summary(rpn_mod4b)
aov(rpn_mod4b)
AICc(rpn_mod4b)
1 - rpn_mod4b$deviance / rpn_mod4b$null.deviance
hist(resid(rpn_mod4b), breaks=12)
plot(rpn_mod4b$residuals ~ rpn_mod4b$fitted.values)

rpn_mod5 <- lm(log(priority_num) ~ Threat_allev + Improve_demo, data=for_threats)
summary(rpn_mod5)
aov(rpn_mod5)
AICc(rpn_mod5)
hist(resid(rpn_mod5), breaks=12)

par(mfrow=c(2,2))
hist(resid(rpn_mod1), breaks=12)
hist(resid(rpn_mod2), breaks=12)
hist(resid(rpn_mod3), breaks=12)
hist(resid(rpn_mod4), breaks=12)

# threats
rpn_mod1t <- lm(priority_num ~ Threat_allev, data=for_threats)
summary(rpn_mod1t)
AICc(rpn_mod1t)
hist(resid(rpn_mod1t), breaks=12)

rpn_modt2 <- glm.nb(priority_num ~ Threat_allev, 
                   link="log",
                   data=for_threats)
summary(rpn_modt2)
aov(rpn_modt2)
AICc(rpn_modt2)
1 - rpn_modt2$deviance / rpn_modt2$null.deviance
hist(resid(rpn_modt2), breaks=12)

# demography
rpn_mod1d <- lm(priority_num ~ Improve_demo, data=for_threats)
summary(rpn_mod1d)
AICc(rpn_mod1d)

rpn_modd2 <- glm.nb(priority_num ~ Improve_demo, 
                   link="log",
                   data=for_threats)
summary(rpn_modd2)
aov(rpn_modd2)
AICc(rpn_modd2)
1 - rpn_modd2$deviance / rpn_modd2$null.deviance
hist(resid(rpn_modd2), breaks=12)

AICc(rpn_mod1)
AICc(rpn_mod3)
AICc(rpn_mod1t)
AICc(rpn_mod1d)

AICc(rpn_mod2)
AICc(rpn_mod4)
AICc(rpn_modt2)
AICc(rpn_modd2)

cand_mods <- list(rpn_mod1, rpn_mod3, rpn_mod1t, rpn_mod1d) #rpn_mod2, 
modavg(cand_mods, parm='Threat_allev')

cand_mods <- list(rpn_mod2, rpn_mod4, rpn_modt2, rpn_modd2) #rpn_mod2, 
modavg(cand_mods, parm='Threat_allev')

cand_mods <- list(rpn_mod1, rpn_mod3, rpn_mod1t, rpn_mod1d) #rpn_mod2, 
modavg(cand_mods, parm='Improve_demo')

#############################################################################
# Some numbers for the manuscript
decline <- for_threats[for_threats$status_cat == "Degrade", ]
improve <- for_threats[for_threats$status_cat == "Improve", ]
nochnge <- for_threats[for_threats$status_cat == "No Change", ]

decline[decline$Threat_allev > 0 & decline$Improve_demo > 0, ]
as.character(decline[decline$Threat_allev == 0 & decline$Improve_demo == 0, ]$Species)
as.character(decline[decline$Threat_allev < 0 & decline$Improve_demo < 0, ]$Species)
as.character(decline[decline$Threat_allev < 0 & decline$Improve_demo == 0, ]$Species)
as.character(decline[decline$Threat_allev == 0 & decline$Improve_demo < 0, ]$Species)
as.character(decline[decline$Threat_allev < 0, ]$Species)

as.character(improve[improve$Threat_allev < 0 & improve$Improve_demo < 0, ]$Species)
as.character(improve[improve$Threat_allev > 0 & improve$Improve_demo > 0, ]$Species)
as.character(improve[improve$Threat_allev > 0 & improve$Improve_demo > 0, ]$Listing_chng)
as.character(improve[improve$Threat_allev == 0 & improve$Improve_demo == 0, ]$Species)
as.character(improve[improve$Threat_allev > 0 & improve$Improve_demo == 0, ]$Species)
as.character(improve[improve$Threat_allev > 0 & improve$Improve_demo == 0, ]$Listing_chng)
as.character(improve[improve$Threat_allev == 0 & improve$Improve_demo > 0, ]$Species)
as.character(improve[improve$Threat_allev == 0 & improve$Improve_demo > 0, ]$Listing_chng)
as.character(improve[improve$Threat_allev < 0, ]$Species)
improve[improve$Threat_allev < 0, ]
as.character(improve[improve$Improve_demo < 0, ]$Species)

for_threats[for_threats$Threat_allev < 0 & for_threats$Improve_demo >= 0, ]

# no change
as.character(nochnge[nochnge$Threat_allev < 0 & nochnge$Improve_demo < 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev < 0 | nochnge$Improve_demo < 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev < 0 | nochnge$Improve_demo < 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev > 0 & nochnge$Improve_demo > 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev > 0 & nochnge$Improve_demo > 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev > 0 | nochnge$Improve_demo > 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev > 0 | nochnge$Improve_demo > 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev == 0 & nochnge$Improve_demo == 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev == 0 & nochnge$Improve_demo == 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev > 0 & nochnge$Improve_demo == 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev == 0 & nochnge$Improve_demo > 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev < 0 & nochnge$Improve_demo == 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev < 0 & nochnge$Improve_demo == 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev == 0 & nochnge$Improve_demo < 0, ]$Species)
as.character(nochnge[nochnge$Threat_allev == 0 & nochnge$Improve_demo < 0, ]$Listing_chng)
as.character(nochnge[nochnge$Threat_allev < 0, ]$Species)
nochnge[nochnge$Threat_allev < 0, ]
as.character(nochnge[nochnge$Improve_demo < 0, ]$Species)

