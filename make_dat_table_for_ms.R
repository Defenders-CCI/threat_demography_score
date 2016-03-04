# make_dat_table_for_ms.R
# Copyright (C) 2015 Defenders of Wildlife, jmalcom@defenders.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

library("WriteXLS")

dat <- read.table("RecoveryActs_near-final.tab",
                  sep="\t",
                  header=TRUE,
                  stringsAsFactors=FALSE)

dat$dups <- duplicated(dat$Species)
dat <- dat[dat$dups == FALSE, ]
head(dat)

new <- dat[, c(1, 12:16, 25:26, 18, 27)]
head(new)

WriteXLS("new", ExcelFileName="Table_S1.xlsx")