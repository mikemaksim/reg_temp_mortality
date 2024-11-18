
# Extracting data from representative surveys

library(haven)
library(dplyr)
library(clipr)

# 2003

GKS_OBDH_2003 = read_sav("GKS_OBDH_2003_FG.sav", encoding = "cp1251")

GKS_OBDH_2003 = GKS_OBDH_2003 %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2003
attributes(GKS_OBDH_2003$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2003

# 2004

GKS_OBDH_2004 = read_sav("GKS_OBDH_2004_FG.sav", encoding = "cp1251")
GKS_OBDH_2004 = GKS_OBDH_2004 %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2004
attributes(GKS_OBDH_2004$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2004

# 2005

GKS_OBDH_2005 = read_sav("GKS_OBDH_2005_FG.sav", encoding = "cp1251")
GKS_OBDH_2005 = GKS_OBDH_2005 %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0),  # AC prevalence by regions in 2005
            housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions in 2005
attributes(GKS_OBDH_2005$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2005

# 2006

GKS_OBDH_2006 = read_sav("GKS_OBDH_2006_FG.sav", encoding = "cp1251")
GKS_OBDH_2006 = GKS_OBDH_2006 %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), # AC prevalence by regions in 2006
            housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions in 2006
attributes(GKS_OBDH_2006$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2006

# 2007

GKS_OBDH_2007_FG = read_sav("GKS_OBDH_2007_FG.sav", encoding = "cp1251")
GKS_OBDH_2007_FG = GKS_OBDH_2007_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2007
attributes(GKS_OBDH_2007_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2007

GKS_OBDH_2007_FZ = read_sav("GKS_OBDH_2007_FZ.sav", encoding = "cp1251")
GKS_OBDH_2007_FZ = GKS_OBDH_2007_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2007
attributes(GKS_OBDH_2007_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2007

# 2008

GKS_OBDH_2008_FG = read_sav("GKS_OBDH_2008_FG.sav", encoding = "cp1251")
GKS_OBDH_2008_FG = GKS_OBDH_2008_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2008
attributes(GKS_OBDH_2008_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2007

GKS_OBDH_2008_FZ = read_sav("GKS_OBDH_2008_FZ.sav", encoding = "cp1251")
GKS_OBDH_2008_FZ = GKS_OBDH_2008_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2008
attributes(GKS_OBDH_2008_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2008

# 2009

GKS_OBDH_2009_FG = read_sav("GKS_OBDH_2009_FG.sav", encoding = "cp1251")
GKS_OBDH_2009_FG = GKS_OBDH_2009_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2009
attributes(GKS_OBDH_2009_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2009

GKS_OBDH_2009_FZ = read_sav("GKS_OBDH_2009_FZ.sav", encoding = "cp1251")
GKS_OBDH_2009_FZ = GKS_OBDH_2009_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2009
attributes(GKS_OBDH_2009_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2009

# 2010

GKS_OBDH_2010_FG = read_sav("GKS_OBDH_2010_FG.sav", encoding = "cp1251")
GKS_OBDH_2010_FG = GKS_OBDH_2010_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2010
attributes(GKS_OBDH_2010_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2010

GKS_OBDH_2010_FZ = read_sav("GKS_OBDH_2010_FZ.sav", encoding = "cp1251")
GKS_OBDH_2010_FZ = GKS_OBDH_2010_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2010
attributes(GKS_OBDH_2010_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2010

# 2011

GKS_OBDH_2011_FG = read_sav("GKS_OBDH_2011_FG.sav", encoding = "cp1251")
GKS_OBDH_2011_FG = GKS_OBDH_2011_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2011
attributes(GKS_OBDH_2011_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2011

GKS_OBDH_2011_FZ = read_sav("GKS_OBDH_2011_FZ.sav", encoding = "cp1251")
GKS_OBDH_2011_FZ = GKS_OBDH_2011_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2011
attributes(GKS_OBDH_2011_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2011

# 2012

GKS_OBDH_2012_FG = read_sav("GKS_OBDH_2012_FG.sav", encoding = "cp1251")
GKS_OBDH_2012_FG = GKS_OBDH_2012_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2012
attributes(GKS_OBDH_2012_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2012

GKS_OBDH_2012_FZ = read_sav("GKS_OBDH_2012_FZ.sav", encoding = "cp1251")
GKS_OBDH_2012_FZ = GKS_OBDH_2012_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2012
attributes(GKS_OBDH_2012_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2012

# 2013

GKS_OBDH_2013_FG = read_sav("GKS_OBDH_2013_FG.sav", encoding = "cp1251")
GKS_OBDH_2013_FG = GKS_OBDH_2013_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2013
attributes(GKS_OBDH_2013_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2013

GKS_OBDH_2013_FZ = read_sav("GKS_OBDH_2013_FZ.sav", encoding = "cp1251")
GKS_OBDH_2013_FZ = GKS_OBDH_2013_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2013
attributes(GKS_OBDH_2013_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2013

# 2014

GKS_OBDH_2014_FG = read_sav("GKS_OBDH_2014_FG.sav", encoding = "cp1251")
GKS_OBDH_2014_FG = GKS_OBDH_2014_FG %>% group_by(TER) %>% 
  summarise(housing = sum(OCENGIL > 3), heating = sum(XOLOD == 1), total = n()) # Satisfaction with housing conditions by regions in 2014
attributes(GKS_OBDH_2014_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2014

GKS_OBDH_2014_FZ = read_sav("GKS_OBDH_2014_FZ.sav", encoding = "cp1251")
GKS_OBDH_2014_FZ = GKS_OBDH_2014_FZ %>% group_by(TER) %>% 
  summarise(per_capita = sum(NALCOND), equipped = sum(NALCOND > 0), total = n()) # AC prevalence by regions in 2014
attributes(GKS_OBDH_2014_FZ$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2014

# 2015

GKS_OBDH_2015_FG = read_sav("GKS_OBDH_2015_FG.sav", encoding = "cp1251")
GKS_OBDH_2015_FG = GKS_OBDH_2015_FG %>% group_by(TER) %>% 
  summarise(per_capita = sum(R9V112), equipped = sum(R9V112 > 0), total = n()) # AC prevalence by regions in 2015
attributes(GKS_OBDH_2015_FG$TER)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2015

# 2016

GKS_OBDH_2016_FG = read_sav("GKS_OBDH_2016_FG.sav", encoding = "cp1251")
GKS_OBDH_2016_FG = GKS_OBDH_2016_FG %>% group_by(ter) %>% 
  summarise(per_capita = sum(r9v112), equipped = sum(r9v112 > 0), total = n()) # AC prevalence by regions in 2016
attributes(GKS_OBDH_2016_FG$ter)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2016

GKS_KOUG_2016 = read_dta("GKS_KOUG_2016/HHOLD.dta", encoding = "cp1251")
GKS_KOUG_2016 = GKS_KOUG_2016 %>% group_by(H00_02) %>% 
  summarise(housing = sum((H02_10 > 3) * KVZV2), heating = sum((H02_09_01 == 1) * KVZV2), total = sum(KVZV2)) # Satisfaction with housing conditions by regions in 2016
attributes(GKS_KOUG_2016$H00_02)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2016

# 2017

GKS_OBDH_2017_FG = read_sav("GKS_OBDH_2017_FG.sav", encoding = "cp1251")
GKS_OBDH_2017_FG = GKS_OBDH_2017_FG %>% group_by(ter) %>% 
  summarise(per_capita = sum(r9v112), equipped = sum(r9v112 > 0), total = n()) # AC prevalence by regions in 2017
attributes(GKS_OBDH_2017_FG$ter)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2017

# 2018

GKS_OBDH_2018_FG = read_sav("GKS_OBDH_2018_FG.sav", encoding = "cp1251")
GKS_OBDH_2018_FG = GKS_OBDH_2018_FG %>% group_by(ter) %>% 
  summarise(per_capita = sum(r9v112), equipped = sum(r9v112 > 0), total = n()) # AC prevalence by regions in 2018
attributes(GKS_OBDH_2018_FG$ter)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2018

GKS_KOUG_2018 = read_dta("GKS_KOUG_2018/HHOLD.dta", encoding = "cp1251")
GKS_KOUG_2018 = GKS_KOUG_2018 %>% group_by(H00_02) %>% 
  summarise(housing = sum((H02_10 > 3) * KVZV, na.rm = T), heating = sum((H02_09_01 == 1) * KVZV, na.rm = T), total = sum(KVZV)) # Satisfaction with housing conditions by regions in 2018
attributes(GKS_KOUG_2016$H00_02)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2018

# 2019

GKS_OBDH_2019_FG = read_sav("GKS_OBDH_2019_FG.sav", encoding = "cp1251")
GKS_OBDH_2019_FG = GKS_OBDH_2019_FG %>% group_by(ter) %>% 
  summarise(per_capita = sum(r9v112), equipped = sum(r9v112 > 0), total = n()) # AC prevalence by regions in 2019
attributes(GKS_OBDH_2019_FG$ter)$labels %>% as.data.frame() # Regions of Russia with corresponding codes as of 2019

```