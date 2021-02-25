library("tidyverse")
library("cowplot")
walk(dir("r/functions",full.names = T),source)

#update mice_behavior obj----
lh.obj<-lh_update()

# all testnames----
lh_plot_single.test(lh.obj,"test_1")
