# LH_behavior
creat store load and plot learned helpless mice
## test example
### lh.obj<-lh_update() 
this function will creat a lh.obj, based on data in /data, and save a copy of lh.obj.rds in /db. incomplete record and single box record is now supported
### lh_plot_single.test(lh.obj,"test_1")
this function will choose the mice according to the given_marker, (test_1 respresents batch:test,first test), and plot trial by trial, mice by mice plot
