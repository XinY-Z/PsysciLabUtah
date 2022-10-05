library(data.table)

set.seed(123)
#load('image_062422.RData')
load(r'(C:\Users\XinZ\Box\DATA\Xin PDRP\image_062422.RData)')
rm(svr_intv1)

dt[, ave_empathy := mean(empathy), client_epi]
