#source('../functions/ts_xyz.R')
args <- commandArgs(trailingOnly = TRUE)
f_name <- args[1]
library('AnalyzeFMRI')
print('library loaded')

first <- 1
last  <- 150
nvolumes <- 1342
#ts_aud_lang <- array(dim=c((last - first + 1), nvolumes))
raw_corr <- f.read.volume(f_name)
raw_corr_des <- sort(raw_corr,decreasing = TRUE)
#index_sorted <- matrix(0, ncol=5, nrow=50)
index_sorted <- NULL
coords <- which(raw_corr==raw_corr_des[1], arr.ind = TRUE, useNames = FALSE)
#ts_aud_lang[first,] <- ts_xyz(coords[1:3],'../run2/')
index_sorted <- matrix(c(coords,raw_corr_des[1]),nrow=1,ncol=5)

mask <- array(dim = c(64,64,30,1))
mask[coords] <- raw_corr_des[first]
print(raw_corr_des[first])
print(raw_corr_des[last])
for (i in 2:last)
{
coords <- which(raw_corr==raw_corr_des[i], arr.ind = TRUE, useNames = FALSE)
index_sorted <- rbind(index_sorted,c(coords,raw_corr_des[i]))
mask[coords] <- raw_corr_des[i]
#ts_aud_lang[i,] <- ts_xyz(coords[1:3],'../run2/')
}

mask[is.na(mask)] = 0

f.write.nifti(mask, file='max_coords_mask', size='float', nii=FALSE)
t1 <- t(index_sorted)
write(t1, file = 'index_sorted', ncolumns = 5)

#write(ts_aud_lang, file = 'aud_lang.ts', ncolumns = nvolumes)


