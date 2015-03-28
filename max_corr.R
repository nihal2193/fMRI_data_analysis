#args[1] is the name of nifti file made up of correlation co-efficients to be read
#args[2] is the name of the reference nifti file(4D)
#args[3] - args[4] range of # of corr values from top

library('AnalyzeFMRI')
print('library loaded')


args 			<- commandArgs(trailingOnly = TRUE)
f1_name 		<- args[1]
f2_name 		<- args[2]
first 			<- args[3] #1
last  			<- args[4] #150

#ts_aud_lang <- array(dim = c((last - first + 1), nvolumes))
raw_corr 		<- f.read.volume(f1_name)
raw_corr_des 	<- sort(raw_corr, decreasing = TRUE)

dim1			<- dim(raw_corr)[1]
dim2			<- dim(raw_corr)[2]
dim3			<- dim(raw_corr)[3]
dim4 			<- dim(raw_corr)[4]

f2_summary		<- f.nifti.read.header(f2_name)
nvolumes 		<- f2_summary$dim[5]


#index_sorted <- matrix(0, ncol = 5, nrow = 50)
index_sorted 	<- NULL

coords 			<- which(raw_corr == raw_corr_des[1], arr.ind = TRUE, useNames = FALSE)

#ts_aud_lang[first,] <- ts_xyz(coords[1:3],'../run2/')
index_sorted 	<- matrix(c(coords, raw_corr_des[1]), nrow = 1, ncol = 5)
ts_1 			<- f.read.nifti.ts(f2_name, coords[1], coords[2], coords[3])
time_series 	<- matrix(ts_1, nrow = 1, ncol = 1342)
																															
mask 			<- array(dim = c(dim1, dim2, dim3, 1))
mask[coords] 	<- raw_corr_des[first]

print(raw_corr_des[first])
print(raw_corr_des[last])

#time_series <- array(dim=c(150,1342))
for (i in 2:last)
{
	coords 			<- which(raw_corr == raw_corr_des[i], arr.ind = TRUE, useNames = FALSE)
	time_series 	<- rbind(time_series, f.read.nifti.ts(f2_name, coords[1], coords[2], coords[3]))
	index_sorted 	<- rbind(index_sorted, c(coords, raw_corr_des[i]))
	mask[coords] 	<- raw_corr_des[i]
	#ts_aud_lang[i,] <- ts_xyz(coords[1:3],'../run2/')
}

mask[is.na(mask)] = 0

f.write.nifti(mask, file = 'max_coords_mask', size = 'float', nii = FALSE)

t1 				<- t(index_sorted)
write(t1, file = paste('index_sorted', first, last, sep = '_'), ncolumns = 5)

time_series[is.na(time_series)] = 0
tst 			<- t(time_series)
write(tst, file =paste('time_series', (last - first), 'points.csv', sep = '_'), sep = ',', ncolumns = nvolumes)
#write(ts_aud_lang, file = 'aud_lang.ts', ncolumns = nvolumes)


