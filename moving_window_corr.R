#moving window correlation between a vector being read from f_name
#and the time series of each voxels 
#args[1] is the file containing the correlation values
#args[2] is the file containing the 4D data
#args[3] is the output filename
library('AnalyzeFMRI')
print('library loaded')

args 	<- commandArgs(trailingOnly = TRUE)
f_name 	<- args[1]
f_nii  	<- args[2]
out 	<- args[3]
w_mode  <- args[4]



#f_name = 'conv_ampl_hrf_22050'

f_name_numeric 	<- as.numeric(readLines(f_name))
print(paste(f_name,'loaded'))

#f_nii is 4D data
#reading the timeseries for whole volume in run2ts
#run2ts 	<- f.read.volume(paste(f_name, '.nii', sep = ''))
run2ts 	<- f.read.volume(f_nii)
print('run2ts loaded')

dim1 	<- dim(run2ts)[1]
dim2 	<- dim(run2ts)[2]
dim3 	<- dim(run2ts)[3]
dim4 	<- dim(run2ts)[4]

#run2ts2d 	<- array(run2ts, dim = c(dim1*dim2*dim3, dim4))
#print('run2ts2d loaded')

#rm(run2ts)
#print('run2ts removed')

corr 	<- array(data=NA,dim = c(dim1, dim2, dim3, (dim4 - as.numeric(w_mode) + 1)))
print("corr allocated")
#for right shift of the f_name_numeric
#f_name_numeric 	<- c(0, f_name_numeric[1:dim4])

for(i in 1:dim1)
{
	for(j in 1:dim2)
	{
		for(k in 1:dim3)
		{
			print(c(i,j,k))
			for(l in 1:(dim4 - as.numeric(w_mode) + 1))
			{
				# print(dim(run2ts[i, j, k, l:(l + as.numeric(w_mode) -1)]))
				# print(dim(f_name_numeric[l:(l + (as.numeric(w_mode) - 1))]))
				corr[i, j, k, l] 	<- cor(run2ts[i, j, k, l:(l + as.numeric(w_mode) -1)], f_name_numeric[l:(l + (as.numeric(w_mode) - 1))], method = c("pearson"))
			}
		}
	}
}

rm(run2ts)

rm(f_name_numeric)

rm(i)
rm(j)
rm(k)
rm(l)

corr[is.na(corr)] = 0
print('corr done')

f.write.nifti(corr, file = out , size ='float', nii = FALSE)
rm(corr)
