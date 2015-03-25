#this script processes the mp3 file and saves the
#rms values for 2 seconds intervals for 1345 time points

#including the library
library(tuneR)

#importing the mp3 file into snd variable
snd 		<- readMP3('../data/AYN.mp3')
length	<- length(snd@left)
print(length)
#leftchannel
sleft  	<- snd@left

#rightchannel
sright	<- snd@right

million = 1000000

if (length > million)
{
	parts = length / million
}

#total time in minutes approx
totaltime 	<- 1: ((length/snd@samp.rate/60) * 60)
timeevents 	<- totaltime[seq(2, length(totaltime), 2)]

tp		<- vector()
ts 		<- vector()
ampl 		<- vector()
ampr 		<- vector()

vtp			<- vector(mode = "numeric", length = 1345)
# vts 		<- vector(mode = "numeric", length = 1345)
vampl 		<- vector(mode = "numeric", length = 1345)
vampr 		<- vector(mode = "numeric", length = 1345)
vprevl		<- vector()
vprevr		<- vector()


for (i in 1:trunc(parts))
{
	print(c("time ", ((million * (i-1)) + 1) , (million * i)))

	#left channel data normalised
	sleftnorm	<- sleft[((million * (i-1)) + 1) : (million * i)] / 2 ^ (snd@bit - 1)																																																																	

	#right channel data normalised	
	srightnorm	<- sright[((million * (i-1)) + 1) :(million * i)] / 2 ^ (snd@bit - 1)
	
	time		<- ((million * (i-1)) : ((million * i) - 1)) / snd@samp.rate
    indices		<- match(timeevents, time)
    indices     <- indices[complete.cases(indices)]
    #ts 		<- c(ts, indices)
    
    vtp[(time[indices[1]]/2) : (time[indices[length(indices)]]/2)] <- time[indices]
    # print(time[indices])
    # print(c((time[indices[1]]/2) ,(time[indices[length(indices)]]/2)))
	# tp <- c(tp, time[indices])
	
	
	# find the rms value for time intervals and save them
	prevplus				<- 	sleftnorm[1:(indices[1] - 1)]
	vprevl					<- 	c(vprevl, prevplus)
	vampl[(indices[1]/2)]	<- 	sqrt(mean(vprevl^2))
	rms						<- 	sqrt(mean(vprevl^2))
	ampl 					<- 	c(ampl,rms)

	prevplus				<- 	srightnorm[1:(indices[1] - 1)]
	vprevr					<- 	c(vprevr, prevplus)
	vampr[(indices[1]/2)]	<- 	sqrt(mean(vprevr^2))
	rms 					<- 	sqrt(mean(vprevr^2))
	ampr 					<- 	c(ampr,rms)


	print((indices[1]/2))
	###insert the code for having root mean
	###square for the time interval of 2 seconds
	for( j in 1:(length(indices)-1))
	{
		rms 						<-	sqrt(mean(sleftnorm[indices[j]:indices[j+1]]^2))
		vampl[(time[indices[j]]/2)] <-	rms
		ampl 						<-	c(ampl,rms)
		
		print((time[indices[j]]/2))
		rms 						<-	sqrt(mean(srightnorm[indices[j]:indices[j+1]]^2))
		vampr[(time[indices[j]]/2)] <-	rms
		ampr 						<- 	c(ampr,rms)
	}
	##update the previous to the last parts of current channel
	if(tail(indices,1) < length(sleftnorm))
	{
		vprevl		<-	sleftnorm[(tail(indices,1) + 1):length(sleftnorm)]
		
	}
	if(tail(indices,1) < tail(srightnorm, 1))
	{
		vprevr		<-	srightnorm[(tail(indices,1) + 1):length(srightnorm)]
	}
	else
	{

	}

	# sleftnorm	<- sleftnorm[indices]
	# srightnorm	<- srightnorm[indices]
	# ampl		<- c(ampl, sleftnorm)
	# ampr		<- c(ampr, srightnorm)	

	#plot(time[indices], sleftnorm, 	main = "left channel", 	xlab = c('Time (s) ', time[1], 'to', time[length(time)]), ylab = 'Amplitude')
	#plot(time[indices], srightnorm, main = "right channel", xlab = c('Time (s) ', time[1], 'to', time[length(time)]), ylab = 'Amplitude')
	
	
}

sleftnorm 	<- sleft[((million * trunc(parts)) + 1) : length] / 2 ^ (snd@bit - 1)
srightnorm	<- sright[((million	* trunc(parts)) + 1) : length] / 2 ^ (snd@bit - 1)

time		<- (((million * trunc(parts))) : (length-1)) / snd@samp.rate
indices		<- match(timeevents, time)
indices		<- indices[complete.cases(indices)]
# tp 		<- c(tp, time[indices])
vtp[(time[indices[1]]/2) : (time[indices[length(indices)]]/2)] <- time[indices]
prevplus				<- 	sleftnorm[1:(indices[1] - 1)]
vprevl					<- 	c(vprevl, prevplus)
vampl[indices[1]/2]		<- 	sqrt(mean(vprevl^2))
ampl 					<- 	c(ampl,rms)

prevplus				<- 	srightnorm[1:(indices[1] - 1)]
vprevr					<- 	c(vprevr, prevplus)
vampr[indices[1]/2]		<- 	sqrt(mean(vprevr^2))
ampr 					<- 	c(ampr,rms)

###insert the code for having root mean
###square for the time interval of 2 seconds
for( j in 1:(length(indices)-1))
{
	rms 						<-	sqrt(mean(sleftnorm[indices[j]:indices[j+1]]^2))
	vampl[(time[indices[j]]/2)] <-	rms
	ampl 						<- 	c(ampl,rms)

	rms 						<-	sqrt(mean(srightnorm[indices[j]:indices[j+1]]^2))
	vampr[(time[indices[j]]/2)] <-	rms
	ampr 						<- 	c(ampr,rms)
}

##update the previous to the last parts of current channel
if(tail(indices,1) < length(sleftnorm))
{
	vprevl		<-	sleftnorm[(tail(indices,1) + 1):length(sleftnorm)]
}
if(tail(indices,1) < length(srightnorm))
{
	vprevr		<-	srightnorm[(tail(indices,1) + 1):length(srightnorm)]
}
    

# sleftnorm	<- sleftnorm[indices]
# srightnorm	<- srightnorm[indices]
# ampl		<- c(ampl, sleftnorm)
# ampr		<- c(ampr, srightnorm)	

#plot(time, sleftnorm, 	main = "left channel", 	xlab = c('Time (s) ', time[1], 'to', time[length(time)]), ylab = 'Amplitude')
#plot(time, srightnorm, main = "right channel", xlab = c('Time (s) ', time[1], 'to', time[length(time)]), ylab = 'Amplitude')
	
