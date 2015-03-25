#plots the rms value of 2 seconds intervals for 1345 time points

plot_log_sound_conv <- function(path2audio, start, end, parnew, color, labels = FALSE)
{
	#png(filename=paste(name,x,'_',y,'_',z,'.png', sep=''))
	par(new = parnew)
	# the sound signal is convolved with hrf and then passed in the plot function
	
	if(!labels)
		plot(log10(as.numeric(readLines(path2audio)))[start:end], type = 'l', axes = FALSE, xlab = '', ylab = '', col = color)
	else
		plot(log10(as.numeric(readLines(path2audio)))[start:end], type ='l', col = color)
}
