#plots the rms value of 2 seconds intervals for 1345 time points
#parnew indicates if the plot is overlayed on previous one or not
plot_log_sound <- function(path2audio, start, end, parnew, color, labels = FALSE)
{
	#png(filename=paste(name,x,'_',y,'_',z,'.png', sep=''))
	par(new = parnew)
	if(!labels)
		plot(log10(as.numeric(readLines(path2audio)))[start:end], type = 'l', axes = FALSE, xlab = '', ylab = '', col = color)
	else
		plot(log10(as.numeric(readLines(path2audio)))[start:end], type ='l', col = color)
}
