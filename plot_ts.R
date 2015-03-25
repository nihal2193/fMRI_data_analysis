#this function plots the timeseries of a point x y z 
#with the time series of sound signal on the same graph

plot_ts <- function(ts, start, end, parnew, color, labels = FALSE)
{
	par(new = parnew)
	if(!labels)
		plot(ts[start:end], type = 'l', axes = FALSE, xlab = '', ylab = '', col = color)
	else
		plot(ts[start:end], type='l')
}
