#Sonify time series data to corresponding pitches on the chromatic scale
#in equal temperament and generate wav file

#Lin Yangchen

rm(list=ls())
require(tuneR)


#=========================================
#	SPECIFY PITCH RANGE AND SPEED
#=========================================

#all available note frequencies in Hz
#(must be in increasing order for findInterval() function).
#accompanying file freq.txt contains notes of the piano.
freq <- rev(scan('freq.txt'))

#specify from which note to which note to map species data to
#max for piano above is 1:88
#violin is 35:80 if subsetting from piano
minnote <- 35
maxnote <- 80
notenums <- minnote:maxnote

#duration of each note (in seconds) in wav file
dur <- 0.05


#=========================================
#	DATA IMPORT AND VISUALIZATION
#=========================================

#import data from a file data.txt (sample provided)
data <- scan('data.txt')
times <- 1:length(data)

#visualise data
pdf('timeseries.pdf')
plot(data, type='l')
dev.off()


#=========================================
#	SONIFY DATA
#=========================================

pitchdata <- rep(NA, length(data))
mindata <- min(data)
mult <- (maxnote - minnote)/(max(data) - min(data))
for(i in 1:length(data))
{
	#rescale species data to range of note numbers
	notedata <- mult*(data[i] - mindata) + minnote
	
	#snap to nearest note number and convert to pitch
	ind <- findInterval(notedata, notenums)
	if(ind != length(notenums))
	{
		diff1 <- notedata - notenums[ind]
		diff2 <- notenums[ind+1] - notedata
		if(diff1 < diff2)
		{
			pitchdata[i] <- freq[notenums[ind]]
		} else
		{
			pitchdata[i] <- freq[notenums[ind+1]]
		}
	} else
	{
		pitchdata[i] <- freq[notenums[ind]]
	}
}

#create sound file
Wobj <- sine(pitchdata[1], duration=dur, xunit='time')
for(timecounter in 2:length(data))
{
	Wobj <- bind(Wobj, sine(pitchdata[timecounter], duration=dur, xunit='time'))
}
writeWave(Wobj, 'sound.wav')





