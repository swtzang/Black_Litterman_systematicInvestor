# load Systematic Investor Toolbox

rm(list=ls())
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#--------------------------------------------------------------------------
# Visualize Market Capitalization History
#--------------------------------------------------------------------------

hist.caps = aa.test.hist.capitalization()   
hist.caps.weight = hist.caps/rowSums(hist.caps)

# Plot Transition of Market Cap Weights in time
plot.transition.map(hist.caps.weight, index(hist.caps.weight), xlab='', name='Market Capitalization Weight History')

# Plot History for each Country's Market Cap
layout( matrix(1:9, nrow = 3, byrow=T) )
col = plota.colors(ncol(hist.caps))
for(i in 1:ncol(hist.caps)) {
  plota(hist.caps[,i], type='l', lwd=5, col=col[i], main=colnames(hist.caps)[i])
}