# https://systematicinvestor.wordpress.com/?s=black+litterman
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


# Use reverse optimization to compute the vector of equilibrium returns
bl.compute.eqret <- function
(
  risk.aversion,  # Risk Aversion
  cov,        # Covariance matrix
  cap.weight,     # Market Capitalization Weights
  risk.free = 0   # Rsik Free Interest Rate
)
{
  return( risk.aversion * cov %*% cap.weight +  risk.free)    
}

#--------------------------------------------------------------------------
# Compute Risk Aversion, prepare Black-Litterman input assumptions
#--------------------------------------------------------------------------
ia = aa.test.create.ia.country()

# compute Risk Aversion
risk.aversion = bl.compute.risk.aversion( ia$hist.returns$USA )

# the latest market capitalization weights
cap.weight = last(hist.caps.weight) 

# create Black-Litterman input assumptions  
ia.bl = ia
ia.bl$expected.return = bl.compute.eqret( risk.aversion, ia$cov, cap.weight )

# Plot market capitalization weights and implied equilibrium returns
layout( matrix(c(1,1,2,3), nrow=2, byrow=T) )
pie(coredata(cap.weight), paste(colnames(cap.weight), round(100*cap.weight), '%'), 
    main = paste('Country Market Capitalization Weights for', format(index(cap.weight),'%b %Y'))
    , col=plota.colors(ia$n))

plot.ia(ia.bl, T)

#--------------------------------------------------------------------------
# Create Efficient Frontier(s)
#--------------------------------------------------------------------------
n = ia$n

# -1 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Historical', equally.spaced.risk = T)       
ef.risk.bl = portopt(ia.bl, constraints, 50, 'Black-Litterman', equally.spaced.risk = T)    

# Plot multiple Efficient Frontiers and Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk), portfolio.risk, T, T)            
plot.ef(ia.bl, list(ef.risk.bl), portfolio.risk, T, T) 

