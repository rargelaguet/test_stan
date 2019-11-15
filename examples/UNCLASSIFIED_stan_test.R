
model_informative_prior = "
data {
	int<lower=0> N; // data
	int<lower=0> n[N];
	int<lower=0> y[N];
	real<lower=0> a; // prior
	real<lower=0> b;
	real<lower=0> e;
}
parameters {
	real<lower=0,upper=1> mu;
	real<lower=0> eta;
	real<lower=0,upper=1> theta[N];
}
transformed parameters {
	real<lower=0> alpha;
	real<lower=0> beta;
	alpha = eta*mu;
	beta = eta*(1-mu);
}
model {
	mu ~ beta(a,b);
	eta ~ exponential(e);
	theta ~ beta(alpha,beta);
	y ~ binomial(n,theta);
}
"

d = structure(list(date = structure(c(16017, 16021, 16024, 16027, 
16028, 16033, 16036, 16038, 16042, 16055, 16058, 16067, 16070, 
16074, 16077, 16081, 16083, 16088, 16092, 16095, 16097, 16102, 
16105, 16109), class = "Date"), opponent = c("davidson", "kansas", 
"florida atlantic", "unc asheville", "east carolina", "vermont", 
"alabama", "arizona", "michigan", "gardner-webb", "ucla", "eastern michigan", 
"elon", "notre dame", "georgia tech", "clemson", "virginia", 
"nc state", "miami", "florida state", "pitt", "syracuse", "wake forest", 
"boston college"), made = c(0L, 0L, 5L, 3L, 0L, 3L, 0L, 1L, 2L, 
4L, 1L, 6L, 5L, 1L, 1L, 0L, 1L, 3L, 2L, 3L, 6L, 4L, 4L, 0L), 
    attempts = c(0L, 0L, 8L, 6L, 1L, 9L, 2L, 1L, 2L, 8L, 5L, 
    10L, 7L, 4L, 5L, 4L, 1L, 7L, 6L, 6L, 7L, 9L, 7L, 1L), 
game = 1:24), 
row.names = c(NA, -24L), class = "data.frame")

a = 6
b = 14
e = 1/20

dat = list(y = d$made, n = d$attempts, N = nrow(d),a = a, b = b, e = e)
m = stan_model(model_code = model_informative_prior)
r = sampling(m, dat, c("mu","eta","alpha","beta","theta"), iter = 10000)

plot(r, pars=c('eta','alpha','beta'))
plot(r, pars=c('mu','theta'))
