<style type="text/css">
table {
   max-width: 95%;
   border: 1px solid #ccc;
}
 
th {
  background-color: #dcdcdc;
}
 
td {
  background-color: #dcdcdc;
}
</style>

# Statistical Inference Course Project: Part 1



### Overview
This project explores the Central Limit Theorem (CLT) using the exponential distribution in R. The CLT states that the arithmetic mean of independent and identically distributed random variables will be a normally distributed random variable about the population mean, $\mu$. We compare this theoretical normal distribution to the distribution of arithmetic means of samples using the exponential distribution in R.

### Simulations
As part of our exploration of the CLT we will simulate 1000 samples of size 40. First, we simulate all 1000*40 draws from an exponential distribution, $Exp(\lambda = 0.2)$, and calculate the mean and standard deviation of the entire population. 


```r
lambda <- 0.2
n <- 40
sims <- 1000

set.seed(1234)
expdistr <- data.frame(rexp(n*sims, lambda))
colnames(expdistr) <- "values"
exp_mean <- mean(expdistr$values)
exp_sd <- sd(expdistr$values)
```
The resulting distribution is plotted below. Also depicted are the population mean, $\mu =$ 4.97, and population standard deviation, $\sigma =$ 4.93.


```r
exp_plot <- ggplot(expdistr, aes(x=values)) + geom_density(alpha=0.5, fill="blue") + ggtitle("Exponential Distribution (40,000 draws)") + xlab("Values") + ylab("Density") +  geom_vline(aes(xintercept=exp_mean), size=1, color="black") + annotate("text", x=11, y=0.125, label=paste("mu ==", round(exp_mean, 2)), parse=T) + annotate("text", x=11, y=0.12, label=paste("sigma ==", round(exp_sd, 2)), parse=T) + theme(plot.title = element_text(face="bold"))

print(exp_plot)
```

![plot of chunk expdistr](figure/expdistr-1.png) 

In order to simulate the distribution of sample means from the above exponential distribution population we use the same 1000*40 draws and organize them into 1000 samples of size 40. We then calculate the mean of each of the 1000 samples and the overall mean of the sample means. 


```r
set.seed(1234)
samples <- matrix(rexp(n*sims, lambda), sims, n)
mns <- data.frame(apply(samples, 1, mean))
colnames(mns) <- "values"
mns$plot <- rep("Sample", 1000)
mns_mean <- mean(mns$values)
```
According to the CLT, the distribution of the sample means should follow a normal distribution with a mean equal to the population mean ($\mu$) of the underlying distribution and a standard deviation equal to the population standard deviation of the underlying distribution divided by the square root of the sample size ($\frac{\sigma}{\sqrt{n}}$).

Because the underlying distribution in this simulation is exponential, we use the theoretical mean and standard deviation of the exponential distribution to find the corresponding theoretical normal distribution according to the CLT. For an exponential distribution, $Exp(\lambda)$, both the mean and standard deviation are equal to $\frac{1}{\lambda}$.  

Using these properties we calculate the mean and standard deviation(referred to as the standard error) of the theoretical CLT normal distribution. 

```r
mu <- 1/lambda
sd <- 1/lambda
se <- sd/sqrt(n)
```
With these paramaters of the normal distribution calculated, we can plot the distribution of the sample means and the theoretical normal distribution according to the CLT together. Such a plot is shown below. The mean and standard deviation of each distribution are also labeled to illustrate the CLT in action.

```r
mean_df <- data.frame(mean=c(round(mns_mean,2), mu), meanlab=c(deparse(formatC(round(mns_mean,2),digits=2,format="f")),deparse(formatC(mu, digits=2, format="f"))), plot=c("Sample", "Theoretical"), hjust=c(2, -2), label=c("E(bar(x)) ==", "mu =="), sd=c(deparse(formatC(round(sd(mns$values),2),digits=2,format="f")), deparse(formatC(round(se,2), digits=2, format="f"))), sdlab=c("sigma[bar(x)] ==", "SE =="))

comb_plot <- ggplot(mns, aes(x=values, fill=plot)) +
geom_density(alpha=0.3) + 
stat_function(fun=dnorm, color="black", geom="ribbon", mapping=aes(ymin=0, ymax=..y.., fill="Theoretical"), alpha=0.3, args=list(mean=mu, sd=se)) +
geom_vline(data=mean_df, aes(xintercept=mean, color=plot)) +
geom_text(data=mean_df, aes(mean, .52, label=paste(label, meanlab), color=plot, hjust=hjust), parse=TRUE, size=5, family="sans", fontface="italic") +
geom_text(data=mean_df, aes(mean, .52, label=paste(sdlab, sd), color=plot, hjust=hjust+0.5, vjust=2), parse=TRUE, size=5, family="sans", fontface="italic") +
scale_x_continuous(limits=c(2,8)) + 
ggtitle("Sample Means Distrubtion vs Theoretical Distribution\n(sample size n=40)") +
theme(plot.title = element_text(face="bold")) + 
xlab("Values") +
ylab("Density")

print(comb_plot)
```

![plot of chunk combinedplot](figure/combinedplot-1.png) 
### Means and Standard Deviations 


|     | Sample| Theoretical| Exponential|
|:----|------:|-----------:|-----------:|
|Mean |   4.97|        5.00|        4.97|
|SD   |   0.77|        0.79|        4.93|
The table above illustrates the accuracy of the CLT with regard to the mean and standard deviation of the distribution of sample means. The sample mean and standard deviation lie fairly close to the theoretically calculated mean and standard deviation according to CLT normal distribution.

Additionally, consistent with the CLT, the sample mean is identical to the mean of the underlying exponential distribution. Also consistent with the CLT, the sample standard deviation is approximately equal to the standard deviation of the underlying exponential distribution divided by the square root of the sample size, as shown below.  
> $\frac{\sigma}{\sqrt{n}} = \frac{4.93}{\sqrt{40}} = 0.79 \approx 0.77 = \sigma_{\bar{x}}$ 

### Distribution
Although the plot of distributions indicates that the sample mean distribution closely resembles the theoretical normal distribution, a more accurate assessment involves computing and comparing the corresponding quantiles of each distribution. A Q-Q plot performing that comparison is shown below with the line representing the theoretical normal distribution and the points representing the sample mean distribution. The x-axis displays the theoretical quantiles of a standard normal distribution.

```r
qqnorm(mns$values)
qqline(mns$values)
```

![plot of chunk qqplot](figure/qqplot-1.png) 

The Q-Q plot indicates that the sample mean distribution essentially follows the CLT normal distribution close to the median, but tends to deviate towards the tails. The accuracy would increase with more samples and a larger sample size, n. Nevertheless, these simulations are an illustrative example of the CLT as applied to an exponential distribution.
