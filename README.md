<!DOCTYPE html>

<head>


</head>



<body>




<div class="container-fluid main-container">



<!-- tabsets -->




<!-- code folding -->













<div class="fluid-row" id="header">







<h1 class="title toc-ignore">ToothGrowth Data Analysis</h1>

<h4 class="author"><em>Juan Mari Sebastian Carino</em></h4>

<h4 class="date"><em>October 20, 2018</em></h4>



</div>





<div id="case-overview" class="section level2">

<h2>Case Overview</h2>

<p>The ToothGrowth data tells about the effect of Vitamin C on tooth growth in guinea pigs. The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three does levels of Vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of Vitamin C and coded as vc).</p>

<p>This description is adapted from the C.I. Bliss (1952). The Statistics of BIoassay. Academic Press.</p>

</div>

<div id="analysis" class="section level2">

<h2>Analysis</h2>

<pre class="r"><code>summary(ToothGrowth)</code></pre>

<pre><code>##       len        supp         dose      

##  Min.   : 4.20   OJ:30   Min.   :0.500  

##  1st Qu.:13.07   VC:30   1st Qu.:0.500  

##  Median :19.25           Median :1.000  

##  Mean   :18.81           Mean   :1.167  

##  3rd Qu.:25.27           3rd Qu.:2.000  

##  Max.   :33.90           Max.   :2.000</code></pre>

<p>The ToothGrowth data has three columns: the len (tooth length), supp (supplement type (VC or OJ)), and dose (dose in milligrams/day). Below is the summary of the ToothGrowth data showing the min, max, median, mean, and 1st &amp; 3rd quantiles.</p>

<p>In this analysis, the author will compare the tooth growth by supp and dose. There will be two sets of tests: one comparison of tooth growth between OJ and VC and one comparison of tooth growth among the number of dosages between OJ and VC. Using t test statistic, the author will test whether there is a difference between the mean of tooth growth given the supplement is OJ and the mean of tooth growth given the supplement is VC.</p>

<p>The hypothesis test shall be: Difference in Toothgrowth means relative to supplement type</p>

<ul>

<li><p>H0: mu_OJ - mu_VC = 0</p></li>

<li><p>Ha: mu_OJ - mu_VC != 0</p></li>

</ul>

<p>Difference in Toothgrowth means relative to number of dosages</p>

<ul>

<li>H0:

<ul>

<li>mu_05 - mu_1 = 0</li>

<li>mu_1 - mu_2 = 0</li>

<li>mu_05 - mu_2 = 0</li>

</ul></li>

<li>Ha:

<ul>

<li>mu_05 - mu_1 != 0</li>

<li>mu_1 - mu_2 != 0</li>

<li>mu_05 - mu_2 != 0</li>

</ul></li>

</ul>

<pre class="r"><code># Subset of data - OJ &amp; VC

OJ &lt;- ToothGrowth$len[ToothGrowth$supp == &quot;OJ&quot;]

VC &lt;- ToothGrowth$len[ToothGrowth$supp == &quot;VC&quot;]



# Standard Deviation

sd_OJ &lt;- sd(OJ)

sd_VC &lt;- sd(VC)



# Mean

mu_OJ &lt;- mean(OJ)

mu_VC &lt;- mean(VC)</code></pre>

<pre class="r"><code># Consolidating Standard Deviations and Means

con &lt;- data.frame(c(mu_OJ, mu_VC), c(sd_OJ, sd_VC), row.names =c(&quot;OJ&quot;, &quot;VC&quot;) )

colnames(con) &lt;- c(&quot;Mean&quot;, &quot;Sd&quot;)

con</code></pre>

<pre><code>##        Mean       Sd

## OJ 20.66333 6.605561

## VC 16.96333 8.266029</code></pre>

<pre class="r"><code># Calculation of T test

t.test(OJ, VC, paired=TRUE, alternative=&quot;two.sided&quot;)</code></pre>

<pre><code>## 

##  Paired t-test

## 

## data:  OJ and VC

## t = 3.3026, df = 29, p-value = 0.00255

## alternative hypothesis: true difference in means is not equal to 0

## 95 percent confidence interval:

##  1.408659 5.991341

## sample estimates:

## mean of the differences 

##                     3.7</code></pre>

<p>Based from the table, the mean of tooth growth using orange juice as supplement type is higher than the mean of tooth growth using ascorbic acid. The standard deviation, on the other hand is higher for ascorbic acid and lower for orange juice. Now, we will see through out t-test statistic if there is a difference in the means between ascorbic acid and orange juice. Under 95% confidence interval, the p-value of our t-test is 0.00255 which is less than 0.05. This means that we reject the null hypothesis and we conclude that there is a difference between two types of supplement type. This may be true because our supplement types are delivery methods; the dosages of Vitamin C contained in these supplement types are different. Based on the graph, it can be seen that orange juice has more effect on tooth length as a supplement type rather than ascorbic acid.</p>

<pre class="r"><code>library(ggplot2)

g &lt;- ggplot(data=ToothGrowth, aes(dose, len))

g + geom_point(aes(colour=dose)) + theme_dark() + facet_grid(.~supp) + ylab(&quot;Tooth Length&quot;) + xlab(&quot;Dosage&quot;) + ggtitle(&quot;Effect of Vitamin C to Tooth Length of Guinea Pigs&quot;)</code></pre>


![png](https://github.com/jmsebastiancarino/Course_Project_-_Statistical_Inference/blob/master/Output%20Plot.png)



<p>The author will perform a t-test statistic to test the difference in the means of tooth growth relative to the amount of dosage of Vitamin C.</p>

<pre class="r"><code># Subset of data - OJ &amp; VC

M &lt;- ToothGrowth$len[ToothGrowth$dose == 0.5]

N &lt;- ToothGrowth$len[ToothGrowth$dose == 1]

O &lt;- ToothGrowth$len[ToothGrowth$dose == 2]



# Standard Deviation

sd_M &lt;- sd(M)

sd_N &lt;- sd(N)

sd_O &lt;- sd(O)



# Mean

mu_M &lt;- mean(M)

mu_N &lt;- mean(N)

mu_O &lt;- mean(O)</code></pre>

<pre class="r"><code># Consolidating the standard deviations and means

con &lt;- data.frame(c(mu_M, mu_N, mu_O), c(sd_M, sd_N, sd_O), row.names =c(&quot;0.5&quot;, &quot;1&quot;, &quot;2&quot;))

colnames(con) &lt;- c(&quot;Mean&quot;, &quot;Sd&quot;)

con</code></pre>

<pre><code>##       Mean       Sd

## 0.5 10.605 4.499763

## 1   19.735 4.415436

## 2   26.100 3.774150</code></pre>

<p>Based from the result, the difference in means per dosage are large. However, the difference in standard deviations are small. To see if this result will be consistent, we will test the results of t-test statistic.</p>

<pre class="r"><code># Dosage 0.5 vs 1

t.test(N, M, paired=TRUE, alternative=&quot;greater&quot;)</code></pre>

<pre><code>## 

##  Paired t-test

## 

## data:  N and M

## t = 6.9669, df = 19, p-value = 6.127e-07

## alternative hypothesis: true difference in means is greater than 0

## 95 percent confidence interval:

##  6.863996      Inf

## sample estimates:

## mean of the differences 

##                    9.13</code></pre>

<pre class="r"><code># Dosage 1 vs 2

t.test(O, N, paired=TRUE, alternative=&quot;two.sided&quot;)</code></pre>

<pre><code>## 

##  Paired t-test

## 

## data:  O and N

## t = 4.6046, df = 19, p-value = 0.0001934

## alternative hypothesis: true difference in means is not equal to 0

## 95 percent confidence interval:

##  3.471814 9.258186

## sample estimates:

## mean of the differences 

##                   6.365</code></pre>

<pre class="r"><code># Dosage 0.5 vs 2

t.test(O, M, paired=TRUE, alternative=&quot;two.sided&quot;)</code></pre>

<pre><code>## 

##  Paired t-test

## 

## data:  O and M

## t = 11.291, df = 19, p-value = 7.19e-10

## alternative hypothesis: true difference in means is not equal to 0

## 95 percent confidence interval:

##  12.6228 18.3672

## sample estimates:

## mean of the differences 

##                  15.495</code></pre>

<p>Based from the results, the mean difference between 0.5 mg/day and 1 mg/day is 9.13; between 1 mg/day and 2 mg/day is 6.365; between 0.5 mg/day and 2 mg/day is 15.495. It can be observed that the mean difference is increasing as the dosage of vitaminc C increases. The p values of the differences based on our three t-test statistics are below 0.05. With that, the author concludes that the increase in tooth length is affected by the amount of Vitamin C dosage irrespective of supplement type. This holds true as well when you check the graph shown earlier.</p>

</div>









</div>







</body>

</html>
