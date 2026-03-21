# Recitation 2: Selection

Objective:  
Learn to use R to model natural selection in a population, calculate allele frequencies over generations, and visualize the impact of selection on a population.

Natural selection changes allele frequencies in a population based on the fitness of different genotypes. For a single locus with two alleles:

* $p = f(i)$ 
* $q = f(j)$
* $p+q=1$

Fitness values (w) for genotypes:

* $w_{ii} = 1$
* $w_{ij} = 1$  (assumes full  dominance of i over j)
* $w_{jj} = 1-s_{jj}$

We'll simulate the evolution of allele i over generations under different selection scenarios.

  #### Step 1: Initialize Parameters
```r
# Initial allele frequency

p <- 0.6
q <- 1 - p

##### Step 2: Build a selection “forecasting” function. THIS FUNCTION MUST ONLY USE 2 PARAMETERS (p, s)

calc_p_t1 = function( p, s ){
q=1-p
num=<<QUESTION 1: What should this code be?, why?>>
dem=<<QUESTION 2: What should this code be? why?>>
p_t1 = num/dem
return(p_t1)

}
```
##### Step 3: Evaluate the function across a different range of selection coefficients using a loop

```r
s_values=seq(from = 0.01, to = 0.3, by = 0.01)

#the function will automatically evaluate all the values at once (this is often used when only 1 variable is evaluated…)

p_results = calc_p_t1(<<QUESTION 3: What should these parameters be?>>)

#we will now create a data frame with both our results, the original allele frequency, and our values of s

my_results = data.frame(s_values, p, p_results)
```
  #### Step 4: Let’s evaluate the magnitude of the change
```r
deltas = my_results$p_results - my_results$p

my_results = cbind(my_results, deltas)
```

#### Step 5: Let’s plot our results: the function of s relative to the values of delta
```r
plot(my_results$s_values, my_results$deltas)

<<QUESTION 4: Print the plot to your report and describe the pattern that you see–and its evolutionary significance>>
```

#### Step 6: Let’s evaluate the significance of the relationship between s relative to the values of delta. 
To this end we will implement a “correlation test.” A correlation test is a statistical method used to determine whether there is a relationship or association (“r”) between two quantitative (numerical) variables and to quantify the strength and direction of that relationship. When r=1: Perfect positive correlation (as one variable increases, the other increases proportionally), r=−1: Perfect negative correlation (as one variable increases, the other decreases proportionally), r=0: No correlation (no linear relationship between the variables).

```r
cor.test(~deltas+s_values, data = my_results)

#Notice that the syntax of the correction test, “~VAR1 + VAR2”, is essentially asking is VAR1 correlated to VAR2

<<QUESTION 5: What is the value of the association? What is the p-value of the association? What does this mean about the relationship between the allele frequency change and the strength of selection>>
```
