# Recitation 3: Temporal fluctuating selection


Temporally fluctuating selection is a form of balancing selection in which the selective pressures acting on a population vary over time. This temporal fluctuation leads to shifts in the traits or alleles that are favored within an environment, depending on changing ecological conditions. Unlike overdominance, where heterozygotes (e.g., xij) consistently exhibit the highest fitness, temporally fluctuating selection operates differently: each homozygote (xii or xjj) is favored at different times. For instance, xii​ may have the highest fitness during the summer, whereas xjj is favored during the winter. This dynamic promotes the persistence of genetic diversity within the population by periodically shifting which genotype is advantageous.

These dynamics create a unique scenario in which temporally fluctuating selection can be conceptualized as a form of "contingent positive selection," where selection coefficients alternate over time. To explore this concept further, we can test the hypothesis using simulations.

###  Step 1: Recall our old and trusty function for generalized selection 😀

```{r}
general_selection = function( p, sii, sij, sjj ){

q=1-p

num=(p^2)*(1-sii) + p*q*(1-sij)

dem=(p^2)*(1-sii) + 2*p*q*(1-sij) + (q^2)*(1-sjj)

p_t1 = num/dem

return(p_t1)
}
```

### Step 2: Now let's propose the “ecology” of our simulation. For our simulation, we will make a few assumptions.

####  Ecology: We will simulate an environment with two seasons–summer and winter.

####  Fitness: We will assume that wii is favored in the summer, wij is favored in the winter. For simplicity, we will assume that the heterozygote is always intermediate, as follows.

  
We have built this model inspired by the ecological dynamics of seasonality in Drosophila melanogaster (the common fruit fly). Seasonality in Drosophila refers to the cyclical changes in their populations, behaviors, and genetic composition in response to seasonal environmental fluctuations, such as temperature, humidity, and resource availability. For example, during warmer months, Drosophila melanogaster populations tend to expand rapidly due to favorable conditions, while colder seasons often result in reduced activity or overwintering survival strategies. Seasonality also influences allele frequencies, with genetic variants conferring fitness advantages under specific seasonal conditions (e.g., heat tolerance in summer, cold resistance in winter) fluctuating predictably across generations. This phenomenon provides a natural framework for studying evolutionary dynamics and adaptation.

|geno|xii|xij|xjj|
|---|---|---|---|
|summer|1|0.8|0.6|
|winter|0.6|0.8|1|

Furthermore, we will assume that the organism that we want to simulate is “multivoltine” (i.e., reproduces many times within a year; e.g., fruit flies). In this context, both the summer and the winter last 5 generations each. We will simulate 100 generations.

### Step 3: Creating a counter for “summer” and “winter.”

In our model, we will assume that summer and winter occur every 5 generations. How can we incorporate this in our simulation? One way is to introduce a “switch” in our code such that for all generations ending in 0-4 (e.g., 10,11,12,13,14…40,41,42…etc) the program will evaluate summer conditions. On the other hand, all generations ending in 5-9 will be evaluated as if they were winters.

```{r}
#We can accomplish this using the “grepl” function

x <- 1:10

grepl("[0-4]$", x)
grepl("[5-9]$", x)

#it work with larger numbers too
x <- 1:40
grepl("[0-4]$", x)
grepl("[5-9]$", x)

#Let’s test it
for(g in 1:100){
if(grepl("[0-4]$", g)){

print("summer")
}else if(grepl("[5-9]$", g)){
print("winter")
}

  

}# close g
```

### <<QUESTION 1: Explain what is going on in these grepl calls, how is this going to help us assess winter and summer>>

###  Step 4: Set up the simulation

Now we will set up the simulation, and we will condition the generation counter to vary its behaviour for summers and winters

### Question 2 is embedded in the code... 
```{r}
#define an initial allele frequency, and the empty variable for "recursive" use

p=0.5

#and the empty variable for "recursive" use

p_recur=c()

  

#lets simulate 100 generations

for(g in 1:100){

  

if(g==1){

p_recur[g]=p

} else{

#<<QUESTION 2: Complete the missing values, recall the difference between “w” and “s”>>

if(grepl("[0-4]$", g)){

print("summer")
sii=<<?>>
sij=<<?>>
sjj=<<?>>

}else if(grepl("[5-9]$", g)){

print("winter")
sii=<<?>>
sij=<<?>>
sjj=<<?>>
}
p_recur[g] = general_selection(p_recur[g-1], sii, sij, sjj)

} # close if-else
} # close loop

#create a dataframe to save your results

seasonal_selection = data.frame(g=1:100, p_recur)
```

### Step 5: Plot the simulations
```{r}
plot(seasonal_selection$g, seasonal_selection$p_recur, type ='l',ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
```
### <<QUESTION 3: Print this graph in your report and describe what is happening. How does the behaviour of this formulation differ from other types of directional selection? Do you think this behavior is “stable”?>>

### Step 6: A challenge … 

### <<QUESTION 4: Modify the existing code to create a simulation where the summer is now 7 generations (and 3 of winter), while also incorporating these new fitnesses: >>... provide a completed code, a figure, and the interpretation of your findings… Particularly, discuss whether your new simulation gave rise to a “stable” condition? Lastly, based on your simulation can you propose a definition of a “stable” vs. “unstable” balanced polymorphism?**

|geno|xii|xij|xjj|
|---|---|---|---|
|summer|1|0.5|0.6|
|winter|0.3|0.5|1|
