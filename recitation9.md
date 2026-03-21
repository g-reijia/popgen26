### Recitation written by Dr. Joaquin Nunez and were accessed from: https://github.com/Jcbnunez/uvm-popgen/tree/main/Recitations for PopGen SP26

# Recitation 9: Fixation index ($F_{ST}$)

$F_{ST}$ is a measure of genetic differentiation between populations. It quantifies the proportion of genetic variation that is due to differences between populations relative to the total genetic variation. $F_{ST}$  values range from 0 to 1:

-   An $F_{ST}$  of 0 indicates no genetic differentiation, meaning populations share the same allele frequencies.
    
-   An $F_{ST}$  of 1 indicates complete differentiation, meaning populations are fixed for different alleles.

$F_{ST}$  is influenced by factors such as gene flow, genetic drift, selection, and mutation. Low $F_{ST}$  values suggest significant gene flow between populations, while high $F_{ST}$  values indicate restricted gene flow and strong population structuring. It is commonly used in population genetics to study migration, local adaptation, and evolutionary processes.

The $F_{ST}$  metric, in essence, is a metric of genetic differentiation and thus can be used to measure a variety of evolutionary processes, from drift to selection.

-   Genetic drift increases $F_{ST}$ because it reduces genetic variation within populations while increasing genetic differences between populations. At the same time, because drift is random, populations that start with similar allele frequencies will experience independent fluctuations. Over time, these random changes lead to divergence in allele frequencies between populations, increasing the total genetic variance among populations relative to the total variance.
    
-   Selection increases $F_{ST}$​ by reducing genetic diversity within populations through allele fixation and by driving divergence in allele frequencies between populations when selective pressures differ. This happens because local adaptation favors different alleles in different environments, leading to greater genetic differentiation $F_{ST}$ among populations.
    
-   Migration reduces $F_{ST}$​ by increasing gene flow between populations, which homogenizes allele frequencies. This reduces genetic differentiation by replenishing genetic diversity within populations and decreasing differences in allele frequencies between them, lowering $F_{ST}$​​.

### Step 1: Let's create a function to calculate $F_{ST}$​ . We will need the frequencies of the alleles in population 1 and in population 2. The formula for $F_{ST}$​  is

$$F_{ST}=\frac{H_{T}-\bar{H_{S}}}{H_{T}} $$ 

where, $H_{T} =$ represents the heterozygosity (i.e., $H=2pq$) of the population estimated using the mean values of $\bar{p}$ and $\bar{q}$ for all populations  (i.e., $\bar{H_{S}}=2\bar{p}\bar{q}$).

Likewise, $\bar{H_{S}}=$ represents the mean heterozygosity  across all populations. That is taking the mean of all individual heterozygosities.

#### Lets transform that into code:

```{r}
fst_calc = function( p1, p2 ){
q1=1-p1
q2=1-p2

Het1=2*p1*q1
Het2=2*p2*q2

Ht = 2*mean(c(p1,p2))*mean(c(q1, q2))
Hs = mean(c(Het1, Het2))
fst =abs(Ht - Hs)/Ht
return(fst)
}

```

###  Step 2: Let's create a simulation to understand $F_{ST}$​ . First, let’s create separate simulations for both habitats, as if they were independent

```{r}
p=0.5
m=0.1 #set migration rate

#We will create two vectors for the recursive functions, one for each population
p1_recur=c()
p2_recur=c()

for(g in 1:100){

if(g==1){
p1_recur[g]=p
p2_recur[g]=p
} else{
#We will first code the action of selection on each population

p1_component = general_selection(p1_recur[g-1], sii=0.2, sij=0.05, sjj=0.0)

p2_component = general_selection(p2_recur[g-1], sii=0.0, sij=0.05, sjj=0.2)

p1_delta = p1_component - p1_recur[g-1]
p2_delta = p2_component - p2_recur[g-1]

p1_recur[g] = p1_recur[g-1] + p1_delta
p2_recur[g] = p2_recur[g-1] + p2_delta

} # close if-else
} # close g loop

sel_only = data.frame(g=1:100, p1_recur, p2_recur)

plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
cl <- rainbow(2)
lines(sel_only$g, sel_only$p1_recur,col = cl[1],type = 'l' )
lines(sel_only$g, sel_only$p2_recur,col = cl[2],type = 'l' )

```

**#<<QUESTION 1: Create code to evaluate $F_{ST}$​  as a function of time. Print the graph of ($F_{ST}$​  vs generation)in your report and describe what is happening. What is the impact of natural selection in $F_{ST}$​ ? What is the Fst at generation 10? What is the $F_{ST}$​  at generation 100? Explain the difference.>> <<Hint, you may choose to use a loop to evaluate the $F_{ST}$​  function at each generation>>**

### Step 3: Let's now explore what happens when there is gene flow?

```{r}
p=0.5
m=0.1 #set migration rate

#We will create two vectors for the recursive functions, one for each population
p1_recur=c()
p2_recur=c()

for(g in 1:100){
if(g==1){
p1_recur[g]=p
p2_recur[g]=p
} else{

#We will first code the action of selection on each population
p1_component = general_selection(p1_recur[g-1], sii=0.2, sij=0.05, sjj=0.0)
p2_component = general_selection(p2_recur[g-1], sii=0.0, sij=0.05, sjj=0.2)

p1_delta = p1_component - p1_recur[g-1]
p2_delta = p2_component - p2_recur[g-1]

#Now we will encode the migration effect
p1_mig_delta = migration_rate( m, p2_recur[g-1], p1_recur[g-1] )
p2_mig_delta = migration_rate( m, p1_recur[g-1], p2_recur[g-1] )

p1_recur[g] = p1_recur[g-1] + p1_delta + p1_mig_delta
p2_recur[g] = p2_recur[g-1] + p2_delta + p2_mig_delta

} # close if-else
} # close g loop

sel_mig = data.frame(g=1:100, p1_recur, p2_recur)

plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
cl <- rainbow(2)
lines(sel_mig$g, sel_mig$p1_recur,col = cl[1],type = 'l' )
lines(sel_mig$g, sel_mig$p2_recur,col = cl[2],type = 'l' )
```

**#<<QUESTION 2: Deploy your code for this new simulation. Print the plot and describe, what has changed? What is the $F_{ST}$​  at generation 10? What is the $F_{ST}$​  at generation 100? Explain the difference.**

### Step 4: Assessing the impacts of genetic drift on $F_{ST}$​ . Lastly, we will simulate what happens when drift alone operates in the population. For simplicity, we will again assume that populations do not have gene flow. We will assume Ne = 100 for both populations.

```{r}
Ne = 100
p = 0.5

drift_p_t1 = function( p, Ne ){

num=rbinom(1, Ne, p)
dem=Ne
p_t1 = num/dem
return(p_t1)
}

#We will create two vectors for the recursive functions, one for each population

p1_recur=c()
p2_recur=c()

for(g in 1:100){
if(g==1){
p1_recur[g]=p
p2_recur[g]=p
} else{
#We will first code the action of selection on each population
#Now we will encode the migration effect

p1_drift = drift_p_t1(p1_recur[g-1], Ne )
p1_delta = p1_drift - p1_recur[g-1]
p2_drift = drift_p_t1(p2_recur[g-1], Ne )
p2_delta = p2_drift - p2_recur[g-1]

p1_recur[g] = p1_recur[g-1] + p1_delta
p2_recur[g] = p2_recur[g-1] + p2_delta

} # close if-else
} # close g loop

drift_2_pops = data.frame(g=1:100, p1_recur, p2_recur)

plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
cl <- rainbow(2)
lines(drift_2_pops$g, drift_2_pops$p1_recur,col = cl[1],type = 'l' )
lines(drift_2_pops$g, drift_2_pops$p2_recur,col = cl[2],type = 'l' )
```

**#<<QUESTION 3: Deploy your code for the drift simulation. Print the plot and describe what is the impact of genetic drift on $F_{ST}$​ ? What is the $F_{ST}$​  at generation 10? What is the Fst at generation 100? Explain the difference.**

**#<<QUESTION 4: Reflect on the behaviour of $F_{ST}$​ as a function of selection, drift, and migration… assuming you only had access to fst data, how can these processes be told apart from each other, if at all?**
