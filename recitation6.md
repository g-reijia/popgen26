# Recitation 6: Integrating forces jointly with temporal fluctuating selection

In addition to temporal fluctuations in selection, other evolutionary forces can also influence the dynamics of genetic variation. Mutation, for instance, continually introduces new genetic variants into the population, which can interact with temporally fluctuating selection to either enhance or constrain adaptive potential. Similarly, other types of selection, such as directional or stabilizing selection, may act in conjunction with or counter to temporally fluctuating selection. For example, a trait favored during a particular season might also be under stabilizing selection to maintain an optimal range across years, or influenced by non-seasonal forces like temperature increases (e.g., climate change) or pollution. The interplay of these forces shapes the evolutionary trajectories of populations, highlighting the complex nature of adaptive processes in changing environments. This integrative dynamics can be modeled as:

$$p_{t+1}=p +\Delta_{s|t}p +\Delta_{s|D}p + \Delta_{\mu}p$$

Where $\Delta_{s|t}p$ is the change in allele frequency due to balancing selection (contingent on the temporal fluctuations), $\Delta_{s|D}p$ is the change in allele frequency due to directional selection (e.g., pollution, climate change, a pathogen), lastly,  $\Delta_{\mu}p$, is the change in allele frequency due to mutation.

In this simulation we will imagine a pleiotropic gene (i.e., a gene that modifies two phenotypes simultaneously). For example, a gene that plays a role in seasonality as well as being involved in response to pollution. Heat-shock proteins (HSPs) are examples of such “swiss army” genes. 
As the formula indicates, we’ll have to code our simulation evaluating each “delta” separately and then combine them to generate a “final evolutionary step.”   
Our model assumptions:

Ecology: We will simulate an environment with two seasons–summer and winter with 5 generations each.

Fitness: We will assume that wii is favored in the summer, wij is favored in the winter. For simplicity, we will assume that the heterozygote is always intermediate, as follows

|Season/Genotype →|xii|xij|xjj|
|-----------------|---|---|---|
|Summer(w)|1.0|0.8|0.6|
|Winter (w)|0.6|0.8|1.0|

Furthermore, we will assume that the organism that we want to simulate is “multivoltine” (i.e., reproduces many times within a year; e.g., fruit flies). In this context, both the summer and the winter last 5 generations each. We will simulate 100 generations. At the same time, we will also assume that these populations live in a polluted habitat and that the “j” allele is disfavored (selection against it = 0.1). The pleiotropic effect of pollution has a dominance coefficient of h=0.3.

At this point it is important to understand that you have two different forms of “independent” selection acting on the same gene… hence there are two entirely different sets of selection coefficients for the same genotypes (it’s apples and oranges… in the same orchard… if that makes sense!). 

### Step 1: Make a formula for seasonal selection (see Recitation 4)

```{r}
# Initial allele frequency
p <- 0.1

temporal_selection = function( p, sii, sij, sjj ){
  q=1-p
  num=(p^2)*(1-sii) + p*q*(1-sij) 
  dem=(p^2)*(1-sii) + 2*p*q*(1-sij) + (q^2)*(1-sjj)
  p_t1 = num/dem
  return(p_t1)
} 
```

### Step 2: Make a formula for directional selection with dominance

<<QUESTION 1: Code a function for directional selection with dominance… you can code the formula for delta (or the rate) directly.. Look at your notes>>

```{r}
#### Write your code/function here for question 1####
```

### Step 3: Make a formula for mutation
<<QUESTION 2: Code a function for mutational effects (ignore back mutation)… look at your lecture notes>>

```{r}
mutation = function( p, u ){
  q=1-p
  p_t1 = #### write your answer here for question 2#####
  return(p_t1)
}
```

Step 4: Create a simulation framework for these forces

```{r}
p_recur=c()

#lets simulate 100 generations
for(g in 1:100){
  
  if(g==1){
    p_recur[g]=p
  } else{
    #seasonal selection
    if(grepl("[0-4]$", g)){
      print("summer")
      sii=0.0; sij=0.2; sjj=0.4 
    }else if(grepl("[5-9]$", g)){
      print("winter")
      sii=0.4; sij=0.2; sjj=0.0 
    }
    seasonal_component = temporal_selection(p_recur[g-1], sii, sij, sjj)
    seasonal_delta = seasonal_component - p_recur[g-1]
    
    #directional component → you should have created a formula that estimates the delta directly!
    s=0.1; h=0.3 
    directional_delta = directional_rate(p_recur[g-1], s, h)
    
    
    #mutational component ### ignore back mutation for now!
    u=1e-8
    mutational_component = mutation(p_recur[g-1], u)
    mutational_delta = mutational_component - p_recur[g-1]
    
    ### Evaluate the evolutionary change
    
    p_recur[g] = p_recur[g-1] + 
      seasonal_delta +  
      directional_delta + 
      mutational_delta
    
  } # close if-else
} # close loop

#create a dataframe to save your results
integrated_selection = data.frame(g=1:100, p_recur)
```

### Step 5: Plot the simulations
```{r}

plot(integrated_selection$g, integrated_selection$p_recur, type ='l',ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
```

<<QUESTION 3: Print this graph in your report and describe what is happening. What is the long term evolutionary fate of these alleles? What inferences can you make about the impact of “multi-stressors” in adaptive evolution in wild populations>>

### Step 6: a small challenge

Mutation rate doesn't seem to be impacting the simulation much… lets see what would happen if we were to introduce a rate of back-mutation into the system (v)... Modify your function for mutation rate to include back mutation:

```{r}
mutation = function( p, u, v ){
q=1-p
p_t1 = p*(1-u)+ (1-p)*v
return(p_t1)
}
```

<<QUESTION 4:  holding v constant ($v=1x10^{-8}$) what magnitude of mutation rates ($\mu$) would you need to have to notice a difference… A ballpark is fine, but try to “force” the simulation to a steady state of **$p = 0.9$** … provide a plot to showcase your point (you may want to simulate 1000 generations…) >>.


