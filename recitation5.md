# Recitation 5: Spatially fluctuating selection

Spatially fluctuating selection is a form of balancing selection in which selective pressures vary across different locations within populations connected by gene flow. This spatial variation results in shifts in the traits or alleles favored in specific environments, depending on local ecological conditions. Unlike overdominance, where heterozygotes (e.g., xij) consistently exhibit the highest fitness, spatially fluctuating selection operates more like temporally fluctuating selection: each homozygote (xii, or xjj) is favored under different conditions. However, in contrast to temporally fluctuating selection, the maintenance of polymorphism under spatially fluctuating selection depends on gene flow between populations.

### <<QUESTION 1: Explain why is this process dependent on gene flow?>>

Indeed, we can contextualize the process of spatially fluctuating selection as the emergence consequence of:

$$ p_{t+1} = p_{t} + \Delta_{ds}p +  \Delta_{m}p $$

**Where $\Delta_{ds}p$ is the change in allele frequency due to directional selection (continent on habitat “c”), $\Delta_{m}p$ is the change in allele frequency due to migration.**

At first glance, the formula might seem straightforward—a standard representation of selection and migration. However, the real nuance emerges in its spatial context. Consider a scenario involving two islands with migration: on one island, allele xii is favored, while on the other, it is disfavored, and the reverse holds true for allele xjj. In this case, we observe a fascinating dynamic which can maintain genetic variation!

Consider the following ecology:
![enter image description here](https://github.com/Jcbnunez/uvm-popgen/blob/main/Figures/Spatial_selection_model.png?raw=true)

## Step 1: Our friend is back!! 😀
We will first code a formula for selection 
```{r}
general_selection = function( p, sii, sij, sjj ){

q=1-p
num=(p^2)*(1-sii) + p*q*(1-sij)
dem=(p^2)*(1-sii) + 2*p*q*(1-sij) + (q^2)*(1-sjj)
p_t1 = num/dem
return(p_t1)
}
```

## Step 2: Create separate simulations for both habitats, as if they were independent

### <<QUESTION 2: Complete the following code>>


```{r}
p=0.5
site_information = list()

#lets simulate 100 generations

for(c in 1:2){
p_recur=c()
	for(g in 1:100){

		if(g==1){

		p_recur[g]=p
		} else{
		#local selection
		if(c==1){
		print("hab. c1")
		#Q2.1
		sii=<<?>>; sij=<<?>>; sjj=<<?>>
		}else if(c==2){
		print("hab. c2")
		#Q2.2
		sii=<<?>>; sij=<<?>>; sjj=<<?>>
		}

		local_component = general_selection(p_recur[g-1], sii, sij, sjj)
		local_delta = local_component - p_recur[g-1]

		p_recur[g] = p_recur[g-1] +
		local_delta

		} # close if-else
	} # close g loop

site_information[[c]] = data.frame(g=1:100, p_recur, c=rep(c, 100) )
} # close c loop
joint_habs = do.call("rbind", site_information)
```
## Step 3: plot using the following code
```{r}
plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
c_bins = unique(joint_habs$c); cl <- rainbow(length(c_bins))

for(i in 1:length(c_bins)){
	message(i)
	dat_plot = joint_habs[which(joint_habs$c == c_bins[i]),]
	lines(dat_plot$g, dat_plot$p_recur,col = cl[i],type = 'l' ) 
}
```
  
### <<QUESTION 3: Print this graph in your report and describe what is happening>>

## Step 4: Add gene flow to the mix! … first lets create a migration function

### <<QUESTION 4: Complete the following code >>

```{r}
migration_rate = function( m, p1, p2 ){
	#Q4.1
	q=<<?>>
	p_t1 = m*(p1-p2)
	#Q4.2
	return(<<?>>)
}
```

## Step 5: Now we will create a simulation that incorporates both selection and migration. Yet, we must incorporate an aspect of nuance here. For migration to work, must simulate both populations as “co-occurring” (i.e., not independent). This will require some coding tricks!

### <<QUESTION 5: Fix the following code >>

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
	p1_component = general_selection(p1_recur[g-1], 
	## Q5.1
	sii=<<?>>; sij=<<?>>; sjj=<<?>>
	)
	p2_component = general_selection(p2_recur[g-1], 
	## Q5.2
	sii=<<?>>; sij=<<?>>; sjj=<<?>>
	)

	p1_delta = p1_component - p1_recur[g-1]
	p2_delta = p2_component - p2_recur[g-1]

	#Now we will encode the migration effect
	p1_mig_delta = migration_rate( m, p2_recur[g-1], p1_recur[g-1] )
	p2_mig_delta = migration_rate( m, p1_recur[g-1], p2_recur[g-1] )

	p1_recur[g] = p1_recur[g-1] + p1_delta + p1_mig_delta
	## Q5.3
	<<?>>[g] = p2_recur[g-1] + p2_delta + p2_mig_delta

	} # close if-else
} # close g loop

sel_mig = data.frame(g=1:100, p1_recur, p2_recur)

plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
cl <- rainbow(2)
## Q5.4
lines(<<?>>$g, <<?>>$p1_recur,col = cl[1],type = 'l' )
## Q5.5
lines(<<?>>$g, <<?>>$p2_recur,col = cl[2],type = 'l' )
```

### <<QUESTION 6: Print this graph in your report and describe what is happening. What is the long term evolutionary fate of these alleles? >>

### <<QUESTION 7: In class we derived the fitness of a spatially balanced heterozygote using a niche specific model, as opposed to a migration-selection balance model. What are the differences between these two approaches? >>
