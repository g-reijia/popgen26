### Recitation written by Dr. Joaquin Nunez and were accessed from: https://github.com/Jcbnunez/uvm-popgen/tree/main/Recitations for PopGen SP26

# Recitation 7: Linked selection

Linked selection refers to the process where natural selection acting on one genetic variant influences neighboring variants due to their physical proximity on a chromosome. This occurs because linked variants tend to be inherited together, allowing positive selection (e.g., on beneficial mutations) or negative selection (e.g., against deleterious mutations) to affect nearby neutral or mildly deleterious variants. For this recitation we are going to simulate a case of linked selection.

### Step 1: Visualize the architecture of the system
**![](https://lh7-rt.googleusercontent.com/docsz/AD_4nXcJ6p1u0mdzHBlOgblqpQfNdFpVf38UhTpvIKqwyq-yok4usuvzY7NZA4vAvefS1An0fTg67f00byRCNfEJkDbzooulEL9vs4K8ucRQX_mI-oGVKLKSNf0OLrS4ZAYcOgdeE0Ru?key=8Ib7_2ci9AIsJUyCzIoqYtxA)**

As discussed in lectures, we will simulate two linked loci with a recombination rate r and varying levels of linkage disequilibrium (D). In this scenario, gene ⍺ is under selection, while gene β is neutral. The core question we aim to address is: how does selection at one locus influence the frequency of a nearby, non-adaptive locus? We will imagine that allele ⍺i starts at 0.5 and allele βi starts at 0.2.

### Step 2: Let’s create code to simulate the process of natural selection at a “selected” and a “neutral-linked” locus.

<<QUESTION 1: Complete the code chunks below. For the formula of selection, we will code the “rate” formula as opposed to the general one. You must write the code in terms of the parameters given $p$, $h$, and $s$>>

```{r}
selecion_rate = function( p, h, s ){
q=?
num=?
dem=?
p_t1=?
return(p_t1)
}
```

<<QUESTION 2: Complete the code chunks below. For the formula of linked selection, we will code the “rate” of change due to linkage to an adaptive allele. You must write the code in terms of the parameters given  $D$,  $p$, $h$, and $s$>>

```{r}
linked_rate = function( p, h, s, D ){
q=?
num=?
dem=?
p_t1=?
return(p_t1)
}
```

### Step 3: One important caveat about linkage disequilibrium is that it depends on both allele frequencies and the recombination rate among loci. Thus, as your simulations “evolve” the allele frequencies, the value of D must also change to match the system the values would change as a function of

$$D = -rD$$

```{r}
rate_of_D = function( r, D ){
delta_d = -r*D
return(delta_d)
}
```

### Step 4: We will deploy the code. We will assume that h = 0.5 and s = 0.1, D = 0.1 and recombination rate r = of $1x10^-8$ 

```{r}
#define an initial allele frequency, and the empty variable for "recursive" use
pa=0.5 # adaptive allele
pb=0.2 # linked allele

r=1e-8
D=0.1
h=0.5
s=0.05

#and the empty variables for "recursive" use
pa_recur=c()
pb_recur=c()
D_recur=c()

#lets simulate 100 generations
for(g in 1:100){
if(g==1){
pa_recur[g]=pa
pb_recur[g]=pb
D_recur[g]=D
} else{


# Evaluate selection
pa_delta = selecion_rate(pa_recur[g-1], h, s)
pa_recur[g] = pa_recur[g-1] + pa_delta

# Evaluate linkage
D_delta = rate_of_D(D_recur[g-1], r)
D_recur[g] = D_recur[g-1] + D_delta

# Now we will deal with the linked loci
pb_delta = linked_rate(pb_recur[g-1], h, s, D_recur[g])
pb_recur[g] = pb_recur[g-1] + pb_delta

} # close if-else
} # close loop

#create a dataframe to save your results
selection_linked = data.frame(g=1:100, pa_recur, pb_recur, D_recur)
```

### Step 5: Let’s plot our findings

  
#### Let's create some plots

```{r}
plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")
lines(selection_linked$g,
selection_linked$pa_recur,col = "red",type = 'l')
lines(selection_linked$g,
selection_linked$pb_recur,col = "blue",type = 'l')
```
  
  <<QUESTION 3: Print this graph in your report and describe what is happening. How are the alleles changing in relationship to each other?>>

<<QUESTION 4: If you were doing a genomic analysis seeking to identify loci under selection, and you were to observe the trajectories of both the ⍺ and the β genes, could you tell which of the two is the “true” adaptive target? Explain your rationale>>

<<QUESTION 5: As we mentioned in the lecture, a more common metric use to measure linkage is r2, defined as:


$$r^2 = {{D^2}\over{p_{⍺}q_{⍺}p_{β}q_{β}}}$$

Create a function that uses the data from your simulation (i.e., object: selection_linked) and calculates the $r^2$ over time. You do not need to put the code in the report, just a graph showing $r^2$ in the y-axis and generation in the x-axis. Describe your observations, what is the impact of selection on linkage?.>>
