# Recitation 4: Balancing Selection

This workshop will guide participants through using R to model balancing selection, a form of natural selection that maintains genetic diversity in a population. Examples of balancing selection include heterozygote advantage, frequency-dependent selection, and environmental variation. We’ll focus on the heterozygote advantage, where the heterozygous genotype (xij) has a higher fitness than either homozygote (xii or xjj). For the purposes of this workshop we will use the “long form” version of the equations for natural selection. In the most general sense; selection, after one generation of random mating, can be modeled as

$pt+1 =\frac{p^2 (1-sii) + pq(1-sij) }{p2 (1-sii ) + 2pq(1-sij)+q2 (1-sjj)}$

## Step 1: 
create a function based on our mathematical model
### Initial allele frequency

```{r}
general_selection = function( p, sii, sij, sjj ){

q=1-p

num=(p^2)*(1-sii) + p*q*(1-sij)
dem=(p^2)*(1-sii) + 2*p*q*(1-sij) + (q^2)*(1-sjj)

p_t1 = num/dem

return(p_t1)

}
```

## Step 2: 
Introduced recursion into the model.The code above allows us to explore a 'stepwise' change in allele frequency due to selection. However, evolution is inherently time-dependent, with populations continuously evolving. More importantly, this process doesn’t occur in a vacuum—populations evolve as a result of various factors that have influenced them over time. To account for this nuance, we must implement our code in a “recurrent-manner” where the input of generation “t+1” is influenced by what occurred in generation “t-1.”

  

Let’s explore these dynamics with a case for positive selection (sii=0, sij=0, sjj=0.2)
```{r}
#define an initial allele frequency
p=0.1
```
  

## create an empty variable for "recursive" use
```{r}
p_recur=c()

#lets simulate 100 generations

for(g in 1:100){

if(g==1){

sii=0; sij=0; sjj=0.2

p_recur[g]=p

} else{

p_recur[g] = general_selection(p_recur[g-1], sii, sij, sjj)

} # close if-else
} # close loop
```

### Step 3: Lets create a data frame to save our results

```{r}
positive_selection = data.frame(g=1:100, p_recur); p_recur=c()
```
## Step 4: Lets graph our results
```{r}
plot(positive_selection$g, positive_selection$p_recur, type ='l')

<<QUESTION 1: Print this graph in your report and describe what happens in this graph>>

Step 5: Lets scale up our results to other selection coefficients

First let’s create a file with all possible combination of “g” and “s” values (recall that g is generation), like so:

#Lets create an empty list to save our “nested loop”

s_and_p = list()

#Now we will run the loop

g=1:100

s=seq(from=0.05, to = 0.3, by = 0.05)

p= 0.1

k=1 ## We are going to learn a trick. We can easily navigate next loop by using “increasing counters (k)”.. Basically, every iteration of the loop k will get +1 added itself

for(s in s){

p_recur=c()

for(g in 1:100){

if(g==1){

p_recur[g]=p

} else{

sii=0; sij=0;

p_recur[g] = general_selection(p_recur[g-1], sii, sij, s)

} # close if-else

} # close g loop

s_and_p[[k]] = data.frame(g=1:100, s= s, p_recur);

k=k+1

p_recur=c()

} # close s loop
```
  

### The object “s_and_p” is a list containing the output of our nested loops… how can we merge it into a single data.frame using do.call()

```{r}
Positive_selection2 = do.call("rbind", s_and_p)
```
  
### Step 6: plot using the following code
```{r}
plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")

s_bins = unique(Positive_selection2$s )

cl <- rainbow(length(s_bins))

for(i in 1:length(s_bins)){

message(i)

dat_plot = Positive_selection2[which(Positive_selection2$s == s_bins[i]),]

lines(dat_plot$g, dat_plot$p_recur,col = cl[i],type = 'l' ) }

```
  

<<QUESTION 2: Print this graph in your report and describe what is happening. Describe the impact of “s” on the speed of selection. Label each line (i.e., each color) based on its selection coefficient>>

  

Step 7: Let’s now take a look at balancing selection. We are going to recycle the same code from steps 5 and 6. However, we are now going to introduce a twist! Instead of coding a case of positive selection, we want to create the conditions for balancing selection (Overdominance). For simplicity, let's assume that the selection against both homozygotes is symmetrical, that is sjj = sii. Yet, sij (the heterozygote) is maximized!

### Let’s create a new list

```{r}
s_and_p_BALSEL = list()

#Now we will run the loop

g=1:100

s=seq(from=0.05, to = 0.3, by = 0.05)

p= 0.1

k=1 ## We are going to learn a trick. We can easily navigate next loop by using “increasing counters (k)”.. Basically, every iteration of the loop k will get +1 added itself

for(s in s){

p_recur=c()

for(g in 1:100){

if(g==1){

p_recur[g]=p

} else{

#<<QUESTION 3: replace, X, Y, and Z for the correct variable or value!>>

sij=X;

sii=Y;

sjj=Z;

p_recur[g] = general_selection(p_recur[g-1], sii, sij, sjj)

} # close if-else

} # close g loop

s_and_p_BALSEL[[k]] = data.frame(g=1:100, s= s, p_recur);

k=k+1

p_recur=c()

} # close s loop
```
  

### The object “s_and_p” is a list containing the output of our nested loops… how can we merge it into a single data.frame using do.call()

```{r}
Balancing_Selection = do.call("rbind", s_and_p_BALSEL)

Step 6: plot using the following code

plot(NULL, xlim=c(1,100), ylim=c(0,1), ylab="Allele Freq", xlab="Generation")

s_bins = unique(Balancing_Selection$s); cl <- rainbow(length(s_bins))

  

for(i in 1:length(s_bins)){

message(i)

dat_plot = Balancing_Selection[which(Balancing_Selection$s == s_bins[i]),]

lines(dat_plot$g, dat_plot$p_recur,col = cl[i],type = 'l' ) }

```
  
<<QUESTION 4: Print this graph in your report and describe what is happening. How does the behaviour of this formulation differ from that of directional selection? Also, describe what is going on with the “red” graph>>

