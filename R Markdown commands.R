---
  output: pdf_document
---
  # CS&SS/STAT 566 Class Lab 
  ## d-separation (discrete data)
  
  
  ### Our DAG:
  ### $B \rightarrow X \leftarrow C$; 
  ### $B \rightarrow Y \leftarrow C$.
  
  $B \sim Binomial (2, 0.5);$
  $Pr(B=0) = Pr(B=2) = 0.25, Pr(B=1) = 0.5$.
  
  $C \sim Binomial (2, 0.5);$
    $Pr(C=0) = Pr(C=2) = 0.25, Pr(C=1) = 0.5$.
    
    ```{r}
    # Number of simulations
    nsim <- 5000
    set.seed(5000)
    
    b <- rbinom(nsim,2,0.5)
    bf <- factor(b)
    
    c <- rbinom(nsim,2,0.5)
    cf <- factor(c)
    ```
    
    Values of $X$ for each value of $B$ and $C$ (similarly for $Y$).
    \begin{tabular}{ c c | c}
    \hline
    B & C & X \\
    \hline
    0 & 0 & 0 \\
    0 & 1 & $Bernoulli(0.5)$ \\
    1 & 0 & $Bernoulli(0.5)$ \\
    1 & 1 & $Bernoulli(0.5)$ + $Bernoulli(0.5)$ \\
    0 & 2 & 1 \\
    2 & 0 & 1 \\
    1 & 2 & 1 + $Bernoulli(0.5)$ \\
    2 & 1 & 1 + $Bernoulli(0.5)$ \\
    2 & 2 & 2\\
    \hline  
    \end{tabular}
    
    ```{r}
    x <- (0*(bf==0) + rbinom(nsim,1,0.5)*(bf==1) + 1*(bf==2)
          + 0*(cf==0) + rbinom(nsim,1,0.5)*(cf==1) + 1*(cf==2))
    xf <- factor(x)
    
    y <- (0*(bf==0) + rbinom(nsim,1,0.5)*(bf==1) + 1*(bf==2)
          + 0*(cf==0) + rbinom(nsim,1,0.5)*(cf==1) + 1*(cf==2))
    yf <- factor(y)
    
    name.list <- list(c(0,1,2),c(0,1,2))
    ```
    
    \clearpage
    
    ## Pairwise comparisons of the generated variables.
    ### Marginal: $B$ vs. $C$.
    ```{r}
    table(bf,cf,dnn=c("b","c"))
    
    # Divide by row totals
    round(table(bf,cf,dnn=c("b","c"))/matrix(rep(table(bf),3),nrow=3),2)
    # Divide by col totals
    round(table(bf,cf,dnn=c("b","c"))/matrix(rep(table(cf),3),nrow=3,byrow=TRUE),2)
    
    # Correlation
    round(cor(b,c),3)
    
    # Chi-squared test of independence
    chisq.test(table(bf,cf,dnn=c("b","c")))
    ```
    
    \clearpage
    
    ### Marginal: $X$ vs. $Y$.
    ```{r}
    table(xf,yf,dnn=c("x","y"))
    
    round(table(xf,yf,dnn=c("x","y"))/matrix(rep(table(xf),3),nrow=3),2)
    
    round(table(xf,yf,dnn=c("x","y"))/matrix(rep(table(yf),3),nrow=3,byrow=TRUE),2)
    
    # Correlation
    round(cor(x,y),3)
    
    # Chi-squared test of independence
    chisq.test(table(xf,yf,dnn=c("x","y")))
    ```
    
    \clearpage
    
    ### Conditional: $X$ vs. $Y$ given $B$.
    ```{r}
    table(xf[(bf==0)],yf[(bf==0)],dnn=c("x","y"))
    table(xf[(bf==1)],yf[(bf==1)],dnn=c("x","y"))
    table(xf[(bf==2)],yf[(bf==2)],dnn=c("x","y"))
    
    names(name.list) <- c("x","y")
    
    ## Dividing by row totals
    round(matrix(table(xf[(bf==0)],yf[(bf==0)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(xf[bf==0]),3),nrow=3),2)
    round(matrix(table(xf[(bf==1)],yf[(bf==1)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(xf[bf==1]),3),nrow=3),2)
    round(matrix(table(xf[(bf==2)],yf[(bf==2)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(xf[bf==2]),3),nrow=3),2)
    
    ## Dividing by col totals
    round(matrix(table(xf[(bf==0)],yf[(bf==0)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(yf[bf==0]),3),nrow=3,byrow=TRUE),2)
    round(matrix(table(xf[(bf==1)],yf[(bf==1)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(yf[bf==1]),3),nrow=3,byrow=TRUE),2)
    round(matrix(table(xf[(bf==2)],yf[(bf==2)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(yf[bf==2]),3),nrow=3,byrow=TRUE),2)
    
    # Correlations
    round(cor(x[(bf==0)],y[(bf==0)]),3)
    round(cor(x[(bf==1)],y[(bf==1)]),3)
    round(cor(x[(bf==2)],y[(bf==2)]),3)
    
    # Chi-squared test of independence
    chisq.test(table(xf[(bf==1)],yf[(bf==1)],dnn=c("x","y")))
    ```
    
    \clearpage
    
    ### Conditional: $B$ vs. $C$ given $X$.
    ```{r}
    table(bf[(xf==0)],cf[(xf==0)],dnn=c("b","c"))
    table(bf[(xf==1)],cf[(xf==1)],dnn=c("b","c"))
    table(bf[(xf==2)],cf[(xf==2)],dnn=c("b","c"))
    
    names(name.list) <- c("b","c")
    
    ## Dividing by row totals
    round(matrix(table(bf[(xf==0)],cf[(xf==0)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(bf[xf==0]),3),nrow=3),2)
    round(matrix(table(bf[(xf==1)],cf[(xf==1)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(bf[xf==1]),3),nrow=3),2)
    round(matrix(table(bf[(xf==2)],cf[(xf==2)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(bf[xf==2]),3),nrow=3),2)
    
    ## Dividing by col totals
    round(matrix(table(bf[(xf==0)],cf[(xf==0)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(cf[xf==0]),3),nrow=3,byrow=TRUE),2)
    round(matrix(table(bf[(xf==1)],cf[(xf==1)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(cf[xf==1]),3),nrow=3,byrow=TRUE),2)
    round(matrix(table(bf[(xf==2)],cf[(xf==2)]),nrow=3,dimnames=name.list)/
            matrix(rep(table(cf[xf==2]),3),nrow=3,byrow=TRUE),2)
    
    # Correlations
    round(cor(b[(xf==0)],c[(xf==0)]),3)
    round(cor(b[(xf==1)],c[(xf==1)]),3)
    round(cor(b[(xf==2)],c[(xf==2)]),3)
    ```
    
    \clearpage
    
    ### Conditional: $X$ vs. $Y$ given $B$ and $C$.
    ```{r}
    table(xf[(bf==0)&(cf==0)],yf[(bf==0)&(cf==0)],dnn=c("x","y"))
    
    for (i in 0:2){
      for (j in 0:2){
        cat(paste("  B =",i,";  C =",j,"\n"))
        tbl <- table(xf[(bf==i)&(cf==j)],yf[(bf==i)&(cf==j)],dnn=c("x","y"))
        print(tbl)
        cat("============\n")
      }
    }
    
    for (i in 0:2){
      for (j in 0:2){	
        cat(paste("  B =",i,";  C =",j," :", round(cor(x[(bf==i)&(cf==j)],y[(bf==i)&(cf==j)]),3),"\n"))
      }
    }
    
    
    ### Conditional 
    ## B vs. C given X, Y
    
    table(bf[(xf==0)&(yf==0)],cf[(xf==0)&(yf==0)],dnn=c("b","c"))
    
    for (i in 0:2){
      for (j in 0:2){
        cat(paste("  X =",i,";  Y =",j))
        tbl <- table(bf[(xf==i)&(yf==j)],cf[(xf==i)&(yf==j)],dnn=c("b","c"))
        print(tbl)
        cat("============\n")
      }
    }
    
    for (i in 0:2){
      for (j in 0:2){	
        cat(paste("  X =",i,";  Y =",j," :", round(cor(b[(xf==i)&(yf==j)],c[(xf==i)&(yf==j)]),3),"\n"))
      }
    }
    ```