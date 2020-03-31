
# Make a contingency table.
# x     m - x    m
# k-x   n-(k-x)  n

a = de genes
b = gene terms
c = all genes

DE & IN | !DE & IN # union(a,b), 
DE ! IN | !DE ! IN # len(a) - union(a,b),  union(N-len(a),



N = 30
a = c(sample(LETTERS,15)) # DE genes.
b = c(sample(LETTERS,15)) # TE genes.
x = length(intersect(a,b))

#m = length(intersect(a,b))
m = 12
n = N - m
#k = length(a)
k = 15
#x = c(0:15)

matrix(c(x,m-x,k-x,n-(k-x),nrow=2,ncol=2))



# Initialize variables
m <- 15       # Genes IN GO term
n <- 15       # Genes NOT IN GO term
k <- 15       # Gene hits, that is, differentially expressed
x <- c(0:15)  # Genes both IN GO term and differentially expressed 'hits'
p <- dhyper(x, m, n, k, log = FALSE)
sum(p)
pvalue <- sum(p[13:16]) # 12 or more genes, one sided

matrix(c(x,m-x,k-x,n-(k-x),nrow=2,ncol=2)

matrix(c(12,3,3,12),ncol=2,nrow=2,
       dimnames = list(TranscriptionElongation = c("T","F"),
		       DiffExpr = c("T","F")))

# Result is the same!
dm <- matrix(c(12, 3, 3, 12),nrow = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
		       Truth = c("Milk", "Tea")))

fisher.test(dm, alternative = "greater")
