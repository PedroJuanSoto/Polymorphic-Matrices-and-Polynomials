def prob_full_rank(q,n):
	x = 1
	for i in range(1,n+1):
		x = x * (1 - (1/(q**i)))
	return x
