def fakulteta(n):
    if n <= 1:
        return 1
    else:
        n * fakulteta(n - 1)