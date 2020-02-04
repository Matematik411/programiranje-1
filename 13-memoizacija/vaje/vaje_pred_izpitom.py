from functools import lru_cache
# 1
# Maximum sum of pairs with specific difference
# Given an array of integers and a number k. We can pair two number of array if difference between them is strictly less than k. The task is to find maximum possible sum of disjoint pairs. Sum of P pairs is sum of all 2P numbers of pairs.

arr = [3, 5, 10, 15, 17, 12, 9]
k = 4

def vsota_parov(list, k):
    list.sort()

    @lru_cache(maxsize=None)
    def do_tja(i):
        # print(i)
        if i < 1:
            return 0
        else:
            spusti = do_tja(i-1)
            vzame = 0
            if abs(list[i] - list[i-1]) < k:
                vzame = list[i] + list[i-1] + do_tja(i-2)

            return max(vzame, spusti)
    
    return do_tja(len(list) - 1)

print(vsota_parov(arr, k))


# 2
# Minimum cost to fill given weight in a bag
# You are given a bag of size W kg and you are provided costs of packets different weights of oranges in array cost[] where cost[i] is basically cost of ‘i’ kg packet of oranges. Where cost[i] = -1 means that ‘i’ kg packet of orange is unavailable

# Find the minimum total cost to buy exactly W kg oranges and if it is not possible to buy exactly W kg oranges then print -1. It may be assumed that there is infinite supply of all available packet types.

W_1 = 5
cost_1 = [20, 10, 4, 50, 100]
W_2 = 5
cost_2 = [-1, -1, 4, 5, -1]

def vrecka(masa, cene):

    @lru_cache(maxsize=None)
    def koliko(ostalo):
        # print(ostalo)
        if ostalo == 0:
            return 0
        
        najceneje = -1
        for i in range(ostalo):
            if cene[i] != -1:
                preostanek = koliko(ostalo - (i + 1))
                if preostanek != -1:
                    if najceneje == -1:
                        najceneje = cene[i] + preostanek
                    else:
                        najceneje = min(cene[i] + preostanek, najceneje)
        
        return najceneje
    
    return koliko(masa)
print(vrecka(W_1, cost_1))
print(vrecka(W_2, cost_2))
        

# 3
# Probability of reaching a point with 2 or 3 steps at a time
# A person starts walking from position X = 0, find the probability to reach exactly on X = N if she can only take either 2 steps or 3 steps. Probability for step length 2 is given i.e. P, probability for step length 3 is 1 – P.

N = 5
P = 0.20

def verjetnost(n, p):

    @lru_cache(maxsize=None)
    def pot(ostalo):
        if ostalo == 0:
            return 1

        else:
            skupaj = 0
            if ostalo >= 2:
                skupaj += p * pot(ostalo - 2)
            if ostalo >= 3:
                skupaj += (1-p) * pot(ostalo - 3)
            
            return skupaj
    return int(10000 * pot(n)) / 10000

print(verjetnost(N, P))

# -------------------------------------------------------------------------------------------------
# 4
# Temple Offerings
# Consider a devotee wishing to give offerings to temples along a mountain range. The temples are located in a row at different heights. Each temple should receive at least one offering. If two adjacent temples are at different altitudes, then the temple that is higher up should receive more offerings than the one that is lower down. If two adjacent temples are at the same height, then their offerings relative to each other does not matter. Given the number of temples and the heights of the temples in order, find the minimum number of offerings to bring.

nr = 3
temples_ex = [1,2,2]

def darovanja(n, templji):
    darovi = [0] * n

    def darilo(i):
        if i == 0:
            return 1
        
        if templji[i-1] >= templji[i]:

            k = 1
            if darovi[i-k] == 1:
                while True:
                    darovi[i-k] += 1
                    k += 1
                    if i-k >= 0:
                        if templji[i-k] <= templji[i-k+1]:
                            break
                    else:
                        break
            
            return 1

        else:
            return darovi[i-1] + 1

    for j in range(n):
        vrednost = darilo(j)
        darovi[j] = vrednost

    return sum(darovi)    

print(darovanja(nr, temples_ex))

# 5
# Weighted Job Scheduling
# Given N jobs where every job is represented by following three elements of it.

# Start Time
# Finish Time
# Profit or Value Associated (>= 0)
# Find the maximum profit subset of jobs such that no two jobs in the subset overlap.

nr = 4
jobs = [[1, 2, 50], [3, 5, 20], [2, 100, 200], [6, 19, 100]]

def sluzbe(n, dela):
    dela.sort(key = lambda x: x[1])
    
    @lru_cache(maxsize=None)
    def ura(h):
        if h <= 1:
            return 0
        
        
        optimalno = ura(h-1)
        for mozni in dela:
            if mozni[1] == h:
                delamo = mozni[2] + ura(mozni[0])
                optimalno = max(optimalno, delamo)

        return optimalno

    return ura(dela[-1][1])

print(sluzbe(nr, jobs))

            
