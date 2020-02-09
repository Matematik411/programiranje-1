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

            
# 6 
# Count all increasing subsequences
# We are given an array of digits (values lie in range from 0 to 9). The task is to count all the sub sequences possible in array such that in each subsequence every digit is greater than its previous digits in the subsequence.

example_array = [3,2,4,5,4]

def nar_podzaporedje(seznam):
    d = len(seznam)

    @lru_cache(maxsize=None)
    def se_konča_na(i):
        if i == 0:
            return 1
        
        skupno = 1 # samo ta

        for j in range(i):
            if seznam[j] < seznam[i]: # dodamo tiste ki so se končali na manjši vrednosti
                skupno += se_konča_na(j)
        
        
        return skupno

    return sum([se_konča_na(k) for k in range(d)])

def nar_podzaporedje_vemo_elemente(seznam):
    # ker vemo da imamo le elemente od 0 do 9, zgradimo pomožni seznam, ki shrani koliko seznamov se je DO ZDAJ končalo na to številko

    konec = [0] * 10

    def mesto(i):
        skupno = 1
        for j in range(seznam[i]):
            skupno += konec[j]
        
        return skupno

    for k in range(len(seznam)):
        konec[seznam[k]] += mesto(k)
    
    return sum(konec)
        
print(nar_podzaporedje(example_array))
print(nar_podzaporedje_vemo_elemente(example_array))


# 7
# Maximum Product Cutting | DP-36
# Given a rope of length n meters, cut the rope in different parts of integer lengths in a way that maximizes product of lengths of all parts. You must make at least one cut. Assume that the length of rope is more than 2 meters.

rope = 10

def rezanje_vrvi(dolzina): # dolzina >= 2

    @lru_cache(maxsize=None)
    def primer(d):
        najboljse = d-1 #samo odrezemo enko stran

        for i in range(2, d-1):
            najboljse = max(najboljse, i * primer(d-i)) # prej razrezano
            najboljse = max(najboljse, i * (d-i)) # prej celo

        return najboljse
    
    return primer(dolzina)

print(rezanje_vrvi(rope))


# 8
# Maximum average sum partition of an array
# Given an array, we partition a row of numbers A into at most K adjacent (non-empty) groups, then the score is the sum of the average of each group. What is the maximum score that can be scored?

A = [1,2,3,4,5,6,7]
K = 4
def average(list):
    return round(sum(list) / len(list),2)

def vsota_povprecij_delov(seznam,st_delov):

    @lru_cache(maxsize=None)
    def prvih(i, k):
        if i < k-1 or i < 0:
            return 0
        elif i == k-1:
            return sum(seznam[:i+1])
        elif k == 1:
            return average(seznam[:i+1])

        optimalno = prvih(i-1, k-1) + seznam[i] # samo naredimo nov del

        for j in range(i-1): # združimo z prejšnjo
            optimalno = max(optimalno, prvih(j, k-1) + average(seznam[j+1:i+1]))
        
        return optimalno
    
    return prvih(len(seznam) - 1, st_delov)
        
print(vsota_povprecij_delov(A, K))


# 9 
# Minimum jumps to reach last building in a matrix
# Given a matrix containing an integer value, In which each cell of the matrix represents height of building. Find minimum jumps needed reach from First building (0, 0) to last (n-1, m-1). Jump from a cell to next cell is absolute difference between two building heights.

stolpnice = [[ 5, 4, 2 ],
             [ 9, 2, 1 ],
             [ 2, 5, 9 ],
             [ 1, 3, 11]] 

def skakac(mreza):
    visina = len(mreza)
    sirina = len(mreza[0])

    @lru_cache(maxsize=None)
    def stevilo_skokov(y, x):
        if y > 0:
            if x > 0:
                return min(
                    stevilo_skokov(y-1, x) + abs(mreza[y][x] - mreza[y-1][x]), # pride od zgoraj
                    stevilo_skokov(y, x-1) + abs(mreza[y][x] - mreza[y][x-1]), # pride iz leve
                    stevilo_skokov(y-1, x-1) + abs(mreza[y][x] - mreza[y-1][x-1]), # skoci po diagonali
                )
            else:
                return stevilo_skokov(y-1, x) + abs(mreza[y][x] - mreza[y-1][x]) # pride od zgoraj
        else:
            if x > 0:
                return stevilo_skokov(y, x-1) + abs(mreza[y][x] - mreza[y][x-1]) # pride iz leve
            else:
                return 0

    return stevilo_skokov(visina - 1, sirina - 1)

print(skakac(stolpnice))

# 10
# Minimum Initial Points to Reach Destination
# Given a grid with each cell consisting of positive, negative or no points i.e, zero points. We can move across a cell only if we have positive points ( > 0 ). Whenever we pass through a cell, points in that cell are added to our overall points. We need to find minimum initial points to reach cell (m-1, n-1) from (0, 0).


point_grid = [ [-2, -3,   3], 
          [-5, -10,  1], 
          [10,  30, -5] ]

R = 3
C = 3
  
def minInitialPoints(points): 
    ''' 
    dp[i][j] represents the minimum initial 
    points player should have so that when  
    starts with cell(i, j) successfully 
    reaches the destination cell(m-1, n-1) 
    '''
    dp = [[0 for x in range(C + 1)]  
             for y in range(R + 1)] 
    m, n = R, C 
      
    if points[m - 1][n - 1] > 0: 
        dp[m - 1][n - 1] = 1
    else: 
        dp[m - 1][n - 1] = abs(points[m - 1][n - 1]) + 1
    ''' 
    Fill last row and last column as base 
    to fill entire table 
    '''
    for i in range (m - 2, -1, -1): 
        dp[i][n - 1] = max(dp[i + 1][n - 1] -
                           points[i][n - 1], 1) 
    for i in range (2, -1, -1): 
        dp[m - 1][i] = max(dp[m - 1][i + 1] -
                           points[m - 1][i], 1) 
    ''' 
    fill the table in bottom-up fashion 
    '''
    for i in range(m - 2, -1, -1): 
        for j in range(n - 2, -1, -1): 
            min_points_on_exit = min(dp[i + 1][j], 
                                     dp[i][j + 1]) 
            dp[i][j] = max(min_points_on_exit -
                               points[i][j], 1) 

    print(dp)          
    return dp[0][0]  

print(minInitialPoints(point_grid))

# 11
# Find minimum adjustment cost of an array
# Given an array of positive integers, replace each element in the array such that the difference between adjacent elements in the array is less than or equal to a given target. We need to minimize the adjustment cost, that is the sum of differences between new and old values. We basically need to minimize &Sum;|A[i] – Anew[i]| where 0 ≤ i ≤ n-1, n is size of A[] and Anew[] is the array with adjacent difference less that or equal to target.

# Assume all elements of the array is less than constant M = 100.

array = [55, 77, 52, 61, 39, 6, 25, 60, 49, 47]
target = 10
M = 100



def poprava_seznama(seznam, tarca):
    n = len(seznam)

    # tabela za vrednosti 
    vrednosti = [[0 for _ in range(M + 1)]
                for _ in range(n)]
    
    # prvi
    for j in range(M + 1):
        vrednosti[0][j] = abs(j - seznam[0])

    # ostali
    for m in range(1, n):
        # poračuna minimalno skupno ceno, za primer, ko se seznam[m] sremeni na to vrednost
        for j in range(M + 1):
            cena = n * M  #nekaj kar ne moremo doseči

            for k in range(M + 1):
                if k >= j - tarca and k <= j + tarca: #katere vrednosti prejsnjega nas zanimajo
                    cena = min(cena,
                                vrednosti[m - 1][k] + abs(j - seznam[m]))
            
            vrednosti[m][j] = cena
    
    #vrniti zelimo minimum zadnje vrstice
    resitev = M * n
    for j in range(M + 1):
        resitev = min(resitev, vrednosti[n-1][j])
    
    return resitev

def poprava_seznama_s_funkcijo(seznam, tarca):
    n = len(seznam)

    @lru_cache(maxsize=None)
    def aux(m, j):
        if m == 0:
            return abs(j - seznam[m])
        
        cena = n * M  #nekaj kar ne moremo doseči

        for k in range(M + 1):
            if k >= j - tarca and k <= j + tarca: #katere vrednosti prejsnjega nas zanimajo
                cena = min(cena,
                            aux(m - 1,k) + abs(j - seznam[m]))
        
        return cena
    
    #vrniti zelimo minimum zadnje vrstice
    resitev = M * n
    for j in range(M + 1):
        resitev = min(resitev, aux(n-1,j))
    
    return resitev

print(poprava_seznama(array, target))
print(poprava_seznama_s_funkcijo(array, target))

# 12
# Longest palindromic subsequence

word = "GEEKSFORGEEKS"


def je_palindrom(niz):
    return niz == niz[::-1]

def podpalindrom(beseda):
    n = len(beseda)

    @lru_cache(maxsize=None)
    def polozaj(i, niz):
        if je_palindrom(niz + beseda[i]):
            optimalno = len(niz) + 1
            if i == n - 1:
                return optimalno
        else:
            optimalno = 0        
        
        if i == n - 1:
            return 1
            
        
        optimalno = max(optimalno, polozaj(i+1, ""))


        for k in range(i+1, n):
            optimalno = max(optimalno, polozaj(k, niz + beseda[i]))
            optimalno = max(optimalno, polozaj(k, niz))
        
        return optimalno

    return polozaj(0, "")

def vrne_podpalindrom(beseda):
    n = len(beseda)

    @lru_cache(maxsize=None)
    def polozaj(i, niz):
        if je_palindrom(niz + beseda[i]):
            optimalno = niz + beseda[i]
            if i == n - 1:
                return optimalno
        else:
            optimalno = ""   
        
        if i == n - 1:
            return beseda[-1]
            
        if len(polozaj(i+1, "")) > len(optimalno):
            optimalno = polozaj(i+1, "")


        for k in range(i+1, n):
            if len(polozaj(k, niz + beseda[i])) > len(optimalno):
                optimalno = polozaj(k, niz + beseda[i])
            if len(polozaj(k, niz)) > len(optimalno):
                optimalno = polozaj(k, niz)
        
        return optimalno

    return polozaj(0, "")

print(podpalindrom(word))
print(vrne_podpalindrom(word))

# 13 
# Knapsack's problem

val = [1,4,5,22,52]
wt =  [4,2,9,12,44]
W = 50

def knapsack(values, weights, w):
    n = len(values)

    @lru_cache(maxsize=None)
    def primer(i, masa): #grem po vrsti
        if i == n - 1:
            if masa  + weights[i] <= w:
                return values[i]
            else:
                return 0

        spustim = primer(i + 1, masa)
        if masa + weights[i] <= w:
            dodam = primer(i + 1, masa + weights[i]) + values[i]
        else:
            dodam = 0
        
        return max(spustim, dodam)
    
    return primer(0, 0)

print(knapsack(val, wt, W))
        
        