# ta del ne dela
# def fac(n):
#     def aux(m, n):
#         if n <= 0:
#             return m
#         else:
#             return aux(m * n, n-1)
#     return aux(1, n)

# fac(3)

# def bin(n, k):

#     result = (fac(n)) / (fac(k) * fac(n-k))
#     return int(result)


# #  Število vseh možnosti lahko direktno izračunamo
# def postavitve_math(n, m, l):
#     k = (n - m*(l + 1) + 1)
#     if k >= 1:
#         return bin(k + m + 1, m+1)
#     else:
#         return 0


# #




# def postavitve_mesane_math(n, sez):
#     m = len(sez)
#     d = sum(sez)
#     k = (m+1) ** (n - d - (m -1))
#     if k >= 1:
#         return k
#     else:
#         return 0

# 3. naloga

from functools import lru_cache


def postavitve(n, m, l):

    @lru_cache(maxsize=None)
    def moznosti(i, k):
        if i < 0 or i >= n:
            return 0
        elif k == 0: # do tega pride le če m = 0
            return 0

        elif k == 1: # postavi na poljubno mesto
            return n - i  - (l - 1)

        mozni = 0 

        
        for j in range((n-1) - (k-1)*(l+1)):
            mozni += moznosti(i + j + l + 1 , k-1)

        return mozni

    return moznosti(0, m)

print(postavitve(9, 3, 2)) #4
print(postavitve(3, 1, 1)) #3





def postavitve_mesano(n, sez):
    l = len(sez)
    if l == 0:
        return 1

    @lru_cache(maxsize=None)
    def moznosti(i, k):
        if i < 0 or i >= n:
            return 0
            
        elif k == l-1: # postavi na poljubno mesto
            return n - i  - ((sez[k]) - 1)

        mozni = 0 

        
        for j in range((n-1) - sum(sez[k+1:]) - (l - k - 1)):
            mozni += moznosti(i + j + sez[k] + 1 , k+1)

        return mozni

    return moznosti(0, 0)