from functools import lru_cache
# tretja naloga
def f(k, n):

    @lru_cache(maxsize=None)
    def preostanek(a, m): 
        # a je trenutna lokacija 
        # m označuje koliko korakov je še za tem

        # ko se ustavi prišteje 1, saj smo končali naše zaporedje, ker smo določali člen po člen, smo dobili enolična zaporedja
        if m == 0: 
            return 1
        
        # prešteje vsa možna nadaljevanja
        vseh = preostanek(a, m-1)
        for i in range(1, k+1):
            vseh += preostanek(a+i, m-1)
            if a-i >= 0:
                vseh += preostanek(a-i, m-1)
        
        return vseh
    
    return preostanek(0, n - 1)

print(f(3, 2))
print(f(1, 4))
print(f(3, 10))