from functools import lru_cache

# Problem 85 - Counting rectangles
def naloga85(tarca, meja): # za 2 M dovolj že 100 rešitev (36,77)
    """Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution."""
    oddaljenost = tarca
    primer = (0,0)

    def mreza(x, y):
        return (x*(x+1)*y*(y+1))//4
    
    for a in range(2,meja):
        for b in range(2,meja):
            k = mreza(a, b)
            d = abs(tarca - k)
            if d < oddaljenost:
                oddaljenost = d
                primer = (a,b)

    return primer


# Highway Billboard Problem
M = 15
polozaji = [6, 9, 12, 14]
prihodki = [5, 6, 3, 7]
t = 2

dobicki = [0] * (M)

def cesta(i):
    if i <= 0:
        return 0
    if i in polozaji:
        spustimo = cesta(i-1)
        uporabimo = prihodki[polozaji.index(i)] + cesta(i-t-1)
        return max(spustimo, uporabimo)
    else:
        return cesta(i-1)    

for p in range(M):
    dobicki[p] = cesta(p)
print(dobicki[-1])


