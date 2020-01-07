from functools import lru_cache

testni_sadovnjak = [[2, 4, 1, 1],
                    [3, 2, 0, 5],
                    [8, 0, 7, 2]]

def obiranje(sadovnjak, vseh_potez):
    visina = len(sadovnjak)
    dolzina = len(sadovnjak[0])

    @lru_cache(maxsize=None)
    def macek(y, x, poteze):
        if y >= visina or x >= dolzina:
            return -1

        if poteze == 1 or (y, x) == (visina - 1, dolzina - 1):
            return sadovnjak[y][x]

    
        poteze -= 1
        return sadovnjak[y][x] + max(
            macek(y, x + 1, poteze),
            macek(y + 1, 0, poteze)
        )

    return macek(0, 0, vseh_potez)
