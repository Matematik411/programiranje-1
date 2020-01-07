from functools import lru_cache

zabojniki = [1, 3, 4, 7, 10]

def nacini(zaboji, nosilnost):
    zaboji.sort(reverse=True)

    @lru_cache(maxsize=None)
    def nalozi(polozaj, preostane):
        
        if preostane < 0:
            return 0
        
        if preostane == 0:
            return 1

        moznosti = 0
        for x in range(polozaj, len(zaboji)):
            moznosti += nalozi(x, preostane - zaboji[x])

        return moznosti

    return nalozi(0, nosilnost) 


def nacini_vrne(zaboji, nosilnost):
    zaboji.sort(reverse=True)
    vsi = []

    @lru_cache(maxsize=None)
    def nalozi(polozaj, preostane, pot):
        if preostane < 0:
            return 0
        
        if preostane == 0:
            vsi.append(pot)
            return 1

        moznosti = 0
        for x in range(polozaj, len(zaboji)):
            moznosti += nalozi(x, preostane - zaboji[x], (zaboji[x],) + pot)

        return moznosti

    nalozi(0, nosilnost, ())
    return vsi

