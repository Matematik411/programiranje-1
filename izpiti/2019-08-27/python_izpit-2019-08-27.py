
from functools import lru_cache

aa =    [[(1, 10), (3, -10)],    # 0
        [(2, 10), (5, -20)],    # 1
        [(3, -10)],             # 2
        [(4, 15)],              # 3
        [(5, 0)]]               # 4

def pot(mesta):
    stevilo = len(mesta)

    @lru_cache(maxsize=None)
    def premik(polozaj, denar):

        if polozaj >= stevilo:
            if denar >= 0:
                return [polozaj]
            else:
                return False

        idealna = [i for i in range(stevilo + 1)]
        for moznost in mesta[polozaj]:
            dogodek = premik(moznost[0], denar + moznost[1])
            if dogodek:
                if (len(dogodek) + 1) < len(idealna):
                    idealna = [polozaj] + dogodek
        return idealna
    
    return premik(0, 0)
        
print(pot(aa))




        
        