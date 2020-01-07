from functools import lru_cache

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]

def pot(mocvara):
    dolzina = len(mocvara)

    @lru_cache(maxsize=None)
    def zaba(polozaj, energija):
        if polozaj >= dolzina:
            return 0

        energija += mocvara[polozaj]
        return 1 + min(
            [zaba(polozaj + skok, energija - skok) 
            for skok in range(1, energija + 1)])


    return zaba(0, 0)


def pot_vrne(mocvara):
    dolzina = len(mocvara)

    @lru_cache(maxsize=None)
    def zaba(polozaj, energija):
        if polozaj >= dolzina:
            return (0,[])

        energija += mocvara[polozaj]

        najkrajsi = dolzina + 1
        potka = []
        for skok in range(1, energija + 1):
            poskus, do_zdaj = zaba(polozaj + skok, energija - skok)
            if (poskus + 1) <= najkrajsi:
                najkrajsi, potka = poskus + 1, [polozaj] + do_zdaj
            
        return najkrajsi, potka

    return zaba(0, 0)