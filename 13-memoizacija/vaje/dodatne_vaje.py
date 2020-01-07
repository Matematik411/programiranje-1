from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################
testni = [100, 3, 2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]
test = [11, 11, 11, 11, 11, 11, 2, 2, 2, 2, 2]


def najdaljse_narascajoce_podzaporedje(sez):
    d = len(sez)
    
    @lru_cache(maxsize=None)
    def zgradi(i, value):
        if i >= d:
            return []
        elif sez[i] < value:
            return zgradi(i+1, value)
        else:
            sprejmemo = [sez[i]] + zgradi(i+1, sez[i])
            zavrnemo = zgradi(i+1, value)
            if len(sprejmemo) > len(zavrnemo):
                return sprejmemo
            else:
                return zavrnemo

    return zgradi(0, min(sez))


def naj(sez):
    return najdaljse_narascajoce_podzaporedje(sez)

###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


# smeri gor | desno | dol | levo
# smeri 0     1       2     3

def pobeg(soba, pozicija, koraki):
    visina = len(soba)
    sirina = len(soba[0])



    @lru_cache(maxsize=None)
    def robotek(pozicija, koraki):
        y, x = pozicija
        if not (0 <= y <= visina - 1) or not (0 <= x <= sirina - 1):
            return 0

        if soba[y][x] == 1:
            return 1
        
        if koraki < 1:
            return 0

        if soba[y][x] != 0:
            return 0
        resen = 0
        resen = max(resen, robotek((y+1, x), koraki - 1), robotek((y, x+1), koraki - 1),robotek((y-1, x), koraki - 1), robotek((y, x-1), koraki - 1))
        return resen

    return robotek(pozicija, koraki)
        
def pobeg2(soba, pozicija, koraki):
    visina = len(soba)
    sirina = len(soba[0])



    @lru_cache(maxsize=None)
    def robotek2(pozicija, koraki, smer):
        y, x = pozicija
        if not (0 <= y <= visina - 1) or not (0 <= x <= sirina - 1):
            return 0

        if soba[y][x] == 1:
            return 1
        
        if koraki < 1:
            return 0

        if soba[y][x] != 0:
            return 0
        resen = 0
        if smer != 0:
            resen = max(resen, robotek2((y-1, x), koraki - 1, 2))
        if smer != 1:
            resen = max(resen, robotek2((y, x-1), koraki - 1, 3))
        if smer != 2:
            resen = max(resen, robotek2((y+1, x), koraki - 1, 0))
        if smer != 3:
            resen = max(resen, robotek2((y, x+1), koraki - 1, 1))

        return resen

    return robotek2(pozicija, koraki, 5)

print(pobeg(soba, (5, 1), 50))
print(pobeg2(soba, (5, 1), 50))