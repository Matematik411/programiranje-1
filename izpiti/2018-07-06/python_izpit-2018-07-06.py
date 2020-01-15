from functools import lru_cache
# 3. naloga
# Dr. Ana Kek je matematicarka, ki se navdušuje nad simetrijo. Po vrnitvi z izleta v Hajjah v Jemnuje zacela iskati simetrije v crtastih crno-belih zapestnicah, ki jih je kupila na potovanju.
# Zato si želi razdeliti zaporedje črnih in belih črt na zapestnicah na simetricne dele.  
# Njen cilj je  poiskati  delitev  z  najmanjšim  številom  delov.   Da  si  delo  olajša,  se  je  odlocila  zaporedje crt predstaviti z nizom nicel in enic.

# a) Napišite funkcijo simetricen, ki preveri ali je nek del simetricen, torej palindrom.
# Primer: >>> simetricen("01010") -> True
def simetricen(zapestnica):
    return zapestnica == zapestnica[::-1]



 
# b)Napišite funkcijo stevilo_delov, ki izracuna na najmanj koliko delov moramo razdeliti zaporedje, da so vsi deli simetricni.
# Primer:>>> stevilo_delov("00101011") -> 3
def stevilo_delov_od_zadaj(zapestnica):
    dolzina = len(zapestnica)

    @lru_cache(maxsize=None)
    def preveri(zac):
        if zac >= dolzina:
            return 0
        elif zac == (dolzina - 1):
            return 1

        else:
            najmanj = 1 + preveri(zac + 1)
            
            for i in range(1, dolzina - zac):
                if simetricen(zapestnica[zac : zac + i + 1]):
                    najmanj = min(najmanj, 1 + preveri(zac + i + 1))

            return najmanj
    
    return preveri(0)



def stevilo_delov_og(zapestnica): # deli in vladaj

    @lru_cache(maxsize=None)
    def preveri(niz):
        if len(niz) == 0:
            return 0
        elif simetricen(niz):
            return 1

        else:
            mozni = []
            for i in range(1, len(niz)):
                mozni.append(preveri(niz[:i]) + preveri(niz[i:]))
            return min(mozni)
    
    return preveri(zapestnica)



# c)Napišite funkcijo razdeli, ki vrne delitev, kjer zaporedje razdelimo na najmanjše možno število simetricnih delov. Ce je takšnih delitev vec, naj funkcija vrne poljubno izmed njih.
# Primer:>>> razdeli("00101011") -> ["0", "01010", "11"] 

def razdeli_og(zapestnica):

    @lru_cache(maxsize=None)
    def preveri_delitev(niz):
        if len(niz) == 0:
            return (0, [""])
        if simetricen(niz):
            return (1, [niz])

        
        mozni = None
        
        for i in range(1, len(niz)):
            n_levo, deli_levo = preveri_delitev(niz[:i])
            n_desno, deli_desno = preveri_delitev(niz[i:])
            n, k = n_desno + n_levo, deli_desno + deli_levo

            if mozni is None:
                mozni = (n, k)
        
            if n < mozni[0]:
                mozni = (n, k)

        return mozni
    return preveri_delitev(zapestnica)[1]


# d)Poleg simetricnih pa se dr. Ana Kek zanima tudi za vsotno-simetricne dele.  To so tisti deli D dolžine n, pri katerih je vsota prvih |_ n/2 _| števk enaka vsoti preostalih števk.
# Napišite funkcijo vsotno_simetricen, ki preveri ali je del vsotno-simetricen.
# Primer:>>> vsotno_simetricen("01001000") -> True
# >>> vsotno_simetricen("1011") -> False

def vsotno_simetricen(niz):
    if len(niz) <= 1:
        return True
    
    stevke = [int(x) for x in niz]
    pol = len(stevke) // 2
    return sum(stevke[:pol]) == sum(stevke[pol:])


# e)Za primer,  da se bo dr. Ana Kek kdaj zacela zanimati tudi za druge vrste simetrij,  funkciji stevilo_delov in razdeli napišite tako, da za drugi argument spremejo funkcijo, ki preverja ali je nek del simetricen.
# Primer:>>> razdeli("00101011", simetricen) -> ["0", "01010", "11"]
# >>> razdeli("00101011", vsotno_simetricen) -> ["00", "101011"]
# >>> razdeli("00101011", simetricen) -> ["0", "01010", "11"]

def stevilo_delov(zapestnica, f):
    dolzina = len(zapestnica)

    @lru_cache(maxsize=None)
    def preveri(zac):
        if zac >= dolzina:
            return 0
        elif zac == (dolzina - 1):
            return 1

        else:
            najmanj = 1 + preveri(zac + 1)
            
            for i in range(1, dolzina - zac):
                if f(zapestnica[zac : zac + i + 1]):
                    najmanj = min(najmanj, 1 + preveri(zac + i + 1))

            return najmanj
    
    return preveri(0)



def razdeli(zapestnica, f):

    @lru_cache(maxsize=None)
    def preveri_delitev(niz):
        if len(niz) == 0:
            return (0, [""])
        if f(niz):
            return (1, [niz])

        
        mozni = None
        
        for i in range(1, len(niz)):
            n_levo, deli_levo = preveri_delitev(niz[:i])
            n_desno, deli_desno = preveri_delitev(niz[i:])
            n, k = n_desno + n_levo, deli_desno + deli_levo

            if mozni is None:
                mozni = (n, k)
        
            if n < mozni[0]:
                mozni = (n, k)

        return mozni
    return preveri_delitev(zapestnica)[1]

