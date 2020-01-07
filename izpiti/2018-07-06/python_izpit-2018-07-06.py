from functools import lru_cache
# 3. naloga
# Dr. Ana Kek je matematiˇcarka, ki se navdušuje nad simetrijo. Po vrnitvi z izleta v Hajjah v Jemnuje zaˇcela iskati simetrije vˇcrtastihˇcrno-belih zapestnicah, ki jih je kupila na potovanju.
# Zato si želi razdeliti zaporedje črnih in belih črt na zapestnicah na simetriˇcne dele.  
# Njen cilj je  poiskati  delitev  z  najmanjšim  številom  delov.   Da  si  delo  olajša,  se  je  odloˇcila  zaporedjeˇcrtpredstaviti z nizom niˇcel in enic.

# a) Napišite funkcijo simetricen, ki preveri ali je nek del simetriˇcen, torej palindrom.
# Primer: >>> simetricen("01010") -> True
def simetricen(zapestnica):
    return zapestnica == zapestnica[::-1]



 
# b)Napišite funkcijo stevilo_delov, ki izraˇcuna na najmanj koliko delov moramo razdeliti zaporedje, da so vsi deli simetriˇcni.
# Primer:>>> stevilo_delov("00101011") -> 3
def stevilo_delov(zapestnica):
    dolzina = len(zapestnica)

    @lru_cache(maxsize=None)
    def preveri(zac, kon):
        if kon >= dolzina or zac > kon:
            return 







# c)Napišite funkcijorazdeli, ki vrne delitev, kjer zaporedje razdelimo na najmanjše možno šte-vilo simetriˇcnih delov.ˇCe je takšnih delitev veˇc, naj funkcija vrne poljubno izmed njih.# Primer:>>> razdeli("00101011")["0", "01010", "11"]d)Poleg simetriˇcnih pa se dr. Ana Kek zanima tudi zavsotno-simetriˇcnedele.  To so tisti deliDdolžinen, pri katerih je vsota prvihbn/2cštevk enaka vsoti preostalih števk.  Napišite funkcijovsotno_simetricen, ki preveri ali je del vsotno-simetriˇcen.# Primer:>>> vsotno_simetricen("01001000")True>>> vsotno_simetricen("1011")FalseNamig: iz niza števkblahko v Pythonu naredite seznam števil[int(c) for c in b], v OCa-mlu isto dosežete tako, da naložite modul “Str” z ukazom#load "str.cma" ;;in uporabiteList.map int_of_string (Str.split (Str.regexp "") b)e)Za primer,  da se bo dr. Ana Kek kdaj zaˇcela zanimati tudi za druge vrste simetrij,  funkcijistevilo_delovinrazdelinapišite tako, da za drugi argument spremejo funkcijo, ki preverja alije nek del simetriˇcen.# Primer:>>> razdeli("00101011", simetricen)["0", "01010", "11"]>>> razdeli("00101011", vsotno_simetricen)["00", "101011"]>>> razdeli("00101011", simetricen)["0", "01010", "11"]