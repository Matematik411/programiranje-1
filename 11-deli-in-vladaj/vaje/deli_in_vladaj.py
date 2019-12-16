###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
###############################################################################

def pivot(a, start, end):
    # BAD INPUT
    if start >= end:
        return start

    vecji = start + 1

    for i in range(start + 1, end + 1):
        if a[start] > a[i]:
            a[vecji], a[i] = a[i], a[vecji]
            vecji += 1

    
    a[start], a[vecji - 1] = a[vecji - 1], a[start]
    return vecji - 1



b = [10, 200, 5, 15, 11, 2, 17, 0, 18]
print(pivot(b, 1, 7))
print(b)   



###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
###############################################################################
def kth_element(a, k):
    spodnja_meja = 0
    zgornja_meja = len(a) - 1

    if k > zgornja_meja:
        return None

    while True:
        primer = pivot(a, spodnja_meja, zgornja_meja)

        if primer == k:
            return a[primer]
        elif primer < k:
            spodnja_meja = primer + 1
        else:
            zgornja_meja = primer - 1
    


def kth_element_w_recursion(a, k):

    def kth_el_part(a, k, start, end):
        if start > end:
            return None
        else:
            pivot_i = pivot(a, start, end)
            if pivot_i == k:
                return a[pivot_i]
            elif pivot_i > k:
                return kth_el_part(a, k, start, pivot_i - 1)
            else:
                return kth_el_part(a, k, pivot_i + 1, end)

    if k > len(a):
        return None
    else:
        return kth_el_part(a, k, 0, len(a)-1)


###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(a):

    def quicksort_part(lower, upper):
        if lower < upper:
            pivotni = pivot(a, lower, upper)
            quicksort_part(lower, pivotni - 1)
            quicksort_part(pivotni + 1, upper)


    quicksort_part(0, len(a) - 1)


import random
def test_quicksort():
    for _ in range(1000):
        a = [random.randint(-1000000, 1000000) for _ in range(1000)]
        b1 = a[:]
        b2 = a[:]
        quicksort(b1)
        b2.sort()
        if b1 != b2:
            return  f"Ne dela na {a}"



###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
# 
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
# 
# Sestavite funkcijo [zlij(target, begin, end, list_1, list_2)], ki v del 
# tabele [target] med start in end zlije tabeli [list_1] in [list_2]. V primeru, 
# da sta elementa v obeh tabelah enaka, naj bo prvi element iz prve tabele.
# 
# Primer:
#  
#     >>> list_1 = [1,3,5,7,10]
#     >>> list_2 = [1,2,3,4,5,6,7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> zlij(target, 0, len(target), list_1, list_2)
#     >>> target
#     [1,1,2,3,3,4,5,5,6,7,7,10]
#
###############################################################################
def zlij(target, begin, end, list_1, list_2): #MORDA POPRAVI UČINKOVITOST, KER TA SPREMINJA SEZNAME
    for i in range(begin, end):

        if list_1 == []:
            target[i] = list_2[0]
            del list_2[0]

        elif list_2 == []:
            target[i] = list_1[0]
            del list_1[0]


        elif list_1[0] < list_2[0]:
            target[i] = list_1[0]
            del list_1[0]


        else:
            target[i] = list_2[0]
            del list_2[0]

list_1 = [1,3,5,7,10]
list_2 = [1,2,3,4,5,6,7]
target = [-1 for _ in range(len(list_1) + len(list_2))]
zlij(target, 0, len(target), list_1, list_2)
print(target)   

###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). 
# Tabelo razdelimo na polovici, ju rekurzivno uredimo in nato zlijemo z uporabo
# funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja.
# Za razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je 
# potrebno narediti na mestu.
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
# >>> mergesort(a)
# [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################
def mergesort(a):
    d = len(a)
    if d <= 1:
        return a
    else:
        #zgradi polovici
        prvi = a[:d // 2]
        drugi = a[d // 2:]
        prvi = mergesort(prvi)
        drugi = mergesort(drugi)
        zlij(a, 0, d, prvi, drugi)
        return a

def test_mergesort():
    for _ in range(1000):
        a = [random.randint(-100000, 100000) for _ in range(100)]
        b1 = a[:]
        b2 = a[:]
        mergesort(b1)
        b2.sort()
        if b1 != b2:
            return  f"Ne dela na {a}"