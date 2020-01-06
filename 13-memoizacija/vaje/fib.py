from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n <= 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.
@lru_cache(maxsize=None)
def fib_cache(n):
    if n <= 1:
        return 1
    else:
        return fib_cache(n-1) + fib_cache(n-2)

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.
def fib_memo_rec(n):
    pass

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
def fib_memo_iter(n):
    if n <= 1:
        return 1
    returns = {}
    for i in range(n):
        if i <= 1:
            returns[i] = 1
        else:
            returns[i] = returns[i-1] + returns[i-2]
    return returns[n-1] + returns[n-2]
    


# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
def fib_iter(n):
    if n <= 1:
        return 1
    last = 1
    second_to_last = 1
    for i in range(n):
        if i <= 1:
            pass
        else:
            second_to_last, last = last, second_to_last + last 
    return second_to_last + last
