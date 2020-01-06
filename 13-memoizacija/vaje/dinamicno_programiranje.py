# (* ========== Vaje 6: Dinamično programiranje  ========== *)
# (* DELAMO OD SPODAJ GOR - "BOTTOM UP" *)

# (*----------------------------------------------------------------------------*]
#  Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
#  samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
#  v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
#  različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
#  zanima, katero pot naj ubere.

#  Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
#  sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
#  optimalni poti.
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  # max_cheese test_matrix;;
#  - : int = 13
# [*----------------------------------------------------------------------------*)

test_matrix = [[1, 2, 0],
              [2, 4, 5],
              [7, 0, 1]]

# (* sestavili bomo matriko, koliko sirov lahko max poberemo od tega polja naprej *)

def max_cheese(cheese_matrix):
    height = len(cheese_matrix)
    width = len(cheese_matrix[0])

    sum_matrix = [[0 for _ in range(width)] for _ in range(height)]
  
    def eat(i, j): 
        cheese = cheese_matrix[i][j]
        if i < (height - 1):
            if j < (width - 1):
                return cheese + (max(sum_matrix[i][j+1], sum_matrix[i+1][j]))
            else:
                return cheese + sum_matrix[i+1][j]
        elif j < (width - 1):
            return cheese + sum_matrix[i][j+1]
        else:
            return cheese



#   (* (* še ena možnost *)
#   let eat2 i j =
#     let cheese = cheese_matrix.(i).(j) in
#     let max_right = if j < (width - 1) then sum_matrix.(i).(j+1) else 0 in
#     let max_down = if i < (height - 1) then sum_matrix.(i+1).(j) else 0 in 
#     cheese + max max_down max_right 
#   in *)


    def loop(i, j): 
        cheese = eat(i, j)
        sum_matrix[i][j] = cheese
        if j > 0:
            loop(i, (j - 1))
        elif i > 0: #nova vrstica
            loop((i - 1), (width - 1))

    loop((height - 1),(width - 1))
    return sum_matrix[0][0]



# gremo še z memoizacijo
from functools import lru_cache

def max_cheese2(cheese_matrix):
    height = len(cheese_matrix)
    width = len(cheese_matrix[0])

    @lru_cache(maxsize=None)
    def mouse(i, j):
        cheese = cheese_matrix[i][j]
        max_right = mouse(i, j + 1) if j < (width - 1) else 0
        max_down = mouse(i + 1, j) if i < (height - 1) else 0
        return cheese + max(max_down, max_right)

    return mouse(0, 0)