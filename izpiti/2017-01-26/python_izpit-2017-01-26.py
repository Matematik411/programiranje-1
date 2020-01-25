# 2.


# v log(n) naj iskano mesto list[i] == i
def fiksna(list, i=0):
    d = len(list) // 2
    # if d == 0:
    #     return list[0] == i
    if d == 0:
        if list[0] == i:
            return i
        else:
            return None
    

    el = list[d]
    if el == i + d:
        return el
    elif el >= i + d:
        return fiksna(list[:d],i)
    else:
        return fiksna(list[d:],i+d)



print(fiksna([-4, -2, 0, 2, 4])) 
print(fiksna([-4, -2, 0, 3, 4]))
print(fiksna([-4, -2, 0, 2, 3]))
import random
print(fiksna([random.randint(0,101) for i in range(100)]))

