
vsebina_datoteke = "1102,34915192,34915192,7,4,7,99,0"


def naloga1(vsebina_datoteke, vnos):
    zacetni = list(map(int, vsebina_datoteke.split(",")))
    seznam = {}
    for i, a in enumerate(zacetni):
        seznam[i] = a

    def preberi(a):
        if a in seznam:
            return seznam[a]
        else:
            return 0
    x = vnos
    output = 0
    i = 0 
    baza = 0
    while seznam[i] != 99:
        if seznam[i] == 1:
            a = preberi(seznam[i+1])
            b = preberi(seznam[i+2])
            seznam[preberi(seznam[i+3])] = a + b
            i += 4
        elif seznam[i] == 2:
            a = preberi(seznam[i+1])
            b = preberi(seznam[i+2])
            seznam[preberi(seznam[i+3])] = a * b
            i += 4
        elif seznam[i] == 3:
            seznam[preberi(seznam[i+1])] = x
            i += 2

        elif seznam[i] == 4:
            a = preberi(seznam[i+1])
            output = a
            i += 2
        elif seznam[i] == 5:
            if preberi(seznam[i+1]) != 0:
                i = preberi(seznam[i+2])
            else:
                i += 3
        elif seznam[i] == 6:
            if preberi(seznam[i+1]) == 0:
                i = preberi(seznam[i+2])
            else:
                i += 3
        elif seznam[i] == 7:
            if preberi(seznam[i+1]) < preberi(seznam[i+2]):
                seznam[preberi(seznam[i+3])] = 1
            else:
                seznam[preberi(seznam[i+3])] = 0
            i += 4
        elif seznam[i] == 8:
            if preberi(seznam[i+1]) == preberi(seznam[i+2]):
                seznam[preberi(seznam[i+3])] = 1
            else:
                seznam[preberi(seznam[i+3])] = 0
            i += 4
        elif seznam[i] == 9:
            a = preberi(seznam[i+1])
            baza += a
            i += 2

        
        else:
            opt = seznam[i] % 100
            m1 = (seznam[i] // 100) % 10  
            m2 = (seznam[i] // 1000) % 10
            m3 = (seznam[i] // 10000) % 10
            if opt in [1, 2, 7, 8]:
                if m1 == 0:
                    a = preberi(seznam[i+1])
                elif m1 == 1:
                    a = seznam[i+1]
                else:
                    a = preberi(seznam[i+1] + baza)
                
                if m2 == 0:
                    b = preberi(seznam[i+2])
                elif m2 == 1:
                    b = seznam[i+2]
                else:
                    b = preberi(seznam[i+2] + baza)

                if opt == 1:
                    if m3 == 0:
                        seznam[preberi(seznam[i+3])] = a + b
                    elif m3 == 1:
                        seznam[i+3]  = a + b
                    else:
                        seznam[preberi(seznam[i+3] + baza)] = a + b

                elif opt == 2:
                    if m3 == 0:
                        seznam[preberi(seznam[i+3])] = a * b
                    elif m3 == 1:
                        seznam[i+3]  = a * b
                    else:
                        seznam[preberi(seznam[i+3] + baza)] = a * b

                elif opt == 7:
                    if m3 == 0:
                        if a < b:
                            seznam[preberi(seznam[i+3])] = 1
                        else:
                            seznam[preberi(seznam[i+3])] = 0

                    elif m3 == 1:
                        if a < b:
                            seznam[i+3]  = 1
                        else:
                            seznam[i+3]  = 0

                    else:
                        if a < b:
                            seznam[preberi(seznam[i+3] + baza)] = 1
                        else:
                            seznam[preberi(seznam[i+3] + baza)] = 0

                elif opt == 8:
                    if m3 == 0:
                        if a == b:
                            seznam[preberi(seznam[i+3])] = 1
                        else:
                            seznam[preberi(seznam[i+3])] = 0
                    elif m3 == 1:
                        if a == b:
                            seznam[i+3]  = 1
                        else:
                            seznam[i+3]  = 0
                    else:
                        if a == b:
                            seznam[preberi(seznam[i+3] + baza)] = 1
                        else:
                            seznam[preberi(seznam[i+3] + baza)] = 0
                            
                i += 4
            

            elif opt in [4, 9]:
                if m1 == 0:
                    a = preberi(seznam[i+1])
                elif m1 == 1:
                    a = seznam[i+1]
                else:
                    a = preberi(seznam[i+1] + baza)

                if opt == 4:
                    output = a
                else:
                    baza += a
                i += 2    

            elif opt in [5, 6]:   
                if m1 == 0:
                    a = preberi(seznam[i+1])
                elif m1 == 1:
                    a = seznam[i+1]
                else:
                    a  = preberi(seznam[i+1] + baza)

                if m2 == 0:
                    b = preberi(seznam[i+2])
                elif m2 == 1:
                    b = seznam[i+2]
                else:
                    b = preberi(seznam[i+2] + baza)    

                if opt == 5:
                    if a != 0:
                        i = b
                    else:
                        i += 3
                else:
                    if a == 0:
                        i = b
                    else:
                        i += 3 

    return str(output)


# if __name__ == '__main__':
#     with open('vaja.txt', encoding='utf-8') as f:
#         vsebina_datoteke = f.read()
odgovor1 = naloga1(vsebina_datoteke, 1)
with open('day_5_1.out', 'w', encoding='utf-8') as f:
    f.write(odgovor1)