import csv
import os
import requests
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'Data_main'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'Main.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'Data.csv'


def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in puskuša vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        page_content = requests.get(url).text
    except requests.exceptions.RequestException as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(e)
        print("Prišlo je do napake!")
        page_content = ""
        #se splača vseeno shraniti nekaj v spremenljivko, zato da se program ne sesuje
    # nadaljujemo s kodo če ni prišlo do napake
    return page_content


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    niz = download_url_to_string(page)
    save_string_to_file(niz, directory, filename)


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz"""
    path = os.path.join(directory, filename)
    with open(path, "r", encoding="UTF-8") as f:
        return f.read()


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne njihov seznam"""
    niz = re.compile(
        r'<div class="ad( featured)?">.*?'
        r'</div>\s*</div>\s*</div>',
        flags=re.DOTALL
        )
    seznam = []
    for oglas in re.finditer(niz, page_content):
        seznam.append(oglas.group(0))
    return seznam
#page_to_ads(read_file_to_string("podatki", "glavna.html"))



# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke
    """
    niz = re.compile(
        r'<h3><a title=".*?">(?P<IME>.*)</a></h3>'
        r'\s*(?P<OPIS>.*?)\s*<div class="additionalInfo">.*?'
        r'<div class="price">(<span>)?(?P<CENA>.*?)(</span>)?</div>\s*</div>\s*?<div class="clear">',
        flags=re.DOTALL
    )
    for zadetek in re.finditer(niz, block):
        return zadetek.groupdict()
#get_dict_from_ad_block(page_to_ads(read_file_to_string("podatki", "glavna.html"))[3])


# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    stran = read_file_to_string(directory, filename)
    bloki = page_to_ads(stran)
    return [
        get_dict_from_ad_block(blok)
        for blok
        in bloki
    ]


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding="utf-8") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da sa ključi vseh
    sloverjev parametra ads enaki in je seznam ads neprazen.

    """
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    kljuci = [key for key in ads[0].keys()]
    write_csv(kljuci, ads, directory, filename)
# k = ads_from_file("glavna.html","podatki")
# write_cat_ads_to_csv(k, "podatki", csv_filename)
# Celoten program poženemo v glavni funkciji

def main():
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran
    save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename)
    # Iz lokalne (html) datoteke preberemo podatke
    # nepotrebno
    # Podatke prebermo v lepšo obliko (seznam slovarjev)
    podatki = ads_from_file(frontpage_filename, cat_directory)
    # Podatke shranimo v csv datoteko
    write_cat_ads_to_csv(podatki, cat_directory, csv_filename)
    # Dodatno: S pomočjo parameteov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prense (četudi že obstaja)
    # in enako za pretvorbo
    # FUNKCIJA VSE NAREDI ŠE ENKRAT

    


if __name__ == '__main__':
    main()
