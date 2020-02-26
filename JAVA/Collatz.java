public class Collatz {

    public static void main(String[] args) {
        System.out.println(naslednik(5)); // izpise vrstico z naslednik 5
        System.out.println(dolzina(5)); 
        System.out.println(najvecji(5));
        izpis(5); 
    }

    public static int naslednik(int clen) {
        if (clen % 2 == 0) return clen / 2;
        return 3 * clen + 1;
    }

    public static int dolzina(int clen) {
        int d = 1;
        while (clen > 1) {
            ++d; // lahko d++, d += 1
            clen = naslednik(clen);
        };
        return d
    }

    public static int najvecji(int clen) {
        int a = clen;
        while (clen > 1) {
            clen = naslednik(clen);
            if (clen > a) a = clen;
        };
        return a
    }

    public static void izpis(int clen) {
        while (clen > 1) {
            System.out.print(clen + " "); // niz + stevilo deluje ok, znak + stevilo pa ne
            clen = naslednik(clen);   // znake sesteva po ascii kodah
        }
        System.out.println(clen); // println se naredi novo vrstico
    }
}








// deljenje je kar isto za oboje
// torej ce imamo dve celi stevili bo celo stevilsko
// int a = 3; int b = 4; double x = (double a) / b

// s[++d] tu najprej poveca d, nato vstavi vrednost
// d[d++] pa najprej vstavi in nato poveca



