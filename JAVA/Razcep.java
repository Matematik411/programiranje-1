public class Razcep {
    // lep izpis razcepa stevila na prafaktorje

    public static void main(String[] args) {
        razcep(1024);
        razcep(5761665);
        razcep(60);
        razcep(25);
        razcep(125);
    
    }

    // napisi se za while zanko, ki sproti deli n
    public static void razcep(int n) {
        System.out.print(n);
        
        char oper = '=';

        // preveri kje ++
        for (int d = 2; d * d <= n, d++) {
            int e = 0;
            while (n  % d == 0) {
                e++;
                n / d;
            };

            if (e > 0) {
                System.out.print(" " + oper + ' ' + d) // argument tukaj je niz, saj je prvi clen niz
                if (e > 1) System.out.print("^" + e);
                oper = '*';
            }
        }

        // na tem mestu je lahko n se vedno vecji od ena, ce je, je prastevilski, in ga samo izspisemo

        if (n > 1) System.out.print(" " + oper + ' ' + n);
        System.out.println();

    }











}