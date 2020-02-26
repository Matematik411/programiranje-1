public class Odvod {

    public static void main(String[] args) {
        double[] p1 = {1,2,3};
        double[] o1 = odvod(p1);
        izpis(o1);
        
        double[] o2 = odvod(new double[] {4, -1, 2, 0, 1}, 2);
        izpis o2;

        double o3 = odvod(new double[] {1});
        izpis o3;

    }

    // double[] je tabela realnih stevil
    public static double[] odvod(double[] p, int n) {
        // pri tabelah pa ni (), ker ni metoda
        if (n >= p.length) return new double[0]; // nicla pove koliko znakov vsebuje

        double[] op = new double[p.length - n] // ze ko ustvarimo tabelo moramo povedati njeno dolzino!
        
        // zacetni faktor je n!
        int f = 1;
        for (int i = 2; i <= n; ++i) f *= i;

        for (int i = n; i < p.length; ++i) {
            op[i - n] = f * p[i];
            f /= (i - n + 1);
            f *= (i + 1);
        };

        return op;
    }

    public static double[] odvod(double[] p) {
        return odvod(p, 1);
    }

    public static void izpis(double[] p) {
        System.out.print('{');
        for (i = 0; i < p.length; ++i) {
            if (i > 0) System.out.print(", ");
            System.out.print(p[i]);
        };
        System.out.println();
    }   


}