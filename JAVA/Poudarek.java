public class Poudarek {

    public static void main(String[] args) {
        System.out.println(poudari("Zadnja novica"));
        System.out.println(poudariBesede("Zadnja *novica* danes!"));

    }

    // String z veliko
    public static String poudari(String niz) {
        String s = "";
        for (int i = 0; i < niz.length(); ++i) {
            char znak = niz.charAt(i); // objektna metoda, zato jo klicemo na objektu
            if (i > 0) s += ' ';
            s += Character.toUpperCase(znak); // staticna metoda, zato jo klicemo na celem razredu
        }
        return s;
    }

    public static String poudariBesede(String niz) {
        String s = "";
        boolean krepko = false;
        for (int i = 0; i < niz.length(); ++i) {
            char znak = niz.charAt(i);

            // ! not, && and, || or
            if (znak == '*') krepko = !krepko;
        
            else if (krepko) s += Character.toUpperCase(znak);

            else s += znak;
        return s;
        }
    }











}