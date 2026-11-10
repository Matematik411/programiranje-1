# Asimptotska notacija

Ponovimo definicije s predavanj:

$$f \in O(g) \iff \exists C > 0, n_0.\ \forall n \ge n_0.\ f(n) \le C \cdot g(n)$$

$$f \in \Omega(g) \iff \exists c > 0, n_0.\ \forall n \ge n_0.\ f(n) \ge c \cdot g(n)$$

$$f \in \Theta(g) \iff f \in O(g) \land f \in \Omega(g)$$

- Vse funkcije so nenegativne.
- Brez dokaza lahko uporabite, da sta relaciji $O$ in $\Theta$ refleksivni in tranzitivni (npr. velja $f \in O(f)$, iz $f \in O(g)$ in $g \in O(h)$ pa sledi $f \in O(h)$).

## Pravila za računanje

Dokažite naslednje trditve. Pri vsaki premislite še, ali velja tudi, če $O$ povsod zamenjamo s $\Theta$.

1. $h \in O(f) \implies a \cdot h \in O(f)$ za vsako konstanto $a > 0$.

    Iz $h(n) \le C \cdot f(n)$ sledi $a \cdot h(n) \le a C \cdot f(n)$, torej vzamemo $C' = a C$ in $n_0' = n_0$.

    S $\Theta$ gre enako, le da z $a$ pomnožimo še spodnjo mejo in dobimo $c' = a c$.

2. $h \in O(f) \implies h \in O(f + g)$ za vsako funkcijo $g$.

    Ker je $g$ nenegativna, je $f \le f + g$, zato je $h(n) \le C \cdot f(n) \le C \cdot (f(n) + g(n))$. Konstanti ostaneta isti.

    Glede $\Theta$ to ne velja, saj lahko $f + g$ raste precej hitreje kot $f$. Vzemimo $h(n) = f(n) = n$ in $g(n) = n^2$. Če bi veljalo $n \ge c \cdot (n + n^2)$, bi po deljenju z $n$ dobili $1 \ge c \cdot (1 + n)$, kar pa ne drži, brž ko je $n \ge 1 / c$.

3. $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 + f_2 \in O(g_1 + g_2)$

    Iz definicije vzemimo $C_1, n_1$ in $C_2, n_2$. Za $n \ge \max(n_1, n_2)$ veljata obe oceni, zato je

    $$f_1(n) + f_2(n) \le C_1 \cdot g_1(n) + C_2 \cdot g_2(n) \le \max(C_1, C_2) \cdot (g_1(n) + g_2(n)).$$

    S $\Theta$ gre enako, za spodnjo mejo vzamemo $c = \min(c_1, c_2)$.

4. $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 \cdot f_2 \in O(g_1 \cdot g_2)$

    Za $n \ge \max(n_1, n_2)$ velja $f_1(n) \le C_1 \cdot g_1(n)$ in $f_2(n) \le C_2 \cdot g_2(n)$. Ker je vse nenegativno, lahko neenakosti zmnožimo:

    $$f_1(n) \cdot f_2(n) \le C_1 C_2 \cdot g_1(n) \cdot g_2(n).$$

    S $\Theta$ enako zmnožimo še spodnji meji in dobimo $c = c_1 c_2$.

## Ocene

5. $f_1 + f_2 \in \Theta(\max(f_1, f_2))$, kjer je $\max(f_1, f_2)$ funkcija $n \mapsto \max(f_1(n), f_2(n))$.

    Vsota dveh nenegativnih števil je vsaj tolikšna kot večje izmed njiju in kvečjemu dvakrat tolikšna:

    $$\max(f_1(n), f_2(n)) \le f_1(n) + f_2(n) \le 2 \cdot \max(f_1(n), f_2(n)).$$

    Torej $c = 1$, $C = 2$ in $n_0 = 0$. Pri vsoti je pomemben le hitreje rastoči člen.

6. S pomočjo pravil iz prejšnjih nalog, pokažite, da je $\frac{n (n + 1)}{2} \in \Theta(n^2)$.

    Razpišemo $\frac{n (n + 1)}{2} = \frac{1}{2} n^2 + \frac{1}{2} n$. Po 1. nalogi je $\frac{1}{2} n^2 \in \Theta(n^2)$ in $\frac{1}{2} n \in \Theta(n)$, po 3. nalogi je vsota v $\Theta(n^2 + n)$, po 5. nalogi pa je $n^2 + n \in \Theta(\max(n^2, n)) = \Theta(n^2)$. Zaradi tranzitivnosti smo končali.

    Gre tudi direktno: $\frac{1}{2} n^2 \le \frac{n (n + 1)}{2} \le n^2$, torej $c = \frac{1}{2}$ in $C = 1$.

7. $\log_b n \in \Theta(\log_2 n)$ za vsako osnovo $b > 1$.

    $$\log_b n = \frac{\log_2 n}{\log_2 b} = \frac{1}{\log_2 b} \cdot \log_2 n,$$

    kjer je ulomek pozitivna konstanta (ker je $b > 1$). Po 1. nalogi smo končali.

    Zato osnovo logaritma ponavadi kar izpustimo in pišemo $O(\log n)$.

8. $\log_2 (n!) \in \Theta(n \log_2 n)$

    Zgornja meja je enostavna:
    $n! = 1 \cdot 2 \cdots n \le n \cdot n \cdots n = n^n$, zato je $\log_2 (n!) \le n \log_2 n$.

    Za spodnjo mejo bi radi vse faktorje ocenili z $n / 2$, a manjši faktorji so manjši od $n / 2$ (res, $6! = 720 < 729 = 3^6$). Zato jih ocenimo kar z $1$, preostale pa z $n / 2$. Teh je vsaj $n / 2$, zato je

    $$n! \ge \left(\frac{n}{2}\right)^{n / 2} \quad \text{in} \quad \log_2 (n!) \ge \frac{n}{2} \log_2 \frac{n}{2} = \frac{n}{2} (\log_2 n - 1).$$

    Za $n \ge 4$ je $\log_2 n - 1 \ge \frac{1}{2} \log_2 n$, torej je $\log_2 (n!) \ge \frac{1}{4} n \log_2 n$.

    To oceno bomo potrebovali, ko bomo pokazali, da vsak algoritem za urejanje s primerjavami potrebuje $\Omega(n \log n)$ primerjav.

9. (Dodatna naloga) $2^{n + 1} \in \Theta(2^n)$, vendar $2^{2n} \notin O(2^n)$.

    Ker je $2^{n + 1} = 2 \cdot 2^n$, prvi del sledi iz 1. naloge.

    Če bi veljalo $2^{2n} \le C \cdot 2^n$, bi po deljenju z $2^n$ dobili $2^n \le C$, kar za velike $n$ ne drži. Konstanta, prišteta eksponentu, se torej spremeni v konstanten faktor, pomnožen eksponent pa ne, saj je $2^{2n} = 4^n$.
