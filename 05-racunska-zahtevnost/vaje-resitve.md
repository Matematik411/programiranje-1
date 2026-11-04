# Asimptotska notacija

Ponovimo definicije s predavanj:

$$f \in O(g) \iff \exists C > 0, n_0.\ \forall n \ge n_0.\ f(n) \le C \cdot g(n)$$

$$f \in \Omega(g) \iff \exists c > 0, n_0.\ \forall n \ge n_0.\ f(n) \ge c \cdot g(n)$$

$$f \in \Theta(g) \iff f \in O(g) \land f \in \Omega(g)$$

- Vse funkcije so nenegativne.
- Brez dokaza lahko uporabite, da sta relaciji $O$ in $\Theta$ refleksivni in tranzitivni (npr. velja $f \in O(f)$, iz $f \in O(g)$ in $g \in O(h)$ pa sledi $f \in O(h)$).

## Pravila za računanje

Dokažite naslednje trditve. Pri vsaki premislite še, ali velja tudi, če $O$ povsod zamenjamo s $\Theta$.

**1.** $h \in O(f) \implies a \cdot h \in O(f)$ za vsako konstanto $a > 0$.

Naj bo $h \in O(f)$. Po definiciji obstajata $C > 0$ in $n_0$, da za vse $n \ge n_0$ velja $h(n) \le C \cdot f(n)$. Potem za vse $n \ge n_0$ velja tudi

$$a \cdot h(n) \le (a C) \cdot f(n),$$

kjer je $a C > 0$. Torej je $a \cdot h \in O(f)$ s konstantama $C' = a C$ in $n_0' = n_0$.

**Ali velja s $\Theta$?** Da. Če je poleg tega $h \in \Omega(f)$, obstajata $c > 0$ in $n_1$, da za vse $n \ge n_1$ velja $h(n) \ge c \cdot f(n)$. Zato je tudi $a \cdot h(n) \ge (a c) \cdot f(n)$, kjer je $a c > 0$. Torej je $a \cdot h \in \Omega(f)$ in skupaj $a \cdot h \in \Theta(f)$.

**2.** $h \in O(f) \implies h \in O(f + g)$ za vsako funkcijo $g$.

Naj bosta $C > 0$ in $n_0$ konstanti iz definicije $h \in O(f)$. Ker je $g(n) \ge 0$, je $f(n) \le f(n) + g(n)$, zato za vse $n \ge n_0$ velja

$$h(n) \le C \cdot f(n) \le C \cdot (f(n) + g(n)).$$

Torej je $h \in O(f + g)$ z istima konstantama. Pri tem smo bistveno uporabili, da je $g$ nenegativna: za $g = -f$ bi bila $f + g$ ničelna funkcija.

**Ali velja s $\Theta$?** Ne. Vzemimo $h(n) = f(n) = n$ in $g(n) = n^2$. Očitno je $h \in \Theta(f)$. Pokažimo, da $h \notin \Omega(f + g)$. Denimo, da obstajata $c > 0$ in $n_0$, da za vse $n \ge n_0$ velja

$$n \ge c \cdot (n + n^2).$$

Izberimo $n \ge \max(n_0, 1, 1 / c)$. Potem je $c n \ge 1$, zato je $c n^2 = (c n) \cdot n \ge n$. Ker je še $c n > 0$, je $c \cdot (n + n^2) = c n + c n^2 > n$, kar je protislovje. Zgornja meja se pri prištevanju torej ohrani, spodnja pa ne, saj lahko $f + g$ raste bistveno hitreje kot $f$.

**3.** $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 + f_2 \in O(g_1 + g_2)$

Naj bosta $C_1, n_1$ konstanti iz definicije $f_1 \in O(g_1)$ in $C_2, n_2$ konstanti iz definicije $f_2 \in O(g_2)$. Vzemimo $C = \max(C_1, C_2)$ in $n_0 = \max(n_1, n_2)$. Za vse $n \ge n_0$ veljata obe oceni, zato je

$$f_1(n) + f_2(n) \le C_1 \cdot g_1(n) + C_2 \cdot g_2(n) \le C \cdot g_1(n) + C \cdot g_2(n) = C \cdot (g_1(n) + g_2(n)).$$

V drugem koraku smo uporabili, da sta $g_1$ in $g_2$ nenegativni.

**Ali velja s $\Theta$?** Da. Naj bosta $c_1, m_1$ konstanti iz definicije $f_1 \in \Omega(g_1)$ in $c_2, m_2$ konstanti iz definicije $f_2 \in \Omega(g_2)$. Spodnjo mejo dokažemo enako, le da vzamemo $c = \min(c_1, c_2) > 0$. Za vse $n \ge \max(m_1, m_2)$ je

$$f_1(n) + f_2(n) \ge c_1 \cdot g_1(n) + c_2 \cdot g_2(n) \ge c \cdot (g_1(n) + g_2(n)).$$

**4.** $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 \cdot f_2 \in O(g_1 \cdot g_2)$

Naj bodo $C_1, n_1, C_2, n_2$ kot pri prejšnji nalogi in $n_0 = \max(n_1, n_2)$. Za vse $n \ge n_0$ velja $0 \le f_1(n) \le C_1 \cdot g_1(n)$ in $0 \le f_2(n) \le C_2 \cdot g_2(n)$. Neenakosti med nenegativnimi števili lahko zmnožimo, zato je

$$f_1(n) \cdot f_2(n) \le C_1 \cdot g_1(n) \cdot C_2 \cdot g_2(n) = (C_1 C_2) \cdot g_1(n) \cdot g_2(n).$$

Torej je $f_1 \cdot f_2 \in O(g_1 \cdot g_2)$ s konstanto $C = C_1 C_2$.

**Ali velja s $\Theta$?** Da. Iz $f_1(n) \ge c_1 \cdot g_1(n) \ge 0$ in $f_2(n) \ge c_2 \cdot g_2(n) \ge 0$ enako dobimo $f_1(n) \cdot f_2(n) \ge (c_1 c_2) \cdot g_1(n) \cdot g_2(n)$ za vse $n \ge \max(m_1, m_2)$.

## Ocene

**5.** $f_1 + f_2 \in \Theta(\max(f_1, f_2))$, kjer je $\max(f_1, f_2)$ funkcija $n \mapsto \max(f_1(n), f_2(n))$.

Ker sta $f_1(n)$ in $f_2(n)$ nenegativni, je večja od njiju kvečjemu enaka njuni vsoti, vsota pa je kvečjemu dvakratnik večje. Za vse $n$ torej velja

$$\max(f_1(n), f_2(n)) \le f_1(n) + f_2(n) \le 2 \cdot \max(f_1(n), f_2(n)).$$

Desna neenakost pove, da je $f_1 + f_2 \in O(\max(f_1, f_2))$ s $C = 2$, leva pa, da je $f_1 + f_2 \in \Omega(\max(f_1, f_2))$ s $c = 1$, v obeh primerih z $n_0 = 0$.

Ker je $O$ tranzitivna, od tod sledi tudi $O(f_1 + f_2) = O(\max(f_1, f_2))$. Pri vsoti dveh funkcij je torej pomemben le hitreje rastoči člen.

**6.** S pomočjo pravil iz prejšnjih nalog, pokažite, da je $\frac{n (n + 1)}{2} \in \Theta(n^2)$.

Velja $\frac{n (n + 1)}{2} = \frac{1}{2} n^2 + \frac{1}{2} n$.

- Ker je $n^2 \in \Theta(n^2)$, je po nalogi 1 (različica s $\Theta$) $\frac{1}{2} n^2 \in \Theta(n^2)$. Enako je $\frac{1}{2} n \in \Theta(n)$.
- Po nalogi 3 (različica s $\Theta$) je $\frac{1}{2} n^2 + \frac{1}{2} n \in \Theta(n^2 + n)$.
- Po nalogi 5 je $n^2 + n \in \Theta(\max(n^2, n))$, za naravna števila pa je $\max(n^2, n) = n^2$.
- Zaradi tranzitivnosti je $\frac{n (n + 1)}{2} \in \Theta(n^2)$.

Za primerjavo: neposredno iz definicije dobimo isto s konstantama $c = \frac{1}{2}$ in $C = 1$ ter $n_0 = 0$, saj je $\frac{1}{2} n^2 \le \frac{n (n + 1)}{2} \le n^2$ za vsa naravna števila $n$.

**7.** $\log_b n \in \Theta(\log_2 n)$ za vsako osnovo $b > 1$.

Po formuli za menjavo osnove je

$$\log_b n = \frac{\log_2 n}{\log_2 b} = \frac{1}{\log_2 b} \cdot \log_2 n.$$

Ker je $b > 1$, je $\log_2 b > 0$, zato je $a = \frac{1}{\log_2 b}$ pozitivna konstanta. Po nalogi 1 (različica s $\Theta$) je $\log_b n = a \cdot \log_2 n \in \Theta(\log_2 n)$. Neposredno iz definicije lahko vzamemo $c = C = a$ in $n_0 = 1$.

Osnova logaritma torej na asimptotsko oceno ne vpliva, zato jo običajno izpustimo in pišemo kar $O(\log n)$.

**8.** $\log_2 (n!) \in \Theta(n \log_2 n)$

*Zgornja meja.* Za $n \ge 1$ je

$$n! = 1 \cdot 2 \cdots n \le n \cdot n \cdots n = n^n.$$

Ker je logaritem naraščajoč, je $\log_2 (n!) \le \log_2 (n^n) = n \log_2 n$. Torej je $\log_2 (n!) \in O(n \log_2 n)$ s $C = 1$ in $n_0 = 1$.

*Spodnja meja.* Ocena $n! \ge (n / 2)^n$ ne drži (že za $n = 6$ je $6! = 720 < 729 = 3^6$), saj so manjši faktorji manjši od $n / 2$. Zato manjše faktorje navzdol ocenimo z $1$, večje pa z $n / 2$. Faktorjev $k$ v $n!$, za katere je $k \ge n / 2$, je vsaj $n / 2$, zato za $n \ge 2$ velja

$$n! \ge \left(\frac{n}{2}\right)^{n / 2}$$

in

$$\log_2 (n!) \ge \frac{n}{2} \log_2 \frac{n}{2} = \frac{n}{2} (\log_2 n - 1).$$

Za $n \ge 4$ je $\log_2 n \ge 2$, zato je $\log_2 n - 1 \ge \frac{1}{2} \log_2 n$ in

$$\log_2 (n!) \ge \frac{n}{2} \cdot \frac{1}{2} \log_2 n = \frac{1}{4} n \log_2 n.$$

Torej je $\log_2 (n!) \in \Omega(n \log_2 n)$ s $c = \frac{1}{4}$ in $n_0 = 4$, skupaj z zgornjo mejo pa $\log_2 (n!) \in \Theta(n \log_2 n)$.

Ta ocena pride prav pri dokazu, da vsak algoritem za urejanje, ki elemente le primerja med sabo, v najslabšem primeru potrebuje $\Omega(n \log n)$ primerjav.

**9.** (Dodatna naloga) $2^{n + 1} \in \Theta(2^n)$, vendar $2^{2n} \notin O(2^n)$.

Ker je $2^{n + 1} = 2 \cdot 2^n$, je po nalogi 1 (različica s $\Theta$) $2^{n + 1} \in \Theta(2^n)$.

Pokažimo, da $2^{2n} \notin O(2^n)$. Denimo nasprotno, da obstajata $C > 0$ in $n_0$, da za vse $n \ge n_0$ velja $2^{2n} \le C \cdot 2^n$. Ko neenakost delimo z $2^n > 0$, dobimo $2^n \le C$ za vse $n \ge n_0$. To ne drži, saj za $n \ge \max(n_0, \log_2 C + 1)$ velja $2^n \ge 2 C > C$.

Pri $2^{n + 1}$ se konstanta v eksponentu spremeni v konstanten faktor, pri $2^{2n} = (2^n)^2 = 4^n$ pa ne.
