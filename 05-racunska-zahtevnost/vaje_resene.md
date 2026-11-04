# Asimptotska notacija – ključni koraki

**1.** $h \in O(f) \implies a \cdot h \in O(f)$

- obstajata $C$, $n_0$: $h(n) \le C \cdot f(n)$
- $a \cdot h(n) \le (a C) \cdot f(n)$
- $C' = a C$ in $n_0' = n_0$
- $\Theta$: da, $c' = a c$, $n_1' = n_1$

**2.** $h \in O(f) \implies h \in O(f + g)$

- obstajata $C$, $n_0$: $h(n) \le C \cdot f(n)$
- $g \ge 0$, zato $f \le f + g$
- $h(n) \le C \cdot (f(n) + g(n))$, ista $C$, $n_0$
- $\Theta$: ne, $h = f = n$, $g = n^2$
- $n \ge c \cdot (n + n^2)$ ne velja za $n \ge \max(n_0, 1, 1/c)$

**3.** $f_1 + f_2 \in O(g_1 + g_2)$

- $C_1, n_1$ in $C_2, n_2$
- $C = \max(C_1, C_2)$, $n_0 = \max(n_1, n_2)$
- $f_1 + f_2 \le C_1 g_1 + C_2 g_2 \le C \cdot (g_1 + g_2)$
- $\Theta$: da, $c = \min(c_1, c_2)$

**4.** $f_1 \cdot f_2 \in O(g_1 \cdot g_2)$

- $0 \le f_1 \le C_1 g_1$ in $0 \le f_2 \le C_2 g_2$ za $n \ge \max(n_1, n_2)$
- zmnožimo (vse nenegativno)
- $C = C_1 C_2$
- $\Theta$: da, $c = c_1 c_2$

**5.** $f_1 + f_2 \in \Theta(\max(f_1, f_2))$

- $\max(f_1, f_2) \le f_1 + f_2 \le 2 \cdot \max(f_1, f_2)$
- $C = 2$, $c = 1$, $n_0 = 0$

**6.** $\frac{n (n + 1)}{2} \in \Theta(n^2)$

- $\frac{n (n + 1)}{2} = \frac{1}{2} n^2 + \frac{1}{2} n$
- nal. 1: $\frac{1}{2} n^2 \in \Theta(n^2)$, $\frac{1}{2} n \in \Theta(n)$
- nal. 3: vsota $\in \Theta(n^2 + n)$
- nal. 5: $\Theta(\max(n^2, n)) = \Theta(n^2)$
- tranzitivnost
- direktno: $c = \frac{1}{2}$, $C = 1$, $n_0 = 0$

**7.** $\log_b n \in \Theta(\log_2 n)$

- $\log_b n = \frac{1}{\log_2 b} \cdot \log_2 n$
- $b > 1$, zato $a = \frac{1}{\log_2 b} > 0$
- nal. 1 ($\Theta$), direktno $c = C = a$, $n_0 = 1$
- osnova ni pomembna, pišemo $O(\log n)$

**8.** $\log_2 (n!) \in \Theta(n \log_2 n)$

- zgoraj: $n! \le n^n$, zato $\log_2 (n!) \le n \log_2 n$
- $C = 1$, $n_0 = 1$
- pozor: $n! \ge (n / 2)^n$ ne velja ($6! = 720 < 729 = 3^6$)
- spodaj: vsaj $n / 2$ faktorjev $\ge n / 2$, ostali $\ge 1$
- $n! \ge (n / 2)^{n / 2}$
- $\log_2 (n!) \ge \frac{n}{2} (\log_2 n - 1)$
- za $n \ge 4$: $\log_2 n - 1 \ge \frac{1}{2} \log_2 n$
- $c = \frac{1}{4}$, $n_0 = 4$
- uporaba: urejanje s primerjavami je $\Omega(n \log n)$

**9.** $2^{n + 1} \in \Theta(2^n)$, $2^{2n} \notin O(2^n)$

- $2^{n + 1} = 2 \cdot 2^n$, nal. 1
- denimo $2^{2n} \le C \cdot 2^n$
- delimo z $2^n$: $2^n \le C$
- protislovje za $n \ge \max(n_0, \log_2 C + 1)$
