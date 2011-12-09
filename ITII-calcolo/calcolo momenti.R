# TODO: Add comment
# 
# Author: ortellic
###############################################################################


'Per il calcolo del valore atteso di h_t^m si deve procedere come
segue:

Il teorema 1 afferma che

E(|e_t|^m) = ... una certa formula.

Ora, essendo  
E(|e_t|^m) = E(|z_t|^m * |h_t|^m)
           = E(|z_t|^m) * E(|h_t|^m)
		   = E(|z_t|^m) * E(h_t^m)

otteniamo dunque che
          
E(h_t^m) = E(|e_t|^m) / E(|z_t|^m)

Nella notazione di Teraesvirta 

E(|z_t|^m) viene indicato con v_m e quindi
tenendo conto della formula del teorema 1 otteniamo
che il valore atteso di E(h_t^m) ha la medesima 
forma di quello di E(|e_t|^m) solo che il primo termine
v_m deve essere eliminato.

La formula recursiva diventa:

E(h_t^m) = 1 / (1-Gc_m) * {Sum_{j=1}^m (m,j) \tilde{G}g_j,c_(m-j) * E(h_t^(m-j))}

dove Gc_m = E(|c_t|^m) = E((b1*|z_t| + b2)^m) che deve essere < 1 e

\tilde{G}g_i,c_j = E(g_t^i * c_t^j) che nel nostro caso con g_t = b0 
diventa b0^i * E((b1*|z_t| + b2)^j)'

