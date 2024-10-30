module ProvaDePropriedades where


{-

--Prova de Propriedades:
  
  sumPowers2 n = sumPowers2 (n-1) + power2 n

  power2 :: Int-> Int
  power2 0 = 1
  power2 n = 2 * power2(n-1)

Agora realizando a prova:

  P(0) : sumPowers 0 + 1 = power2 (0+1)
      1 + 1 = power2 1
      2 = 2 * power2 0
      2 = 2*1
      2 = 2, logo é verdade o caso base

  P(n-1) : sumPower2 (n-1)+ 1 = power2 (n)

  P(n) : sumPowers2 n + 1 = power2 (n+1)
        sumPowers2 (n-1) + power n + 1=
      sumPowers2 (n-1) + 1 + powe2 n=
      power2 n + power2 n = 
      2 * power2 n = 
      power2 (n+1) , logo é verdade


--Prova sobre Listas:

	- caso base: provamos P ( [ ] )
	- passo indutivo: Provamos P (a:x) supondo que P(x) é verdadeiro.

	ex: sumList (double x) = 2 * sumList x

	sumList [ ] = 0
	sumList (a:as) = a + sumList as
	
	double [ ] = [ ]
	double (a:as) = 2*a : double as

	Caso base: P ( [ ] ):
	sumList ( double [ ]) = 2 * sumList [ ]
								2 * 0 = 0
	sumList [ ] = 0, logo é verdade

	P(x): 
		sumList(double x) = 2* sumList x

	P(a:x):
		sumList ( double (a:x)) = 2 * sumList (a:x)
		sumList ( (2*a) : double x) =
		(2*a) + sumList (double x) =
		(2*a) + (2 * sumList x) =
		2 * (a + sumList x) =
		2 * (sumList a:x), portanto provamos usando as regras e formações das funções.

-}