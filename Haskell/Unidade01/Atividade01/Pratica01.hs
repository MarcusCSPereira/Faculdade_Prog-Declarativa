import Data.Char
import GHC.Real (notANumber)
import Numeric (readHex, showHex)

-- 1
areaTri :: Float -> Float -> Float
areaTri b h = (b * h) / 2

-- 2
retornaMenor :: Int -> Int -> Int -> Int
retornaMenor a b c
  | a < b && a < c = a
  | b < a && b < c = b
  | otherwise = c

-- 3
todosIguais :: Int -> Int -> Int -> Int -> Bool
todosIguais a b c d
  | a == b && b == c && c == d = True
  | otherwise = False

-- 4
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c
  | a /= b && b /= c && a /= c = True
  | otherwise = False

-- 5
mediaPonderada :: Float -> Float -> Float -> Float -> Float
mediaPonderada a b c d = (a + 2*b + 3*c + 4*d) / 10

-- 6
calculaDesconto :: Float -> Float
calculaDesconto preco = preco - (preco * 0.2)

-- 7
calculaXor :: Bool -> Bool -> Bool
calculaXor a b =
  a /= b

-- 8
progressaoAriti :: Float -> Float -> Float
progressaoAriti a b =
  b + 9 * a

-- Outra forma de fazer a questão 8
progressaoAritiRecu :: Float -> Float -> Float
progressaoAritiRecu a b = calcTerm 10 a b
  where
    calcTerm 1 a b = a
    calcTerm n a b = calcTerm (n - 1) (a + b) b

-- 9
testaNumero :: Int -> String
testaNumero a
  | a > 0 = "Positivo"
  | a < 0 = "Negativo"
  | otherwise = "Nulo"

-- 10
funcaoNand :: Bool -> Bool -> Bool
funcaoNand a b
  | a == b = False
  | a /= b = True

-- 11
verificaCaracter :: Char -> Bool
verificaCaracter a =
  a >= '0' && a <= '9'

-- 12
testaDigito :: Char -> Bool
testaDigito a = isDigit a

-- 13
converteMaiuscula :: Char -> Char
converteMaiuscula c = toUpper c

-- 14
converteMinuscula :: Char -> Char
converteMinuscula c = toLower c

-- 15
somaAlgorismos :: Int -> String
somaAlgorismos a
  | a < 0 || a >= 1000 = "Entrada Inválida"
  | a < 10 = "1"
  | a < 100 = "2"
  | otherwise = "3"

-- 16
mediaAritimedica :: Float -> Float -> Float -> Float
mediaAritimedica a b c = (a+b+c) / 3

-- 17
ladostriangulo :: Float -> Float -> Float -> Bool
ladostriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- 18
tipoTriangulo :: Float -> Float -> Float -> String
tipoTriangulo a b c
  | a^2 + b^2 == c^2        = "Triângulo Retângulo"
  | a^2 + b^2 > c^2         = "Triângulo Acutângulo"
  | otherwise               = "Triângulo Obtusângulo"

-- 19
somaPA :: Float -> Float -> Int -> Float
somaPA a1 r n = n' * (a1 + an) / 2
  where
    n' = fromIntegral n  -- Convertendo n para Float
    an = a1 + (n' - 1) * r  -- Calculando o último termo da PA

-- 20
localizacaoPonto :: Float -> Float -> String
localizacaoPonto x y
  | x == 0 && y == 0 = "Origem"
  | x == 0 && y > 0  = "Eixo Y positivo"
  | x == 0 && y < 0  = "Eixo Y negativo"
  | y == 0 && x > 0  = "Eixo X positivo"
  | y == 0 && x < 0  = "Eixo X negativo"
  | x > 0 && y > 0   = "Primeiro quadrante"
  | x < 0 && y > 0   = "Segundo quadrante"
  | x < 0 && y < 0   = "Terceiro quadrante"
  | x > 0 && y < 0   = "Quarto quadrante"

-- 21
qualMes :: Int -> String
qualMes m
  | m <=0 || m > 12 = "Entrada Inválida"
  | m == 1 = "Janeiro"
  | m == 2 = "Fevereiro"
  | m == 3 = "Março"
  | m == 4 = "Abril"
  | m == 5 = "Maio"
  | m == 6 = "Junho"
  | m == 7 = "Julho"
  | m == 8 = "Agosto"
  | m == 9 = "Setembro"
  | m == 10 = "Outubro"
  | m == 11 = "Novembro"
  | m == 12 = "Dezembro"

-- 22
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- 23
raizesEQ2Grau :: Float -> Float -> Float -> (Float, Float)
raizesEQ2Grau a b c = (x1, x2)
  where
    delta = b^2 - 4*a*c
    x1 = (-b + sqrt delta) / (2*a)
    x2 = (-b - sqrt delta) / (2*a)

-- 24
converteTemperaturaFparaC :: Float -> Float
converteTemperaturaFparaC f = (f - 32) * 5/9

-- 25
calculaIMC :: Float -> Float -> String
calculaIMC peso altura
  | imc < 18.5 = "Abaixo do peso"
  | imc < 25 = "Peso normal"
  | imc < 30 = "Sobrepeso"
  | imc < 35 = "Obesidade grau 1"
  | imc < 40 = "Obesidade grau 2"
  | otherwise = "Obesidade grau 3"
  where
    imc = peso / altura^2

-- 25
calculaIdade :: Int -> String
calculaIdade idade
  | idade <= 0 = "Idade inválida"
  | idade < 13 = "Criança"
  | idade < 18 = "Adolescente"
  | idade < 60 = "Adulto"
  | otherwise = "Idoso"

-- 26
classificaNota :: Int -> String
classificaNota nota
  | nota < 0 || nota > 100 = "Nota inválida"
  | nota < 50 = "F"
  | nota < 60 = "D"
  | nota < 70 = "C"
  | nota < 80 = "B"
  | nota <= 100 = "A"

-- 27
proximoCaracter :: Char -> Char
proximoCaracter c = chr (ord c + 1)

-- 28

-- a) O paradigma funcional é o paradigma que funciona baseado em funções matemáticas, onde se utiliza de funções puras, imutabilidade e recursão, ou seja, não se altera o estado do programa, apenas se cria novos estados, e não se utiliza dados mutáveis. Logo programar no paradigma funcional consiste em definir funções e utiliza-las para avaliar expressões.

-- b) O paradigma imperativo, é o paradigma que funciona baseado em comandos, onde o estado do programa é alterado, nele já se uso dados mutáveis. Logo programar no paradigma imperativo consiste em definir comandos e utiliza-los para alterar o estado do programa, e assim obter resultados a partir dessa troca de estado.

--c) A imutabilidade, consiste em não alterar o estado do programa, ou seja, não alterar o valor de variáveis, mas sim criar novas variáveis com novos valores. A imutabilidade é uma característica do paradigma funcional.

--d) O efeito colateral consiste em mudanças de estado que ocorrem fora do escopo de uma função, ou seja quando ela modifica uma variável global ou um parâmetro passado por referência. O efeito colateral é geralmente evitado no paradigma funcional.

--e) As funções puras são como funções da matemática, que para os mesmos argumentos de entrada sempre retornam o mesmo valor na saída, além disso elas não produzem efeitos colaterais. As funções puras são uma característica do paradigma funcional.

--f) A transparência referencial é um conceito que diz que uma função pode ser substituída pelo seu valor de retorno sem alterar o resultado, ou seja sem alterar o comportamento do programa, isso ocorre geralmente quando se utiliza funções puras e imutáveis.Applicative

--g) A lazy evaluation ou avaliação preguiçosa consiste em um método usado em que o computador só avalia valores de expressões quando eles vão ser utilizados, ou seja, quando forem realmente necessários, as vezes valores nem são avaliados se não forem utilizados. A lazy evaluation é uma característica do Haskell.

--h) A Eager evaluation ou avaliação ansiosa consiste em um método usado em que o computador avalia valores de expressões assim que eles são definidos, assim que são ligados a alguma variável ou função, ou seja, avalia todos os valores de uma expressão antes de serem utilizados. A eager evaluation é uma característica do paradigma imperativo.

--i) Uma função de alta ordem é uma função que recebe outra função como argumento ou retorna uma função como resultado. Funções de alta ordem são uma característica do Haskell.

--29

-- (a) abs: Retorna o valor absoluto de um número.
-- abs (-5)   -- Resultado: 5

-- (b) signum: Retorna o sinal de um número. Retorna -1 para números negativos, 1 para positivos, e 0 para zero.
--signum (-10)   -- Resultado: -1
--signum 0       -- Resultado: 0
--signum 5       -- Resultado: 1

-- (c) sqrt: Retorna a raiz quadrada de um número.
--sqrt 25.0   -- Resultado: 5.0

-- (d) exp: Retorna e (a base do logaritmo natural) elevado à potência de um número.
--exp 1   -- Resultado: 2.718281828459045 (aproximadamente o valor de 'e')

-- (e) log: Retorna o logaritmo natural de um número.
--log 2.718281828459045   -- Resultado: 1.0 (aproximadamente)

-- (f) negate: Inverte o sinal de um número.
--negate 5   -- Resultado: -5
--negate (-3) -- Resultado: 3

-- (g) logBase: Calcula o logaritmo de um número em uma base específica.
--logBase 2 8   -- Resultado: 3.0 (pois 2^3 = 8)

-- (h) floor: Retorna o maior número inteiro que é menor ou igual ao número fornecido.
--floor 3.7   -- Resultado: 3
--floor (-3.7) -- Resultado: -4

-- (i) ceiling: Retorna o menor número inteiro que é maior ou igual ao número fornecido.
--ceiling 3.7   -- Resultado: 4

-- (j) round: Arredonda um número para o inteiro mais próximo.
--round 4.5   -- Resultado: 4 (em Haskell, o arredondamento é para o inteiro par mais próximo)
--round 5.5   -- Resultado: 6

-- (k) truncate: Remove a parte decimal de um número, efetivamente "truncando" o número para o inteiro mais próximo que é menor ou igual ao número fornecido.
--truncate 4.7   -- Resultado: 4
--truncate (-4.7) -- Resultado: -4

-- (l) length: Retorna o número de elementos em uma lista.
-- Exemplo:
--length [1, 2, 3, 4, 5]   -- Resultado: 5
--length "Haskell"         -- Resultado: 7

-- (m) show: Converte um valor em uma string que representa o valor.
-- Exemplo:
--show 123   -- Resultado: "123"
--show True  -- Resultado: "True"
--show [1, 2, 3] -- Resultado: "[1,2,3]"


--30

--a) 5 + 6 + 9 + 7 + 1 = 28

--b) 255 + 15 + 10 = 280
-- Converter de hexadecimal para decimal
--let hexToDec = read "0xff" :: Int   -- Resultado: 255

--31
--Forma 1: dobro = dobro 2 + dobro 2 -> dobro = 2 + 2 + 2 + 2 = 8
--Forma 2: dobro 2 = 2 + 2 = 4 -> dobro = 4 + 4 = 8

--32
calculaDigitos :: Int
calculaDigitos = length (show (2 ^ 100))

--Curiosidades:

{-
  Podemos fazer assim em Haskell: 

  type Base = Float
  type Altura = Float
  type Area = Float

  area :: Base -> Altura -> Area
  area b h =  (b*h) / 2.0


  Em Haskell, operadores são funções que podem ser aplicadas entre dois argumentos. 
    Eles são geralmente utilizados em notação infixa, o que significa que o operador 
    fica entre seus operandos, como em `a + b`.

    Tipos Comuns de Operadores:
    
    1. Operadores Aritméticos:
      - `+`: Soma dois números. Exemplo: 3 + 5 resulta em 8.
      - `-`: Subtrai o segundo número do primeiro. Exemplo: 10 - 4 resulta em 6.
      - `*`: Multiplica dois números. Exemplo: 7 * 3 resulta em 21.
      - `/`: Divide o primeiro número pelo segundo (para números de ponto flutuante). Exemplo: 10 / 2 resulta em 5.0.
      - `div`: Divisão inteira. Exemplo: 10 `div` 3 resulta em 3.
      - `mod`: Resto da divisão inteira. Exemplo: 10 `mod` 3 resulta em 1.
    
    2. Operadores de Comparação:
      - `==`: Verifica se dois valores são iguais. Exemplo: 5 == 5 resulta em True.
      - `/=`: Verifica se dois valores são diferentes. Exemplo: 5 /= 4 resulta em True.
      - `<`: Verifica se o primeiro valor é menor que o segundo. Exemplo: 3 < 5 resulta em True.
      - `>`: Verifica se o primeiro valor é maior que o segundo. Exemplo: 7 > 2 resulta em True.
      - `<=`: Verifica se o primeiro valor é menor ou igual ao segundo. Exemplo: 4 <= 4 resulta em True.
      - `>=`: Verifica se o primeiro valor é maior ou igual ao segundo. Exemplo: 5 >= 3 resulta em True.
    
    3. Operadores Lógicos:
      - `&&`: Operador E lógico. Retorna True se ambos os operandos forem True. Exemplo: True && False resulta em False.
      - `||`: Operador OU lógico. Retorna True se pelo menos um dos operandos for True. Exemplo: True || False resulta em True.
      - `not`: Operador de negação lógica. Inverte o valor lógico de um booleano. Exemplo: not True resulta em False.
    
    4. Operadores de Concatenacão:
      - `++`: Concatena duas listas. Exemplo: [1, 2] ++ [3, 4] resulta em [1, 2, 3, 4].
      - `:`: Cons (constructor) adiciona um elemento na frente de uma lista. Exemplo: 1 : [2, 3] resulta em [1, 2, 3].
    
    Operadores Personalizados:
    - Em Haskell, você pode definir seus próprios operadores. Eles devem começar com um símbolo especial (como `+`, `*`, `!`, etc.) e são definidos como funções.
      Exemplo:
      - Definindo um operador `**` para elevar um número ao quadrado:
      
        x ** y = x ^ y

    Operadores de Seção:
    - Operadores em Haskell podem ser "seccionados", onde um dos operandos é fixado e o outro é aplicado posteriormente.
      Exemplo:
      - `(2*)` é uma função que multiplica qualquer número por 2. `(2*) 5` resulta em 10.
      - `(*3)` é uma função que multiplica qualquer número por 3. `(*3) 4` resulta em 12.

  Declaração de Variáveis em Haskell:

    1. Imutabilidade:
      - Em Haskell, as "variáveis" são, na verdade, constantes imutáveis. Uma vez que um valor é atribuído a uma variável, 
        ele não pode ser alterado. Isso significa que você não "muda" o valor de uma variável, como faria em linguagens imperativas.
    
    2. Bindings:
      - A declaração de uma variável em Haskell é chamada de "binding". Um binding associa um nome a um valor.
      - A sintaxe básica para declarar uma variável é:
        
        nomeDaVariavel = valor

      - Exemplo:
        
        x = 10
        nome = "Marcos"
    
    3. Tipagem:
      - Haskell possui inferência de tipos, então você geralmente não precisa declarar explicitamente o tipo de uma variável.
        O compilador deduz o tipo automaticamente.
      - No entanto, você pode especificar o tipo explicitamente se desejar, usando a seguinte sintaxe:
        
        nomeDaVariavel :: Tipo
        nomeDaVariavel = valor
        
      - Exemplo:
        
        idade :: Int
        idade = 24

        piValue :: Float
        piValue = 3.14

    4. Pattern Matching:
      - Você pode usar pattern matching ao declarar variáveis, o que permite a decomposição de estruturas de dados em seus componentes.
      - Exemplo:
        
        (a, b) = (1, 2)  -- Aqui, a é 1 e b é 2.

      - Outro exemplo com listas:
        
        (x:xs) = [1, 2, 3, 4]  -- Aqui, x é 1 e xs é [2, 3, 4].

    5. Lazy Evaluation:
      - As variáveis em Haskell são avaliadas de maneira "preguiçosa" (lazy), o que significa que o valor associado a uma variável só é 
        computado quando ele é realmente necessário. Isso permite a criação de estruturas de dados infinitas ou retardar cálculos até o último momento.

      - Exemplo de lazy evaluation:
        
        numeros = [1..]  -- Cria uma lista infinita de números começando em 1.
        primeiro = head numeros  -- "head" é avaliado imediatamente, mas o resto da lista não é gerado até ser necessário.
    
    6. Shadows:
      - Em Haskell, você pode "reutilizar" o nome de uma variável em um escopo diferente. Isso é chamado de "shadowing".
      - A nova variável "sombra" a antiga no novo escopo, mas fora desse escopo, a antiga variável permanece inalterada.

      - Exemplo de shadowing:
        
        x = 5
        f y = let x = 10 in x + y  -- Aqui, a variável x dentro da função f "sombra" a variável x global.

    7. Let Bindings:
      - Você pode declarar variáveis locais dentro de expressões usando `let` bindings.
      - A sintaxe é:
        
        let variavel = valor in expressão

      - Exemplo:
        
        let z = 7 in z + 3  -- Resultado: 10

    8. Where Clauses:
      - Uma alternativa ao `let` binding é o uso de `where` para declarar variáveis locais que são utilizadas em uma expressão.
      - A sintaxe é:
        
        expressão onde
        variavel = valor

      - Exemplo:
        
        somaQuadrados x y = quadradoX + quadradoY
          where
            quadradoX = x * x
            quadradoY = y * y
        
        -- `quadradoX` e `quadradoY` são variáveis locais acessíveis apenas na expressão `somaQuadrados`.

  Tipos de Dados em Haskell:

    1. Tipos Básicos:
      - `Int`: Representa números inteiros de precisão fixa. O intervalo depende da implementação, mas geralmente é -2^29 a 2^29-1.
        Exemplo: 
        idade :: Int
        idade = 24

      - `Integer`: Representa números inteiros de precisão arbitrária, o que significa que pode lidar com números muito grandes.
        Exemplo:
        grandeNumero :: Integer
        grandeNumero = 12345678901234567890

      - `Float`: Representa números de ponto flutuante com precisão simples (aproximadamente 7 dígitos de precisão).
        Exemplo:
        piValue :: Float
        piValue = 3.14

      - `Double`: Representa números de ponto flutuante com precisão dupla (aproximadamente 15 dígitos de precisão).
        Exemplo:
        precisoPi :: Double
        precisoPi = 3.141592653589793

      - `Bool`: Representa valores booleanos, que podem ser `True` ou `False`.
        Exemplo:
        isAdulto :: Bool
        isAdulto = True

      - `Char`: Representa um único caractere. Os caracteres são escritos entre aspas simples.
        Exemplo:
        inicial :: Char
        inicial = 'M'

      - `String`: Representa uma sequência de caracteres. `String` é simplesmente uma lista de `Char`.
        Exemplo:
        nome :: String
        nome = "Marcos"

    2. Listas:
      - Listas são coleções de elementos do mesmo tipo.
      - Exemplo:
        numeros :: [Int]
        numeros = [1, 2, 3, 4, 5]

      - Strings são listas de caracteres, como mencionado acima.

    3. Tuplas:
      - Tuplas são coleções de elementos que podem ser de tipos diferentes. O tamanho e os tipos são fixos.
      - Exemplo:
        pessoa :: (String, Int)
        pessoa = ("Marcos", 24)

      - Uma tupla pode conter mais de dois elementos:
        tripla :: (String, Int, Bool)
        tripla = ("Ana", 30, True)

    4. Maybe:
      - O tipo `Maybe` é usado para representar um valor que pode ou não estar presente. É útil para tratar casos onde pode ocorrer ausência de valor.
      - `Maybe` pode ser `Nothing` (ausência de valor) ou `Just valor` (valor presente).
      - Exemplo:
        idadePossivel :: Maybe Int
        idadePossivel = Just 24  -- valor presente

        idadeFaltando :: Maybe Int
        idadeFaltando = Nothing  -- valor ausente

    5. Either:
      - O tipo `Either` é usado para representar um valor que pode ser de dois tipos diferentes, normalmente para expressar sucesso ou erro.
      - `Either` pode ser `Left erro` (geralmente usado para erros) ou `Right valor` (geralmente usado para o valor de sucesso).
      - Exemplo:
        resultado :: Either String Int
        resultado = Right 42  -- Sucesso com valor 42

        erro :: Either String Int
        erro = Left "Erro: Divisão por zero"  -- Erro com uma mensagem

    6. Funções:
      - Em Haskell, funções também são tipos de dados. Uma função que recebe um `Int` e retorna um `Bool` tem o tipo `Int -> Bool`.
      - Exemplo:
        ehPar :: Int -> Bool
        ehPar n = n `mod` 2 == 0

    7. Tipos Algebricos:
      - Haskell permite a definição de novos tipos através de `data`. Tipos algebricos podem ser simples, com um único construtor, ou complexos, com múltiplos construtores.
      - Exemplo de tipo com um único construtor:
        data Cor = Vermelho | Verde | Azul
        
        minhaCor :: Cor
        minhaCor = Verde

      - Exemplo de tipo com múltiplos construtores e parâmetros:
        data Forma = Circulo Float | Retangulo Float Float
        
        area :: Forma -> Float
        area (Circulo r) = pi * r * r
        area (Retangulo l a) = l * a

    8. Tipos Polimórficos:
      - Haskell suporta tipos polimórficos, onde um tipo pode ser parametrizado com outros tipos.
      - Exemplo:
        data Caixa a = Vazia | Conteudo a
        
        minhaCaixa :: Caixa Int
        minhaCaixa = Conteudo 42
        
        minhaOutraCaixa :: Caixa String
        minhaOutraCaixa = Conteudo "Haskell"

    9. Tipos de Classe:
      - Haskell tem um sistema de classes de tipos, onde tipos podem pertencer a várias classes, como `Eq`, `Ord`, `Show`, etc.
      - `Eq`: Classe para tipos que podem ser comparados por igualdade (`==`, `/=`).
      - `Ord`: Classe para tipos que podem ser ordenados (`<`, `>`, `<=`, `>=`).
      - `Show`: Classe para tipos que podem ser convertidos para uma `String` (`show`).
      - `Read`: Classe para tipos que podem ser lidos de uma `String` (`read`).
      
      Exemplo de uso:
      show (read "123" :: Int)  -- Converte a string "123" para o número 123, e depois para a string "123" novamente.
-} 

imprimeSimbolo :: Int -> String -> String
imprimeSimbolo n s
    | n == 0 = ""
    | n > 0 = s ++ imprimeSimbolo(n-1) s

titulo :: String
titulo = "Bolsonaro"

cabecalho :: String
cabecalho = imprimeSimbolo 30 "*" ++ "\n" ++ imprimeSimbolo 30 "*" ++ titulo ++ "\n" ++ imprimeSimbolo 30 "*"