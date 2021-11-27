# PFL - Trabalho Prático 1

O trabalho prático teve como objetivo implementar as seguintes funções:
* **fibRec** - Função que se calcula o enésimo número de Fibonacci de forma recursiva;
     - *Casos de Teste* 
        > fibRec 0 = 0
        > fibRec 1 = 1
        > fibRec 6 = 8
        > fibRec 10 = 55

     
     
* **fibLista** - Função que gera uma lista de números de Fibonacci de tamanho n (função auxiliar fibListaux) e retorna o elemento de ordem n;
    - *Casos de Teste* 
        > fibLista 0 = 0
        > fibLista 1 = 1
        > fibLista 4 = 3
        > fibLista 5 = 5 
     
     
* **fibListaInfinita** - Função que gera uma lista infitita de números de Fibonacci (com a função auxiliar fibListaInfinita) e retorna o elemento de ordem n;
    - *Casos de Teste* : 
        > fibListaInfinita 0 = 0
        > fibListaInfinita 1 = 1
        > fibListaInfinita 6 = 8
        > fibListaInfinita 10 = 55 
 
* **scanner** - Função que converte uma string em BigNumber;
     - *Estratégia de Implementação* - Transforma a string em Int com o "read" e separa cada número adicionando-o na lista, formando assim um BN com o "map".
     - *Casos de Teste* : 
         - Números positivos
            > scanner "2" = [2]
            > scanner "45667" = [4,5,6,6,7]
        - Números negativos
            > scanner "-56789" = [-5,-6,-7,-8,-9]
        - Zero
            > scanner "0" = [0]
        
     
* **output** -  Função que converte um BigNumber em string;
     - *Estratégia de Implementação* - Transforma cada número do BN em string dentro de uma lista com a função "(map show (b)" e depois concatena-os em uma só string com o "concat"
     - *Casos de Teste* : 
        - Números positivos
            > output [5,6] = "56"     
            > output [2] = "2"
        - Números negativos
            > output [-5,-6,-7,-8,-9] = "-56789"
            > output [-2,-5] = "-25" 
        - Zero
            > output [0] = "0"
* **somaBN** - Função que soma dois BigNumbers;
     - *Estratégia de Implementação* - Fazer o "reverse" dos dois BN a somar para que ao somar seja facilitado o processo de carry. Quando se obter o resultado final é feito o "reverse" de novo. Foram feitas várias funções auxiliares:
         - "fillwithzero" - esta função adiciona zeros à esquerda de um BN e é utilizada caso se queira somar BN's de tamanhos diferentes.
        - "clear left zeros" - limpa todos os zeros à esquerda do BN que existam no resultado final
        - "carrypos" e "carryneg" - caso a soma de um número do BN dê >9 ou <-9 estas funções somam 1 ao elemento da frente.
     - *Casos de Teste* : 
         - Dois números positivos
            > somaBN [2,4,5] [7,0,9,6] = [7,3,4,1]      
            > somaBN [3,9,5] [7,0,9] = [1,1,0,4]
        - Dois números negativos
            > somaBN [-3,-9,-5] [-7,-0,-9] = [-1,-1,0,-4]
            > somaBN [-6,-0] [-7]  = [-6,-7] 
        - Um número positivo e outro negativo
            > somaBN [-6,-0] [7]  = [-5,-3]
            > somaBN [-6,-0] [6,0] = [0]
            > somaBN [-5] [8,6,3,2] = [8,6,2,7]
        - Zero
            > somaBN [-5] [0] = [-5]

* **subBN** - Função que subtrai dois BigNumbers;
     - *Estratégia de Implementação* - Implementação similar à somaBN. Faz-se "reverse" e caso necessário o "fillwithzero". Faz-se a subtração de um elemento de um BN por outro elemento do BN a subtrair com o "zip". Caso seja necessário é efetuado o carry ou o "changeSignal" que muda o sinal a todos os elementos do BN final.
     - *Casos de Teste* : 
         - Dois números positivos
            > subBN [4,7] [3,6,3] = [-3,-1,-6]      
            > subBN [1,0,0] [5,0] = [5,0]
        - Dois números negativos
            > subBN [-4,-7] [-3,-6,-3] = [3,1,6]
            > subBN [-7,-2] [-8,-7,-2] = [8,0,0] 
        - Um número positivo e outro negativo
            > subBN [-7,-2] [8,7,2] = [-9,-4,-4]
            > subBN [-8] [8] = [-1,-6]
            
        - Zero
            > subBN [8] [0] = [8]
            > subBN [0] [3,4,5] = [-3,-4,-5]

* **mulBN** - Função que multiplica dois BigNumbers;
     - *Estratégia de Implementação* - Para multiplicar dois BN resolvemos multiplicar cada elemento do 2ºBN por todos os elementos do 1ºBN. No fim somamos o resultado de cada multiplicação com o somaBN até obter o resultado final. Foram usadas funções auxiliares para que preenchessem os BNs com zeros até ficarem todos do mesmo tamanho, para fazer "carry" e para verificar se o BN é negativo ou positivo. 
     - *Casos de Teste* : 
         - Dois números positivos
            > mulBN [5,3] [9,5] = [5,0,3,5]
            > mulBN [7,4,2] [9] = [6,6,7,8]
        - Dois números negativos
            > mulBN [-5,-3] [-9,-5] = [5,0,3,5]
        - Um número positivo e outro negativo
            > mulBN [-7] [3,4,5] = [-2,-4,-1,-5]
            > 
        - Zero
            > mulBN [0] [3,4,5] = [0]

* **divBN** - Função que divide dois BigNumbers e retorna um par “(quociente, resto)”;
    - *Estratégia de Implementação* - Começamos por ver que BN é o maior. Para verificar isso fizemos funções auxiliares para comparar os dois BNs. Para a divisão fizemos funções recursivas em que retira do divisor os primeiros n números em que n = tamanho do dividendo. Caso esse número retirado fosse menor que o dividendo iamos buscar o próximo elemento a ser retirado ao divisor. Seguidamente utilizamos uma função em que multiplicava o dividendo por q+1 enquanto este fosse menor que a parte do divisor que estamos a usar. Quando parasse guardava o valor de q como o primeiro valor do quociente. Subtrai-se com subBN o valor da parte do divisor pelo dividendo já multiplicado por q e obtem-se um resto. Caso ainda houvesse mais elementos no divisor a ser utilizados, juntariam-se ao resto para fazer o processo todo de novo. Caso já se tivessem usado todos os elementos do divisor a função retornaria o par do resultado do quociente com o resto.
     - *Casos de Teste* : 
         - Dois números positivos
            > divBN [1,5] [2] = ([7],[1]) 
            > divBN [3,4,6,8] [6,5] = ([5,3],[2,3])
        - Zero
            > divBN [-3,-4] [0] = ([0],[0])

* **fibRecBN** - Função que se calcula o enésimo número de Fibonacci de forma recursiva;
    - *Casos de Teste* : 
        > fibRecBN [8] = [2,1]
        > fibRecBN [1,0] = [5,5]
        > fibRecBN [0] = [0]
        > fibRecBN [1] = [1]
     
* **fibListaBN** - Função que gera uma lista de números de Fibonacci de tamanho n (função auxiliar fibListaux) e retorna o elemento de ordem n;
    - *Casos de Teste* : 
        > fibListaBN [0] = [0]
        > fibListaBN [1]  = [1]
        > fibListaBN [1,0] = [5,5]
     
* **fibListaInfinitaBN** - Função que gera uma lista infitita de números de Fibonacci (com a função auxiliar fibListaInfinita) e retorna o elemento de ordem n;
     - *Casos de Teste* : 
     
* **safeDivBN** - Função que divide dois BigNumbers que é capaz de detetar divisões por zero em compile-time e retorna um par “(quociente, resto)”;
     - *Casos de Teste* :
        - Dois números positivos
            > safeDivBN [3,4,6,8] [6,5] = Just ([5,3],[2,3])
        - Zero
            > safeDivBN [-3,-4] [0] = Nothing


***4. Compare as resoluções das alíneas 1 e 3 com tipos (Int -> Int), (Integer ->Integer) e (BigNumber -> BigNumber), comparando a sua aplicação a números grandes e verificando qual o maior número que cada uma aceita como argumento.***
O maior número que o *Int* aceita como  argumento é 9223372036854775807, no entanto o *Integer* são inteiros que têm uma precisão arbitrária que pode ter um intervalo tão grande enquanto existe memória, por isso não existe o maior número para o Integer. Quanto aos *BigNumbers* tambem não existe limite podendo ser uma lista infinita.  

Na tabela abaixo conseguimos ver a comparação entre os Ints e os BigNumbers.



| Função               | Tipo       | NºFibonacci| Tempo    | 
| --------             | --------   | --------   | -------- |
| fibRec               | Int        | 2          | 0.02s    | 
| fibRecBN             | BigNumber  | [2]        | 0.02s    |
| fibRec               | Int        | 30         | 5.27s    |
| fibRecBN             | BigNumber  |[3,0]       | 22.02s  
| fibLista             | Int        | 2          | 0.00s    |
| fibListaBN           | BigNumber  | [2]        | 0.02s    |
| fibLista             | Int        | 30         | 4.09s    |
| fibListaBN           | BigNumber  | [3,0]      | 27.98s   |
| fibListaInfinita     | Int        | 30         | 0.01s    |
| fibListaInfinitaBN   | BigNumber  | ?          | ?        |


Resolvemos comparar com os números 2 e 30 pois conseguimos ver bem a diferença entre estes dois valores. Quando calculamos o resultado do Número de Fibonacci 2 com as funções tanto de BigNumber ou Int o resultado varia entre 0.00s e 0.02s. No entanto reparamos que quando usamos um número maior, neste caso usamos o 30, os resultados variam muito. Na função fibRec com Int demora 5.27s e com BigNumber demorou 22.02s. Na função fibLista notamos uma diferença similar de tempo. O Int demorou 4.09s e o BigNumber 27.98s. Uma função que nos surpreendeu na velocidade foi o fibListaInfita pois demorou apenas 0.01s a calcular o número de Fibonacci 30, testamos com números maiores e a eficiencia continuou igualmente rápida.





**Realizado por G4_08:**
* *Marina Dias* - up201806787
* *Pedro Vale* - up201806083
