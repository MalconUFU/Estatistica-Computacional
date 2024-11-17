#PRIMEIRA LISTA DE EXERCÍCIOS
#ESTATÍSTICA COMPUTACIONAL
#Malcon Rezende Rodrigues - 12211BCC034

#Exercício 1. Crie os seguintes vetores:

a <- c(10:30)
b <- c(30:10)
c <- c(10:30, 29:10)

#---------------------------------------------------------------
#Exercício 2. Use a função help do R para descobrir o funcionamento das funções rep e seq. Em seguida, utilize estas funções para resolver os seguintes itens:

#(a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências do número 2.

d <- rep(seq(from = 2, to = 8, by = 2), 10)

#(b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8.

e <- rep(seq(2, 8, by = 2), length.out = 41)

#---------------------------------------------------------------
#Exercício 3. Utilize a estrutura de vetores do R para realizar as seguintes somas:

#(a) SOMATÓRIO DE 20 A 30 ((n**2)+(4n))
sum((c(20:30)**2) + 4*c(20:30))

#(b) SOMATÓRIO DE 10 A 20 ((3**n)/(n)) + ((2**n)/(n**2))
sum(((3**c(10:20))/c(10:20)) + ((2**c(10:20))/(c(10:20)**2)))

#---------------------------------------------------------------
#Exercício 4. Numa urna há bolas idênticas numeradas de 1 até 100. Serão extraídas 40 bolas com reposição desta urna. Simule este experimento e guarde o resultado dos sorteios em um vetor.

bolas <- sample(x = 100, size = 40, replace = TRUE, prob = NULL)
 
#(a) Quantas bolas pares foram sorteadas?

sum(bolas %% 2 == 0)

#(b) Quantas bolas maiores do que 70 foram sorteadas?

sum(bolas > 70)

#(c) Em quais retiradas(posições) foram sorteadas as bolas ímpares?

posImpar <- c()
i <- 1
for(j in 1:40){
  
    if(bolas[j] %% 2 != 0){
      
      posImpar[i] <- j
      i <- i + 1
      
    }
  
}
posImpar

#---------------------------------------------------------------
#Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um dado até que o número 4 seja obtido pela segunda vez. A função deverá retornar o número de lançamentos que foram necessários até o 4 ser obtido pela segunda vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7

aparece_numero <- function(n, v){
  
  num <- n
  vezes <- v
  lancamentos <- 0
  i <- 0
  
  while(i != vezes){
    
    dado <- sample(x = 6, size = 1, replace = TRUE, prob = NULL)
    lancamentos <- lancamentos + 1
    
    if(dado == num){
      
      i <- i + 1
      
    }
    
  }
 
  return (lancamentos)
}

#---------------------------------------------------------------
#Exercício 6. Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada replicação, guarde o número de lançamentos num vetor chamado quantidades. Por fim, calcule a média de quantidades. Interprete o resultado obtido.

aparece_numero_mil <- function(n, v){
  
  num <- n
  vezes <- v
  quantidades <- c()
  
  for(k in 1:10000){
    
    lancamentos <- 0
    i <- 0
  
    while(i != vezes){
    
      dado <- sample(x = 6, size = 1, replace = TRUE, prob = NULL)
      lancamentos <- lancamentos + 1
    
      if(dado == num){
      
        i <- i + 1
      
      }
    
    }
    
    quantidades[k] <- lancamentos
  
  }
  
  return (quantidades)
}

quantidades <- aparece_numero_mil(3,3)
mean(quantidades)

#Interpretação do resultado: Para a chamada aparece_numero_mil(3,3), obtivemos o número 18.0549, que significa o número médio de lançamentos necessário para que se obtenha o número três, três vezes; nesse caso, foram necessários em média 18 lançamentos.

#---------------------------------------------------------------
#Exercício 7. Os dois primeiros termos da sequência de Fibonacci são iguais a 1. Os termos subsequentes da sequência são encontrados somando os dois termos imediatamente anteriores. Escreva uma função com parâmetro de entrada n chamada fibonacci que retornará os primeiros n termos da sequência de Fibonacci para qualquer 𝑛 ≥ 3. Exemplo: fibonacci 10 -> 1 1 2 3 5 8 13 21 34 55

fibonacci <- function(n){
  
  fib <- c()
  i <- 1
  s <- n
  
  while(i <= s){
    
    if(i <= 2){
      
      fib[i] = 1
      
    }
    else{
      
      fib[i] = fib[i-1] + fib[i-2]
      
    }
    
    i <- i + 1
  }
  
  return(fib)
  
}

#---------------------------------------------------------------
#Exercício 8. Michael Scott é gerente regional da empresa Dunder Mufflin. Para as festividades de fim de ano, Michael propôs aos funcionários Dwight Schrute, Jim Halpert, Kevin Malone e Creed Bratton a realização de um amigo oculto entre eles. Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa sortear ela mesma (Michael tira Michael, por exemplo). Simule o sorteio do amigo oculto. Se ele deu certo, atribua o valor 1; caso contrário, atribua o valor 0 (zero). Em seguida, replique este experimento cem mil vezes e calcule a proporção de vezes que o amigo oculto deu errado.

i <- 1
sorteios <- c()

while(i < 10000){
  
  pessoa1 <- sample(x = c("Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton"), size = 1, replace = FALSE)
  
  pessoa2 <- sample(x = c("Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton"), size = 1, replace = FALSE)
  
  if(pessoa1 == pessoa2){
    
    sorteios[i] = 0
    
  }
  else{
    
    sorteios[i] = 1
    
  }
  
  i <- i + 1
  
}

proporcao.erro <- 1 - mean(sorteios)

#---------------------------------------------------------------
#Exercício 9. No jogo de Craps dois dados são lançados: • se a soma for 7 ou 11, você ganha o jogo; • se a soma for 2,3 ou 12, você perde o jogo; • caso contrário, os dois dados são rolados novamente até obter-se 7 (você perde) ou até obter-se a soma inicial (você ganha). Simule uma partida do jogo de Craps. Em seguida, replique este experimento 100 mil vezes; para cada experimento, atribua 1 para uma vitória e zero para uma derrota. Calcule a proporção de vezes que você ganhou o jogo. Exemplo: as seguintes sequências (cada entrada é a soma dos dois dados) resultam em vitória: (7), (11), (5, 4, 5), (4, 5, 6, 12, 4); as seguintes sequências resultam em derrota: (2), (4, 11, 7), (8, 5, 2, 3, 9, 7).

#SIMULAÇÃO PARTIDA DE CRAPS

resultado <- c()

for(j in 1:10000){
  
  primeira.jogada <- 0
  i <- TRUE

  while(i){

    dado1 <- sample(x = 1:6, size = 1, replace = TRUE)
    dado2 <- sample(x = 1:6, size = 1, replace = TRUE)
    soma <- dado1 + dado2
  
    if(primeira.jogada == 0){

      if(soma == 7 || soma == 11){
      
        resultado[j] <- 1
        i <- FALSE
      
      }
  
      if(soma == 2 || soma == 3 || soma == 12){
      
        resultado[j] <- 0
        i <- FALSE
      
      }
  
      else{
  
        point <- soma
        primeira.jogada <- 1
  
      }
    
    }
  
      else{
      
        if(soma == 7){
        
          resultado[j] <- 0
          i <- FALSE
      
        }
      
        if(soma == point){
        
          resultado[j] <- 1
          i <- FALSE
        
        }
      
      }
  }
  
  j <- j + 1
  
}

mean(resultado)

#---------------------------------------------------------------
#Exercício 10. Luke Skywalker realizará o seguinte passeio aleatório na reta: a reta do passeio é formada pelos números inteiros de zero até 𝑁; Luke está em um ponto 𝐿 que é maior do que zero e menor do que 𝑁; Luke lança uma moeda honesta; se sair coroa, ele dá um passo para a esquerda (e termina na posição 𝐿 − 1 da reta); se sair cara, ele dá um passo para a direita (e termina na posição 𝐿 + 1 da reta). Luke continuará a lançar a moeda e se deslocará até que ele chegue em sua casa (e lá ele vai dormir e o passeio acaba) ou até que ele chegue (caia) no precipício (e, óbvio, o passeio também acaba nesse caso).

#(a) Para 𝑁 = 20, crie uma função cuja entrada seja 𝐿 (um número maior do que zero e menor do que 20) e que retorne 1 se Luke terminou um passeio em sua casa ou retorne zero se Luke caiu no precipício.

passeio.de.luke <- function(l){
  
  n <- 20
  
  if(l <= 0 || l >= n){
    
    return("Luke está perdidinho")
    
  }
  else{
  
    while(l != 0 && l != n){
  
      moeda <- sample(x = c("Cara", "Coroa"), size = 1)
  
      if(moeda == "Coroa"){
      
        l <- l - 1
      
      }
      else{
      
      l <- l + 1
      
      }
    }
    
    if(l == n) return (1)
    else return (0)
    
  }
}

#(b) Crie uma função cuja entrada seja 𝐿; esta função deverá replicar o passeio da letra (a) 10 mil vezes e retornar a proporção de vezes que Luke chegou em sua casa. Sugestão: crie um vetor que, para cada replicação, guardará o resultado de um passeio; cada entrada deste vetor será zero ou 1; zero se Luke caiu no precipício e 1 se Luke chegou em casa.

passeio.de.luke.10000 <- function(l){
  
  passeios <- numeric(01000)
  
  for(k in 1:10000){
    
    passeios[k] <- passeio.de.luke(l)
    
  }
  
  return(mean(passeios))
  
}

#(c) Use a função criada em (b) para 𝐿 = 1, 2, … , 19 e, em seguida, use esses valores para plotar um gráfico de 𝑥 = 1 ∶ 19 por 𝑦, em que 𝑦 são as proporções retornadas pela função criada em (b) para cada 𝑥.

library(ggplot2)

grafico.passeios <- numeric(19)

for(k in 1:19){
  
  grafico.passeios[k] <- passeio.de.luke.10000(k)
  
}

dados <- data.frame(Posicao = 1:19, Proporcao = grafico.passeios)

ggplot(data = dados, mapping = aes(x = Posicao, y = Proporcao)) + geom_line() + geom_point() + theme_minimal() + labs(title = "Passeio de Luke", x = "Posição", y = "Proporção de Chegada")

#---------------------------------------------------------------
#Exercício 11. Link desperta após um sono de cem anos e encontra o reino de Hyrule em ruínas, necessitando recuperar suas memórias e enfrentar o mal desencadeado por Calamity Ganon. Desprovido de suas lembranças, Link adota um método peculiar para explorar o reino: lançando um dado de 4 lados. As faces deste dado são rotuladas como 𝐿, 𝑅, 𝑈 e 𝐷. A direção de cada passo que Link dá é determinada pelo resultado do dado que ele lança: se sair 𝐿 ele dá um passo para a esquerda; se sair 𝑅, ele dá um passo para a direita; se sair 𝑈, ele dá um passo para cima; se sair 𝐷, ele dá um passo para baixo. Suponha que Link comece sua jornada no ponto (0, 0) e que os quatro primeiros lançamentos do dado foram 𝑅, 𝑈, 𝐿 e 𝐿. Portanto, a sequência de passos de Link, neste caso, é: (0, 0) → (1, 0) → (1, 1) → (0, 1) → (−1, 1). As Figuras 1, 2 e 3 apresentam, respectivamente, Link no início de sua jornada, após o primeiro lançamento do dado e após o segundo lançamento do dado; a Figura 4 apresenta o percurso total feito por Link após os quatro lançamentos.

#a) Simule um passeio de 8 passos com Link começando na origem. A saída desta simulação deve apresentar o ponto do plano em que Link está após os 8 passos.

num <- 8
i <- 0

x <- 0
y <- 0

while(i != num){
  
  passo <- sample(x = c('L', 'R', 'U', 'D'), size = 1)
  
  if(passo == 'L'){
    
    x <- x - 1
    
  }
  else if(passo == 'R'){
    
    x <- x + 1
    
  }
  else if(passo == 'U'){
    
    y <- y + 1
    
  }
  else{
    
    y <- y - 1
    
  }
  
  i <- i + 1
  
}

cat("(", x, ", ", y, ")", sep = "")

#b) Agora replique dez mil vezes o experimento de (a) e determine a proporção de vezes em que Link retornou para a origem depois de 8 passos. Comente o que esta proporção significa.

num <- 8
passos <- c()

for(k in 1:10000){
  
  i <- 0
  x <- 0
  y <- 0
  
  while(i != num){
    
    passo <- sample(x = c('L', 'R', 'U', 'D'), size = 1)
    
    if(passo == 'L'){
      
      x <- x - 1
      
    }
    else if(passo == 'R'){
      
      x <- x + 1
      
    }
    else if(passo == 'U'){
      
      y <- y + 1
      
    }
    else{
      
      y <- y - 1
      
    }
    
    i <- i + 1
    
  }
  
  if(x == 0 && y == 0){
    
    passos[k] <- 1 #importante
    
  }
  else{
    
    passos[k] <- 0
    
  }
  
}

mean(passos)

#Em um dos testes, o valor retornado foi 0,024, ou seja, em apenas 2,4% das vezes, Link consegue retornar para a origem em oito passos. Para voltar à origem, é necessário que L = R e U = D. Isso, em média, ocorre em 2,4% das vezes.

#Escreva uma função em que o usuário entra com um número 𝑁: se o número 𝑁 for ímpar, então a função retorna: “impossível retornar a origem depois de um número ímpar de passos” (quando estiver estudando esta questão, se convença que ’isso é verdade, é impossível regressar à origem em um número passos ímpar); se o número 𝑁 for par, então a função retorna uma frase informando a proporção de vezes em que Link retornou a origem depois de 𝑁 passos ao se realizar o experimento dez mil vezes.

link <- function(n){
  
  if(n %% 2 != 0){
    
    return("Impossível retornar a origem depois de um número ímpar de passos")
    
  }
    
  passos <- numeric(10000)
  k <- 0
    
  for(k in 1:10000){
      
    i <- 0
    x <- 0
    y <- 0
      
     while(i != n){
        
      passo <- sample(x = c('L', 'R', 'U', 'D'), size = 1)
        
      if(passo == 'L'){
          
        x <- x - 1
          
      }
      else if(passo == 'R'){
          
         x <- x + 1
          
      }
      else if(passo == 'U'){
          
        y <- y + 1
          
      }
      else{
          
        y <- y - 1
          
      }
        
      i <- i + 1
        
      }
      
      if(x == 0 && y == 0){
        
        passos[k] <- 1
        
      }
      else{
        
        passos[k] <- 0
        
      }
      
    }
  
  return(cat("Em", n,"passos, em média, Link retornou", mean(passos), "das vezes"))
  
}

link(8)

#---------------------------------------------------------------
#Exercício 12. Considere o seguinte jogo: Steven e Garnit escolherão, cada um, uma sequência de tamanho 3 em que cada entrada da sequência é cara ou coroa; logo em seguida, uma moeda será lançada três vezes; se aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba; caso não apareça a sequência de nenhum deles, a moeda é lançada pela quarta vez e os três últimos lançamentos são analisados; se nestes três últimos lançamentos aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba. Se isto não acontecer, a moeda é lançada pela quinta vez e os três últimos resultados são analisados; se aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba. Este processo é realizado até que apareça a sequência que um dos dois escolheu; se aparecer primeiro a sequência de Steven, ele ganha; se aparecer primeiro a sequência de Garnit, ela vence. Convencione que cara seja 1 e que coroa seja zero. Supondo que Steven escolheu a sequência (0, 1, 0) e que Garnit escolheu a sequência (0, 0, 1), simule uma partida deste jogo. A simulação deve retornar steven caso Steven tenha vencido ou deve retornar garnit caso contrário. Replique o experimento 10 mil vezes e calcule a média de vitórias de Garnit. Comente o resultado obtido. (6 pontos) 

#Observação: Suponha que os três primeiros lançamentos foram (1,0,0). Logo, ninguém ganhou e a moeda é lançada pela quarta vez. Suponha que o quarto lançamento foi 0; logo os três últimos lançamentos foram (0,0,0) e ninguém ganhou. Na quinta vez saiu 1 e, portanto, os três últimos lançamentos foram (0,0,1) e o jogo acaba com vitória de Garnit. As sequências (0, 1, 0), (1, 0, 1, 0) e (1, 1, 0, 1, 0) fazem Steven vitorioso; as sequências (0, 0, 1), (0, 0, 0, 1) e (1, 0, 0, 0, 1) fazem Garnit vitoriosa.

#SIMULAÇÃO

garnit <- c(0, 0, 1)
steven <- c(0, 1, 0)
moeda <- numeric(3)
primeira_rodada <- 0
vitoria <- 0

while(vitoria == 0){
  
  if(primeira_rodada == 0){
  
    for(k in 1:3){
    
      moeda[k] <- sample(x = c(0,1), size = 1)
    
    }
    
    primeira_rodada <- 1
    
  }
  else{
    
    moeda[1] <- moeda[2]
    moeda[2] <- moeda[3]
    moeda[3] <- sample(x = c(0,1), size = 1)
    
  }
  
  if(all(moeda == garnit)){
    
    print("garnit venceu")
    vitoria <- 1
    
  }
  if(all(moeda == steven)){
    
    print("steven venceu")
    vitoria <- 1
    
  }
  
}

#FUNÇÃO PARA EXPERIMENTO DE 10K

stevenuniverse <- function(n){
  
  garnit <- c(0, 0, 1)
  steven <- c(0, 1, 0)
  vitorias_garnit <- numeric(n)
  
  for(q in 1:n){

    moeda <- numeric(3)
    primeira_rodada <- 0
    vitoria <- 0
    
    while(vitoria == 0){
      
      if(primeira_rodada == 0){
        
        for(k in 1:3){
          
          moeda[k] <- sample(x = c(0,1), size = 1)
          
        }
        
        primeira_rodada <- 1
        
      }
      else{
        
        moeda[1] <- moeda[2]
        moeda[2] <- moeda[3]
        moeda[3] <- sample(x = c(0,1), size = 1)
        
      }
      
      if(all(moeda == garnit)){
        
        vitorias_garnit[q] <- 1
        vitoria <- 1
        
      }
      if(all(moeda == steven)){
        
        vitoria <- 1
        
      }
      
    }
    
  }
  
  return(mean(vitorias_garnit))
  
}

stevenuniverse(10000)

#A FUNÇÃO STEVENUNIVERSE(10000) RETORNA 0.6660, OU SEJA, A ESCOLHA DE SEQUÊNCIA DA GARNIT VENCE 66% DAS VEZES. ISSO OCORRE POR QUÊ A SEQUÊNCIA DE GARNIT TEM UMA VANTAGEM PROBABILÍSTICA SOBRE A SEQUÊNCIA DE STEVEN, ISSO É CHAMADO DE PARADOXO DE PENNY. É COMO DIZER QUE A SEQUÊNCIA DE GARNIT (0,0,1) APARECE "PRIMEIRO" QUE A DE STEVEN (0, 1, 0)

#---------------------------------------------------------------
#Exercício 13. Harold Frederick Shipman (Nottingham, 14 de janeiro de 1946 — Wakefield, 13 de janeiro de 2004), conhecido como “Doutor Morte”, foi um médico e assassino em série britânico condenado pela morte de muitos pacientes entre as décadas de 1970 e 1990. Dr. Shipman é, talvez, o assassino em série mais prolífico da História Moderna. O arquivo dados.txt contém informações sobre o sexo, a idade, o local da morte (casa do paciente; hospital; casa de repouso) e o ano da morte das vítimas de Shipman. Antes de responder as questões abaixo, abra o arquivo dados.txt e compreenda sua estrutura. Importe o arquivo para o R e utilize-o para responder os seguintes itens.

dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")
str(dados)


#(a) Escolha um gráfico apropriado para representar as frequências das categorias da variável sexo. Comente os resultados encontrados.

library(ggplot2)

dados$Genero <- as.factor(dados$Genero)

ggplot(data = dados, mapping = aes(x = dados$Genero, fill = dados$Genero)) + geom_bar() + scale_fill_manual(values = c("Men" = "#ffffb3", "Women" = "#bebada"), labels = c("Men" = "Masculino", "Women" = "Feminino")) + labs(title = "Assassinatos de Doutor Morte por Gênero", x = "Sexo", y = "Mortes", fill = "Sexo") + scale_x_discrete(labels = c("Men" = "Masculino", "Women" = "Feminino")) +  theme_minimal()

#Representanto a tabela por meio de um gráfico de barras, que é ideial para categoria de gênero (possuí um número pequeno de níveis), podemos concluir que o número de vítimas do gênero feminino é consideravelmente maior que as do gênero masculino. São mais de 170 vítimas do sexo feminino para menos de 50 do sexo masculino, indicando que grande parte das vítimas do Doutor Morte eram mulheres.

#----------------------------------

#(b) Apresente o histograma da variável idade em 8 (argumento bins na geometria do histograma) intervalos. Comente os resultados obtidos. Analise este gráfico para cada gênero.

ggplot(data = dados, mapping = aes(x = dados$Idade, fill = dados$Genero)) + scale_fill_manual(values = c("Men" = "#99d8c9", "Women" = "#2ca25f"), labels = c("Men" = "Masculino", "Women" = "Feminino")) + geom_histogram(bins = 8) + facet_wrap(dados$Genero, labeller = as_labeller(c("Men" = "Masculino", "Women" = "Feminino"))) + labs(title = "Assassinatos de Doutor Morte por Idade", x = "Idade", y = "Mortes", fill = "Sexo") +  theme_minimal()

#Representando a tabela por meio de um histograma com oito intervalos de idade, observamos que a maioria das vítimas do Doutor Morte eram idosos, com uma concentração de mortes entre 60 e 80 anos. Além disso, ao utilizar a função facet_wrap para separar o gráfico por gênero, notamos que o principal alvo do Doutor Morte eram mulheres idosas.

#----------------------------------

#(c) Apresente o boxplot da variável idade. Comente os resultados obtidos.

ggplot(data = dados, mapping = aes(x = dados$Genero, y = dados$Idade, fill = dados$Genero)) + scale_fill_manual(values = c("Men" = "#99d8c9", "Women" = "#2ca25f"), labels = c("Men" = "Masculino", "Women" = "Feminino")) + geom_boxplot() + labs(title = "Boxplot das Idades das Vítimas por Gênero", x = "Gênero", y = "Idade", fill = "Sexo") + scale_x_discrete(labels = c("Men" = "Masculino", "Women" = "Feminino")) + theme_minimal()

#Por meio do boxplot, podemos observar que Doutor Morte tinha como alvo principal pessoas idosas na faixa de 70 a 85 anos, com uma leve assimetira nas idades das vítimas femininas. Há outliers que representam vítimas mais jovens, tanto masculinas como femininas, que indicam que havia casos específicos em que o Doutor Morte assassinava vítimas mais jovens.

#Sobre o boxplot feminino: A mediana das idades está um pouco abaixo de 80 anos, significando que metade das vítimas, 50%, tinham menos que 80 anos. A mediana não é centralizada, indicando que havia grande variação nas idades das vítimas. O intervalo interquartil (caixa) é pequeno, o que sugere que a maior parte das vítimas femininas estava concentrada na faixa de 70 a 80 anos. O boxplot feminino apresenta outliers abaixo de 55 anos, indicando casos isolados em que Doutor Morte assassinou vítimas mais jovens.

#Sobre o boxplot masculino: A mediana das idades dos homens está próxima dos 80 anos, centralizada, indicando uma distribuição simétricas das idades das vítimas. O intervalo interquartil, semelhante ao das vítimas femininas, indica que as idades das vítimas masculinas está entre 70 e 85 anos. Há também outliers nas vítimas masculinas, localizando-se abaixo dos 50 anos, indicando que o Doutor Morte assassinou homens com idade em torno dos 40 anos.

#----------------------------------

#(d) Apresente um gráfico para representar o local da morte. Comente os resultados obtidos.

dados$LocalDaMorte <- as.factor(dados$LocalDaMorte)

ggplot(data = dados, mapping = aes(x = dados$LocalDaMorte, fill = dados$LocalDaMorte)) + scale_fill_manual(values = c("Hospital" = "#fc9272", "Nursing home" = "#fee0d2", "Own home" = "#de2d26"), labels = c(title = "Local da Morte", "Hospital" = "Hospital", "Nursing home" = "Lar de Idosos", "Own home" = "Em casa")) + geom_bar() + labs(title = "Locais de morte das vítimas do Doutor Morte", x = "Local", y = "Vítimas", fill = "Local da Morte") + scale_x_discrete(labels = c("Hospital" = "Hospital", "Nursing home" = "Lar de Idosos", "Own home" = "Em casa")) + theme_minimal() 

#Com base no gráfico de barras que mostra o número de vítimas do Doutor Morte por local de morte, é possível observar que a grande maioria (mais de 200 vítimas) morreu em suas próprias casas. Por outro lado, um número significativamente menor de vítimas, inferior a 25, morreu em lares de idosos ou em hospitais. Isso indica que o local mais comum dos crimes foi a própria residência das vítimas.

#----------------------------------

#(e) Analise graficamente o ano da morte das vítimas de Harold Shipman.


ggplot(data = dados, aes(x = dados$AnoDaMorte)) + geom_line(stat = "count") +  labs(title = "Tendência de Mortes por Ano", x = "Ano", y = "Número de Mortes") + theme_minimal()

#Com base no gráfico em linha dos anos em que as vítimas do Doutor Morte foram assassinadas, é possível identificar quando foi o período de maior atividade do serial killer. Embora existam vítimas a partir de 1975, o número de assassinatos começa a aumentar significativamente a partir de 1982. Observa-se uma queda no início da década de 1990, possivelmente indicando uma mudança no comportamento ou nas circunstâncias do assassino. A partir de 1992, o número de vítimas volta a crescer, atingindo seu pico em 1995. Após esse ponto, há uma queda no número de vítimas, sugerindo o fim do período de maior atividade.

#----------------------------------

#(f) Com base nas informações obtidas nos itens anteriores, escreva um parágrafo sobre o padrão e o perfil das vítimas de Harold Shipman.


#Harold Shipman tinha como principais vítimas mulheres idosas, com idades entre 70 e 85 anos. A maior parte de seus crimes ocorreu nas próprias residências das vítima, sendo raros os casos em lares de idosos ou hospitais. Seu período de maior atividade foi entre 1982 e 1995, com um pico em 1995, antes de uma quedano número de mortes. Embora a maioria de suas vítimas fossem mulheres idosas, há registros de assassinatos de homens e também de algumas pessoas mais jovens.

#---------------------------------------------------------------

#O conjunto primatas.txt apresenta informações sobre tamanho (centímetros), peso (libras) e gênero de bonobos e de chimpanzés. Abra o arquivo e veja como ele está organizado.

#(a) Importe o arquivo para o ambiente do R. Conheça sua estrutura e peça um resumo dos dados com alguma função

primatas <- read.table(file = "primatas.txt", header = TRUE, sep = ":")

str(primatas) #Estrutura dos dados (quantidade, tipo de dado)
summary(primatas)

#(b) Construa um gráfico de barras contando quantas espécies de bonobos e chimpanzés há no conjunto. Construa também um gráfico de barras mostrando a frequência de machos e fêmeas de cada espécie.

library(ggplot2)

primatas$especie <- as.factor(primatas$especie)
primatas$genero <- as.factor(primatas$genero)

ggplot(data = primatas, aes(x = primatas$especie, fill = primatas$especie)) + scale_fill_manual(values = c("bonobo" = "orange", "chimpanze" = "lightyellow")) + geom_bar() + labs(title = "Quantidade de primatas de cada espécie", x = "Primatas", y = "Quantidade", fill = "Primata") + theme_classic()

ggplot(data = primatas, aes(x = primatas$genero, fill = interaction(primatas$especie, primatas$genero))) + scale_fill_manual(values = c("bonobo.macho" = "#ff9933", "bonobo.femea" = "#ffcc99", "chimpanze.macho" = "#cccc00", "chimpanze.femea" = "#ffff99"), labels = c("bonobo.femea" = "Bonobo Femea", "chimpanze.femea" = "Chimpanze Femea", "bonobo.macho" = "Bonobo Macho", "chimpanze.macho" = "Chimpanze Macho")) + geom_bar() + facet_wrap(~primatas$especie) + labs(title = "Quantidade de primatas de cada espécie dividida por gênero", x = "Primata", y = "Quantidade", fill = "Primata") + theme_classic()


# (c)Construa um gráfico para comparar as fêmeas e os machos dos bonobos. Em seguida, construa, também, um gráfico para comparar as fêmeas e os machos dos chimpanzés.

ggplot(data = primatas, aes(x = primatas$genero, y = primatas$altura, fill = interaction(primatas$genero, primatas$especie))) + scale_fill_manual(values = c("macho.bonobo" = "#ff9933", "femea.bonobo" = "#ffcc99", "macho.chimpanze" = "#cccc00", "femea.chimpanze" = "#ffff99"), labels = c("femea.bonobo" = "Bonobo Femea", "femea.chimpanze" = "Chimpanze Femea", "macho.bonobo" = "Bonobo Macho", "macho.chimpanze" = "Chimpanze Macho")) + geom_boxplot() + facet_wrap(~primatas$especie) + labs(title = "Comparação das Alturas de Primatas Dividido por Gênero", x = "Gênero", y = "Altura (em cm)", fill = "Primatas") + theme_minimal()

# (d) Construa um gráfico para comparar as fêmeas dos bonobos e dos chimpanzés. Em seguida, construa também um gráfico para comparar os machos dos bonobos e dos chimpanzés.

ggplot(data = primatas, aes(x = primatas$especie, y = primatas$peso, fill = interaction(primatas$genero, primatas$especie))) + scale_fill_manual(values = c("macho.bonobo" = "#ff9933", "femea.bonobo" = "#ffcc99", "macho.chimpanze" = "#cccc00", "femea.chimpanze" = "#ffff99"), labels = c("femea.bonobo" = "Bonobo Femea", "femea.chimpanze" = "Chimpanze Femea", "macho.bonobo" = "Bonobo Macho", "macho.chimpanze" = "Chimpanze Macho")) + labs(title = "Comparação do peso entre fêmeas e machos de bonobos e chipamzes", x = "Primata", y = "Peso (Kg)", fill = "Primatas") + geom_boxplot() + facet_wrap(primatas$genero) + theme_minimal()

# (e) A partir das análises dos itens anteriores, escreva um pequeno texto contendo informações sobre os bonobos e os chimpanzés, como exemplo: diferenças entre os gêneros de cada espécie e diferenças entre as espécies.

#A espécie dos bonobos apresenta um dimorfismo sexual em relação ao peso e à altura. Os machos bonobos são notavelmente mais altos, atingindo cerca de 135 cm, enquanto as fêmeas apresentam uma altura média, normalmente abaixo de 130 cm. Quanto ao peso, essa diferença é ainda maior: as fêmeas pesam em média 35 kg, sendo aproximadamente 10 kg mais leves que os machos.

#De maneira similar, os chimpanzés também mostram dimorfismo sexual. Os machos são significativamente mais altos e pesados que as fêmeas. A altura média dos machos é de cerca de 135 cm, enquanto as fêmeas, em geral, ficam abaixo de 125 cm. Em termos de peso, a diferença entre os sexos também é marcante: os machos pesam em torno de 60 kg, e as fêmeas pesam um pouco menos de 55 kg.

#Ao comparar as duas espécies, é possível notar que os machos chimpanzés são mais altos e mais pesados que os machos bonobos. No entanto, ao comparar as fêmeas, as bonobos fêmeas são mais altas do que as fêmeas chimpanzés, embora estas últimas sejam mais pesadas.

# (f) A partir das variáveis tamanho, peso e genero, construa um modelo de árvore de decisão utilizando estruturas condicionais que seja capaz de prever a espécie de uma observação. Calcule a acurácia do modelo.

primatas <- read.table(file = "primatas.txt", header = TRUE, sep = ":")
primatas <- na.omit(primatas)

n <- round(0.8 * nrow(primatas))

indices <- sample(1:nrow(primatas), size = n, replace = FALSE)

treino <- primatas[indices,]
teste <- primatas[-indices,]

library(rpart)
library(rpart.plot)

arvore <- rpart(formula = especie ~ ., data = treino, method = "class")

rpart.plot(arvore, extra = 101)

previsao <- predict(arvore, newdata = teste, type = "class")

mean(previsao == teste$especie)
