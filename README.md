# Projeto 1ª VA - Interpretador em Haskell

Este repositório contém a implementação de um interpretador de uma linguagem imperativa simples em Haskell, desenvolvido utilizando a metodologia TDD (Test-Driven Development) com a biblioteca HUnit. 

---

## 📋 Lista de Requisitos e Tarefas

- [x] **1. Melhorar os tipos suportados:**
  - [x] a. Tipo `Float`; operações com Int e Float resultam em Float; com dois Int, em Int.
  - [x] b. `Div` usa `div` com dois inteiros e `/` quando há um Float.
  - [x] c. Arrays unidimensionais de inteiros, booleanos e floats: declaração, leitura, escrita e impressão.
- [x] **2. Chamada de função:**
  - [x] a. Avaliação de argumentos da esquerda para a direita.
  - [x] b. Escopo de variáveis: considerar variáveis globais em adição às locais.
- [x] **3. Atribuição:**
  - [x] a. Verificação de compatibilidade de tipo na atribuição.
  - [x] b. Conversão implícita (Int -> Float, e Float -> Int com truncamento da parte decimal).
- [x] **4. Extra 1:** Implementação da estrutura de controle `Switch-Case`.
- [x] **4. Extra 2:** Implementação da estrutura de repetição `For`.

---

## 🛠️ Justificativa para Mudanças nos Tipos das Funções

Durante o desenvolvimento, algumas alterações estruturais nos tipos foram necessárias para acomodar os novos requisitos:
* **Tipos da linguagem (`data Type`)**: Foram acrescentados `TFloat`, para o tipo float, e `TArray Type`, para arrays. O `TArray` guarda o tipo dos elementos, permitindo declarar arrays de inteiros (`TArray TInt`), floats (`TArray TFloat`) e booleanos (`TArray TBool`).
* **Árvore Sintática (`data Stmt` e `data Expr`)**: Foram expandidas para incluir as novas estruturas, como o construtor `Switch Expr [(Expr, Stmt)] Stmt` e o construtor `For Stmt Expr Stmt Stmt`, cujos campos são a inicialização, a condição, a atualização e o corpo do laço. Para os arrays, foram acrescentados `ArrayLit [Expr]` (valores iniciais, como `{1, 2, 3}`), `Index String Expr` (leitura de `v[i]`) e `ArrayAssign String Expr Expr` (escrita em `v[i]`). Para os números com vírgula, foi acrescentado o literal `FLit Float`.
* **Valores do Interpretador (`data Value`)**: O tipo foi expandido com `VFloat Float`, para valores float, e `VArray [Value]`, que guarda os elementos de um array como uma lista de valores.
* **Assinaturas de Avaliação**: As funções `evalExpr` e `execStmt` mantiveram sua essência de passagem de ambiente (`Env` e `FunEnv`), mas a lógica interna foi adaptada estruturalmente para lidar com o encadeamento do estado, especialmente na avaliação da esquerda para a direita de argumentos de funções.

---

## ▶️ Como Instalar e Executar

### 1. Instalar as dependências

Testado no Ubuntu (GitHub Codespaces). No terminal:

    sudo apt-get update
    sudo apt-get install -y ghc libghc-hunit-dev libghc-silently-dev

- `ghc`: o compilador e o GHCi (modo interativo do Haskell).
- `libghc-hunit-dev`: a biblioteca de testes HUnit.
- `libghc-silently-dev`: a biblioteca silently, usada para capturar o que é impresso.

O pacote `containers` (que fornece o `Data.Map`) já vem junto com o GHC.

### 2. Abrir o interpretador no GHCi

    ghci -package silently -package containers -package HUnit interpretador-29-09-2026-2.hs

### 3. Rodar os testes

No GHCi (prompt `ghci>`):

    runTestTT suite

O resultado esperado é `Errors: 0  Failures: 0`.

### 4. Rodar um programa específico

    runProg Map.empty prog1          (programas sem chamada de função)
    runProg fenvExemplo prog4        (programas que chamam funções)
    runProgIO Map.empty prog7        (mostra o que o programa imprime)

### 5. Verificação manual de erros

**Atribuição com tipos incompatíveis** — o `prog17` declara `int x = 10` e
depois tenta fazer `x = true;`:

    runProg Map.empty prog17

Saída esperada:

    Exception: Tipos incompativeis

---

## 🤖 Declaração de Uso de Inteligência Artificial e Conduta

Ao participar da disciplina de Paradigmas de Programação, declaramos estar cientes e de acordo com as diretrizes relacionadas ao uso de Inteligência Artificial (IA) e à responsabilidade sobre o conteúdo entregue.

**1. Uso consciente da IA:**
A ferramenta de Inteligência Artificial foi utilizada estritamente como apoio no processo de aprendizagem, mantendo um uso ético e limitado. A IA atuou como um "tutor" ou "pair programmer" para tirar dúvidas de sintaxe e regras da linguagem Haskell, não substituindo o nosso domínio pleno sobre a lógica e os algoritmos implementados.

**2. Responsabilidade pelo conteúdo entregue:**
Temos completo entendimento de todo o material entregue, nos responsabilizamos integralmente pelo código e estamos aptos a explicá-lo com clareza em possíveis questionamentos.

**3. Transparência sobre o uso da IA:**
Declaramos que o material e a lógica do código são originais da equipe.
* **Ferramenta utilizada:** Google Gemini e Claude.
* **Percentual de código gerado por IA:** Gemini ~1% e Claude ~3% (A IA não gerou a solução ou blocos funcionais do projeto, apenas orientação/explicação de dúvidas).
* **Forma de utilização e exemplos de prompts:** A IA foi utilizada para ajudar a compreensão de erros do compilador (ex: *"Non-exhaustive patterns in case"*), tirar dúvidas sobre formatação e blocos `do` em Haskell e ajuda com o ambiente (GitHub Codespaces, instalação dos pacotes) e com alguns comandos git, incluindo a resolução de um conflito de merge.

**4. Declaração de reuso de material:**
Declaramos que não houve reuso não declarado de materiais de terceiros. Toda a base do projeto partiu dos arquivos iniciais fornecidos pelo professor da disciplina, Sidney de Carvalho Nogueira, sendo o restante de autoria própria da equipe.
