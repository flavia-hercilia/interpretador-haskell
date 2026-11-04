# Projeto 1ª VA - Interpretador em Haskell

Este repositório contém a implementação de um interpretador de uma linguagem imperativa simples em Haskell, desenvolvido utilizando a metodologia TDD (Test-Driven Development) com a biblioteca HUnit. 

---

## 📋 Lista de Requisitos e Tarefas

- [x] **1. Melhorar os tipos suportados:**
  - [x] a. Acrescentar suporte ao tipo `Float` e atualizar `evalBinOp` (Int + Float = Float; Int + Int = Int).
  - [x] b. Operador `Div` implementado como `div` (inteiros) e `/` (floats).
  - [x] c. Adicionar suporte a arrays unidimensionais de inteiros, booleanos e floats.
- [x] **2. Chamada de função:**
  - [x] a. Avaliação de argumentos da esquerda para a direita.
  - [x] b. Escopo de variáveis: considerar variáveis globais em adição às locais.
- [x] **3. Atribuição:**
  - [x] a. Verificação de compatibilidade de tipo na atribuição.
  - [x] b. Conversão implícita (Int -> Float, e Float -> Int com truncamento da parte decimal).
- [x] **4. Extra 1:** Implementação da estrutura de controle `Switch-Case`.
- [ ] **4. Extra 2:** Implementação da estrutura de repetição `For`.

---

## 🛠️ Justificativa para Mudanças nos Tipos das Funções

Durante o desenvolvimento, algumas alterações estruturais nos tipos foram necessárias para acomodar os novos requisitos:
* **Árvore Sintática (`data Stmt` e `data Expr`)**: Foram expandidas para incluir as novas estruturas, como o construtor `Switch Expr [(Expr, Stmt)] Stmt` e o construtor `For` (em desenvolvimento), além das expressões de acesso e declaração de Arrays.
* **Valores do Interpretador (`data Value`)**: O tipo foi expandido para suportar os novos tipos nativos, incluindo construtores como `VFloat Float` e representações para Arrays.
* **Assinaturas de Avaliação**: As funções `evalExpr` e `execStmt` mantiveram sua essência de passagem de ambiente (`Env` e `FunEnv`), mas a lógica interna foi adaptada estruturalmente para lidar com o encadeamento do estado, especialmente na avaliação da esquerda para a direita de argumentos de funções.

---

## 🤖 Declaração de Uso de Inteligência Artificial e Conduta

Ao participar da disciplina de Paradigmas de Programação, declaramos estar cientes e de acordo com as diretrizes relacionadas ao uso de Inteligência Artificial (IA) e à responsabilidade sobre o conteúdo entregue.

**1. Uso consciente da IA:**
A ferramenta de Inteligência Artificial foi utilizada estritamente como apoio no processo de aprendizagem, mantendo um uso ético e limitado. A IA atuou como um "tutor" ou "pair programmer" para tirar dúvidas de sintaxe e regras da linguagem Haskell, não substituindo o nosso domínio pleno sobre a lógica e os algoritmos implementados.

**2. Responsabilidade pelo conteúdo entregue:**
Temos completo entendimento de todo o material entregue, nos responsabilizamos integralmente pelo código e estamos aptos a explicá-lo com clareza em possíveis questionamentos.

**3. Transparência sobre o uso da IA:**
Declaramos que o material e a lógica do código são originais da equipe.
* **Ferramenta utilizada:** Google Gemini.
* **Percentual de código gerado por IA:** ~1% (A IA não gerou a solução ou blocos funcionais do projeto).
* **Forma de utilização e exemplos de prompts:** A IA foi utilizada para ajudar a compreensão de erros do compilador (ex: *"Non-exhaustive patterns in case"*), tirar dúvidas sobre formatação e blocos `do` em Haskell. Todo o fluxo seguiu o padrão TDD, com os testes sendo escritos por nós antes das implementações.

**4. Declaração de reuso de material:**
Declaramos que não houve reuso não declarado de materiais de terceiros. Toda a base do projeto partiu dos arquivos iniciais fornecidos pelo professor da disciplina, Sidney de Carvalho Nogueira, sendo o restante de autoria própria da equipe.
