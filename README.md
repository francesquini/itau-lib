itau-lib
========

Esta é uma biblioteca em Haskell para automatizar o acesso às informações de contas do Banco Itaú. Por hora ela pode ser usada apenas para consultas. 

Este não é um aplicativo final, mas uma biblioteca. Se você está interessado em ferramentas prontas para auxiliar a conciliação bancária em aplicativos como Quicken, Microsoft Money, GNUCash e HomeBank (ou qualquer outro compatível com o formato OFX), incluíndo outros bancos como Banco do Brasil, Caixa Econômica, Itaú e Santander, você provavelmente está atrás deste outro projeto (que alias, utiliza esta biblioteca): https://github.com/maxiwell/ofx-bot.

Entre as informações atualmente acessíveis pela biblioteca estão:
* Conta corrente
    * Extrato (CSV e OFX)
    * Lançamentos Futuros
    * Saldo Provisorio
    * Saldo Disponivel
    * Saldo Bloqueado
    * CEP
    * CEP Plus
    * Total Com Limite
    * Saldo Efetivo
    * Juros Dias
    * Juros Valor
    * Juros Vencimento
    * Juros Taxa Mensal
    * Juros Taxa Anual
    * Juros CET Mensal
    * Juros CET Anual
     
* Cartão de crédito
  * CSV com os lançamentos das faturas anterior, atual e próxima.
  

Requisitos:

* É necessário possuir um **haskell-platform** e um **cabal** recente em funcionamento.

```bash
apt-get install haskell-platform cabal-install
cabal update
cabal install cabal
```

* A última linha garante que você tenha a última versão do cabal instalada. Algumas distribuições ainda vêm com uma versão bem antiga.

* Para compilar:

```bash
cd itau-lib
./build.sh
```
* Caso a sua instalação de Haskell seja nova, a compilação provavelmente instalará uma série de pacotes que precisarão ser baixados (automaticamente). Isso pode levar algum tempo.


Como usar:
-------------

No seu projeto Cabal adicione a biblioteca itau-lib:

* Modifique o seu projeto.cabal e adicione itau-lib aos build-depends.

* Instale a biblioteca no seu sandbox 

```bash
cabal sandbox init -- NOP caso já exista uma sandbox
cabal sandbox add-source CAMINHO/PARA/itau-lib
cabal install --only-dependencies
```

Notas:
------------
* Este é um projeto pessoal e não tem vínculo algum com o banco Itaú. 
* Este projeto foi criado para facilitar o controle da minha conta corrente e eu o disponibilizo aqui na esperança de que talvez possa ser útil para alguém. Não há garantia (implícita ou explícita) ou suporte em relação a este código. Em outras palavras, este código pode fazer seu computador criar pernas e matar o seu gato... Use por sua conta e risco.

