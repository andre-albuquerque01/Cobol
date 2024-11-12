# Sistema de Cálculo de Saldo Médio de Clientes em COBOL
Este programa foi desenvolvido em COBOL e está sendo executado em um ambiente mainframe. Ele tem como objetivo calcular o saldo médio dos clientes de uma instituição financeira, gerando um relatório detalhado com base nas informações de conta, nome e períodos de movimentação.

## Funcionalidades
O sistema realiza as seguintes operações:

1. Leitura de Dados: Lê registros de um arquivo de entrada (ERESUMO.DAT) contendo informações de clientes, incluindo o número da conta, o nome e cinco períodos de movimentação. Cada período contém o valor mantido na conta e o número de dias em que o valor permaneceu.

2. Cálculo do Saldo Médio: Calcula o saldo médio do cliente com base nos valores e períodos fornecidos.

3. Geração de Relatório: Cria um relatório (RELATO.DAT) contendo:
    * Total de registros lidos.
    * Registros processados com sucesso.
    * Registros rejeitados por inconsistência.
    * O saldo médio de cada cliente.
4. Resumo de Erros: Registra erros encontrados durante a execução e rejeita registros inconsistentes.

## Estrutura do Código
O programa segue uma estrutura COBOL padrão, dividida nas seções:

* IDENTIFICATION DIVISION: Identificação do programa.
* ENVIRONMENT DIVISION: Configuração do ambiente de execução, definindo arquivos de entrada e saída.
* DATA DIVISION: Definição de variáveis e estruturas de dados, como registros e contadores.
* PROCEDURE DIVISION: Implementação das operações, incluindo:
    * Inicialização dos arquivos e variáveis.
    * Leitura de registros e validação de dados.
    * Cálculo do saldo médio.
    * Impressão e fechamento do relatório.
## Autor e Informações Adicionais
* Autor: Andre Albuquerque
* Data de Criação: 31/10/2024
* Compilador: COBOL com extensão cobc
* Observação: A sigla ER refere-se a "ERESUMO".
Este programa é uma solução eficiente para o processamento de dados financeiros, fornecendo uma análise rápida do saldo médio de clientes em um formato claro e organizado.
