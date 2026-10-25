# Apuraí · Eleições 2026

**Apuraí** é um painel em [Shiny](https://shiny.posit.co/) para acompanhar, ao vivo, a apuração do
**2º turno da eleição para Presidente da República (Eleições 2026)**, com os dados públicos do
Tribunal Superior Eleitoral (TSE).

## O que o painel mostra

- **Placar nacional** (Brasil + exterior) e **placar do exterior**, com a linha dos 50% dos votos válidos
- **Mapa do Brasil** colorido por estado, conforme o candidato à frente e o tamanho da vantagem
- **Mapa do exterior**, com um círculo por cidade com seção eleitoral (tamanho = votos) e atalhos de zoom
  (Mundo, Américas, Fronteiras, Europa, África e Oriente Médio, Ásia e Oceania)
- **Tabelas ordenáveis** por estado, por país e por cidade do exterior: clique no nome da coluna
  para descobrir onde cada candidato teve mais votos
- Atualização automática a cada **30 segundos**

## De onde vêm os dados

Tudo é lido diretamente de `resultados.tse.jus.br`, sem intermediários:

| O quê | Arquivo do TSE | Quando fica disponível |
|---|---|---|
| Total, estados e exterior | `…/ele2026/6258/dados/<uf>/<uf>-c0001-e006258-u.jws` | a partir das 17h (Brasília) |
| Exterior, seção por seção | boletins de urna em `…/ele2026/arquivo-urna/3221/…` | ao longo do dia, conforme as seções encerram |

Antes das 17h, o painel do exterior soma os **boletins de urna** (arquivos `.bu`, em formato binário
ASN.1) que o TSE já publicou. O Apuraí traz um leitor próprio desses arquivos, escrito em R puro.
Depois das 17h, passa a usar a totalização oficial.

Os mapas são desenhados em SVG pelo próprio app, a partir de arquivos na pasta `dados/`:

- `uf_brasil.geojson`: limites das UFs (simplificado de *click_that_hood*, Code for Germany)
- `mundo.geojson`: países (simplificado de *Natural Earth*, domínio público)
- `cidades_exterior.csv`: código TSE, país e coordenadas das 186 cidades com seção no exterior

## Como rodar

```r
install.packages(c("shiny", "jsonlite", "dplyr", "curl", "tidyr"))
shiny::runApp()
```

Ou abra o `apurai.Rproj` no RStudio e clique em **Run App**.

## Avisos

- Os resultados são **parciais** até o fim da totalização. A fonte oficial é sempre o TSE.
- Os códigos da eleição (`6258`) e do pleito (`3221`) são detectados automaticamente a partir de
  `ele-c.json`. Para outras eleições, ajuste `PADRAO` no início do `app.R`.

## Licença

[MIT](LICENSE) © 2026 Caroline C. Vasconcelos

---

Desenvolvido por **Caroline C. Vasconcelos** · [github.com/ccvasconcelos](https://github.com/ccvasconcelos)
