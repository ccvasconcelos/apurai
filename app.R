# =============================================================
#  Apuraí Eleições 2026 – apuração do 2º turno, Presidente
#  Dados públicos do TSE (resultados.tse.jus.br)
#
#  - Total, UFs e exterior: arquivos oficiais de totalização
#    (liberados pelo TSE a partir das 17h de Brasília)
#  - Exterior ANTES das 17h: soma dos Boletins de Urna (BU) que
#    as seções do exterior já enviaram e o TSE já publicou
#  Atualiza sozinho a cada 30 s.
# =============================================================

library(shiny)
library(jsonlite)
library(dplyr)
library(curl)

base <- "https://resultados.tse.jus.br/oficial"

# Valores de 2026 (usados se a detecção automática falhar)
PADRAO <- list(ciclo = "ele2026", eleicao = "6258", pleito = "3221")

ufs <- c("ac","al","am","ap","ba","ce","df","es","go","ma","mg","ms","mt",
         "pa","pb","pe","pi","pr","rj","rn","ro","rr","rs","sc","se","sp","to")

num <- function(x) as.numeric(gsub(",", ".", x))

# ---- Downloads em paralelo -----------------------------------
baixar <- function(urls) {
  res  <- vector("list", length(urls))
  pool <- new_pool(host_con = 8)
  for (k in seq_along(urls)) local({
    kk <- k
    curl_fetch_multi(urls[kk], pool = pool,
      done = function(r) if (r$status_code == 200) res[[kk]] <<- r$content,
      fail = function(e) NULL)
  })
  multi_run(pool = pool)
  res
}

# ---- Descobre ciclo, eleição e pleito do 2º turno ------------
descobrir_eleicao <- function() {
  tryCatch({
    cfg <- fromJSON(file.path(base, "comum/config/ele-c.json"),
                    simplifyVector = FALSE)
    for (p in cfg$pl) for (e in p$e)
      if (identical(e$t, "2") && grepl("Federal", e$nm) && grepl("2026", e$nm))
        return(list(ciclo = p$c, eleicao = e$cd, pleito = p$cd))
    PADRAO
  }, error = function(e) PADRAO)
}

# ---- Arquivos .jws: o conteúdo JSON fica no meio (base64url) --
ler_jws <- function(txt) {
  p <- strsplit(txt, ".", fixed = TRUE)[[1]][2]
  p <- chartr("-_", "+/", p)
  p <- paste0(p, strrep("=", (4 - nchar(p) %% 4) %% 4))
  fromJSON(rawToChar(base64_dec(p)), simplifyVector = FALSE)
}

# ---- Totalização oficial de uma abrangência (br, zz ou UF) ---
tabela_total <- function(j, local) {
  cand <- list()
  for (a in j$carg[[1]]$agr) for (p in a$par) for (cc in p$cand)
    cand[[length(cand) + 1]] <- cc
  tibble(
    local   = toupper(local),
    numero  = sapply(cand, `[[`, "n"),
    nome    = sapply(cand, `[[`, "nmu"),
    votos   = num(sapply(cand, `[[`, "vap")),
    pct     = num(sapply(cand, `[[`, "pvap")),
    secoes  = num(j$s$pst),          # % de seções totalizadas
    st      = num(j$s$st),           # nº de seções totalizadas
    ts      = num(j$s$ts),           # nº total de seções
    hora    = paste(j$dg, j$hg)
  )
}

ler_totais <- function(info, abr) {
  ele6 <- sprintf("%06d", as.integer(info$eleicao))
  urls <- sprintf("%s/%s/%s/dados/%s/%s-c0001-e%s-u.jws",
                  base, info$ciclo, info$eleicao, abr, abr, ele6)
  conteudo <- baixar(urls)
  bind_rows(Map(function(r, a) {
    if (is.null(r)) return(NULL)
    tryCatch(tabela_total(ler_jws(rawToChar(r)), a), error = function(e) NULL)
  }, conteudo, abr))
}

# ---- Leitor mínimo de Boletim de Urna (.bu, ASN.1 DER) -------
der_nodes <- function(b, s = 1L, e = length(b)) {
  out <- list(); i <- s
  while (i <= e) {
    t <- as.integer(b[i]); i <- i + 1L
    tag <- bitwAnd(t, 31L)
    if (tag == 31L) {
      tag <- 0L
      repeat { x <- as.integer(b[i]); i <- i + 1L
               tag <- tag * 128L + bitwAnd(x, 127L)
               if (bitwAnd(x, 128L) == 0L) break }
    }
    L <- as.integer(b[i]); i <- i + 1L
    if (L >= 128L) { n <- L - 128L; L <- 0
      for (k in seq_len(n)) { L <- L * 256 + as.integer(b[i]); i <- i + 1L } }
    no <- list(cls = t %/% 64L, cons = bitwAnd(t, 32L) > 0, tag = tag,
               ini = i, fim = i + L - 1L)
    if (no$cons) no$ch <- der_nodes(b, i, i + L - 1L)
    out[[length(out) + 1L]] <- no
    i <- i + L
  }
  out
}
der_int <- function(b, no) {
  if (no$fim < no$ini) return(0)
  v <- 0; for (k in no$ini:no$fim) v <- v * 256 + as.integer(b[k]); v
}

# Devolve data.frame(numero, votos). numero: candidato, "branco" ou "nulo"
ler_bu <- function(b) {
  topo <- der_nodes(b)[[1]]
  oct <- Filter(function(n) !n$cons && n$cls == 0 && n$tag == 4 &&
                  n$fim - n$ini > 100, topo$ch)
  if (length(oct) == 0) return(NULL)
  bu  <- der_nodes(b, oct[[1]]$ini, oct[[1]]$fim)
  res <- list()
  achar <- function(nos) for (n in nos) if (n$cons) {
    ch   <- n$ch
    tipo <- Filter(function(x) x$cls == 2 && x$tag == 1 && !x$cons, ch)
    qtd  <- Filter(function(x) x$cls == 2 && x$tag == 2 && !x$cons, ch)
    idv  <- Filter(function(x) x$cls == 2 && x$tag == 3 &&  x$cons, ch)
    if (length(tipo) && length(qtd)) {
      tv <- der_int(b, tipo[[1]])
      numero <- if (tv == 1 && length(idv)) {
        ids <- idv[[1]]$ch; as.character(der_int(b, ids[[length(ids)]]))
      } else c("2" = "branco", "3" = "nulo")[as.character(tv)]
      if (!is.na(numero))
        res[[length(res) + 1L]] <<- data.frame(numero = numero,
                                               votos = der_int(b, qtd[[1]]))
    }
    achar(ch)
  }
  achar(bu)
  do.call(rbind, res)
}

# ---- Exterior pelos Boletins de Urna (com cache) -------------
cache_bu <- new.env()

atualizar_bu_exterior <- function(info) {
  dir_bu <- sprintf("%s/%s/arquivo-urna/%s", base, info$ciclo, info$pleito)
  pl6 <- sprintf("%06d", as.integer(info$pleito))
  cs  <- fromJSON(sprintf("%s/config/zz/zz-p%s-cs.json", dir_bu, pl6),
                  simplifyVector = FALSE)

  secoes <- list()
  for (m in cs$abr[[1]]$mu) for (z in m$zon) for (s in z$sec)
    if (!is.null(s$da) && is.null(s$nsp))      # só seções principais com BU
      secoes[[length(secoes) + 1]] <- list(mu = m$cd, cidade = m$nm,
                                           zona = z$cd, secao = s$ns,
                                           chave = paste(m$cd, z$cd, s$ns, s$ha))
  novas <- Filter(function(s) !exists(s$chave, envir = cache_bu), secoes)

  if (length(novas)) {
    pasta <- sapply(novas, function(s)
      sprintf("%s/dados/zz/%s/%s/%s", dir_bu, s$mu, s$zona, s$secao))
    aux_urls <- mapply(function(s, d)
      sprintf("%s/p%s-zz-m%s-z%s-s%s-aux.json", d, pl6, s$mu, s$zona, s$secao),
      novas, pasta)
    aux <- baixar(aux_urls)

    bu_urls <- mapply(function(a, d) {
      if (is.null(a)) return(NA_character_)
      j <- tryCatch(fromJSON(rawToChar(a), simplifyVector = FALSE),
                    error = function(e) NULL)
      if (is.null(j) || !length(j$hashes)) return(NA_character_)
      h <- j$hashes[[1]]
      arq <- Filter(function(x) identical(x$tp, "bu"), h$arq)
      if (!length(arq)) return(NA_character_)
      sprintf("%s/%s/%s", d, h$hash, arq[[1]]$nm)
    }, aux, pasta)

    ok <- !is.na(bu_urls)
    bus <- vector("list", length(novas))
    bus[ok] <- baixar(bu_urls[ok])

    for (k in seq_along(novas)) if (!is.null(bus[[k]])) {
      v <- tryCatch(ler_bu(bus[[k]]), error = function(e) NULL)
      if (!is.null(v)) {
        v$cidade <- novas[[k]]$cidade
        v$mu     <- novas[[k]]$mu
        assign(novas[[k]]$chave, v, envir = cache_bu)
      }
    }
  }

  chaves <- intersect(sapply(secoes, `[[`, "chave"), ls(cache_bu))
  list(
    votos     = bind_rows(mget(chaves, envir = cache_bu)),
    lidas     = length(chaves),
    hora      = paste(cs$dg, cs$hg)
  )
}

# =============================================================
# =============================================================
#  MAPAS (SVG desenhado aqui mesmo, sem pacotes extras)
# =============================================================

cidades_ext <- read.csv("dados/cidades_exterior.csv", encoding = "UTF-8",
                        colClasses = c(codigo = "character"))

# Converte um GeoJSON em caminhos SVG, com a projeção dada
geo_svg <- function(arquivo, proj, id_campo) {
  g <- fromJSON(arquivo, simplifyVector = FALSE)
  anel <- function(r) {
    xy <- t(vapply(r, function(p) proj(p[[1]], p[[2]]), numeric(2)))
    paste0("M", paste(sprintf("%.1f,%.1f", xy[, 1], xy[, 2]), collapse = "L"), "Z")
  }
  do.call(rbind, lapply(g$features, function(f) {
    co <- f$geometry$coordinates
    polis <- if (f$geometry$type == "Polygon") list(co) else co
    d <- paste(unlist(lapply(polis, function(pl) lapply(pl, anel))), collapse = "")
    data.frame(id = f$properties[[id_campo]], nome = f$properties$nome, d = d)
  }))
}

# Brasil: equiretangular com correção de latitude
K_BR <- 18
proj_br <- function(lon, lat) c((lon + 74.2) * cos(15 * pi / 180) * K_BR, (5.6 - lat) * K_BR)
VB_BR <- sprintf("0 0 %.0f %.0f", (74.2 - 34.6) * cos(15 * pi / 180) * K_BR, (5.6 + 34) * K_BR)
mapa_br <- geo_svg("dados/uf_brasil.geojson", proj_br, "sigla")

# Mundo: equiretangular, recortado entre 84°N e 57°S
K_MU <- 4
proj_mu <- function(lon, lat) c((lon + 180) * K_MU, (84 - lat) * K_MU)
mapa_mu <- geo_svg("dados/mundo.geojson", proj_mu, "iso")
vb_mu <- function(lon1, lon2, lat1, lat2) {
  a <- proj_mu(lon1, lat2); b <- proj_mu(lon2, lat1)
  sprintf("%.0f %.0f %.0f %.0f", a[1], a[2], b[1] - a[1], b[2] - a[2])
}
zooms <- list(
  "Mundo"          = vb_mu(-180, 180, -57, 84),
  "Américas"       = vb_mu(-130, -30, -56, 55),
  "Fronteiras"     = vb_mu(-75, -45, -36, 8),
  "Europa"         = vb_mu(-12, 42, 34, 62),
  "África e Oriente Médio" = vb_mu(-20, 62, -36, 40),
  "Ásia e Oceania" = vb_mu(65, 180, -48, 45)
)

# Cor pela vantagem: vermelho (13) ou azul (22), mais forte quanto maior
cor_margem <- function(p13) {
  vapply(p13, function(p) {
    if (is.na(p)) return("#E6E1D3")
    d <- abs(p - 50); i <- findInterval(d, c(5, 10, 20)) + 1
    if (p >= 50) c("#F1C6C1", "#E08A80", "#C4473D", "#8F1F1A")[i]
    else         c("#C5D5EE", "#88ABDD", "#4677C0", "#173F80")[i]
  }, character(1))
}

legenda_mapa <- function(nm) {
  passos <- c("até 5", "5–10", "10–20", "+20")
  bloco <- function(cores, nome) div(class = "leg-lado",
    span(class = "leg-nome", nome),
    div(class = "leg-cores", lapply(seq_along(cores), function(i)
      span(style = paste0("background:", cores[i]), title = paste(passos[i], "p.p."))))
  )
  div(class = "legenda",
      bloco(c("#F1C6C1", "#E08A80", "#C4473D", "#8F1F1A"), nm[["13"]]),
      span(class = "leg-meio", "vantagem em pontos percentuais"),
      bloco(rev(c("#C5D5EE", "#88ABDD", "#4677C0", "#173F80")), nm[["22"]]))
}

# =============================================================
#  INTERFACE
# =============================================================

INTERVALO <- 30   # segundos entre atualizações

fmt  <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
pctf <- function(x, dig = 2) ifelse(is.na(x), "—",
  paste0(formatC(x, format = "f", digits = dig, decimal.mark = ","), "%"))
titulo <- function(x) tools::toTitleCase(tolower(x))

regioes <- c(AC="Norte",AM="Norte",AP="Norte",PA="Norte",RO="Norte",RR="Norte",TO="Norte",
  AL="Nordeste",BA="Nordeste",CE="Nordeste",MA="Nordeste",PB="Nordeste",PE="Nordeste",
  PI="Nordeste",RN="Nordeste",SE="Nordeste",DF="Centro-Oeste",GO="Centro-Oeste",
  MS="Centro-Oeste",MT="Centro-Oeste",ES="Sudeste",MG="Sudeste",RJ="Sudeste",SP="Sudeste",
  PR="Sul",RS="Sul",SC="Sul")

css <- "
:root{
  --verde:#009C3B; --amarelo:#FFDF00; --azul:#002776; --branco:#FFFFFF;
  --tinta:#0E1B2C; --suave:#5B6676; --linha:#E6E1D3; --papel:#F8F6F0;
  --c13:#B7322C; --c22:#1D5BA8;
  --serif:'Fraunces', Georgia, serif; --sans:'Inter', system-ui, sans-serif;
}
*{box-sizing:border-box}
body{margin:0;background:var(--papel);color:var(--tinta);font-family:var(--sans);-webkit-font-smoothing:antialiased}
.container-fluid{padding:0}

/* cabeçalho */
.topo{position:relative;color:#fff;padding:38px 24px 32px;overflow:hidden;
  background:
    radial-gradient(900px 400px at 85% -20%, rgba(0,156,59,.55), transparent 60%),
    radial-gradient(700px 380px at 10% 130%, rgba(255,223,0,.18), transparent 60%),
    linear-gradient(135deg,#00205F 0%, #002776 55%, #003A8C 100%);}
.topo::after{content:'';position:absolute;right:-120px;top:-120px;width:420px;height:420px;
  transform:rotate(45deg);border:1px solid rgba(255,223,0,.25);border-radius:24px}
.topo-in{position:relative;z-index:2;display:flex;justify-content:space-between;align-items:flex-end;gap:24px;flex-wrap:wrap;max-width:1240px;margin:0 auto}
.marca{font-family:var(--serif);font-weight:600;font-size:56px;letter-spacing:-1px;line-height:1;margin:0}
.marca span{color:var(--amarelo)}
.sub{margin-top:10px;font-size:13px;letter-spacing:.22em;text-transform:uppercase;color:#DCE6F5}
.faixa{height:6px;display:grid;grid-template-columns:3fr 2fr 1fr 2fr}
.faixa i:nth-child(1){background:var(--verde)} .faixa i:nth-child(2){background:var(--amarelo)}
.faixa i:nth-child(3){background:#fff} .faixa i:nth-child(4){background:var(--azul)}
.status{display:flex;gap:12px;align-items:center}
.aovivo{display:flex;align-items:center;gap:8px;font-size:12px;letter-spacing:.15em;text-transform:uppercase;
  background:rgba(255,255,255,.1);padding:8px 14px;border-radius:999px;border:1px solid rgba(255,255,255,.18)}
.ponto{width:8px;height:8px;border-radius:50%;background:var(--verde);animation:pulso 1.8s infinite}
@keyframes pulso{0%{box-shadow:0 0 0 0 rgba(0,156,59,.7)}70%{box-shadow:0 0 0 10px rgba(0,156,59,0)}100%{box-shadow:0 0 0 0 rgba(0,156,59,0)}}
.btn-atualizar{background:var(--amarelo)!important;color:var(--azul)!important;border:0!important;
  font-weight:600;border-radius:999px!important;padding:8px 18px!important;font-size:13px}

/* corpo */
.corpo{max-width:1240px;margin:0 auto;padding:28px 24px 48px}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:16px;margin-bottom:22px}
.kpi{background:#fff;border:1px solid var(--linha);border-radius:14px;padding:16px 18px;position:relative;overflow:hidden}
.kpi::before{content:'';position:absolute;left:0;top:0;bottom:0;width:4px;background:var(--verde)}
.kpi:nth-child(2)::before{background:var(--amarelo)} .kpi:nth-child(3)::before{background:var(--azul)}
.kpi .rot{font-size:11px;letter-spacing:.16em;text-transform:uppercase;color:var(--suave)}
.kpi .val{font-family:var(--serif);font-size:30px;font-weight:600;margin-top:6px;line-height:1.1}
.kpi .nota{font-size:12px;color:var(--suave);margin-top:4px}
.grade2{display:grid;grid-template-columns:repeat(auto-fit,minmax(460px,1fr));gap:20px;margin-bottom:20px}
.cartao{background:#fff;border:1px solid var(--linha);border-radius:18px;padding:22px 24px;
  box-shadow:0 1px 0 rgba(14,27,44,.03), 0 12px 30px -18px rgba(14,27,44,.18);min-width:0}
.cartao h3{font-family:var(--serif);font-weight:600;font-size:22px;margin:0 0 2px}
.cartao .leg{font-size:12px;color:var(--suave);letter-spacing:.04em;margin-bottom:18px}
.cab{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;flex-wrap:wrap}

/* placar */
.placar{display:grid;grid-template-columns:1fr 1fr;gap:12px}
.cand.dir{text-align:right}
.cand .num{display:inline-block;font-size:11px;font-weight:700;color:#fff;border-radius:6px;padding:2px 7px}
.cand .nome{font-size:13px;font-weight:600;letter-spacing:.08em;text-transform:uppercase;margin-top:8px}
.cand .pc{font-family:var(--serif);font-size:46px;font-weight:600;line-height:1;margin-top:6px;letter-spacing:-1px}
.cand .vt{font-size:13px;color:var(--suave);margin-top:6px}
.frente{display:inline-block;margin-left:6px;font-size:10px;letter-spacing:.14em;text-transform:uppercase;
  color:var(--verde);border:1px solid var(--verde);border-radius:999px;padding:1px 8px;vertical-align:middle}
.barra{position:relative;height:14px;border-radius:999px;overflow:hidden;background:#EEE9DC;margin-top:18px;display:flex}
.barra b{display:block;height:100%;transition:width .8s ease}
.meio{position:absolute;left:50%;top:-6px;bottom:-6px;width:2px;background:var(--tinta);opacity:.55}
.marca50{text-align:center;font-size:10px;letter-spacing:.14em;color:var(--suave);margin-top:6px}
.espera{background:#FFFBE0;border:1px dashed #E8C900;color:#5B4B00;border-radius:12px;padding:12px 14px;font-size:13px;margin-bottom:16px}

/* mapas */
.mapa{position:relative;width:100%}
.mapa svg{width:100%;height:auto;display:block}
.mapa path.uf{stroke:#fff;stroke-width:1.2;transition:fill .6s, opacity .2s;cursor:pointer}
.mapa path.uf:hover{stroke:var(--tinta);stroke-width:2}
.mapa path.pais{fill:#ECE7DA;stroke:#fff;stroke-width:.6;vector-effect:non-scaling-stroke}
.mapa circle{stroke:#fff;stroke-width:1.2;vector-effect:non-scaling-stroke;fill-opacity:.9;cursor:pointer;transition:r .4s}
.mapa circle:hover{stroke:var(--tinta);stroke-width:2;fill-opacity:1}
.mapa svg.mundo{border-radius:12px;background:linear-gradient(180deg,#F4F7FB,#EEF3F8);transition:all .3s}
.mapa .rot-uf{font-size:11px;font-weight:700;fill:var(--tinta);opacity:.55;pointer-events:none;text-anchor:middle;font-family:var(--sans)}
.dica{position:fixed;pointer-events:none;background:var(--tinta);color:#fff;font-size:12px;line-height:1.5;
  padding:8px 11px;border-radius:8px;box-shadow:0 8px 20px rgba(0,0,0,.25);z-index:9999;display:none;max-width:260px}
.dica b{font-weight:600}
.legenda{display:flex;align-items:center;justify-content:center;gap:12px;flex-wrap:wrap;margin-top:14px;font-size:11px;color:var(--suave)}
.leg-lado{display:flex;align-items:center;gap:6px}
.leg-nome{font-weight:600;letter-spacing:.06em;text-transform:uppercase;font-size:10px}
.leg-cores{display:flex} .leg-cores span{width:22px;height:9px;display:block}
.leg-cores span:first-child{border-radius:3px 0 0 3px} .leg-cores span:last-child{border-radius:0 3px 3px 0}
.leg-meio{font-size:10px;letter-spacing:.06em}
.zoom{display:flex;flex-wrap:wrap;gap:6px;margin-bottom:12px}
.zoom button, .abas button{border:1px solid var(--linha);background:#fff;color:var(--tinta);font-size:12px;
  padding:5px 12px;border-radius:999px;cursor:pointer;font-family:var(--sans)}
.zoom button.ativo, .abas button.ativo{background:var(--azul);color:#fff;border-color:var(--azul)}

/* tabelas ordenáveis */
.abas{display:flex;gap:6px}
.rolagem{max-height:520px;overflow:auto;border-radius:10px}
table.ord{width:100%;border-collapse:collapse;font-size:13px;font-variant-numeric:tabular-nums}
table.ord th{position:sticky;top:0;z-index:1;background:#fff;text-align:right;font-size:10.5px;letter-spacing:.1em;
  text-transform:uppercase;color:var(--suave);padding:9px 8px;border-bottom:2px solid var(--tinta);
  cursor:pointer;user-select:none;white-space:nowrap}
table.ord th:hover{color:var(--tinta)}
table.ord th::after{content:'↕';margin-left:4px;opacity:.3;font-size:10px}
table.ord th.asc::after{content:'▲';opacity:1;color:var(--verde)}
table.ord th.desc::after{content:'▼';opacity:1;color:var(--verde)}
table.ord th.txt, table.ord td.txt{text-align:left}
table.ord td{padding:7px 8px;border-bottom:1px solid var(--linha);text-align:right;white-space:nowrap}
table.ord td.txt{white-space:normal}
table.ord td .pais{display:block;font-size:11px;color:var(--suave)}
table.ord tr:hover td{background:#FBF9F3}
table.ord td.forte{font-weight:600}
.mini{display:inline-flex;width:64px;height:6px;border-radius:999px;overflow:hidden;background:#EEE9DC;vertical-align:middle}
.mini b{display:block;height:100%}
.dicatab{font-size:11px;color:var(--suave);margin:-8px 0 12px}

.rodape{text-align:center;font-size:12px;color:var(--suave);margin-top:30px;line-height:1.7}
.rodape .faixa{width:120px;margin:0 auto 12px;border-radius:3px;overflow:hidden;height:4px}
.autoria{margin-top:14px;font-size:11px;letter-spacing:.04em;color:#8A93A0}
.autoria a{color:var(--azul);text-decoration:none;border-bottom:1px solid var(--amarelo)}
.autoria a:hover{color:var(--verde)}

@media (max-width:640px){
  .topo{padding:26px 16px 22px} .marca{font-size:42px}
  .corpo{padding:20px 14px 40px} .grade2{grid-template-columns:1fr} .cartao{padding:18px 14px}
  .cand .pc{font-size:36px} .ocultar-cel{display:none}
  table.ord{font-size:12px} table.ord th, table.ord td{padding:7px 5px}
}
"

# JavaScript: ordenar tabelas, abas, zoom do mapa e dicas (tooltip)
js <- "
window.apOrdem = window.apOrdem || {};
window.apAba = window.apAba || 'paises';
window.apZoom = window.apZoom || 'Mundo';

function apAplicaOrdem(tb){
  const st = window.apOrdem[tb.id]; if(!st) return;
  const ths = tb.querySelectorAll('th');
  ths.forEach(t => t.classList.remove('asc','desc'));
  const th = ths[st.k]; if(!th) return;
  th.classList.add(st.d > 0 ? 'asc' : 'desc');
  const corpo = tb.tBodies[0]; const linhas = Array.from(corpo.rows);
  const txt = th.classList.contains('txt');
  linhas.sort((a,b) => {
    const va = a.cells[st.k].dataset.v, vb = b.cells[st.k].dataset.v;
    return txt ? st.d * va.localeCompare(vb, 'pt') : st.d * (parseFloat(va) - parseFloat(vb));
  });
  linhas.forEach(l => corpo.appendChild(l));
}
function apAplicaAba(){
  document.querySelectorAll('.abas button').forEach(b => b.classList.toggle('ativo', b.dataset.aba === window.apAba));
  document.querySelectorAll('[data-painel]').forEach(p => p.style.display = (p.dataset.painel === window.apAba ? '' : 'none'));
}
function apAplicaZoom(){
  const svg = document.querySelector('svg.mundo'); if(!svg) return;
  const bt = document.querySelector('.zoom button[data-z=\"' + window.apZoom + '\"]'); if(!bt) return;
  document.querySelectorAll('.zoom button').forEach(b => b.classList.toggle('ativo', b === bt));
  svg.setAttribute('viewBox', bt.dataset.vb);
  const larg = parseFloat(bt.dataset.vb.split(' ')[2]);
  const f = Math.pow(larg / 1440, 0.75);
  svg.querySelectorAll('circle').forEach(c => c.setAttribute('r', (parseFloat(c.dataset.r0) * f).toFixed(2)));
}
function apTudo(){ document.querySelectorAll('table.ord').forEach(apAplicaOrdem); apAplicaAba(); apAplicaZoom(); }

document.addEventListener('click', function(e){
  const th = e.target.closest('table.ord th');
  if (th) {
    const tb = th.closest('table'); const k = Array.from(th.parentNode.children).indexOf(th);
    const st = window.apOrdem[tb.id];
    const d = (st && st.k === k) ? -st.d : (th.classList.contains('txt') ? 1 : -1);
    window.apOrdem[tb.id] = {k: k, d: d}; apAplicaOrdem(tb); return;
  }
  const ab = e.target.closest('.abas button');
  if (ab) { window.apAba = ab.dataset.aba; apAplicaAba(); return; }
  const z = e.target.closest('.zoom button');
  if (z) { window.apZoom = z.dataset.z; apAplicaZoom(); return; }
});

// dica ao passar o mouse nos mapas
document.addEventListener('mousemove', function(e){
  const dica = document.getElementById('dica'); if(!dica) return;
  const alvo = e.target.closest('[data-dica]');
  if (!alvo) { dica.style.display = 'none'; return; }
  dica.innerHTML = alvo.dataset.dica; dica.style.display = 'block';
  const x = Math.min(e.clientX + 14, window.innerWidth - dica.offsetWidth - 8);
  dica.style.left = x + 'px'; dica.style.top = (e.clientY + 14) + 'px';
});

$(document).on('shiny:value', function(){ setTimeout(apTudo, 0); });
"

faixa <- function() div(class = "faixa", tags$i(), tags$i(), tags$i(), tags$i())

# ---- componentes ---------------------------------------------
cor_cand <- function(n) if (n == "13") "var(--c13)" else if (n == "22") "var(--c22)" else "#888"

barra_dupla <- function(p1, p2, c1, c2, classe = "barra") {
  tot <- p1 + p2
  if (is.na(tot) || tot <= 0) return(div(class = classe, if (classe == "barra") div(class = "meio")))
  w1 <- 100 * p1 / tot
  div(class = classe,
      tags$b(style = sprintf("width:%.3f%%;background:%s", w1, c1)),
      tags$b(style = sprintf("width:%.3f%%;background:%s", 100 - w1, c2)),
      if (classe == "barra") div(class = "meio"))
}

placar <- function(d, nm, aviso = NULL) {
  ordem <- sort(names(nm))
  get <- function(n, col) if (is.null(d)) NA else d[[col]][d$numero == n][1]
  vot <- sapply(ordem, get, col = "votos"); pc <- sapply(ordem, get, col = "pct")
  lider <- if (all(!is.na(vot)) && vot[1] != vot[2]) ordem[which.max(vot)] else ""
  bloco <- function(n, k) {
    div(class = paste("cand", if (k == 2) "dir"),
        span(class = "num", style = paste0("background:", cor_cand(n)), n),
        if (n == lider) span(class = "frente", "à frente"),
        div(class = "nome", nm[[n]]),
        div(class = "pc", style = paste0("color:", cor_cand(n)), pctf(pc[k])),
        div(class = "vt", if (is.na(vot[k])) "aguardando votos" else paste(fmt(vot[k]), "votos")))
  }
  tagList(
    if (!is.null(aviso)) div(class = "espera", aviso),
    div(class = "placar", bloco(ordem[1], 1), bloco(ordem[2], 2)),
    barra_dupla(ifelse(is.na(pc[1]), 0, pc[1]), ifelse(is.na(pc[2]), 0, pc[2]),
                cor_cand(ordem[1]), cor_cand(ordem[2])),
    div(class = "marca50", "50% DOS VOTOS VÁLIDOS")
  )
}

# Tabela ordenável. d: data.frame com colunas local, (sub), v1, v2
tabela_ord <- function(id, d, nm, rotulo, sub = NULL, extra = NULL) {
  o <- sort(names(nm))
  d$tot <- d$v1 + d$v2
  d$p1 <- ifelse(d$tot > 0, 100 * d$v1 / d$tot, NA)
  d$p2 <- ifelse(d$tot > 0, 100 * d$v2 / d$tot, NA)
  c1 <- cor_cand(o[1]); c2 <- cor_cand(o[2])
  curto <- function(x) gsub(" .*", "", x)       # "FLAVIO BOLSONARO" -> "FLAVIO"
  cab <- tags$tr(
    tags$th(class = "txt", rotulo),
    tags$th(style = paste0("color:", c1), curto(nm[[o[1]]])),
    tags$th(style = paste0("color:", c1), "%"),
    tags$th(style = paste0("color:", c2), curto(nm[[o[2]]])),
    tags$th(style = paste0("color:", c2), "%"),
    tags$th("Total"),
    if (!is.null(extra)) tags$th(class = "ocultar-cel", extra$titulo),
    tags$th(class = "ocultar-cel", style = "cursor:default", "")
  )
  linhas <- lapply(seq_len(nrow(d)), function(i) {
    p1 <- d$p1[i]; p2 <- d$p2[i]
    tags$tr(
      tags$td(class = "txt", `data-v` = d$local[i], d$local[i],
              if (!is.null(sub)) span(class = "pais", d[[sub]][i])),
      tags$td(`data-v` = d$v1[i], fmt(d$v1[i])),
      tags$td(`data-v` = ifelse(is.na(p1), -1, p1), class = if (isTRUE(p1 > 50)) "forte",
              style = paste0("color:", c1), pctf(p1, 1)),
      tags$td(`data-v` = d$v2[i], fmt(d$v2[i])),
      tags$td(`data-v` = ifelse(is.na(p2), -1, p2), class = if (isTRUE(p2 > 50)) "forte",
              style = paste0("color:", c2), pctf(p2, 1)),
      tags$td(`data-v` = d$tot[i], fmt(d$tot[i])),
      if (!is.null(extra)) tags$td(class = "ocultar-cel", `data-v` = d[[extra$col]][i],
                                   extra$fmt(d[[extra$col]][i])),
      tags$td(class = "ocultar-cel", `data-v` = 0, barra_dupla(d$v1[i], d$v2[i], c1, c2, "mini"))
    )
  })
  div(class = "rolagem", tags$table(class = "ord", id = id, tags$thead(cab), tags$tbody(linhas)))
}

dica_html <- function(titulo, sub, nm, v1, v2) {
  o <- sort(names(nm)); tot <- v1 + v2
  p <- function(v) if (tot > 0) pctf(100 * v / tot) else "—"
  paste0("<b>", htmltools::htmlEscape(titulo), "</b>",
         if (!is.null(sub)) paste0("<br><span style='opacity:.7'>", htmltools::htmlEscape(sub), "</span>"),
         "<br>", nm[[o[1]]], ": ", fmt(v1), " (", p(v1), ")",
         "<br>", nm[[o[2]]], ": ", fmt(v2), " (", p(v2), ")")
}

# =============================================================
ui <- fluidPage(
  title = "Apuraí Eleições 2026",
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$meta(name = "theme-color", content = "#002776"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Inter:wght@400;500;600;700&display=swap"),
    tags$style(HTML(css)),
    tags$script(HTML(js))
  ),
  div(id = "dica", class = "dica"),
  div(class = "topo", div(class = "topo-in",
      div(h1(class = "marca", HTML("Apur<span>aí</span>")),
          div(class = "sub", "Eleições 2026 · 2º turno · Presidente da República")),
      div(class = "status",
          div(class = "aovivo", span(class = "ponto"), textOutput("relogio", inline = TRUE)),
          actionButton("atualizar", "Atualizar agora", class = "btn-atualizar")))),
  faixa(),
  div(class = "corpo",
      div(class = "kpis",
          div(class = "kpi", div(class = "rot", "Seções totalizadas · Total"),
              div(class = "val", textOutput("secoes_br", inline = TRUE)),
              div(class = "nota", textOutput("nota_br", inline = TRUE))),
          div(class = "kpi", div(class = "rot", "Seções apuradas · Exterior"),
              div(class = "val", textOutput("secoes_zz", inline = TRUE)),
              div(class = "nota", textOutput("fonte_zz", inline = TRUE))),
          div(class = "kpi", div(class = "rot", "Última atualização do TSE"),
              div(class = "val", style = "font-size:22px", textOutput("hora", inline = TRUE)),
              div(class = "nota", paste("Atualiza a cada", INTERVALO, "segundos")))),
      div(class = "grade2",
          div(class = "cartao", h3("Brasil"),
              div(class = "leg", "Total nacional, incluindo os votos do exterior"),
              uiOutput("placar_br")),
          div(class = "cartao", h3("Exterior"),
              div(class = "leg", textOutput("leg_zz", inline = TRUE)),
              uiOutput("placar_zz"))),
      div(class = "grade2",
          div(class = "cartao", h3("Mapa do Brasil"),
              div(class = "leg", "Quem venceu em cada estado e por quanto · passe o mouse para ver os números"),
              uiOutput("mapa_br")),
          div(class = "cartao", h3("Mapa do exterior"),
              div(class = "leg", "Cada círculo é uma cidade com seção eleitoral · tamanho = votos"),
              uiOutput("mapa_mu"))),
      div(class = "grade2",
          div(class = "cartao", h3("Estados"),
              div(class = "leg", "Clique no nome de uma coluna para ordenar"),
              uiOutput("tab_uf")),
          div(class = "cartao",
              div(class = "cab",
                  div(h3("Exterior"), div(class = "leg", "Clique no nome de uma coluna para ordenar")),
                  div(class = "abas",
                      tags$button(`data-aba` = "paises", class = "ativo", "Por país"),
                      tags$button(`data-aba` = "cidades", "Por cidade"))),
              uiOutput("tab_ext"))),
      div(class = "rodape", faixa(),
          "Fonte: Tribunal Superior Eleitoral — resultados.tse.jus.br", br(),
          "Resultados parciais até o fim da totalização. Antes das 17h (Brasília), o exterior é a soma dos boletins de urna publicados pelo TSE.",
          div(class = "autoria", "Desenvolvido por Caroline C. Vasconcelos · ",
              tags$a(href = "https://github.com/ccvasconcelos/apurai", target = "_blank",
                     "github.com/ccvasconcelos/apurai"))))
)

# =============================================================
server <- function(input, output, session) {

  info <- descobrir_eleicao()

  dados <- reactive({
    invalidateLater(INTERVALO * 1000)
    input$atualizar
    tot <- ler_totais(info, c("br", "zz", ufs))
    bu  <- tryCatch(atualizar_bu_exterior(info), error = function(e) NULL)
    list(tot = tot, bu = bu, quando = format(Sys.time(), "%H:%M:%S"))
  })

  nomes <- reactive({
    t <- dados()$tot
    if (!nrow(t)) return(c("13" = "LULA", "22" = "FLAVIO BOLSONARO"))
    x <- distinct(t, numero, nome); setNames(x$nome, x$numero)
  })

  total_de <- function(abr) {
    t <- dados()$tot
    if (!nrow(t)) return(NULL)
    filter(t, local == toupper(abr))
  }
  liberado <- function(d) !is.null(d) && nrow(d) > 0 && d$st[1] > 0

  # votos do exterior por cidade (boletins de urna), com país e coordenadas
  ext_cidades <- reactive({
    bu <- dados()$bu
    if (is.null(bu) || !nrow(bu$votos) || is.null(bu$votos$mu)) return(NULL)
    o <- sort(names(nomes()))
    bu$votos |>
      filter(numero %in% o) |>
      group_by(mu, cidade) |>
      summarise(v1 = sum(votos[numero == o[1]]), v2 = sum(votos[numero == o[2]]), .groups = "drop") |>
      left_join(cidades_ext, by = c("mu" = "codigo"), suffix = c("_tse", "")) |>
      mutate(local = ifelse(is.na(cidade), titulo(cidade_tse), cidade),
             pais  = ifelse(is.na(pais), "—", pais))
  })

  # por UF (totalização oficial)
  por_uf <- reactive({
    d <- dados()$tot |> filter(local %in% toupper(ufs), st > 0)
    if (!nrow(d)) return(NULL)
    o <- sort(names(nomes()))
    d |> group_by(local) |>
      summarise(v1 = sum(votos[numero == o[1]]), v2 = sum(votos[numero == o[2]]),
                secoes = first(secoes), .groups = "drop")
  })

  exterior <- reactive({
    of <- total_de("zz")
    if (liberado(of)) return(list(d = of, fonte = "Totalização oficial",
                                  secoes = pctf(of$secoes[1])))
    bu <- dados()$bu
    if (is.null(bu) || !nrow(bu$votos)) return(NULL)
    ts <- if (!is.null(of) && nrow(of)) of$ts[1] else NA
    nm <- nomes()
    d <- bu$votos |>
      filter(numero %in% names(nm)) |>
      group_by(numero) |> summarise(votos = sum(votos), .groups = "drop") |>
      mutate(nome = nm[numero], pct = 100 * votos / sum(votos))
    list(d = d, fonte = "Soma dos boletins de urna",
         secoes = if (is.na(ts)) fmt(bu$lidas) else paste(fmt(bu$lidas), "de", fmt(ts)))
  })

  # ---- textos -------------------------------------------------
  output$relogio <- renderText(paste("Ao vivo ·", dados()$quando))
  output$secoes_br <- renderText({
    d <- total_de("br"); if (liberado(d)) pctf(d$secoes[1]) else "—"
  })
  output$nota_br <- renderText({
    d <- total_de("br")
    if (liberado(d)) paste(fmt(d$st[1]), "de", fmt(d$ts[1]), "seções")
    else "Liberação às 17h de Brasília (16h em Manaus)"
  })
  output$secoes_zz <- renderText({ e <- exterior(); if (is.null(e)) "—" else e$secoes })
  output$fonte_zz  <- renderText({ e <- exterior(); if (is.null(e)) "" else e$fonte })
  output$leg_zz    <- renderText({
    e <- exterior()
    if (is.null(e)) "Aguardando os primeiros boletins de urna"
    else paste("Votos de brasileiros fora do país ·", tolower(e$fonte))
  })
  output$hora <- renderText({
    d <- total_de("br")
    if (liberado(d)) d$hora[1] else if (!is.null(dados()$bu)) dados()$bu$hora else "—"
  })

  # ---- placares ----------------------------------------------
  output$placar_br <- renderUI({
    d <- total_de("br")
    if (liberado(d)) placar(d, nomes())
    else placar(NULL, nomes(), aviso = "A totalização oficial começa às 17h de Brasília (16h em Manaus).")
  })
  output$placar_zz <- renderUI({
    e <- exterior(); placar(if (is.null(e)) NULL else e$d, nomes())
  })

  # ---- mapa do Brasil ----------------------------------------
  output$mapa_br <- renderUI({
    nm <- nomes(); d <- por_uf()
    p13 <- setNames(rep(NA_real_, nrow(mapa_br)), mapa_br$id)
    if (!is.null(d)) p13[d$local] <- ifelse(d$v1 + d$v2 > 0, 100 * d$v1 / (d$v1 + d$v2), NA)
    cores <- cor_margem(p13[mapa_br$id])
    caminhos <- vapply(seq_len(nrow(mapa_br)), function(i) {
      uf <- mapa_br$id[i]
      x <- if (!is.null(d)) d[d$local == uf, ] else NULL
      dica <- if (!is.null(x) && nrow(x))
        dica_html(mapa_br$nome[i], paste(pctf(x$secoes, 1), "das seções"), nm, x$v1, x$v2)
      else paste0("<b>", mapa_br$nome[i], "</b><br>aguardando totalização")
      sprintf('<path class="uf" d="%s" fill="%s" data-dica="%s"></path>',
              mapa_br$d[i], cores[i], htmltools::htmlEscape(dica, attribute = TRUE))
    }, character(1))
    tagList(
      if (is.null(d)) div(class = "espera", "O mapa se colore quando o TSE liberar a totalização (17h de Brasília)."),
      div(class = "mapa", HTML(sprintf('<svg viewBox="%s" role="img" aria-label="Mapa do Brasil por UF">%s</svg>',
                                       VB_BR, paste(caminhos, collapse = "")))),
      legenda_mapa(nm))
  })

  # ---- mapa do exterior --------------------------------------
  output$mapa_mu <- renderUI({
    nm <- nomes(); d <- ext_cidades()
    fundo <- paste(sprintf('<path class="pais" d="%s"></path>', mapa_mu$d), collapse = "")
    circulos <- ""
    if (!is.null(d)) {
      d <- d |> filter(!is.na(lat), v1 + v2 > 0) |> arrange(desc(v1 + v2))  # grandes embaixo
      if (nrow(d)) {
        r0 <- 2.2 + 16 * sqrt((d$v1 + d$v2) / max(d$v1 + d$v2))
        p13 <- 100 * d$v1 / (d$v1 + d$v2)
        xy <- t(mapply(proj_mu, d$lon, d$lat))
        dicas <- mapply(function(a, b, c, e) dica_html(a, b, nm, c, e), d$local, d$pais, d$v1, d$v2)
        circulos <- paste(sprintf(
          '<circle cx="%.1f" cy="%.1f" r="%.2f" data-r0="%.2f" fill="%s" data-dica="%s"></circle>',
          xy[, 1], xy[, 2], r0, r0, cor_margem(p13),
          htmltools::htmlEscape(dicas, attribute = TRUE)), collapse = "")
      }
    }
    tagList(
      div(class = "zoom", lapply(names(zooms), function(z)
        tags$button(`data-z` = z, `data-vb` = zooms[[z]], class = if (z == "Mundo") "ativo", z))),
      if (is.null(d)) div(class = "espera", "Aguardando os primeiros boletins de urna do exterior."),
      div(class = "mapa", HTML(sprintf('<svg class="mundo" viewBox="%s" role="img" aria-label="Mapa do exterior">%s%s</svg>',
                                       zooms[["Mundo"]], fundo, circulos))),
      legenda_mapa(nm))
  })

  # ---- tabelas ordenáveis ------------------------------------
  output$tab_uf <- renderUI({
    d <- por_uf()
    if (is.null(d)) return(div(class = "espera", "A tabela por estado aparece quando o TSE liberar a totalização."))
    nomes_uf <- setNames(mapa_br$nome, mapa_br$id)
    d <- d |> mutate(regiao = paste0(nomes_uf[local], " · ", regioes[local]))
    tabela_ord("t_uf", d, nomes(), "Estado", sub = "regiao",
               extra = list(titulo = "Seções", col = "secoes", fmt = function(x) pctf(x, 1)))
  })

  output$tab_ext <- renderUI({
    d <- ext_cidades()
    if (is.null(d)) return(div(class = "espera", "Aguardando os primeiros boletins de urna do exterior."))
    paises <- d |> group_by(pais) |>
      summarise(v1 = sum(v1), v2 = sum(v2), n = n(), .groups = "drop") |>
      mutate(local = pais, cidades = ifelse(n == 1, "1 cidade", paste(n, "cidades")))
    tagList(
      div(`data-painel` = "paises",
          tabela_ord("t_paises", arrange(paises, desc(v1 + v2)), nomes(), "País", sub = "cidades")),
      div(`data-painel` = "cidades", style = "display:none",
          tabela_ord("t_cidades", arrange(d, desc(v1 + v2)), nomes(), "Cidade", sub = "pais")))
  })
}

shinyApp(ui, server)
