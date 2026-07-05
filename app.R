# =====================================================================
#  App Shiny — Estimación interválica con dos muestras (con interpretación)
#  Los alumnos pueden usar el ejemplo, PEGAR sus datos o SUBIR un CSV.
#  Bioestadística · Maestría en Salud Pública · UNAP
# =====================================================================
library(shiny)

TEAL <- "#0E7C86"; CORAL <- "#E4572E"; GRAY <- "#5B6B70"; DEEP <- "#0A3D4A"

css <- "
  body{font-family:Segoe UI,Roboto,sans-serif;color:#1E2A2E}
  .titlebox{background:#0A3D4A;color:#fff;padding:14px 18px;border-radius:0 0 10px 10px;margin-bottom:10px}
  .titlebox h2{margin:0;font-size:20px}
  .titlebox p{margin:3px 0 0;font-size:13px;color:#8FD9CE}
  .well{background:#F2F7F7;border:none;border-radius:12px}
  .concept{background:#E3F1EF;border-left:5px solid #0E7C86;padding:10px 14px;border-radius:8px;margin-bottom:12px;font-size:13.5px}
  .concept b{color:#0A3D4A}
  .interp{background:#fff;border:1px solid #dfeaea;border-radius:10px;padding:12px 14px;margin-top:12px;font-size:14px;line-height:1.65}
  .interp .lead{font-weight:700;color:#0A3D4A}
  .verdict{padding:9px 12px;border-radius:9px;font-weight:600;margin-top:10px}
  .yes{background:#E3F1EF;color:#0A3D4A}
  .no{background:#fdece7;color:#E4572E}
  .err{background:#fdece7;color:#E4572E;padding:10px 12px;border-radius:9px;font-weight:600}
  .big{font-size:22px;font-weight:700;color:#0A3D4A}
  .nav-tabs>li.active>a{color:#0E7C86 !important;font-weight:700}
"

ci_diff <- function(diff, ee, df, level){ tc <- qt(1-(1-level)/2, df); c(diff-tc*ee, diff+tc*ee, tc) }
labD <- function(x){a<-abs(x); if(a<0.2)"trivial" else if(a<0.5)"pequeño" else if(a<0.8)"pequeño-mediano" else if(a<1.1)"grande" else "muy grande"}
parse_nums <- function(txt){
  if(is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
  x <- suppressWarnings(as.numeric(strsplit(trimws(txt), "[ ,;\t\r\n]+")[[1]])); x[!is.na(x)]
}
fuente <- function(id, opciones){ radioButtons(id, "Origen de los datos", opciones, inline=TRUE) }

# ---- Datasets de ejemplo (deterministas) para descargar y practicar ----
set.seed(2026)
.pre <- round(rnorm(15,150,12))
ej_presion <- data.frame(
  paciente = 1:15,
  pre_PAS  = .pre,
  post_PAS = .pre - round(rnorm(15,9,4))       # mismo paciente, con una baja real
)                                   # pareadas: presión sistólica antes/después
ej_glucosa <- data.frame(
  grupo   = rep(c("Control","Dieta"), each=18),
  glucosa = round(c(rnorm(18,132,16), rnorm(18,118,15)))
)                                   # independientes: glucosa por grupo
ej_epi <- data.frame(
  exposicion = rep(c("Fumador","No fumador"), c(95,105)),
  evento     = c(rep(c("EPOC","Sano"), c(45,50)),
                 rep(c("EPOC","Sano"), c(25,80))))  # 2x2: tabaquismo × EPOC

glosario_tab <- "
<table style='font-size:13.5px'>
<tr><th>Término</th><th>Qué significa</th></tr>
<tr><td><b>Intervalo de confianza (IC)</b></td><td>Rango plausible del valor real del parámetro, con 95% de confianza.</td></tr>
<tr><td><b>Media / Desviación</b></td><td>Promedio del grupo / cuánto se dispersan los datos alrededor de la media.</td></tr>
<tr><td><b>Error estándar</b></td><td>Cuánto varía el estimador de muestra en muestra (baja al crecer n).</td></tr>
<tr><td><b>Independientes vs. pareadas</b></td><td>Grupos de personas distintas vs. las mismas personas medidas dos veces.</td></tr>
<tr><td><b>Riesgo</b></td><td>Probabilidad del evento en un grupo: casos / total del grupo.</td></tr>
<tr><td><b>RR (riesgo relativo)</b></td><td>Cociente de riesgos. RR=2 → el doble de riesgo. Referencia sin efecto: 1.</td></tr>
<tr><td><b>OR (odds ratio)</b></td><td>Cociente de odds. Obligatorio en casos y controles. Referencia sin efecto: 1.</td></tr>
<tr><td><b>RD (dif. de riesgos)</b></td><td>Exceso absoluto de riesgo por la exposición (riesgo expuestos − no expuestos).</td></tr>
<tr><td><b>NNH</b></td><td>Personas expuestas por cada caso adicional (1 / |RD|).</td></tr>
<tr><td><b>Valor \"sin efecto\"</b></td><td><b>0</b> para diferencias; <b>1</b> para cocientes (OR/RR).</td></tr>
</table>"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class="titlebox",
      h2("Estimación interválica con dos muestras"),
      p("Interactiva e interpretada · usa el ejemplo, pega tus datos o sube un CSV · UNAP")),
  tabsetPanel(
    # ================= PARTE A =================
    tabPanel("A · Medias independientes",
      sidebarLayout(
        sidebarPanel(class="well",
          fuente("Amode", c("Resumen (n, media, sd)"="resumen","Pegar datos"="pegar","Subir CSV"="csv")),
          conditionalPanel("input.Amode=='resumen'",
            fluidRow(column(6, numericInput("An1","Grupo 1 · n",115)), column(6, numericInput("An2","Grupo 2 · n",74))),
            fluidRow(column(6, numericInput("Am1","Media 1",3055.7)), column(6, numericInput("Am2","Media 2",2771.9))),
            fluidRow(column(6, numericInput("As1","Desv. 1",752.7)), column(6, numericInput("As2","Desv. 2",659.6)))),
          conditionalPanel("input.Amode=='pegar'",
            textAreaInput("Apaste1","Valores del Grupo 1 (espacio/coma/salto de línea)","", rows=3),
            textAreaInput("Apaste2","Valores del Grupo 2","", rows=3)),
          conditionalPanel("input.Amode=='csv'",
            fileInput("Afile","Sube un CSV", accept=".csv"), uiOutput("AcolUI")),
          selectInput("Alevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          checkboxInput("Awelch","Usar Welch (recomendado)",TRUE),
          helpText("Ejemplo: peso al nacer (g) según tabaquismo materno.")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> El promedio de <b>dos grupos de personas distintas</b>. El IC da el rango plausible de la <b>diferencia de medias</b>. \"Sin efecto\": el <b>0</b>.")),
          htmlOutput("Aout"), plotOutput("Aplot",height="250px")))),
    # ================= PARTE B =================
    tabPanel("B · Medias pareadas",
      sidebarLayout(
        sidebarPanel(class="well",
          fuente("Bmode", c("Resumen (n, media, sd)"="resumen","Pegar pre y post"="pegar","Subir CSV"="csv")),
          conditionalPanel("input.Bmode=='resumen'",
            numericInput("Bn","N.º de pares (n)",10),
            numericInput("Bmd","Media de las diferencias (post − pre)",1.58),
            numericInput("Bsd","Desv. de las diferencias",1.23)),
          conditionalPanel("input.Bmode=='pegar'",
            textAreaInput("Bpre","Medida ANTES (pre), un valor por sujeto","", rows=3),
            textAreaInput("Bpost","Medida DESPUÉS (post), mismo orden","", rows=3)),
          conditionalPanel("input.Bmode=='csv'",
            fileInput("Bfile","Sube un CSV", accept=".csv"), uiOutput("BcolUI")),
          selectInput("Blevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          helpText("Ejemplo: somníferos de Student (1908).")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> <b>Dos medidas en las mismas personas</b> (antes–después). Se analiza la <b>diferencia de cada sujeto</b>. \"Sin efecto\": el <b>0</b>.")),
          htmlOutput("Bout"), plotOutput("Bplot",height="190px")))),
    # ================= PARTE C =================
    tabPanel("C · OR y RR (tabla 2×2)",
      sidebarLayout(
        sidebarPanel(class="well",
          fuente("Cmode", c("Escribir tabla 2×2"="manual","Subir CSV"="csv")),
          conditionalPanel("input.Cmode=='manual'",
            fluidRow(column(6, numericInput("Ca","a · exp. evento",30)), column(6, numericInput("Cb","b · exp. sin evento",44))),
            fluidRow(column(6, numericInput("Cc","c · no exp. evento",29)), column(6, numericInput("Cd","d · no exp. sin evento",86)))),
          conditionalPanel("input.Cmode=='csv'",
            fileInput("Cfile","Sube un CSV", accept=".csv"), uiOutput("CcolUI")),
          selectInput("Clevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          helpText("Ejemplo: tabaquismo materno × bajo peso al nacer.")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> Si una <b>exposición</b> se asocia con un <b>evento</b>. <b>RR</b> compara riesgos; <b>OR</b> compara odds. \"Sin efecto\": el <b>1</b>. Además <b>RD</b> y <b>NNH</b>.")),
          htmlOutput("Cout"), plotOutput("Cplot",height="230px")))),
    # ================= GLOSARIO Y TRABAJOS =================
    tabPanel("📚 Glosario y trabajos",
      fluidRow(
        column(5,
          div(class="concept", HTML("<b>Glosario de conceptos</b> — consulta rápida mientras usas la calculadora.")),
          HTML(glosario_tab)),
        column(7,
          h4("Datasets de ejemplo"),
          p(style="font-size:13px;color:#5B6B70","Descárgalos y súbelos con \"Subir CSV\" en la pestaña indicada (o ábrelos para ver sus variables)."),
          wellPanel(class="well",
            HTML("<b>presion_pre_post.csv</b> → pestaña <b>B (pareadas)</b><br>
                  <span style='font-size:12.5px;color:#5B6B70'>Variables: <b>paciente</b> (id), <b>pre_PAS</b> (presión sistólica antes), <b>post_PAS</b> (después). Usa pre_PAS como PRE y post_PAS como POST.</span>"),
            downloadButton("dl_presion","Descargar CSV")),
          wellPanel(class="well",
            HTML("<b>glucosa_dos_grupos.csv</b> → pestaña <b>A (independientes)</b><br>
                  <span style='font-size:12.5px;color:#5B6B70'>Variables: <b>grupo</b> (Control/Dieta), <b>glucosa</b> (mg/dL). Variable numérica = glucosa; grupo = grupo.</span>"),
            downloadButton("dl_glucosa","Descargar CSV")),
          wellPanel(class="well",
            HTML("<b>tabaquismo_epoc.csv</b> → pestaña <b>C (OR/RR)</b><br>
                  <span style='font-size:12.5px;color:#5B6B70'>Variables: <b>exposicion</b> (Fumador/No fumador), <b>evento</b> (EPOC/Sano). Exposición = exposicion; evento = evento.</span>"),
            downloadButton("dl_epi","Descargar CSV"))
        )),
      hr(),
      h4("Trabajos propuestos (resuélvelos con la app y entrégalos)"),
      HTML("<ol style='line-height:1.7;font-size:14px'>
        <li><b>Independientes.</b> Con <i>glucosa_dos_grupos.csv</i>, estima el IC 95% de la diferencia de glucosa entre Control y Dieta (Welch). ¿El IC incluye 0? Interpreta la magnitud.</li>
        <li><b>Pareadas.</b> Con <i>presion_pre_post.csv</i>, calcula el IC 95% del cambio de presión (post − pre). ¿Baja la presión? ¿Cuánto?</li>
        <li><b>OR/RR.</b> Con <i>tabaquismo_epoc.csv</i>, obtén RR y OR con IC 95%. ¿Hay asociación? Reporta también RD y NNH.</li>
        <li><b>Nivel de confianza.</b> Repite el trabajo 1 al 90% y al 99%. ¿Cómo cambia el ancho del IC? Explica por qué.</li>
        <li><b>OR vs. RR.</b> En el trabajo 3, compara OR y RR. ¿Cuál es mayor? ¿Por qué? ¿Cuál reportarías?</li>
        <li><b>Tus datos.</b> Trae un dato real de tu área (o de ENDES/MINSA), cárgalo (pegar o CSV) y redacta la conclusión con la regla de oro (0 / 1).</li>
      </ol>"),
      p(style="font-size:12px;color:#5B6B70","Entrega: captura de la pantalla de resultados + un párrafo de interpretación por cada trabajo.")
    )
  ),
  div(style="padding:8px 18px;font-size:12px;color:#5B6B70",
      HTML("Regla de oro: si el IC de una <b>diferencia</b> incluye <b>0</b>, o el de un <b>cociente</b> (OR/RR) incluye <b>1</b>, no hay evidencia de efecto."))
)

server <- function(input, output){
  errbox <- function(msg) HTML(sprintf("<div class='err'>%s</div>", msg))
  # descargas de datasets de ejemplo
  output$dl_presion <- downloadHandler("presion_pre_post.csv", function(f) write.csv(ej_presion, f, row.names=FALSE))
  output$dl_glucosa <- downloadHandler("glucosa_dos_grupos.csv", function(f) write.csv(ej_glucosa, f, row.names=FALSE))
  output$dl_epi     <- downloadHandler("tabaquismo_epoc.csv", function(f) write.csv(ej_epi, f, row.names=FALSE))
  Acsv <- reactive({ req(input$Afile); read.csv(input$Afile$datapath, stringsAsFactors=TRUE) })
  Bcsv <- reactive({ req(input$Bfile); read.csv(input$Bfile$datapath, stringsAsFactors=TRUE) })
  Ccsv <- reactive({ req(input$Cfile); read.csv(input$Cfile$datapath, stringsAsFactors=TRUE) })
  output$AcolUI <- renderUI({ df<-Acsv(); num<-names(df)[sapply(df,is.numeric)]
    tagList(selectInput("Aout_col","Variable numérica", num),
            selectInput("Agrp_col","Variable de grupo (2 niveles)", names(df))) })
  output$BcolUI <- renderUI({ df<-Bcsv(); num<-names(df)[sapply(df,is.numeric)]
    tagList(selectInput("Bpre_col","Columna PRE (numérica)", num),
            selectInput("Bpost_col","Columna POST (numérica)", num, selected=tail(num,1))) })
  output$CcolUI <- renderUI({ df<-Ccsv()
    tagList(selectInput("Cexp_col","Exposición (2 niveles)", names(df)),
            selectInput("Cev_col","Evento (2 niveles)", names(df), selected=tail(names(df),1))) })

  # ---------- PARTE A ----------
  Agroups <- reactive({
    if(input$Amode=="resumen"){
      list(n1=input$An1,m1=input$Am1,s1=input$As1,n2=input$An2,m2=input$Am2,s2=input$As2)
    } else if(input$Amode=="pegar"){
      g1<-parse_nums(input$Apaste1); g2<-parse_nums(input$Apaste2)
      if(length(g1)<2||length(g2)<2) return(list(err="Pega al menos 2 valores numéricos en cada grupo."))
      list(n1=length(g1),m1=mean(g1),s1=sd(g1),n2=length(g2),m2=mean(g2),s2=sd(g2))
    } else {
      req(input$Aout_col,input$Agrp_col); df<-Acsv()
      y<-df[[input$Aout_col]]; g<-as.factor(df[[input$Agrp_col]]); lv<-levels(g)
      if(length(lv)<2) return(list(err="La variable de grupo debe tener al menos 2 niveles."))
      x1<-y[g==lv[1]]; x2<-y[g==lv[2]]
      if(sum(!is.na(x1))<2||sum(!is.na(x2))<2) return(list(err="Cada grupo necesita ≥2 datos."))
      list(n1=sum(!is.na(x1)),m1=mean(x1,na.rm=T),s1=sd(x1,na.rm=T),
           n2=sum(!is.na(x2)),m2=mean(x2,na.rm=T),s2=sd(x2,na.rm=T),
           etq=sprintf("Grupos comparados: %s vs %s (%s)", lv[1],lv[2],input$Aout_col))
    }
  })
  Ares <- reactive({ g<-Agroups(); if(!is.null(g$err)) return(g)
    n1<-g$n1;n2<-g$n2;m1<-g$m1;m2<-g$m2;s1<-g$s1;s2<-g$s2; lv<-as.numeric(input$Alevel); diff<-m1-m2
    if(input$Awelch){ ee<-sqrt(s1^2/n1+s2^2/n2)
      df<-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
    } else { sp2<-((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2); ee<-sqrt(sp2)*sqrt(1/n1+1/n2); df<-n1+n2-2 }
    ci<-ci_diff(diff,ee,df,lv); sp2<-((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
    list(diff=diff,lo=ci[1],hi=ci[2],df=df,t=diff/ee,d=diff/sqrt(sp2),lv=lv,
         m=c(m1,m2), e2=c(qt(1-(1-lv)/2,n1-1)*s1/sqrt(n1), qt(1-(1-lv)/2,n2-1)*s2/sqrt(n2)), etq=g$etq)
  })
  output$Aout <- renderUI({ r<-Ares(); if(!is.null(r$err)) return(errbox(r$err))
    inc<-(r$lo<=0 && r$hi>=0); pc<-round(r$lv*100)
    mayor <- if(r$diff>=0) "el Grupo 1 supera al Grupo 2" else "el Grupo 2 supera al Grupo 1"
    interp <- if(inc) sprintf("Como el intervalo <b>incluye el 0</b>, un valor de \"ninguna diferencia\" es plausible: <b>no hay evidencia</b> de que difieran al %d%%.", pc) else
      sprintf("Como el intervalo <b>no incluye el 0</b>, <b>sí hay diferencia</b> al %d%%: en promedio %s en <b>%.2f</b> (entre %.2f y %.2f). Efecto <b>%s</b> (d = %.2f).",
              pc, mayor, abs(r$diff), abs(r$lo), abs(r$hi), labD(r$d), r$d)
    HTML(sprintf("%sDiferencia de medias: <span class='big'>%.2f</span> &nbsp; IC %d%%: <b style='color:%s'>[%.2f ; %.2f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> Con %d%% de confianza, la <b>verdadera diferencia</b> está entre <b>%.2f</b> y <b>%.2f</b>. %s
      <div class='verdict %s'>%s</div></div>",
      ifelse(is.null(r$etq),"",paste0("<div style='font-size:12px;color:#5B6B70;margin-bottom:4px'>",r$etq,"</div>")),
      r$diff, pc, TEAL, r$lo, r$hi, pc, r$lo, r$hi, interp,
      ifelse(inc,"no","yes"), ifelse(inc,"IC incluye 0 → sin evidencia.","IC no incluye 0 → diferencia significativa."))) })
  output$Aplot <- renderPlot({ r<-Ares(); if(!is.null(r$err)) return(NULL)
    yl<-range(c(r$m-r$e2,r$m+r$e2)); pad<-diff(yl)*0.25+1e-9
    plot(1:2,r$m,pch=19,cex=1.8,col=c(TEAL,CORAL),xlim=c(.5,2.5),ylim=yl+c(-pad,pad),
         xaxt="n",xlab="",ylab="Media",main="Medias e IC")
    arrows(1:2,r$m-r$e2,1:2,r$m+r$e2,angle=90,code=3,length=.12,lwd=2,col=c(TEAL,CORAL))
    axis(1,at=1:2,labels=c("Grupo 1","Grupo 2")) })

  # ---------- PARTE B ----------
  Bdiffs <- reactive({
    if(input$Bmode=="resumen"){ list(n=input$Bn, md=input$Bmd, sd=input$Bsd)
    } else if(input$Bmode=="pegar"){
      pre<-parse_nums(input$Bpre); post<-parse_nums(input$Bpost)
      if(length(pre)<2||length(pre)!=length(post)) return(list(err="Pega PRE y POST con la misma cantidad de valores (≥2)."))
      d<-post-pre; list(n=length(d), md=mean(d), sd=sd(d))
    } else {
      req(input$Bpre_col,input$Bpost_col); df<-Bcsv()
      d<-na.omit(df[[input$Bpost_col]]-df[[input$Bpre_col]])
      if(length(d)<2) return(list(err="Se necesitan ≥2 pares completos."))
      list(n=length(d), md=mean(d), sd=sd(d))
    }
  })
  Bres <- reactive({ g<-Bdiffs(); if(!is.null(g$err)) return(g)
    n<-g$n;md<-g$md;sd<-g$sd;lv<-as.numeric(input$Blevel); ee<-sd/sqrt(n); ci<-ci_diff(md,ee,n-1,lv)
    list(md=md,lo=ci[1],hi=ci[2],t=md/ee,dz=md/sd,n=n,lv=lv) })
  output$Bout <- renderUI({ r<-Bres(); if(!is.null(r$err)) return(errbox(r$err))
    inc<-(r$lo<=0 && r$hi>=0); pc<-round(r$lv*100); dir<-if(r$md>=0)"aumenta" else "disminuye"
    interp <- if(inc) sprintf("Como el intervalo <b>incluye el 0</b>, es plausible que no haya cambio: <b>sin evidencia</b> al %d%%.", pc) else
      sprintf("Como el intervalo <b>no incluye el 0</b>, <b>sí hay cambio</b> al %d%%: la medida <b>%s %.2f</b> por sujeto (entre %.2f y %.2f). Efecto <b>%s</b> (d<sub>z</sub>=%.2f).",
              pc, dir, abs(r$md), r$lo, r$hi, labD(r$dz), r$dz)
    HTML(sprintf("Cambio medio (n=%d): <span class='big'>%.2f</span> &nbsp; IC %d%%: <b style='color:%s'>[%.2f ; %.2f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> Con %d%% de confianza, el <b>cambio real</b> por sujeto está entre <b>%.2f</b> y <b>%.2f</b>. %s
      <div class='verdict %s'>%s</div></div>",
      r$n, r$md, pc, TEAL, r$lo, r$hi, pc, r$lo, r$hi, interp,
      ifelse(inc,"no","yes"), ifelse(inc,"IC incluye 0 → sin evidencia.","IC no incluye 0 → cambio significativo."))) })
  output$Bplot <- renderPlot({ r<-Bres(); if(!is.null(r$err)) return(NULL)
    rng<-range(c(r$lo,r$hi,0)); pad<-diff(rng)*0.3+1e-9
    plot(NA,xlim=rng+c(-pad,pad),ylim=c(0,2),yaxt="n",xlab="Cambio (post − pre)",ylab="",main="IC del cambio medio")
    abline(v=0,lty=2,col=GRAY); segments(r$lo,1,r$hi,1,lwd=5,col=TEAL); points(r$md,1,pch=19,cex=2,col=TEAL) })

  # ---------- PARTE C ----------
  Ccounts <- reactive({
    if(input$Cmode=="manual"){ list(a=input$Ca,b=input$Cb,c=input$Cc,d=input$Cd)
    } else {
      req(input$Cexp_col,input$Cev_col); df<-Ccsv()
      e<-as.factor(df[[input$Cexp_col]]); v<-as.factor(df[[input$Cev_col]])
      if(nlevels(e)<2||nlevels(v)<2) return(list(err="Exposición y evento deben tener 2 niveles."))
      tb<-table(e,v); el<-levels(e); vl<-levels(v)
      list(a=tb[1,1],b=tb[1,2],c=tb[2,1],d=tb[2,2], etq=sprintf("Expuesto=%s · Evento=%s", el[1], vl[1]))
    }
  })
  Cres <- reactive({ g<-Ccounts(); if(!is.null(g$err)) return(g)
    a<-g$a;b<-g$b;cc<-g$c;d<-g$d; lv<-as.numeric(input$Clevel); z<-qnorm(1-(1-lv)/2)
    if(min(a,b,cc,d)<=0) return(list(err="Todas las celdas deben ser > 0 para el IC en escala log."))
    OR<-(a*d)/(b*cc); seOR<-sqrt(1/a+1/b+1/cc+1/d)
    RR<-(a/(a+b))/(cc/(cc+d)); seRR<-sqrt(1/a-1/(a+b)+1/cc-1/(cc+d))
    list(OR=OR,orL=exp(log(OR)-z*seOR),orH=exp(log(OR)+z*seOR),
         RR=RR,rrL=exp(log(RR)-z*seRR),rrH=exp(log(RR)+z*seRR),
         re=a/(a+b),rn=cc/(cc+d),RD=a/(a+b)-cc/(cc+d),NNH=1/abs(a/(a+b)-cc/(cc+d)),lv=lv,etq=g$etq) })
  output$Cout <- renderUI({ r<-Cres(); if(!is.null(r$err)) return(errbox(r$err))
    pc<-round(r$lv*100); incRR<-(r$rrL<=1&&r$rrH>=1); incOR<-(r$orL<=1&&r$orH>=1); inc<-(incRR||incOR)
    pctMas<-round((r$RR-1)*100)
    fraseRR <- if(incRR) "El IC del RR <b>incluye 1</b>: podría no haber asociación." else
      sprintf("El riesgo del evento es <b>%.0f%% %s</b> en expuestos (RR = %.2f), y el IC <b>no incluye 1</b>.",
              abs(pctMas), ifelse(pctMas>=0,"mayor","menor"), r$RR)
    nota <- if(r$OR > r$RR*1.15) "<br><b>Ojo OR vs. RR:</b> como el evento no es raro, el OR <b>exagera</b> el efecto; para comunicar el riesgo usa el RR." else ""
    impacto <- sprintf("<b>Impacto:</b> el riesgo pasa de %.1f%% (no expuestos) a %.1f%% (expuestos) → exceso de <b>%.1f por 100</b> (RD); <b>1 caso adicional por cada %.1f expuestos</b> (NNH).",
              r$rn*100, r$re*100, r$RD*100, r$NNH)
    HTML(sprintf("%sRR = <span class='big'>%.2f</span> IC %d%% <b style='color:%s'>[%.2f ; %.2f]</b> ·
      OR = <span class='big'>%.2f</span> IC %d%% <b style='color:%s'>[%.2f ; %.2f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> %s %s %s
      <div class='verdict %s'>%s</div></div>",
      ifelse(is.null(r$etq),"",paste0("<div style='font-size:12px;color:#5B6B70;margin-bottom:4px'>",r$etq,"</div>")),
      r$RR,pc,TEAL,r$rrL,r$rrH, r$OR,pc,TEAL,r$orL,r$orH, fraseRR, impacto, nota,
      ifelse(inc,"no","yes"), ifelse(inc,"Algún IC incluye 1 → sin evidencia.","Ningún IC incluye 1 → asociación significativa."))) })
  output$Cplot <- renderPlot({ r<-Cres(); if(!is.null(r$err)) return(NULL)
    plot(NA,xlim=c(.4,max(4,r$orH*1.1)),ylim=c(.5,2.5),log="x",yaxt="n",
         xlab="Medida de asociación (escala log)",ylab="",main="Forest plot")
    abline(v=1,lty=2,col=GRAY)
    segments(r$rrL,1,r$rrH,1,lwd=3,col=TEAL);  points(r$RR,1,pch=18,cex=2.4,col=TEAL)
    segments(r$orL,2,r$orH,2,lwd=3,col=CORAL); points(r$OR,2,pch=18,cex=2.4,col=CORAL)
    axis(2,at=1:2,labels=c("RR","OR"),las=1) })
}

shinyApp(ui, server)
