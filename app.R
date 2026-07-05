# =====================================================================
#  App Shiny — Estimación interválica con dos muestras (con interpretación)
#  Bioestadística · Maestría en Salud Pública · UNAP
#  Ejecutar:  shiny::runApp("app.R")   (o el botón "Run App" en RStudio)
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
  .big{font-size:22px;font-weight:700;color:#0A3D4A}
  .nav-tabs>li.active>a{color:#0E7C86 !important;font-weight:700}
"

ci_diff <- function(diff, ee, df, level){ tc <- qt(1-(1-level)/2, df); c(diff-tc*ee, diff+tc*ee, tc) }
labD <- function(x){a<-abs(x); if(a<0.2)"trivial" else if(a<0.5)"pequeño" else if(a<0.8)"pequeño-mediano" else if(a<1.1)"grande" else "muy grande"}

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class="titlebox",
      h2("Estimación interválica con dos muestras"),
      p("Calculadora interactiva e interpretada · Bioestadística · Maestría en Salud Pública · UNAP")),
  tabsetPanel(
    # ---------------- PARTE A ----------------
    tabPanel("A · Medias independientes",
      sidebarLayout(
        sidebarPanel(class="well",
          h4("Dos grupos distintos"),
          fluidRow(column(6, numericInput("An1","Grupo 1 · n",115)),
                   column(6, numericInput("An2","Grupo 2 · n",74))),
          fluidRow(column(6, numericInput("Am1","Media 1",3055.7)),
                   column(6, numericInput("Am2","Media 2",2771.9))),
          fluidRow(column(6, numericInput("As1","Desv. 1",752.7)),
                   column(6, numericInput("As2","Desv. 2",659.6))),
          selectInput("Alevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          checkboxInput("Awelch","Usar Welch (recomendado)",TRUE),
          helpText("Ejemplo: peso al nacer (g) según tabaquismo materno.")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> Comparas el <b>promedio</b> de dos grupos formados por <b>personas distintas</b> (p. ej. fumadoras vs. no fumadoras). El intervalo te da el rango plausible de la <b>diferencia de medias</b>. Referencia de \"sin efecto\": el <b>0</b>.")),
          htmlOutput("Aout"), plotOutput("Aplot",height="250px")))),
    # ---------------- PARTE B ----------------
    tabPanel("B · Medias pareadas",
      sidebarLayout(
        sidebarPanel(class="well",
          h4("Mismos sujetos, dos medidas"),
          numericInput("Bn","N.º de pares (n)",10),
          numericInput("Bmd","Media de las diferencias (post − pre)",1.58),
          numericInput("Bsd","Desv. de las diferencias",1.23),
          selectInput("Blevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          helpText("Ejemplo: somníferos de Student (1908).")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> Comparas <b>dos medidas en las mismas personas</b> (antes–después). Se analiza la <b>diferencia de cada sujeto</b>, no dos grupos. Cada persona es su propio control, por eso es más sensible. Referencia de \"sin efecto\": el <b>0</b>.")),
          htmlOutput("Bout"), plotOutput("Bplot",height="190px")))),
    # ---------------- PARTE C ----------------
    tabPanel("C · OR y RR (tabla 2×2)",
      sidebarLayout(
        sidebarPanel(class="well",
          h4("Tabla 2×2 (expuesto × evento)"),
          fluidRow(column(6, numericInput("Ca","a · exp. evento",30)),
                   column(6, numericInput("Cb","b · exp. sin evento",44))),
          fluidRow(column(6, numericInput("Cc","c · no exp. evento",29)),
                   column(6, numericInput("Cd","d · no exp. sin evento",86))),
          selectInput("Clevel","Confianza",c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
          helpText("Ejemplo: tabaquismo materno × bajo peso al nacer.")),
        mainPanel(
          div(class="concept", HTML("<b>¿Qué calculas aquí?</b> Mides si una <b>exposición</b> se asocia con un <b>evento</b>. El <b>RR</b> compara riesgos; el <b>OR</b> compara odds. Referencia de \"sin efecto\": el <b>1</b>. Además obtienes el impacto absoluto: <b>RD</b> (exceso de riesgo) y <b>NNH</b> (personas por 1 caso extra).")),
          htmlOutput("Cout"), plotOutput("Cplot",height="230px"))))
  ),
  div(style="padding:8px 18px;font-size:12px;color:#5B6B70",
      HTML("Regla de oro: si el IC de una <b>diferencia</b> incluye <b>0</b>, o el de un
            <b>cociente</b> (OR/RR) incluye <b>1</b>, no hay evidencia de efecto."))
)

server <- function(input, output){
  # ---------- PARTE A ----------
  Ares <- reactive({
    n1<-input$An1;n2<-input$An2;m1<-input$Am1;m2<-input$Am2;s1<-input$As1;s2<-input$As2
    lv<-as.numeric(input$Alevel); diff<-m1-m2
    if(input$Awelch){ ee<-sqrt(s1^2/n1+s2^2/n2)
      df<-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
    } else { sp2<-((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2); ee<-sqrt(sp2)*sqrt(1/n1+1/n2); df<-n1+n2-2 }
    ci<-ci_diff(diff,ee,df,lv); sp2<-((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
    list(diff=diff,lo=ci[1],hi=ci[2],df=df,ee=ee,t=diff/ee,d=diff/sqrt(sp2),lv=lv,
         m=c(m1,m2), err=c(qt(1-(1-lv)/2,n1-1)*s1/sqrt(n1), qt(1-(1-lv)/2,n2-1)*s2/sqrt(n2)))
  })
  output$Aout <- renderUI({ r<-Ares(); inc<-(r$lo<=0 && r$hi>=0); pc<-round(r$lv*100)
    mayor <- if(r$diff>=0) "el Grupo 1 supera al Grupo 2" else "el Grupo 2 supera al Grupo 1"
    interp <- if(inc){
      sprintf("Como el intervalo <b>incluye el 0</b>, un valor de \"ninguna diferencia\" es plausible: <b>no hay evidencia</b> de que los grupos difieran al %d%%.", pc)
    } else {
      sprintf("Como el intervalo <b>no incluye el 0</b>, <b>sí hay diferencia</b> al %d%%: en promedio, %s en <b>%.1f unidades</b> (plausiblemente entre %.1f y %.1f). El tamaño de efecto es <b>%s</b> (d = %.2f).",
              pc, mayor, abs(r$diff), abs(r$lo), abs(r$hi), labD(r$d), r$d) }
    HTML(sprintf("Diferencia de medias: <span class='big'>%.1f</span> &nbsp; IC %d%%: <b style='color:%s'>[%.1f ; %.1f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> Con %d%% de confianza, la <b>verdadera diferencia</b> de medias está entre <b>%.1f</b> y <b>%.1f</b>. %s
      <div class='verdict %s'>%s</div></div>",
      r$diff, pc, TEAL, r$lo, r$hi, pc, r$lo, r$hi, interp,
      ifelse(inc,"no","yes"),
      ifelse(inc,"IC incluye 0 → sin evidencia de diferencia.","IC no incluye 0 → diferencia significativa."))) })
  output$Aplot <- renderPlot({ r<-Ares()
    plot(1:2,r$m,pch=19,cex=1.8,col=c(TEAL,CORAL),xlim=c(.5,2.5),
         ylim=range(c(r$m-r$err,r$m+r$err))+c(-80,80),xaxt="n",xlab="",ylab="Media",main="Medias e IC")
    arrows(1:2,r$m-r$err,1:2,r$m+r$err,angle=90,code=3,length=.12,lwd=2,col=c(TEAL,CORAL))
    axis(1,at=1:2,labels=c("Grupo 1","Grupo 2")) })
  # ---------- PARTE B ----------
  Bres <- reactive({ n<-input$Bn;md<-input$Bmd;sd<-input$Bsd;lv<-as.numeric(input$Blevel)
    ee<-sd/sqrt(n); ci<-ci_diff(md,ee,n-1,lv); list(md=md,lo=ci[1],hi=ci[2],t=md/ee,dz=md/sd,n=n,lv=lv) })
  output$Bout <- renderUI({ r<-Bres(); inc<-(r$lo<=0 && r$hi>=0); pc<-round(r$lv*100)
    dir <- if(r$md>=0) "aumenta" else "disminuye"
    interp <- if(inc){
      sprintf("Como el intervalo <b>incluye el 0</b>, es plausible que no haya cambio: <b>no hay evidencia</b> de efecto al %d%%.", pc)
    } else {
      sprintf("Como el intervalo <b>no incluye el 0</b>, <b>sí hay cambio</b> al %d%%: en promedio la medida <b>%s %.2f</b> por sujeto (plausiblemente entre %.2f y %.2f). Efecto <b>%s</b> (d<sub>z</sub> = %.2f).",
              pc, dir, abs(r$md), r$lo, r$hi, labD(r$dz), r$dz) }
    HTML(sprintf("Cambio medio: <span class='big'>%.2f</span> &nbsp; IC %d%%: <b style='color:%s'>[%.2f ; %.2f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> Con %d%% de confianza, el <b>cambio real</b> por sujeto está entre <b>%.2f</b> y <b>%.2f</b>. %s
      <div class='verdict %s'>%s</div></div>",
      r$md, pc, TEAL, r$lo, r$hi, pc, r$lo, r$hi, interp,
      ifelse(inc,"no","yes"),
      ifelse(inc,"IC incluye 0 → sin evidencia de cambio.","IC no incluye 0 → cambio significativo."))) })
  output$Bplot <- renderPlot({ r<-Bres(); rng<-range(c(r$lo,r$hi,0)); pad<-diff(rng)*0.3
    plot(NA,xlim=rng+c(-pad,pad),ylim=c(0,2),yaxt="n",xlab="Cambio (post − pre)",ylab="",main="IC del cambio medio")
    abline(v=0,lty=2,col=GRAY); segments(r$lo,1,r$hi,1,lwd=5,col=TEAL); points(r$md,1,pch=19,cex=2,col=TEAL) })
  # ---------- PARTE C ----------
  Cres <- reactive({ a<-input$Ca;b<-input$Cb;cc<-input$Cc;d<-input$Cd;lv<-as.numeric(input$Clevel); z<-qnorm(1-(1-lv)/2)
    OR<-(a*d)/(b*cc); seOR<-sqrt(1/a+1/b+1/cc+1/d)
    RR<-(a/(a+b))/(cc/(cc+d)); seRR<-sqrt(1/a-1/(a+b)+1/cc-1/(cc+d))
    list(OR=OR,orL=exp(log(OR)-z*seOR),orH=exp(log(OR)+z*seOR),
         RR=RR,rrL=exp(log(RR)-z*seRR),rrH=exp(log(RR)+z*seRR),
         re=a/(a+b),rn=cc/(cc+d),RD=a/(a+b)-cc/(cc+d),NNH=1/abs(a/(a+b)-cc/(cc+d)),lv=lv) })
  output$Cout <- renderUI({ r<-Cres(); pc<-round(r$lv*100)
    incRR<-(r$rrL<=1&&r$rrH>=1); incOR<-(r$orL<=1&&r$orH>=1); inc<-(incRR||incOR)
    pctMas <- round((r$RR-1)*100)
    fraseRR <- if(incRR) sprintf("El IC del RR <b>incluye 1</b>: podría no haber asociación.") else
      sprintf("El riesgo del evento es <b>%.0f%% %s</b> en expuestos (RR = %.2f), y el IC <b>no incluye 1</b>.",
              abs(pctMas), ifelse(pctMas>=0,"mayor","menor"), r$RR)
    nota <- if(r$OR > r$RR*1.15) "<br><b>Ojo OR vs. RR:</b> como el evento no es raro, el OR (odds) <b>exagera</b> el efecto frente al RR; para comunicar el riesgo, usa el RR." else ""
    impacto <- sprintf("<b>Impacto absoluto:</b> el riesgo pasa de %.1f%% (no expuestos) a %.1f%% (expuestos) → exceso de <b>%.1f por cada 100</b> (RD). Es decir, <b>1 caso adicional por cada %.1f expuestos</b> (NNH).",
              r$rn*100, r$re*100, r$RD*100, r$NNH)
    HTML(sprintf("RR = <span class='big'>%.2f</span> IC %d%% <b style='color:%s'>[%.2f ; %.2f]</b> &nbsp;·&nbsp;
      OR = <span class='big'>%.2f</span> IC %d%% <b style='color:%s'>[%.2f ; %.2f]</b>
      <div class='interp'><span class='lead'>Interpretación.</span> %s %s %s
      <div class='verdict %s'>%s</div></div>",
      r$RR,pc,TEAL,r$rrL,r$rrH, r$OR,pc,TEAL,r$orL,r$orH, fraseRR, impacto, nota,
      ifelse(inc,"no","yes"),
      ifelse(inc,"Algún IC incluye 1 → sin evidencia de asociación.","Ningún IC incluye 1 → asociación significativa."))) })
  output$Cplot <- renderPlot({ r<-Cres()
    plot(NA,xlim=c(.4,max(4,r$orH*1.1)),ylim=c(.5,2.5),log="x",yaxt="n",
         xlab="Medida de asociación (escala log)",ylab="",main="Forest plot")
    abline(v=1,lty=2,col=GRAY)
    segments(r$rrL,1,r$rrH,1,lwd=3,col=TEAL);  points(r$RR,1,pch=18,cex=2.4,col=TEAL)
    segments(r$orL,2,r$orH,2,lwd=3,col=CORAL); points(r$OR,2,pch=18,cex=2.4,col=CORAL)
    axis(2,at=1:2,labels=c("RR","OR"),las=1) })
}

shinyApp(ui, server)
