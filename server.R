#cores na paleta de #2596be

load("objetosshiny.RData")

server <- function(input, output, session) {
  
###############descritiva#######################################################
  
  output$quimio <- renderValueBox({
    valueBox(h4("Quimioterapia"), tags$i(class=""), color = "navy")
  })
  
  output$sexo <- renderValueBox({
    valueBox(h4("Sexo"), tags$i(class=""), color = "teal")
  })
  
  output$tumoral <- renderValueBox({
    valueBox(h4("Estágio tumoral"), tags$i(class=""), color = "purple")
  })
  
  output$comorbidade <- renderValueBox({
    valueBox(h4("Índice de comorbidade"), tags$i(class=""),
             color = "light-blue")
  })
  
  output$Plot39 <- renderPlotly({
    df1 <- data.frame(
      cov = c("Não tratado", "Tratado"),
      value = table(summarise(group_by(df,id),unique(chemo))[2]))
    
    fig <- plot_ly(data = df1, 
                   labels = ~cov, 
                   values = ~df1$value.Freq,
                   marker = list(colors = c("#071e26","#26a4d0")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  
  output$Plot1 <- renderPlotly({
    df2 <- data.frame(
      cov = c("Masculino","Feminino"),
      value = table(summarise(group_by(df,id),unique(sex))[2]))
    
    fig <- plot_ly(data = df2, 
                   labels = ~cov, 
                   values = ~df2$value.Freq,
                   marker = list(colors = c("#39cccc","#071e26")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  output$Plot40 <- renderPlotly({
    df3 <- data.frame(
      cov = levels(df$dukes),
      value = table(summarise(group_by(df,id),unique(dukes))[2]))
    
    fig <- plot_ly(data = df3, 
                   labels = ~cov, 
                   values = ~df3$value.Freq,
                   marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  output$Plot41 <- renderPlotly({
    if (input$Wid13=="1º reint.") {
    df4 <- df |>
      filter(enum == 1)
    df4 <- data.frame(
      cov = levels(df4$charlson),
      value = table(summarise(group_by(df4,id),unique(charlson))[2]))
    
    fig <- plot_ly(data = df4, 
                   labels = ~cov, 
                   values = ~df4$value.Freq,
                   marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                   type = 'pie', hole = 0.6,
                   textinfo='label+percent',
                   showlegend = FALSE) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if (input$Wid13=="2º reint.") {
      df4 <- df |>
        filter(enum == 2)
      df4 <- data.frame(
        cov = levels(df4$charlson),
        value = table(summarise(group_by(df4,id),unique(charlson))[2]))
      
      fig <- plot_ly(data = df4, 
                     labels = ~cov, 
                     values = ~df4$value.Freq,
                     marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                     type = 'pie', hole = 0.6,
                     textinfo='label+percent',
                     showlegend = FALSE) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if (input$Wid13=="3º reint.") {
      df4 <- df |>
        filter(enum == 3)
      df4 <- data.frame(
        cov = levels(df4$charlson),
        value = table(summarise(group_by(df4,id),unique(charlson))[2]))
      
      fig <- plot_ly(data = df4, 
                     labels = ~cov, 
                     values = ~df4$value.Freq,
                     marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                     type = 'pie', hole = 0.6,
                     textinfo='label+percent',
                     showlegend = FALSE) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if (input$Wid13=="4º reint.") {
      df4 <- df |>
        filter(enum == 4)
      df4 <- data.frame(
        cov = levels(df4$charlson),
        value = table(summarise(group_by(df4,id),unique(charlson))[2]))
      
      fig <- plot_ly(data = df4, 
                     labels = ~cov, 
                     values = ~df4$value.Freq,
                     marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                     type = 'pie', hole = 0.6,
                     textinfo='label+percent',
                     showlegend = FALSE) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if (input$Wid13=="5º reint.") {
      df4 <- df |>
        filter(enum == 5)
      df4 <- data.frame(
        cov = levels(df4$charlson),
        value = table(summarise(group_by(df4,id),unique(charlson))[2]))
      
      fig <- plot_ly(data = df4, 
                     labels = ~cov, 
                     values = ~df4$value.Freq,
                     marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                     type = 'pie', hole = 0.6,
                     textinfo='label+percent',
                     showlegend = FALSE) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if (input$Wid13=="6º reint.") {
      df4 <- df |>
        filter(enum == 6)
      df4 <- data.frame(
        cov = levels(df4$charlson),
        value = table(summarise(group_by(df4,id),unique(charlson))[2]))
      
      fig <- plot_ly(data = df4, 
                     labels = ~cov, 
                     values = ~df4$value.Freq,
                     marker = list(colors = c("#5639cc","#071e26","#26a4d0")),
                     type = 'pie', hole = 0.6,
                     textinfo='label+percent',
                     showlegend = FALSE) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    fig
  })
  
  
  output$Plot2 <- renderPlotly({
    if (input$Wid14=="1º reint.") {
      ekm1 <- survfit(Surv(time,event)~chemo, data=df1)
      
      tc1 <- ekm1$time[1:161]
      tc2 <- ekm1$time[162:(length(ekm1$time)-1)]
      sc1 <- ekm1$surv[1:161]
      sc2 <- ekm1$surv[162:(length(ekm1$time)-1)]
      
      d1 <- data.frame(Tempo = c(tc1,tc2), `Sobrevivência estimada` = round(c(sc1,sc2),2), Quimioterapia=c(rep("Não tratado",length(tc1)),rep("Tratado",length(tc2))))
      
      fig <- ggplot(d1)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Quimioterapia))+
        scale_color_manual(values=c("#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid14=="2º reint.") {
      ekm1 <- survfit(Surv(time,event)~chemo, data=df2)
      
      tc1 <- ekm1$time[1:173]
      tc2 <- ekm1$time[174:(length(ekm1$time)-1)]
      sc1 <- ekm1$surv[1:173]
      sc2 <- ekm1$surv[174:(length(ekm1$time)-1)]
      
      d1 <- data.frame(Tempo = c(tc1,tc2), `Sobrevivência estimada` = round(c(sc1,sc2),2), Quimioterapia=c(rep("Não tratado",length(tc1)),rep("Tratado",length(tc2))))
      
      fig <- ggplot(d1)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Quimioterapia))+
        scale_color_manual(values=c("#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid14=="3º reint.") {
      ekm1 <- survfit(Surv(time,event)~chemo, data=df3)
      
      tc1 <- ekm1$time[1:170]
      tc2 <- ekm1$time[171:(length(ekm1$time)-1)]
      sc1 <- ekm1$surv[1:170]
      sc2 <- ekm1$surv[171:(length(ekm1$time)-1)]
      
      d1 <- data.frame(Tempo = c(tc1,tc2), `Sobrevivência estimada` = round(c(sc1,sc2),2), Quimioterapia=c(rep("Não tratado",length(tc1)),rep("Tratado",length(tc2))))
      
      fig <- ggplot(d1)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Quimioterapia))+
        scale_color_manual(values=c("#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    fig
      })
    
  output$Plot36 <- renderPlotly({
    if (input$Wid15=="1º reint.") {
      ekm2 <- survfit(Surv(time,event)~sex, data=df1)
      
      ts1 <- ekm2$time[1:192]
      ts2 <- ekm2$time[193:(length(ekm2$surv)-1)]
      ss1 <- ekm2$surv[1:192]
      ss2 <- ekm2$surv[193:(length(ekm2$surv)-1)]
      
      d2 <- data.frame(Tempo = c(ts1,ts2), `Sobrevivência estimada` = round(c(ss1,ss2),2), Sexo=c(rep("Masculino",length(ts1)),rep("Feminino",length(ts2))))
      
      fig <- ggplot(d2)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Sexo))+
        scale_color_manual(values=c("#071e26","#39cccc"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid15=="2º reint.") {
      ekm2 <- survfit(Surv(time,event)~sex, data=df2)
      
      ts1 <- ekm2$time[1:221]
      ts2 <- ekm2$time[222:(length(ekm2$surv)-1)]
      ss1 <- ekm2$surv[1:221]
      ss2 <- ekm2$surv[222:(length(ekm2$surv)-1)]
      
      d2 <- data.frame(Tempo = c(ts1,ts2), `Sobrevivência estimada` = round(c(ss1,ss2),2), Sexo=c(rep("Masculino",length(ts1)),rep("Feminino",length(ts2))))
      
      fig <- ggplot(d2)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Sexo))+
        scale_color_manual(values=c("#071e26","#39cccc"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid15=="3º reint.") {
      ekm2 <- survfit(Surv(time,event)~sex, data=df3)
      
      ts1 <- ekm2$time[1:219]
      ts2 <- ekm2$time[220:(length(ekm2$surv)-1)]
      ss1 <- ekm2$surv[1:219]
      ss2 <- ekm2$surv[220:(length(ekm2$surv)-1)]
      
      d2 <- data.frame(Tempo = c(ts1,ts2), `Sobrevivência estimada` = round(c(ss1,ss2),2), Sexo=c(rep("Masculino",length(ts1)),rep("Feminino",length(ts2))))
      
      fig <- ggplot(d2)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Sexo))+
        scale_color_manual(values=c("#071e26","#39cccc"))+
        labs(x="", y="")+
        theme_bw()
    }
      fig
    })
    
  output$Plot37 <- renderPlotly({
    if (input$Wid16=="1º reint.") {
      ekm3 <- survfit(Surv(time,event)~dukes, data=df1)
      
      td1 <- ekm3$time[1:152]
      td2 <- ekm3$time[154:289]
      td3 <- ekm3$time[290:length(ekm3$time)]
      sd1 <- ekm3$surv[1:152]
      sd2 <- ekm3$surv[154:289]
      sd3 <- ekm3$surv[290:length(ekm3$surv)]
      
      d3 <- data.frame(Tempo = c(td1,td2,td3), 
                       `Sobrevivência estimada` = round(c(sd1,sd2,sd3),2), 
                       Dukes=c(rep("A-B",length(td1)),rep("C",length(td2)),rep("D", length(td3))))
      
      fig <- ggplot(d3)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Dukes))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid16=="2º reint.") {
      ekm3 <- survfit(Surv(time,event)~dukes, data=df2)
      
      td1 <- ekm3$time[1:171]
      td2 <- ekm3$time[172:314]
      td3 <- ekm3$time[315:length(ekm3$time)]
      sd1 <- ekm3$surv[1:171]
      sd2 <- ekm3$surv[172:314]
      sd3 <- ekm3$surv[315:length(ekm3$surv)]
      
      d3 <- data.frame(Tempo = c(td1,td2,td3), 
                       `Sobrevivência estimada` = round(c(sd1,sd2,sd3),2), 
                       Dukes=c(rep("A-B",length(td1)),rep("C",length(td2)),rep("D", length(td3))))
      
      fig <- ggplot(d3)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Dukes))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid16=="3º reint.") {
      ekm3 <- survfit(Surv(time,event)~dukes, data=df3)
      
      td1 <- ekm3$time[1:170]
      td2 <- ekm3$time[171:312]
      td3 <- ekm3$time[313:length(ekm3$time)]
      sd1 <- ekm3$surv[1:170]
      sd2 <- ekm3$surv[171:312]
      sd3 <- ekm3$surv[313:length(ekm3$surv)]
      
      d3 <- data.frame(Tempo = c(td1,td2,td3), 
                       `Sobrevivência estimada` = round(c(sd1,sd2,sd3),2), 
                       Dukes=c(rep("A-B",length(td1)),rep("C",length(td2)),rep("D", length(td3))))
      
      fig <- ggplot(d3)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Dukes))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
      fig
    })
    
  output$Plot38 <- renderPlotly({
    if (input$Wid17=="1º reint.") {
      ekm4 <- survfit(Surv(time,event)~charlson, data=df1)
      
      tcc1 <- ekm4$time[1:235]
      tcc2 <- ekm4$time[237:252]
      tcc3 <- ekm4$time[253:length(ekm4$time)]
      scc1 <- ekm4$surv[1:235]
      scc2 <- ekm4$surv[237:252]
      scc3 <- ekm4$surv[253:length(ekm4$surv)]
      
      d4 <- data.frame(Tempo=c(tcc1,tcc2,tcc3), `Sobrevivência estimada`= round(c(scc1,scc2,scc3),2), Charlson = c(rep("0",length(tcc1)),rep("1-2",length(tcc2)),rep("3", length(tcc3))))
      
      fig <- ggplot(d4)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Charlson))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid17=="2º reint.") {
      ekm4 <- survfit(Surv(time,event)~charlson, data=df2)
      
      tcc1 <- ekm4$time[1:263]
      tcc2 <- ekm4$time[264:286]
      tcc3 <- ekm4$time[287:length(ekm4$time)]
      scc1 <- ekm4$surv[1:263]
      scc2 <- ekm4$surv[264:286]
      scc3 <- ekm4$surv[287:length(ekm4$surv)]
      
      d4 <- data.frame(Tempo=c(tcc1,tcc2,tcc3), `Sobrevivência estimada`= round(c(scc1,scc2,scc3),2), Charlson = c(rep("0",length(tcc1)),rep("1-2",length(tcc2)),rep("3", length(tcc3))))
      
      fig <- ggplot(d4)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Charlson))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
    if (input$Wid17=="3º reint.") {
      ekm4 <- survfit(Surv(time,event)~charlson, data=df3)
      
      tcc1 <- ekm4$time[1:266]
      tcc2 <- ekm4$time[267:285]
      tcc3 <- ekm4$time[286:length(ekm4$time)]
      scc1 <- ekm4$surv[1:266]
      scc2 <- ekm4$surv[267:285]
      scc3 <- ekm4$surv[286:length(ekm4$surv)]
      
      d4 <- data.frame(Tempo=c(tcc1,tcc2,tcc3), `Sobrevivência estimada`= round(c(scc1,scc2,scc3),2), Charlson = c(rep("0",length(tcc1)),rep("1-2",length(tcc2)),rep("3", length(tcc3))))
      
      fig <- ggplot(d4)+
        geom_step(aes(x=Tempo, y=`Sobrevivência.estimada`, color=Charlson))+
        scale_color_manual(values=c("#5639cc","#071e26","#26a4d0"))+
        labs(x="", y="")+
        theme_bw()
    }
      fig
    })
  
  output$Plot42 <- renderPlotly({
    `Tempo até a reinternação` <- df$time
    Paciente <- df$id
    Evento  <- factor(ifelse(df$event==0,"Censura","Falha"))
    fig <- ggplot(df)+
               aes(x=`Tempo até a reinternação`, y=Paciente, colour=Evento)+
               geom_point()+
               labs(y="Pacientes",x="Tempos",color="")+
               scale_colour_manual(values = c("Censura" = "#26a4d0","Falha"= "#071e26"))+
               theme_bw()
    fig
    
  })
  
  output$Plot47 <- renderPlotly({
    tte <- TTTE_Analytical(Surv(t.start,t.stop, event)~1,method="cens",data = df)
    `i/n` <- round(tte$`i/n`,3)
    `φ_n` <- round(tte$phi_n,3)
    
   fig <-  ggplot()+
     aes(x=`i/n`,y=`φ_n`)+
     geom_abline(intercept=0,slope=1, linetype=2, color="blue",linewidth=1)+
     geom_line(linewidth=1)+
     labs(y="φ_n(i/n)",x="i/n")+
     theme_bw()
   fig
  })


#############freqreg############################################################

  output$box11 <- renderValueBox({
    valueBox(h4("Quimioterapia"), tags$i(class=""), color = "navy")
  })
  
  output$box12 <- renderValueBox({
    valueBox(h4("Sexo"), tags$i(class=""), color = "teal")
  })
  
  output$box13 <- renderValueBox({
    valueBox(h4("Estágio tumoral"), tags$i(class=""), color = "purple")
  })
  
  output$box14 <- renderValueBox({
    valueBox(h4("Índice de comorbidade"), tags$i(class=""),
             color = "light-blue")
  })
  
  output$Plot27 <- renderPlotly({
    tabela <- sm(estfreq1)
    
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b></b>','<b>Parâmetro</b>','<b>Estimativa</b>','<b>E.P.</b>','<b>Valor-p</b>','<b>IC (2.5%)</b>','<b>IC (97.5%)</b>'),
        line = list(color = "#071e26"),
        fill = list(color = "#071e26"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("Intercepto","Quimio (Tratado)", "Sexo (Feminino)",
                               "Estágio tumoral C (A-B)", "Estágio tumoral D (A-B)",
                               "Índice de comorbidade 1-2 (0)", "Índice de comorbidade ≥3 (0)","Weibull","Fragilidade"),
                         rbind("β0","β1","β2","β3","β4","β5","β6","γ","ξ"),
                         round(tabela,3))),
        line = list(color = "#071e26"),
        fill = list(color = c('#cdd2d4', 'white')),
        align = c('left', 'center'),
        font = list(color = c("#071e26"), size = 12)
      ))
    fig
  })
  
  output$Plot28 <- renderPlotly({
    tabela <- sm1(estfreq2)
    
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b></b>','<b>Parâmetro</b>','<b>Estimativa</b>','<b>E.P.</b>','<b>Valor-p</b>','<b>IC (2.5%)</b>','<b>IC (97.5%)</b>'),
        line = list(color = "#5639cc"),
        fill = list(color = "#5639cc"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("Intercepto", "Sexo (Feminino)",
                               "Estágio tumoral C (A-B)", "Estágio tumoral D (A-B)",
                               "Índice de comorbidade 1-2 (0)", "Índice de comorbidade ≥3 (0)","Weibull","Fragilidade"),
                         rbind("β0","β1","β2","β3","β4","β5","γ","ξ"),
                         round(tabela,3))),
        line = list(color = "#5639cc"),
        fill = list(color = c("#eeebfa", 'white')),
        align = c('left', 'center'),
        font = list(color = c("#5639cc"), size = 12)
      ))
    fig
  })
  
#############freqreg1###########################################################

  output$box22 <- renderValueBox({
    valueBox(
      round(-2*estfreq1$value+2*7,2), "AIC do modelo completo", tags$i(class=""),
      color = "purple"
    )
  })
  
  output$box23 <- renderValueBox({
    valueBox(
      round(-2*estfreq2$value+2*6,2), "AIC do modelo reduzido", tags$i(class=""),
      color = "fuchsia"
    )
  })
  
  output$box24 <- renderValueBox({
    valueBox(
      round(pchisq(-2*(-estfreq1$value+estfreq2$value),1,lower.tail=FALSE),3), "Valor-p do TRV", tags$i(class=""),
      color = "navy"
    )
  })
  
  output$Plot31 <- renderPlotly({
    figres1
  })
  
  output$Plot33 <- renderPlot({
    figres2
  })
  
  output$Plot32 <- renderPlotly({
    figres5
  })
  
  output$Plot32.1 <- renderPlot({
    figres6
  })
  
#############bayesreg###########################################################

  output$box15 <- renderValueBox({
    valueBox(h4("Quimioterapia"), tags$i(class=""), color = "navy")
  })
  
  output$box16 <- renderValueBox({
    valueBox(h4("Sexo"), tags$i(class=""), color = "teal")
  })
  
  output$box17 <- renderValueBox({
    valueBox(h4("Estágio tumoral"), tags$i(class=""), color = "purple")
  })
  
  output$box18 <- renderValueBox({
    valueBox(h4("Índice de comorbidade"), tags$i(class=""),
             color = "light-blue")
  })
  
  output$Plot29 <- renderPlotly({
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b></b>','<b>Parâmetro</b>','<b>Estimativa</b>','<b>E.P.</b>','<b>IC (2.5%)</b>',
                   '<b>IC (97.5%)</b>'),
        line = list(color = "#071e26"),
        fill = list(color = "#071e26"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("Intercepto","Quimio (Tratado)", "Sexo (Feminino)",
                               "Estágio tumoral C (A-B)", "Estágio tumoral D (A-B)",
                               "Índice de comorbidade 1-2 (0)", "Índice de comorbidade ≥3 (0)","Weibull","Fragilidade"),
                         rbind("β0","β1","β2","β3","β4","β5","β6","γ","ξ"),
                         round(Fit4$Summary1[1:9,-c(3,4,6)],3))),
        line = list(color = "#071e26"),
        fill = list(color = c('#cdd2d4', 'white')),
        align = c('left', 'center'),
        font = list(color = c("#071e26"), size = 12)
      ))
    fig
  })
  
  output$Plot30 <- renderPlotly({
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b></b>','<b>Parâmetro</b>','<b>Estimativa</b>','<b>E.P.</b>','<b>IC (2.5%)</b>',
                   '<b>IC (97.5%)</b>'),
        line = list(color = "#5639cc"),
        fill = list(color = "#5639cc"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("Intercepto", "Sexo (Feminino)",
                               "Estágio tumoral C (A-B)", "Estágio tumoral D (A-B)",
                               "Índice de comorbidade 1-2 (0)", "Índice de comorbidade ≥3 (0)","Weibull","Fragilidade"),
                         rbind("β0","β1","β2","β3","β4","β5","γ","ξ"),
                         round(Fit3$Summary1[1:8,-c(3,4,6)],3))),
        line = list(color = "#5639cc"),
        fill = list(color = c("#eeebfa", 'white')),
        align = c('left', 'center'),
        font = list(color = c("#5639cc"), size = 12)
      ))
    fig
  })
  
#############bayesreg1##########################################################

  output$box6 <- renderValueBox({
    valueBox(
      "400000", "Iterações", tags$i(class=""),
      color = "navy"
    )
  })
  
  output$box7 <- renderValueBox({
    valueBox(
      "17.02h", "Tempo comp. - completo", tags$i(class=""),
      color = "blue"
    )
  })
  
  output$box8 <- renderValueBox({
    valueBox(
      "14.55h", "Tempo comp. - reduzido", tags$i(class=""),
      color = "light-blue"
    )
  })
  
  output$box9 <- renderValueBox({
    valueBox(
      round(Fit4$Acceptance.Rate,3), "Taxa de aceitação completo", icon = icon(""),
      color = "purple"
    )
  })
  
  output$box10 <- renderValueBox({
    valueBox(
      round(Fit3$Acceptance.Rate,3), "Taxa de aceitação reduzido", tags$i(class=""),
      color = "fuchsia"
    )
  })
  
  output$Plot34 <- renderPlotly({
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b> </b>','<b>MCSE</b>','<b>ESS</b>'),
        line = list(color = "#071e26"),
        fill = list(color = "#071e26"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("β0","β1","β2","β3","β4","β5","β6","γ","ξ","Deviance", "LP"),
                         round(Fit4$Summary1[,3:4],3))),
        line = list(color = "#071e26"),
        fill = list(color = c('#cdd2d4', 'white')),
        align = c('left', 'center'),
        font = list(color = c("#071e26"), size = 12)))
    fig
  })
  
  output$Plot35 <- renderPlotly({
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b> </b>','<b>MCSE</b>','<b>ESS</b>'),
        line = list(color = "#5639cc"),
        fill = list(color = "#5639cc"),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(cbind(rbind("β0","β1","β2","β3","β4","β5","γ","ξ","Deviance", "LP"),
                         round(Fit3$Summary1[,3:4],3))),
        line = list(color = "#5639cc"),
        fill = list(color = c("#eeebfa", 'white')),
        align = c('left', 'center'),
        font = list(color = c("#5639cc"), size = 12)))
    fig
  })
  
  output$Plot14 <- renderPlot({
    
    if (input$Wid4=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,1])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,1]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[0]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,1])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,1],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid4=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,1])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,1]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[0]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,1])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,1],60)+
        labs(title="")+
        theme_bw()
    }
    fig
  })
  
  output$Plot15 <- renderPlot({
    
    if (input$Wid5=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,2])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,2]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[1]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,2])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,2],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid5=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,2])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,2]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[1]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,2])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,2],60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot16 <- renderPlot({
    
    if (input$Wid6=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,3])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,3]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[2]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,3])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,3],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid6=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,3])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,3]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[2]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,3])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,3],60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot17 <- renderPlot({
    
    if (input$Wid7=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,4])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,4]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[3]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,4])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,4],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid7=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,4])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,4]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[3]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,4])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,4],60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot18 <- renderPlot({
    
    if (input$Wid8=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,5])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,5]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[4]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,5])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,5],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid8=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,5])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,5]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[4]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,5])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,5],60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot19 <- renderPlot({
    
    if (input$Wid9=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,6])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,6]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[5]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,6])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,6],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid9=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,6])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,6]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(beta[5]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,6])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,6],60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot20 <- renderPlot({
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,7])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,7]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(beta[6]))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,7])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,7],60)+
        labs(title="")+
        theme_bw()
    fig    
  })
  
  output$Plot21 <- renderPlot({
    if (input$Wid10=="Completo") {
    fig <- ggplot()+
      aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,8])+
      geom_line() +
      geom_hline(yintercept=mean(Fit4$Posterior1[,8]), color = "#26a4d0")+
      labs(x="Iteração", y=expression(gamma))+
      theme_bw()+
      
      ggplot()+
      aes(x=Fit4$Posterior1[,8])+
      geom_density(colour="#071e26", fill="#071e26")+
      labs(x="Densidade", y="Valores")+
      theme_bw() +
      
      ggAcf(Fit4$Posterior1[,8],60)+
      labs(title="")+
      theme_bw()
  }
  
  if (input$Wid10=="Reduzido") {
    fig <- ggplot()+
      aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,7])+
      geom_line() +
      geom_hline(yintercept=mean(Fit3$Posterior1[,7]), color = "#ccc4f0")+
      labs(x="Iteração", y=expression(gamma))+
      theme_bw()+
      
      ggplot()+
      aes(x=Fit3$Posterior1[,7])+
      geom_density(colour="#071e26", fill="#5639cc")+
      labs(x="Densidade", y="Valores")+
      theme_bw() +
      
      ggAcf(Fit3$Posterior1[,7],60)+
      labs(title="")+
      theme_bw()
  }
    fig
  })
  
  output$Plot21.1 <- renderPlot({
    if (input$Wid10.1=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Posterior1), y=Fit4$Posterior1[,9])+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Posterior1[,9]), color = "#26a4d0")+
        labs(x="Iteração", y=expression(xi))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Posterior1[,9])+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Posterior1[,9],60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid10.1=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Posterior1), y=Fit3$Posterior1[,8])+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Posterior1[,8]), color = "#ccc4f0")+
        labs(x="Iteração", y=expression(xi))+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Posterior1[,8])+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Posterior1[,8],60)+
        labs(title="")+
        theme_bw()
    }
    fig
  })
  
  output$Plot22 <- renderPlot({
    
    if (input$Wid11=="Completo") {
      fig <- ggplot()+
        aes(x=1:length(Fit4$Deviance), y=Fit4$Deviance)+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Deviance), color ="#26a4d0")+
        labs(x="Iteração", y="Desvio")+
        theme_bw()+
        ggplot()+
        aes(x=Fit4$Deviance)+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Deviance,60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid11=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:length(Fit3$Deviance), y=Fit3$Deviance)+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Deviance), color = "#ccc4f0")+
        labs(x="Iteração", y="Desvio")+
        theme_bw()+
        ggplot()+
        aes(x=Fit3$Deviance)+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Deviance,60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
  output$Plot23 <- renderPlot({
    
    if (input$Wid12=="Completo") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit4$Monitor), y=Fit4$Monitor)+
        geom_line() +
        geom_hline(yintercept=mean(Fit4$Monitor), color ="#26a4d0")+
        labs(x="Iteração", y="LP")+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit4$Monitor)+
        geom_density(colour="#071e26", fill="#071e26")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit4$Monitor,60)+
        labs(title="")+
        theme_bw()
    }
    
    if (input$Wid12=="Reduzido") {
      fig <- ggplot()+
        aes(x=1:nrow(Fit3$Monitor), y=Fit3$Monitor)+
        geom_line() +
        geom_hline(yintercept=mean(Fit3$Monitor), color = "#ab9ce6")+
        labs(x="Iteração", y="LP")+
        theme_bw()+
        
        ggplot()+
        aes(x=Fit3$Monitor)+
        geom_density(colour="#071e26", fill="#5639cc")+
        labs(x="Densidade", y="Valores")+
        theme_bw() +
        
        ggAcf(Fit3$Monitor,60)+
        labs(title="")+
        theme_bw()
    }
    fig    
  })
  
#############bayesreg2##########################################################
  
  output$box19 <- renderValueBox({
    valueBox(
      round(Fit4$DIC1[3],2), "DIC do modelo completo", tags$i(class=""),
      color = "purple"
    )
  })
  
  output$box20 <- renderValueBox({
    valueBox(
      round(Fit3$DIC1[3],2), "DIC do modelo reduzido", tags$i(class=""),
      color = "fuchsia"
    )
  })
  
  output$box21 <- renderValueBox({
    valueBox(
      "0.73", "Fator de Bayes", tags$i(class=""),
      color = "navy"
    )
  })
  
  output$Plot43 <- renderPlotly({
    figres3
  })
  
  output$Plot45 <- renderPlot({
    figres4
  })
  
  output$Plot44 <- renderPlotly({
    figres7
  })
  
  output$Plot46 <- renderPlot({
    figres8
  })
  
}

