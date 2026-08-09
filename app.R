library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(httr2)
library(jsonlite)
library(digest)

PLAYERS_FILE <- "players.csv"
LOCAL_MATCHES_FILE <- "matches_local.csv"
ADMIN_PASSWORD_HASH <- Sys.getenv("ADMIN_PASSWORD_HASH")
DROPBOX_TOKEN <- Sys.getenv("DROPBOX_TOKEN")
DROPBOX_MATCHES_PATH <- Sys.getenv("DROPBOX_MATCHES_PATH", unset = "/matches.csv")
participation_bonus <- 11

stage_rules <- tibble::tribble(
  ~stage, ~win_base, ~loss_base,
  "Group Stage", 10, 10,
  "Round of 16", 12, 9,
  "Quarterfinal", 14, 8,
  "Semifinal", 16, 7,
  "Final", 20, 6
)

empty_matches <- tibble(
  match_id=integer(), date=as.Date(character()), cup=character(), stage=character(),
  player_a=character(), goals_a=integer(), player_b=character(), goals_b=integer(),
  rating_a_before=double(), rating_b_before=double(), participation_a=double(), participation_b=double(),
  rating_change_a=double(), rating_change_b=double(), rating_a_after=double(), rating_b_after=double()
)

normalize_matches <- function(x){
  if(is.null(x) || nrow(x)==0) return(empty_matches)
  reqn <- names(empty_matches)
  for(nm in reqn) if(!nm %in% names(x)) x[[nm]] <- NA
  x <- x[,reqn]
  x %>% mutate(
    match_id=as.integer(match_id), date=as.Date(date), cup=as.character(cup), stage=as.character(stage),
    player_a=as.character(player_a), goals_a=as.integer(goals_a), player_b=as.character(player_b), goals_b=as.integer(goals_b),
    across(c(rating_a_before,rating_b_before,participation_a,participation_b,rating_change_a,rating_change_b,rating_a_after,rating_b_after), as.numeric)
  )
}

hash_password <- function(x) digest(x, algo="sha256", serialize=FALSE)
is_admin_password_valid <- function(x) nzchar(ADMIN_PASSWORD_HASH) && identical(hash_password(x), ADMIN_PASSWORD_HASH)

gap_adjustment <- function(g){ if(g<=20)0 else if(g<=40).3 else if(g<=60).5 else if(g<=80).7 else if(g<=100)1 else 1.5 }
draw_bonus <- function(g){ if(g<=20)0 else if(g<=40)1 else if(g<=60)2 else if(g<=80)3 else if(g<=100)4 else 5 }

load_players <- function() read_csv(PLAYERS_FILE, show_col_types=FALSE)

dropbox_download_matches <- function(){
  if(!nzchar(DROPBOX_TOKEN)){
    if(!file.exists(LOCAL_MATCHES_FILE)) return(empty_matches)
    return(normalize_matches(read_csv(LOCAL_MATCHES_FILE, show_col_types=FALSE)))
  }
  req <- request("https://content.dropboxapi.com/2/files/download") %>%
    req_headers(Authorization=paste("Bearer",DROPBOX_TOKEN),
                `Dropbox-API-Arg`=toJSON(list(path=DROPBOX_MATCHES_PATH),auto_unbox=TRUE))
  resp <- tryCatch(req_perform(req), error=function(e) NULL)
  if(is.null(resp) || resp_status(resp)>=400) return(empty_matches)
  tmp <- tempfile(fileext=".csv"); writeBin(resp_body_raw(resp),tmp)
  normalize_matches(read_csv(tmp,show_col_types=FALSE))
}

dropbox_upload_matches <- function(x){
  x <- normalize_matches(x); write_csv(x,LOCAL_MATCHES_FILE)
  if(!nzchar(DROPBOX_TOKEN)) return(invisible(TRUE))
  tmp <- tempfile(fileext=".csv"); write_csv(x,tmp)
  req <- request("https://content.dropboxapi.com/2/files/upload") %>%
    req_headers(Authorization=paste("Bearer",DROPBOX_TOKEN),
                `Dropbox-API-Arg`=toJSON(list(path=DROPBOX_MATCHES_PATH,mode="overwrite",autorename=FALSE,mute=TRUE,strict_conflict=FALSE),auto_unbox=TRUE),
                `Content-Type`="application/octet-stream") %>% req_body_file(tmp)
  resp <- req_perform(req)
  if(resp_status(resp)>=300) stop("Dropbox upload failed")
  invisible(TRUE)
}

player_rating_before <- function(p,players,matches){
  base <- players %>% filter(player==p) %>% pull(base_rating); if(length(base)==0) base <- 300
  pm <- matches %>% filter(player_a==p | player_b==p); if(nrow(pm)==0) return(base)
  base + sum(ifelse(pm$player_a==p,pm$rating_change_a,pm$rating_change_b),na.rm=TRUE) +
    sum(ifelse(pm$player_a==p,pm$participation_a,pm$participation_b),na.rm=TRUE)
}

joined_cup <- function(p,cup,m){ nrow(m)>0 && any(m$cup==cup & (m$player_a==p | m$player_b==p),na.rm=TRUE) }

calculate_match <- function(pa,ga,pb,gb,stage,cup,players,prior){
  ra0 <- player_rating_before(pa,players,prior); rb0 <- player_rating_before(pb,players,prior)
  ba <- ifelse(joined_cup(pa,cup,prior),0,participation_bonus); bb <- ifelse(joined_cup(pb,cup,prior),0,participation_bonus)
  ra <- ra0+ba; rb <- rb0+bb; gap <- abs(ra-rb); adj <- gap_adjustment(gap)
  rule <- stage_rules %>% filter(stage==!!stage); if(nrow(rule)==0) stop("Invalid stage")
  wb <- rule$win_base[1]; lb <- rule$loss_base[1]
  if(ga>gb){
    ca <- if(ra<=rb) round(wb*(1+adj)) else round(wb/(1+adj))
    cb <- if(rb>=ra) -round(lb*(1+adj)) else -round(lb/(1+adj))
  } else if(gb>ga){
    cb <- if(rb<=ra) round(wb*(1+adj)) else round(wb/(1+adj))
    ca <- if(ra>=rb) -round(lb*(1+adj)) else -round(lb/(1+adj))
  } else {
    pts <- draw_bonus(gap)
    if(ra==rb){ca<-0;cb<-0} else if(ra<rb){ca<-pts;cb<--pts} else {ca<--pts;cb<-pts}
  }
  tibble(rating_a_before=ra,rating_b_before=rb,participation_a=ba,participation_b=bb,
         rating_change_a=ca,rating_change_b=cb,rating_a_after=ra+ca,rating_b_after=rb+cb)
}

recalculate_all <- function(m,players){
  m <- normalize_matches(m) %>% arrange(match_id); if(nrow(m)==0) return(empty_matches)
  out <- empty_matches
  for(i in seq_len(nrow(m))){
    r <- m[i,]; c <- calculate_match(r$player_a,r$goals_a,r$player_b,r$goals_b,r$stage,r$cup,players,out)
    out <- bind_rows(out,tibble(match_id=r$match_id,date=r$date,cup=r$cup,stage=r$stage,player_a=r$player_a,goals_a=r$goals_a,
                               player_b=r$player_b,goals_b=r$goals_b,rating_a_before=c$rating_a_before,rating_b_before=c$rating_b_before,
                               participation_a=c$participation_a,participation_b=c$participation_b,rating_change_a=c$rating_change_a,
                               rating_change_b=c$rating_change_b,rating_a_after=c$rating_a_after,rating_b_after=c$rating_b_after))
  }
  out
}

build_ranking <- function(players,matches){
  bind_rows(lapply(players$player,function(p){
    b<-players%>%filter(player==p); pm<-matches%>%filter(player_a==p|player_b==p)
    rc<-if(nrow(pm)) sum(ifelse(pm$player_a==p,pm$rating_change_a,pm$rating_change_b),na.rm=TRUE) else 0
    bo<-if(nrow(pm)) sum(ifelse(pm$player_a==p,pm$participation_a,pm$participation_b),na.rm=TRUE) else 0
    w<-if(nrow(pm)) sum((pm$player_a==p&pm$goals_a>pm$goals_b)|(pm$player_b==p&pm$goals_b>pm$goals_a)) else 0
    l<-if(nrow(pm)) sum((pm$player_a==p&pm$goals_a<pm$goals_b)|(pm$player_b==p&pm$goals_b<pm$goals_a)) else 0
    gf<-if(nrow(pm)) sum(ifelse(pm$player_a==p,pm$goals_a,pm$goals_b),na.rm=TRUE) else 0
    ga<-if(nrow(pm)) sum(ifelse(pm$player_a==p,pm$goals_b,pm$goals_a),na.rm=TRUE) else 0
    tibble(player=p,rating=b$base_rating+rc+bo,matches=b$base_matches+nrow(pm),wins=b$base_wins+w,losses=b$base_losses+l,
           goals_for=b$base_gf+gf,goals_against=b$base_ga+ga)
  })) %>% mutate(goal_difference=goals_for-goals_against,win_rate=ifelse(matches==0,0,100*wins/matches)) %>%
    arrange(desc(rating),desc(wins),desc(goal_difference),player) %>% mutate(rank=row_number()) %>% select(rank,everything())
}

player_history <- function(p,players,matches){
  base <- players%>%filter(player==p)%>%pull(base_rating); pm<-matches%>%filter(player_a==p|player_b==p)%>%arrange(match_id)
  h<-tibble(game=0,label="Baseline",rating=base); cur<-base
  if(nrow(pm)==0) return(h)
  for(i in seq_len(nrow(pm))){r<-pm[i,]; cur<-cur+ifelse(r$player_a==p,r$participation_a,r$participation_b)+ifelse(r$player_a==p,r$rating_change_a,r$rating_change_b)
    opp<-ifelse(r$player_a==p,r$player_b,r$player_a); h<-bind_rows(h,tibble(game=i,label=paste0(r$cup," vs ",opp),rating=cur))}
  h
}

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f4f6f8}.cardx{background:white;border-radius:14px;padding:18px;box-shadow:0 2px 12px rgba(0,0,0,.08);margin-bottom:18px}.metric{font-size:28px;font-weight:800}.metric-label{color:#6c757d;font-size:12px;text-transform:uppercase}.admin-ok{color:#138a36;font-weight:700}.admin-no{color:#b42318;font-weight:700}.danger-box{border:1px solid #d92d20}"))),
  navbarPage("Football Rating",id="tabs",
    tabPanel("Ranking",div(class="cardx",h3("Live Ranking"),p("Click a player to open the full profile."),DTOutput("ranking"))),
    tabPanel("Players",div(class="cardx",selectInput("profile_player","Player",choices=NULL),uiOutput("profile_header")),
      fluidRow(column(2,div(class="cardx",div(class="metric-label","Rating"),div(class="metric",textOutput("mr")))),column(2,div(class="cardx",div(class="metric-label","Rank"),div(class="metric",textOutput("mrank")))),column(2,div(class="cardx",div(class="metric-label","Matches"),div(class="metric",textOutput("mm")))),column(2,div(class="cardx",div(class="metric-label","Wins"),div(class="metric",textOutput("mw")))),column(2,div(class="cardx",div(class="metric-label","Win rate"),div(class="metric",textOutput("mwr")))),column(2,div(class="cardx",div(class="metric-label","Goal diff"),div(class="metric",textOutput("mgd"))))),
      fluidRow(column(8,div(class="cardx",h3("Rating Progress"),plotOutput("rating_chart",height="380px"))),column(4,div(class="cardx",h3("Performance"),plotOutput("perf",height="380px")))),
      div(class="cardx",h3("Player Match History"),DTOutput("player_matches"))),
    tabPanel("Matches",div(class="cardx",h3("Official Matches"),DTOutput("matches_public"))),
    tabPanel("Admin",uiOutput("admin_panel"))
  )
)

server <- function(input,output,session){
  players<-reactiveVal(load_players()); matches<-reactiveVal(dropbox_download_matches()); admin<-reactiveVal(FALSE); selected_id<-reactiveVal(NULL)
  observe({invalidateLater(20000,session); x<-tryCatch(dropbox_download_matches(),error=function(e)NULL); if(!is.null(x))matches(x)})
  rankdat<-reactive(build_ranking(players(),matches()))
  observe(updateSelectInput(session,"profile_player",choices=players()$player))

  output$ranking<-renderDT(datatable(rankdat()%>%mutate(win_rate=paste0(round(win_rate,1),"%")),selection="single",rownames=FALSE,options=list(pageLength=25,dom="tip")))
  observeEvent(input$ranking_rows_selected,{i<-input$ranking_rows_selected;if(length(i)==1){updateSelectInput(session,"profile_player",selected=rankdat()$player[i]);updateTabsetPanel(session,"tabs",selected="Players")}})
  stats<-reactive({req(input$profile_player);rankdat()%>%filter(player==input$profile_player)})
  output$profile_header<-renderUI({s<-stats();div(style="font-size:30px;font-weight:800",s$player,tags$span(style="font-size:16px;color:#6c757d;margin-left:12px",paste0("#",s$rank," • Rating ",s$rating)))})
  output$mr<-renderText(stats()$rating);output$mrank<-renderText(paste0("#",stats()$rank));output$mm<-renderText(stats()$matches);output$mw<-renderText(stats()$wins);output$mwr<-renderText(paste0(round(stats()$win_rate,1),"%"));output$mgd<-renderText(ifelse(stats()$goal_difference>0,paste0("+",stats()$goal_difference),stats()$goal_difference))
  output$rating_chart<-renderPlot({h<-player_history(input$profile_player,players(),matches());p<-ggplot(h,aes(game,rating))+geom_point(size=3)+theme_minimal(base_size=13)+labs(x="Official match",y="Rating",title=paste(input$profile_player,"Rating History"));if(nrow(h)>=2)p<-p+geom_line(linewidth=1.1);p})
  output$perf<-renderPlot({s<-stats();d<-tibble(result=c("Wins","Losses","Other"),count=c(s$wins,s$losses,max(0,s$matches-s$wins-s$losses)));ggplot(d,aes(result,count))+geom_col(width=.65)+geom_text(aes(label=count),vjust=-.4,size=5)+theme_minimal(base_size=13)+labs(x=NULL,y="Matches")})
  output$player_matches<-renderDT({p<-input$profile_player;m<-matches()%>%filter(player_a==p|player_b==p)%>%arrange(desc(match_id));if(nrow(m)==0)return(datatable(data.frame(Message="No match history."),rownames=FALSE));datatable(m%>%mutate(Opponent=ifelse(player_a==p,player_b,player_a),GF=ifelse(player_a==p,goals_a,goals_b),GA=ifelse(player_a==p,goals_b,goals_a),Result=case_when(GF>GA~"Win",GF<GA~"Loss",TRUE~"Draw"),Change=ifelse(player_a==p,rating_change_a,rating_change_b),RatingAfter=ifelse(player_a==p,rating_a_after,rating_b_after))%>%transmute(Date=date,Cup=cup,Stage=stage,Opponent,Score=paste(GF,"-",GA),Result,`Rating Change`=ifelse(Change>0,paste0("+",Change),as.character(Change)),`Rating After`=RatingAfter),rownames=FALSE,options=list(pageLength=15,dom="tip"))})
  output$matches_public<-renderDT({m<-matches();if(nrow(m)==0)return(datatable(data.frame(Message="No official matches yet."),rownames=FALSE));datatable(m%>%arrange(desc(match_id))%>%transmute(ID=match_id,Date=date,Cup=cup,Stage=stage,`Player A`=player_a,Score=paste(goals_a,"-",goals_b),`Player B`=player_b),rownames=FALSE,options=list(pageLength=20,dom="tip"))})

  output$admin_panel<-renderUI({if(!admin())div(class="cardx",h3("Admin Login"),passwordInput("admin_password","Password"),actionButton("admin_login","Login",class="btn-primary"),br(),br(),div(class="admin-no","Match entry is locked.")) else tagList(
    div(class="cardx",fluidRow(column(8,h3("Admin"),div(class="admin-ok","Admin access enabled.")),column(4,actionButton("admin_logout","Logout")))),
    div(class="cardx",h3("Add Official Match"),textInput("cup","Cup ID",placeholder="Example: CUP-02"),selectInput("stage","Stage",choices=stage_rules$stage),selectInput("player_a","Player A",choices=players()$player),numericInput("goals_a","Goals A",0,min=0),selectInput("player_b","Player B",choices=players()$player,selected=players()$player[min(2,length(players()$player))]),numericInput("goals_b","Goals B",0,min=0),actionButton("add_match","Save Match",class="btn-success")),
    div(class="cardx",h3("Manage Existing Matches"),p("Select a match to edit or delete. All later ratings will be recalculated automatically."),DTOutput("admin_matches")),uiOutput("edit_panel"),
    div(class="cardx",downloadButton("download_matches","Download Matches CSV")) )})

  observeEvent(input$admin_login,{if(is_admin_password_valid(input$admin_password)){admin(TRUE);showNotification("Admin login successful.",type="message")}else showNotification("Incorrect password.",type="error")})
  observeEvent(input$admin_logout,{admin(FALSE);selected_id(NULL)})

  observeEvent(input$add_match,{req(admin());if(trimws(input$cup)==""){showNotification("Cup ID is required.",type="error");return()};if(input$player_a==input$player_b){showNotification("Players must be different.",type="error");return()};m<-matches();id<-if(nrow(m)==0)1L else as.integer(max(m$match_id,na.rm=TRUE)+1L);raw<-tibble(match_id=id,date=Sys.Date(),cup=trimws(input$cup),stage=input$stage,player_a=input$player_a,goals_a=as.integer(input$goals_a),player_b=input$player_b,goals_b=as.integer(input$goals_b),rating_a_before=NA_real_,rating_b_before=NA_real_,participation_a=NA_real_,participation_b=NA_real_,rating_change_a=NA_real_,rating_change_b=NA_real_,rating_a_after=NA_real_,rating_b_after=NA_real_);reb<-recalculate_all(bind_rows(m,raw),players());tryCatch({dropbox_upload_matches(reb);matches(reb);showNotification("Match saved and ranking recalculated.",type="message")},error=function(e)showNotification(paste("Save failed:",conditionMessage(e)),type="error",duration=NULL))})

  output$admin_matches<-renderDT({req(admin());m<-matches();if(nrow(m)==0)return(datatable(data.frame(Message="No matches available."),rownames=FALSE));datatable(m%>%arrange(desc(match_id))%>%transmute(ID=match_id,Date=date,Cup=cup,Stage=stage,`Player A`=player_a,`Goals A`=goals_a,`Player B`=player_b,`Goals B`=goals_b,`A Change`=rating_change_a,`B Change`=rating_change_b),selection="single",rownames=FALSE,options=list(pageLength=15,dom="tip"))})
  observeEvent(input$admin_matches_rows_selected,{req(admin());i<-input$admin_matches_rows_selected;if(length(i)==1){ord<-matches()%>%arrange(desc(match_id));selected_id(ord$match_id[i])}})

  output$edit_panel<-renderUI({req(admin(),selected_id());r<-matches()%>%filter(match_id==selected_id());req(nrow(r)==1);div(class="cardx danger-box",h3(paste("Edit Match #",r$match_id)),textInput("edit_cup","Cup ID",value=r$cup),selectInput("edit_stage","Stage",choices=stage_rules$stage,selected=r$stage),selectInput("edit_pa","Player A",choices=players()$player,selected=r$player_a),numericInput("edit_ga","Goals A",r$goals_a,min=0),selectInput("edit_pb","Player B",choices=players()$player,selected=r$player_b),numericInput("edit_gb","Goals B",r$goals_b,min=0),actionButton("update_match","Update Selected Match",class="btn-warning"),tags$span(" "),actionButton("delete_match","Delete Selected Match",class="btn-danger")))

  observeEvent(input$update_match,{req(admin(),selected_id());if(trimws(input$edit_cup)==""){showNotification("Cup ID is required.",type="error");return()};if(input$edit_pa==input$edit_pb){showNotification("Players must be different.",type="error");return()};m<-matches();p<-which(m$match_id==selected_id());m$cup[p]<-trimws(input$edit_cup);m$stage[p]<-input$edit_stage;m$player_a[p]<-input$edit_pa;m$goals_a[p]<-as.integer(input$edit_ga);m$player_b[p]<-input$edit_pb;m$goals_b[p]<-as.integer(input$edit_gb);reb<-recalculate_all(m,players());tryCatch({dropbox_upload_matches(reb);matches(reb);selected_id(NULL);showNotification("Match updated. All ratings recalculated.",type="message")},error=function(e)showNotification(paste("Update failed:",conditionMessage(e)),type="error",duration=NULL))})

  observeEvent(input$delete_match,{req(admin(),selected_id());showModal(modalDialog(title="Delete Match",paste0("Delete match #",selected_id(),"? Ratings will be recalculated."),footer=tagList(modalButton("Cancel"),actionButton("confirm_delete","Delete Match",class="btn-danger")),easyClose=FALSE))})
  observeEvent(input$confirm_delete,{req(admin(),selected_id());m<-matches()%>%filter(match_id!=selected_id());reb<-recalculate_all(m,players());tryCatch({dropbox_upload_matches(reb);matches(reb);selected_id(NULL);removeModal();showNotification("Match deleted. All ratings recalculated.",type="message")},error=function(e){removeModal();showNotification(paste("Delete failed:",conditionMessage(e)),type="error",duration=NULL)})})

  output$download_matches<-downloadHandler(filename=function()paste0("football_matches_backup_",Sys.Date(),".csv"),content=function(file)write_csv(matches(),file))
}

shinyApp(ui,server)
