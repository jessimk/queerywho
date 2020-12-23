
library(tidyverse)
library(shiny)

## source for card grid: https://stackoverflow.com/questions/55741986/create-dynamic-number-of-card-elements-in-shiny-flowlayout
## source for checkbox img div toggling: https://stackoverflow.com/questions/37187202/css-show-hide-elements-based-on-checkbox

cards <- tribble(
    ~rn,  ~card, ~card_link,
    1, 'Carol', 'https://media1.tenor.com/images/2f1f43f094695c9d6955ca512500a49a/tenor.gif',
    2, 'Furiosa', 'https://media.giphy.com/media/zmJaqO8evZORq/giphy.gif',
    3, 'Celine Sciamma', 'https://france-amerique.com/wp-content/uploads/2020/02/celine-Sciama05-scaled-e1581545250719.jpg',
    4, 'Roxane Gay', 'https://www.c21uwm.com/tennessenscholars/wp-content/uploads/2018/11/Roxanne-Gay.jpeg',
    
    5, 'Bette Porter', 'https://media.giphy.com/media/MYD7995yDwoSPfZ49O/giphy.gif',
    6, 'Abby McEnany', 'https://media1.tenor.com/images/6bc3143805fdb0a0b9bd7c1f289af920/tenor.gif',
    7, 'Shane', 'https://media.giphy.com/media/RfHEZLMwFDifW9krfj/giphy.gif',
    8, 'Alice', 'https://media.giphy.com/media/TE67UwJmyyYyp5UG5D/giphy.gif',
    
    9, 'Jean Milburn', 'https://i.pinimg.com/originals/53/b3/5a/53b35a7d95a9576cb79bdae37f6df027.gif',
    10, 'Jenny Shimizu', 'https://64.media.tumblr.com/60eb88de0bda14de02576ba558424a9f/tumblr_p4uktvJk7d1rzbj5mo9_540.gifv',
    14, 'Angela Merkel', 'https://thumbs.gfycat.com/SophisticatedSoggyGermanspaniel-size_restricted.gif',
    19, 'Janelle Monae', 'https://media.giphy.com/media/X9jpz0H0zOXwaY0N3d/giphy.gif',
    
    11, 'Eddy Martinez', 'https://media.giphy.com/media/4Zim7xcHTmLKEs0Cy7/giphy.gif',
    12, 'Adelle Haenel', 'https://64.media.tumblr.com/3a52fc1a0949b2e3c2b95194b9bf2b73/0d092af29a152173-9c/s400x600/f0eb3186184f10aa34f19fbb77a718255718ba58.gifv',
    18, 'Sandra Oh', 'https://64.media.tumblr.com/28917f88e0945bf14a574aab22701e28/tumblr_p6wxilaPBP1vsvwv3o1_400.gif',
    16, 'Sue Bird', 'https://media.giphy.com/media/c2bjs3fDkmIPmlek78/giphy.gif', 
    
    17, 'Sarah Paulson', 'https://media2.giphy.com/media/X2eaw8wodBolMtB0Bo/giphy-downsized-medium.gif',
    15, 'Abby Wambach', 'https://64.media.tumblr.com/1b2d70ac555c954be58f0b67efb06e2c/tumblr_o1htl7kGqv1uqf1aho1_400.gifv', 
    13, 'Hannah Gadsby', 'https://logoonline.mtvnimages.com/uri/mgid:file:http:shared:s3.amazonaws.com/articles.newnownext.com-production/wp-content/uploads/2019/04/tired-1554904992-1554904994.gif',
    20, 'Poussey Washington', 'https://i.gifer.com/3Ng2.gif',
    
    24, 'Missy Elliott','https://media.giphy.com/media/j5K6QjsEGzJpvjIvwK/giphy.gif',
    21, 'Gentleman Jack', 'https://i.pinimg.com/originals/ed/1f/da/ed1fda3f4a2457f0b55a04c3a42f574e.gif',
    22, 'Beth Ditto','https://media.giphy.com/media/YzCpdWpg7qa88/giphy.gif', 
    23, 'Lena Waithe','https://media.giphy.com/media/7zQ0eaph4P3GH0cPRW/giphy.gif'
    )

card <- function(.card, .card_link) {
    HTML(
        paste0(
      '
      <div class="container1">
      <h5><i>', .card, '</i></h5>
      
      <input type="checkbox">

        <div class="card_img">
        <img src="', .card_link, '" style="width:100%">
        </div>
      
          <div class="container2">
          <br>
          </div>


      </div>')
    )
}


ui <- fluidPage(
  
  titlePanel("Queery Who?"),
  
  sidebarLayout(
    sidebarPanel(
                 br(), 
                 br(),
                 span("Draw a card. Guess your opponent's card before they guess yours. As you learn about your opponent's card, hide cards (with the checkbox) to narrow down the possibilities. 
                   Traditionally, it's yes/no questions only but meh. More rules can be found ",a( href='https://winning-moves.com/images/guesswho%20rules.pdf', "here.")),
                 br(),
                 br(),
                 actionButton("random_draw", "Draw a card!"),
                 br(),
                 br(),
                 htmlOutput("image"),
                 br(),
                 htmlOutput("img_text"),
                 br(),
                 width = 3
    ),
    
    mainPanel(
            
            tags$head(tags$style('.container1 {
                      width: 200px;
                       clear: both;
                       /* Add shadows to create the "card" effect */
                       box-shadow: 0 4px 6px 0 rgba(0,0,0,0.2);
                       transition: 0.3s;
                       }
                       /* On mouse-over, add a deeper shadow */
                       .container1:hover {
                       box-shadow: 0 8px 10px 0 rgba(0,0,0,0.2);
                       }
                       /* Add some padding inside the card container */
                       .container1 {
                       width: 200px;
                       padding: 4px 4px;
                       }
                       
                      .container1 input:checked ~ .card_img { display: none; }
                       
                        /* Add some padding inside the card container */
                       .container2 {
                       width: 200px;
                       padding: 4px 4px;
                       }
                       

                                 }')),
            uiOutput("card_grid")
        )
    ),
)

server <- function(input, output, session) {

    src <- eventReactive(input$random_draw, {
        sample(1:dim(cards)[1], 1)
    })
    
    output$image <- renderText({
        paste('<img src="',cards$card_link[src()],'" width="200">', sep="")
    })
    
    output$img_text <- renderText({
        paste('Lucky! You\'ve drawn ',cards$card[src()],'!', sep="")
    })
    
    output$card_grid <- renderUI({
        
        # make the cards
        args <- lapply(1:dim(cards)[1], function(.x) card(.card = cards[.x, "card"],
                                              .card_link = cards[.x, "card_link"]))
        
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 2px;
        ")
        
        do.call(shiny::flowLayout, args)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

