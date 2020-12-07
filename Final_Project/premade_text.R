intro1 <- "The inspiration for this project came from a collection of readings 
           in GOV 20 that focus on the processes of state building and under 
           what conditions countries would not feel the need to develop 
           effective states. I will define state capacity more clearly in the 
           'Comparing Countries' section. One of the arguments put forward by 
           Robert Bates in his analysis of state development in Africa is that 
           it depends on the revenues available to the political elite. If an 
           elite can rule a country without having to tax its citizens it'll 
          take that option because it is the path of least resistance. Taxation 
          implies a reciprocal relationship  whereby the citzens pay their taxes
          and in return the government provides efficient state services.
          Elites rarely want to do this.
          Consequently, if a government can find other revenues such as 
          political aid or natural resources, they will exploit these instead."

intro2 <- "While I would have loved to look into political aid as a
           variable in my model, I found myself quite overwhelmed trying to 
           collect the data for state efficiency and natural resources. For that
           reason, I left political aid out in this project."

to_think_about <- "One aspect of this project I have been very careful about is 
                   drawing conclusions that suggest a causal relationship. While
                   I can show that states which rely on natural resources are 
                   more often less democratic, or rely less on developing a 
                   high tax capacity, I can not claim for sure that this
                   reliance has caused the latter cases to occur. To draw 
                   causal conclusions, I would have had to been able to 
                   manipulate my data in some way, such as randomly assigning 
                   natural resource endowment to certain states. This, 
                   obviously, is quite infeasible hence why I am careful
                   about making causal inferences."




tab1_row1_col <- "This graph shows us that there is a negative 
                  correlation between the percentage of GDP that is 
                  from Tax Revenue and that which is from Natural 
                  Resources. While we cannot draw causal conclusions from
                  the data, it does show that countries which rely on
                  natural resources, are less likely to rely on tax
                  revenue. Using this graph, it becomes easier to think how 
                  exactly to put together the model."



about_p1 <- "For my project, I wanted to look at the relationship between 
             natural resource revenues and the structure of the state. In
             particular, I was interested in whether natural 
             resources offloaded the need to develop high tax capacities. One 
             of my conclusions was that there seems to be a relationship between
             tax revenues as % of GDP and natural resource revenues in 
             Monarchical regimes and Multiparty regimes. They both predict
             that natural resource revenues would be high if tax revenues 
             were 0%. As the tax revenue percentage increases, the model 
             predicts that the natural resource percentage is likely to 
             decrease."

about_p2 <- "If I were to develop this project, I would want to gather more data
             to take into account other sources of revenue such as politcal and
             economic aid. Similarly, I would like to look at variances of state
             effectiveness on a county level. Throughout this project, it stayed
             very macro; however, I think it would be really interesting to look
             at the variables which effect if the postman comes on a local level
             within one country."

about_p3 <- "My name is Josh Willcox and I am currently a second-year 
              undergraduate intending to do a joint concentration in Near 
              Eastern Languages and Civilisations with History."

about_p4 <- "Another very interesting conlcusion from my project was the 
             relationship between natural resoure revenues and democracy. In 
             the final section of the 'model' tab, you can find a graph showing
             the predicted democractic ratings for countries with differing
             dependence on natural resources. It suggests that if a country 
             relies more on natural resources, it is much more probable that it
             is autocratic."

variablesexplanation <-  "While this map and table is helpful in getting
                         a broad picture of which states rely most on resources
                         and which states have weak states, we must be careful
                         of thinking that this is the full picture. As can be 
                         seen from some of the variables, there are many 
                         countries where data is missing. One could imagine that
                         countries with the weakest states are least likely to 
                         to have data about them. This poses a problem when I 
                         come to try put indicators into regressions. I may end 
                         up running into data collection problems as the data I 
                         need most to prove that states are weak are the least
                         likely to be found for the very fact that the state is
                         weak. Another thing to be careful of is the 
                         reliability of these variables. While I used the World 
                         Bank indicators for the very reason of reliability,
                         there still may be some discrpeancies in the reporting 
                         of the data."

def_efficiency <- "What exactly do I mean when I use the term state
                   effectiveness?
                   It's important to note that I'm not referring to whether
                   the state is interventionist or not. Communist, socialist, 
                   free-market - state effectiveness can range wildly across 
                   these types of systems. What I'm referring to here, is how 
                   well the state machinery runs. Does the postman come? Are 
                   national trains on time? If you ignore a parking fine, does 
                   someone knock on your door? Are judges independent? Can you 
                   pay off police officers? Most importantly of all these: Does 
                   the state have the power to make you pay taxes?"

why_efficiency <- "Why am I interested in state efficiency? For this project, I 
                   wanted to see if there was a relationship between types of 
                   state revenues and efficiency. If political elites rely on 
                   oil or natural minerals, do they bother with making the 
                   trains arrive on time? Similarly, I thought it would be 
                   interesting to find if legal rights were weaker in big 
                   natural resource producers."

interestingfinds <- "Among some of the interesting variables here, I like being
                     able to differentiate between countries that rely on 
                     oil and those that rely on natural minerals. The ease of 
                     business index is interesting too. It ranks
                     economies from 1 to 190, with first place being the best.
                     There seems to be quite a few overlaps with the countries 
                     that depend highly on natural resources. This also applies
                     to the strenght of legal rights index."

regime_exp <- "In this section, you can see a visual of the different types of
               regimes around the world. It may be useful to outline how exactly
               these regimes have been defined. Monarchies are not only 
               countries where there is a royal family, but also where they are 
               the principal decision makers. This explains why the UK is not 
               coded as a monarchy. Military, Democracatic and Single Party 
               Regimes should be pretty self explanatory. As for the Multiparty 
               regime, this is where 'Politics are highly biased in favor of the
               ruling party, but competition is real'(Magaloni, 
               Chu and Min 2013, p. 8)" 

formula1 <- "$$ resource_i = \\beta_0 + \\beta_1x_{MIL, i} + \\beta_2x_{MON, i} 
                + \\beta_3x_{MUL, i} + \\beta_4x_{SIN, i}  + \\beta_5taxrev_i +
                \\beta_6x_{MIL, i}*taxrev_i + \\beta_7x_{MON, i}*taxrev_i +
                \\beta_8x_{MUL, i}*taxrev_i + \\beta_9x_{SIN, i}*taxrev_i + 
                \\epsilon_i $$"

formulaexpl <- "While the equation for this model might seem rather long, it is 
                only because I have done an interaction term using a variable 
                with five different categories (the regime types). The regime 
                aspects of the equation either take a value of 0 or 1 depending
                on if the regime is true or not. The taxrev parts refer to tax 
                revenue as a percentage of GDP. Having things as a percentage of
                GDP is particularly useful for making models becuase it means I 
                don't have to concentrate as much on controlling for the size of
                the country/its economy." 

resultsform1a <- "One thing to note before looking at the values themselves is 
                 that the intercept here is Democracy  hence why it does not
                 have values."

resultsform1b <- "The first thing to notice is the regimes which do not seem to
                  give any clear answer. That applies for democracies, military
                  rule, and singe party regimes. The confidence intervals all
                  straddle 0 and are very large especially given that 
                  the outcome is supposed to be somewhere between 0 and 100. 
                  This may be a consequence of using countries. 
                  There are only 8 countries assigned as single party regimes.
                  This may
                  be making it difficult to create good models. That said, the 
                  lack of a clear answer is interesting in itself. It implies 
                  that perhaps there is something about these countries whereby
                  their institutions are effective/ineffective regardless of 
                  the presence of natural resources."

resultsform1c <- "As for the clearer conclusions, the results for monarchies and
                  multiparties are interesting. Their confidence intervals are 
                  both wide, but they are comfortably positive. From this, we 
                  can conclude that if a country has a multiparty or monarchical
                  regime, resource revenues likely make up a substantial 
                  proportion of their GDP."

resultsform1d <- "While, as seen from the graphs below, we can create graphs and
                  predictions for military and multiparty regimes whose tax 
                  revenue is 20% of GDP, the table below attempts to put this
                  into perspective. Is it likely that a country whose tax 
                  revenue is 20% of GDP is a monarchy or some other type of non
                  democratic regime? No. When the democracy rating is 0 (on a 
                  scale from -10 to 10), the predicted tax revenue as % of GDP
                  is below 20%. What this should tell us is that we 
                  need to be careful about what exactly we use our model to 
                  predict. Is our quesiton within the scope of the model's 
                  capabilities?"

formula2 <- "$$ democracy_i = \\beta_0 + \\beta_1resource_i 
                \\epsilon_i $$"

formulaexpl2 <- "In this section, I wanted to put aside state efficiency and
                 look solely at the relationship between democracy and relying 
                 on natural resources. In the first model, my regime types were
                 coded into five categories. While this is useful when it is 
                 an input, it becomes a bit more confusing when I'm trying to 
                 make it an output. I therefore found a different data set that
                 ranked democracies from -10 to 10 with the 10 being a full 
                 democracy and -10 being a full autocracy. The data is set 
                 is used briefly above."

resultsform2a <- "While this regression is somewhat simpler, it does a good job
                  of looking at the relationship between dependence on natural
                  resources and democracy. The intercept point refers to the 
                  predicted output when the revenue from natural resources as a 
                  a percentage of GDP is 0. The confidence interval shows that 
                  a country with no resources is likely to be closer to a full 
                  democracy than autocracy. Additionally, the coefficient for 
                  resource percentage says a lot. The confidence interval is 
                  comfortably negative. It may seem a small number, but it has 
                  to be remembered that this only for each 1% change. If a 
                  country's natural resource revenue was between 30 and 40 
                  percent, the predicted democracy level goes below 0."