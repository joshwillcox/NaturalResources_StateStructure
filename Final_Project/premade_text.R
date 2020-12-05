intro <- "There has been much speculation in academia about the effects of 
          a country's reliance on natural resources. Some have argued that
          when states can rely on natural resources for their financing,
          they will not attempt to create effective states with a high tax 
          extraction capacity. If they can get away with not taxing their 
          population while remaining fiscally stable, they won't. Within this
          project, I wanted to see if there were certain features of states that
          correlated with a reliance on natural resources. I look at variables 
          such as regime type, the strength of legal rights, military 
          expenditure etc."

to_think_about <- "One aspect of this project I have been very careful about is 
                   drawing conclusions that suggest a causal relationship. While
                   I can show that states which rely on natural resources are 
                   more often less democratic, I can not claim that this
                   reliance has caused authoritarianism to dominate. To draw 
                   causal conclusions, I would have had to been able to 
                   manipulate my data in some way, such as randomly assigning 
                   natural resource endowment to certain states. This, obviously
                   , is quite infeasible hence why I am careful about where I 
                   make causal inferences."




tab1_row1_col <- "This graph shows us that there is a negative 
                  correlation between the percentage of GDP that is 
                  from Tax Revenue and that which is from Natural 
                  Resources. While we cannot draw causal conclusions from
                  the data, it does show that countries which rely on
                  natural resources, are less likely to rely on tax
                  revenue. Using this graph, it becomes easier to think how 
                  exactly to put together the model."



about_p1 <- "For my project, I want to look at the effects that natural 
              resources/foreign aid can have on the strucutre of the state and
              its capacity. In one of my classes recently, we talked about how
              access to resources or foreign aid incentivises countries not to 
              develop a strong state. They prefer to not tax citizens and depend
              on other sources because then they do not have to provide benefits
              to the population. In the small graph I have made for this
              milestone, I have tried to see if there is any connection between 
              tax revenue as a  percentage of GDP and natural resource rents as 
              a percentage of GDP."

about_p2 <- "I still need to find a way to create the map of the world as the 
             data frame which I downloaded hasn't worked so far. I also want to
             create a tab that has a panel which lets me look at each country 
             and the factors within each country such as tax revenue as a 
             percentage of state revenue or natural resources. The map issue is 
             my biggest issue at the moment. This week, I created the seperate
             R file to put all my text and was able to bring in my graph without 
             copy and pasting from my changingdata.R file"

about_p3 <- "My name is Josh Willcox and I am currently a second-year 
              undergraduate intending to do a joint concentration in Near 
              Eastern Languages and Civilisations with History."

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

def_efficiency <- "What exactly do I mean when I use the term state effectiveness?
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
                  There are only 8 countries assigned as singe regimes. This may
                  be making it difficult to create good models."

resultsform1c <- "As for the clearer conclusions,"
