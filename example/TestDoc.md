# Class - TwitterBot
## Description
This is the class where everything you've worked on thus far comes together!
You can see that we've provided a path to a CSV file full of tweets and the
column from which they can be extracted. When run as an application, this
program builds a Markov Chain from the training data in the CSV file,
generates 10 random tweets, and prints them to the terminal.
<p>
This class also provides the writeTweetsToFile method, which can be used to
create a file containing randomly generated tweets.
<p>
Note: All IOExceptions thrown by writers should be caught and handled
properly.
# Field - mc
## Description
The MarkovChain you'll be using to generate tweets
# Method - 
## Description
Given a column and a buffered reader, initializes the TwitterBot by
training the MarkovChain with sentences sourced from the reader. Uses
the RandomNumberGenerator().
*
## Parameter - br
- a buffered reader containing tweet data
## Parameter - tweetColumn
- the column in the reader where the text of the tweet
