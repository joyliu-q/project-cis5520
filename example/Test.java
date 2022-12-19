/**
 * This is the class where everything you've worked on thus far comes together!
 * You can see that we've provided a path to a CSV file full of tweets and the
 * column from which they can be extracted. When run as an application, this
 * program builds a Markov Chain from the training data in the CSV file,
 * generates 10 random tweets, and prints them to the terminal.
 * <p>
 * This class also provides the writeTweetsToFile method, which can be used to
 * create a file containing randomly generated tweets.
 * <p>
 * Note: All IOExceptions thrown by writers should be caught and handled
 * properly.
 */
public class TwitterBot {
    /**
     * This is a path to the CSV file containing the tweets. The main method
     * below uses the tweets in this file when calling Twitterbot. If you want
     * to run the Twitterbot on the other files we provide, change this path to
     * a different file. (You may need to adjust the TWEET_COLUMN too.)
     */
    static final String PATH_TO_TWEETS = "files/dog_feelings_tweets.csv";
    /** Column in the PATH_TO_TWEETS CSV file to read tweets from */
    static final int TWEET_COLUMN = 2;
    /** File to store generated tweets */
    static final String PATH_TO_OUTPUT_TWEETS = "files/generated_tweets.txt";

    /** The MarkovChain you'll be using to generate tweets */
    MarkovChain mc;
    /** RandomNumber generator to pick random numbers */
    NumberGenerator ng;

    /**
     * Given a column and a buffered reader, initializes the TwitterBot by
     * training the MarkovChain with sentences sourced from the reader. Uses
     * the RandomNumberGenerator().
     *
     * @param br          - a buffered reader containing tweet data
     * @param tweetColumn - the column in the reader where the text of the tweet
     *                    itself is stored
     */
    public TwitterBot(BufferedReader br, int tweetColumn) {
        this(br, tweetColumn, new RandomNumberGenerator());
    }

    /**
     * Given a column and a buffered reader, initializes the TwitterBot by
     * training the MarkovChain with all the sentences obtained as training data
     * from the buffered reader.
     *
     * @param br          - a buffered reader containing tweet data
     * @param tweetColumn - the column in the buffered reader where the text
     *                    of the tweet itself is stored
     * @param ng          - A NumberGenerator for the ng field, also to be
     *                    passed to MarkovChain
     */
    public TwitterBot(BufferedReader br, int tweetColumn, NumberGenerator ng) {
        mc = new MarkovChain(ng);
        this.ng = ng;
        /* SOLN */
        List<List<String>> sentences = TweetParser.csvDataToTrainingData(
                br, tweetColumn
        );
        for (List<String> s : sentences) {
            mc.train(s.iterator());
        }
        /* // Complete this method. */
    }
}
