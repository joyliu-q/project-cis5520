package subdir;

import java.util.*;
import Twitterbot.subdir.ProbabilityDistribution;
import Twitterbot.subdir.RandomNumberGenerator;

/**
 * A Markov Chain is a data structure that tracks the frequency with which one
 * value follows another value in sequence. This project uses a MarkovChain to
 * model tweets by gathering the frequency information from a Twitter feed. We
 * can use the MarkovChain to generate "plausible" tweets by conducting a random
 * walk through the chain according to the frequencies. Please see the homework
 * instructions for more information on Markov Chains.
 */
public class MarkovChain {
    /**
     * No need to write any constructors. They are provided for you.
     */
    public MarkovChain() {
        this(new RandomNumberGenerator());
    }

    /**
     * No need to write any constructors. They are provided for you.
     *
     * @param ng - A (non-null) NumberGenerator used to walk through the
     *           MarkovChain
     */
    public MarkovChain(NumberGenerator ng) {
        if (ng == null) {
            throw new IllegalArgumentException(
                    "NumberGenerator input cannot be null");
        }
        this.chain = new TreeMap<>();
        this.ng = ng;
        this.startWords = new ProbabilityDistribution<>();
        reset();
    }

    /**
     * Adds a bigram to the Markov Chain dictionary. Note that the dictionary is
     * a field called chain of type {@code Map<String,
     * ProbabilityDistribution<String>> }.
     *
     * @param first  - The first word of the Bigram (should not be null)
     * @param second - The second word of the Bigram (should not be null)
     * @throws IllegalArgumentException - when either parameter is null
     */
    void addBigram(String first, String second) {
        // Complete this method.
    }

    /**
     * Adds a sentence's training data to the MarkovChain frequency
     * information.
     * <p>
     * This method is meant to be called multiple times. Each call to this
     * method should provide this method with an Iterator that represents one
     * sentence. If we were to train a Markov Chain with four unique sentences,
     * we would convert each sentence into an iterator and call train() four
     * times, once on each of the iterators. Remember to update startWords
     * accordingly.
     * <p>
     * Look at the homework instructions for more details on bigrams. You should
     * use addBigram() in this method.
     * <p>
     * Once you reach the last word of a sentence, add a bigram of that word and
     * the string {@code "<END>"} (we provide it as the variable END_TOKEN). This
     * functions similarly to None in OCaml, letting us notate an absence of
     * data without various issues that come up if we use null.
     * Using this string will teach the Markov Chain how to end a sentence.
     * <p>
     * Do nothing if the sentence is empty.
     *
     * @param sentence - an iterator representing one sentence of training data
     * @throws IllegalArgumentException - when the sentence Iterator is null
     */
    public void train(Iterator<String> sentence) {
        // Complete this method.
    }

    /**
     * Returns the ProbabilityDistribution for a given token. Returns null if
     * none exists.
     *
     * @param token - the token for which the ProbabilityDistribution is sought
     * @throws IllegalArgumentException - when parameter is null.
     * @return a ProbabilityDistribution or null
     */
    ProbabilityDistribution<String> get(String token) {
        if (token == null) {
            throw new IllegalArgumentException("token cannot be null.");
        }
        return chain.get(token);
    }

    /**
     * Given a starting String, sets up the Iterator functionality such that:
     * (1) the Markov Chain will begin a walk at start. (2) the first call to
     * next() made after calling reset(start) will return start.
     * <p>
     * If start is {@code "<END>"}, then hasNext() should return false.
     *
     * start need not actually be a part of the chain (but it should have no
     * successor). In the edge case that you've reset a word that isn't
     * present in the training data, the first call to next() will return the word,
     * and a second call to next() will throw a NoSuchElementException.
     * Likewise, the first call to hasNext() will return true, and a call to
     * hasNext() after a call to next() will return false.
     *
     * @param start - the element that will be the first word in a walk on the
     *              Markov Chain.
     * @throws IllegalArgumentException - when parameter is null.
     */
    public void reset(String start) {
        if (start == null) {
            throw new IllegalArgumentException("start cannot be null");
        }
        // Complete this method.
    }

    /**
     * DO NOT EDIT THIS METHOD. WE COMPLETED IT FOR YOU.
     * <p>
     * Sets up the Iterator functionality with a random start word such that the
     * MarkovChain will now move along a walk beginning with that start word.
     * <p>
     * The first call to next() after calling reset() will return the random
     * start word selected by this call to reset().
     */
    public void reset() {
        if (startWords.getTotal() == 0) {
            reset(END_TOKEN);
        } else {
            reset(startWords.pick(ng));
        }
    }

    /**
     * This method should check if there is another word to retrieve from the
     * Markov Chain based on the current word of our walk. Remember that the
     * end of a sentence is denoted by the token {@code "<END>"}.
     * <p>
     * Your solution should be very short.
     *
     * @return true if {@link #next()} will return a non-trivial String
     *         (i.e. it is a meaningful part of the sentence - see {@link #train})
     *         and false otherwise
     */
    public boolean hasNext() {
        return false; // Complete this method.
    }

    /**
     * If reset() or reset(String start) was called immediately before this,
     * this method should return the string that was set up by that reset call.
     * Otherwise, this method should return a successor picked from the
     * probability distribution associated with the word returned by the
     * previous call to next().
     *
     * @return the next word in the MarkovChain (chosen at random via the number
     *         generator if it is a successor)
     * @throws NoSuchElementException if there are no more words on the walk
     *                                through the chain.
     */
    public String next() {
        return null; // Complete this method.
    }

    /**
     * Modifies all ProbabilityDistributions to output words in the order
     * specified.
     *
     * @param words - an ordered list of words that the distributions should
     *              generate
     * @throws IllegalArgumentException - when parameter is null or empty
     *
     */
    public void fixDistribution(List<String> words) {
        if (words == null || words.isEmpty()) {
            throw new IllegalArgumentException("Invalid word list for fixDistribution");
        }
        fixDistribution(words, words.size() == 1);
    }

    /**
     * Modifies all ProbabilityDistributions to output words in the order
     * specified.
     *
     * @param words     - an ordered list of words that the distribution should
     *                  generate
     * @param pickFirst - whether to pick the first word in {@code words}
     *                  from {@code startWords}
     * @throws IllegalArgumentException - when parameter is null or empty or when
     *                                  first word in the list is not in startWords
     */
    public void fixDistribution(List<String> words, boolean pickFirst) {
        if (words == null || words.isEmpty()) {
            throw new IllegalArgumentException("Invalid word list for fixDistribution");
        }

        String curWord = words.remove(0);
        if (startWords.count(curWord) < 1) {
            throw new IllegalArgumentException(
                    "first word " + curWord + " " +
                            "not present in " + "startWords");
        }

        List<Integer> probabilityNumbers = new LinkedList<>();
        if (pickFirst) {
            probabilityNumbers.add(startWords.index(curWord));
        }

        while (words.size() > 0) {
            ProbabilityDistribution<String> curDistribution;
            // if we were just at null, reset. otherwise, continue on the chain
            if (curWord == null) {
                curDistribution = startWords;
            } else {
                curDistribution = chain.get(curWord);
            }

            String nextWord = words.remove(0);
            if (nextWord != null) {
                if (curDistribution.count(nextWord) < 1) {
                    throw new IllegalArgumentException(
                            "word " + nextWord +
                                    " not found as a child of" + " word " + curWord);
                }
                probabilityNumbers.add(curDistribution.index(nextWord));
            } else {
                probabilityNumbers.add(0);
            }
            curWord = nextWord;
        }

        ng = new ListNumberGenerator(probabilityNumbers);
    }

    /**
     * Use this method to print out markov chains with words and probability
     * distributions.
     */
    public String toString() {
        StringBuilder res = new StringBuilder();
        for (Map.Entry<String, ProbabilityDistribution<String>> c : chain.entrySet()) {
            res.append(c.getKey());
            res.append(": ");
            res.append(c.getValue().toString());
            res.append("\n");
        }
        return res.toString();
    }
}