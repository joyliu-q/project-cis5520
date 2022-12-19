# Class - FileLineIterator

## Description
FileLineIterator provides a useful wrapper around Java's provided BufferedReader and provides practice with implementing an Iterator. Your solution should not read the entire file into memory at once, instead reading a line whenever the next() method is called. <p> Note: Any IOExceptions thrown by readers should be caught and handled properly. Do not use the ready() method from BufferedReader.

# Method - FileLineIterator
## Description
Creates a FileLineIterator for the reader. Fill out the constructor so that a user can instantiate a FileLineIterator. Feel free to create and instantiate any variables that your implementation requires here. See recitation and lecture notes for guidance. <p> If an IOException is thrown by the BufferedReader, then hasNext should return false. <p> The only method that should be called on BufferedReader is readLine() and close(). You cannot call any other methods.

## Parameter
`reader` - A reader to be turned to an Iterator
## Throws - IllegalArgumentException
if reader is null
# Method - FileLineIterator
## Description
Creates a FileLineIterator from a provided filePath by creating a FileReader and BufferedReader for the file. <p> DO NOT MODIFY THIS METHOD.

## Parameter
`filePath` - a string representing the file
## Throws - IllegalArgumentException
if filePath is null or if the file doesn't exist
# Method - fileToReader
## Description
Takes in a filename and creates a BufferedReader. See Java's documentation for BufferedReader to learn how to construct one given a path to a file.

## Parameter
`filePath` - the path to the CSV file to be turned to a BufferedReader
## Return
a BufferedReader of the provided file contents
## Throws - IllegalArgumentException
if filePath is null or if the file doesn't exist
# Method - hasNext
## Description
Returns true if there are lines left to read in the file, and false otherwise. <p> If there are no more lines left, this method should close the BufferedReader.

## Return
a boolean indicating whether the FileLineIterator can produce another line from the file
# Method - next
## Description
Returns the next line from the file, or throws a NoSuchElementException if there are no more strings left to return (i.e. hasNext() is false). <p> This method also advances the iterator in preparation for another invocation. If an IOException is thrown during a next() call, your iterator should make note of this such that future calls of hasNext() will return false and future calls of next() will throw a NoSuchElementException

## Return
the next line in the file
## Throws - java.util.NoSuchElementException
if there is no more data in the file
