# Class - FileLineIterator

## Description
FileLineIterator provides a useful wrapper around Java's providedBufferedReader and provides practice with implementing an Iterator. Yoursolution should not read the entire file into memory at once, instead readinga line whenever the next() method is called.<p>Note: Any IOExceptions thrown by readers should be caught and handledproperly. Do not use the ready() method from BufferedReader.

# Method - FileLineIterator
## Description
Creates a FileLineIterator for the reader. Fill out the constructor sothat a user can instantiate a FileLineIterator. Feel free to create andinstantiate any variables that your implementation requires here. Seerecitation and lecture notes for guidance.<p>If an IOException is thrown by the BufferedReader, then hasNext shouldreturn false.<p>The only method that should be called on BufferedReader is readLine() andclose(). You cannot call any other methods.

## Parameter
`reader` - A reader to be turned to an Iterator@throws IllegalArgumentException if reader is null
# Method - FileLineIterator
## Description
Creates a FileLineIterator from a provided filePath by creating aFileReader and BufferedReader for the file.<p>DO NOT MODIFY THIS METHOD.

## Parameter
`filePath` - a string representing the file@throws IllegalArgumentException if filePath is null or if the filedoesn't exist
# Method - fileToReader
## Description
Takes in a filename and creates a BufferedReader.See Java's documentation for BufferedReader to learn how to construct onegiven a path to a file.

## Parameter
`filePath` - the path to the CSV file to be turned to aBufferedReader@return a BufferedReader of the provided file contents@throws IllegalArgumentException if filePath is null or if the filedoesn't exist
# Method - hasNext
## Description
Returns true if there are lines left to read in the file, and falseotherwise.<p>If there are no more lines left, this method should close theBufferedReader.

## Return
a boolean indicating whether the FileLineIterator can produceanother line from the file
# Method - next
## Description
Returns the next line from the file, or throws a NoSuchElementExceptionif there are no more strings left to return (i.e. hasNext() is false).<p>This method also advances the iterator in preparation for anotherinvocation. If an IOException is thrown during a next() call, youriterator should make note of this such that future calls of hasNext()will return false and future calls of next() will throw aNoSuchElementException

## Return
the next line in the file@throws java.util.NoSuchElementException if there is no more data in thefile
