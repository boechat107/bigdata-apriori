# bigdata-apriori

An efficient implementation of the Apriori algorithm for a huge amount of input
data. The main motivation is the lack of a rigid input file formatting and the 
use of the minimum amount of memory.

## Usage

Under development...

Example:

```clojure
bigdata-apriori.core> (apriori 0.5 [[1 2 3] [1] [2 3] [2 1] [2] [3 2]])
{#{2 3} 3}
```

## License

Distributed under the Eclipse Public License, the same as Clojure.
