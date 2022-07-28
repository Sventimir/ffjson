# ffjson

Ffjson is a multi-stream JSON processing tool, similar in spirit to
jq, but intended to grow even more powerful. Aside from being a useful
tool, this program also explores how [tagless-final approach](https://www.researchgate.net/publication/266718917_Typed_Tagless_Final_Interpreters)
can be used to build extensible programming language interpreters.
In this particular case the core JSON syntax is used directly to parse
and serialise JSON streams, and simultaneously is also extended to
build a language for transforming JSON streams.

## Compilation

Make sure the Haskell stack is installed; then just run:

```
$ stack build
```

## Basic usage

Just calling `ffjson` with no arguments will read the JSON data from
the standard input and write the same JSON nicely formatted to standard
output. The `-i` option allows to specify other input source (in fact
more than one is possible). It should be followed by either a filename
or a URL. The program will automatically recognise the type of input
stream and interpret it accordingly:

```
$ ffjson -i https://example.com/api
```

Multiple input streams can be given like so:

```
$ ffjson -i https://example.com/api -i /home/user/data.json
```

Each input stream can be given a name by following it with `-n` option:

```
$ ffjson -i /home/user/data.json -n data
```

Unnamed input streams are assigned subsequent natural numbers (starting from
0) as identifiers. In the absence of any `-i` options, standard input is
assumed to be the sole input stream indexed with `0`.

Output streams are added similarly to inputs using `-o` option. It should be
followed by the name (or index) of a stream to output. Optionally the name
can immediately be followed by colon (`:`) and a filename (default is standard
output):

```
$ ffjson -i https://example.com/api -n data -o data:/home/user/data.json
```

Between input and output any stream can be transformed by a
**filter**, described by a dedicated language which will be explained
later. Just for a quick example the following filter will assume the
input to be a dictionary and will extract its property named "data":

```
$ ffjson -i https://example.com/api -n json \
         -f [json].data[processed] \
         -o processed:/home/user/data.json
```

Note that the filter does not alter the stream "json", but rather creates
a new stream named "processed", which is then output to a file. However,
names of input and output stream in filter definition can be omitted, in
which case the input is stream `0` and the output stream shadows (i.e. has
the same name as) the input stream.

Also note that both `-i` and `-o` options can be omitted if they're equal to
standard input and standard output respectively.

## Additional opions

For the moment the following formatting options are available:

* `--raw` (or `-r` for short) will output JSON without any indentation.
* `--indent` followed by a number sets the indentation to the given number
             of spaces.
* `--ratios` displays fractions as ratios (e.g. `1 / 3`) rather than as
             decimals.

## Filters

The heart of the program is its filter language, which allows to encode
virtually any computation on the given JSON input. Its syntax is an extension
of the core JSON syntax, which means that any valid JSON is simultaneously
a valid filter expression, which transforms any input into that constant
JSON. For instance:

```
$ echo '{}' | ffjson -r -f '{"a" : "123}'
{"a": 123}
```

If the input JSON is an object, its keys can be extracted as follows:
```
$ echo '{"a": 1, "b": 2}' | ffjson -r -f .a
1
```

that is using a dot (`.`) followed by the key. If the key contains characters
other than letter and numbers, it should be quoted in order to be parsed
properly:

```
$ echo '{"a-b": []}' | ffjson -r -f '."a-b"'
[]
```

Similarly, items can be extracted from arrays by their index (which starts
at 0).

```
$ echo '[1, 2, 3]' | ffjson -r -f '.[1]'
2
```

As one might expect, expressions can be nested inside JSON, for instance:

```
$ echo '[1, 2, 3]' | ffjson -r -f '{"a": .[0], "b": .[1], "c": .[2]}'
{"a":1,"b":2,"c":3}
```

These two kinds of filters are collectively called **getters** and they can
be composed together by concatenation (no spaces between them are allowed
though):

```
$ echo '{"a": [{"a": 1, "b": 2}, {"c": 0}]}' | ffjson -r -f '.a.[0].b'
2
```

Note that both getters will return `null` if the required value is not
present in the input and will fail if they encounter an unexpected data
type (an object for object getters and an array for array getters).

In addition to getters, the language offers operators. For the moment
all basic arithmetic operations are allowed (NOTE: they only work on
numbers). Additionally, a special composition operator allows us to
compose filters together:

```
$ echo '{"a": {"b": 15, "c": 25}}' | ffjson -r -f '.a | (.b / .c)'
0.6
```

NOTE: There is no operator precedence currently implemented, so the
parentheses are necessary to compose `.a` filter with the whole `.b / .c`
expression rather than with `.b` only.

The same program with `--ratios` option enabled will yield:

```
$ echo '{"a": {"b": 15, "c": 25}}' | ffjson -r -f '.a | (.b / .c)' --ratios
3 / 5
```

Finally the language contains **functions**, which are higher-order filters
(like operators). Each function accepts a number of filters as arguments.
Arguments must follow the function name, separated by spaces. No parentheses
are required to call a function, unless a complex expression is being given as
an argument. Currying is not implemented yet.

```
$ echo '{"a": 1, "b": 2, "c": 3}' | ffjson -r -f 'keys id'
["a","b","c"]
```

The filter `id` returns input JSON without any change. It's needed solely to
satisfy `keys` function's requirement of 1 argument. `keys` function, as one
might expect, returns an array of keys of the input object.

`map` function is particularly handy if one needs to process whole arrays.
For instance:

```
$ echo '[{"a": 1, "b": 2}, {"a": 3, "b": 4}]' | ffjson -r -f 'map (.a + .b)'
[3,7]
```

For more examples, consult test cases in `tests/Evaulator.hs`. A full list of
available functions and operators will be supplied later. For the moment they
can be found in the source code, in `src/Parser/Language.hs`.
