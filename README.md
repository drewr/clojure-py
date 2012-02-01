# clojure-py

An implementation of Clojure in pure Python.

## Why Python? 

It is our belief that static virtual machines make very poor runtimes for dynamic languages. They constrain the languages to their view of what the "world should look like" and limit the options available to language implementors. We are attempting to prove this by writing an implementation of Clojure that runs on the Python VM. We believe that with a proper dynamic JIT (like pypy) a version of clojure running on a dynamic VM can outperform its JVM and CLR counterparts. 

## How can I help?

Check out the Wiki for more information about the roadmap for this project. Then check out the issues list for any items marked "isolated change". These are changes that should be somewhat easy for a newcommer to pick up and will not involve messing around with the internals of the implementation much. Also feel free to join our [mailing list](http://groups.google.com/group/clojure-py-dev)

## Running

    python clojure.py
