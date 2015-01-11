CL-JSYNC
========

JSON can't encode graphs with cycles. Ain't nobody on earth can correctly implement a YAML parser. The solution? [JSYNC] [1].

I wrote this for a one-off project before I discovered [paren-psos] [2], but CL-JSYNC still turned out to be faster for serializing large cycle-heavy object graphs.

[1]:http://jsync.org/
[2]:https://github.com/gonzojive/paren-psos