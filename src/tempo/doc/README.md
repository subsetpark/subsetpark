

#tempo [![Build Status](https://secure.travis-ci.org/selectel/tempo.png)](http://travis-ci.org/selectel/tempo)#


__Authors:__ Dmitry Groshev ([`groshev@selectel.ru`](mailto:groshev@selectel.ru)), Sergei Levedev ([`lebedev@selectel.ru`](mailto:lebedev@selectel.ru)).


`tempo` is a library for parsing and formatting dates in
Erlang. It provides a clean and nice interface to libc's
[strptime](http://linux.die.net/man/3/strptime) and
[strftime](http://linux.die.net/man/3/strftime) functions,
which are unfortunately missing from Erlang's standard library.

###<a name="Is_it_any_good?">Is it any good?</a>##



Yes.

###<a name="How_can_I_use_it?">How can I use it?</a>##


The only two functions you have to remember are [`tempo:parse/2`](tempo.md#parse-2)
and [`tempo:format/2`](tempo.md#format-2). Here are some examples:<pre>1> {ok, Bin} = tempo:format(iso8601, {now, now()}).
{ok,<<"2013-01-05T13:29:18+0400">>}
2> tempo:parse(iso8601, {datetime, Bin}).
{ok,{{2013,1,5},{13,29,18}}}</pre>

As you might have noticed, both of the functions follow a common
pattern -- *Format* first, then a *Value*, tagged by its actual
or expected type. Predefined formats include: `iso8601`, `rfc1123`,
and `rfc2822`, but in fact, you can use any format, as long as it
follows libc conventions:<pre>(tempo_dev@localhost)1> {ok, Bin} = tempo:format(<<"%A, %Y-%d-%m">>, {now, now()}).
{ok,<<"Thursday, 2012-07-06">>}</pre>

###<a name="Limitations">Limitations</a>##


Unfortunately, dealing with time on various platforms is messy, so limitations are
unavoidable. Here's a shortlist of those we know of:

* Parsing years before `1900` causes a `{error, format_mismatch}` on OS X.


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="tempo.md" class="module">tempo</a></td></tr></table>
