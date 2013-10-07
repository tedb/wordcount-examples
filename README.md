Concurrent word count demos
===

These are quick language demos for concurrent word counts in Erlang and Go, for the CHUUG meeting of October 2013.  The intent is to download a list of URL's (text file, 1 per line), then fetch & process those URL's in parallel (i.e. concurrently) to strip HTML and yield a composite word frequency count.

Note, at the moment both versions have a hard-coded seed URL of http://tedb.us/urls.txt .  This is an early version and some work on the HTML-stripping regexes might be required.  It seems to basically work for demo purposes but there are probably bugs.

Erlang
---

For Erlang, concurrency is achieved by handling each URL in a process and results are communicated by Erlang messages.

To run:
	
	cd Erlang
	escript wordcount.erl

or, to run compiled, first comment out the escript line at the top of wordcount.erl with a %, then:

	cd Erlang
	erlc wordcount.erl
	erl -noshell -s wordcount -s init stop

Go
---

Similarly, for Go, each URL is handled in a goroutine, and reuslts are communicated using a Go channel.

To run:
	
	cd Go
	go run wordcount.go

or, to run compiled:

	cd Go
	go build wordcount.go
	./wordcount

License
---

The MIT License (MIT)

Copyright (c) 2013 Ted Behling <ted@tedb.us>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
