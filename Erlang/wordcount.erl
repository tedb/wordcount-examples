#!/usr/bin/env escript

-module(wordcount).
-author('Ted Behling <ted@tedb.us>').
-export([main/1, handle_url/2, get_body/1]).
-mode(compile).

%main([_String]) ->
main(_) ->
	% First start up Erlang's built-in HTTP client application
	application:start(inets),

	% Fetch the seed URL and parse lines into a list of URLs
	SeedUrl = "http://tedb.us/urls.txt",
	HtmlUrlString = get_body(SeedUrl),
	Urls = string:tokens(HtmlUrlString, "\n"),
	UrlCount = length(Urls),

	% Spawn a process for each URL
	% Pass our own process ID (PID) so the new process can send messages, received by gather_counts/1
	io:format("Spawning ~p processes...~n", [UrlCount]),
	_Pids = [ spawn(wordcount, handle_url, [self(), Url]) || Url <- Urls ],
	%io:format("Spawned: ~p~n", [Pids]),

	print_counts(gather_counts(UrlCount)).

handle_url(GatherPid, Url) ->
	io:format("Handling URL ~p in ~p~n", [Url, self()]),
	Text = html2text(get_body(Url)),
	Words = string:tokens(Text, "\r\n\t "),
	%io:format("Words: ~p~n", [Words]),
	Dict = count_words(Words),

	io:format("Done counting words for URL: ~p~n", [Url]),
	GatherPid ! {count, Dict}.

% Accumulate word count messages until we have them all
% Each message is {count, Dict} and contains a dict of word counts
% When done, merge the dicts together and print a report
gather_counts(UrlCount) ->
	gather_counts(UrlCount, []).
gather_counts(UrlCount, Acc) ->
	case length(Acc) >= UrlCount of
		true ->
			% we're done
			merge_counts(Acc);
		false ->
			% we're still receiving word counts
			receive {count, Count} ->
				io:format("Received ~p word counts~n", [length([Count | Acc])]),
				gather_counts(UrlCount, [Count | Acc])
			end
	end.

% Utility function to simply and naively fetch a URL body
get_body(Url) ->
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	%io:format("Fetched:~n---~n~p~n---~n", [Body]),
	Body.

% Convert an HTML string to text (note, it is debatable what this really means)
% Execute external html2text program (using PATH),
% passing HTML on stdin and reading text from stdout.
% Maybe replace with a temporary named pipe?
html2text(Html) ->
	%% SECURITY WARNING: huge shell injection vulnerability here;
	%% For demo purposes only!  Erlang makes it a pain and/or impossible to pass STDIN with EOF then read STDOUT (a la "wc")
	os:cmd(io_lib:format("echo '~s' | html2text -nobs", [bash_escape_ticks(Html)])).

% Make apostrophes safe(r) for shell command; replace ' with '"'"'
bash_escape_ticks(String) ->
	re:replace(String, "'", "'\"'\"'", [global,{return,list}]).

% Count words from list, returning dict
count_words(Words) ->
	Dict = dict:new(),
	count_words(Words, Dict).
% base case; return the dict
count_words([], Dict) ->
	Dict;
% tail-call recursive algorithm to iterate over Words and increment the counters in our dictionary
count_words([Word | Words], Dict) ->
	Dict2 = dict:update_counter(Word, 1, Dict),
	count_words(Words, Dict2).

% Merge all the wordcount dicts together
merge_counts(Dicts) ->
	%io:format("Counts (~p):~n~p~n", [length(Dicts), Dicts]),
	merge_counts(Dicts, dict:new()).

merge_counts([], Acc) ->
	Acc;
merge_counts([Dict | Dicts], Acc) ->
	Acc2 = dict:merge(fun(_, X, Y) -> X + Y end, Acc, Dict),
	merge_counts(Dicts, Acc2).

print_counts(Dict) ->
	TrimmedList = [ io_lib:format("~s: ~p~n", [Key, Value]) || {Key, Value} <- dict:to_list(Dict), Value > 1],
	io:format("~s~n", [TrimmedList]).
