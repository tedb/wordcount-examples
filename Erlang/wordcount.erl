#!/usr/bin/env escript
% To compile with erlc, comment out line above using a % 

-module(wordcount).
-author('Ted Behling <ted@tedb.us>').
-export([start/0, main/1, handle_url/2, get_body/1, html2text/1]).
-mode(compile).

%main([_String]) ->
% only needed by erlscript
main(_) ->
	start().

start() ->
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

	io:format("~s", [format_counts(gather_counts(UrlCount))]).

handle_url(GatherPid, Url) ->
	io:format("Handling URL ~p in ~p~n", [Url, self()]),
	Text = html2text(get_body(Url)),
	Words = string:tokens(Text, "\r\n\t "),
	%io:format("Words: ~p~n", [Words]),
	Dict = count_words(Words),

	io:format("Done counting words for URL: ~p (~p)~n", [Url, length(Words)]),
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
	%os:cmd(io_lib:format("echo '~s' | html2text -nobs", [bash_escape_ticks(Html)])).

	% Regexes lifted from  http://www.cpan.org/authors/Tom_Christiansen/scripts/striphtml.gz 
	% first we'll shoot all the <!-- comments -->
	Html2 = re:replace(Html, "
	 <!                   # comments begin with a `<!'
                        # followed by 0 or more comments;

    (.*?)               # this is actually to eat up comments in non 
                        # random places

     (                  # not suppose to have any white space here

                        # just a quick start; 
      --                # each comment starts with a `--'
        .*?             # and includes all text up to and including
      --                # the *next* occurrence of `--'
        \\s*             # and may have trailing while space
                        #   (albeit not leading white space XXX)
     )+                 # repetire ad libitum  XXX should be * not +
    (.*?)               # trailing non comment text
   >                    # up to a `>'
	", " ", [global, extended, dotall]), % equivalent of Perl /gsx

	% remove <script></script> and <style></style> blocks
	Html3 = re:replace(Html2, "<script[^>]*>([\\S\\s]*?)</script>|<style[^>]*>([\\S\\s]*?)</style>", " ", [global, extended, dotall]),

	% next we'll remove all the <tags>
	Html4 = re:replace(Html3, "
	<                    # opening angle bracket

    (?:                 # Non-backreffing grouping paren
         [^>'\"] *       # 0 or more things that are neither > nor ' nor double-quote
            |           #    or else
         \".*?\"          # a section between double quotes (stingy match)
            |           #    or else
         '.*?'          # a section between single quotes (stingy match)
    ) +                 # repetire ad libitum
                        #  hm.... are null tags <> legal? XXX
    >                    # closing angle bracket
	", " ", [global, extended, dotall]), % equivalent of Perl /gsx

	binary_to_list(iolist_to_binary(Html4)).

% Make apostrophes safe(r) for shell command; replace ' with '"'"'
%bash_escape_ticks(String) ->
%	re:replace(String, "'", "'\"'\"'", [global,{return,list}]).

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
	%io:format("Done merging~n"),
	Acc;
merge_counts([Dict | Dicts], Acc) ->
	Acc2 = dict:merge(fun(_, X, Y) -> X + Y end, Acc, Dict),
	%io:format("Merged: ~p~n", [Acc2]),
	merge_counts(Dicts, Acc2).

% Convert dict to list of key/value tuples, filter to words appearing more than once, sort it descending, and print it
format_counts(Dict) ->
	%io:format("formatting: ~p~n", [Dict]),
	TrimmedList = [ {Key, Value} || {Key, Value} <- dict:to_list(Dict), Value > 1],
	SortedList = lists:reverse(lists:sort(fun({_K1, V1}, {_K2, V2}) -> V1 < V2 end, TrimmedList)),
	FormattedList = [ io_lib:format("~s: ~p~n", [Key, Value]) || {Key, Value} <- SortedList],
	FormattedList.
