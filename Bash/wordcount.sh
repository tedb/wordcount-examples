#!/bin/bash

# Download the URL, then for each line, split each word on whitespace (bash default behavior),
# outputting each word on a line
handle_url() {
	URL=$1

	curl -s $URL | while read line
	do
		for word in $line
		do
			echo $word
		done
	done
}

# Export the function so parallel can use it
export -f handle_url

# Download the list of URLs, passing it to parallel on stdin; parallel then lists all the words, which we sort alphabetically, count, and print the top 100
curl -s http://tedb.us/urls.txt | parallel --gnu handle_url | sort | uniq -c | sort -rn | head -100
