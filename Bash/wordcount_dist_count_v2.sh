#!/bin/bash

if [[ $BASH_VERSION < 4 ]]; then echo "Must use bash version >= 4"; exit 1; fi

#declare -A animals
#animals=( ["moo"]="cow" )
#echo "${animals["moo"]}"
#for sound in "${!animals[@]}"; do echo "$sound - ${animals["$sound"]}"; done

# Download the URL, then for each line, split each word on whitespace (bash default behavior),
# storing each word in an associative array.  Then, print the word counts
handle_url() {
    URL=$1

    curl -s $URL | while read -a word
    do
        if [[ "x$word" == "x" ]]; then continue; fi
        echo "$word"
    done | sort | uniq -c

}

# Export the function so parallel can use it
export -f handle_url

# read "count word" pairs, one per line, and return summary
gather_counts() {
    declare -A counts

    while read count word
    do
        echo $word
        counts["$word"]=$((${counts["$word"]} + $count))
    done

    declare -p counts
}

# Download the list of URLs, passing it to parallel on stdin; parallel then lists all the words, which we sort alphabetically, count, and print the top 100
curl -s http://tedb.us/urls.txt | parallel --gnu -j 100 handle_url | gather_counts # | sort | uniq -c | sort -rn | head -100

    # declare associative array
#    declare -A counts


#    declare -p counts

    # for word in "${!counts[@]}";
    # do 
    #     echo F "$word ${counts["$word"]}"
    # done
