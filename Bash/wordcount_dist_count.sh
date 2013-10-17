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

    # declare associative array
    declare -A counts

    curl -s $URL | while read -a word
    do
        if [[ "x$word" == "x" ]]; then continue; fi

        # escape shell metacharacter quotes and dollar signs from input; this is probably a bad way to do this
        #line=$(echo $line | sed -e 's/"/\\"/g;s/\$/\\$/g')
        #for word in $line
        #do
            #word=$(echo -E $word | )
            echo word "$word"
            counts["$word"]=$((${counts["$word"]} + 1))
            echo E $word: ${counts["$word"]}
        #done
    done

    declare -p counts
    # for word in "${!counts[@]}";
    # do 
    #     echo F "$word ${counts["$word"]}"
    # done
}

# Export the function so parallel can use it
export -f handle_url

# Download the list of URLs, passing it to parallel on stdin; parallel then lists all the words, which we sort alphabetically, count, and print the top 100
curl -s http://tedb.us/urls.txt | parallel --gnu -j 100 handle_url # | sort | uniq -c | sort -rn | head -100
