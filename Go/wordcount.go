package main

import (
	"net/http"
	"strings"
	"fmt"
	"io/ioutil"
	"sort"
	"regexp"
)

func main() {
	// Fetch the seed URL and parse lines into a list of URLs
	seed_url := "http://tedb.us/urls.txt"

	html_url_string := get_body(seed_url)
	urls := strings.Split(strings.TrimSpace(html_url_string), "\n")
	url_count := len(urls)

	fmt.Println("URLs fetched:", url_count, urls)

	// make a channel on which we will receive maps of word counts
	counts_channel := make(chan map[string]int)

	// spawn a goroutine for each URL
	for _, this_url := range urls {
		go handle_url(counts_channel, this_url)
	}

	fmt.Print(strings.Join(format_counts(gather_counts(counts_channel, url_count)), "\n"))
}

func handle_url(counts_channel chan map[string]int, url string) {
	fmt.Println("handling URL", url)
	text := html2text(get_body(url))
	words := strings.Fields(text)
	//fmt.Println("Words:", words)
	count_map := count_words(words)

	fmt.Printf("Done counting words for URL: %v (%v)\n", url, len(words))
	counts_channel <- count_map
}

func get_body(url string) string {
	resp, err := http.Get(url)
	if err != nil {
		panic("Couldn't fetch body for URL" + url + ": " + err.Error())
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	//fmt.Println("Got body:", string(body))
	return string(body)
}

func html2text(html string) string {
	re1 := regexp.MustCompile("<!(.*?)(--.*?--\\s*)+(.*?)>")
	re2 := regexp.MustCompile("<script[^>]*>([\\S\\s]*?)</script>|<style[^>]*>([\\S\\s]*?)</style>")
	re3 := regexp.MustCompile("<(?:[^>'\"]*|\".*?\"|'.*?')+>")
	html = re1.ReplaceAllString(html, "")
	html = re2.ReplaceAllString(html, "")
	html = re3.ReplaceAllString(html, "")

	return html
}

func count_words(words []string) map[string]int {
	counts := make(map[string]int)
	for _, this_word := range words {
		counts[this_word]++
	}
	return counts
}

func gather_counts(counts_channel chan map[string]int, url_count int) map[string]int {
	fmt.Println("Gathering", url_count, "counts")
	counts := make(map[string]int)

	// only receive from the channel the number of URL's that we started with
	for i := 0; i < url_count; i++ {
		// merge values from new map from the channel onto existing map
		for k, v := range <-counts_channel {
			counts[k] += v
		}
	}
	return counts
}


// Convert dict to list of key/value tuples, filter to words appearing more than once, sort it descending, and print it
func format_counts(counts map[string]int) []string {
	//fmt.Println("Formatting:", counts)
	// make array of name/value pairs, removing single-word appearances
	trimmed_list := make(KeyValuePairs, 0, len(counts))
	for k, v := range counts {
		if v > 1 {
			trimmed_list = append(trimmed_list, &KeyValue{k, v})
		}
	}

	// sorts list in-place, in reverse
	sort.Sort(ByReverseCount{trimmed_list})

	formatted_list := make([]string, 0, len(trimmed_list))
	for _, v := range trimmed_list {
		formatted_list = append(formatted_list, fmt.Sprintf("%s: %d", v.Key, v.Value))
	}

	return formatted_list
}

// Create types and funcs to allow sorting by KV value
type KeyValue struct {
	Key string
	Value int
}

type KeyValuePairs []*KeyValue

func (kvs KeyValuePairs) Len() int {
	return len(kvs)
}

func (kvs KeyValuePairs) Swap(i, j int) {
	kvs[i], kvs[j] = kvs[j], kvs[i]
}

type ByReverseCount struct {KeyValuePairs}

func (kvs ByReverseCount) Less(i, j int) bool {
	return kvs.KeyValuePairs[j].Value < kvs.KeyValuePairs[i].Value
}
