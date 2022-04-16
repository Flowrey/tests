#include <stdio.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#include <cjson/cJSON.h>

#include <curl/curl.h>

struct MemoryStruct {
	char *memory;
	size_t size;
};

static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
	size_t realsize = size * nmemb;
	struct MemoryStruct *mem = (struct MemoryStruct *)userp;

	char *ptr = realloc(mem->memory, mem->size + realsize + 1);
	if(!ptr) {
	  printf("not enough memory (realloc returned NULL)\n");
	  return 0;
	}

	mem->memory = ptr;
	memcpy(&(mem->memory[mem->size]), contents, realsize);
	mem->size += realsize;
	mem->memory[mem->size] = 0;

	return realsize;
}

char * get_yt_initial_data(char* title_str, regex_t *regex) {
	// CURL INIT
	CURL *curl;
	CURLcode res;

	struct MemoryStruct chunk;
	
	chunk.memory = malloc(1);  /* will be grown as needed by the realloc above */
	chunk.size = 0;    /* no data at this point */

	curl_global_init(CURL_GLOBAL_ALL);

	curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.42.0");
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

	// regex INIT
	int reti;
	int match;
	size_t nmatch = 0;
        regmatch_t *pmatch = NULL;
	nmatch = regex->re_nsub;
	pmatch = malloc (sizeof (*pmatch) * nmatch);

	// MAIN
	char *arg = curl_easy_escape(curl, title_str, 0);
	char *base_url = "https://www.youtube.com/results?search_query=";
	int url_size = sizeof(char) * ( strlen(arg) + strlen(base_url) + 1 );
	char *url = malloc(url_size);
	snprintf(url, url_size, "%s%s", base_url, arg);

	//printf("DEBUG: %s\n", url);
	curl_easy_setopt(curl, CURLOPT_URL, url);
	res = curl_easy_perform(curl);
	reti = regexec(regex, chunk.memory, nmatch, pmatch, 0);
	if (!reti) {
		//printf("DEBUG: submatch offsets are %d %d\n", pmatch[2].rm_so, pmatch[2].rm_eo);
		if(pmatch[2].rm_so != -1) {
			int data_size = sizeof(char) * ( pmatch[2].rm_eo - pmatch[2].rm_so + 1 );
			char *yt_initial_data = malloc(data_size);
			snprintf(yt_initial_data, data_size, "%.*s", pmatch[2].rm_eo, &chunk.memory[pmatch[2].rm_so]);
			free(chunk.memory);
			curl_easy_cleanup(curl);
			return yt_initial_data;
		}
	}
	else if (reti == REG_NOMATCH) {
	    puts("Can't get yt_initial_data");
	}
	else {
	    regerror(reti, regex, chunk.memory, chunk.size);
	    fprintf(stderr, "Regex match failed:");
	    exit(1);
	}
	free(chunk.memory);
	curl_easy_cleanup(curl);
}

int main(int argc, char** argv) {
	// Regex
	regex_t regex;
	if ( regcomp(&regex, "(var ytInitialData = )(.*)(;</script><script)", REG_EXTENDED) ) {
	    fprintf(stderr, "Could not compile regex\n");
	    exit(1);
	}

	// CURL INIT
	CURL *curl;
	CURLcode res;

	struct MemoryStruct chunk;
	
	chunk.memory = malloc(1);
	chunk.size = 0;

	curl_global_init(CURL_GLOBAL_ALL);

	curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.42.0");
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

	// cJON INIT
        const cJSON *media = NULL;
        const cJSON *track = NULL;
        const cJSON *sectionRenderer = NULL;

	// MAIN
	curl_easy_setopt(curl, CURLOPT_URL, "https://musicbrainz.org/ws/2/release/6c1adf00-edaf-4fe0-9d57-8dc5da90a4a9?inc=artists+recordings&fmt=json");
	res = curl_easy_perform(curl);

	cJSON *json = cJSON_Parse(chunk.memory);
	cJSON *medias = cJSON_GetObjectItemCaseSensitive(json, "media");
	cJSON_ArrayForEach(media, medias) {
		cJSON *tracks = cJSON_GetObjectItemCaseSensitive(media, "tracks");
		cJSON_ArrayForEach(track, tracks) {
			cJSON *title = cJSON_GetObjectItemCaseSensitive(track, "title");
			char *title_str = title->valuestring;
			//printf("DEBUG: %s\n", title_str);

			// Youtube Section
			char* yt_initial_data = get_yt_initial_data(title_str, &regex);
			cJSON *j_yt_initial_data = cJSON_Parse(yt_initial_data);
			if (j_yt_initial_data == NULL) {
			       const char *error_ptr = cJSON_GetErrorPtr();
			       if (error_ptr != NULL) {
			       	fprintf(stderr, "Error before");
			       }
			       exit(1);
			}
			cJSON *contents = cJSON_GetObjectItemCaseSensitive(j_yt_initial_data, "contents");
			cJSON *twoColumnSearchResultsRenderer = cJSON_GetObjectItemCaseSensitive(contents, "twoColumnSearchResultsRenderer");
			cJSON *primaryContents = cJSON_GetObjectItemCaseSensitive(twoColumnSearchResultsRenderer, "primaryContents");
			cJSON *sectionListRenderer = cJSON_GetObjectItemCaseSensitive(primaryContents, "sectionListRenderer");
			contents = cJSON_GetObjectItemCaseSensitive(sectionListRenderer, "contents");

			contents = cJSON_GetArrayItem(contents, 0);
			cJSON *itemSectionRenderer = cJSON_GetObjectItemCaseSensitive(contents, "itemSectionRenderer");
			contents = cJSON_GetObjectItemCaseSensitive(itemSectionRenderer, "contents");
			cJSON_ArrayForEach(sectionRenderer, contents) {
				cJSON *videoRenderer = cJSON_GetObjectItemCaseSensitive(sectionRenderer, "videoRenderer");
				if (cJSON_IsObject(videoRenderer)) {
					// Video Id
					cJSON *videoId = cJSON_GetObjectItemCaseSensitive(videoRenderer, "videoId");
					char *videoId_str =  videoId->valuestring;

					// Video Title
					cJSON *title = cJSON_GetObjectItemCaseSensitive(videoRenderer, "title");
					cJSON *runs = cJSON_GetObjectItemCaseSensitive(title, "runs");
					contents = cJSON_GetArrayItem(runs, 0);
					cJSON *text = cJSON_GetObjectItemCaseSensitive(contents, "text");
					char *videoTitle = text->valuestring;
					printf("Title: %s\n", videoTitle);
					printf("Id: %s\n", videoId_str);
				}
			}
		}
	}
		
	free(chunk.memory);
	curl_easy_cleanup(curl);
  	curl_global_cleanup();

	return 0;
}
