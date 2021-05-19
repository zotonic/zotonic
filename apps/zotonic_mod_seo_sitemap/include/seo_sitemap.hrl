% Notifications for sitemap generation.
% See for additional documentation https://www.sitemaps.org/protocol.html

% Number of URLs per urlset, used for paging the sitemap source queries.
-define(URLSET_SIZE, 5000).

%% @doc Fetch the sitemaps to be included
%% Type: map
%% Return: list of maps with keys 'count' (for the number of resources), 'source' (for the sitemap_urlset notification)
%% and optionally 'size' (number of entries per partition) and 'lastmod' (last modification date of this set)
-record(seo_sitemap_index, {
    }).

%% @doc Fetch the entries for a sitemap file and resource range. Note that it is
%% allowed to have more or less than one entry per resource. The 'from' counts from 1, not 0.
%% It is advisable to have the entries with the highest priority or changefreq at the start
%% of the sitemap.
%% Type: list( map() )
%% Return: list of maps with key 'loc', and optional keys 'lastmod', 'priority', and 'changefreq'.
-record(seo_sitemap_urlset, {
        source :: binary(),
        offset :: pos_integer(),
        limit :: pos_integer()
    }).

