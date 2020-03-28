-module(index).
-export([data/1, site/1]).

-define(HOSTNAME, "blog.zdsmith.com").

data(_) ->
    #{
       blog => {eterm, "subsetpark.config"},
       posts => {markdown, "posts/*.md"},
       pages => {markdown, "pages/*.md"},
       about => {markdown, "front.md"}
      }.

site(Data) ->
    #{
       "site/index.html" =>
       {template, "templates/index.html", #{site_root => ""}},

       "site/feed.xml" =>
       {template, "templates/feeds.jinja",
        #{site_root => "", host => ?HOSTNAME, context => get_context(Data)}},

       "site/posts/index.html" =>
       {template, "templates/posts.html", #{site_root => "../"}},

       "site/pages/{{page.title|slugify}}.html" =>
       {template_map, "templates/page.html",
        {page, pages(Data)},
        #{site_root => "../"}},

       "site/posts/{{post.title|slugify}}.html" =>
       {template_map, "templates/post.html",
        {post, posts(Data)},
        #{site_root => "../"}},

       "site/assets/*.css" =>
       {files, "assets/*.css"},

       "site/images/*.png" =>
       {files, "images/*.png"},

       "site/images/*.gif" =>
       {files, "images/*.gif"},

       "site/static/*.html" =>
       {files, "assets/*.html"},

       "site/CNAME" => {string, ?HOSTNAME}
      }.

is_post(Post) ->
    case lists:keyfind("status", 1, Post) of
        {"status", "post"} ->
            true;
        _ -> false
    end.

datestr_to_822(DateStr) ->
    {ok, DateTime} = tempo:parse(<<"%Y-%m-%d">>, list_to_binary(DateStr), datetime),
    case DateTime of
        format_mismatch ->
            io:format("Can't convert date: ~p~n", [DateStr]),
            erlang:error(date_format_mismatch);
        _ -> ok
    end,
    {ok, Formatted} = tempo:format(rfc2822, DateTime, datetime),
    Formatted.

add_post_context(Post) ->
  {_, Date} = lists:keyfind("date", 1, Post),
  Rfc822 = datestr_to_822(Date),
  lists:keystore("rss_date", 1, Post, {"rss_date", Rfc822}).

get_context(Data) ->
    code:add_path("src/tempo/ebin"),
    Posts = posts(Data),
    PostContext = lists:map(fun add_post_context/1, Posts),
    PostContext.

posts(Data) ->
    Posts = plist:value(posts, Data),
    Posts2 = lists:filter(fun is_post/1, Posts),
    Posts2.

pages(Data) ->
    plist:value(pages, Data).
