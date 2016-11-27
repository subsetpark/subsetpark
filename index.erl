-module(index).
-export([data/1, site/1]).

data(_) ->
    #{
       blog => {eterm, "subsetpark.config"},
       posts => {markdown, "posts/*.md"},
       pages => {markdown, "pages/*.md"},
       about => {markdown, "about.md"}
    }.

site([Data]) ->
    #{
       "site/index.html" =>
       {template, "templates/index.html", #{site_root => ""}},

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

       "site/static/*.html" =>
       {files, "assets/*.html"}
    }.

is_post(Post) ->
    case lists:keyfind("status", 1, Post) of
        {"status", "post"} ->
            true;
        _ -> false
    end.

posts(Data) ->
    Posts = plist:value(posts, Data),
    Posts2 = lists:filter(fun is_post/1, Posts),
    Posts2.

pages(Data) ->
    plist:value(pages, Data).
