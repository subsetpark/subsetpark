-module(index).

-export([data/1, related_notes/1, site/1,
         truncatechars/2]).

-define(HOSTNAME, "blog.zdsmith.com").

%
% API
%

data(_) ->
    #{blog => {eterm, "subsetpark.config"},
      posts => {markdown, "posts/*.md"},
      pages => {markdown, "pages/*.md"},
      notes => {markdown, "notes/*.md"},
      about => {markdown, "front.md"}}.

site(Data) ->
    Notes = notes(Data),
    NotesWithContents = lists:zip(Notes,
                                  file_contents(Notes)),
    #{"site/index.html" =>
          {template, "templates/index.html", #{site_root => ""}},
      "site/feed.xml" =>
          {template, "templates/feeds.jinja",
           #{site_root => "", host => ?HOSTNAME,
             context => get_context(Data)}},
      "site/posts/index.html" =>
          {template, "templates/posts.html",
           #{site_root => "../"}},
      "site/notes/index.html" =>
          {template, "templates/notes.html",
           #{site_root => "../"}},
      "site/pages/{{page.title|slugify}}.html" =>
          {template_map, "templates/page.html",
           {page, pages(Data)}, #{site_root => "../"}},
      "site/posts/{{post.title|slugify}}.html" =>
          {template_map, "templates/post.html",
           {post, posts(Data)}, #{site_root => "../"}},
      "site/notes/{{note.topic|slugify}}.html" =>
          {template_map, "templates/note.html", {note, Notes},
           #{site_root => "../", all_notes => NotesWithContents}},
      "site/assets/*.css" => {files, "assets/*.css"},
      "site/images/*.png" => {files, "images/*.png"},
      "site/images/current/*.png" =>
          {files, "images/current/*.png"},
      "site/images/*.gif" => {files, "images/*.gif"},
      "site/static/*.html" => {files, "assets/*.html"},
      "site/CNAME" => {string, ?HOSTNAME}}.

%
% Private
%

is_post(Post) ->
    case lists:keyfind("status", 1, Post) of
      {"status", "post"} -> true;
      _ -> false
    end.

datestr_to_822(DateStr) ->
    {ok, DateTime} = tempo:parse(<<"%Y-%m-%d">>,
                                 list_to_binary(DateStr), datetime),
    case DateTime of
      format_mismatch ->
          io:format("Can't convert date: ~p~n", [DateStr]),
          erlang:error(date_format_mismatch);
      _ -> ok
    end,
    {ok, Formatted} = tempo:format(rfc2822, DateTime,
                                   datetime),
    Formatted.

add_post_context(Post) ->
    {_, Date} = lists:keyfind("date", 1, Post),
    Rfc822 = datestr_to_822(Date),
    lists:keystore("rss_date", 1, Post,
                   {"rss_date", Rfc822}).

get_context(Data) ->
    code:add_path("src/tempo/ebin"),
    Posts = posts(Data),
    PostContext = [add_post_context(V1) || V1 <- Posts],
    PostContext.

posts(Data) ->
    Posts = plist:value(posts, Data),
    Posts2 = [V1 || V1 <- Posts, is_post(V1)],
    Posts2.

pages(Data) -> plist:value(pages, Data).

notes(Data) -> plist:value(notes, Data).

file_contents(Files) ->
    FileNames = [proplists:get_value('__file__', F)
                 || F <- Files],
    [file_contents_1(V1) || V1 <- FileNames].

file_contents_1(FileName) ->
    {ok, Contents} = file:read_file(FileName), Contents.

note_topic(NoteMeta) ->
    proplists:get_value("topic", NoteMeta).

%
% Custom Filters
%

related_notes([Topic, AllNotes]) ->
    CP = binary:compile_pattern(list_to_binary(Topic)),
    Related = [V1
               || V1 <- AllNotes, is_related(V1, CP, Topic)],
    [[<<"<h2>">>, note_topic(NoteMeta), <<"</h2>">>,
      <<"<p>">>,
      lpad_markdown:to_html(highlight_cp(NoteContent, CP)),
      <<"</p>">>]
     || {NoteMeta, NoteContent} <- Related].

truncatechars([String], N) ->
    case size(String) of
      Size when Size > N ->
          [binary:part(String, 0, N), <<"...">>];
      _ -> String
    end.

%
% Private for filters/tags
%

is_related({NoteMeta, NoteContent}, CP, Topic) ->
    case {note_topic(NoteMeta),
          binary:match(NoteContent, CP)}
        of
      {Topic, _} -> false;
      {_, nomatch} -> false;
      _ -> true
    end.

highlight_cp(NoteContent, CP) ->
    binary:replace(NoteContent, CP, <<"**">>,
                   [global, {insert_replaced, 1}]).
