-module(index).

-export([data/1, related_notes/1, site/1, with_hyperlinks/2]).

-define(HOSTNAME, "blog.zdsmith.com").
-define(ROOT, "../").

-type plist() :: plist().

%
% API
%
data(_) ->
    #{blog => {eterm, "subsetpark.config"},
      posts => {markdown, "posts/*.md"},
      pages => {markdown, "pages/*.md"},
      notes => {markdown, "notes/*.md"},
      front => {markdown, "front.md"}}.

site(Data) ->
    % Populate included BEAM files.
    true = code:add_path("_build/default/lib/tempo/ebin/"),
    Notes = notes(Data),
    Notes2 = with_metadata(Notes),
    #{"site/index.html" => {template, "templates/index.html", #{site_root => ""}},
      "site/feed.xml" =>
          {template,
           "templates/feeds.jinja",
           #{site_root => "", host => ?HOSTNAME, context => posts_with_context(Data)}},
      "site/posts/index.html" => {template, "templates/posts.html", #{site_root => ?ROOT, all_posts => posts(Data)}},
      "site/notes/index.html" =>
          {template, "templates/notes.html", #{site_root => ?ROOT, notes => Notes2}},
      "site/pages/{{page.title|slugify}}.html" =>
          {template_map, "templates/page.html", {page, pages(Data)}, #{site_root => ?ROOT}},
      "site/posts/{{post.title|slugify}}.html" =>
          {template_map, "templates/post.html", {post, posts(Data)}, #{site_root => ?ROOT}},
      "site/notes/{{note.topic|slugify}}.html" =>
          {template_map,
           "templates/note.html",
           {note, Notes2},
           #{site_root => ?ROOT, all_notes => Notes2}},
      "site/assets/*.css" => {files, "assets/*.css"},
      "site/images/*.png" => {files, "images/*.png"},
      "site/images/current/*.png" => {files, "images/current/*.png"},
      "site/images/*.gif" => {files, "images/*.gif"},
      "site/static/*.html" => {files, "assets/*.html"},
      "site/CNAME" => {string, ?HOSTNAME}}.

%
% Private
%
-spec is_post(plist()) -> boolean().
is_post(Post) ->
    case {os:getenv("LPAD_DRAFT"), plist:value("status", Post)} of
      {"true", _} ->
          true;
      {_, "post"} ->
          true;
      _ ->
          false
    end.

-spec datestr_to_822(string()) -> string().
datestr_to_822(DateStr) ->
    case tempo:parse(<<"%Y-%m-%d">>, list_to_binary(DateStr), datetime) of
      format_mismatch ->
          io:format("Can't convert date: ~p~n", [DateStr]),
          erlang:error(date_format_mismatch);
      {ok, DateTime} ->
          {ok, Formatted} = tempo:format(rfc2822, DateTime, datetime),
          Formatted
    end.

-spec add_post_context(plist()) -> plist().
add_post_context(Post) ->
    {_, Date} = lists:keyfind("date", 1, Post),
    Rfc822 = datestr_to_822(Date),
    lists:keystore("rss_date", 1, Post, {"rss_date", Rfc822}).

-spec posts_with_context(plist()) -> [plist()].
posts_with_context(Data) ->
    Posts = posts(Data),
    [add_post_context(V1) || V1 <- Posts].

posts(Data) ->
    Posts = plist:value(posts, Data),
    [V1 || V1 <- Posts, is_post(V1)].

pages(Data) ->
    plist:value(pages, Data).

notes(Data) ->
    plist:value(notes, Data).

with_metadata(Notes) ->
    [with_cp(with_topic(with_contents(V1))) || V1 <- Notes].

with_contents(NoteMeta) ->
    FileName = note_filename(NoteMeta),
    {ok, Contents} = file:read_file(FileName),
    [{contents, Contents} | NoteMeta].

with_topic(NoteMeta) ->
    FileName = note_filename(NoteMeta),
    BaseName = filename:basename(FileName, ".md"),
    [{topic, BaseName} | NoteMeta].

with_cp(NoteMeta) ->
    Topic = note_topic(NoteMeta),
    CP = binary:compile_pattern(list_to_binary(Topic)),
    [{cp, CP} | NoteMeta].

note_topic(NoteMeta) ->
    plist:value(topic, NoteMeta).

note_content(NoteMeta) ->
    plist:value(contents, NoteMeta).

note_cp(NoteMeta) ->
    plist:value(cp, NoteMeta).

note_filename(NoteMeta) ->
    plist:value('__file__', NoteMeta).

note_path(NoteMeta) ->
    [<<?ROOT>>, <<"notes/">>, erlydtl_filters:slugify(note_topic(NoteMeta)), <<".html">>].

%
% Custom Filters
%
-spec with_hyperlinks(binary(), [plist()]) -> string().
with_hyperlinks(MainNote, AllNotes) ->
    lists:foldl(fun (NoteMeta, ContentAcc) ->
                        case is_related(MainNote, NoteMeta) of
                          true ->
                              link_to_note(ContentAcc, NoteMeta);
                          _ ->
                              ContentAcc
                        end
                end,
                note_content(MainNote),
                AllNotes).

-spec related_notes(list()) -> iolist().
related_notes([NoteMeta, AllNotes]) ->
    CP = note_cp(NoteMeta),
    AllRelated = [V1 || V1 <- AllNotes, is_related(V1, NoteMeta)],
    [[<<"<h2>">>,
      <<"<a href=">>,
      note_path(RelatedMeta),
      <<">">>,
      note_topic(RelatedMeta),
      <<"</a>">>,
      <<"</h2>">>,
      lpad_markdown:to_html(highlight_cp(note_content(RelatedMeta), CP))]
     || RelatedMeta <- AllRelated].

%
% Private for filters/tags
%
-spec is_related(plist(), plist()) -> boolean().
is_related(NoteMeta, RelatedMeta) ->
    NoteContent = note_content(NoteMeta),
    NoteTopic = note_topic(NoteMeta),
    RelatedCP = note_cp(RelatedMeta),
    case {note_topic(RelatedMeta), binary:match(NoteContent, RelatedCP)} of
      {NoteTopic, _} ->
          false;
      {_, nomatch} ->
          false;
      _ ->
          true
    end.

-spec highlight_cp(binary(), binary:cp()) -> binary().
highlight_cp(NoteContent, CP) ->
    binary:replace(NoteContent, CP, <<"**">>, [global, {insert_replaced, 1}]).

-spec link_to_note(binary(), plist()) -> iolist().
link_to_note(NoteContent, RelatedMeta) ->
    CP = note_cp(RelatedMeta),
    Path = note_path(RelatedMeta),
    [binary:replace(NoteContent, CP, <<"[]">>, [global, {insert_replaced, 1}]),
     <<"(">>,
     Path,
     <<")">>].
