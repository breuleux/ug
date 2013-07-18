
{html}:[<!DOCTYPE html>]
html ..

  head ..
    meta [http-equiv = Content-type] [content = text/html; charset=UTF-8] ..
    {insert_document}: xlinks
    title ..
      {meta}: title
    {insert_document}: css

  body ..
    {
      engine["maybe text @@ link"] = site_link
    }
    #nav ..
      logo <- {Raw('<img src="%sassets/media/ug.png" alt="Quaint" />' % siteroot)}
      #logo .. {logo} @@ index
      .navlink #doc .. Tour @@ tour
      .navlink #source .. Source::https://github.com/breuleux/ug

    h1 .title ..
      {meta}: title

    #main ..
      {insert_document}: main

    #foot ..
      .footlink ..
        {ghurl = "https://raw.github.com/breuleux/ug/master/"}
        {ghdocurl = "https://raw.github.com/breuleux/ug/master/doc/content/"}
        {
          GenFrom('meta', lambda d: ('<a href="%s%s">Source for this file</a>'
                                     % (ghdocurl, d['realpath'])))
        }

    {insert_document}: js
    {insert_document}: errors
