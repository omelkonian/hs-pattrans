{-# LANGUAGE QuasiQuotes #-}
module HTML where

import Control.Monad (forM_)

import Data.List (isSuffixOf)

import Text.Hamlet (Html, shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Parser

writeHTML :: FilePath  -- ^ output filename
          -> Html      -- ^ HTML <body>
          -> IO ()     -- ^ wrap given HTML and writes to the filesystem
writeHTML fname body = writeFile fname $ renderHtml [shamlet| 
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
  <html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <title>

    <script src="#{googleLib}/jquery/3.3.1/jquery.min.js">

    <link rel="stylesheet" href="#{googleLib}/jqueryui/1.12.1/themes/smoothness/jquery-ui.css">
    <script src="#{googleLib}/jqueryui/1.12.1/jquery-ui.min.js">
    
    <script src="#{cdnLib}/jquery.colorbox/1.6.4/jquery.colorbox-min.js">

    <link rel="stylesheet" href="#{cdnLib}/justifiedGallery/3.7.0/css/justifiedGallery.min.css">
    <script src="#{cdnLib}/justifiedGallery/3.7.0/js/jquery.justifiedGallery.min.js">

  <body>
    #{body}
  |]
  where
    googleLib = "https://ajax.googleapis.com/ajax/libs"
    cdnLib    = "https://cdnjs.cloudflare.com/ajax/libs" 

generateAll :: FilePath  -- ^ path where generated output resides
            -> IO ()     -- ^ generates HTML that navigates through the results
generateAll outDir = cd outDir $ do
  let gotoParent fname = [shamlet|
    <a href=#{fname} style="text-decoration: none"> &#8624;
  |]
  let mkChart images = [shamlet|
    <div id="chart">
      $forall f <- images
        <a class='gallery' href="#{f}">
          <img src="#{f}">

    <script>
      \$(document).ready(function() {
        \$(`#chart`).justifiedGallery({
          rowHeight : 140,
          lastRow : 'nojustify',
          margins : 3
        }).on('jg.complete', function () {
          \$(this).find('a').colorbox({
            maxWidth : '80%',
            maxHeight : '80%',
            opacity : 0.8,
            transition : 'elastic',
            current : ''
          });
        });
      });
    |]

  results <- listDirs
  rImages <- filter (".png" `isSuffixOf`) <$> listFiles
  writeHTML "index.html" [shamlet|
    <h2> Results
    ^{mkChart rImages}
    <ul>
      $forall r <- results
        <li>
          <a href=#{r}/index.html>#{r}
  |]

  forM_ results $ \r -> cd r $ do
    groups <- listDirs
    gImages <- filter (".png" `isSuffixOf`) <$> listFiles
    writeHTML "index.html" [shamlet|
      <h2>
        ^{gotoParent "../index.html"}
        #{r}
      ^{mkChart gImages}
      <ul>
        $forall g <- groups
          <li>
            <a href=#{g}/index.html>#{g}
    |]

    forM_ groups $ \g -> cd g $ do
      pieces <- listDirs
      pImages <- filter (".png" `isSuffixOf`) <$> listFiles
      writeHTML ("index.html") [shamlet|
        <h2>
          ^{gotoParent "../index.html"}
          #{g}
        ^{mkChart pImages}
        <ul>
          $forall p <- pieces
            <li>
              <a href=#{p}.html>#{p}
      |]

      forM_ pieces $ \p -> do
        images <- filter (".png" `isSuffixOf`) <$> listFilesRecursively
        writeHTML (p ++ ".html") [shamlet| 
          <h2>
            ^{gotoParent "index.html"}
            #{p}
          ^{mkChart images}
        |]
