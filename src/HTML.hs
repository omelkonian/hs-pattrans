{-# LANGUAGE QuasiQuotes #-}
module HTML where

import Control.Monad (forM_)

import Data.List (isInfixOf, isSuffixOf)

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
  results <- listDirs
  forM_ results $ \r -> do
    allImages <- listFilesRecursively
    print allImages
    let images = filter (".png" `isSuffixOf`) allImages
    let experts    = filter ("experts" `isInfixOf`) images
        algorithms = filter ("algorithms" `isInfixOf`) images
        random     = filter ("random" `isInfixOf`) images
        ngram      = filter ("ngram" `isInfixOf`) images
    writeHTML (r ++ ".html") [shamlet| 
      <div id="accordion">
        <h2> Experts
        <div id="chart0">
          $forall f <- experts
            <a class='gallery' href="#{f}">
              <img src="#{f}">

        <h2> Algorithms
        <div id="chart1">
          $forall f <- algorithms
            <a class='gallery' href="#{f}"> <img src="#{f}">

        <h2> Random
        <div id="chart2">
          $forall f <- random
            <a class='gallery' href="#{f}"> <img src="#{f}">

        <h2> N-Gram
        <div id="chart3">
          $forall f <- ngram
            <a class='gallery' href="#{f}"> <img src="#{f}">

        <script>
          \$(document).ready(function() {
            for (var i = 0; i < 4; i++) {
              \$(`#chart${i}`).justifiedGallery({
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
            }
            \$('#accordion').accordion({header: "h2", active:false, collapsible: true});
          });
      |]

  writeHTML "index.html" [shamlet|
    <h2> Results
    <ul>
      $forall r <- results
        <li>
          <a href=#{r}.html>#{r}
    |]


