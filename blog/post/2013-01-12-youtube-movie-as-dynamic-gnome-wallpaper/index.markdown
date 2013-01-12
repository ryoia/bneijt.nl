---
title: Youtube movie as dynamic GNOME wallpaper
date: 2013-01-12
---

GNOME has support for dynamic wallpapers based, but if you want it to be really dynamic you will need a lot of images. To make it easier, I decided to turn to
[Best of the Web HD](http://www.youtube.com/watch?v=TXdsqWqR4ro)
(there is also a [version two](http://www.youtube.com/watch?v=WJkg6j7IkyE) and [three](http://www.youtube.com/watch?v=0YlTOSiVLnQ)).

First run youtube-dl on the video you want to download:

    youtube-dl --format 45 'http://www.youtube.com/watch?v=TXdsqWqR4ro'

I selected the best format I could think of from the format list you can get when you run `youtube-dl --list-formats`.

Now, let's split up the video into separate frames:

    gst-launch filesrc location=TXdsqWqR4ro.webm ! decodebin ! \
    videorate ! video/x-raw-yuv,framerate=1/4 ! \
    jpegenc ! multifilesink location=img%d.jpg

The important part of this pipeline is the videorate plugin with a framerate of one fourth, which makes sure you don't dump every frame but one in four.

Now that we have a directory full of files, we can start doing some Haskell:

    #!/usr/bin/runghc
    import System.Directory
    import Data.List
    import System.FilePath

    imageFileSuffixes = [".jpg", ".png"]

    isImageFileName :: String -> Bool
    isImageFileName name = any (flip isSuffixOf name) imageFileSuffixes

    imageFiles candidates = filter isImageFileName candidates
    backgroundEntry path = "<static><duration>300</duration><file>" ++ path ++ "</file></static>"

    backgroundXml files = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<background>" ++ (concat (map backgroundEntry files)) ++ "</background>"

    main = do
        cwd <- getCurrentDirectory
        relativePaths <- getDirectoryContents cwd
        let paths = map (combine cwd) relativePaths
        putStrLn (backgroundXml (imageFiles paths))

If you save this file as gen.hs and make it executable (`chmod u+x gen.hs`) you can generate
the background xml using `./gen.hs > best_of_web_hd.xml`.

This gives us an XML file with the absolute path of each image. Now the last part of the puzzle to get it to work on Ubuntu GNOME remix 12.10:

    gsettings set org.gnome.desktop.background picture-uri 'file://path/to/best_of_web_hd.xml'

I think you should also be able to use the normal background selector, but I could not get the GNOME Shell wallpaper selector to see anything other than images.

See also
--------
Some other posts, doing the same kind of thing:

  * [Ask ubuntu question one](http://askubuntu.com/questions/71008/how-do-i-customize-desktop-wallpaper-slideshow)
  and [two](http://askubuntu.com/questions/134/how-do-i-create-a-desktop-wallpaper-slideshow)
  * [Linuxjournal article](http://www.linuxjournal.com/content/create-custom-transitioning-background-your-gnome-228-desktop)

Another cool video to use is [beautiful nature scenery HD](http://www.youtube.com/watch?v=YW8p8JO2hQw).


