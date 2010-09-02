=====
mp3fs
=====
------------------------------------------------------------------
A filesystem which translates many types of music files into mp3's
------------------------------------------------------------------

Author: Chris Stucchio (stucchio@gmail.com)

mp3fs solves the problem that you store your music files in lossless flac or other formats,
but your mp3 player or phone wants mp3 files.

What it does and how it works
=============================

A simple example illustrates what mp3fs does::

    $ ls music
    01 Ghosts I.flac  03 Ghosts I.wav  mp3file.mp3
    02 Ghosts I.wav   04 Ghosts I.ogg  nin_ghosts_I-IV_flac
    list_of_music.txt
    $ mkdir mp3music
    $ mp3fs music mp3music
    $ ls mp3music
    01 Ghosts I.mp3  03 Ghosts I.mp3  mp3file.mp3
    02 Ghosts I.mp3  04 Ghosts I.mp3  nin_ghosts_I-IV_flac

Now, lets copy some mp3 files to our music player::

    $ cp mp3music/01\ Ghosts\ I.mp3 /media/NEXUS\ ONE

When you type this command, mp3fs will convert 01\ Ghosts\ 1.flac into 01\ Ghosts\ I.mp3 and
then the mp3 version will be copied to /media/NEXUS\ One. In short, mp3fs lazily converts your
music files to mp3 format.

Installing
==========

Installation is the haskell standard::

    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install

Requirements
------------
You need the following haskell libraries:
    Unixutils >= 1.2.2,
    HFuse >= 0.2.1,

You also need the following command-line tools.
    lame (required for all formats)
    flac (required to convert flac files)
    oggdec (required to convert ogg files)
    faad (required for mp4 and aac files)

If you lack some of these, mp3fs will degrade gracefully. If you lack lame, it just won't work.

Some caveats:

  * Obviously this is somewhat slow and CPU heavy.

  * Any calls to getfilestat (e.g., via ls -l in an mp3fs directory) will cause the file to be converted.
    ls -l will be a VERY slow operation.

  * Nautilus (the Gnome file manager) will, by default, call getfilestat on all files contained in the
    root of any newly mounted directory. The result is that when you mount an mp3fs filesystem, all files
    will *immediately* be converted to mp3. This can be slow. This can be switched off via gconfeditor,
    at the path /apps/nautilus/preferences/media_automount_open

Getting the code
================

Github: http://github.com/stucchio/Mp3FS

Bitbucket: http://bitbucket.org/stucchio/mp3fs


Contact
=======

If you see a bug, email me. stucchio@gmail.com
