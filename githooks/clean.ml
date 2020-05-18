#!/usr/bin/env ocaml

#use "topfind";;
#thread

#require "shexp.process";;
#use "./githooks/common.ml";;
#require "fileutils"

let abshooks = Stdlib.List.map (Filename.concat destination) hooks_names

let () = FileUtil.rm abshooks

