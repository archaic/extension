(ns extension.ansi
  (:require [clojure.string :as s]))

(def sgr-codex
  {:none 0
   :bold 1
   :underline 3
   :blink 5
   :reverse 7
   :hidden 8
   :strike 9
   :black 30
   :red 31
   :green 32
   :yellow 33
   :blue 34
   :magenta 35
   :cyan 36
   :white 37
   :fg-256 38
   :fg-reset 39
   :bg-black 40
   :bg-red 41
   :bg-green 42
   :bg-yellow 43
   :bg-blue 44
   :bg-magenta 45
   :bg-cyan 46
   :bg-white 47
   :bg-256 48
   :bg-reset 49})

(defn esc
  [codes]
  (let [codes
        (map sgr-codex
             codes
             codes)
        
        codes
        (s/join \;
                codes)]
    
    (str \u001b \[
         codes
         \m)))

(defn escape
  [& codes]
  (esc codes))

(defn sgr
  [string & codes]
  (str (esc codes)
       string
       (escape :none)))
