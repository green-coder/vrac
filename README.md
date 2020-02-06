# Vrac

> Declarative html template library from the future.

## Status

Vrac is still a work in progress, its API is likely to change often.

## Introduction

Vrac is a Clojure(Script) library for creating front end applications in a truly declarative and expressive way.

Front end applications are usually a blend of data and HTML manipulation.
Vrac embraces this mix while keeping its users out of the mud by letting them
*declare* what they want, and then it uses external systems that do the hard work automatically.

## Syntax

```clojure
(ns coscup-demo.core
  (:require [vrac.core :as v :refer [defc]]))

;; Define some components

(defc speaker-comp [speaker]
  {:id :coscup/speaker}
  [:div
   [:div "Name: " (:speaker/name speaker)]
   [:div "Bio: " (:speaker/bio speaker)]])

(defc session-comp [session]
  {:id :coscup/session}
  [:div
   [:h3 "Title: " (:session/title session)]
   [:p "Description: " (:session/description session)]
   (let [speakers (:session/speakers session)]
     (when speakers
       [:div "Speakers: "
        [:ul
         (for [speaker speakers]
           [:li
            [:coscup/speaker {:id (:speaker/id speaker)
                              :speaker speaker}]])]]))
   [:div "Room: " (:room/name session)]])

(defc root-comp []
  {:id :coscup/root}
  [:div
   [:h1.class1#some-id.class2
    {:style {:background-color "lime"}}
    "Welcome to Vrac's demo"]
   [:ul
    (for [session (:session/my-favorites nil)]
      [:li [:coscup/session.item {:session session}]])]])

;; Derive data queries from them ...

(let [env {:components {:coscup/speaker speaker-comp
                        :coscup/session session-comp
                        :coscup/root root-comp}}]
  (v/get-queries env :coscup/root))
; => [#:session{:my-favorites [:session/title
;                              :session/description
;                              #:session{:speakers [:speaker/id :speaker/name :speaker/bio]}
;                              :room/name]}]

;; Derive renderers from them ...
; ...

;; Derive all your app's needs and beat/obliterate the average ...
; ...
```

## Under the hood

Behind the `defc` macro, Vrac components are mostly data.
These components are parsed via Clojure spec, then Vrac understand how the app's data is used. This allows
to generate data queries to download the app's data from remote servers and display them in the browser's page.

The way Vrac components are designed ensures that they do not depend on any specific implementation.

As of today:
- Vrac uses data queries in the [EQL](https://edn-query-language.org) format,
- the app's data is fetched by the [Pathom](https://github.com/wilkerlucio/pathom) library,
- and the html is rendered via [React](https://reactjs.org/).

However, the Vrac components are not limited or coupled to those libraries.

## Getting started

There is no release on Clojars at the moment. To use it via Clojure Deps:

```clojure
{:deps {vrac {:git/url "https://github.com/green-coder/vrac.git"
              :sha "<commit-sha>"}}}
```

Alternatively, you can use the [Vrac template](https://github.com/green-coder/vrac-template)
to generate a Shadow-CLJS project to get started more quickly.

## See also

- You can read the [Vrac samples](https://github.com/green-coder/vrac-samples),
  they are updated each time new Vrac features are developed.

## License

The Vrac library is developed by Vincent Cantin.
It is distributed under the terms of the Eclipse Public License version 2.0.
