# Vrac

> Declarative html template library from the future.

## Status

Vrac is still an early work in progress. It is not ready to be used yet.

The following documentation is "from the future", the library does not work in that way yet.

## Introduction

Vrac is a Clojure(Script) library for creating front end applications in a truly
declarative and expressive way.

Vrac components are not render functions, they are mostly a data structure which
captures how data should be manipulated before being mixed with HTML patterns.

Vrac components resemble a Clojure functions which output Hiccup, but keep in mind that:
- there is a `(quote ...)` around the render expression that keeps it as data,
- their content is not really Clojure expressions. Instead, it is a small
  Domain Specific Language designed to look familiar to Clojure programmers
  and easy to learn.

The directing idea of Vrac is to let the user say what he wants to display in the
browser in a declarative way, instead of asking the user to write a rendering process.
The DSL of the components was designed to capture as much of the user's intent as possible,
so that automated systems have sufficient information to turn it into a working web app.

In this way, the hardest part of the work (the mechanical plumbing) just have to be done once,
by the implementors of those automated systems.
The user can then focus on the business logic and appearance of his web app.

## Usage

Vrac is made of a collection of namespaces which provide functionalities for
many different aspects of a front end application. While mostly decoupled and optional,
those namespaces are made to play well together and complement each other.

There are namespaces for:
- describing and validating Vrac components using a DSL defined by a spec,
- traversing, manipulating and combining the templates of the Vrac components,
- deriving EQL queries from Vrac components,
- maintaining a normalized local database,
- handling events, local database reducers and other effects,
- deriving a flow graph of derived data from Vrac components,
- implementing cascading reactivity in the flow graph,
- deriving renderers from Vrac components,
- deriving translation resources from Vrac components,
- interoperability with 3rd party renderers, like React.

### The components

This is what a Vrac component looks like:

```clojure
(ns my-app.core
  (:require [vrac.core :as v :refer [defc]]))

(defc user-profile-component [user]

  ;; Optional meta data.
  {:id :my-app/user-profile}

  ;; The template, wrapped into (quote ...) by the defc macro.
  [:.user-profile
   [:img.picture {:src (:user/picture-url user)}]
   [:.name "Name: " (:user/name user)]
   [:.bio "Bio: " (:user/bio user)]])
```

Notes:
- The `:id` value for a component is how it can be referred to from other components.
  It has to be a namespaced keyword.
- `:.user-profile` is a shortcut for `:div.user-profile` in Hiccup, a `div` html node with the class `user-profile`.
- `(:user/picture-url user)` represents the usage of the field `:user/picture-url` on
  the data passed to the component under the key `:user`.

This last point is **very important** because this data construct gives
the system the information of which fields are required to display this component.

### The DSL's directive

Additionally to documenting the data usage, the DSL in the components supports those directives:
- `if` and `when` allow to specify conditional rendering,
- `for` allows to specify rendering on elements of collection,
- `let` allows you to specify local names to refer to different data parts.

Example:

```clojure
(let [blog-url (:user/blog-url user)
      friends (:user/friends user)]
  [:div (if blog-url
          "No blog"
          [:a {:href blog-url} "Link to my blog"])
        [:div "My Clojurian friends:"
              [:ul (for [friend friends]
                     (when (:user/clojurian? friend)
                       [:li (:user/name friend)]))]]])
```

Those directives (and the fact that we read them as data) provides a lot of semantic
information about the intent of the user.
Not only the system knows what data is required, now it knows **in which situations**
it is required.

From that kind of template, Vrac can derive an EQL query representing the data required
by a tree of components in any given state. We can then download that data from a server
if it is not already available locally.

Vrac can also help the user understanding his own source code by providing answers
to questions like:
- Which components use a given data?
- In which state do I need to be to have the component `:my-ns/my-component` displayed?
- Why do I have a given component's instance displayed?
- Where is in the local database the data displayed at a specific place in the web page?

### Components composition

> Imagine there's no countries<br>
> It isn't hard to do<br>
> Nothing to kill or die for<br>
> And no religion too<br>
> Imagine all the people living life in peace, you<br>
><br>
> You may say I'm a dreamer<br>
> But I'm not the only one<br>
> I hope some day you'll join us<br>
> And the world will be as one<br>
>
> -- John Lennon

In Vrac, the only purpose of components is to be reusable by the user.
Their boundaries is up to the user and are not meant to be used has hint
for improving performances during the rendering process.

Components which refer to each other are conceptually "inlined" one into
another, and the values passed as parameter are also inlined.

They are treated the same as macros.

Vrac provides functions which rename conflicting names during the component
expansion, so that the user does not have to worry about it.

Example of component composition:

```clojure
(defc todo-item-comp [todo-item]
  {:id :my-app/todo-item}
  [:.title {:class {:done? (:todo-item/done? todo-item)}}
           (:todo-item/title todo-item)])

(defc todo-list-comp [todo-list]
  {:id :my-app/todo-list}
  [:div [:h3 (:todo-list/title todo-list)]
        [:ul (for [item (:todo-list/items todo-list)]
                    ; component reference and param passing.
               [:li [:my-app/todo-item {:todo-item item}]])]])
```

The expanded template would look like:

```clojure
[:div [:h3 (:todo-list/title todo-list)]
      [:ul (for [item (:todo-list/items todo-list)]
             [:li [:.title {:class {:done? (:todo-item/done? item)}}
                           (:todo-item/title item)])
```

### Data-flow graph and derived data

Sometimes the data in the local database needs some processing before being
displayed on a web page.
Vrac allows the user to specify those data processings by describing
Clojure functions calls directly inside a component's template.

However since the functions' implementation are not readable, the user
needs to annotation in the metadata the fields which are used in the function,
to make sure that Vrac still have the full picture of any component's requirements.

Example:

```clojure
(defn {^:eql [:todo-item/done?]} only-done-items
  [items]
  (into [] (filter :todo-item/done?) items))

(defc todo-list-comp [todo-list]
  {:id :my-app/todo-list}
  [:div [:ul (for [done-item (only-done-items (:todo-list/items todo-list))]
               [:li [:my-app/todo-item {:todo-item done-item}]])]
        [:div (clojure.core/count (only-done-items (:todo-list/items todo-list)))
              " item(s) done"])
```

Alternatively, derived data can also be calculated on demand by using some specific
keys which are resolved dynamically. Both approach are equivalent, but using a
function call allows more flexibility on specifying the process' inputs.

```clojure
(defc todo-list-comp [todo-list]
  {:id :my-app/todo-list}
  [:div [:ul (for [done-item (:todo-list/done-items todo-list))]
               [:li [:my-app/todo-item {:todo-item done-item}]])]
        [:div (:todo-list/done-items-count todo-list)))
              " item(s) done"])
```

**Important note:** It does not matter if the same process on the same data
is repeated multiple times in the components. The user only have to make sure
that the processing is correct. The system, once feed with this kind of information,
can make sure all by itself that the data is processed without redundancies,
and updated only when necessary.

Similarly, prop drilling in Vrac is a **non-issue** in terms of performances,
as the semantic of rendering is decoupled from the process of rendering.

### Custom semantic in templates

Vrac's template format is open, it can incorporate custom kinds of information
which can be interpreted as the user wish.

For instance, expressions to be translated could be specified in this way:

```clojure
(defc my-page-component []
  [:.menu
    [:.item (translation "Homepage")])
    [:.item (translation "News (%1)" (:news/count nil)})])
```

The key is to customize how the template is read, and act accordingly.

> "It's just data."

In the case of translations, because it is additional information which when
feed into the system, Vrac would then be able to answer these questions:
- What are my translations in my whole app?
- If I want to visualize a given translation, which component should I display?
- What should be the state of the local database so see it?
- What resources do I need to have translated in order to display a given
  page without missing text?


### No technology lock in

The way Vrac components are designed ensures that they do not depend on any specific implementation.

As of today:
- Vrac uses data queries in the [EQL](https://edn-query-language.org) format,
- the app's data is fetched by the [Pathom](https://github.com/wilkerlucio/pathom) library,
- and the html is rendered via [React](https://reactjs.org/).

However, the Vrac components are not limited or strongly coupled to those libraries.

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
