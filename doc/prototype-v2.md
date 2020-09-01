This document describes the whole structure of a Vrac app, and how Vrac internally works.

# Vrac's architecture

## The small bits

### The events and the "event processing function"

An event is just a Clojure value.

When you trigger an event via a "dispatch function", the event is sent to an
"event processing function" provided by you which should return a sequence of effect descriptions.

How an event is structured and interpreted is up to you, Vrac does not enforce anything about that.
How the event processing function is working is also totally up to you as well.

For convenience, Vrac provides some utility functions to help you organize the event processing a bit,
but they are optional, and you can just build your own.

### The event's effects

The "event processing function" is going to return a sequence of effect descriptions
in the shape `[effect-type effect-description]`.

Example:
```clojure
[:msg->server {:action :buy
               :item :turnip
               :quantity 10
               :unit-price [90 :bell]}]
```

The next step is to execute effects based on those descriptions.
For the example above, it would mean to send a message to the server.

Vrac does not automatically process effect for you, you have to do it
by providing an "execute effects function".

There is one type of effect that Vrac is going to handle on its own,
it's `:client-db/diff`. This type of effect describes the change you
want to apply to the client db.

### The db's state and diffs

Vrac is updating the client db via effects of type `:client-db/diff`.
The format of the effect description is a diff from the [Diffuse library](https://github.com/green-coder/diffuse).

Example:
```clojure
(require '[diffuse.helper :as h])

[:client-db/diff (h/map-assoc :foo "hello"
                              :bar [1 2 3])]
```

The diff is applied to the client db to get its new state, then the old state,
the new state and the diff are used to update all the computed values currently
used by the app.

### The graph of computed values

A Vrac app has a "directed acyclic graph" of "compute nodes" which represents all
the operations being applied on the client db to get all the values which are currently
used by the app. These operations include data reads.

Vrac keep track of the partial order between those nodes in order to update them
in a typological order, from the client db to the render nodes.

Vrac finds which nodes might be affected by a change using a system of subscription
on the different paths in the client db, then it recomputes their output
based on:
- their old inputs,
- their new inputs,
- the diffs between those inputs,
- and their old output value.

The result of the computation for a compute node is a diff between its old output value
and the new one.

If the diff is not `nil`, then their dependent nodes are added to a queue
(if they are not already inside), and they will be recomputed once it's their turn.
This re-computation goes on until there are no longer any compute node in the queue.

### The render nodes

After the graph of computed values is up-to-date and reflects the new state of the client db,
the render nodes also receive the changes in a way similar to how the compute nodes
get updated.

The render nodes are sorted by depth, in order to update them in a topological order,
from the root to the leaves.

Vrac updates the attributes and children managed by each node, using:
- their old inputs,
- their new inputs,
- the diffs between those inputs,
- and their previous output.

Each render node as a type and its associated behavior:
- The `:html-if` and `:html-case` nodes are branching conditionally between different children.
- The `:html-for` nodes are managing dynamic lists of children.
- The `:html-component` nodes are managing the inclusion of Vrac components.
- The `:html-content` nodes are managing DOM nodes and their properties.

When a render node is included in the render tree, it declares its data requirements to Vrac.
When a render node is removed from the render tree, its data requirements are lifted.

For example, this can happen when we add/remove a person to/from a list of persons
which is displayed in the app.

Additionally, the `:html-component` node is doing some extra computation when
included/removed in/from the render tree, in order to deal with the parameters w.r.t. many
of the Vrac features related to data access and dynamic (anonymous) Vrac components.

### The template's grammar

## 
