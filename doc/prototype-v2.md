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

#### Compute nodes

A Vrac app has a "directed acyclic graph" of "compute nodes" which represents all
the operations being applied on the client db to get all the values which are currently
used by the app.

Vrac keeps track of the partial order between those nodes in order to update them
in a typological order, from the client db to the render nodes.

#### Data nodes

The client db is a "data node" because it is a data source for the compute nodes,
the templates, or anything else in the app which can read data.

Compute nodes are also data nodes.

#### Subscriber tree

When a data node changes, a diff describes how its value changes.
Vrac finds which dependent nodes might be affected by the change using a hierarchical
system of subscription per data node named a "subscriber tree" in which
subscribers can be notified of changes on a particular part of the data node's value.

Finding which dependent nodes need to be notified is done extremely fast.
The computational cost depends on the size of the change (which is ridiculously small
most of the time), and on the number of subscribers which will be notified (only them).

#### Update process of a compute node

Any compute node needs to be updated when there is a change on at least one of its inputs.

Its update function is provided by the user. For each of the input, the update function can access: 
- their value before the change,
- their value after the change,
- the diff representing the change.

Additionally, a compute node can access its own value.

The result of an update function contains a diff representing what has changed
on its compute node.

If the diff is not `nil`, then the dependent nodes are added to a priority queue,
and will be updated once it's their turn.
The update process stops when there are no longer any compute node in the queue.

#### Topological order and update queue

Nodes have to be updated in a topological order to ensure that any input of a compute
node that "should be updated" has effectively been updated before the current node.

To ensure this property, we calculate the `depth` of each node when we insert it
into the graph. It consists of the max of the `depth` of all its input nodes +1.
This property only has to be calculated once for the lifetime of a compute node.

When a compute node to be updated is sent to a priority queue, we add it to a set
of nodes which have the same `depth`. Nodes with the same `depth` don't depend
on each other, which means that the elements of each set can be updated in any order
within their group.

### Dynamics in the computation graph

Some compute nodes represent branching (`if`, `when`, `case`), they are a little special.
Depending on their "condition" input, their output will be computed using different nodes.

For obvious performance reasons, the `then` and the `else` of a `if` node should not exist
at the same time. We want only one of them be instantiated and updated at any time.
It means that when the condition input's truthy value is changing, some compute nodes
have to be unregistered and discarded while some other have to be created and registered.
The same dynamics happen with the `for` nodes, when elements are added/removed from the
iterated collection.

The `depth` of a `if` node is calculated based on both `then` and `else` inputs, even if
they are not instantiated. As a consequence, when a `if` node is updated and needs to rely
on freshly created input nodes (`then` or `else`), it can evaluate them immediately without
going back to the priority queue and result a result.

Nodes to add the graph are added immediately, and node which should be removed are
effectively removed after the update of all the compute nodes and render nodes.
The reason for this delay is to make sure that we are not removing then re-adding
the intermediate nodes which provide data to both the nodes removed and the nodes added.

### The render nodes

After all the compute nodes are up-to-date and reflects the new state of the client db,
the render nodes receive the changes on the values they use.

The render nodes are sorted by depth from the root, in order to update them in a
topological order.

The depth of a render node does not change after it is instantiated.
If we want to drag and drop a render node between 2 places, we discard it in
its original location then instantiate it in its new location, providing the illusion
of the node being moved in its hierarchy.
The view will always be 100% driven by the data, Vrac does not support any manual DOM
mutation.

Vrac updates the attributes and children managed by each node, using:
- their old inputs,
- their new inputs,
- the diffs between those inputs,
- and their previous output.

Each render node has a type, and an associated behavior:
- The `:html-if` and `:html-case` nodes are branching conditionally between different children render nodes.
- The `:html-for` nodes are managing dynamic lists of children render nodes.
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
