## Vcup properties

Also named "props" in the source code, the Vcup properties are always specified using Clojure keywords.

When the keyword's namespace is `"a"` (e.g. `:a/for`),
the keyword refers to [an HTML attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes/for).

When the keyword's namespace is `"p"` (e.g. `:p/htmlFor`),
the keyword refers to [a DOM element's property](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLabelElement/htmlFor).

When the keyword's namespace is `"on"` (e.g. `:on/click`),
the keyword refers to [an event](https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event),
and the value in an event handler function.

When the keyword has no namespace, its meaning depends on its name:
1. Either the keyword refers to some Vrac specific properties which provide
   additional convenience to the user like `:style`, `:class`, `:ref`, or
2. if the keyword starts with `"data-"` then it represents
   [a custom data attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Global_attributes/data-*)
   (e.g. `:data-testid`), or else
3. if the prop is applied on an SVG on MathML element then it represents an attribute, or else
4. it represents a DOM element's property (e.g. `:htmlFor`, which is equivalent to `:p/htmlFor`).

### Letter case in prop names

Vrac doesn't modify the letter case, the user is directly speaking the browser's language.

In practice, it means that:
1. Attributes names are [the same as specified in this page](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes),
   e.g.:
   1. `:a/readonly`
   2. `:a/tabindex`
   3. `:a/accept-charset`
2. DOM element's properties names are [the same as specified in this page](https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement#instance_properties),
   e.g.:
   1. `:p/readOnly` or `:readOnly`
   2. `:p/tabIndex` or `:tabIndex`
   3. `:p/autocorrect` or `:autocorrect`
3. Event names are [the same as specified in this page](https://developer.mozilla.org/en-US/docs/Web/API/Element#events),
   e.g.:
   1. `:on/dblclick`
   2. `:on/keydown`
   3. `:on/fullscreenchange`
