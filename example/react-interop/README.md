# Vrac + React 19, interop demo

This project shows how to use Vrac and React 19 together.
1. How to embed React roots and components inside a Vrac app.
2. How to use the reactive data from the Vrac app inside the embedded React components.
   The source code shows how to do it:
   1. using Siagent: just deref the reactive nodes, "et voilà".
   2. using UIx: use the `use-reactive` React hook.

## How to run this project

```shell
npm install
npm start
```

This will run Shadow-CLJS. Once the project finished to compile,
follow the [displayed link](http://localhost:3000) to see the app.

## Production build

Compiling for production:

```shell
npm run release
```

The report describing the size of the different parts is `public/js/report.html`.
