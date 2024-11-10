clRPC
-----

![Demo](./demo.gif)

> [!WARNING]
>
> Just a fun experiment

Common Lisp offers fantastic REPL-driven development. Also letting you update code at runtime, so you can modify server procedures on-the-fly. Plus, Lisp's macros make parsing types super easy, so I thought, "Why not try building a little experimental project to create type-safe APIs?" This is inspired by [tRPC](https://github.com/trpc/trpc), but instead of TypeScript on the backend, it is Common Lisp.

## Some TODOs
- Implement mutations
- Enable passing data to procedures (For example a database handle)
- Support output type other than strings
- Generate documentation from CLOS documentation slot
