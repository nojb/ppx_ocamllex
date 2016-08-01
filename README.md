> An (experimental) `ppx` embedding of `ocamllex`

## Requirements

- `OCaml 4.03`
- `ocamlfind`
- `ocamlbuild`
- `ppx_tools`

## Building

````bash
$ git clone https://github.com/nojb/ppx_ocamllex
$ cd ppx_ocamllex
$ make # produces ocamllex.native
````

## Playing with it

```ocaml
$ ocamlfind ppx_tools/rewriter ./ppx_ocamllex.native <<EOF
let f lexbuf = (* recognizes the regexp {|a*b|} *)
  match%ocamllex lexbuf with
  | 'a' -> f lexbuf
  | 'b' -> ()
  | _ -> failwith "lex"
EOF
```

## Contact

Nicolas Ojeda Bar <n.oje.bar@gmail.com>
