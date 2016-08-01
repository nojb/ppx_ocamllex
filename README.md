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
$ ocamlfind ppx_tools/rewriter ./ppx_ocamllex.native <<EOF > foo.ml
let rec f lexbuf = (* recognizes the regexp {|a*b|} *)
  match%ocamllex lexbuf with
  | 'a' -> f lexbuf
  | 'b' -> ()
  | _ -> failwith "lex"

let () =
  f (Lexing.from_string (read_line ()))
EOF
$ ocamlc -I lib lib/lexing_plus.mli lib/lexing_plus.ml foo.ml
$ ./a.out
aaab
$ ./a.out
aaac
Fatal error: exception Failure("lex")
```

## Contact

Nicolas Ojeda Bar <n.oje.bar@gmail.com>
