# More OCaml

* Study repository for learning OCaml
* Basic `dune` setup
* TDD via `OUnit`
* Exercises More OCaml

## Prerequesites

* Docker
* VSCode Insiders
* VSCode Remote: Containers
* git

## Getting started

```sh
git clone https://github.com/marionebl/more-ocaml.git
cd more-ocaml

code-insiders .

# Inside the container
dune runtest -w
```

<details open>
<summary>
    Manual Setup
</summary>

```sh
git clone https://github.com/marionebl/more-ocaml.git
cd more-ocaml

# Non macOS: https://opam.ocaml.org/doc/Install.html 
brew install opam

opam init
opam install . --deps-only

# Inside the container
dune runtest -w
```

</details>