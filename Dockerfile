# Published to marionebl/more-ocaml
FROM ocaml/opam2:ubuntu-18.04-ocaml-4.07

RUN sudo apt-get update
RUN sudo apt-get install m4 bmake cpio net-tools -y

RUN cd /home/opam/opam-repository
RUN git pull
RUN git checkout c23c1a7071910f235d5bc173cbadb97cd450e9fb
RUN cd -

RUN opam update
ADD more-ocaml.opam.locked more-ocaml.opam.locked
ADD more-ocaml.opam more-ocaml.opam
RUN opam install . --deps-only --with-test --locked
RUN rm more-ocaml.opam more-ocaml.opam.locked

RUN sudo apt-get install fswatch -y