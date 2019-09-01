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

USER root
SHELL ["/bin/bash", "--login" , "-c"]
RUN ln -s "/home/opam/.opam" "/root/.opam"
ENV OPAM_SWITCH_PREFIX='/root/.opam/4.07'
ENV CAML_LD_LIBRARY_PATH='/root/.opam/4.07/lib/stublibs:/root/.opam/4.07/lib/ocaml/stublibs:/root/.opam/4.07/lib/ocaml'
ENV OCAML_TOPLEVEL_PATH='/root/.opam/4.07/lib/toplevel'
ENV MANPATH=':/root/.opam/4.07/man'
ENV PATH='/root/.opam/4.07/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'
