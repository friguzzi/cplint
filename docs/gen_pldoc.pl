:- use_module(library(pldoc)).
:- use_module(library(doc_files)).
:- use_module(library(filesex)).

:- initialization(gen_doc, main).

doc_file(cplint_util).
doc_file(pita).
doc_file(mcintyre).
doc_file(slipcover).
doc_file(viterbi).
doc_file(kbest).
doc_file(pitaind).
doc_file(lemur).
doc_file(highlight).

load_all :-
    ensure_loaded(library(clpr)),
    forall(doc_file(File),
           ( directory_file_path('../prolog', File, Path),
             ensure_loaded(Path))).

gen_doc :-
    load_all,
    Opts = [ doc_root('./pldoc'),
             include_reexported(false)
           ],

    forall(doc_file(File),
           ( directory_file_path('../prolog', File, Path),
             doc_save(Path, Opts))).