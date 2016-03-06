
manual.pdf: manual.tex bib.bib inference.tex learning.tex learning_par.tex parameters.tex syntax.tex semantics.tex inf_goals.tex
	pdflatex manual
	bibtex manual
	pdflatex manual
	pdflatex manual

manual.html: manual.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pandoc manual.tex -o manual.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 
  

help-cplint.pdf: help-cplint.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex semantics.tex inf_goals.tex
	pdflatex help-cplint
	bibtex help-cplint
	pdflatex help-cplint
	pdflatex help-cplint

help-cplint.html: help-cplint.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pandoc help-cplint.tex -o help-cplint.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 --mathml
