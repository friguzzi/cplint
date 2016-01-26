
manual.pdf: manual.tex bib.bib inference.tex learning.tex learning_par.tex parameters.tex syntax.tex
	pdflatex manual
	bibtex manual
	pdflatex manual
	pdflatex manual

manual.html: manual.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex
	pandoc manual.tex -o manual.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 
  
help-cplint.html: help-cplint.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex
	pandoc help-cplint.tex -o help-cplint.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 

help-cplint.pdf: help-cplint.tex inference.tex learning.tex learning_par.tex parameters.tex syntax.tex
	pdflatex help-cplint
	bibtex help-cplint
	pdflatex help-cplint
	pdflatex manual
