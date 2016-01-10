
manual.pdf: manual.tex manual.bbl
	pdflatex manual
	pdflatex manual

manual.html: manual.tex manual.bbl
	pandoc manual.tex -o manual.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 
  
help-cplint.html: help-cplint.tex
	pandoc help-cplint.tex -o help-cplint.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=2 
