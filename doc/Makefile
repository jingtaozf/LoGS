
LoGS_obj.pdf: LoGS_obj.tex LoGS_obj_diagram.pdf
	pdflatex LoGS_obj.tex

LoGS_obj_diagram.ps: LoGS.dot
	dot -Tps LoGS.dot -o LoGS_obj_diagram.ps

LoGS_obj_diagram.pdf: LoGS_obj_diagram.ps
	ps2pdfwr -sPAPERSIZE=a0 LoGS_obj_diagram.ps LoGS_obj_diagram.pdf

LoGS.pdf: LoGS.texinfo
	pdftex LoGS.texinfo
	texindex LoGS.??
	pdftex LoGS.texinfo

clean: 
	rm -rf LoGS.aux LoGS.cp LoGS.fn LoGS.ky LoGS.log LoGS.pdf LoGS.pg LoGS.toc LoGS.tp LoGS.vr \#LoGS.texinfo# LoGS.info LoGS.texinfo~ LoGS.cp LoGS.fn
