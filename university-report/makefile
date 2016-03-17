CORPUS := introduction.md chapter1.md chapter2.md chapter3.md conclusion.md
METADATA := metadata.yaml
TEMPLATE := mytemplate.latex
OUTPUT := university-report.pdf

report : $(CORPUS) $(METADATA)
	pandoc $(METADATA) 		\
	$(CORPUS)		 		\
	-f markdown				\
	--template=$(TEMPLATE)	\
	-o $(OUTPUT)			\
	-s
